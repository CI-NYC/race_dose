# READ ME ----------------------------------------------------------------------
#
#       Author: Shodai Inose
# Last updated: 5 Aug 2024
#
#         Note: Modified code originally written by Nick in here:
#               https://github.com/CI-NYC/OUD-dynamic-dosing/blob/main/scripts/04_lmtp_formatting.R
#               + Modified code written by Sarah Forrest
#
# ------------------------------------------------------------------------------

library(tidyverse)

# read in daily data with relapse variables
visits_relapse <- readRDS(here::here("data/ctn94_nonimpute/clean_visits_with_relapse_nonimpute.rds"))

visits_relapse |> filter(relapse_date <= initiation_date) |> select(who) |> distinct() |> nrow() # 1 person with relapse date before initiation date (XR-NTX)

### relevant for race-dose paper: 

# make initiation date into day 1
visits_relapse_initiated <- visits_relapse |>
    filter(never_initiated == FALSE) |>
    filter(when >= initiation_date) |> # only looking at on or after initiation
    group_by(who) |>
    mutate(days_since_initiation = as.numeric(when - initiation_date + 1), # days from initiation
           weeks_since_initiation = ceiling(days_since_initiation / 7)) # weeks from initiation 

# restricting data to criteria for paper
visits_relapse_initiated <- visits_relapse_initiated |>
    filter(weeks_since_initiation <= 4,
           xrace != 4) |>
    filter(medicine == "bup" | medicine == "met") 

# for people with less than 4 weeks of medication data, add in the missing visits up to week 4 (28 days per person)
visits_relapse_initiated <- left_join(map_dfr(split(visits_relapse_initiated, visits_relapse_initiated$who),
                                              function(x) {
                                                  tibble(who = x$who[1],
                                                         when = seq(min(x$initiation_date), min(x$initiation_date) + lubridate::days(27), by = "day"))
                                              }), 
                                      visits_relapse_initiated) |>
    group_by(who) |>
    fill(c("initiation_date", "rand_dt", "project", "medicine", "relapse_date"), # these variables do not change across visits
         .direction = "down") |>
    arrange(who, when) |>
    mutate(day_of_intervention = ifelse(is.na(day_of_intervention), as.numeric(when - rand_dt + 1), day_of_intervention), # days from randomization
           week_of_intervention = ifelse(is.na(week_of_intervention), ceiling(day_of_intervention / 7), week_of_intervention), # weeks from randomization 
           days_since_initiation = ifelse(is.na(days_since_initiation), as.numeric(when - initiation_date + 1), days_since_initiation), # days from initiation
           weeks_since_initiation = ifelse(is.na(weeks_since_initiation), ceiling(days_since_initiation / 7), weeks_since_initiation) # weeks from initiation 
    )

visits_relapse_initiated |> nrow() # 48944 rows, 1748 patients * 28 days

# Update missing values based on specific conditions for relapse
visits_relapse_initiated <- visits_relapse_initiated |>
    group_by(who) |>
    mutate(relapse = case_when(
        is.na(relapse) & lag(relapse) == 1 ~ 1, # if previously relapsed, still considered relapse
        is.na(relapse) & relapse_date <= when ~ 1,
        is.na(relapse) & relapse_date > when ~ 0,
        TRUE ~ relapse
    ))

# create weekly (from treatment initiation) relapse variable
visits_relapse_initiated <- visits_relapse_initiated |>
    group_by(who, weeks_since_initiation) |>
    mutate(relapse_this_week = as.numeric(any(relapse)))

# imputing missing (and 0 doses)
visits_relapse_initiated_imputed <- visits_relapse_initiated |>
    mutate(indicator_0_dose = ifelse(mg == 0, 1, 0), # making 0 dose and missing dose indicators to confirm cleaning
           indicator_missing_dose = ifelse(is.na(mg), 1, 0)) |>
    mutate(bup_dose = if_else(medicine == "bup" & bup_dose == 0, as.numeric(NA), bup_dose), # turning 0 into NA to make filling easier
           met_dose = if_else(medicine == "met" & met_dose == 0, as.numeric(NA), met_dose), # turning 0 into NA to make filling easier
           naltrexone_dose = if_else(medicine == "nal" & naltrexone_dose == 0, as.numeric(NA), naltrexone_dose)) |> # turning 0 into NA to make filling easier
    group_by(who) |>
    mutate(
        bup_dose = case_when(relapse == 1 & medicine == "bup" ~ bup_dose,
                             relapse == 0 & medicine == "bup" & is.na(bup_dose) ~ zoo::na.locf(bup_dose, na.rm = FALSE), # use previous dose to impute until relapse
                             TRUE ~ bup_dose),
        met_dose = case_when(relapse == 1 & medicine == "met" ~ met_dose,
                             relapse == 0 & medicine == "met" & is.na(met_dose) ~ zoo::na.locf(met_dose, na.rm = FALSE), # use previous dose to impute until relapse
                             TRUE ~ met_dose)
    ) |>
    ungroup()

visits_relapse_initiated_imputed <- visits_relapse_initiated_imputed |>
    group_by(who, weeks_since_initiation) |> 
    mutate(max_dose_this_week = case_when(medicine == "bup" ~ max(bup_dose, na.rm = TRUE),
                                          medicine == "met" ~ max(met_dose, na.rm = TRUE),
                                          medicine == "nal" ~ max(naltrexone_dose, na.rm = TRUE),
                                          TRUE ~ 0
    )) |> 
    ungroup()

visits_relapse_initiated_imputed <- visits_relapse_initiated_imputed |>
    mutate(max_dose_this_week = ifelse(max_dose_this_week == -Inf, as.numeric(NA), max_dose_this_week)) # setting -Inf to NA, shouldn't affect anyone who hasn't relapsed

visits_relapse_initiated_imputed |> 
    filter(medicine == "bup" | medicine == "met") |>
    filter(relapse == 0, is.na(max_dose_this_week)) |> nrow() # making sure that no one who hasn't yet relapsed is missing a dose

visits_relapse_initiated_imputed |> 
    filter(medicine == "bup" | medicine == "met") |>
    filter(relapse == 0, max_dose_this_week == 0) |> nrow() 

#saveRDS(visits_relapse_initiated_imputed, here::here("data/ctn94_nonimpute/race_dose/visits_relapse_post_initiated_imputed.rds"))

visits_relapse_initiated_imputed_distinct <- visits_relapse_initiated_imputed |> 
    distinct(who, weeks_since_initiation, .keep_all = TRUE)

# Create weekly relapse variables in wide format -------------------------------

visits_relapse_initiated_imputed_distinct <- group_by(visits_relapse_initiated_imputed_distinct, who, weeks_since_initiation) |> 
    mutate(dose_this_week = max_dose_this_week) |> 
    ungroup() |> 
    select(who, medicine, project, rand_dt, initiation_date, weeks_since_initiation, dose_this_week, relapse_this_week)

visits_relapse_initiated_imputed_distinct |> filter()

# Long to wide format
relapse_wide <- pivot_wider(visits_relapse_initiated_imputed_distinct, 
                            names_from = weeks_since_initiation, 
                            names_glue = "wk{weeks_since_initiation}.{.value}", 
                            values_from = c(
                                dose_this_week, 
                                relapse_this_week
                            ), values_fill = NA)


# Read in patient-level dataset created in ctn0094_02_relapse.R
patients <- readRDS(here::here("data/ctn94_nonimpute/clean_patients_with_relapse.rds"))

final_wide <- patients |>
    select(-c(trt, rand_dt, end_of_detox, medicine, switched_meds, med_switch_date, never_initiated,
              relapse_overall, relapse_date, opioiduse12, fusedt12, opioiduse24, fusedt24, project)) |>
    right_join(relapse_wide, by = c("who" = "who"))

# checking that no one who hasn't relapsed isn't missing a dose (should all be 0)
final_wide |> filter(wk2.relapse_this_week == 0, is.na(wk2.dose_this_week)) |> nrow() # n = 0
final_wide |> filter(wk3.relapse_this_week == 0, is.na(wk3.dose_this_week)) |> nrow() # n = 0
final_wide |> filter(wk4.relapse_this_week == 0, is.na(wk4.dose_this_week)) |> nrow() # n = 0

# Save
saveRDS(final_wide, here::here("data/ctn94_nonimpute/race_dose/final_analysis_data
                               .rds"))
