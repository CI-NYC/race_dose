# READ ME ----------------------------------------------------------------------
#
#       Author: Shodai Inose
# Last updated: 1 Aug 2024
#
#         Note: Modified code originally written by Nick in here:
#               https://github.com/CI-NYC/OUD-dynamic-dosing/blob/main/scripts/02_defining_relapse.R
#               + Modified code written by Sarah Forrest
#
# ------------------------------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)

source(here::here("scripts/source/covariates.R"))

# Read in visits dataset created in ctn0094_01_clean.R
visits <- readRDS(here::here("data/ctn94_nonimpute/visits.rds"))

# Create variables -------------------------------------------------------------

# Create variables `in_detox` and `use_today` (visit-level)
# `use_today` criteria:
# a positive urine test
# OR selfopioid is yes
# OR missing urine and self-report
visits_relapse <- mutate(visits,
                         in_detox = when < end_of_detox,
                         use_today = 
                             case_when((!is.na(uopioid) & uopioid == 1) ~ TRUE, 
                                       (!is.na(selfopioid) & selfopioid == 1) ~ TRUE, 
                                       (is.na(uopioid) & is.na(selfopioid)) ~ TRUE,
                                       ((!is.na(uopioid) & uopioid == 0) & 
                                            (!is.na(selfopioid) & selfopioid == 0)) ~ FALSE,
                                       (is.na(selfopioid) & uopioid == 0) ~ FALSE, 
                                       (is.na(uopioid) & selfopioid == 0) ~ FALSE, 
                                       TRUE ~ NA))

#Create variable `use_this_week` (week-level)
visits_relapse <- group_by(visits_relapse, who, week_of_intervention) |> 
    mutate(use_this_week = any(use_today)) |> 
    ungroup()

# Create variables `days_of_use`, `weeks_of_use`, `last_observed`, and `relapse_overall` (patient-level)
# Days of use defined as number of consecutive days of opioid use without being in detox
# Relapse defined as occurring on the last day of 7 days of daily use of non-study opioids or on the first day of the fourth week of at-least-once-weekly use
visits_relapse <- group_by(visits_relapse, who) |> 
    mutate(days_of_use = sequence(rle(use_today & !in_detox)$lengths) * use_today * !in_detox, 
           last_observed = max(when)) |> # last date a patient was observed in the study
    ungroup() |> 
    left_join({
        group_by(visits_relapse, who, week_of_intervention) |> 
            summarize(use_this_week = any(use_this_week), 
                      in_detox = all(in_detox)) |> 
            mutate(weeks_of_use = sequence(rle(use_this_week & !in_detox)$lengths) * use_this_week * !in_detox) |> 
            ungroup()
    }) |> 
    group_by(who) |> 
    mutate(relapse_overall = any(days_of_use == 7 | weeks_of_use == 4)) |> 
    ungroup()

# Calculate relapse dates ------------------------------------------------------

# Extract the last day of 7 consecutive days of opioid use for each patient (if applicable)
who_7_day <- split(visits_relapse, visits_relapse$who) |> 
    map_dfr(function(data) {
        out = with(data, when[detect_index(days_of_use == 7, \(x) x)])
        if (length(out) == 0) out = NA_Date_
        tibble(relapse_date_7day = out)
    }, .id = "who")

# Extract the first day of the fourth week of use of opioid use for each patient (if applicable)
who_4_week <- split(visits_relapse, visits_relapse$who) |> 
    map_dfr(function(data) {
        out = with(data, when[detect_index(weeks_of_use == 4 & lag(weeks_of_use, 1) == 3, \(x) x)])
        if (length(out) == 0) out = NA_Date_
        tibble(relapse_date_4week = out)
    }, .id = "who")

# Combine the 7-day and 4-week relapse dates, along with patient information
relapse_dates <- who_7_day |> 
    left_join(who_4_week) |> 
    left_join(unique(select(visits_relapse, who, project, opioiduse12, fusedt12, opioiduse24, fusedt24))) |> 
    mutate(relapse_date = case_when( # Determine the final relapse date based on specific conditions
        is.na(relapse_date_7day) & is.na(relapse_date_4week) & project == 30 & !is.na(opioiduse12) & opioiduse12 == 1 ~ fusedt12,
        is.na(relapse_date_7day) & is.na(relapse_date_4week) & (project == 27 | project == 51) & !is.na(opioiduse24) & opioiduse24 == 1 ~ fusedt24,
        is.na(relapse_date_7day) ~ relapse_date_4week,
        is.na(relapse_date_4week) ~ relapse_date_7day,
        TRUE ~ pmin(relapse_date_7day, relapse_date_4week)
    ))

# Further processing of relapse dates based on project-specific criteria
relapse_dates <- relapse_dates |>
    mutate(
        relapse_date = case_when(
            project == 30 & !is.na(opioiduse12) & opioiduse12 == 1 ~ # 12 week `fusedt12` used for CTN 30
                if_else(between(abs(as.numeric(relapse_date - fusedt12)), 1, 28), relapse_date, fusedt12),
            project %in% c(27, 51) & !is.na(opioiduse24) & opioiduse24 == 1 ~ # 24 week `fusedt24` used for CTN 27 and 51
                if_else(between(abs(as.numeric(relapse_date - fusedt24)), 1, 28), relapse_date, fusedt24),
            TRUE ~ relapse_date
        )
    )

# Create patient-level dataset with relapse dates
dates <- select(visits_relapse, who, rand_dt, last_observed, end_of_detox) |> 
    distinct() |> 
    left_join(relapse_dates)

# Adjust `relapse_date` or use a fallback date based on project-specific criteria
dates <- dates |>
    mutate(relapse_date = if_else(
        is.na(relapse_date) & last_observed <= rand_dt + 
            ifelse(project %in% c(27, 51), weeks(23), weeks(11)),
        pmax(end_of_detox, last_observed + 1),
        relapse_date
    ))

# Save
#saveRDS(dates, "data/ctn0094/drv/patient_dates.rds")
#write.csv(dates, "data/ctn0094/drv/patient_dates.csv")


# Create visit-level dataset with relapse --------------------------------------

# Create `relapse` variable and adjust `relapse_overall` if they have a relapse date assigned
visits_relapse <- visits_relapse |>
    left_join(select(dates, who, relapse_date)) |>
    group_by(who, week_of_intervention) |>
    mutate(
        relapse = as.numeric(relapse_date <= when), # relapse on or after relapse date
        relapse = ifelse(is.na(relapse_date), 0, relapse)
    ) |>
    mutate(relapse_overall = ifelse(!is.na(relapse_date), TRUE, FALSE)) |>
    ungroup()

# Save
saveRDS(visits_relapse, here::here("data/ctn94_nonimpute/clean_visits_with_relapse_nonimpute.rds"))


# Create patient-level dataset with relapse ------------------------------------

patients <- select(visits_relapse, who, any_of(c(demog, comorbidities, treatment_info, outcomes))) |> 
    distinct(who, .keep_all = TRUE)

# Save
saveRDS(patients, here::here("data/ctn94_nonimpute/clean_patients_with_relapse.rds"))


# Create week-level dataset with relapse ---------------------------------------

weeks <- select(visits_relapse, 
                who, any_of(c(demog, comorbidities, treatment_info, outcomes, weekly_indicators))) |> 
    distinct(who, week_of_intervention, .keep_all = TRUE)

# Save
saveRDS(weeks, here::here("data/ctn94_nonimpute/clean_weeks_with_relapse.rds"))
