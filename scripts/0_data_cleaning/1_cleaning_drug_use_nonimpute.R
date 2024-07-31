# READ ME ----------------------------------------------------------------------
#
#       Author: Shodai Inose
# Last updated: 26 Jul 2024
#
#         Note: Modified code originally written by Nick in here:
#               https://github.com/CI-NYC/OUD-dynamic-dosing
#               Mofidied code written by Sarah in here:
#               https://github.com/CI-NYC/harmonize_0027_0030_0051/blob/main/scripts/ctn0094_01_clean.R 
#
# ------------------------------------------------------------------------------

# Set up -----------------------------------------------------------------------

# Load packages
library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)
library(data.table)

# Read in raw datasets
raw_drug_use <- read_csv(here::here("raw_data/druguse_06062021.csv")) # N = 298,889 total visits (1 row per visit)
drug_use <- raw_drug_use # Make a copy
raw_baseline <- read_csv(here::here("raw_data/baselinectn94.csv")) # N = 2,199 patients (1 row per patient)

# Set specific date formatting for date variables
drug_use <- mutate(drug_use, 
                   across(c("fusedt12", "fusedt24", "when"), 
                          \(x) as.Date(x, format = "%m/%d/%y")), 
                   rand_dt = as.Date(rand_dt, origin = "1960-01-01"))

# Manipulate baseline variable dataset
baseline <- raw_baseline |>
    select(who, bamphetamine30_base, bcannabis30_base, bbenzo30_base) # Retain ID and baseline drug use variables only


# Merge datasets ---------------------------------------------------------------

to_remove <- c("raceg", "x2race", "x3race", "isHispanic", "i")
not_factors <- c("who", "fusedt12", "fusedt24", "when", "rand_dt", "mg", "age", "hcows")

# Create visits dataset
visits <- select(drug_use, -all_of(to_remove)) |> # Remove race/eth variable
    select(-bamphetamine30_base, -bcannabis30_base, -bbenzo30_base) |> # Remove the baseline drug use variables
    left_join(baseline, by = "who") # Join to baseline dataset


# Replace missing values with NAs ----------------------------------------------

# Find all variables with values of -8 or -9 to replace with NAs
check_variables_with_minus_8_9 <- lapply(visits, function(column) any(column %in% c(-8, -9)))
filtered_variables_with_minus_8_9 <- names(check_variables_with_minus_8_9)[unlist(check_variables_with_minus_8_9)]
to_replace <- filtered_variables_with_minus_8_9

# Replace -8 and -9 with NA for the specified variables
visits <- visits |>
    mutate(across(all_of(to_replace), ~ ifelse(. %in% c(-8, -9), NA, .)))


# Mutate variables -------------------------------------------------------------

# Create `medicine_assigned` variable from `trt` the assigned treatment arm 
# 1= ctn27Methadone, 2=ctn27outpatientBUP, 3=ctn30outpatientBUP, 4=ctn51inpatientBup, 5=ctn51NTX
visits <- visits |>
    mutate(medicine_assigned = case_when(
        trt == 1 ~ 1, # met
        trt %in% c(2, 3, 4) ~ 2, # bup
        trt == 5 ~ 3, # nal
        TRUE ~ NA
    ))

# Convert ordered variables to factors
visits <- visits |>
    mutate(across(!all_of(not_factors), factor)) |>
    droplevels()

binary <- c("opioiduse12", "opioiduse24", "alcdisorder", "cocdisorder", "hasBrainDamage", "hasEpilepsy", "hasSchiz", "hasBipolar", "hasAnxPan", "hasMajorDep", "falcohol", "fdrug", "bamphetamine30_base", "bcannabis30_base", "bbenzo30_base", "ivdrug", "selfopioid", "uopioid")

visits <- mutate(visits, 
                 across(all_of(binary), \(x) as.numeric(as.character(x))), 
                 project = fct_relevel(project, c("27", "30", "51")), 
                 sex = fct_recode(sex, "male" = "1", "female" = "2"),
                 xrace = fct_relevel(xrace, c("1", "2", "3", "4")), 
                 trt = fct_relevel(trt, c("1", "2", "3", "4", "5")),
                 edu = fct_relevel(edu, c("1", "2", "3")),
                 mar = fct_relevel(mar, c("1", "2", "3")), 
                 medicine = fct_recode(medicine, "met" = "1", "bup" = "2", "nal" = "3"),
                 medicine_assigned = fct_recode(medicine_assigned, "met" = "1", "bup" = "2", "nal" = "3"))

# Starting `never_initiated` indicator -- to be continued further in script
visits <- mutate(visits, 
                 never_initiated = is.na(when), # TRUE if when = NA
                 when = as_date(if_else(never_initiated, ymd(rand_dt), ymd(when)))) 

### As of now... 
# N = 9 patients in CTN 27 never initiated
# N = 9 patients in CTN 51 never initiated


# Add rows for missing visits --------------------------------------------------

# Identify missing visits in consecutive date sequence
visits <- left_join(map_dfr(split(visits, visits$who),
                            function(x) {
                                tibble(who = x$who[1],
                                       when = seq(min(x$when), max(x$when), by = "day"))
                            }), 
                    visits)
# 111 rows added (298889 --> 299000 visits)

# Fill in values in missing rows with the last non-missing values for that patient
# The new rows are repeats of whatever `medicine` were in the previous visit
# `selfopioid` and `uopioid` are akk set to NA regardless of previous values
visits <- group_by(visits, who) |> 
    mutate(across(!c("selfopioid", "uopioid", "mg"), ~zoo::na.locf(.x, na.rm = FALSE))) |> # don't impute mg variable
    ungroup()

# finding people where starting date doesn't match randomization date

visits |>
    group_by(who) |>
    summarize(min_date = min(when)) |>
    left_join(visits |> select(who, rand_dt)) |>
    mutate(diff_starting_date = ifelse(rand_dt != min_date, 1, 0)) |>
    filter(diff_starting_date == 1) |>
    distinct() 

# Adjust dates for 3 specific patient IDs ---------------------------------------

# Push dates back by one for the 3 patients who seem to be shifted according to their randomization dates -- discussed
visits <- mutate(visits, 
                 when = case_when(
                     who %in% c("0403-06-1059", "0403-06-1060", "0605-06-2005") ~ when - 1, 
                     TRUE ~ when
                 ))

# Create/mutate variables ------------------------------------------------------

# `never_initiated` signaling patients who never had `mg` recorded (including `mg` always 0)
# The majority of these patients also never had `medicine` recorded
visits <- group_by(visits, who) |> 
    mutate(never_initiated = if_else(sum(mg == 0 | is.na(mg)) == n(), 
                                     TRUE, never_initiated[1])) |> 
    ungroup()

#visits |> filter(never_initiated == TRUE)  |> select(who, project) |> distinct() |> group_by(project) |> summarize(count = n())# never_initiated_df |> distinct(who)
# N = 22 patients in CTN27 did not initiate, N = 96 patients in CTN51 did not initiate
# N = 118 patients never initiated treatment


# Address patients who swapped medicines ---------------------------------------
# Note: all patients who swapped medicines are in CTN 27

# Swap `medicine` entries where `mg` is always NA or 0 for one medicine, likely in error
# Medicine is reset to the 
visits <- group_by(visits, who) |> 
    mutate(double_meds = n_distinct(medicine, na.rm = TRUE) > 1,
           num_med_1 = sum(medicine == "met", na.rm = TRUE),
           num_med_2 = sum(medicine == "bup", na.rm = TRUE),
           num_med_1_blank = sum(medicine == "met" & !is.na(medicine) & (is.na(mg) | mg == 0)),
           num_med_2_blank = sum(medicine == "bup" & !is.na(medicine) & (is.na(mg) | mg == 0))) |> 
    ungroup() |> 
    mutate(medicine = as.factor(
        case_when(
            medicine == "met" & double_meds & (num_med_1 == num_med_1_blank) ~ "bup",
            medicine == "bup" & double_meds & (num_med_2 == num_med_2_blank) ~ "met",
            TRUE ~ as.character(medicine)
        ))) |> 
    select(-num_med_1, -num_med_2, -num_med_1_blank, -num_med_2_blank, -double_meds)
# N = 14 patients switched medicines at some point in the raw data
# N = 12 patients had `medicine` entries swapped due to likely error
# N = 2 patients seem to have legitimately swapped medicines
# 0401-03-1174 (day 147) and 0502-01-1075 (day 42)

# Create `switched medicines` indicator
visits <- group_by(visits, who) |> 
    mutate(switched_meds = n_distinct(medicine, na.rm = TRUE) > 1,
           med_switch_date = when[which(medicine != lag(medicine, default = first(medicine)))[1]]) |>
    ungroup()


# Create dose variables for each medicine --------------------------------------

# Set dosing floor and ceiling for bup (should go from 2 to 32, by 2s)
visits <- mutate(visits, 
                 bup_dose = if_else(medicine == "bup", mg, NA_real_), 
                 bup_dose = if_else(bup_dose > 32, 32, bup_dose), 
                 bup_dose = if_else(bup_dose == 1, 2, bup_dose),
                 bup_dose = floor(bup_dose / 2) * 2,               
                 met_dose = if_else(medicine == "met", mg, NA_real_),
                 naltrexone_dose = if_else(medicine == "nal", mg, NA_real_))

# find treatment initiation date (first day of non NA or 0 dose)
initiation <- visits |>
    group_by(who) |>
    filter(!is.na(mg) & mg != 0) |>
    slice_min(when) |>
    select(who, initiation_date = when)

visits <- visits |>
    left_join(initiation)

visits |> filter(is.na(initiation_date), never_initiated == FALSE) |> nrow() # check to make sure no one who initiated is missing an initiation date

# `end_of_detox` (patient-level) signaling 2 weeks after their first visit, when the detoxification stage is over
# day and week of intervention (visit-level) counting up from the patient's first visit
visits <- group_by(visits, who) |> 
    mutate(end_of_detox = min(when) + 14, 
           day_of_intervention = as.numeric(when - min(when) + 1), 
           week_of_intervention = ceiling(day_of_intervention / 7))

saveRDS(visits, here::here("data/ctn94_nonimpute/visits.rds"))
