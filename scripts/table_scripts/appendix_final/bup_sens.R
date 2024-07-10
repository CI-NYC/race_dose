library(tidyverse)
library(xtable)

bup_cont_total_trial_indicator <- readRDS(here::here("data/table_data/appendix/bup_cont_total_trial_indicator.rds"))
bup_cont_pair_trial_indicator <- readRDS(here::here("data/table_data/appendix/bup_cont_pair_trial_indicator.rds"))

bup_cont_total_drop30 <- readRDS(here::here("data/table_data/appendix/bup_cont_total_drop30.rds"))
bup_cont_pair_drop30 <- readRDS(here::here("data/table_data/appendix/bup_cont_pair_drop30.rds"))

bup_cont_total_stratify <- readRDS(here::here("data/table_data/appendix/theta_df_bup.rds"))
bup_cont_pair_stratify <- readRDS(here::here("data/table_data/appendix/diff_df_bup.rds")) |>
    arrange(trial)


bup_table_appendix <- data.frame(Mean = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                          Mean = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                          Mean = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00)
)

bup_cont_total_as_trial_indicator <- bup_cont_total_trial_indicator |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, week)

bup_cont_pair_as_trial_indicator <- bup_cont_pair_trial_indicator |>
    select(time, race, type, theta, conf.low, conf.high) |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, time)

bup_cont_total_all_trial_indicator <- bup_cont_total_trial_indicator |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, week)

bup_cont_pair_all_trial_indicator <- bup_cont_pair_trial_indicator |>
    select(time, race, type, theta, conf.low, conf.high) |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, time)

bup_cont_total_as_drop30 <- bup_cont_total_drop30 |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, week)

bup_cont_pair_as_drop30 <- bup_cont_pair_drop30 |>
    select(time, race, type, theta, conf.low, conf.high) |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, time)

bup_cont_total_all_drop30 <- bup_cont_total_drop30 |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, week)

bup_cont_pair_all_drop30 <- bup_cont_pair_drop30 |>
    select(time, race, type, theta, conf.low, conf.high) |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, time)

bup_table_appendix[1, 1] <- paste0(round(bup_cont_total_stratify[1, 4], 1), 
                            " (", round(bup_cont_total_stratify[1, 6], 1), ", ",
                            round(bup_cont_total_stratify[1, 7], 1), ")")

bup_table_appendix[2, 1] <- paste0(round(bup_cont_total_stratify[2, 4], 1), 
                            " (", round(bup_cont_total_stratify[2, 6], 1), ", ",
                            round(bup_cont_total_stratify[2, 7], 1), ")")

bup_table_appendix[3, 1] <- paste0(round(bup_cont_total_stratify[3, 4], 1), 
                          " (", round(bup_cont_total_stratify[3, 6], 1), ", ",
                          round(bup_cont_total_stratify[3, 7], 1), ")")

bup_table_appendix[4, 1] <- paste0(round(bup_cont_total_as_trial_indicator[9, 3], 1), 
                            " (", round(bup_cont_total_as_trial_indicator[9, 5], 1), ", ",
                            round(bup_cont_total_as_trial_indicator[9, 6], 1), ")")

bup_table_appendix[5, 1] <- paste0(round(bup_cont_total_as_trial_indicator[10, 3], 1), 
                            " (", round(bup_cont_total_as_trial_indicator[10, 5], 1), ", ",
                            round(bup_cont_total_as_trial_indicator[10, 6], 1), ")")

bup_table_appendix[6, 1] <- paste0(round(bup_cont_total_as_trial_indicator[11, 3], 1), 
                            " (", round(bup_cont_total_as_trial_indicator[11, 5], 1), ", ",
                            round(bup_cont_total_as_trial_indicator[11, 6], 1), ")")

bup_table_appendix[7, 1] <- paste0(round(bup_cont_total_as_trial_indicator[12, 3], 1), 
                            " (", round(bup_cont_total_as_trial_indicator[12, 5], 1), ", ",
                            round(bup_cont_total_as_trial_indicator[12, 6], 1), ")")

bup_table_appendix[8, 1] <- paste0(round(bup_cont_total_all_trial_indicator[9, 3], 1), 
                           " (", round(bup_cont_total_all_trial_indicator[9, 5], 1), ", ",
                           round(bup_cont_total_all_trial_indicator[9, 6], 1), ")")

bup_table_appendix[9, 1] <- paste0(round(bup_cont_total_all_trial_indicator[10, 3], 1), 
                           " (", round(bup_cont_total_all_trial_indicator[10, 5], 1), ", ",
                           round(bup_cont_total_all_trial_indicator[10, 6], 1), ")")

bup_table_appendix[10, 1] <- paste0(round(bup_cont_total_all_trial_indicator[11, 3], 1), 
                            " (", round(bup_cont_total_all_trial_indicator[11, 5], 1), ", ",
                            round(bup_cont_total_all_trial_indicator[11, 6], 1), ")")

bup_table_appendix[11, 1] <- paste0(round(bup_cont_total_all_trial_indicator[12, 3], 1), 
                            " (", round(bup_cont_total_all_trial_indicator[12, 5], 1), ", ",
                            round(bup_cont_total_all_trial_indicator[12, 6], 1), ")")

bup_table_appendix[12, 1] <- paste0(round(bup_cont_total_as_drop30[9, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[9, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[9, 6], 1), ")")

bup_table_appendix[13, 1] <- paste0(round(bup_cont_total_as_drop30[10, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[10, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[10, 6], 1), ")")

bup_table_appendix[14, 1] <- paste0(round(bup_cont_total_as_drop30[11, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[11, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[11, 6], 1), ")")

bup_table_appendix[15, 1] <- paste0(round(bup_cont_total_as_drop30[12, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[12, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[12, 6], 1), ")")

bup_table_appendix[16, 1] <- paste0(round(bup_cont_total_all_drop30[9, 3], 1), 
                           " (", round(bup_cont_total_all_drop30[9, 5], 1), ", ",
                           round(bup_cont_total_all_drop30[9, 6], 1), ")")

bup_table_appendix[17, 1] <- paste0(round(bup_cont_total_all_drop30[10, 3], 1), 
                           " (", round(bup_cont_total_all_drop30[10, 5], 1), ", ",
                           round(bup_cont_total_all_drop30[10, 6], 1), ")")

bup_table_appendix[18, 1] <- paste0(round(bup_cont_total_all_drop30[11, 3], 1), 
                           " (", round(bup_cont_total_all_drop30[11, 5], 1), ", ",
                           round(bup_cont_total_all_drop30[11, 6], 1), ")")

bup_table_appendix[19, 1] <- paste0(round(bup_cont_total_all_drop30[12, 3], 1), 
                           " (", round(bup_cont_total_all_drop30[12, 5], 1), ", ",
                           round(bup_cont_total_all_drop30[12, 6], 1), ")")


# COLUMN 2

bup_table_appendix[1, 2] <- paste0(round(bup_cont_total_stratify[4, 4], 1), 
                                   " (", round(bup_cont_total_stratify[4, 6], 1), ", ",
                                   round(bup_cont_total_stratify[4, 7], 1), ")")

bup_table_appendix[2, 2] <- paste0(round(bup_cont_total_stratify[5, 4], 1), 
                                   " (", round(bup_cont_total_stratify[5, 6], 1), ", ",
                                   round(bup_cont_total_stratify[5, 7], 1), ")")

bup_table_appendix[3, 2] <- paste0(round(bup_cont_total_stratify[6, 4], 1), 
                                   " (", round(bup_cont_total_stratify[6, 6], 1), ", ",
                                   round(bup_cont_total_stratify[6, 7], 1), ")")

bup_table_appendix[4, 2] <- paste0(round(bup_cont_total_as_trial_indicator[1, 3], 1), 
                          " (", round(bup_cont_total_as_trial_indicator[1, 5], 1), ", ",
                          round(bup_cont_total_as_trial_indicator[1, 6], 1), ")")

bup_table_appendix[5, 2] <- paste0(round(bup_cont_total_as_trial_indicator[2, 3], 1), 
                          " (", round(bup_cont_total_as_trial_indicator[2, 5], 1), ", ",
                          round(bup_cont_total_as_trial_indicator[2, 6], 1), ")")

bup_table_appendix[6, 2] <- paste0(round(bup_cont_total_as_trial_indicator[3, 3], 1), 
                          " (", round(bup_cont_total_as_trial_indicator[3, 5], 1), ", ",
                          round(bup_cont_total_as_trial_indicator[3, 6], 1), ")")

bup_table_appendix[7, 2] <- paste0(round(bup_cont_total_as_trial_indicator[4, 3], 1), 
                          " (", round(bup_cont_total_as_trial_indicator[4, 5], 1), ", ",
                          round(bup_cont_total_as_trial_indicator[4, 6], 1), ")")

bup_table_appendix[8, 2] <- paste0(round(bup_cont_total_all_trial_indicator[1, 3], 1), 
                          " (", round(bup_cont_total_all_trial_indicator[1, 5], 1), ", ",
                          round(bup_cont_total_all_trial_indicator[1, 6], 1), ")")

bup_table_appendix[9, 2] <- paste0(round(bup_cont_total_all_trial_indicator[2, 3], 1), 
                          " (", round(bup_cont_total_all_trial_indicator[2, 5], 1), ", ",
                          round(bup_cont_total_all_trial_indicator[2, 6], 1), ")")

bup_table_appendix[10, 2] <- paste0(round(bup_cont_total_all_trial_indicator[3, 3], 1), 
                           " (", round(bup_cont_total_all_trial_indicator[3, 5], 1), ", ",
                           round(bup_cont_total_all_trial_indicator[3, 6], 1), ")")

bup_table_appendix[11, 2] <- paste0(round(bup_cont_total_all_trial_indicator[4, 3], 1), 
                           " (", round(bup_cont_total_all_trial_indicator[4, 5], 1), ", ",
                           round(bup_cont_total_all_trial_indicator[4, 6], 1), ")")

bup_table_appendix[12, 2] <- paste0(round(bup_cont_total_as_drop30[1, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[1, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[1, 6], 1), ")")

bup_table_appendix[13, 2] <- paste0(round(bup_cont_total_as_drop30[2, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[2, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[2, 6], 1), ")")

bup_table_appendix[14, 2] <- paste0(round(bup_cont_total_as_drop30[3, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[3, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[3, 6], 1), ")")

bup_table_appendix[15, 2] <- paste0(round(bup_cont_total_as_drop30[4, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[4, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[4, 6], 1), ")")

bup_table_appendix[16, 2] <- paste0(round(bup_cont_total_all_drop30[1, 3], 1), 
                           " (", round(bup_cont_total_all_drop30[1, 5], 1), ", ",
                           round(bup_cont_total_all_drop30[1, 6], 1), ")")

bup_table_appendix[17, 2] <- paste0(round(bup_cont_total_all_drop30[2, 3], 1), 
                           " (", round(bup_cont_total_all_drop30[2, 5], 1), ", ",
                           round(bup_cont_total_all_drop30[2, 6], 1), ")")

bup_table_appendix[18, 2] <- paste0(round(bup_cont_total_all_drop30[3, 3], 1), 
                           " (", round(bup_cont_total_all_drop30[3, 5], 1), ", ",
                           round(bup_cont_total_all_drop30[3, 6], 1), ")")

bup_table_appendix[19, 2] <- paste0(round(bup_cont_total_all_drop30[4, 3], 1), 
                           " (", round(bup_cont_total_all_drop30[4, 5], 1), ", ",
                           round(bup_cont_total_all_drop30[4, 6], 1), ")")

# COLUMN 4

bup_table_appendix[1, 4] <- paste0(round(bup_cont_total_stratify[7, 4], 1), 
                                   " (", round(bup_cont_total_stratify[7, 6], 1), ", ",
                                   round(bup_cont_total_stratify[7, 7], 1), ")")

bup_table_appendix[2, 4] <- paste0(round(bup_cont_total_stratify[8, 4], 1), 
                                   " (", round(bup_cont_total_stratify[8, 6], 1), ", ",
                                   round(bup_cont_total_stratify[8, 7], 1), ")")

bup_table_appendix[3, 4] <- paste0(round(bup_cont_total_stratify[9, 4], 1), 
                                   " (", round(bup_cont_total_stratify[9, 6], 1), ", ",
                                   round(bup_cont_total_stratify[9, 7], 1), ")")

bup_table_appendix[4, 4] <- paste0(round(bup_cont_total_as_trial_indicator[5, 3], 1), 
                          " (", round(bup_cont_total_as_trial_indicator[5, 5], 1), ", ",
                          round(bup_cont_total_as_trial_indicator[5, 6], 1), ")")

bup_table_appendix[5, 4] <- paste0(round(bup_cont_total_as_trial_indicator[6, 3], 1), 
                          " (", round(bup_cont_total_as_trial_indicator[6, 5], 1), ", ",
                          round(bup_cont_total_as_trial_indicator[6, 6], 1), ")")

bup_table_appendix[6, 4] <- paste0(round(bup_cont_total_as_trial_indicator[7, 3], 1), 
                          " (", round(bup_cont_total_as_trial_indicator[7, 5], 1), ", ",
                          round(bup_cont_total_as_trial_indicator[7, 6], 1), ")")

bup_table_appendix[7, 4] <- paste0(round(bup_cont_total_as_trial_indicator[8, 3], 1), 
                          " (", round(bup_cont_total_as_trial_indicator[8, 5], 1), ", ",
                          round(bup_cont_total_as_trial_indicator[8, 6], 1), ")")

bup_table_appendix[8, 4] <- paste0(round(bup_cont_total_all_trial_indicator[5, 3], 1), 
                          " (", round(bup_cont_total_all_trial_indicator[5, 5], 1), ", ",
                          round(bup_cont_total_all_trial_indicator[5, 6], 1), ")")

bup_table_appendix[9, 4] <- paste0(round(bup_cont_total_all_trial_indicator[6, 3], 1), 
                          " (", round(bup_cont_total_all_trial_indicator[6, 5], 1), ", ",
                          round(bup_cont_total_all_trial_indicator[6, 6], 1), ")")

bup_table_appendix[10, 4] <- paste0(round(bup_cont_total_all_trial_indicator[7, 3], 1), 
                           " (", round(bup_cont_total_all_trial_indicator[7, 5], 1), ", ",
                           round(bup_cont_total_all_trial_indicator[7, 6], 1), ")")

bup_table_appendix[11, 4] <- paste0(round(bup_cont_total_all_trial_indicator[8, 3], 1), 
                           " (", round(bup_cont_total_all_trial_indicator[8, 5], 1), ", ",
                           round(bup_cont_total_all_trial_indicator[8, 6], 1), ")")

bup_table_appendix[12, 4] <- paste0(round(bup_cont_total_as_drop30[5, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[5, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[5, 6], 1), ")")

bup_table_appendix[13, 4] <- paste0(round(bup_cont_total_as_drop30[6, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[6, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[6, 6], 1), ")")

bup_table_appendix[14, 4] <- paste0(round(bup_cont_total_as_drop30[7, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[7, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[7, 6], 1), ")")

bup_table_appendix[15, 4] <- paste0(round(bup_cont_total_as_drop30[8, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[8, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[8, 6], 1), ")")

bup_table_appendix[16, 4] <- paste0(round(bup_cont_total_all_drop30[5, 3], 1), 
                           " (", round(bup_cont_total_all_drop30[5, 5], 1), ", ",
                           round(bup_cont_total_all_drop30[5, 6], 1), ")")

bup_table_appendix[17, 4] <- paste0(round(bup_cont_total_all_drop30[6, 3], 1), 
                           " (", round(bup_cont_total_all_drop30[6, 5], 1), ", ",
                           round(bup_cont_total_all_drop30[6, 6], 1), ")")

bup_table_appendix[18, 4] <- paste0(round(bup_cont_total_all_drop30[7, 3], 1), 
                           " (", round(bup_cont_total_all_drop30[7, 5], 1), ", ",
                           round(bup_cont_total_all_drop30[7, 6], 1), ")")

bup_table_appendix[19, 4] <- paste0(round(bup_cont_total_all_drop30[8, 3], 1), 
                           " (", round(bup_cont_total_all_drop30[8, 5], 1), ", ",
                           round(bup_cont_total_all_drop30[8, 6], 1), ")")

# COLUMN 3
bup_table_appendix[1, 3] <- paste0(round(bup_cont_pair_stratify[1, 1], 1), 
                            " (", round(bup_cont_pair_stratify[1, 5], 1), ", ",
                            round(bup_cont_pair_stratify[1, 6], 1), ")")

bup_table_appendix[2, 3] <- paste0(round(bup_cont_pair_stratify[3, 1], 1), 
                            " (", round(bup_cont_pair_stratify[3, 5], 1), ", ",
                            round(bup_cont_pair_stratify[3, 6], 1), ")")

bup_table_appendix[3, 3] <- paste0(round(bup_cont_pair_stratify[5, 1], 1), 
                            " (", round(bup_cont_pair_stratify[5, 5], 1), ", ",
                            round(bup_cont_pair_stratify[5, 6], 1), ")")

bup_table_appendix[4, 3] <- paste0(round(bup_cont_pair_as_trial_indicator[1, 4], 1), 
                          " (", round(bup_cont_pair_as_trial_indicator[1, 5], 1), ", ",
                          round(bup_cont_pair_as_trial_indicator[1, 6], 1), ")")

bup_table_appendix[5, 3] <- paste0(round(bup_cont_pair_as_trial_indicator[2, 4], 1), 
                          " (", round(bup_cont_pair_as_trial_indicator[2, 5], 1), ", ",
                          round(bup_cont_pair_as_trial_indicator[2, 6], 1), ")")

bup_table_appendix[6, 3] <- paste0(round(bup_cont_pair_as_trial_indicator[3, 4], 1), 
                          " (", round(bup_cont_pair_as_trial_indicator[3, 5], 1), ", ",
                          round(bup_cont_pair_as_trial_indicator[3, 6], 1), ")")

bup_table_appendix[7, 3] <- paste0(round(bup_cont_pair_as_trial_indicator[4, 4], 1), 
                          " (", round(bup_cont_pair_as_trial_indicator[4, 5], 1), ", ",
                          round(bup_cont_pair_as_trial_indicator[4, 6], 1), ")")

bup_table_appendix[8, 3] <- paste0(round(bup_cont_pair_all_trial_indicator[1, 4], 1), 
                          " (", round(bup_cont_pair_all_trial_indicator[1, 5], 1), ", ",
                          round(bup_cont_pair_all_trial_indicator[1, 6], 1), ")")

bup_table_appendix[9, 3] <- paste0(round(bup_cont_pair_all_trial_indicator[2, 4], 1), 
                          " (", round(bup_cont_pair_all_trial_indicator[2, 5], 1), ", ",
                          round(bup_cont_pair_all_trial_indicator[2, 6], 1), ")")

bup_table_appendix[10, 3] <- paste0(round(bup_cont_pair_all_trial_indicator[3, 4], 1), 
                           " (", round(bup_cont_pair_all_trial_indicator[3, 5], 1), ", ",
                           round(bup_cont_pair_all_trial_indicator[3, 6], 1), ")")

bup_table_appendix[11, 3] <- paste0(round(bup_cont_pair_all_trial_indicator[4, 4], 1), 
                           " (", round(bup_cont_pair_all_trial_indicator[4, 5], 1), ", ",
                           round(bup_cont_pair_all_trial_indicator[4, 6], 1), ")")

bup_table_appendix[12, 3] <- paste0(round(bup_cont_pair_as_drop30[1, 4], 1), 
                           " (", round(bup_cont_pair_as_drop30[1, 5], 1), ", ",
                           round(bup_cont_pair_as_drop30[1, 6], 1), ")")

bup_table_appendix[13, 3] <- paste0(round(bup_cont_pair_as_drop30[2, 4], 1), 
                           " (", round(bup_cont_pair_as_drop30[2, 5], 1), ", ",
                           round(bup_cont_pair_as_drop30[2, 6], 1), ")")

bup_table_appendix[14, 3] <- paste0(round(bup_cont_pair_as_drop30[3, 4], 1), 
                           " (", round(bup_cont_pair_as_drop30[3, 5], 1), ", ",
                           round(bup_cont_pair_as_drop30[3, 6], 1), ")")

bup_table_appendix[15, 3] <- paste0(round(bup_cont_pair_as_drop30[4, 4], 1), 
                           " (", round(bup_cont_pair_as_drop30[4, 5], 1), ", ",
                           round(bup_cont_pair_as_drop30[4, 6], 1), ")")

bup_table_appendix[16, 3] <- paste0(round(bup_cont_pair_all_drop30[1, 4], 1), 
                           " (", round(bup_cont_pair_all_drop30[1, 5], 1), ", ",
                           round(bup_cont_pair_all_drop30[1, 6], 1), ")")

bup_table_appendix[17, 3] <- paste0(round(bup_cont_pair_all_drop30[2, 4], 1), 
                           " (", round(bup_cont_pair_all_drop30[2, 5], 1), ", ",
                           round(bup_cont_pair_all_drop30[2, 6], 1), ")")

bup_table_appendix[18, 3] <- paste0(round(bup_cont_pair_all_drop30[3, 4], 1), 
                           " (", round(bup_cont_pair_all_drop30[3, 5], 1), ", ",
                           round(bup_cont_pair_all_drop30[3, 6], 1), ")")

bup_table_appendix[19, 3] <- paste0(round(bup_cont_pair_all_drop30[4, 4], 1), 
                           " (", round(bup_cont_pair_all_drop30[4, 5], 1), ", ",
                           round(bup_cont_pair_all_drop30[4, 6], 1), ")")

# COLUMN 5

bup_table_appendix[1, 5] <- paste0(round(bup_cont_pair_stratify[2, 1], 1), 
                                   " (", round(bup_cont_pair_stratify[2, 5], 1), ", ",
                                   round(bup_cont_pair_stratify[2, 6], 1), ")")

bup_table_appendix[2, 5] <- paste0(round(bup_cont_pair_stratify[4, 1], 1), 
                                   " (", round(bup_cont_pair_stratify[4, 5], 1), ", ",
                                   round(bup_cont_pair_stratify[4, 6], 1), ")")

bup_table_appendix[3, 5] <- paste0(round(bup_cont_pair_stratify[6, 1], 1), 
                                   " (", round(bup_cont_pair_stratify[6, 5], 1), ", ",
                                   round(bup_cont_pair_stratify[6, 6], 1), ")")

bup_table_appendix[4, 5] <- paste0(round(bup_cont_pair_as_trial_indicator[5, 4], 1), 
                          " (", round(bup_cont_pair_as_trial_indicator[5, 5], 1), ", ",
                          round(bup_cont_pair_as_trial_indicator[5, 6], 1), ")")

bup_table_appendix[5, 5] <- paste0(round(bup_cont_pair_as_trial_indicator[6, 4], 1), 
                          " (", round(bup_cont_pair_as_trial_indicator[6, 5], 1), ", ",
                          round(bup_cont_pair_as_trial_indicator[6, 6], 1), ")")

bup_table_appendix[6, 5] <- paste0(round(bup_cont_pair_as_trial_indicator[7, 4], 1), 
                          " (", round(bup_cont_pair_as_trial_indicator[7, 5], 1), ", ",
                          round(bup_cont_pair_as_trial_indicator[7, 6], 1), ")")

bup_table_appendix[7, 5] <- paste0(round(bup_cont_pair_as_trial_indicator[8, 4], 1), 
                          " (", round(bup_cont_pair_as_trial_indicator[8, 5], 1), ", ",
                          round(bup_cont_pair_as_trial_indicator[8, 6], 1), ")")

bup_table_appendix[8, 5] <- paste0(round(bup_cont_pair_all_trial_indicator[5, 4], 1), 
                          " (", round(bup_cont_pair_all_trial_indicator[5, 5], 1), ", ",
                          round(bup_cont_pair_all_trial_indicator[5, 6], 1), ")")

bup_table_appendix[9, 5] <- paste0(round(bup_cont_pair_all_trial_indicator[6, 4], 1), 
                          " (", round(bup_cont_pair_all_trial_indicator[6, 5], 1), ", ",
                          round(bup_cont_pair_all_trial_indicator[6, 6], 1), ")")

bup_table_appendix[10, 5] <- paste0(round(bup_cont_pair_all_trial_indicator[7, 4], 1), 
                           " (", round(bup_cont_pair_all_trial_indicator[7, 5], 1), ", ",
                           round(bup_cont_pair_all_trial_indicator[7, 6], 1), ")")

bup_table_appendix[11, 5] <- paste0(round(bup_cont_pair_all_trial_indicator[8, 4], 1), 
                           " (", round(bup_cont_pair_all_trial_indicator[8, 5], 1), ", ",
                           round(bup_cont_pair_all_trial_indicator[8, 6], 1), ")")

bup_table_appendix[12, 5] <- paste0(round(bup_cont_pair_as_drop30[5, 4], 1), 
                           " (", round(bup_cont_pair_as_drop30[5, 5], 1), ", ",
                           round(bup_cont_pair_as_drop30[5, 6], 1), ")")

bup_table_appendix[13, 5] <- paste0(round(bup_cont_pair_as_drop30[6, 4], 1), 
                           " (", round(bup_cont_pair_as_drop30[6, 5], 1), ", ",
                           round(bup_cont_pair_as_drop30[6, 6], 1), ")")

bup_table_appendix[14, 5] <- paste0(round(bup_cont_pair_as_drop30[7, 4], 1), 
                           " (", round(bup_cont_pair_as_drop30[7, 5], 1), ", ",
                           round(bup_cont_pair_as_drop30[7, 6], 1), ")")

bup_table_appendix[15, 5] <- paste0(round(bup_cont_pair_as_drop30[8, 4], 1), 
                           " (", round(bup_cont_pair_as_drop30[8, 5], 1), ", ",
                           round(bup_cont_pair_as_drop30[8, 6], 1), ")")

bup_table_appendix[16, 5] <- paste0(round(bup_cont_pair_all_drop30[5, 4], 1), 
                           " (", round(bup_cont_pair_all_drop30[5, 5], 1), ", ",
                           round(bup_cont_pair_all_drop30[5, 6], 1), ")")

bup_table_appendix[17, 5] <- paste0(round(bup_cont_pair_all_drop30[6, 4], 1), 
                           " (", round(bup_cont_pair_all_drop30[6, 5], 1), ", ",
                           round(bup_cont_pair_all_drop30[6, 6], 1), ")")

bup_table_appendix[18, 5] <- paste0(round(bup_cont_pair_all_drop30[7, 4], 1), 
                           " (", round(bup_cont_pair_all_drop30[7, 5], 1), ", ",
                           round(bup_cont_pair_all_drop30[7, 6], 1), ")")

bup_table_appendix[19, 5] <- paste0(round(bup_cont_pair_all_drop30[8, 4], 1), 
                           " (", round(bup_cont_pair_all_drop30[8, 5], 1), ", ",
                           round(bup_cont_pair_all_drop30[8, 6], 1), ")")

print(xtable(bup_table_appendix))

view(bup_table_appendix |>
         rename("NHW Mean" = "Mean",
                "NHB Mean" = "Mean.1",
                "Hisp. Mean" = "Mean.2",
                "NHB v. NHW" = "Difference",
                "Hisp. v. NHW" = "Difference.1"
         ))
