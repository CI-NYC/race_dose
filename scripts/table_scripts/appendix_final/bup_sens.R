library(tidyverse)
library(xtable)

bup_cont_total_trial_indicator <- readRDS(here::here("data/table_data/appendix/bup_cont_total_trial_indicator.rds"))
bup_cont_pair_trial_indicator <- readRDS(here::here("data/table_data/appendix/bup_cont_pair_trial_indicator.rds"))

bup_cont_total_drop30 <- readRDS(here::here("data/table_data/appendix/bup_cont_total_trial_indicator_drop30.rds"))
bup_cont_pair_drop30 <- readRDS(here::here("data/table_data/appendix/bup_cont_pair_trial_indicator_drop30.rds"))

bup_cont_total_stratify <- readRDS(here::here("data/table_data/appendix/theta_df_bup.rds"))
bup_cont_pair_stratify <- readRDS(here::here("data/table_data/appendix/diff_df_bup.rds")) |>
    arrange(trial)


bup_table_appendix <- data.frame(Mean = c(0.00, 0.00, 0.00, 0.00, 0.00),
                          Mean = c(0.00, 0.00, 0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00, 0.00, 0.00, 0.00),
                          Mean = c(0.00, 0.00, 0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00, 0.00, 0.00, 0.00)
)

bup_cont_total_as_trial_indicator <- bup_cont_total_trial_indicator |>
    arrange(race, week)

bup_cont_pair_as_trial_indicator <- bup_cont_pair_trial_indicator |>
    select(time, race, theta, conf.low, conf.high) |>
    arrange(race, time)

bup_cont_total_as_drop30 <- bup_cont_total_drop30 |>
    arrange(race, week)

bup_cont_pair_as_drop30 <- bup_cont_pair_drop30 |>
    select(time, race, theta, conf.low, conf.high) |>
    arrange(race, time)

bup_table_appendix[1, 1] <- paste0(round(bup_cont_total_stratify[1, 4], 1), 
                            " (", round(bup_cont_total_stratify[1, 6], 1), ", ",
                            round(bup_cont_total_stratify[1, 7], 1), ")")

bup_table_appendix[2, 1] <- paste0(round(bup_cont_total_stratify[2, 4], 1), 
                            " (", round(bup_cont_total_stratify[2, 6], 1), ", ",
                            round(bup_cont_total_stratify[2, 7], 1), ")")

bup_table_appendix[3, 1] <- paste0(round(bup_cont_total_stratify[3, 4], 1), 
                          " (", round(bup_cont_total_stratify[3, 6], 1), ", ",
                          round(bup_cont_total_stratify[3, 7], 1), ")")

bup_table_appendix[4, 1] <- paste0(round(bup_cont_total_as_trial_indicator[3, 3], 1), 
                            " (", round(bup_cont_total_as_trial_indicator[3, 5], 1), ", ",
                            round(bup_cont_total_as_trial_indicator[3, 6], 1), ")")

bup_table_appendix[5, 1] <- paste0(round(bup_cont_total_as_drop30[3, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[3, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[3, 6], 1), ")")

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

bup_table_appendix[5, 2] <- paste0(round(bup_cont_total_as_drop30[1, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[1, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[1, 6], 1), ")")

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

bup_table_appendix[4, 4] <- paste0(round(bup_cont_total_as_trial_indicator[2, 3], 1), 
                          " (", round(bup_cont_total_as_trial_indicator[2, 5], 1), ", ",
                          round(bup_cont_total_as_trial_indicator[2, 6], 1), ")")

bup_table_appendix[5, 4] <- paste0(round(bup_cont_total_as_drop30[2, 3], 1), 
                           " (", round(bup_cont_total_as_drop30[2, 5], 1), ", ",
                           round(bup_cont_total_as_drop30[2, 6], 1), ")")

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

bup_table_appendix[4, 3] <- paste0(round(bup_cont_pair_as_trial_indicator[1, 3], 1), 
                          " (", round(bup_cont_pair_as_trial_indicator[1, 4], 1), ", ",
                          round(bup_cont_pair_as_trial_indicator[1, 5], 1), ")")

bup_table_appendix[5, 3] <- paste0(round(bup_cont_pair_as_drop30[1, 3], 1), 
                           " (", round(bup_cont_pair_as_drop30[1, 4], 1), ", ",
                           round(bup_cont_pair_as_drop30[1, 5], 1), ")")

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

bup_table_appendix[4, 5] <- paste0(round(bup_cont_pair_as_trial_indicator[2, 3], 1), 
                          " (", round(bup_cont_pair_as_trial_indicator[2, 4], 1), ", ",
                          round(bup_cont_pair_as_trial_indicator[2, 5], 1), ")")

bup_table_appendix[5, 5] <- paste0(round(bup_cont_pair_as_drop30[2, 3], 1), 
                           " (", round(bup_cont_pair_as_drop30[2, 4], 1), ", ",
                           round(bup_cont_pair_as_drop30[2, 5], 1), ")")

print(xtable(bup_table_appendix))

view(bup_table_appendix |>
         rename("NHW Mean" = "Mean",
                "NHB Mean" = "Mean.1",
                "Hisp. Mean" = "Mean.2",
                "NHB v. NHW" = "Difference",
                "Hisp. v. NHW" = "Difference.1"
         ))
