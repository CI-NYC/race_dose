library(tidyverse)
library(xtable)

met_cont_total <- readRDS(here::here("data/table_data/appendix/met_cont_total_outlier.rds"))
met_cont_pair <- readRDS(here::here("data/table_data/appendix/met_cont_pair_outlier.rds"))


sens_table <- data.frame(Mean = c(0.00, 0.00, 0.00, 0.00),
                         Mean = c(0.00, 0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00,  0.00, 0.00),
                          Mean = c(0.00, 0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00,  0.00, 0.00)
)

met_cont_total_as <- met_cont_total |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, week)

met_cont_pair_as <- met_cont_pair |>
    select(time, race, type, theta, conf.low, conf.high) |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, time)

met_cont_total_all <- met_cont_total |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, week)

met_cont_pair_all <- met_cont_pair |>
    select(time, race, type, theta, conf.low, conf.high) |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, time)

met_cont_total <- met_cont_total |>
    arrange(race, type, week)

met_cont_pair <- met_cont_pair |>
    select(time, race, type, theta, conf.low, conf.high) |>
    arrange(race, type, time)

sens_table[1, 1] <- paste0(round(met_cont_total_as[5, 3], 1), 
                          " (", round(met_cont_total_as[5, 5], 1), ", ",
                          round(met_cont_total_as[5, 6], 1), ")")

sens_table[2, 1] <- paste0(round(met_cont_total_as[6, 3], 1), 
                          " (", round(met_cont_total_as[6, 5], 1), ", ",
                          round(met_cont_total_as[6, 6], 1), ")")

sens_table[3, 1] <- paste0(round(met_cont_total_all[5, 3], 1), 
                          " (", round(met_cont_total_all[5, 5], 1), ", ",
                          round(met_cont_total_all[5, 6], 1), ")")

sens_table[4, 1] <- paste0(round(met_cont_total_all[6, 3], 1), 
                          " (", round(met_cont_total_all[6, 5], 1), ", ",
                          round(met_cont_total_all[6, 6], 1), ")")

sens_table[1, 2] <- paste0(round(met_cont_total_as[1, 3], 1), 
                          " (", round(met_cont_total_as[1, 5], 1), ", ",
                          round(met_cont_total_as[1, 6], 1), ")")

sens_table[2, 2] <- paste0(round(met_cont_total_as[2, 3], 1), 
                          " (", round(met_cont_total_as[2, 5], 1), ", ",
                          round(met_cont_total_as[2, 6], 1), ")")

sens_table[3, 2] <- paste0(round(met_cont_total_all[1, 3], 1), 
                          " (", round(met_cont_total_all[1, 5], 1), ", ",
                          round(met_cont_total_all[1, 6], 1), ")")

sens_table[4, 2] <- paste0(round(met_cont_total_all[2, 3], 1), 
                          " (", round(met_cont_total_all[2, 5], 1), ", ",
                          round(met_cont_total_all[2, 6], 1), ")")

sens_table[1, 4] <- paste0(round(met_cont_total_as[3, 3], 1), 
                          " (", round(met_cont_total_as[3, 5], 1), ", ",
                          round(met_cont_total_as[3, 6], 1), ")")

sens_table[2, 4] <- paste0(round(met_cont_total_as[4, 3], 1), 
                          " (", round(met_cont_total_as[4, 5], 1), ", ",
                          round(met_cont_total_as[4, 6], 1), ")")

sens_table[3, 4] <- paste0(round(met_cont_total_all[3, 3], 1), 
                          " (", round(met_cont_total_all[3, 5], 1), ", ",
                          round(met_cont_total_all[3, 6], 1), ")")

sens_table[4, 4] <- paste0(round(met_cont_total_all[4, 3], 1), 
                          " (", round(met_cont_total_all[4, 5], 1), ", ",
                          round(met_cont_total_all[4, 6], 1), ")")

sens_table[1, 3] <- paste0(round(met_cont_pair_as[1, 4], 1), 
                          " (", round(met_cont_pair_as[1, 5], 1), ", ",
                          round(met_cont_pair_as[1, 6], 1), ")")

sens_table[2, 3] <- paste0(round(met_cont_pair_as[2, 4], 1), 
                          " (", round(met_cont_pair_as[2, 5], 1), ", ",
                          round(met_cont_pair_as[2, 6], 1), ")")

sens_table[3, 3] <- paste0(round(met_cont_pair_all[1, 4], 1), 
                          " (", round(met_cont_pair_all[1, 5], 1), ", ",
                          round(met_cont_pair_all[1, 6], 1), ")")

sens_table[4, 3] <- paste0(round(met_cont_pair_all[2, 4], 1), 
                          " (", round(met_cont_pair_all[2, 5], 1), ", ",
                          round(met_cont_pair_all[2, 6], 1), ")")

sens_table[1, 5] <- paste0(round(met_cont_pair_as[3, 4], 1), 
                          " (", round(met_cont_pair_as[3, 5], 1), ", ",
                          round(met_cont_pair_as[3, 6], 1), ")")

sens_table[2, 5] <- paste0(round(met_cont_pair_as[4, 4], 1), 
                          " (", round(met_cont_pair_as[4, 5], 1), ", ",
                          round(met_cont_pair_as[4, 6], 1), ")")

sens_table[3, 5] <- paste0(round(met_cont_pair_all[3, 4], 1), 
                          " (", round(met_cont_pair_all[3, 5], 1), ", ",
                          round(met_cont_pair_all[3, 6], 1), ")")

sens_table[4, 5] <- paste0(round(met_cont_pair_all[4, 4], 1), 
                          " (", round(met_cont_pair_all[4, 5], 1), ", ",
                          round(met_cont_pair_all[4, 6], 1), ")")


print(xtable(sens_table))

view(sens_table |>
         rename("NHW Mean" = "Mean",
                "NHB Mean" = "Mean.1",
                "Hisp. Mean" = "Mean.2",
                "NHB v. NHW" = "Difference",
                "Hisp. v. NHW" = "Difference.1"
         ))

