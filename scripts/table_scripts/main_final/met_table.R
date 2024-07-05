library(tidyverse)
library(xtable)

met_cont_total <- readRDS(here::here("data/table_data/main/met_cont_total.rds"))
met_cont_pair <- readRDS(here::here("data/table_data/main/met_cont_pair.rds"))

met_table <- data.frame(Mean = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                        Mean = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                        Difference = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                        Mean = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                        Difference = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00)
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

met_table[1, 1] <- paste0(round(met_cont_total_as[9, 3], 1), 
                          " (", round(met_cont_total_as[9, 5], 1), ", ",
                          round(met_cont_total_as[9, 6], 1), ")")

met_table[2, 1] <- paste0(round(met_cont_total_as[10, 3], 1), 
                          " (", round(met_cont_total_as[10, 5], 1), ", ",
                          round(met_cont_total_as[10, 6], 1), ")")

met_table[3, 1] <- paste0(round(met_cont_total_as[11, 3], 1), 
                          " (", round(met_cont_total_as[11, 5], 1), ", ",
                          round(met_cont_total_as[11, 6], 1), ")")

met_table[4, 1] <- paste0(round(met_cont_total_as[12, 3], 1), 
                          " (", round(met_cont_total_as[12, 5], 1), ", ",
                          round(met_cont_total_as[12, 6], 1), ")")

met_table[5, 1] <- paste0(round(met_cont_total_all[9, 3], 1), 
                          " (", round(met_cont_total_all[9, 5], 1), ", ",
                          round(met_cont_total_all[9, 6], 1), ")")

met_table[6, 1] <- paste0(round(met_cont_total_all[10, 3], 1), 
                          " (", round(met_cont_total_all[10, 5], 1), ", ",
                          round(met_cont_total_all[10, 6], 1), ")")

met_table[7, 1] <- paste0(round(met_cont_total_all[11, 3], 1), 
                          " (", round(met_cont_total_all[11, 5], 1), ", ",
                          round(met_cont_total_all[11, 6], 1), ")")

met_table[8, 1] <- paste0(round(met_cont_total_all[12, 3], 1), 
                          " (", round(met_cont_total_all[12, 5], 1), ", ",
                          round(met_cont_total_all[12, 6], 1), ")")

met_table[1, 2] <- paste0(round(met_cont_total_as[1, 3], 1), 
                          " (", round(met_cont_total_as[1, 5], 1), ", ",
                          round(met_cont_total_as[1, 6], 1), ")")

met_table[2, 2] <- paste0(round(met_cont_total_as[2, 3], 1), 
                          " (", round(met_cont_total_as[2, 5], 1), ", ",
                          round(met_cont_total_as[2, 6], 1), ")")

met_table[3, 2] <- paste0(round(met_cont_total_as[3, 3], 1), 
                          " (", round(met_cont_total_as[3, 5], 1), ", ",
                          round(met_cont_total_as[3, 6], 1), ")")

met_table[4, 2] <- paste0(round(met_cont_total_as[4, 3], 1), 
                          " (", round(met_cont_total_as[4, 5], 1), ", ",
                          round(met_cont_total_as[4, 6], 1), ")")

met_table[5, 2] <- paste0(round(met_cont_total_all[1, 3], 1), 
                          " (", round(met_cont_total_all[1, 5], 1), ", ",
                          round(met_cont_total_all[1, 6], 1), ")")

met_table[6, 2] <- paste0(round(met_cont_total_all[2, 3], 1), 
                          " (", round(met_cont_total_all[2, 5], 1), ", ",
                          round(met_cont_total_all[2, 6], 1), ")")

met_table[7, 2] <- paste0(round(met_cont_total_all[3, 3], 1), 
                          " (", round(met_cont_total_all[3, 5], 1), ", ",
                          round(met_cont_total_all[3, 6], 1), ")")

met_table[8, 2] <- paste0(round(met_cont_total_all[4, 3], 1), 
                          " (", round(met_cont_total_all[4, 5], 1), ", ",
                          round(met_cont_total_all[4, 6], 1), ")")

met_table[1, 4] <- paste0(round(met_cont_total_as[5, 3], 1), 
                          " (", round(met_cont_total_as[5, 5], 1), ", ",
                          round(met_cont_total_as[5, 6], 1), ")")

met_table[2, 4] <- paste0(round(met_cont_total_as[6, 3], 1), 
                          " (", round(met_cont_total_as[6, 5], 1), ", ",
                          round(met_cont_total_as[6, 6], 1), ")")

met_table[3, 4] <- paste0(round(met_cont_total_as[7, 3], 1), 
                          " (", round(met_cont_total_as[7, 5], 1), ", ",
                          round(met_cont_total_as[7, 6], 1), ")")

met_table[4, 4] <- paste0(round(met_cont_total_as[8, 3], 1), 
                          " (", round(met_cont_total_as[8, 5], 1), ", ",
                          round(met_cont_total_as[8, 6], 1), ")")

met_table[5, 4] <- paste0(round(met_cont_total_all[5, 3], 1), 
                          " (", round(met_cont_total_all[5, 5], 1), ", ",
                          round(met_cont_total_all[5, 6], 1), ")")

met_table[6, 4] <- paste0(round(met_cont_total_all[6, 3], 1), 
                          " (", round(met_cont_total_all[6, 5], 1), ", ",
                          round(met_cont_total_all[6, 6], 1), ")")

met_table[7, 4] <- paste0(round(met_cont_total_all[7, 3], 1), 
                          " (", round(met_cont_total_all[7, 5], 1), ", ",
                          round(met_cont_total_all[7, 6], 1), ")")

met_table[8, 4] <- paste0(round(met_cont_total_all[8, 3], 1), 
                          " (", round(met_cont_total_all[8, 5], 1), ", ",
                          round(met_cont_total_all[8, 6], 1), ")")

met_table[1, 3] <- paste0(round(met_cont_pair_as[1, 4], 1), 
                          " (", round(met_cont_pair_as[1, 5], 1), ", ",
                          round(met_cont_pair_as[1, 6], 1), ")")

met_table[2, 3] <- paste0(round(met_cont_pair_as[2, 4], 1), 
                          " (", round(met_cont_pair_as[2, 5], 1), ", ",
                          round(met_cont_pair_as[2, 6], 1), ")")

met_table[3, 3] <- paste0(round(met_cont_pair_as[3, 4], 1), 
                          " (", round(met_cont_pair_as[3, 5], 1), ", ",
                          round(met_cont_pair_as[3, 6], 1), ")")

met_table[4, 3] <- paste0(round(met_cont_pair_as[4, 4], 1), 
                          " (", round(met_cont_pair_as[4, 5], 1), ", ",
                          round(met_cont_pair_as[4, 6], 1), ")")

met_table[5, 3] <- paste0(round(met_cont_pair_all[1, 4], 1), 
                          " (", round(met_cont_pair_all[1, 5], 1), ", ",
                          round(met_cont_pair_all[1, 6], 1), ")")

met_table[6, 3] <- paste0(round(met_cont_pair_all[2, 4], 1), 
                          " (", round(met_cont_pair_all[2, 5], 1), ", ",
                          round(met_cont_pair_all[2, 6], 1), ")")

met_table[7, 3] <- paste0(round(met_cont_pair_all[3, 4], 1), 
                          " (", round(met_cont_pair_all[3, 5], 1), ", ",
                          round(met_cont_pair_all[3, 6], 1), ")")

met_table[8, 3] <- paste0(round(met_cont_pair_all[4, 4], 1), 
                          " (", round(met_cont_pair_all[4, 5], 1), ", ",
                          round(met_cont_pair_all[4, 6], 1), ")")

met_table[1, 5] <- paste0(round(met_cont_pair_as[5, 4], 1), 
                          " (", round(met_cont_pair_as[5, 5], 1), ", ",
                          round(met_cont_pair_as[5, 6], 1), ")")

met_table[2, 5] <- paste0(round(met_cont_pair_as[6, 4], 1), 
                          " (", round(met_cont_pair_as[6, 5], 1), ", ",
                          round(met_cont_pair_as[6, 6], 1), ")")

met_table[3, 5] <- paste0(round(met_cont_pair_as[7, 4], 1), 
                          " (", round(met_cont_pair_as[7, 5], 1), ", ",
                          round(met_cont_pair_as[7, 6], 1), ")")

met_table[4, 5] <- paste0(round(met_cont_pair_as[8, 4], 1), 
                          " (", round(met_cont_pair_as[8, 5], 1), ", ",
                          round(met_cont_pair_as[8, 6], 1), ")")

met_table[5, 5] <- paste0(round(met_cont_pair_all[5, 4], 1), 
                          " (", round(met_cont_pair_all[5, 5], 1), ", ",
                          round(met_cont_pair_all[5, 6], 1), ")")

met_table[6, 5] <- paste0(round(met_cont_pair_all[6, 4], 1), 
                          " (", round(met_cont_pair_all[6, 5], 1), ", ",
                          round(met_cont_pair_all[6, 6], 1), ")")

met_table[7, 5] <- paste0(round(met_cont_pair_all[7, 4], 1), 
                          " (", round(met_cont_pair_all[7, 5], 1), ", ",
                          round(met_cont_pair_all[7, 6], 1), ")")

met_table[8, 5] <- paste0(round(met_cont_pair_all[8, 4], 1), 
                          " (", round(met_cont_pair_all[8, 5], 1), ", ",
                          round(met_cont_pair_all[8, 6], 1), ")")

print(xtable(met_table))

view(met_table |>
         rename("NHW Mean" = "Mean",
                "NHB Mean" = "Mean.1",
                "Hisp. Mean" = "Mean.2",
                "NHB v. NHW" = "Difference",
                "Hisp. v. NHW" = "Difference.1"
         ))
