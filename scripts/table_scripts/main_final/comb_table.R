library(tidyverse)
library(xtable)

comb_binomial_total <- readRDS(here::here("data/table_data/main/comb_binomial_total.rds"))
comb_binomial_pair <- readRDS(here::here("data/table_data/main/comb_binomial_pair.rds"))

comb_table <- data.frame(Mean = c(0.00, 0.00, 0.00, 0.00),
                        Mean = c(0.00, 0.00, 0.00, 0.00),
                        Difference = c(0.00, 0.00, 0.00, 0.00),
                        Mean = c(0.00, 0.00, 0.00, 0.00),
                        Difference = c(0.00, 0.00, 0.00, 0.00)
)

comb_binomial_total_as <- comb_binomial_total |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, week)

comb_binomial_pair_as <- comb_binomial_pair |>
    select(time, race, type, theta, conf.low, conf.high) |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, time)

comb_binomial_total_all <- comb_binomial_total |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, week)

comb_binomial_pair_all <- comb_binomial_pair |>
    select(time, race, type, theta, conf.low, conf.high) |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, time)

comb_table[1, 1] <- paste0(round(comb_binomial_total_as[5, 3], 3) * 100, 
                          " (", round(comb_binomial_total_as[5, 5], 3) * 100, ", ",
                          round(comb_binomial_total_as[5, 6], 3) * 100, ")")

comb_table[2, 1] <- paste0(round(comb_binomial_total_as[6, 3], 3) * 100, 
                          " (", round(comb_binomial_total_as[6, 5], 3) * 100, ", ",
                          round(comb_binomial_total_as[6, 6], 3) * 100, ")")

comb_table[3, 1] <- paste0(round(comb_binomial_total_all[5, 3], 3) * 100, 
                          " (", round(comb_binomial_total_all[5, 5], 3) * 100, ", ",
                          round(comb_binomial_total_all[5, 6], 3) * 100, ")")

comb_table[4, 1] <- paste0(round(comb_binomial_total_all[6, 3], 3) * 100, 
                          " (", round(comb_binomial_total_all[6, 5], 3) * 100, ", ",
                          round(comb_binomial_total_all[6, 6], 3) * 100, ")")

comb_table[1, 2] <- paste0(round(comb_binomial_total_as[1, 3], 3) * 100, 
                          " (", round(comb_binomial_total_as[1, 5], 3) * 100, ", ",
                          round(comb_binomial_total_as[1, 6], 3) * 100, ")")

comb_table[2, 2] <- paste0(round(comb_binomial_total_as[2, 3], 3) * 100, 
                          " (", round(comb_binomial_total_as[2, 5], 3) * 100, ", ",
                          round(comb_binomial_total_as[2, 6], 3) * 100, ")")

comb_table[3, 2] <- paste0(round(comb_binomial_total_all[1, 3], 3) * 100, 
                          " (", round(comb_binomial_total_all[1, 5], 3) * 100, ", ",
                          round(comb_binomial_total_all[1, 6], 3) * 100, ")")

comb_table[4, 2] <- paste0(round(comb_binomial_total_all[2, 3], 3) * 100, 
                          " (", round(comb_binomial_total_all[2, 5], 3) * 100, ", ",
                          round(comb_binomial_total_all[2, 6], 3) * 100, ")")

comb_table[1, 4] <- paste0(round(comb_binomial_total_as[3, 3], 3) * 100, 
                          " (", round(comb_binomial_total_as[3, 5], 3) * 100, ", ",
                          round(comb_binomial_total_as[3, 6], 3) * 100, ")")

comb_table[2, 4] <- paste0(round(comb_binomial_total_as[4, 3], 3) * 100, 
                          " (", round(comb_binomial_total_as[4, 5], 3) * 100, ", ",
                          round(comb_binomial_total_as[4, 6], 3) * 100, ")")

comb_table[3, 4] <- paste0(round(comb_binomial_total_all[3, 3], 3) * 100, 
                          " (", round(comb_binomial_total_all[3, 5], 3) * 100, ", ",
                          round(comb_binomial_total_all[3, 6], 3) * 100, ")")

comb_table[4, 4] <- paste0(round(comb_binomial_total_all[4, 3], 3) * 100, 
                          " (", round(comb_binomial_total_all[4, 5], 3) * 100, ", ",
                          round(comb_binomial_total_all[4, 6], 3) * 100, ")")

comb_table[1, 3] <- paste0(round(comb_binomial_pair_as[1, 4], 3) * 100, 
                          " (", round(comb_binomial_pair_as[1, 5], 3) * 100, ", ",
                          round(comb_binomial_pair_as[1, 6], 3) * 100, ")")

comb_table[2, 3] <- paste0(round(comb_binomial_pair_as[2, 4], 3) * 100, 
                          " (", round(comb_binomial_pair_as[2, 5], 3) * 100, ", ",
                          round(comb_binomial_pair_as[2, 6], 3) * 100, ")")

comb_table[3, 3] <- paste0(round(comb_binomial_pair_all[1, 4], 3) * 100, 
                          " (", round(comb_binomial_pair_all[1, 5], 3) * 100, ", ",
                          round(comb_binomial_pair_all[1, 6], 3) * 100, ")")

comb_table[4, 3] <- paste0(round(comb_binomial_pair_all[2, 4], 3) * 100, 
                          " (", round(comb_binomial_pair_all[2, 5], 3) * 100, ", ",
                          round(comb_binomial_pair_all[2, 6], 3) * 100, ")")

comb_table[1, 5] <- paste0(round(comb_binomial_pair_as[3, 4], 3) * 100, 
                          " (", round(comb_binomial_pair_as[3, 5], 3) * 100, ", ",
                          round(comb_binomial_pair_as[3, 6], 3) * 100, ")")

comb_table[2, 5] <- paste0(round(comb_binomial_pair_as[4, 4], 3) * 100, 
                          " (", round(comb_binomial_pair_as[4, 5], 3) * 100, ", ",
                          round(comb_binomial_pair_as[4, 6], 3) * 100, ")")

comb_table[3, 5] <- paste0(round(comb_binomial_pair_all[3, 4], 3) * 100, 
                          " (", round(comb_binomial_pair_all[3, 5], 3) * 100, ", ",
                          round(comb_binomial_pair_all[3, 6], 3) * 100, ")")

comb_table[4, 5] <- paste0(round(comb_binomial_pair_all[4, 4], 3) * 100, 
                          " (", round(comb_binomial_pair_all[4, 5], 3) * 100, ", ",
                          round(comb_binomial_pair_all[4, 6], 3) * 100, ")")

print(xtable(comb_table))

view(comb_table |>
         rename("NHW Mean" = "Mean",
                "NHB Mean" = "Mean.1",
                "Hisp. Mean" = "Mean.2",
                "NHB v. NHW" = "Difference",
                "Hisp. v. NHW" = "Difference.1"
         ))
