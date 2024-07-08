library(tidyverse)
library(xtable)

bup_cont_pair_cows <- readRDS(here::here("data/table_data/appendix/bup_cont_pair_cows_nonimpute.rds")) |>
    arrange(race, time, cows)
#bup_cont_total_cows <- readRDS(here::here("data/table_data/appendix/bup_cont_total_cows_nonimpute.rds"))
comb_binomial_pair_cows <- readRDS(here::here("data/table_data/appendix/comb_binomial_pair_cows_nonimpute.rds")) |>
    arrange(race, time, cows)
#comb_binomial_total_cows <- readRDS(here::here("data/table_data/appendix/comb_binomial_total_cows_nonimpute.rds"))
met_cont_pair_cows <- readRDS(here::here("data/table_data/appendix/met_cont_pair_cows_nonimpute.rds")) |>
    arrange(race, time, cows)
#met_cont_total_cows <- readRDS(here::here("data/table_data/appendix/met_cont_total_cows_nonimpute.rds")) 


cows_table <- data.frame(Difference = c(0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00, 0.00)
)

# COLUMN 1
cows_table[1, 1] <- paste0(round(bup_cont_pair_cows[2, 1], 1), 
                            " (", round(bup_cont_pair_cows[2, 5], 1), ", ",
                            round(bup_cont_pair_cows[2, 6], 1), ")")

cows_table[2, 1] <- paste0(round(met_cont_pair_cows[2, 1], 1), 
                            " (", round(met_cont_pair_cows[2, 5], 1), ", ",
                            round(met_cont_pair_cows[2, 6], 1), ")")

cows_table[3, 1] <- paste0(round(comb_binomial_pair_cows[2, 1], 3) * 100, 
                            " (", round(comb_binomial_pair_cows[2, 5], 3) * 100, ", ",
                           round(comb_binomial_pair_cows[2, 6], 3) * 100, ")")

# COLUMN 2

cows_table[1, 2] <- paste0(round(bup_cont_pair_cows[1, 1], 1), 
                           " (", round(bup_cont_pair_cows[1, 5], 1), ", ",
                           round(bup_cont_pair_cows[1, 6], 1), ")")

cows_table[2, 2] <- paste0(round(met_cont_pair_cows[1, 1], 1), 
                           " (", round(met_cont_pair_cows[1, 5], 1), ", ",
                           round(met_cont_pair_cows[1, 6], 1), ")")

cows_table[3, 2] <- paste0(round(comb_binomial_pair_cows[1, 1], 3) * 100, 
                           " (", round(comb_binomial_pair_cows[1, 5], 3) * 100, ", ",
                           round(comb_binomial_pair_cows[1, 6], 3) * 100, ")")

# COLUMN 3
cows_table[1, 3] <- paste0(round(bup_cont_pair_cows[4, 1], 1), 
                           " (", round(bup_cont_pair_cows[4, 5], 1), ", ",
                           round(bup_cont_pair_cows[4, 6], 1), ")")

cows_table[2, 3] <- paste0(round(met_cont_pair_cows[4, 1], 1), 
                           " (", round(met_cont_pair_cows[4, 5], 1), ", ",
                           round(met_cont_pair_cows[4, 6], 1), ")")

cows_table[3, 3] <- paste0(round(comb_binomial_pair_cows[4, 1], 3) * 100, 
                           " (", round(comb_binomial_pair_cows[4, 5], 3) * 100, ", ",
                           round(comb_binomial_pair_cows[4, 6], 3) * 100, ")")

# COLUMN 4
cows_table[1, 4] <- paste0(round(bup_cont_pair_cows[3, 1], 1), 
                           " (", round(bup_cont_pair_cows[3, 5], 1), ", ",
                           round(bup_cont_pair_cows[3, 6], 1), ")")

cows_table[2, 4] <- paste0(round(met_cont_pair_cows[3, 1], 1), 
                           " (", round(met_cont_pair_cows[3, 5], 1), ", ",
                           round(met_cont_pair_cows[3, 6], 1), ")")

cows_table[3, 4] <- paste0(round(comb_binomial_pair_cows[3, 1], 3) * 100, 
                           " (", round(comb_binomial_pair_cows[3, 5], 3) * 100, ", ",
                           round(comb_binomial_pair_cows[3, 6], 3) * 100, ")")
print(xtable(cows_table))

view(cows_table |>
         rename("NHB v. NHW HIGH" = "Difference",
                "NHB v. NHW LOW" = "Difference.1",
                "Hispanic v. NHW HIGH" = "Difference.2",
                "Hispanic v. NHW LOW" = "Difference.3"
         ))
