library(tidyverse)
library(xtable)
appendix_table <- data.frame(Mean = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                          Mean = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                          Mean = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00)
)

bup_cont_total_all <- bup_cont_total |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, week)

bup_cont_pair_all <- bup_cont_pair |>
    select(time, race, type, theta, conf.low, conf.high) |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, time)

met_cont_total_all <- met_cont_total |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, week)

met_cont_pair_all <- met_cont_pair |>
    select(time, race, type, theta, conf.low, conf.high) |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, time)

comb_binomial_total_all <- comb_binomial_total |>
    filter(type == "Adjusted by expanded set") |>
    arrange(race, type, week)

comb_binomial_pair_all <- comb_binomial_pair |>
    filter(type == "Adjusted by expanded set") |>
    select(time, race, type, theta, conf.low, conf.high) |>
    arrange(race, type, time)

appendix_table[1, 1] <- paste0(round(bup_cont_total_all[9, 3], 1), 
                            " (", round(bup_cont_total_all[9, 5], 1), ", ",
                            round(bup_cont_total_all[9, 6], 1), ")")

appendix_table[2, 1] <- paste0(round(bup_cont_total_all[10, 3], 1), 
                            " (", round(bup_cont_total_all[10, 5], 1), ", ",
                            round(bup_cont_total_all[10, 6], 1), ")")

appendix_table[3, 1] <- paste0(round(bup_cont_total_all[11, 3], 1), 
                            " (", round(bup_cont_total_all[11, 5], 1), ", ",
                            round(bup_cont_total_all[11, 6], 1), ")")

appendix_table[4, 1] <- paste0(round(bup_cont_total_all[12, 3], 1), 
                            " (", round(bup_cont_total_all[12, 5], 1), ", ",
                            round(bup_cont_total_all[12, 6], 1), ")")

appendix_table[5, 1] <- paste0(round(met_cont_total_all[9, 3], 1), 
                            " (", round(met_cont_total_all[9, 5], 1), ", ",
                            round(met_cont_total_all[9, 6], 1), ")")

appendix_table[6, 1] <- paste0(round(met_cont_total_all[10, 3], 1), 
                            " (", round(met_cont_total_all[10, 5], 1), ", ",
                            round(met_cont_total_all[10, 6], 1), ")")

appendix_table[7, 1] <- paste0(round(met_cont_total_all[11, 3], 1), 
                            " (", round(met_cont_total_all[11, 5], 1), ", ",
                            round(met_cont_total_all[11, 6], 1), ")")

appendix_table[8, 1] <- paste0(round(met_cont_total_all[12, 3], 1), 
                            " (", round(met_cont_total_all[12, 5], 1), ", ",
                            round(met_cont_total_all[12, 6], 1), ")")

appendix_table[9, 1] <- paste0(round(comb_binomial_total_all[5, 3], 2), 
                            " (", round(comb_binomial_total_all[5, 5], 2), ", ",
                            round(comb_binomial_total_all[5, 6], 2), ")")

appendix_table[10, 1] <- paste0(round(comb_binomial_total_all[6, 3], 2), 
                             " (", round(comb_binomial_total_all[6, 5], 2), ", ",
                             round(comb_binomial_total_all[6, 6], 2), ")")

appendix_table[1, 2] <- paste0(round(bup_cont_total_all[1, 3], 1), 
                            " (", round(bup_cont_total_all[1, 5], 1), ", ",
                            round(bup_cont_total_all[1, 6], 1), ")")

appendix_table[2, 2] <- paste0(round(bup_cont_total_all[2, 3], 1), 
                            " (", round(bup_cont_total_all[2, 5], 1), ", ",
                            round(bup_cont_total_all[2, 6], 1), ")")

appendix_table[3, 2] <- paste0(round(bup_cont_total_all[3, 3], 1), 
                            " (", round(bup_cont_total_all[3, 5], 1), ", ",
                            round(bup_cont_total_all[3, 6], 1), ")")

appendix_table[4, 2] <- paste0(round(bup_cont_total_all[4, 3], 1), 
                            " (", round(bup_cont_total_all[4, 5], 1), ", ",
                            round(bup_cont_total_all[4, 6], 1), ")")

appendix_table[5, 2] <- paste0(round(met_cont_total_all[1, 3], 1), 
                            " (", round(met_cont_total_all[1, 5], 1), ", ",
                            round(met_cont_total_all[1, 6], 1), ")")

appendix_table[6, 2] <- paste0(round(met_cont_total_all[2, 3], 1), 
                            " (", round(met_cont_total_all[2, 5], 1), ", ",
                            round(met_cont_total_all[2, 6], 1), ")")

appendix_table[7, 2] <- paste0(round(met_cont_total_all[3, 3], 1), 
                            " (", round(met_cont_total_all[3, 5], 1), ", ",
                            round(met_cont_total_all[3, 6], 1), ")")

appendix_table[8, 2] <- paste0(round(met_cont_total_all[4, 3], 1), 
                            " (", round(met_cont_total_all[4, 5], 1), ", ",
                            round(met_cont_total_all[4, 6], 1), ")")

appendix_table[9, 2] <- paste0(round(comb_binomial_total_all[1, 3], 2), 
                            " (", round(comb_binomial_total_all[1, 5], 2), ", ",
                            round(comb_binomial_total_all[1, 6], 2), ")")

appendix_table[10, 2] <- paste0(round(comb_binomial_total_all[2, 3], 2), 
                             " (", round(comb_binomial_total_all[2, 5], 2), ", ",
                             round(comb_binomial_total_all[2, 6], 2), ")")

appendix_table[1, 4] <- paste0(round(bup_cont_total_all[5, 3], 1), 
                            " (", round(bup_cont_total_all[5, 5], 1), ", ",
                            round(bup_cont_total_all[5, 6], 1), ")")

appendix_table[2, 4] <- paste0(round(bup_cont_total_all[6, 3], 1), 
                            " (", round(bup_cont_total_all[6, 5], 1), ", ",
                            round(bup_cont_total_all[6, 6], 1), ")")

appendix_table[3, 4] <- paste0(round(bup_cont_total_all[7, 3], 1), 
                            " (", round(bup_cont_total_all[7, 5], 1), ", ",
                            round(bup_cont_total_all[7, 6], 1), ")")

appendix_table[4, 4] <- paste0(round(bup_cont_total_all[8, 3], 1), 
                            " (", round(bup_cont_total_all[8, 5], 1), ", ",
                            round(bup_cont_total_all[8, 6], 1), ")")

appendix_table[5, 4] <- paste0(round(met_cont_total_all[5, 3], 1), 
                            " (", round(met_cont_total_all[5, 5], 1), ", ",
                            round(met_cont_total_all[5, 6], 1), ")")

appendix_table[6, 4] <- paste0(round(met_cont_total_all[6, 3], 1), 
                            " (", round(met_cont_total_all[6, 5], 1), ", ",
                            round(met_cont_total_all[6, 6], 1), ")")

appendix_table[7, 4] <- paste0(round(met_cont_total_all[7, 3], 1), 
                            " (", round(met_cont_total_all[7, 5], 1), ", ",
                            round(met_cont_total_all[7, 6], 1), ")")

appendix_table[8, 4] <- paste0(round(met_cont_total_all[8, 3], 1), 
                            " (", round(met_cont_total_all[8, 5], 1), ", ",
                            round(met_cont_total_all[8, 6], 1), ")")

appendix_table[9, 4] <- paste0(round(comb_binomial_total_all[3, 3], 2), 
                            " (", round(comb_binomial_total_all[3, 5], 2), ", ",
                            round(comb_binomial_total_all[3, 6], 2), ")")

appendix_table[10, 4] <- paste0(round(comb_binomial_total_all[4, 3], 2), 
                             " (", round(comb_binomial_total_all[4, 5], 2), ", ",
                             round(comb_binomial_total_all[4, 6], 2), ")")

appendix_table[1, 3] <- paste0(round(bup_cont_pair_all[1, 4], 1), 
                            " (", round(bup_cont_pair_all[1, 5], 1), ", ",
                            round(bup_cont_pair_all[1, 6], 1), ")")

appendix_table[2, 3] <- paste0(round(bup_cont_pair_all[2, 4], 1), 
                            " (", round(bup_cont_pair_all[2, 5], 1), ", ",
                            round(bup_cont_pair_all[2, 6], 1), ")")

appendix_table[3, 3] <- paste0(round(bup_cont_pair_all[3, 4], 1), 
                            " (", round(bup_cont_pair_all[3, 5], 1), ", ",
                            round(bup_cont_pair_all[3, 6], 1), ")")

appendix_table[4, 3] <- paste0(round(bup_cont_pair_all[4, 4], 1), 
                            " (", round(bup_cont_pair_all[4, 5], 1), ", ",
                            round(bup_cont_pair_all[4, 6], 1), ")")

appendix_table[5, 3] <- paste0(round(met_cont_pair_all[1, 4], 1), 
                            " (", round(met_cont_pair_all[1, 5], 1), ", ",
                            round(met_cont_pair_all[1, 6], 1), ")")

appendix_table[6, 3] <- paste0(round(met_cont_pair_all[2, 4], 1), 
                            " (", round(met_cont_pair_all[2, 5], 1), ", ",
                            round(met_cont_pair_all[2, 6], 1), ")")

appendix_table[7, 3] <- paste0(round(met_cont_pair_all[3, 4], 1), 
                            " (", round(met_cont_pair_all[3, 5], 1), ", ",
                            round(met_cont_pair_all[3, 6], 1), ")")

appendix_table[8, 3] <- paste0(round(met_cont_pair_all[4, 4], 1), 
                            " (", round(met_cont_pair_all[4, 5], 1), ", ",
                            round(met_cont_pair_all[4, 6], 1), ")")

appendix_table[9, 3] <- paste0(round(comb_binomial_pair_all[1, 4], 2), 
                            " (", round(comb_binomial_pair_all[1, 5], 2), ", ",
                            round(comb_binomial_pair_all[1, 6], 2), ")")

appendix_table[10, 3] <- paste0(round(comb_binomial_pair_all[2, 4], 2), 
                             " (", round(comb_binomial_pair_all[2, 5], 2), ", ",
                             round(comb_binomial_pair_all[2, 6], 2), ")")

appendix_table[1, 5] <- paste0(round(bup_cont_pair_all[5, 4], 1), 
                            " (", round(bup_cont_pair_all[5, 5], 1), ", ",
                            round(bup_cont_pair_all[5, 6], 1), ")")

appendix_table[2, 5] <- paste0(round(bup_cont_pair_all[6, 4], 1), 
                            " (", round(bup_cont_pair_all[6, 5], 1), ", ",
                            round(bup_cont_pair_all[6, 6], 1), ")")

appendix_table[3, 5] <- paste0(round(bup_cont_pair_all[7, 4], 1), 
                            " (", round(bup_cont_pair_all[7, 5], 1), ", ",
                            round(bup_cont_pair_all[7, 6], 1), ")")

appendix_table[4, 5] <- paste0(round(bup_cont_pair_all[8, 4], 1), 
                            " (", round(bup_cont_pair_all[8, 5], 1), ", ",
                            round(bup_cont_pair_all[8, 6], 1), ")")

appendix_table[5, 5] <- paste0(round(met_cont_pair_all[5, 4], 1), 
                            " (", round(met_cont_pair_all[5, 5], 1), ", ",
                            round(met_cont_pair_all[5, 6], 1), ")")

appendix_table[6, 5] <- paste0(round(met_cont_pair_all[6, 4], 1), 
                            " (", round(met_cont_pair_all[6, 5], 1), ", ",
                            round(met_cont_pair_all[6, 6], 1), ")")

appendix_table[7, 5] <- paste0(round(met_cont_pair_all[7, 4], 1), 
                            " (", round(met_cont_pair_all[7, 5], 1), ", ",
                            round(met_cont_pair_all[7, 6], 1), ")")

appendix_table[8, 5] <- paste0(round(met_cont_pair_all[8, 4], 1), 
                            " (", round(met_cont_pair_all[8, 5], 1), ", ",
                            round(met_cont_pair_all[8, 6], 1), ")")

appendix_table[9, 5] <- paste0(round(comb_binomial_pair_all[3, 4], 2), 
                            " (", round(comb_binomial_pair_all[3, 5], 2), ", ",
                            round(comb_binomial_pair_all[3, 6], 2), ")")

appendix_table[10, 5] <- paste0(round(comb_binomial_pair_all[4, 4], 2), 
                             " (", round(comb_binomial_pair_all[4, 5], 2), ", ",
                             round(comb_binomial_pair_all[4, 6], 2), ")")

print(xtable(appendix_table))

view(appendix_table |>
         rename("NHW Mean" = "Mean",
                "NHB Mean" = "Mean.1",
                "Hisp. Mean" = "Mean.2",
                "NHB v. NHW" = "Difference",
                "Hisp. v. NHW" = "Difference.1"
         ))

