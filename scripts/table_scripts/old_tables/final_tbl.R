library(tidyverse)
library(xtable)
final_table <- data.frame(Mean = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                          Mean = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                          Mean = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00),
                          Difference = c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00)
                          )

bup_cont_total_as <- bup_cont_total |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, week)

bup_cont_pair_as <- bup_cont_pair |>
    select(time, race, type, theta, conf.low, conf.high) |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, time)

met_cont_total_as <- met_cont_total |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, week)

met_cont_pair_as <- met_cont_pair |>
    select(time, race, type, theta, conf.low, conf.high) |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, time)

comb_binomial_total_as <- comb_binomial_total |>
    filter(type == "Adjusted by age & sex") |>
    arrange(race, type, week)

comb_binomial_pair_as <- comb_binomial_pair |>
    filter(type == "Adjusted by age & sex") |>
    select(time, race, type, theta, conf.low, conf.high) |>
    arrange(race, type, time)

final_table[1, 1] <- paste0(round(bup_cont_total_as[9, 3], 1), 
                        " (", round(bup_cont_total_as[9, 5], 1), ", ",
                        round(bup_cont_total_as[9, 6], 1), ")")

final_table[2, 1] <- paste0(round(bup_cont_total_as[10, 3], 1), 
                            " (", round(bup_cont_total_as[10, 5], 1), ", ",
                            round(bup_cont_total_as[10, 6], 1), ")")

final_table[3, 1] <- paste0(round(bup_cont_total_as[11, 3], 1), 
                            " (", round(bup_cont_total_as[11, 5], 1), ", ",
                            round(bup_cont_total_as[11, 6], 1), ")")

final_table[4, 1] <- paste0(round(bup_cont_total_as[12, 3], 1), 
                            " (", round(bup_cont_total_as[12, 5], 1), ", ",
                            round(bup_cont_total_as[12, 6], 1), ")")

final_table[5, 1] <- paste0(round(met_cont_total_as[9, 3], 1), 
                            " (", round(met_cont_total_as[9, 5], 1), ", ",
                            round(met_cont_total_as[9, 6], 1), ")")

final_table[6, 1] <- paste0(round(met_cont_total_as[10, 3], 1), 
                            " (", round(met_cont_total_as[10, 5], 1), ", ",
                            round(met_cont_total_as[10, 6], 1), ")")

final_table[7, 1] <- paste0(round(met_cont_total_as[11, 3], 1), 
                            " (", round(met_cont_total_as[11, 5], 1), ", ",
                            round(met_cont_total_as[11, 6], 1), ")")

final_table[8, 1] <- paste0(round(met_cont_total_as[12, 3], 1), 
                            " (", round(met_cont_total_as[12, 5], 1), ", ",
                            round(met_cont_total_as[12, 6], 1), ")")

final_table[9, 1] <- paste0(round(comb_binomial_total_as[5, 3], 2), 
                            " (", round(comb_binomial_total_as[5, 5], 2), ", ",
                            round(comb_binomial_total_as[5, 6], 2), ")")

final_table[10, 1] <- paste0(round(comb_binomial_total_as[6, 3], 2), 
                            " (", round(comb_binomial_total_as[6, 5], 2), ", ",
                            round(comb_binomial_total_as[6, 6], 2), ")")

final_table[1, 2] <- paste0(round(bup_cont_total_as[1, 3], 1), 
                            " (", round(bup_cont_total_as[1, 5], 1), ", ",
                            round(bup_cont_total_as[1, 6], 1), ")")

final_table[2, 2] <- paste0(round(bup_cont_total_as[2, 3], 1), 
                            " (", round(bup_cont_total_as[2, 5], 1), ", ",
                            round(bup_cont_total_as[2, 6], 1), ")")

final_table[3, 2] <- paste0(round(bup_cont_total_as[3, 3], 1), 
                            " (", round(bup_cont_total_as[3, 5], 1), ", ",
                            round(bup_cont_total_as[3, 6], 1), ")")

final_table[4, 2] <- paste0(round(bup_cont_total_as[4, 3], 1), 
                            " (", round(bup_cont_total_as[4, 5], 1), ", ",
                            round(bup_cont_total_as[4, 6], 1), ")")

final_table[5, 2] <- paste0(round(met_cont_total_as[1, 3], 1), 
                            " (", round(met_cont_total_as[1, 5], 1), ", ",
                            round(met_cont_total_as[1, 6], 1), ")")

final_table[6, 2] <- paste0(round(met_cont_total_as[2, 3], 1), 
                            " (", round(met_cont_total_as[2, 5], 1), ", ",
                            round(met_cont_total_as[2, 6], 1), ")")

final_table[7, 2] <- paste0(round(met_cont_total_as[3, 3], 1), 
                            " (", round(met_cont_total_as[3, 5], 1), ", ",
                            round(met_cont_total_as[3, 6], 1), ")")

final_table[8, 2] <- paste0(round(met_cont_total_as[4, 3], 1), 
                            " (", round(met_cont_total_as[4, 5], 1), ", ",
                            round(met_cont_total_as[4, 6], 1), ")")

final_table[9, 2] <- paste0(round(comb_binomial_total_as[1, 3], 2), 
                            " (", round(comb_binomial_total_as[1, 5], 2), ", ",
                            round(comb_binomial_total_as[1, 6], 2), ")")

final_table[10, 2] <- paste0(round(comb_binomial_total_as[2, 3], 2), 
                             " (", round(comb_binomial_total_as[2, 5], 2), ", ",
                             round(comb_binomial_total_as[2, 6], 2), ")")

final_table[1, 4] <- paste0(round(bup_cont_total_as[5, 3], 1), 
                            " (", round(bup_cont_total_as[5, 5], 1), ", ",
                            round(bup_cont_total_as[5, 6], 1), ")")

final_table[2, 4] <- paste0(round(bup_cont_total_as[6, 3], 1), 
                            " (", round(bup_cont_total_as[6, 5], 1), ", ",
                            round(bup_cont_total_as[6, 6], 1), ")")

final_table[3, 4] <- paste0(round(bup_cont_total_as[7, 3], 1), 
                            " (", round(bup_cont_total_as[7, 5], 1), ", ",
                            round(bup_cont_total_as[7, 6], 1), ")")

final_table[4, 4] <- paste0(round(bup_cont_total_as[8, 3], 1), 
                            " (", round(bup_cont_total_as[8, 5], 1), ", ",
                            round(bup_cont_total_as[8, 6], 1), ")")

final_table[5, 4] <- paste0(round(met_cont_total_as[5, 3], 1), 
                            " (", round(met_cont_total_as[5, 5], 1), ", ",
                            round(met_cont_total_as[5, 6], 1), ")")

final_table[6, 4] <- paste0(round(met_cont_total_as[6, 3], 1), 
                            " (", round(met_cont_total_as[6, 5], 1), ", ",
                            round(met_cont_total_as[6, 6], 1), ")")

final_table[7, 4] <- paste0(round(met_cont_total_as[7, 3], 1), 
                            " (", round(met_cont_total_as[7, 5], 1), ", ",
                            round(met_cont_total_as[7, 6], 1), ")")

final_table[8, 4] <- paste0(round(met_cont_total_as[8, 3], 1), 
                            " (", round(met_cont_total_as[8, 5], 1), ", ",
                            round(met_cont_total_as[8, 6], 1), ")")

final_table[9, 4] <- paste0(round(comb_binomial_total_as[3, 3], 2), 
                            " (", round(comb_binomial_total_as[3, 5], 2), ", ",
                            round(comb_binomial_total_as[3, 6], 2), ")")

final_table[10, 4] <- paste0(round(comb_binomial_total_as[4, 3], 2), 
                             " (", round(comb_binomial_total_as[4, 5], 2), ", ",
                             round(comb_binomial_total_as[4, 6], 2), ")")

final_table[1, 3] <- paste0(round(bup_cont_pair_as[1, 4], 1), 
                            " (", round(bup_cont_pair_as[1, 5], 1), ", ",
                            round(bup_cont_pair_as[1, 6], 1), ")")

final_table[2, 3] <- paste0(round(bup_cont_pair_as[2, 4], 1), 
                            " (", round(bup_cont_pair_as[2, 5], 1), ", ",
                            round(bup_cont_pair_as[2, 6], 1), ")")

final_table[3, 3] <- paste0(round(bup_cont_pair_as[3, 4], 1), 
                            " (", round(bup_cont_pair_as[3, 5], 1), ", ",
                            round(bup_cont_pair_as[3, 6], 1), ")")

final_table[4, 3] <- paste0(round(bup_cont_pair_as[4, 4], 1), 
                            " (", round(bup_cont_pair_as[4, 5], 1), ", ",
                            round(bup_cont_pair_as[4, 6], 1), ")")

final_table[5, 3] <- paste0(round(met_cont_pair_as[1, 4], 1), 
                            " (", round(met_cont_pair_as[1, 5], 1), ", ",
                            round(met_cont_pair_as[1, 6], 1), ")")

final_table[6, 3] <- paste0(round(met_cont_pair_as[2, 4], 1), 
                            " (", round(met_cont_pair_as[2, 5], 1), ", ",
                            round(met_cont_pair_as[2, 6], 1), ")")

final_table[7, 3] <- paste0(round(met_cont_pair_as[3, 4], 1), 
                            " (", round(met_cont_pair_as[3, 5], 1), ", ",
                            round(met_cont_pair_as[3, 6], 1), ")")

final_table[8, 3] <- paste0(round(met_cont_pair_as[4, 4], 1), 
                            " (", round(met_cont_pair_as[4, 5], 1), ", ",
                            round(met_cont_pair_as[4, 6], 1), ")")

final_table[9, 3] <- paste0(round(comb_binomial_pair_as[1, 4], 2), 
                            " (", round(comb_binomial_pair_as[1, 5], 2), ", ",
                            round(comb_binomial_pair_as[1, 6], 2), ")")

final_table[10, 3] <- paste0(round(comb_binomial_pair_as[2, 4], 2), 
                             " (", round(comb_binomial_pair_as[2, 5], 2), ", ",
                             round(comb_binomial_pair_as[2, 6], 2), ")")

final_table[1, 5] <- paste0(round(bup_cont_pair_as[5, 4], 1), 
                            " (", round(bup_cont_pair_as[5, 5], 1), ", ",
                            round(bup_cont_pair_as[5, 6], 1), ")")

final_table[2, 5] <- paste0(round(bup_cont_pair_as[6, 4], 1), 
                            " (", round(bup_cont_pair_as[6, 5], 1), ", ",
                            round(bup_cont_pair_as[6, 6], 1), ")")

final_table[3, 5] <- paste0(round(bup_cont_pair_as[7, 4], 1), 
                            " (", round(bup_cont_pair_as[7, 5], 1), ", ",
                            round(bup_cont_pair_as[7, 6], 1), ")")

final_table[4, 5] <- paste0(round(bup_cont_pair_as[8, 4], 1), 
                            " (", round(bup_cont_pair_as[8, 5], 1), ", ",
                            round(bup_cont_pair_as[8, 6], 1), ")")

final_table[5, 5] <- paste0(round(met_cont_pair_as[5, 4], 1), 
                            " (", round(met_cont_pair_as[5, 5], 1), ", ",
                            round(met_cont_pair_as[5, 6], 1), ")")

final_table[6, 5] <- paste0(round(met_cont_pair_as[6, 4], 1), 
                            " (", round(met_cont_pair_as[6, 5], 1), ", ",
                            round(met_cont_pair_as[6, 6], 1), ")")

final_table[7, 5] <- paste0(round(met_cont_pair_as[7, 4], 1), 
                            " (", round(met_cont_pair_as[7, 5], 1), ", ",
                            round(met_cont_pair_as[7, 6], 1), ")")

final_table[8, 5] <- paste0(round(met_cont_pair_as[8, 4], 1), 
                            " (", round(met_cont_pair_as[8, 5], 1), ", ",
                            round(met_cont_pair_as[8, 6], 1), ")")

final_table[9, 5] <- paste0(round(comb_binomial_pair_as[3, 4], 2), 
                            " (", round(comb_binomial_pair_as[3, 5], 2), ", ",
                            round(comb_binomial_pair_as[3, 6], 2), ")")

final_table[10, 5] <- paste0(round(comb_binomial_pair_as[4, 4], 2), 
                             " (", round(comb_binomial_pair_as[4, 5], 2), ", ",
                             round(comb_binomial_pair_as[4, 6], 2), ")")

print(xtable(final_table))

view(final_table |>
         rename("NHW Mean" = "Mean",
                "NHB Mean" = "Mean.1",
                "Hisp. Mean" = "Mean.2",
                "NHB v. NHW" = "Difference",
                "Hisp. v. NHW" = "Difference.1"
                ))




