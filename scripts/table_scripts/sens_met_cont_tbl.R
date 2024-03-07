library(xtable)

met_cont_total <- met_cont_total |>
    arrange(race, type, week)

met_cont_pair <- met_cont_pair |>
    select(time, race, type, theta, conf.low, conf.high) |>
    arrange(race, type, time)

met_cont_unadj_tbl <- data.frame(race = c("Black", "Hispanic", "White"),
                 wk3 = c(0.00, 0.00, 0.00),
                 wk3diff = c(0.00, 0.00, 0.00),
                 wk4 = c(0.00, 0.00, 0.00), 
                 wk4diff = c(0.00, 0.00, 0.00),
                 type = c("UN", "UN", "UN"))

# Unadjusted 

# Black
met_cont_unadj_tbl[1, 2] <- paste0(round(met_cont_total[1, 3], 1), " ",
                                    "(", round(met_cont_total[1, 5], 1), ", ", round(met_cont_total[1, 6], 1), ")")
met_cont_unadj_tbl[1, 4] <- paste0(round(met_cont_total[2, 3], 1), " ",
                                    "(", round(met_cont_total[2, 5], 1), ", ", round(met_cont_total[2, 6], 1), ")")

met_cont_unadj_tbl[1, 3] <- paste0(round(met_cont_pair[1, 4], 1), " ",
                                    "(", round(met_cont_pair[1, 5], 1), ", ", round(met_cont_pair[1, 6], 1), ")")
met_cont_unadj_tbl[1, 5] <- paste0(round(met_cont_pair[2, 4], 1), " ",
                                    "(", round(met_cont_pair[2, 5], 1), ", ", round(met_cont_pair[2, 6], 1), ")")

# Hispanic
met_cont_unadj_tbl[2, 2] <- paste0(round(met_cont_total[7, 3], 1), " ",
                                    "(", round(met_cont_total[7, 5], 1), ", ", round(met_cont_total[7, 6], 1), ")")
met_cont_unadj_tbl[2, 4] <- paste0(round(met_cont_total[8, 3], 1), " ",
                                    "(", round(met_cont_total[8, 5], 1), ", ", round(met_cont_total[8, 6], 1), ")")

met_cont_unadj_tbl[2, 3] <- paste0(round(met_cont_pair[7, 4], 1), " ",
                                    "(", round(met_cont_pair[7, 5], 1), ", ", round(met_cont_pair[7, 6], 1), ")")
met_cont_unadj_tbl[2, 5] <- paste0(round(met_cont_pair[8, 4], 1), " ",
                                    "(", round(met_cont_pair[8, 5], 1), ", ", round(met_cont_pair[8, 6], 1), ")")


# White
met_cont_unadj_tbl[3, 2] <- paste0(round(met_cont_total[13, 3], 1), " ",
                                    "(", round(met_cont_total[13, 5], 1), ", ", round(met_cont_total[13, 6], 1), ")")
met_cont_unadj_tbl[3, 4] <- paste0(round(met_cont_total[14, 3], 1), " ",
                                    "(", round(met_cont_total[14, 5], 1), ", ", round(met_cont_total[14, 6], 1), ")")

met_cont_as_tbl <- data.frame(race = c("Black", "Hispanic", "White"),
                              wk3 = c(0.00, 0.00, 0.00),
                              wk3diff = c(0.00, 0.00, 0.00),
                              wk4 = c(0.00, 0.00, 0.00), 
                              wk4diff = c(0.00, 0.00, 0.00),
                              type = c("AS", "AS", "AS"))

# Age + Sex

# Black
met_cont_as_tbl[1, 2] <- paste0(round(met_cont_total[3, 3], 1), " ",
                                "(", round(met_cont_total[3, 5], 1), ", ", round(met_cont_total[3, 6], 1), ")")
met_cont_as_tbl[1, 4] <- paste0(round(met_cont_total[4, 3], 1), " ",
                                "(", round(met_cont_total[4, 5], 1), ", ", round(met_cont_total[4, 6], 1), ")")

met_cont_as_tbl[1, 3] <- paste0(round(met_cont_pair[3, 4], 1), " ",
                                "(", round(met_cont_pair[3, 5], 1), ", ", round(met_cont_pair[3, 6], 1), ")")
met_cont_as_tbl[1, 5] <- paste0(round(met_cont_pair[4, 4], 1), " ",
                                "(", round(met_cont_pair[4, 5], 1), ", ", round(met_cont_pair[4, 6], 1), ")")


# Hispanic
met_cont_as_tbl[2, 2] <- paste0(round(met_cont_total[9, 3], 1), " ",
                                "(", round(met_cont_total[9, 5], 1), ", ", round(met_cont_total[9, 6], 1), ")")
met_cont_as_tbl[2, 4] <- paste0(round(met_cont_total[10, 3], 1), " ",
                                "(", round(met_cont_total[10, 5], 1), ", ", round(met_cont_total[10, 6], 1), ")")

met_cont_as_tbl[2, 3] <- paste0(round(met_cont_pair[9, 4], 1), " ",
                                "(", round(met_cont_pair[9, 5], 1), ", ", round(met_cont_pair[9, 6], 1), ")")
met_cont_as_tbl[2, 5] <- paste0(round(met_cont_pair[10, 4], 1), " ",
                                "(", round(met_cont_pair[10, 5], 1), ", ", round(met_cont_pair[10, 6], 1), ")")

# White
met_cont_as_tbl[3, 2] <- paste0(round(met_cont_total[15, 3], 1), " ",
                                "(", round(met_cont_total[15, 5], 1), ", ", round(met_cont_total[15, 6], 1), ")")
met_cont_as_tbl[3, 4] <- paste0(round(met_cont_total[16, 3], 1), " ",
                                "(", round(met_cont_total[16, 5], 1), ", ", round(met_cont_total[16, 6], 1), ")")


met_cont_exp_tbl <- data.frame(race = c("Black", "Hispanic", "White"),
                               wk3 = c(0.00, 0.00, 0.00),
                               wk3diff = c(0.00, 0.00, 0.00),
                               wk4 = c(0.00, 0.00, 0.00), 
                               wk4diff = c(0.00, 0.00, 0.00),
                               type = c("EXP", "EXP", "EXP"))

# Black
met_cont_exp_tbl[1, 2] <- paste0(round(met_cont_total[5, 3], 1), " ",
                                 "(", round(met_cont_total[5, 5], 1), ", ", round(met_cont_total[5, 6], 1), ")")
met_cont_exp_tbl[1, 4] <- paste0(round(met_cont_total[6, 3], 1), " ",
                                 "(", round(met_cont_total[6, 5], 1), ", ", round(met_cont_total[6, 6], 1), ")")

met_cont_exp_tbl[1, 3] <- paste0(round(met_cont_pair[5, 4], 1), " ",
                                 "(", round(met_cont_pair[5, 5], 1), ", ", round(met_cont_pair[5, 6], 1), ")")
met_cont_exp_tbl[1, 5] <- paste0(round(met_cont_pair[6, 4], 1), " ",
                                 "(", round(met_cont_pair[6, 5], 1), ", ", round(met_cont_pair[6, 6], 1), ")")

# Hispanic
met_cont_exp_tbl[2, 2] <- paste0(round(met_cont_total[11, 3], 1), " ",
                                 "(", round(met_cont_total[11, 5], 1), ", ", round(met_cont_total[11, 6], 1), ")")
met_cont_exp_tbl[2, 4] <- paste0(round(met_cont_total[12, 3], 1), " ",
                                 "(", round(met_cont_total[12, 5], 1), ", ", round(met_cont_total[12, 6], 1), ")")

met_cont_exp_tbl[2, 3] <- paste0(round(met_cont_pair[11, 4], 1), " ",
                                 "(", round(met_cont_pair[11, 5], 1), ", ", round(met_cont_pair[11, 6], 1), ")")
met_cont_exp_tbl[2, 5] <- paste0(round(met_cont_pair[12, 4], 1), " ",
                                 "(", round(met_cont_pair[12, 5], 1), ", ", round(met_cont_pair[12, 6], 1), ")")


# White
met_cont_exp_tbl[3, 2] <- paste0(round(met_cont_total[17, 3], 1), " ",
                                 "(", round(met_cont_total[17, 5], 1), ", ", round(met_cont_total[17, 6], 1), ")")
met_cont_exp_tbl[3, 4] <- paste0(round(met_cont_total[18, 3], 1), " ",
                                 "(", round(met_cont_total[18, 5], 1), ", ", round(met_cont_total[18, 6], 1), ")")

met_tbl_final <- met_cont_unadj_tbl |>
    merge(met_cont_as_tbl, all = TRUE) |>
    merge(met_cont_exp_tbl, all = TRUE) |>
    mutate(type = factor(type, levels = c("UN", "AS", "EXP"))) |>
    relocate(type, .before = race) |>
    arrange(type, race) |>
    select(-type)

print(xtable(met_tbl_final))

view(met_tbl_final)

