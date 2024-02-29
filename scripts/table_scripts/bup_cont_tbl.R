bup_cont_total <- bup_cont_total |>
    arrange(race, type, week)

bup_cont_pair <- bup_cont_pair |>
    select(time, race, type, effect_est, ci_lwr, ci_upr) |>
    arrange(race, type, time)

bup_cont_unadj_tbl <- data.frame(race = c("Black", "Hispanic", "White"),
                                 wk1 = c(0.00, 0.00, 0.00), 
                                 wk1diff = c(0.00, 0.00, 0.00), 
                                 wk2 = c(0.00, 0.00, 0.00), 
                                 wk2diff = c(0.00, 0.00, 0.00), 
                                 wk3 = c(0.00, 0.00, 0.00),
                                 wk3diff = c(0.00, 0.00, 0.00),
                                 wk4 = c(0.00, 0.00, 0.00), 
                                 wk4diff = c(0.00, 0.00, 0.00),
                                 type = c("UN", "UN", "UN"))

# Unadjusted 

# Black
bup_cont_unadj_tbl[1, 2] <- paste0(round(bup_cont_total[1, 3], 1), " ",
                                   "(", round(bup_cont_total[1, 5], 1), ", ", round(bup_cont_total[1, 6], 1), ")")
bup_cont_unadj_tbl[1, 4] <- paste0(round(bup_cont_total[2, 3], 1), " ",
                                   "(", round(bup_cont_total[2, 5], 1), ", ", round(bup_cont_total[2, 6], 1), ")")
bup_cont_unadj_tbl[1, 6] <- paste0(round(bup_cont_total[3, 3], 1), " ",
                                   "(", round(bup_cont_total[3, 5], 1), ", ", round(bup_cont_total[3, 6], 1), ")")
bup_cont_unadj_tbl[1, 8] <- paste0(round(bup_cont_total[4, 3], 1), " ",
                                   "(", round(bup_cont_total[4, 5], 1), ", ", round(bup_cont_total[4, 6], 1), ")")

bup_cont_unadj_tbl[1, 3] <- paste0(round(bup_cont_pair[1, 4], 1), " ",
                                   "(", round(bup_cont_pair[1, 5], 1), ", ", round(bup_cont_pair[1, 6], 1), ")")
bup_cont_unadj_tbl[1, 5] <- paste0(round(bup_cont_pair[2, 4], 1), " ",
                                   "(", round(bup_cont_pair[2, 5], 1), ", ", round(bup_cont_pair[2, 6], 1), ")")
bup_cont_unadj_tbl[1, 7] <- paste0(round(bup_cont_pair[3, 4], 1), " ",
                                   "(", round(bup_cont_pair[3, 5], 1), ", ", round(bup_cont_pair[3, 6], 1), ")")
bup_cont_unadj_tbl[1, 9] <- paste0(round(bup_cont_pair[4, 4], 1), " ",
                                   "(", round(bup_cont_pair[4, 5], 1), ", ", round(bup_cont_pair[4, 6], 1), ")")

# Hispanic
bup_cont_unadj_tbl[2, 2] <- paste0(round(bup_cont_total[13, 3], 1), " ",
                                   "(", round(bup_cont_total[13, 5], 1), ", ", round(bup_cont_total[13, 6], 1), ")")
bup_cont_unadj_tbl[2, 4] <- paste0(round(bup_cont_total[14, 3], 1), " ",
                                   "(", round(bup_cont_total[14, 5], 1), ", ", round(bup_cont_total[14, 6], 1), ")")
bup_cont_unadj_tbl[2, 6] <- paste0(round(bup_cont_total[15, 3], 1), " ",
                                   "(", round(bup_cont_total[15, 5], 1), ", ", round(bup_cont_total[15, 6], 1), ")")
bup_cont_unadj_tbl[2, 8] <- paste0(round(bup_cont_total[16, 3], 1), " ",
                                   "(", round(bup_cont_total[16, 5], 1), ", ", round(bup_cont_total[16, 6], 1), ")")

bup_cont_unadj_tbl[2, 3] <- paste0(round(bup_cont_pair[13, 4], 1), " ",
                                   "(", round(bup_cont_pair[13, 5], 1), ", ", round(bup_cont_pair[13, 6], 1), ")")
bup_cont_unadj_tbl[2, 5] <- paste0(round(bup_cont_pair[14, 4], 1), " ",
                                   "(", round(bup_cont_pair[14, 5], 1), ", ", round(bup_cont_pair[14, 6], 1), ")")
bup_cont_unadj_tbl[2, 7] <- paste0(round(bup_cont_pair[15, 4], 1), " ",
                                   "(", round(bup_cont_pair[15, 5], 1), ", ", round(bup_cont_pair[15, 6], 1), ")")
bup_cont_unadj_tbl[2, 9] <- paste0(round(bup_cont_pair[16, 4], 1), " ",
                                   "(", round(bup_cont_pair[16, 5], 1), ", ", round(bup_cont_pair[16, 6], 1), ")")

# White
bup_cont_unadj_tbl[3, 2] <- paste0(round(bup_cont_total[25, 3], 1), " ",
                                   "(", round(bup_cont_total[25, 5], 1), ", ", round(bup_cont_total[25, 6], 1), ")")
bup_cont_unadj_tbl[3, 4] <- paste0(round(bup_cont_total[26, 3], 1), " ",
                                   "(", round(bup_cont_total[26, 5], 1), ", ", round(bup_cont_total[26, 6], 1), ")")
bup_cont_unadj_tbl[3, 6] <- paste0(round(bup_cont_total[27, 3], 1), " ",
                                   "(", round(bup_cont_total[27, 5], 1), ", ", round(bup_cont_total[27, 6], 1), ")")
bup_cont_unadj_tbl[3, 8] <- paste0(round(bup_cont_total[28, 3], 1), " ",
                                   "(", round(bup_cont_total[28, 5], 1), ", ", round(bup_cont_total[28, 6], 1), ")")

bup_cont_as_tbl <- data.frame(race = c("Black", "Hispanic", "White"),
                              wk1 = c(0.00, 0.00, 0.00), 
                              wk1diff = c(0.00, 0.00, 0.00), 
                              wk2 = c(0.00, 0.00, 0.00), 
                              wk2diff = c(0.00, 0.00, 0.00), 
                              wk3 = c(0.00, 0.00, 0.00),
                              wk3diff = c(0.00, 0.00, 0.00),
                              wk4 = c(0.00, 0.00, 0.00), 
                              wk4diff = c(0.00, 0.00, 0.00),
                              type = c("AS", "AS", "AS"))

# Age + Sex

# Black
bup_cont_as_tbl[1, 2] <- paste0(round(bup_cont_total[5, 3], 1), " ",
                                "(", round(bup_cont_total[5, 5], 1), ", ", round(bup_cont_total[5, 6], 1), ")")
bup_cont_as_tbl[1, 4] <- paste0(round(bup_cont_total[6, 3], 1), " ",
                                "(", round(bup_cont_total[6, 5], 1), ", ", round(bup_cont_total[6, 6], 1), ")")
bup_cont_as_tbl[1, 6] <- paste0(round(bup_cont_total[7, 3], 1), " ",
                                "(", round(bup_cont_total[7, 5], 1), ", ", round(bup_cont_total[7, 6], 1), ")")
bup_cont_as_tbl[1, 8] <- paste0(round(bup_cont_total[8, 3], 1), " ",
                                "(", round(bup_cont_total[8, 5], 1), ", ", round(bup_cont_total[8, 6], 1), ")")

bup_cont_as_tbl[1, 3] <- paste0(round(bup_cont_pair[5, 4], 1), " ",
                                "(", round(bup_cont_pair[5, 5], 1), ", ", round(bup_cont_pair[5, 6], 1), ")")
bup_cont_as_tbl[1, 5] <- paste0(round(bup_cont_pair[6, 4], 1), " ",
                                "(", round(bup_cont_pair[6, 5], 1), ", ", round(bup_cont_pair[6, 6], 1), ")")
bup_cont_as_tbl[1, 7] <- paste0(round(bup_cont_pair[7, 4], 1), " ",
                                "(", round(bup_cont_pair[7, 5], 1), ", ", round(bup_cont_pair[7, 6], 1), ")")
bup_cont_as_tbl[1, 9] <- paste0(round(bup_cont_pair[8, 4], 1), " ",
                                "(", round(bup_cont_pair[8, 5], 1), ", ", round(bup_cont_pair[8, 6], 1), ")")

# Hispanic
bup_cont_as_tbl[2, 2] <- paste0(round(bup_cont_total[17, 3], 1), " ",
                                "(", round(bup_cont_total[17, 5], 1), ", ", round(bup_cont_total[17, 6], 1), ")")
bup_cont_as_tbl[2, 4] <- paste0(round(bup_cont_total[18, 3], 1), " ",
                                "(", round(bup_cont_total[18, 5], 1), ", ", round(bup_cont_total[18, 6], 1), ")")
bup_cont_as_tbl[2, 6] <- paste0(round(bup_cont_total[19, 3], 1), " ",
                                "(", round(bup_cont_total[19, 5], 1), ", ", round(bup_cont_total[19, 6], 1), ")")
bup_cont_as_tbl[2, 8] <- paste0(round(bup_cont_total[20, 3], 1), " ",
                                "(", round(bup_cont_total[20, 5], 1), ", ", round(bup_cont_total[20, 6], 1), ")")

bup_cont_as_tbl[2, 3] <- paste0(round(bup_cont_pair[17, 4], 1), " ",
                                "(", round(bup_cont_pair[17, 5], 1), ", ", round(bup_cont_pair[17, 6], 1), ")")
bup_cont_as_tbl[2, 5] <- paste0(round(bup_cont_pair[18, 4], 1), " ",
                                "(", round(bup_cont_pair[18, 5], 1), ", ", round(bup_cont_pair[18, 6], 1), ")")
bup_cont_as_tbl[2, 7] <- paste0(round(bup_cont_pair[19, 4], 1), " ",
                                "(", round(bup_cont_pair[19, 5], 1), ", ", round(bup_cont_pair[19, 6], 1), ")")
bup_cont_as_tbl[2, 9] <- paste0(round(bup_cont_pair[20, 4], 1), " ",
                                "(", round(bup_cont_pair[20, 5], 1), ", ", round(bup_cont_pair[20, 6], 1), ")")

# White
bup_cont_as_tbl[3, 2] <- paste0(round(bup_cont_total[29, 3], 1), " ",
                                "(", round(bup_cont_total[29, 5], 1), ", ", round(bup_cont_total[29, 6], 1), ")")
bup_cont_as_tbl[3, 4] <- paste0(round(bup_cont_total[30, 3], 1), " ",
                                "(", round(bup_cont_total[30, 5], 1), ", ", round(bup_cont_total[30, 6], 1), ")")
bup_cont_as_tbl[3, 6] <- paste0(round(bup_cont_total[31, 3], 1), " ",
                                "(", round(bup_cont_total[31, 5], 1), ", ", round(bup_cont_total[31, 6], 1), ")")
bup_cont_as_tbl[3, 8] <- paste0(round(bup_cont_total[32, 3], 1), " ",
                                "(", round(bup_cont_total[32, 5], 1), ", ", round(bup_cont_total[32, 6], 1), ")")

bup_cont_exp_tbl <- data.frame(race = c("Black", "Hispanic", "White"),
                               wk1 = c(0.00, 0.00, 0.00), 
                               wk1diff = c(0.00, 0.00, 0.00), 
                               wk2 = c(0.00, 0.00, 0.00), 
                               wk2diff = c(0.00, 0.00, 0.00), 
                               wk3 = c(0.00, 0.00, 0.00),
                               wk3diff = c(0.00, 0.00, 0.00),
                               wk4 = c(0.00, 0.00, 0.00), 
                               wk4diff = c(0.00, 0.00, 0.00),
                               type = c("EXP", "EXP", "EXP"))

# Black
bup_cont_exp_tbl[1, 2] <- paste0(round(bup_cont_total[9, 3], 1), " ",
                                 "(", round(bup_cont_total[9, 5], 1), ", ", round(bup_cont_total[9, 6], 1), ")")
bup_cont_exp_tbl[1, 4] <- paste0(round(bup_cont_total[10, 3], 1), " ",
                                 "(", round(bup_cont_total[10, 5], 1), ", ", round(bup_cont_total[10, 6], 1), ")")
bup_cont_exp_tbl[1, 6] <- paste0(round(bup_cont_total[11, 3], 1), " ",
                                 "(", round(bup_cont_total[11, 5], 1), ", ", round(bup_cont_total[11, 6], 1), ")")
bup_cont_exp_tbl[1, 8] <- paste0(round(bup_cont_total[12, 3], 1), " ",
                                 "(", round(bup_cont_total[12, 5], 1), ", ", round(bup_cont_total[12, 6], 1), ")")

bup_cont_exp_tbl[1, 3] <- paste0(round(bup_cont_pair[9, 4], 1), " ",
                                 "(", round(bup_cont_pair[9, 5], 1), ", ", round(bup_cont_pair[9, 6], 1), ")")
bup_cont_exp_tbl[1, 5] <- paste0(round(bup_cont_pair[10, 4], 1), " ",
                                 "(", round(bup_cont_pair[10, 5], 1), ", ", round(bup_cont_pair[10, 6], 1), ")")
bup_cont_exp_tbl[1, 7] <- paste0(round(bup_cont_pair[11, 4], 1), " ",
                                 "(", round(bup_cont_pair[11, 5], 1), ", ", round(bup_cont_pair[11, 6], 1), ")")
bup_cont_exp_tbl[1, 9] <- paste0(round(bup_cont_pair[12, 4], 1), " ",
                                 "(", round(bup_cont_pair[12, 5], 1), ", ", round(bup_cont_pair[12, 6], 1), ")")

# Hispanic
bup_cont_exp_tbl[2, 2] <- paste0(round(bup_cont_total[21, 3], 1), " ",
                                 "(", round(bup_cont_total[21, 5], 1), ", ", round(bup_cont_total[21, 6], 1), ")")
bup_cont_exp_tbl[2, 4] <- paste0(round(bup_cont_total[22, 3], 1), " ",
                                 "(", round(bup_cont_total[22, 5], 1), ", ", round(bup_cont_total[22, 6], 1), ")")
bup_cont_exp_tbl[2, 6] <- paste0(round(bup_cont_total[23, 3], 1), " ",
                                 "(", round(bup_cont_total[23, 5], 1), ", ", round(bup_cont_total[23, 6], 1), ")")
bup_cont_exp_tbl[2, 8] <- paste0(round(bup_cont_total[24, 3], 1), " ",
                                 "(", round(bup_cont_total[24, 5], 1), ", ", round(bup_cont_total[24, 6], 1), ")")

bup_cont_exp_tbl[2, 3] <- paste0(round(bup_cont_pair[21, 4], 1), " ",
                                 "(", round(bup_cont_pair[21, 5], 1), ", ", round(bup_cont_pair[21, 6], 1), ")")
bup_cont_exp_tbl[2, 5] <- paste0(round(bup_cont_pair[22, 4], 1), " ",
                                 "(", round(bup_cont_pair[22, 5], 1), ", ", round(bup_cont_pair[22, 6], 1), ")")
bup_cont_exp_tbl[2, 7] <- paste0(round(bup_cont_pair[23, 4], 1), " ",
                                 "(", round(bup_cont_pair[23, 5], 1), ", ", round(bup_cont_pair[23, 6], 1), ")")
bup_cont_exp_tbl[2, 9] <- paste0(round(bup_cont_pair[24, 4], 1), " ",
                                 "(", round(bup_cont_pair[24, 5], 1), ", ", round(bup_cont_pair[24, 6], 1), ")")

# White
bup_cont_exp_tbl[3, 2] <- paste0(round(bup_cont_total[33, 3], 1), " ",
                                 "(", round(bup_cont_total[33, 5], 1), ", ", round(bup_cont_total[33, 6], 1), ")")
bup_cont_exp_tbl[3, 4] <- paste0(round(bup_cont_total[34, 3], 1), " ",
                                 "(", round(bup_cont_total[34, 5], 1), ", ", round(bup_cont_total[34, 6], 1), ")")
bup_cont_exp_tbl[3, 6] <- paste0(round(bup_cont_total[35, 3], 1), " ",
                                 "(", round(bup_cont_total[35, 5], 1), ", ", round(bup_cont_total[35, 6], 1), ")")
bup_cont_exp_tbl[3, 8] <- paste0(round(bup_cont_total[36, 3], 1), " ",
                                 "(", round(bup_cont_total[36, 5], 1), ", ", round(bup_cont_total[36, 6], 1), ")")

bup_tbl_final <- bup_cont_unadj_tbl |>
    merge(bup_cont_as_tbl, all = TRUE) |>
    merge(bup_cont_exp_tbl, all = TRUE) |>
    mutate(type = factor(type, levels = c("UN", "AS", "EXP"))) |>
    relocate(type, .before = race) |>
    arrange(type, race) |>
    select(-type)

print(xtable(bup_tbl_final))

view(bup_tbl_final)
