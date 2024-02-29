bup_binomial_total <- bup_binomial_total |>
    arrange(race, type, week)

bup_binomial_pair <- bup_binomial_pair |>
    select(time, race, type, effect_est, ci_lwr, ci_upr) |>
    arrange(race, type, time)

bup_binomial_unadj_tbl <- data.frame(race = c("Black", "Hispanic", "White"),
                                     wk3 = c(0.00, 0.00, 0.00),
                                     wk3diff = c(0.00, 0.00, 0.00),
                                     wk4 = c(0.00, 0.00, 0.00), 
                                     wk4diff = c(0.00, 0.00, 0.00),
                                     type = c("UN", "UN", "UN"))

# Unadjusted 

# Black
bup_binomial_unadj_tbl[1, 2] <- paste0(round(bup_binomial_total[1, 3], 2), " ",
                                       "(", round(bup_binomial_total[1, 5], 2), ", ", round(bup_binomial_total[1, 6], 2), ")")
bup_binomial_unadj_tbl[1, 4] <- paste0(round(bup_binomial_total[2, 3], 2), " ",
                                       "(", round(bup_binomial_total[2, 5], 2), ", ", round(bup_binomial_total[2, 6], 2), ")")

bup_binomial_unadj_tbl[1, 3] <- paste0(round(bup_binomial_pair[1, 4], 2), " ",
                                       "(", round(bup_binomial_pair[1, 5], 2), ", ", round(bup_binomial_pair[1, 6], 2), ")")
bup_binomial_unadj_tbl[1, 5] <- paste0(round(bup_binomial_pair[2, 4], 2), " ",
                                       "(", round(bup_binomial_pair[2, 5], 2), ", ", round(bup_binomial_pair[2, 6], 2), ")")

# Hispanic
bup_binomial_unadj_tbl[2, 2] <- paste0(round(bup_binomial_total[7, 3], 2), " ",
                                       "(", round(bup_binomial_total[7, 5], 2), ", ", round(bup_binomial_total[7, 6], 2), ")")
bup_binomial_unadj_tbl[2, 4] <- paste0(round(bup_binomial_total[8, 3], 2), " ",
                                       "(", round(bup_binomial_total[8, 5], 2), ", ", round(bup_binomial_total[8, 6], 2), ")")

bup_binomial_unadj_tbl[2, 3] <- paste0(round(bup_binomial_pair[7, 4], 2), " ",
                                       "(", round(bup_binomial_pair[7, 5], 2), ", ", round(bup_binomial_pair[7, 6], 2), ")")
bup_binomial_unadj_tbl[2, 5] <- paste0(round(bup_binomial_pair[8, 4], 2), " ",
                                       "(", round(bup_binomial_pair[8, 5], 2), ", ", round(bup_binomial_pair[8, 6], 2), ")")

# White
bup_binomial_unadj_tbl[3, 2] <- paste0(round(bup_binomial_total[13, 3], 2), " ",
                                       "(", round(bup_binomial_total[13, 5], 2), ", ", round(bup_binomial_total[13, 6], 2), ")")
bup_binomial_unadj_tbl[3, 4] <- paste0(round(bup_binomial_total[14, 3], 2), " ",
                                       "(", round(bup_binomial_total[14, 5], 2), ", ", round(bup_binomial_total[14, 6], 2), ")")

bup_binomial_as_tbl <- data.frame(race = c("Black", "Hispanic", "White"),
                                  wk3 = c(0.00, 0.00, 0.00),
                                  wk3diff = c(0.00, 0.00, 0.00),
                                  wk4 = c(0.00, 0.00, 0.00), 
                                  wk4diff = c(0.00, 0.00, 0.00),
                                  type = c("AS", "AS", "AS"))

# Unadjusted 

# Black
bup_binomial_as_tbl[1, 2] <- paste0(round(bup_binomial_total[3, 3], 2), " ",
                                    "(", round(bup_binomial_total[3, 5], 2), ", ", round(bup_binomial_total[3, 6], 2), ")")
bup_binomial_as_tbl[1, 4] <- paste0(round(bup_binomial_total[4, 3], 2), " ",
                                    "(", round(bup_binomial_total[4, 5], 2), ", ", round(bup_binomial_total[4, 6], 2), ")")

bup_binomial_as_tbl[1, 3] <- paste0(round(bup_binomial_pair[3, 4], 2), " ",
                                    "(", round(bup_binomial_pair[3, 5], 2), ", ", round(bup_binomial_pair[3, 6], 2), ")")
bup_binomial_as_tbl[1, 5] <- paste0(round(bup_binomial_pair[4, 4], 2), " ",
                                    "(", round(bup_binomial_pair[4, 5], 2), ", ", round(bup_binomial_pair[4, 6], 2), ")")

# Hispanic
bup_binomial_as_tbl[2, 2] <- paste0(round(bup_binomial_total[9, 3], 2), " ",
                                    "(", round(bup_binomial_total[9, 5], 2), ", ", round(bup_binomial_total[9, 6], 2), ")")
bup_binomial_as_tbl[2, 4] <- paste0(round(bup_binomial_total[10, 3], 2), " ",
                                    "(", round(bup_binomial_total[10, 5], 2), ", ", round(bup_binomial_total[10, 6], 2), ")")

bup_binomial_as_tbl[2, 3] <- paste0(round(bup_binomial_pair[9, 4], 2), " ",
                                    "(", round(bup_binomial_pair[9, 5], 2), ", ", round(bup_binomial_pair[9, 6], 2), ")")
bup_binomial_as_tbl[2, 5] <- paste0(round(bup_binomial_pair[10, 4], 2), " ",
                                    "(", round(bup_binomial_pair[10, 5], 2), ", ", round(bup_binomial_pair[10, 6], 2), ")")

# White
bup_binomial_as_tbl[3, 2] <- paste0(round(bup_binomial_total[15, 3], 2), " ",
                                    "(", round(bup_binomial_total[15, 5], 2), ", ", round(bup_binomial_total[15, 6], 2), ")")
bup_binomial_as_tbl[3, 4] <- paste0(round(bup_binomial_total[16, 3], 2), " ",
                                    "(", round(bup_binomial_total[16, 5], 2), ", ", round(bup_binomial_total[16, 6], 2), ")")

bup_binomial_exp_tbl <- data.frame(race = c("Black", "Hispanic", "White"),
                                   wk3 = c(0.00, 0.00, 0.00),
                                   wk3diff = c(0.00, 0.00, 0.00),
                                   wk4 = c(0.00, 0.00, 0.00), 
                                   wk4diff = c(0.00, 0.00, 0.00),
                                   type = c("EXP", "EXP", "EXP"))

# Black
bup_binomial_exp_tbl[1, 2] <- paste0(round(bup_binomial_total[5, 3], 2), " ",
                                     "(", round(bup_binomial_total[5, 5], 2), ", ", round(bup_binomial_total[5, 6], 2), ")")
bup_binomial_exp_tbl[1, 4] <- paste0(round(bup_binomial_total[6, 3], 2), " ",
                                     "(", round(bup_binomial_total[6, 5], 2), ", ", round(bup_binomial_total[6, 6], 2), ")")

bup_binomial_exp_tbl[1, 3] <- paste0(round(bup_binomial_pair[5, 4], 2), " ",
                                     "(", round(bup_binomial_pair[5, 5], 2), ", ", round(bup_binomial_pair[5, 6], 2), ")")
bup_binomial_exp_tbl[1, 5] <- paste0(round(bup_binomial_pair[6, 4], 2), " ",
                                     "(", round(bup_binomial_pair[6, 5], 2), ", ", round(bup_binomial_pair[6, 6], 2), ")")

# Hispanic
bup_binomial_exp_tbl[2, 2] <- paste0(round(bup_binomial_total[11, 3], 2), " ",
                                     "(", round(bup_binomial_total[11, 5], 2), ", ", round(bup_binomial_total[11, 6], 2), ")")
bup_binomial_exp_tbl[2, 4] <- paste0(round(bup_binomial_total[12, 3], 2), " ",
                                     "(", round(bup_binomial_total[12, 5], 2), ", ", round(bup_binomial_total[12, 6], 2), ")")

bup_binomial_exp_tbl[2, 3] <- paste0(round(bup_binomial_pair[11, 4], 2), " ",
                                     "(", round(bup_binomial_pair[11, 5], 2), ", ", round(bup_binomial_pair[11, 6], 2), ")")
bup_binomial_exp_tbl[2, 5] <- paste0(round(bup_binomial_pair[12, 4], 2), " ",
                                     "(", round(bup_binomial_pair[12, 5], 2), ", ", round(bup_binomial_pair[12, 6], 2), ")")

# White
bup_binomial_exp_tbl[3, 2] <- paste0(round(bup_binomial_total[17, 3], 2), " ",
                                     "(", round(bup_binomial_total[17, 5], 2), ", ", round(bup_binomial_total[17, 6], 2), ")")
bup_binomial_exp_tbl[3, 4] <- paste0(round(bup_binomial_total[18, 3], 2), " ",
                                     "(", round(bup_binomial_total[18, 5], 2), ", ", round(bup_binomial_total[18, 6], 2), ")")

bup_tbl_final_binomial <- bup_binomial_unadj_tbl |>
    merge(bup_binomial_as_tbl, all = TRUE) |>
    merge(bup_binomial_exp_tbl, all = TRUE) |>
    mutate(type = factor(type, levels = c("UN", "AS", "EXP"))) |>
    relocate(type, .before = race) |>
    arrange(type, race) |>
    select(-type)

print(xtable(bup_tbl_final_binomial))

view(bup_tbl_final_binomial)
