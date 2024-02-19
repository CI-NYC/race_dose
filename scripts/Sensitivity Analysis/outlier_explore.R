is_outlier <- function(dose) {
    q3 <- quantile(dose, probs = c(0.75))
    q1 <- quantile(dose, probs = c(0.25))
    low <- q1 - 1.5 * (q3 - q1)
    high <- q3 + 1.5 * (q3 - q1)
    val <- case_when(dose > high ~ 1,
                     #dose < low ~ 1,
                     TRUE ~ 0)
    return(val)
}

data_bup <- readRDS(here::here("data/processed/data_bup_imputed.rds")) |>
    mutate(wk1.outlier = ifelse((is_outlier(wk1.dose_this_week) == 1 & wk1.censor == 1), 1, 0)) |>
    mutate(wk2.outlier = ifelse((is_outlier(wk2.dose_this_week) == 1 & wk2.censor == 1), 1, 0)) |>
    mutate(wk3.outlier = ifelse((is_outlier(wk3.dose_this_week) == 1 & wk3.censor == 1), 1, 0)) |>
    mutate(wk4.outlier = ifelse((is_outlier(wk4.dose_this_week) == 1 & wk4.censor == 1), 1, 0))

data_met <- readRDS(here::here("data/processed/data_met_imputed.rds"))

    #mutate(wk1.outlier = ifelse((is_outlier(wk1.dose_this_week) == 1 & wk1.censor == 1), 1, 0)) |>
    #mutate(wk2.outlier = ifelse((is_outlier(wk2.dose_this_week) == 1 & wk2.censor == 1), 1, 0)) |>
    #mutate(wk3.outlier = ifelse((is_outlier(wk3.dose_this_week) == 1 & wk3.censor == 1), 1, 0)) |>
    #mutate(wk4.outlier = ifelse((is_outlier(wk4.dose_this_week) == 1 & wk4.censor == 1), 1, 0))

data_met_3 <- data_met |> filter(wk3.censor == 1) |>
    mutate(wk3.outlier = ifelse(is_outlier(wk3.dose_this_week) == 1, 1, 0))

data_met_3 |>
    filter(wk3.outlier == 0) |>
    summarize(max_met = max(wk3.dose_this_week)) #120 mg

data_met_3 <- data_met_3 |>
    mutate(wk3.dose_this_week = ifelse(wk3.outlier == 1, 120, wk3.dose_this_week))

data_met_4 <- data_met |> filter(wk4.censor == 1) |>
    mutate(wk4.outlier = ifelse(is_outlier(wk4.dose_this_week) == 1, 1, 0))

data_met_4 |>
    filter(wk4.outlier == 0) |>
    summarize(max_met = max(wk4.dose_this_week)) #140 mg

data_met_4 <- data_met_4 |>
    mutate(wk4.dose_this_week = ifelse(wk4.outlier == 1, 140, wk4.dose_this_week))
