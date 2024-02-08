is_outlier <- function(dose) {
    q3 <- quantile(dose, probs = c(0.75))
    q1 <- quantile(dose, probs = c(0.25))
    low <- q1 - 1.5 * (q3 - q1)
    high <- q3 + 1.5 * (q3 - q1)
    val <- case_when(dose > high ~ 1,
                     dose < low ~ 1,
                     TRUE ~ 0)
    return(val)
}

data_bup <- readRDS(here::here("data/processed/data_bup_imputed.rds")) |>
    mutate(wk3.binary = ifelse(wk3.dose_this_week < 16, 0, 1),
           wk4.binary = ifelse(wk4.dose_this_week < 16, 0, 1)) |>
    #group_by(xrace) |>
    mutate(wk1.outlier = is_outlier(wk1.dose_this_week)) |>
    mutate(wk2.outlier = is_outlier(wk2.dose_this_week)) |>
    mutate(wk3.outlier = is_outlier(wk3.dose_this_week)) |>
    mutate(wk4.outlier = is_outlier(wk4.dose_this_week)) 


# appropriate dose if greater than 60 mg
data_met <- readRDS(here::here("data/processed/data_met_imputed.rds")) |>
    mutate(wk3.binary = ifelse(wk3.dose_this_week < 60, 0, 1),
           wk4.binary = ifelse(wk4.dose_this_week < 60, 0, 1)) |>
    #group_by(xrace) |>
    mutate(wk1.outlier = is_outlier(wk1.dose_this_week)) |>
    mutate(wk2.outlier = is_outlier(wk2.dose_this_week)) |>
    mutate(wk3.outlier = is_outlier(wk3.dose_this_week)) |>
    mutate(wk4.outlier = is_outlier(wk4.dose_this_week)) 
