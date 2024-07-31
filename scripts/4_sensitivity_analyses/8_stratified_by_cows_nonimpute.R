#devtools::install_github("nt-williams/lmtp@separate-variable-sets") #separate variable sets
library(tidyverse)
library(ggplot2)
library(lmtp)
library(SuperLearner)

source(here::here("scripts/source/shift_functions.R")) #loading shift functions


## Reading Data

data_bup_nonimpute <- (readRDS(here::here("data/processed/data_bup.rds")) |>
    filter(is.na(hcows_bin)))$who

data_bup <- readRDS(here::here("data/processed/data_bup_imputed.rds")) |>
    filter(who %in% data_bup_nonimpute == FALSE)

data_bup_cows_1 <- data_bup |>
    filter(hcows_bin == 1)

data_bup_cows_1 |>
    group_by(xrace) |>
    summarize(count = n())

data_bup_cows_0 <- data_bup |>
    filter(hcows_bin == 0)

data_bup_cows_0 |>
    group_by(xrace) |>
    summarize(count = n())

data_bup_cows_0 |>
    filter(wk4.censor == 1) |>
    group_by(xrace) |>
    summarize(count = n())

bup_list <- list(data_bup_cows_1, data_bup_cows_0)

data_met_nonimpute <- (readRDS(here::here("data/processed/data_met.rds")) |>
                           filter(is.na(hcows_bin)))$who

data_met <- readRDS(here::here("data/processed/data_met_imputed.rds")) |>
    filter(who %in% data_met_nonimpute == FALSE)

data_met_cows_1 <- data_met |>
    filter(hcows_bin == 1)

data_met_cows_1 |>
    group_by(xrace) |>
    summarize(count = n())

data_met_cows_0 <- data_met |>
    filter(hcows_bin == 0)

data_met_cows_0 |>
    group_by(xrace) |>
    summarize(count = n())

met_list <- list(data_met_cows_1, data_met_cows_0)

data_comb_nonimpute <- c(data_bup_nonimpute, data_met_nonimpute)

data_comb <- readRDS(here::here("data/processed/data_comb_imputed.rds")) |>
    filter(who %in% data_comb_nonimpute == FALSE)

data_comb_cows_1 <- data_comb |>
    filter(hcows_bin == 1)

data_comb_cows_1 |>
    group_by(xrace) |>
    summarize(count = n())

data_comb_cows_0 <- data_comb |>
    filter(hcows_bin == 0)

data_comb_cows_0 |>
    group_by(xrace) |>
    summarize(count = n())

comb_list <- list(data_comb_cows_1, data_comb_cows_0)

B_bup_as <- c("sex", "age") 
B_met_as <- c("sex", "age") 
B_comb_as <- c("sex", "age") 

# learners list

lrnrs_out <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.xgboost", "SL.ranger") # must be capable of binary classification

lrnrs_trt <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.xgboost", "SL.ranger") # must be capable of binary classification

# Censoring (weeks 3-4)

## Bup Analysis

## Bup Analysis

# creating empty list to store results

race_tmle_list_cs_limited <- list()

set.seed(1)

for(z in 1:2)
{
    data_bup <- bup_list[[z]]
for(j in 1:3) # looping over race
{
    tmle_list_cs_limited <- list()
    
    for (i in 4:4) # looping over weeks (3-4 for now)
    {
        Y <- c(paste0(paste0("wk", i), ".dose_this_week")) # outcome (dose)
        
        C <- c(paste0(paste0("wk", i), ".censor")) # censor
        
        B_bup_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", #"hcows_bin",
                                      "alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing",
                                      as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                             outcome = c("sex", "age"))
        
        # limited baseline covariates (sex + age)
        progressr::with_progress(tmle_list_cs_limited[[i]] <- lmtp_tmle(
            data = data_bup, 
            trt = "xrace",
            cens = C,
            outcome = Y, 
            baseline = B_bup_as, 
            shift = func_list[[j]], 
            outcome_type = "continuous", 
            learners_outcome = lrnrs_out,
            learners_trt = lrnrs_trt,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE))
        
        print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
        
        # saving results
        saveRDS(tmle_list_cs_limited[[i]], here::here(paste0("./data/cows_nonimpute/","result_newcensor_limited", j, sep = "_", i, sep = "_", z, ".rds")))
    }
    
    race_tmle_list_cs_limited[[j]] <- tmle_list_cs_limited
}
}


## Met Analysis


## Met Analysis

race_tmle_list_cs_met_limited <- list()

set.seed(1)

for(z in 1:2)
{
    data_met <- met_list[[z]]
for(j in 1:3) # looping over race
{
    tmle_list_cs_met_limited <- list()
    
    for (i in 4:4) # looping over weeks (3-4 for now)
    {
        Y <- c(paste0(paste0("wk", i), ".dose_this_week")) # outcome (dose)
        
        C <- c(paste0(paste0("wk", i), ".censor")) # censor
        
        if (i > 1)
        {
            
            B_met_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", #"hcows_bin",
                                      "alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug",
                                      as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                             outcome = c("sex", "age"))
            
        }
        
     
        # limited baseline covariates (sex + age)
        progressr::with_progress(tmle_list_cs_met_limited[[i]] <- lmtp_tmle(
            data = data_met, 
            trt = "xrace",
            cens = C,
            outcome = Y, 
            baseline = B_met_as, 
            shift = func_list[[j]], 
            outcome_type = "continuous", 
            learners_outcome = lrnrs_out,
            learners_trt = lrnrs_trt,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE)) 
        
        print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
        
        # saving results
        saveRDS(tmle_list_cs_met_limited[[i]], here::here(paste0("./data/cows_nonimpute/","result_newcensor_met_limited", j, sep = "_", i, sep = "_", z, ".rds")))
    }
    
    race_tmle_list_cs_met_limited[[j]] <- tmle_list_cs_met_limited
}
}


##### COMBINED
race_tmle_list_cs_comb_limited <- list()

set.seed(1)

for(z in 1:2)
{
    data_comb <- comb_list[[z]]
for(j in 1:3) # looping over race
{
    tmle_list_cs_comb <- list()
    tmle_list_cs_comb_limited <- list()
    
    for (i in 4:4) # looping over weeks (3-4 for now)
    {
        Y <- c(paste0(paste0("wk", i), ".binary")) # outcome (dose)
        
        C <- c(paste0(paste0("wk", i), ".censor")) # censor
        
        if (i > 1)
        {
            B_comb_as <- list(trt = c("sex", "age"),
                              cens = c("sex", "age", #"hcows_bin",
                                       "alcdisorder", 
                                       "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                       "hasBipolar","hasAnxPan",
                                       "hasMajorDep",
                                       "bamphetamine30_base", "bcannabis30_base",
                                       "bbenzo30_base", "ivdrug", "ivdrug_missing",
                                       as.character(paste0(paste0("wk", i-1), ".dose_this_week")),
                                       "trt_ind"#indicator for bup/met
                                       ), 
                              outcome = c("sex", "age"))
        }
        
        # limited baseline covariates (sex + age)
        progressr::with_progress(tmle_list_cs_comb_limited[[i]] <- lmtp_tmle(
            data = data_comb, 
            trt = "xrace",
            cens = C,
            outcome = Y, 
            baseline = B_comb_as, 
            shift = func_list[[j]], 
            outcome_type = "binomial", 
            learners_outcome = lrnrs_out,
            learners_trt = lrnrs_trt,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE)) 
        
        print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
        
        # saving results
        saveRDS(tmle_list_cs_comb_limited[[i]], here::here(paste0("./data/cows_nonimpute/comb_binomial/", "result_newcensor_comb_limited", j, sep = "_", i, sep = "_", z, ".rds")))
    }
    
    race_tmle_list_cs_comb_limited[[j]] <- tmle_list_cs_comb_limited
}
}
