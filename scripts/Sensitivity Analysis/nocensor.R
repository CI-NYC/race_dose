library(tidyverse)
library(ggplot2)
library(lmtp)
library(gtsummary)
library(SuperLearner)

source("scripts/source/shift_functions.R") #loading shift functions

## Reading Data

# appropriate dose if greater than 16 mg
data_bup <- readRDS(here::here("data/processed/data_bup_imputed.rds")) |>
    mutate(wk3.binary = ifelse(wk3.dose_this_week < 16, 0, 1),
           wk4.binary = ifelse(wk4.dose_this_week < 16, 0, 1))

# appropriate dose if greater than 60 mg
data_met <- readRDS(here::here("data/processed/data_met_imputed.rds")) |>
    mutate(wk3.binary = ifelse(wk3.dose_this_week < 60, 0, 1),
           wk4.binary = ifelse(wk4.dose_this_week < 60, 0, 1))

# combining bup and met for combined analysis
data_comb <- data_bup |>
    merge(data_met, all = TRUE)

# baseline bup covariates

B_bup <- c("sex", "age", "hcows","alcdisorder", 
           "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
           "hasBipolar","hasAnxPan",
           "hasMajorDep",
           "bamphetamine30_base", "bcannabis30_base",
           "bbenzo30_base", "ivdrug", "ivdrug_missing") 

# baseline methadone covariates

B_met <- c("sex", "age", "hcows","alcdisorder", 
           "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
           "hasBipolar","hasAnxPan",
           "hasMajorDep",
           "bamphetamine30_base", "bcannabis30_base",
           "bbenzo30_base", "ivdrug") 

## Methadone Models

# learners list

lrnrs_out_met <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.gbm", "SL.bartMachine") # must be capable of binary classification

lrnrs_trt_met <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.gbm", "SL.bartMachine") # must be capable of binary classification

## Bup Models

# learners list

lrnrs_out <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.gbm", "SL.bartMachine") # must be capable of continuous classification

lrnrs_trt <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.gbm", "SL.bartMachine") # must be capable of binary classification

### Continuous Analysis

## Met Analysis

set.seed(1)

for(j in 1:3) # looping over race
{
    tmle_list_cs_met <- list()
    tmle_list_cs_met_limited <- list()
    
    for (i in 1:4) # looping over weeks (1-3 for now)
    {
        Y <- c(paste0(paste0("wk", i), ".dose_this_week")) # outcome (dose)
        
        
        C <- c(paste0(paste0("wk", i), ".censor")) # censor
        
        # expanded baseline covariates (see above)
        progressr::with_progress(tmle_list_cs_met[[i]] <- lmtp_tmle(
            data = data_met, 
            trt = "xrace",
            cens = C,
            outcome = Y, 
            baseline = B_met, 
            shift = func_list[[j]], 
            outcome_type = "continuous", 
            learners_outcome = lrnrs_out_met,
            learners_trt = lrnrs_trt_met,
            folds = 10,
            .SL_folds = 10,
            mtp = TRUE)) # must be true for continuous outcome
        
        # limited baseline covariates (sex + age)
        progressr::with_progress(tmle_list_cs_met_limited[[i]] <- lmtp_tmle(
            data = data_met, 
            trt = "xrace",
            cens = C,
            outcome = Y, 
            baseline = c("sex", "age"), 
            shift = func_list[[j]], 
            outcome_type = "continuous", 
            learners_outcome = lrnrs_out_met,
            learners_trt = lrnrs_trt_met,
            folds = 10,
            .SL_folds = 10,
            mtp = TRUE)) # must be true for continuous outcome
        
        print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
        
        # saving results
        saveRDS(tmle_list_cs_met[[i]], here::here(paste0("./data/102623/","result_met", j, sep = "_", i, ".rds")))
        saveRDS(tmle_list_cs_met_limited[[i]], here::here(paste0("./data/102623/","result_met_limited", j, sep = "_", i, ".rds")))
    }
    
    race_tmle_list_cs_met[[j]] <- tmle_list_cs_met
    race_tmle_list_cs_met_limited[[j]] <- tmle_list_cs_met_limited
}

## Bup Analysis

# creating empty list to store results

race_tmle_list_cs <- list()
race_tmle_list_cs_limited <- list()

set.seed(1)

for(j in 1:3) # looping over race
{
    tmle_list_cs <- list()
    tmle_list_cs_limited <- list()
    
    for (i in 1:4) # looping over weeks (1-3 for now)
    {
        Y <- c(paste0(paste0("wk", i), ".dose_this_week")) # outcome (dose)
        
        
        C <- c(paste0(paste0("wk", i), ".censor")) # censor
        
        # expanded baseline covariates (see above)
        progressr::with_progress(tmle_list_cs[[i]] <- lmtp_tmle(
            data = data_bup, 
            trt = "xrace",
            cens = C,
            outcome = Y, 
            baseline = B_bup, 
            shift = func_list[[j]], 
            outcome_type = "continuous", 
            learners_outcome = lrnrs_out,
            learners_trt = lrnrs_trt,
            folds = 10,
            .SL_folds = 10,
            mtp = TRUE)) # must be true for continuous outcome
        
        # limited baseline covariates (sex + age)
        progressr::with_progress(tmle_list_cs_limited[[i]] <- lmtp_tmle(
            data = data_bup, 
            trt = "xrace",
            cens = C,
            outcome = Y, 
            baseline = c("sex", "age"), 
            shift = func_list[[j]], 
            outcome_type = "continuous", 
            learners_outcome = lrnrs_out,
            learners_trt = lrnrs_trt,
            folds = 10,
            .SL_folds = 10,
            mtp = TRUE)) # must be true for continuous outcome
        
        print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
        
        # saving results
        saveRDS(tmle_list_cs[[i]], here::here(paste0("./data/102623/","result", j, sep = "_", i, ".rds")))
        saveRDS(tmle_list_cs_limited[[i]], here::here(paste0("./data/102623/","result_limited", j, sep = "_", i, ".rds")))
    }
    
    race_tmle_list_cs[[j]] <- tmle_list_cs
    race_tmle_list_cs_limited[[j]] <- tmle_list_cs_limited
}

#### Binomial Analysis

## Met Analysis

race_tmle_list_cs_met<- list()
race_tmle_list_cs_met_limited <- list()

set.seed(1)

for(j in 1:3) # looping over race
{
    tmle_list_cs_met <- list()
    tmle_list_cs_met_limited <- list()
    
    for (i in 3:4) # looping over weeks (3-4 for now)
    {
        Y <- c(paste0(paste0("wk", i), ".binary")) # outcome (dose)
        
        
        C <- c(paste0(paste0("wk", i), ".censor")) # censor
        
        data <- data_met |>
            filter(eval(as.name(paste0(paste0("wk", i), ".censor"))) == 1)
        
        # all baseline covariates
        progressr::with_progress(tmle_list_cs_met[[i]] <- lmtp_tmle(
            data = data, 
            trt = "xrace",
            #cens = C,
            outcome = Y, 
            baseline = B_met, 
            shift = func_list[[j]], 
            outcome_type = "binomial", 
            learners_outcome = lrnrs_out_met,
            learners_trt = lrnrs_trt_met,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE)) 
        
        # limited baseline covariates (sex + age)
        progressr::with_progress(tmle_list_cs_met_limited[[i]] <- lmtp_tmle(
            data = data, 
            trt = "xrace",
            #cens = C,
            outcome = Y, 
            baseline = c("sex", "age"), 
            shift = func_list[[j]], 
            outcome_type = "binomial", 
            learners_outcome = lrnrs_out_met,
            learners_trt = lrnrs_trt_met,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE)) 
        
        print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
        
        # saving results
        saveRDS(tmle_list_cs_met[[i]], here::here(paste0("./data/102623/met_binomial/", "result_nocensor_met", j, sep = "_", i, ".rds")))
        saveRDS(tmle_list_cs_met_limited[[i]], here::here(paste0("./data/102623/met_binomial/", "result_nocensor_met_limited", j, sep = "_", i, ".rds")))
    }
    
    race_tmle_list_cs_met[[j]] <- tmle_list_cs_met
    race_tmle_list_cs_met_limited[[j]] <- tmle_list_cs_met_limited
}

## Bup Analysis

# creating empty list to store results
race_tmle_list_cs<- list()
race_tmle_list_cs_limited <- list()

set.seed(1)

for(j in 1:3) # looping over race
{
    tmle_list_cs <- list()
    tmle_list_cs_limited <- list()
    
    for (i in 3:4) # looping over weeks (3-4 for now)
    {
        Y <- c(paste0(paste0("wk", i), ".binary")) # outcome (dose)
        
        
        C <- c(paste0(paste0("wk", i), ".censor")) # censor
        
        data <- data_bup |>
            filter(eval(as.name(paste0(paste0("wk", i), ".censor"))) == 1)
        
        # all covariates
        progressr::with_progress(tmle_list_cs[[i]] <- lmtp_tmle(
            data = data, 
            trt = "xrace",
            #cens = C,
            outcome = Y, 
            baseline = B_bup, 
            shift = func_list[[j]], 
            outcome_type = "binomial", 
            learners_outcome = lrnrs_out,
            learners_trt = lrnrs_trt,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE)) 
        
        # limited baseline covariates (sex + age)
        progressr::with_progress(tmle_list_cs_limited[[i]] <- lmtp_tmle(
            data = data, 
            trt = "xrace",
            #cens = C,
            outcome = Y, 
            baseline = c("sex", "age"), 
            shift = func_list[[j]], 
            outcome_type = "binomial", 
            learners_outcome = lrnrs_out,
            learners_trt = lrnrs_trt,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE)) 
        
        print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
        
        # saving results
        saveRDS(tmle_list_cs[[i]], here::here(paste0("./data/102623/bup_binomial/","result_nocensor_", j, sep = "_", i, ".rds")))
        saveRDS(tmle_list_cs_limited[[i]], here::here(paste0("./data/102623/bup_binomial/","result_nocensor_limited", j, sep = "_", i, ".rds")))
    }
    race_tmle_list_cs[[j]] <- tmle_list_cs
    race_tmle_list_cs_limited[[j]] <- tmle_list_cs_limited
}

## Combined Analysis

# creating empty list to store results

race_tmle_list_cs <- list()
race_tmle_list_cs_limited <- list()

set.seed(1)

race_tmle_list_cs_comb<- list()
race_tmle_list_cs_comb_limited <- list()

set.seed(1)

for(j in 1:3) # looping over race
{
    tmle_list_cs_comb <- list()
    tmle_list_cs_comb_limited <- list()
    
    for (i in 3:4) # looping over weeks (3-4 for now)
    {
        Y <- c(paste0(paste0("wk", i), ".binary")) # outcome (dose)
        
        C <- c(paste0(paste0("wk", i), ".censor")) # censor
        
        data <- data_comb |>
            filter(eval(as.name(paste0(paste0("wk", i), ".censor"))) == 1)
        
        # all baseline covariates
        progressr::with_progress(tmle_list_cs_comb[[i]] <- lmtp_tmle(
            data = data, 
            trt = "xrace",
            #cens = C,
            outcome = Y, 
            baseline = B_bup, 
            shift = func_list[[j]], 
            outcome_type = "binomial", 
            learners_outcome = lrnrs_out,
            learners_trt = lrnrs_trt,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE)) 
        
        # limited baseline covariates (sex + age)
        progressr::with_progress(tmle_list_cs_comb_limited[[i]] <- lmtp_tmle(
            data = data, 
            trt = "xrace",
            #cens = C,
            outcome = Y, 
            baseline = c("sex", "age"), 
            shift = func_list[[j]], 
            outcome_type = "binomial", 
            learners_outcome = lrnrs_out,
            learners_trt = lrnrs_trt,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE)) 
        
        print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
        
        # saving results
        saveRDS(tmle_list_cs_comb[[i]], here::here(paste0("./data/102623/comb_binomial/", "result_nocensor_comb", j, sep = "_", i, ".rds")))
        saveRDS(tmle_list_cs_comb_limited[[i]], here::here(paste0("./data/102623/comb_binomial/", "result_nocensor_comb_limited", j, sep = "_", i, ".rds")))
    }
    
    race_tmle_list_cs_comb[[j]] <- tmle_list_cs_comb
    race_tmle_list_cs_comb_limited[[j]] <- tmle_list_cs_comb_limited
}

