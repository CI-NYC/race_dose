#devtools::install_github("nt-williams/lmtp@separate-variable-sets") #separate variable sets
library(tidyverse)
library(ggplot2)
library(lmtp)
library(SuperLearner)

source("scripts/source/shift_functions.R") #loading shift functions


## Reading Data

data_bup <- readRDS(here::here("data/processed/data_bup_imputed.rds"))

data_bup_cows_1 <- data_bup |>
    filter(hcows_bin == "1")

data_bup_cows_0 <- data_bup |>
    filter(hcows_bin == "0")

bup_list <- list(data_bup_cows_1, data_bup_cows_0)

data_met <- readRDS(here::here("data/processed/data_met_imputed.rds"))

data_met_cows_1 <- data_met |>
    filter(hcows_bin == "1")

data_met_cows_0 <- data_met |>
    filter(hcows_bin == "0")

met_list <- list(data_met_cows_1, data_met_cows_0)

data_comb <- readRDS(here::here("data/processed/data_comb_imputed.rds"))

data_comb_cows_1 <- data_comb |>
    filter(hcows_bin == "1")

data_comb_cows_0 <- data_comb |>
    filter(hcows_bin == "0")

comb_list <- list(data_comb_cows_1, data_comb_cows_0)

B_bup_as <- c("sex", "age") 
B_met_as <- c("sex", "age") 
B_comb_as <- c("sex", "age") 

# learners list

lrnrs_out <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.xgboost") # must be capable of binary classification

lrnrs_trt <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.xgboost") # must be capable of binary classification

# No censoring (weeks 1-2)
race_tmle_list_cs_met_limited <- list()

set.seed(1)

for(z in 1:2) # looping over race
{
    
    data_met <- met_list[z] |>
        as.data.frame()
    
    for(j in 1:3) # looping over race
    {
        tmle_list_cs_met_limited <- list()
        
        for (i in 1:2) # looping over weeks (3-4 for now)
        {
            Y <- c(paste0(paste0("wk", i), ".dose_this_week")) # outcome (dose)
            
            C <- c(paste0(paste0("wk", i), ".censor")) # censor
            
            # limited baseline covariates (sex + age)
            progressr::with_progress(tmle_list_cs_met_limited[[i]] <- lmtp_tmle(
                data = data_met, 
                trt = "xrace",
                cens = C,
                outcome = Y, 
                baseline = B_met_as, 
                shift = func_list[[j]], 
                outcome_type = "continuous", 
                learners_outcome = lrnrs_out_met,
                learners_trt = lrnrs_trt_met,
                folds = 10,
                mtp = FALSE))
            
            print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
            
            # saving results
            saveRDS(tmle_list_cs_met_limited[[i]], here::here(paste0("./data/cows/","result_met_newcensor_limited", j, sep = "_", i,
                                                                 sep = "_", z, ".rds")))
        }
        
        race_tmle_list_cs_met_limited[[j]] <- tmle_list_cs_met_limited
    }
}

race_tmle_list_cs_limited <- list()

set.seed(1)

for(z in 1:2) # looping over race
{
    
    data_bup <- bup_list[z] |>
        as.data.frame()
    
    for(j in 1:3) # looping over race
    {
        tmle_list_cs_limited <- list()
        
        for (i in 1:2) # looping over weeks (3-4 for now)
        {
            Y <- c(paste0(paste0("wk", i), ".dose_this_week")) # outcome (dose)
            
            C <- c(paste0(paste0("wk", i), ".censor")) # censor
            
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
                mtp = FALSE))
            
            print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
            
            # saving results
            saveRDS(tmle_list_cs_limited[[i]], here::here(paste0("./data/cows/","result_newcensor_limited", j, sep = "_", i,
                                                                 sep = "_", z, ".rds")))
        }
        
        race_tmle_list_cs_limited[[j]] <- tmle_list_cs_limited
    }
}

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
    
    for (i in 3:4) # looping over weeks (3-4 for now)
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
                                      as.character(paste0(paste0("wk", i-1), ".dose_this_week")),
                                      "project"),
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
        saveRDS(tmle_list_cs_limited[[i]], here::here(paste0("./data/cows/","result_newcensor_limited", j, sep = "_", i, sep = "_", z, ".rds")))
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
    
    for (i in 3:4) # looping over weeks (3-4 for now)
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
                                      as.character(paste0(paste0("wk", i-1), ".dose_this_week")),
                                      "project"),
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
            learners_outcome = lrnrs_out_met,
            learners_trt = lrnrs_trt_met,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE)) 
        
        print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
        
        # saving results
        saveRDS(tmle_list_cs_met_limited[[i]], here::here(paste0("./data/cows/","result_newcensor_met_limited", j, sep = "_", i, sep = "_", z, ".rds")))
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
    
    for (i in 3:4) # looping over weeks (3-4 for now)
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
                                       "trt_ind", #indicator for bup/met
                                       "project"), 
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
        saveRDS(tmle_list_cs_comb_limited[[i]], here::here(paste0("./data/cows/comb_binomial/", "result_newcensor_comb_limited", j, sep = "_", i, sep = "_", z, ".rds")))
    }
    
    race_tmle_list_cs_comb_limited[[j]] <- tmle_list_cs_comb_limited
}
}
