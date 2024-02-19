#devtools::install_github("nt-williams/lmtp@separate-variable-sets") #version allowing diff var sets

library(tidyverse)
library(ggplot2)
library(lmtp)
library(SuperLearner)
source("scripts/source/shift_functions.R") #loading shift functions

wk3_dat <- readRDS(here::here("data/sensitivity/data_met_week3_outlier.rds"))
wk4_dat <- readRDS(here::here("data/sensitivity/data_met_week4_outlier.rds"))

## Methadone Models

# learners list

lrnrs_out_met <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.gbm", "SL.bartMachine") # must be capable of binary classification

lrnrs_trt_met <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.gbm", "SL.bartMachine") # must be capable of binary classification

### Continuous Analysis

## Met Analysis

race_tmle_list_cs_met <- list()
race_tmle_list_cs_met_limited <- list()

set.seed(1)

for(j in 1:3) # looping over race
{
    tmle_list_cs_met <- list()
    tmle_list_cs_met_limited <- list()
    
    for (i in 3:4) # looping over weeks (1-3 for now)
    {
        Y <- c(paste0(paste0("wk", i), ".dose_this_week")) # outcome (dose)
        
        C <- c(paste0(paste0("wk", i), ".censor")) # censor
        
        if(i == 3)
        {
            dat <- wk3_dat
        }
        else if(i == 4)
        {
            dat <- wk4_dat
        }

            B_met <- list(trt = c("sex", "age", "hcows_bin","alcdisorder", 
                                  "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                  "hasBipolar","hasAnxPan",
                                  "hasMajorDep",
                                  "bamphetamine30_base", "bcannabis30_base",
                                  "bbenzo30_base", "ivdrug"),
                          cens = c("sex", "age", "hcows_bin","alcdisorder", 
                                   "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                   "hasBipolar","hasAnxPan",
                                   "hasMajorDep",
                                   "bamphetamine30_base", "bcannabis30_base",
                                   "bbenzo30_base", "ivdrug",
                                   as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                          outcome = c("sex", "age", "hcows_bin","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug"))
            
            B_met_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows_bin","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug",
                                      as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                             outcome = c("sex", "age"))
            

        
        # expanded baseline covariates (see above)
        progressr::with_progress(tmle_list_cs_met[[i]] <- lmtp_tmle(
            data = dat, 
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
            mtp = FALSE))
        
        # limited baseline covariates (sex + age)
        progressr::with_progress(tmle_list_cs_met_limited[[i]] <- lmtp_tmle(
            data = dat, 
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
        saveRDS(tmle_list_cs_met[[i]], here::here(paste0("./data/sensitivity/","result_met", j, sep = "_", i, ".rds")))
        saveRDS(tmle_list_cs_met_limited[[i]], here::here(paste0("./data/sensitivity/","result_met_limited", j, sep = "_", i, ".rds")))
    }
    
    race_tmle_list_cs_met[[j]] <- tmle_list_cs_met
    race_tmle_list_cs_met_limited[[j]] <- tmle_list_cs_met_limited
}
