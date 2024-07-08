#devtools::install_github("nt-williams/lmtp@separate-variable-sets") #separate variable sets
library(tidyverse)
library(ggplot2)
library(lmtp)
library(SuperLearner)

source(here::here("scripts/source/shift_functions.R")) #loading shift functions

## Loading Data


# appropriate dose if greater than 16 mg
data_comb <- readRDS(here::here("data/processed/data_comb_imputed.rds")) |>
    filter(project != "30") |>
    mutate(project_51 = ifelse(project == "51", "1", "0")) |>
    mutate(project_51 = factor(project_51, levels = c("0", "1")))

## Bup Models


# learners list

lrnrs_out <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.xgboost") # must be capable of binary classification

lrnrs_trt <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.xgboost") # must be capable of binary classification

## Combined Models


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
        
        if (i > 1)
        {
            B_comb <- list(trt = c("sex", "age", "hcows_bin","alcdisorder", 
                                   "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                   "hasBipolar","hasAnxPan",
                                   "hasMajorDep",
                                   "bamphetamine30_base", "bcannabis30_base",
                                   "bbenzo30_base", "ivdrug", "ivdrug_missing", "project_51"),
                           cens = c("sex", "age", "hcows_bin","alcdisorder", 
                                    "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                    "hasBipolar","hasAnxPan",
                                    "hasMajorDep",
                                    "bamphetamine30_base", "bcannabis30_base",
                                    "bbenzo30_base", "ivdrug", "ivdrug_missing",
                                    as.character(paste0(paste0("wk", i-1), ".dose_this_week")),
                                    "trt_ind", "project_51"), #indicator for bup/met
                           outcome = c("sex", "age", "hcows_bin","alcdisorder", 
                                       "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                       "hasBipolar","hasAnxPan",
                                       "hasMajorDep",
                                       "bamphetamine30_base", "bcannabis30_base",
                                       "bbenzo30_base", "ivdrug", "ivdrug_missing", "project_51"))
            
            B_comb_as <- list(trt = c("sex", "age", "project_51"),
                              cens = c("sex", "age", "hcows_bin","alcdisorder", 
                                       "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                       "hasBipolar","hasAnxPan",
                                       "hasMajorDep",
                                       "bamphetamine30_base", "bcannabis30_base",
                                       "bbenzo30_base", "ivdrug", "ivdrug_missing",
                                       as.character(paste0(paste0("wk", i-1), ".dose_this_week")),
                                       "trt_ind", "project_51"), #indicator for bup/met
                              outcome = c("sex", "age", "project_51"))
        }
        else
        {
            B_comb <- list(trt = c("sex", "age", "hcows_bin","alcdisorder", 
                                   "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                   "hasBipolar","hasAnxPan",
                                   "hasMajorDep",
                                   "bamphetamine30_base", "bcannabis30_base",
                                   "bbenzo30_base", "ivdrug", "ivdrug_missing", "project_51"),
                           cens = c("sex", "age", "hcows_bin","alcdisorder", 
                                    "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                    "hasBipolar","hasAnxPan",
                                    "hasMajorDep",
                                    "bamphetamine30_base", "bcannabis30_base",
                                    "bbenzo30_base", "ivdrug", "ivdrug_missing", "project_51"),
                           outcome = c("sex", "age", "hcows_bin","alcdisorder", 
                                       "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                       "hasBipolar","hasAnxPan",
                                       "hasMajorDep",
                                       "bamphetamine30_base", "bcannabis30_base",
                                       "bbenzo30_base", "ivdrug", "ivdrug_missing", "project_51"))
            
            B_comb_as <- list(trt = c("sex", "age", "project_51"),
                              cens = c("sex", "age", "hcows_bin","alcdisorder", 
                                       "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                       "hasBipolar","hasAnxPan",
                                       "hasMajorDep",
                                       "bamphetamine30_base", "bcannabis30_base",
                                       "bbenzo30_base", "ivdrug", "ivdrug_missing", "project_51"),
                              outcome = c("sex", "age", "project_51"))
        }
        
        # all baseline covariates
        progressr::with_progress(tmle_list_cs_comb[[i]] <- lmtp_tmle(
            data = data_comb, 
            trt = "xrace",
            cens = C,
            outcome = Y, 
            baseline = B_comb,
            shift = func_list[[j]], 
            outcome_type = "binomial", 
            learners_outcome = lrnrs_out,
            learners_trt = lrnrs_trt,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE)) 
        
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
        saveRDS(tmle_list_cs_comb[[i]], here::here(paste0("./data/trial_indicator_drop30/comb_binomial/", "result_newcensor_comb", j, sep = "_", i, ".rds")))
        saveRDS(tmle_list_cs_comb_limited[[i]], here::here(paste0("./data/trial_indicator_drop30/comb_binomial/", "result_newcensor_comb_limited", j, sep = "_", i, ".rds")))
    }
    
    race_tmle_list_cs_comb[[j]] <- tmle_list_cs_comb
    race_tmle_list_cs_comb_limited[[j]] <- tmle_list_cs_comb_limited
}

