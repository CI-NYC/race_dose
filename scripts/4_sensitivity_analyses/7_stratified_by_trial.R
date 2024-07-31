#devtools::install_github("nt-williams/lmtp@separate-variable-sets") #separate variable sets
library(tidyverse)
library(ggplot2)
library(lmtp)
library(gtsummary)
library(SuperLearner)

source(here::here("scripts/source/shift_functions.R")) #loading shift functions

data_bup <- readRDS(here::here("data/processed/data_bup_imputed.rds"))

data_bup_27 <- data_bup |>
    filter(project == "27")

data_bup_30 <- data_bup |>
    filter(project == "30")

data_bup_51 <- data_bup |>
    filter(project == "51")

data_list <- list(data_bup_27, data_bup_30, data_bup_51)


# Censoring (week 4)

## Bup Analysis

lrnrs_out <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.xgboost", "SL.ranger") # must be capable of binary classification

lrnrs_trt <- c("SL.mean", "SL.glm", "SL.glmnet", "SL.earth", "SL.xgboost", "SL.ranger") # must be capable of binary classification

B_bup_as <- c("sex", "age") 

## Bup Analysis

# creating empty list to store results

race_tmle_list_cs_limited <- list()

set.seed(1)

for(z in 1:3) # looping over race
{
    
data_bup <- data_list[z] |>
        as.data.frame()
    
for(j in 1:3) # looping over race
{
    tmle_list_cs_limited <- list()
    
    for (i in 4:4) # looping over weeks (3-4 for now)
    {
        Y <- c(paste0(paste0("wk", i), ".dose_this_week")) # outcome (dose)
        
        C <- c(paste0(paste0("wk", i), ".censor")) # censor
        
        if (z == 3)
        {
            
            B_bup_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows_bin",
                                      "alcdisorder", 
                                      #"alcdisorder_missing", 
                                      "cocdisorder", 
                                      #"cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing",
                                      as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                             outcome = c("sex", "age"))
        }
        else if (z == 2)
        {
            B_bup_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows_bin",
                                      "alcdisorder", 
                                      #"alcdisorder_missing", 
                                      "cocdisorder",
                                      #"cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", #"ivdrug_missing",
                                      as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                             outcome = c("sex", "age"))
        }
        else{
            B_bup_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows_bin","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", #"ivdrug_missing",
                                      as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                             outcome = c("sex", "age"))
        }
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
        saveRDS(tmle_list_cs_limited[[i]], here::here(paste0("./data/trial_stratify/","result_newcensor_limited", j, sep = "_", i,
                                                             sep = "_", z, ".rds")))
    }
    
    #race_tmle_list_cs_limited[[j]] <- tmle_list_cs_limited
}
}
