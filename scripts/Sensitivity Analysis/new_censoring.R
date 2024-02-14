#devtools::install_github("nt-williams/lmtp@separate-variable-sets") #version allowing diff var sets

library(tidyverse)
library(ggplot2)
library(lmtp)
library(SuperLearner)

source("scripts/source/shift_functions.R") #loading shift functions

## Reading Data

# appropriate dose if greater than 16 mg
data_bup <- readRDS(here::here("data/processed/data_bup_imputed.rds")) |>
    mutate(wk3.binary = ifelse(wk3.dose_this_week < 16, 0, 1),
           wk4.binary = ifelse(wk4.dose_this_week < 16, 0, 1)) |>
    mutate(type = 0)

# appropriate dose if greater than 60 mg
data_met <- readRDS(here::here("data/processed/data_met_imputed.rds")) |>
    mutate(wk3.binary = ifelse(wk3.dose_this_week < 60, 0, 1),
           wk4.binary = ifelse(wk4.dose_this_week < 60, 0, 1)) |>
    mutate(type = 1)

# combining bup and met for combined analysis
data_comb <- data_bup |>
    merge(data_met, all = TRUE) |>
    mutate(trt_ind = ifelse(type == 0, "0", "1")) |>
    mutate(trt_ind = factor(trt_ind))

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

race_tmle_list_cs_met <- list()
race_tmle_list_cs_met_limited <- list()

for(j in 1:3) # looping over race
{
    tmle_list_cs_met <- list()
    tmle_list_cs_met_limited <- list()
    
    for (i in 3:4) # looping over weeks (1-3 for now)
    {
        Y <- c(paste0(paste0("wk", i), ".dose_this_week")) # outcome (dose)
        
        C <- c(paste0(paste0("wk", i), ".censor")) # censor
        
        if (i > 1)
        {
            B_met <- list(trt = c("sex", "age", "hcows","alcdisorder", 
                                  "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                  "hasBipolar","hasAnxPan",
                                  "hasMajorDep",
                                  "bamphetamine30_base", "bcannabis30_base",
                                  "bbenzo30_base", "ivdrug"),
                          cens = c("sex", "age", "hcows","alcdisorder", 
                                   "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                   "hasBipolar","hasAnxPan",
                                   "hasMajorDep",
                                   "bamphetamine30_base", "bcannabis30_base",
                                   "bbenzo30_base", "ivdrug",
                                   as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                          outcome = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug"))

            B_met_as <- list(trt = c("sex", "age"),
                          cens = c("sex", "age", "hcows","alcdisorder", 
                                   "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                   "hasBipolar","hasAnxPan",
                                   "hasMajorDep",
                                   "bamphetamine30_base", "bcannabis30_base",
                                   "bbenzo30_base", "ivdrug",
                                   as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                          outcome = c("sex", "age"))

        }
        else
        {
            B_met <- list(trt = c("sex", "age", "hcows","alcdisorder", 
                                       "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                       "hasBipolar","hasAnxPan",
                                       "hasMajorDep",
                                       "bamphetamine30_base", "bcannabis30_base",
                                       "bbenzo30_base", "ivdrug"),
                          cens = c("sex", "age", "hcows","alcdisorder", 
                                   "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                   "hasBipolar","hasAnxPan",
                                   "hasMajorDep",
                                   "bamphetamine30_base", "bcannabis30_base",
                                   "bbenzo30_base", "ivdrug"),
                          outcome = c("sex", "age", "hcows","alcdisorder", 
                                           "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                           "hasBipolar","hasAnxPan",
                                           "hasMajorDep",
                                           "bamphetamine30_base", "bcannabis30_base",
                                           "bbenzo30_base", "ivdrug"))
            
            B_met_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug"),
                             outcome = c("sex", "age"))
                    }
        
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
            mtp = FALSE))
        
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
        saveRDS(tmle_list_cs_met[[i]], here::here(paste0("./data/102623/","result_newcensor_met", j, sep = "_", i, ".rds")))
        saveRDS(tmle_list_cs_met_limited[[i]], here::here(paste0("./data/102623/","result_newcensor_met_limited", j, sep = "_", i, ".rds")))
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
    
    for (i in 3:4) # looping over weeks (1-3 for now)
    {
        Y <- c(paste0(paste0("wk", i), ".dose_this_week")) # outcome (dose)
        
        C <- c(paste0(paste0("wk", i), ".censor")) # censor
        
        if (i > 1)
        {
            B_bup <- list(trt = c("sex", "age", "hcows","alcdisorder", 
                                  "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                  "hasBipolar","hasAnxPan",
                                  "hasMajorDep",
                                  "bamphetamine30_base", "bcannabis30_base",
                                  "bbenzo30_base", "ivdrug", "ivdrug_missing"),
                          cens = c("sex", "age", "hcows","alcdisorder", 
                                   "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                   "hasBipolar","hasAnxPan",
                                   "hasMajorDep",
                                   "bamphetamine30_base", "bcannabis30_base",
                                   "bbenzo30_base", "ivdrug", "ivdrug_missing",
                                   as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                          outcome = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing"))
            
            B_bup_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing",
                                      as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                             outcome = c("sex", "age"))
        }
        else
        {
            B_bup <- list(trt = c("sex", "age", "hcows","alcdisorder", 
                                  "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                  "hasBipolar","hasAnxPan",
                                  "hasMajorDep",
                                  "bamphetamine30_base", "bcannabis30_base",
                                  "bbenzo30_base", "ivdrug", "ivdrug_missing"),
                          cens = c("sex", "age", "hcows","alcdisorder", 
                                 "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                 "hasBipolar","hasAnxPan",
                                 "hasMajorDep",
                                 "bamphetamine30_base", "bcannabis30_base",
                                 "bbenzo30_base", "ivdrug", "ivdrug_missing"),
                          outcome = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing"))
            
            B_bup_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing"),
                             outcome = c("sex", "age"))
        }
        
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
            mtp = FALSE)) 
        
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
        saveRDS(tmle_list_cs[[i]], here::here(paste0("./data/102623/","result_newcensor", j, sep = "_", i, ".rds")))
        saveRDS(tmle_list_cs_limited[[i]], here::here(paste0("./data/102623/","result_newcensor_limited", j, sep = "_", i, ".rds")))
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
        
        if (i > 1)
        {
            B_met <- list(trt = c("sex", "age", "hcows","alcdisorder", 
                                  "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                  "hasBipolar","hasAnxPan",
                                  "hasMajorDep",
                                  "bamphetamine30_base", "bcannabis30_base",
                                  "bbenzo30_base", "ivdrug"),
                          cens = c("sex", "age", "hcows","alcdisorder", 
                                   "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                   "hasBipolar","hasAnxPan",
                                   "hasMajorDep",
                                   "bamphetamine30_base", "bcannabis30_base",
                                   "bbenzo30_base", "ivdrug",
                                   as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                          outcome = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug"))
            
            B_met_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug",
                                      as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                             outcome = c("sex", "age"))
            
        }
        else
        {
            B_met <- list(trt = c("sex", "age", "hcows","alcdisorder", 
                                  "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                  "hasBipolar","hasAnxPan",
                                  "hasMajorDep",
                                  "bamphetamine30_base", "bcannabis30_base",
                                  "bbenzo30_base", "ivdrug"),
                          cens = c("sex", "age", "hcows","alcdisorder", 
                                 "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                 "hasBipolar","hasAnxPan",
                                 "hasMajorDep",
                                 "bamphetamine30_base", "bcannabis30_base",
                                 "bbenzo30_base", "ivdrug"),
                          outcome = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug"))
            
            B_met_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug"),
                             outcome = c("sex", "age"))
        }
        
        # all baseline covariates
        progressr::with_progress(tmle_list_cs_met[[i]] <- lmtp_tmle(
            data = data_met, 
            trt = "xrace",
            cens = C,
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
            data = data_met, 
            trt = "xrace",
            cens = C,
            outcome = Y, 
            baseline = B_met_as, 
            shift = func_list[[j]], 
            outcome_type = "binomial", 
            learners_outcome = lrnrs_out_met,
            learners_trt = lrnrs_trt_met,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE)) 
        
        print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
        
        # saving results
        saveRDS(tmle_list_cs_met[[i]], here::here(paste0("./data/102623/met_binomial/", "result_newcensor_met", j, sep = "_", i, ".rds")))
        saveRDS(tmle_list_cs_met_limited[[i]], here::here(paste0("./data/102623/met_binomial/", "result_newcensor_met_limited", j, sep = "_", i, ".rds")))
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
        
        if (i > 1)
        {
            B_bup <- list(trt = c("sex", "age", "hcows","alcdisorder", 
                                  "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                  "hasBipolar","hasAnxPan",
                                  "hasMajorDep",
                                  "bamphetamine30_base", "bcannabis30_base",
                                  "bbenzo30_base", "ivdrug", "ivdrug_missing"),
                          cens = c("sex", "age", "hcows","alcdisorder", 
                                   "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                   "hasBipolar","hasAnxPan",
                                   "hasMajorDep",
                                   "bamphetamine30_base", "bcannabis30_base",
                                   "bbenzo30_base", "ivdrug", "ivdrug_missing",
                                   as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                          outcome = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing"))
            
            B_bup_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing",
                                      as.character(paste0(paste0("wk", i-1), ".dose_this_week"))),
                             outcome = c("sex", "age"))
        }
        else
        {
            B_bup <- list(trt = c("sex", "age", "hcows","alcdisorder", 
                                  "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                  "hasBipolar","hasAnxPan",
                                  "hasMajorDep",
                                  "bamphetamine30_base", "bcannabis30_base",
                                  "bbenzo30_base", "ivdrug", "ivdrug_missing"),
                          cens = c("sex", "age", "hcows","alcdisorder", 
                                 "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                 "hasBipolar","hasAnxPan",
                                 "hasMajorDep",
                                 "bamphetamine30_base", "bcannabis30_base",
                                 "bbenzo30_base", "ivdrug", "ivdrug_missing"),
                          outcome = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing"))
            
            B_bup_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing"),
                             outcome = c("sex", "age"))
        }
        
        # all covariates
        progressr::with_progress(tmle_list_cs[[i]] <- lmtp_tmle(
            data = data_bup, 
            trt = "xrace",
            cens = C,
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
            data = data_bup, 
            trt = "xrace",
            cens = C,
            outcome = Y, 
            baseline = B_bup_as,
            shift = func_list[[j]], 
            outcome_type = "binomial", 
            learners_outcome = lrnrs_out,
            learners_trt = lrnrs_trt,
            folds = 10,
            .SL_folds = 10,
            mtp = FALSE)) 
        
        print(paste0(j, sep = " ", i)) # to keep track of which race/week the loop is currently on
        
        # saving results
        saveRDS(tmle_list_cs[[i]], here::here(paste0("./data/102623/bup_binomial/","result_newcensor_", j, sep = "_", i, ".rds")))
        saveRDS(tmle_list_cs_limited[[i]], here::here(paste0("./data/102623/bup_binomial/","result_newcensor_limited", j, sep = "_", i, ".rds")))
    }
    race_tmle_list_cs[[j]] <- tmle_list_cs
    race_tmle_list_cs_limited[[j]] <- tmle_list_cs_limited
}

## Combined Analysis

# creating empty list to store results

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
            B_comb <- list(trt = c("sex", "age", "hcows","alcdisorder", 
                                  "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                  "hasBipolar","hasAnxPan",
                                  "hasMajorDep",
                                  "bamphetamine30_base", "bcannabis30_base",
                                  "bbenzo30_base", "ivdrug", "ivdrug_missing"),
                          cens = c("sex", "age", "hcows","alcdisorder", 
                                   "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                   "hasBipolar","hasAnxPan",
                                   "hasMajorDep",
                                   "bamphetamine30_base", "bcannabis30_base",
                                   "bbenzo30_base", "ivdrug", "ivdrug_missing",
                                   as.character(paste0(paste0("wk", i-1), ".dose_this_week")),
                                   "trt_ind"), #indicator for bup/met
                          outcome = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing"))
            
            B_comb_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing",
                                      as.character(paste0(paste0("wk", i-1), ".dose_this_week")),
                                      "trt_ind"), #indicator for bup/met
                             outcome = c("sex", "age"))
        }
        else
        {
            B_comb <- list(trt = c("sex", "age", "hcows","alcdisorder", 
                                  "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                  "hasBipolar","hasAnxPan",
                                  "hasMajorDep",
                                  "bamphetamine30_base", "bcannabis30_base",
                                  "bbenzo30_base", "ivdrug", "ivdrug_missing"),
                          cens = c("sex", "age", "hcows","alcdisorder", 
                                 "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                 "hasBipolar","hasAnxPan",
                                 "hasMajorDep",
                                 "bamphetamine30_base", "bcannabis30_base",
                                 "bbenzo30_base", "ivdrug", "ivdrug_missing"),
                          outcome = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing"))
            
            B_comb_as <- list(trt = c("sex", "age"),
                             cens = c("sex", "age", "hcows","alcdisorder", 
                                      "alcdisorder_missing", "cocdisorder", "cocdisorder_missing",
                                      "hasBipolar","hasAnxPan",
                                      "hasMajorDep",
                                      "bamphetamine30_base", "bcannabis30_base",
                                      "bbenzo30_base", "ivdrug", "ivdrug_missing"),
                             outcome = c("sex", "age"))
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
        saveRDS(tmle_list_cs_comb[[i]], here::here(paste0("./data/102623/comb_binomial/", "result_newcensor_comb", j, sep = "_", i, ".rds")))
        saveRDS(tmle_list_cs_comb_limited[[i]], here::here(paste0("./data/102623/comb_binomial/", "result_newcensor_comb_limited", j, sep = "_", i, ".rds")))
    }
    
    race_tmle_list_cs_comb[[j]] <- tmle_list_cs_comb
    race_tmle_list_cs_comb_limited[[j]] <- tmle_list_cs_comb_limited
}
