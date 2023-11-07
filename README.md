# OUD Treatment Dosage Means by Race/Ethnicity

Code and Visualizations Related to Treatment Dosage by Race/Ethnicity

## Data Cleaning

1. White, non-Hispanic Black, and Hispanic patients were kept in the data for analysis
2. Patients who never initiated treatment were removed from the data
3. Patients were considered censored upon initial relapse 
4. Data were filtered to contain first four weeks of treatment

## Analysis

1. Unadjusted means were calculated for each treatment (t = 2) between groups (k = 3) across timepoints (j = 4)
2. Means adjusted for sex and age were calculated for each treatment (t = 2) between groups (k = 3) across timepoints (j = 4)
3. Means adjusted for additional baseline covariates were calculated for each treatment (t = 2) between groups (k = 3) across timepoints (j = 4)

- Software:
- Packages: The `lmtp` was used to calculate TMLE estimates of the means for adjusted models
- Superlearner candidates for adjusted models (10 folds): `mean`, `glm`, `earth`, `glmnet`, `gbm`, `bartMachine`

## Results
