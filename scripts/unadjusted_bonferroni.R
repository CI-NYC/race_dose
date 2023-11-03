# input: dataframe for group 1 and dataframe for group 2
# output: dataframe containing differences as well as confidence intervals.

unadjusted_diff <- function(est_trt, est_ctl, weeks = 4, ci_level = 0.95,
                              ci_type = c("simult")) {

    all_weeks <- tibble(
        week = as.integer(0),
        ci_lwr = as.double(0),
        ci_upr = as.double(0),
        est = as.double(0),
        std_err = as.double(0),
        test_stat = as.double(0),
        pval = as.double(0))
    
    if(ci_type == "simult") #creating simultaneous CI
    {
        for (i in (1:weeks))
        {
        est_trt_new <- est_trt |>
                filter(week == i)
            
        est_ctl_new <- est_ctl |>
                filter(week == i)
            
        n1 <- est_trt_new$count
        n2 <- est_ctl_new$count
        
        s1 <- est_trt_new$sd
        s2 <- est_ctl_new$sd
        
        sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2))
        
        se_pooled <- sqrt(sp^2 * ((1/n1) + (1/n2)))
        
         ci_upr <- (est_trt_new$avg - est_ctl_new$avg) + 
            qt(p=(1 - ci_level)/(2 * weeks),
               df =  n1 + n2 -2, lower.tail=FALSE) *
             se_pooled
        ci_lwr <- (est_trt_new$avg - est_ctl_new$avg) -
            qt(p=(1 - ci_level)/(2 * weeks), 
               df =  n1 + n2 -2, lower.tail=FALSE) *
            se_pooled
        
        est <- est_trt_new$avg - est_ctl_new$avg
        
        test_stat <- abs(est / se_pooled)
        pval <- pnorm(-test_stat)
        
        week <- tibble(
            week = i,
            ci_lwr = ci_lwr,
            ci_upr = ci_upr,
            est = est_trt_new$avg - est_ctl_new$avg,
            std_err = se_pooled,
            test_stat = test_stat,
            pval = pval)
        
        all_weeks <- all_weeks |>
            rbind(week)
        }
    }
    return(all_weeks)
}
