# input: dataframe for group 1 and dataframe for group 2
# output: dataframe containing differences as well as confidence intervals.

unadjusted_diff <- function(est_trt, est_ctl, weeks = 4, ci_level = 0.95,
                              ci_type) {

    #creating empty dataframe to store results
    
    all_weeks <- tibble(
        time = as.integer(0),
        ci_lwr = as.double(0),
        ci_upr = as.double(0),
        effect_est = as.double(0),
        std_err = as.double(0),
        test_stat = as.double(0),
        pval = as.double(0))
    
    if(ci_type == "bonf") #creating Bonferroni CI
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
        
        sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2)) #pooled standard dev
        
        se_pooled <- sqrt(sp^2 * ((1/n1) + (1/n2))) #pooled standard error
        
         ci_upr <- (est_trt_new$avg - est_ctl_new$avg) + 
            qt(p=(1 - ci_level)/(2 * weeks),
               df =  n1 + n2 -2, lower.tail=FALSE) *
             se_pooled
        ci_lwr <- (est_trt_new$avg - est_ctl_new$avg) -
            qt(p=(1 - ci_level)/(2 * weeks), 
               df =  n1 + n2 -2, lower.tail=FALSE) *
            se_pooled
        
        effect_est <- est_trt_new$avg - est_ctl_new$avg #mean diff
        
        test_stat <- abs((effect_est - 0) / se_pooled) #test stat
        
        pval <- pnorm(-test_stat) #p-value
        
        this_week <- tibble(
            time = i,
            ci_lwr = ci_lwr,
            ci_upr = ci_upr,
            effect_est = est_trt_new$avg - est_ctl_new$avg,
            std_err = se_pooled,
            test_stat = test_stat,
            pval = pval)
        
        all_weeks <- all_weeks |>
            rbind(this_week)
        }
    }
    else if(ci_type == "simult") #creating simultaneous CI
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
            
            se_pooled <- sqrt((s1^2/n1 + s2^2/n2)) #pooled standard error
            
            ci_upr <- (est_trt_new$avg - est_ctl_new$avg) + 
                sqrt(qchisq(p = ci_level, df = weeks))*
                se_pooled
            ci_lwr <- (est_trt_new$avg - est_ctl_new$avg) -
                sqrt(qchisq(p = ci_level, df = weeks)) *
                se_pooled
            
            effect_est <- est_trt_new$avg - est_ctl_new$avg #mean diff
            
            test_stat <- abs((effect_est - 0) / se_pooled) #test stat
            
            pval <- pnorm(-test_stat) #p-value
            
            this_week <- tibble(
                time = i,
                ci_lwr = ci_lwr,
                ci_upr = ci_upr,
                effect_est = est_trt_new$avg - est_ctl_new$avg,
                std_err = se_pooled,
                test_stat = test_stat,
                pval = pval)
            
            all_weeks <- all_weeks |>
                rbind(this_week)
        }
    }
    else if (ci_type == "marginal") #non-FWER adjsuted
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
            
            std_err <- sqrt((s1^2/n1) + (s2^2/n2)) #pooled standard error
            
            ci_upr <- (est_trt_new$avg - est_ctl_new$avg) + 
                abs(qnorm(1 - (1 - ci_level)/2, lower.tail = TRUE)) *
                std_err
            ci_lwr <- (est_trt_new$avg - est_ctl_new$avg) -
                abs(qnorm(1 - (1 - ci_level)/2, lower.tail = TRUE)) *
                std_err
            
            effect_est <- est_trt_new$avg - est_ctl_new$avg #mean diff
            
            test_stat <- effect_est/std_err #test stat
            
            pval = 2 * (1 - pnorm(abs(test_stat))) #p-value
            
            this_week <- tibble(
                time = i,
                ci_lwr = ci_lwr,
                ci_upr = ci_upr,
                effect_est = est_trt_new$avg - est_ctl_new$avg,
                std_err = std_err,
                test_stat = test_stat,
                pval = pval)
            
            all_weeks <- all_weeks |>
                rbind(this_week)
        }
    }
    all_weeks = all_weeks[-1,]
    return(all_weeks)
}

unadjusted_diff_binom <- function(est_trt, est_ctl, weeks = 4, ci_level = 0.95,
                            ci_type = "marginal") {
    
    all_weeks <- tibble(
        time = as.integer(0),
        ci_lwr = as.double(0),
        ci_upr = as.double(0),
        effect_est = as.double(0),
        std_err = as.double(0),
        test_stat = as.double(0),
        pval = as.double(0))
    
    if (ci_type == "marginal") #non-FWER adjusted
    {
        for (i in (3:weeks))
        {
            est_trt_new <- est_trt |>
                filter(week == i)
            
            est_ctl_new <- est_ctl |>
                filter(week == i)
            
            p1 <- est_trt_new$prop
            p2 <- est_ctl_new$prop
            
            n1 <- est_trt_new$n
            n2 <- est_ctl_new$n
            
            effect_est <- p1 - p2 #prop diff
            
            std_err <- sqrt(((p1 * (1 - p1))/n1) + ((p2 * (1 - p2))/n2))

            ci_upr <- effect_est + 
                abs(qnorm(1 - (1 - ci_level)/2, lower.tail = TRUE)) *
                std_err
            
            ci_lwr <- effect_est -
                abs(qnorm(1 - (1 - ci_level)/2, lower.tail = TRUE)) *
                std_err
            
            test_stat <- effect_est/std_err #test stat
            
            pval = 2 * (1 - pnorm(abs(test_stat))) #p-value
            
            this_week <- tibble(
                time = i,
                ci_lwr = ci_lwr,
                ci_upr = ci_upr,
                effect_est = effect_est,
                std_err = std_err,
                test_stat = test_stat,
                pval = pval)
            
            all_weeks <- all_weeks |>
                rbind(this_week)
        }
    }
    all_weeks = all_weeks[-1,]
    return(all_weeks)
}
