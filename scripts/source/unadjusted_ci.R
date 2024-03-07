# input: dataframe for group 1 and dataframe for group 2
# output: dataframe containing differences as well as confidence intervals.

unadjusted_diff <- function(est_trt, est_ctl, weeks = 4, ci_level = 0.95,
                              ci_type) {

    #creating empty dataframe to store results
    
    all_weeks <- tibble(
        time = as.integer(0),
        conf.low = as.double(0),
        conf.high = as.double(0),
        theta = as.double(0),
        std.error = as.double(0),
        test_stat = as.double(0),
        p.value = as.double(0))
    
    if(ci_type == "bonf") #creating Bonferroni CI
    {
        for (i in (1:weeks))
        {
        est_trt_new <- est_trt |>
                filter(week == i)
            
        est_ctl_new <- est_ctl |>
                filter(week == i)
            
        n1 <- est_trt_new$uncensored
        n2 <- est_ctl_new$uncensored
        
        s1 <- est_trt_new$sd
        s2 <- est_ctl_new$sd
        
        sp <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2)/(n1 + n2 - 2)) #pooled standard dev
        
        se_pooled <- sqrt(sp^2 * ((1/n1) + (1/n2))) #pooled standard error
        
         conf.high <- (est_trt_new$avg - est_ctl_new$avg) + 
            qt(p=(1 - ci_level)/(2 * weeks),
               df =  n1 + n2 -2, lower.tail=FALSE) *
             se_pooled
        conf.low <- (est_trt_new$avg - est_ctl_new$avg) -
            qt(p=(1 - ci_level)/(2 * weeks), 
               df =  n1 + n2 -2, lower.tail=FALSE) *
            se_pooled
        
        theta <- est_trt_new$avg - est_ctl_new$avg #mean diff
        
        test_stat <- abs((theta - 0) / se_pooled) #test stat
        
        p.value <- pnorm(-test_stat) #p-value
        
        this_week <- tibble(
            time = i,
            conf.low = conf.low,
            conf.high = conf.high,
            theta = est_trt_new$avg - est_ctl_new$avg,
            std.error = se_pooled,
            test_stat = test_stat,
            p.value = p.value)
        
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
            
            n1 <- est_trt_new$uncensored
            n2 <- est_ctl_new$uncensored
            
            s1 <- est_trt_new$sd
            s2 <- est_ctl_new$sd
            
            se_pooled <- sqrt((s1^2/n1 + s2^2/n2)) #pooled standard error
            
            conf.high <- (est_trt_new$avg - est_ctl_new$avg) + 
                sqrt(qchisq(p = ci_level, df = weeks))*
                se_pooled
            conf.low <- (est_trt_new$avg - est_ctl_new$avg) -
                sqrt(qchisq(p = ci_level, df = weeks)) *
                se_pooled
            
            theta <- est_trt_new$avg - est_ctl_new$avg #mean diff
            
            test_stat <- abs((theta - 0) / se_pooled) #test stat
            
            p.value <- pnorm(-test_stat) #p-value
            
            this_week <- tibble(
                time = i,
                conf.low = conf.low,
                conf.high = conf.high,
                theta = est_trt_new$avg - est_ctl_new$avg,
                std.error = se_pooled,
                test_stat = test_stat,
                p.value = p.value)
            
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
            
            n1 <- est_trt_new$uncensored
            n2 <- est_ctl_new$uncensored
            
            s1 <- est_trt_new$sd
            s2 <- est_ctl_new$sd
            
            std.error <- sqrt((s1^2/n1) + (s2^2/n2)) #pooled standard error
            
            conf.high <- (est_trt_new$avg - est_ctl_new$avg) + 
                abs(qnorm(1 - (1 - ci_level)/2, lower.tail = TRUE)) *
                std.error
            conf.low <- (est_trt_new$avg - est_ctl_new$avg) -
                abs(qnorm(1 - (1 - ci_level)/2, lower.tail = TRUE)) *
                std.error
            
            theta <- est_trt_new$avg - est_ctl_new$avg #mean diff
            
            test_stat <- theta/std.error #test stat
            
            p.value = 2 * (1 - pnorm(abs(test_stat))) #p-value
            
            this_week <- tibble(
                time = i,
                conf.low = conf.low,
                conf.high = conf.high,
                theta = est_trt_new$avg - est_ctl_new$avg,
                std.error = std.error,
                test_stat = test_stat,
                p.value = p.value)
            
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
        conf.low = as.double(0),
        conf.high = as.double(0),
        theta = as.double(0),
        std.error = as.double(0),
        test_stat = as.double(0),
        p.value = as.double(0))
    
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
            
            theta <- p1 - p2 #prop diff
            
            std.error <- sqrt(((p1 * (1 - p1))/n1) + ((p2 * (1 - p2))/n2))

            conf.high <- theta + 
                abs(qnorm(1 - (1 - ci_level)/2, lower.tail = TRUE)) *
                std.error
            
            conf.low <- theta -
                abs(qnorm(1 - (1 - ci_level)/2, lower.tail = TRUE)) *
                std.error
            
            test_stat <- theta/std.error #test stat
            
            p.value = 2 * (1 - pnorm(abs(test_stat))) #p-value
            
            this_week <- tibble(
                time = i,
                conf.low = conf.low,
                conf.high = conf.high,
                theta = theta,
                std.error = std.error,
                test_stat = test_stat,
                p.value = p.value)
            
            all_weeks <- all_weeks |>
                rbind(this_week)
        }
    }
    all_weeks = all_weeks[-1,]
    return(all_weeks)
}
