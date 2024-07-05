# shifting race values to White (1)

race_shift_white <- function(data, xrace) { # return list changing treatment
    out <- list()
    a <- data[[xrace]]
    
    for (i in 1:length(a)) {
        if (as.character(a[i]) %in% c("2", "3")) {
            out[[i]] <- as.character("1")
        } else {
            out[[i]] <- as.character(a[i])
        }
    }
    factor(unlist(out), ordered = FALSE, levels = c("1", "2", "3")) # need this for glmnet
}

# shifting race values to Black (2)
race_shift_black <- function(data, xrace) {
    out <- list()
    a <- data[[xrace]]
    
    for (i in 1:length(a)) {
        if (as.character(a[i]) %in% c("1", "3")) {
            out[[i]] <- as.character("2")
        } else {
            out[[i]] <- as.character(a[i])
        }
    }
    factor(unlist(out), ordered = FALSE, levels = c("1", "2", "3")) # need this for glmnet
}

# shifting race values to Hispanic (3)

race_shift_hispanic <- function(data, xrace) {
    out <- list()
    a <- data[[xrace]]
    
    for (i in 1:length(a)) {
        if (as.character(a[i]) %in% c("1", "2")) {
            out[[i]] <- as.character("3")
        } else {
            out[[i]] <- as.character(a[i])
        }
    }
    factor(unlist(out), ordered = FALSE, levels = c("1", "2", "3")) # need this for glmnet
}

# function list to loop over

func_list <- c(race_shift_white, race_shift_black, race_shift_hispanic)

### Code from Nick Williams to calculate variance with ID

recalc_var_with_id <- function(x, id) {
    clusters <- split(x$eif, id)
    j <- length(clusters)
    se <- sqrt(var(vapply(clusters, function(x) mean(x), 1)) / j)
    x$low  <- x$theta - (qnorm(0.975) * se)
    x$high <- x$theta + (qnorm(0.975) * se)
    x$standard_error <- se
    x$id <- id
    x
}
