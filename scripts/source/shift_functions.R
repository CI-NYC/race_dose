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
