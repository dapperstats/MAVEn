#' Calculate median CO2(ul/h)
#'
#' @param x list of CO2 values from MAVEn dataset
#'
#' @return
#'
co2_convertion_median <- function(x) {
    co2.ul.h <- median(x) * 1000 * 60
    return(co2.ul.h)
}

#' Convert CO2 MAVEn measurement to CO2 (ul/h)
#'
#' @param x list of CO2 values from MAVEn dataset
#'
#' @return
#'
co2_convertion <- function(x) {
    co2.ul.h <- x * 1000 * 60
    return(co2.ul.h)
}

#' Standard error of the mean
#'
#' @param x list of values
#' @param n number of cycles
#'
#' @return
#'
sem <- function(x, n) {
    sem <- sd(x)/sqrt(n)
    return(sem)
}


#' Lower confidence interval
#'
#' @param x list of values
#' @param n number of cycles
#'
#' @return
#'
lower.ci <- function(x, n) {
    lower.ci <- mean(x) - qt(1 - (0.05/2), n - 1) * sem(x, n)
    return(lower.ci)
}


#' Upper confidence interval
#'
#' @param x list of values
#' @param n number of cycles
#'
#' @return
#'
upper.ci <- function(x, n) {
    upper.ci <- mean(x) + qt(1 - (0.05/2), n - 1) * sem(x, n)
    return(upper.ci)
}
