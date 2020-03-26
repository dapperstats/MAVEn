co2_convertion_median <- function(x) {
    co2.ul.h <- median(x) * 1000 * 60
    return(co2.ul.h)
}

co2_convertion <- function(x) {
    co2.ul.h <- x * 1000 * 60
    return(co2.ul.h)
}

sem <- function(x, n) {
    sem <- sd(x)/sqrt(n)
    return(sem)
}


lower.ci <- function(x, n) {
    lower.ci <- mean(x) - qt(1 - (0.05/2), n - 1) * sem(x, n)
    return(lower.ci)
}


upper.ci <- function(x, n) {
    upper.ci <- mean(x) + qt(1 - (0.05/2), n - 1) * sem(x, n)
    return(upper.ci)
}
