co2_convertion <- function(x){
  co2.ul.h <- median(x) * 1000 * 60
}


sem <- function(x, n){
  sem <- sd(x)/sqrt(n)
}


lower.ci <- function(x, n){
  lower.ci <- mean(x) - qt(1 - (0.05 / 2), n - 1) * sem(x,n)
}


upper.ci <- function(x, n){
  upper.ci <- mean(x) + qt(1-(0.05/2), n - 1) * sem(x, n)
}
