calculate_cycles <- function(maven, n_chambers = 16, 
                        chamber_measure_duration = 120) {
    maven <- maven %>% arrange(Seconds)
    cycle_duration <- n_chambers * chamber_measure_duration
    n_cycles = nrow(maven)/(cycle_duration)
    
    return(n_cycles)
}

# assign cycle numbers to the dataframe based on the first chamber measurement
# this function might need to change depending on whether we need to start at 1
assign_cyclenumber <- function(maven, n_chambers = 16,
                        chamber_measure_duration = 120) {
    
    maven <- maven %>% arrange(Seconds)
    cycle_duration <- n_chambers * chamber_measure_duration
    n_cycles = nrow(maven)/(cycle_duration)
    message(paste("There were", n_cycles, "cycles detected in this dataset."))
    maven$cycle <- "NA"
    index <- 1
    
    for (i in 1:ceiling(n_cycles)) {
        maven[index:(index + cycle_duration), "cycle"] <- i
        index <- index + cycle_duration
    }
    
    return(maven)
}

assign_cyclenumber_at1 <- function(maven, n_chambers = 16,
    chamber_measure_duration = 120) {
    
    maven <-  arrange(maven, Seconds)
    
    diff.list <- c()
    
    #what rows, when difference between the two rows when its greater than 16-1
    for(i in 2:length(maven$Chamber)){
        diff <- maven$Chamber[i - 1] - maven$Chamber[i]
        diff.list <- append(diff.list, diff)
    }
       
    index <- which(diff.list > 10)[1] + 1
    
    cycle_duration <- n_chambers * chamber_measure_duration
    n_cycles = (nrow(maven) - maven$Seconds[index])/(cycle_duration)
    message(paste("There were", n_cycles, "cycles detected in this dataset."))
    maven$cycle <- "NA"
    
    for (i in 1:ceiling(n_cycles)) {
        maven[index:(index + cycle_duration), "cycle"] <- i
        index <- index + cycle_duration
    }
    
    return(maven)
}
