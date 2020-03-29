
assign_cyclenumber <- function(maven,
    n_chambers = 16,
    chamber_measure_duration = 120) {
    
    df <-  maven %>% arrange(Seconds)
    diff.list <- c()
    
    # Detect the difference between a row and use it as an indicator for 
    #   the start of the next cycle
    
    for (i in 2:length(df$Chamber)) {
        diff <- df$Chamber[i - 1] - df$Chamber[i]
        diff.list <- append(diff.list, diff)
    }
    
    # In some cases, the first value may not be detected because of where
    #   the instrument starts its reading. 
    # This time_list will go through the end of the dataframe
    
    time_list <- unique(c(df$Seconds[df$Chamber == 1][1],
        df$Seconds[which(diff.list > 1) + 1],
        df$Seconds[nrow(df)]))
    
    cycle_duration <- n_chambers * chamber_measure_duration
    n_cycles <- (nrow(df) - df$Seconds[time_list[1]]) / (cycle_duration)
    n_cycles <- ceiling(n_cycles)
    df$cycle <- "NA"
    
    for (i in 1:n_cycles) {
        df[df$Seconds >= time_list[i] &
               df$Seconds <= time_list[i + 1], "cycle"] <- i
    }
    
    ## detect if there is an incomplete cycle (i.e. instrument was shut off early)
    start_lastcycle <- tail(time_list, n = 2)[1]
    end_lastcycle <-  start_lastcycle + cycle_duration
    
    if(df$Seconds[dim(df)[1]] <= end_lastcycle){
        df <- df %>% mutate(cycle = ifelse(Seconds >= start_lastcycle, NA, cycle))

    }

    #message(paste("There were", max(df$cycle, na.rm = T), "complete cycles detected in this dataset."))
    return(df)
}
