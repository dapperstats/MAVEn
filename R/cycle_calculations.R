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
    
    df <- maven %>% arrange(Seconds)
    cycle_duration <- n_chambers * chamber_measure_duration
    n_cycles = nrow(df)/(cycle_duration)
    message(paste("There were", n_cycles, "cycles detected in this dataset."))
    df$cycle <- "NA"
    index <- 1
    
    for (i in 1:ceiling(n_cycles)) {
        df[index:(index + cycle_duration), "cycle"] <- i
        index <- index + cycle_duration
    }
    
    df <- filter(df, !is.na(Seconds))
    return(df)
}

assign_cyclenumber_at1 <- function(maven, n_chambers = 16,
    chamber_measure_duration = 120) {
    
    df <-  maven %>% arrange(Seconds)
    
    diff.list <- c()
    
    #what rows, when difference between the two rows when its greater than 16-1
    for(i in 2:length(df$Chamber)){
        diff <- df$Chamber[i - 1] - df$Chamber[i]
        diff.list <- append(diff.list, diff)
    }
    
    index <- which(diff.list > 10)[1] + 1
        
    cycle_duration <- n_chambers * chamber_measure_duration
    n_cycles = (nrow(df) - df$Seconds[index])/(cycle_duration)
    message(paste("There were", n_cycles, "cycles detected in this dataset."))
    df$cycle <- "NA"
    
    for (i in 1:ceiling(n_cycles)) {
        df[index:(index + cycle_duration), "cycle"] <- i
        index <- index + cycle_duration
    }
    
    df <- filter(df, !is.na(Seconds))
    return(df)
}

assign_cyclenumber_at1_variable <- function(maven,
    n_chambers = 16,
    chamber_measure_duration = 120) {
    
    df <-  maven %>% arrange(Seconds)
    diff.list <- c()
    
    #what rows, when difference between the two rows when its greater than 16-1
    
    for (i in 2:length(df$Chamber)) {
        diff <- df$Chamber[i - 1] - df$Chamber[i]
        diff.list <- append(diff.list, diff)
    }
    
    index.list <- c(which(diff.list > 1), nrow(df))
    
    ## In some cases, the first value may not be detected
    ##  this should capture all the necessary values
    time.list <- unique(c(df[df$Chamber == 1, "Seconds"][1],
        df[which(diff.list > 1) + 1, "Seconds"],
        df[nrow(df), "Seconds"]))
    
    cycle_duration <- n_chambers * chamber_measure_duration
    n_cycles = (nrow(df) - df$Seconds[index]) / (cycle_duration)
    message(paste("There were", n_cycles, "cycles detected in this dataset."))
    df$cycle <- "NA"
    
    if((ceiling(n_cycles) - 1) > 1) {
        for (i in 1:ceiling(n_cycles) - 1) {
            #df[index.list[i]+1:index.list[i+1], "cycle"] <- i
            df[df$Seconds >= time.list[i] &
                    df$Seconds <= time.list[i + 1], "cycle"] <- i
            }
        } else {
        
        for (i in 1:ceiling(n_cycles)) {
            #df[index.list[i]+1:index.list[i+1], "cycle"] <- i
            df[df$Seconds >= time.list[i] &
                    df$Seconds <= time.list[i + 1], "cycle"] <- i
            }
        }
    
    print(unique(df$cycle))
    
    return(df)
}
