# Function name will need to change; this function currently operates to detect
# the first time Chamber 1 is read in the dataseries and allows for a variable
# number of measurements per chamber. Waiting on James to confirm that this is
# how he wants to proceed.
# assign_cyclenumber
assign_cyclenumber_at1_variable <- function(maven,
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
    
    time.list <- unique(c(df[df$Chamber == 1, "Seconds"][1],
        df[which(diff.list > 1) + 1, "Seconds"],
        df[nrow(df), "Seconds"]))
    
    cycle_duration <- n_chambers * chamber_measure_duration
    n_cycles = (nrow(df) - df$Seconds[time.list[1]]) / (cycle_duration)
    message(paste("There were", n_cycles, "cycles detected in this dataset."))
    df$cycle <- "NA"
    
    if((ceiling(n_cycles) - 1) > 1) {
        for (i in 1:ceiling(n_cycles) - 1) {
            df[df$Seconds >= time.list[i] &
                    df$Seconds <= time.list[i + 1], "cycle"] <- i
        }
        
    } else {
        for (i in 1:ceiling(n_cycles)) {
            df[df$Seconds >= time.list[i] &
                    df$Seconds <= time.list[i + 1], "cycle"] <- i
        }
    }
    
    return(df)
}


# assign cycle numbers to the dataframe based on the first chamber measurement
# no flexibility in the number of measurements per chamber
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

# This function operates with indexing and uses a fixed number of measurements. 
# Unfortuntately, this assumption isn't always met

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

