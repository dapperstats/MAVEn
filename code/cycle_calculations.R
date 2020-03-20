
# assign cycle numbers to the dataframe
cycle_number <- function(maven, n_chambers = 16, chamber_measure_duration = 120){
  cycle_duration <- n_chambers * chamber_measure_duration
  
  maven$cycle <- "NA"
  index = 1
  
  for(i in 1:round(n_cycles)){
    #print(index)
    maven[index: (index + cycle_duration), "cycle"] <- i
    index <- index + cycle_duration
  }
  
  return(maven)
}
