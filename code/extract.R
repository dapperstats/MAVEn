## Creates a dataframe for the fly metabolism
extract_metabolism <- function(maven){
  
  met <- maven %>% 
    select(Seconds:BP_kPa, c_FRC_mlmin:CO2_mlmin, cycle, CO2_mlminFly1:CO2_mlminFly16) %>%
    pivot_longer(cols = CO2_mlminFly1:CO2_mlminFly16, names_to = "parameter", values_to = "result") %>%
    filter(result > 0) %>% ## can we make this assumption?
    arrange(parameter) %>% 
    group_by(Chamber, cycle) %>%
    mutate(measurement_number = Seconds - min(Seconds) + 1)
  
  return(met)
}

# Creates a dataframe for the fly activity
extract_activity <- function(maven){
  
  met <- maven %>% 
    select(Seconds:BP_kPa, c_FRC_mlmin:CO2_mlmin, cycle, Act_1:Act_16) %>%
    pivot_longer(cols = Act_1:Act_16, names_to = "parameter", values_to = "result") %>%
    filter(result > 0) %>% ## can we make this assumption?
    arrange(parameter) %>% 
    group_by(Chamber, cycle) %>%
    mutate(measurement_number = Seconds - min(Seconds) + 1)
  
  return(met)
}

