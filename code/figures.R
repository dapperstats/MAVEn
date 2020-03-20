plot_maven_overview <- function(maven_raw){
  experiment <- maven_raw %>% 
    select(Seconds, TC1, FRC_mlmin, CO2ppm, Chamber) %>% 
    pivot_longer(names_to = "Measurement", values_to = "value", -Seconds) %>%
    mutate(minutes = Seconds/60)
  
  p <- ggplot(data = experiment, aes(x = minutes, y = value)) +
    geom_line() +
    facet_wrap(~ Measurement, scales = "free_y", ncol = 1) +
    labs(title = "MAVEn run summary", 
         x = "Time (min)",
         y = "Result value") +
    theme_classic()
  
  return(p)
}


metabolism_diag <- function(maven_raw, metabolism_summary_cycle){
  
  df <- maven_raw %>%
    select(Seconds:BP_kPa, c_FRC_mlmin:CO2_mlmin, CO2_mlminFly1:CO2_mlminFly16) %>%
    pivot_longer(cols = CO2_mlminFly1:CO2_mlminFly16, names_to = "parameter", values_to = "result") %>%
    mutate(parameter = as.numeric(gsub(x = parameter, "CO2_mlminFly","")),
           result = co2_convertion(result))

  
  p <- ggplot(data = df, aes(x = Seconds, y = result)) +
    facet_grid(parameter ~ .) + 
    geom_line() + 
    geom_point(data = metabolism_summary_cycle %>% mutate(parameter = Chamber), 
               aes(x = median_time, y = median_co2_ul.h, col = parameter),
               size = 4) +
    labs(title = "Metabolism diagnostic",
         x = "Seconds", y = expression(CO[2]~(mu*L~h^-1)))
  
  return(p)
    
}

metablism_trend <- function(fly_metabolism){
  p <- ggplot(data = fly_metabolism %>%
                mutate(result = co2_convertion(result)), 
              aes(x = measurement_number, y = result, col = cycle)) +
    geom_point() +
    facet_wrap(~ Chamber, scales = "free_y") +
    labs(title = "Fly Metabolism Trend",
         x = "Measurement Time", 
         y = expression(CO[2]~(mu*L~h^-1))) +
    scale_color_viridis_d(option = "D", 
                          begin = 0.2, end = 0.8)
  return(p)
}



# activity_diag <- function(maven_raw, metabolism_summary_cycle, activity_summary_cycle){
#   
#   df <- maven_raw %>%
#     select(Seconds:BP_kPa, c_FRC_mlmin:CO2_mlmin, Act_1:Act_16) %>%
#     pivot_longer(cols = Act_1:Act_16, names_to = "parameter", values_to = "result") %>%
#     mutate(parameter = as.numeric(gsub(x = parameter, "Act_","")))
#   
#   
#   p <- ggplot(data = df, aes(x = Seconds, y = result)) +
#     facet_grid(parameter ~ .) + 
#     geom_line() + 
#     geom_point(data = metabolism_summary_cycle %>% mutate(parameter = Chamber), 
#                aes(x = median_time, y = median_co2_ul.h, col = parameter),
#                size = 4) +
#     labs(title = "Metabolism diagnostic",
#          x = "Seconds", y = expression(CO[2]~(mu*L~h^-1)))
#   
#   return(p)
#   
# }