summarize_metabolism <- function(extract_metabolism, type = ""){
  
  met_summary <- extract_metabolism %>%
    group_by(Chamber, cycle) %>%
    summarize(median_co2_ul.h = co2_convertion_median(result),
              median_time = median(Seconds),
              median_temp = median(TempC)) 
  
  if(type == "by_chamber"){
    met_summary <- met_summary %>%
      group_by(Chamber) %>%
      summarize(mean = mean(median_co2_ul.h), 
                sd = sd(median_co2_ul.h),
                n = round(n_cycles), 
                sem = sem(median_co2_ul.h, n),
                lower.ci = lower.ci(median_co2_ul.h, n),
                upper.ci = upper.ci(median_co2_ul.h, n))
  }
  
  return(met_summary)
  
}


summarize_activity <- function(extract_activity, type = ""){
  
  act_summary <- extract_axtivity %>%
    group_by(Chamber, cycle) %>%
    summarize(median_activity = median(result)) 
  
  if(type = "by_chamber"){ 
    act_summary <- act_summary %>%
      group_by(Chamber) %>%
      summarize(mean = mean(median_activity), 
                sd = sd(median_activity),
                n = round(n_cycles), 
                sem = sem(median_activity, n),
                lower.ci = lower.ci(median_activity, n),
                upper.ci = upper.ci(median_activity, n))
  }
  
  return(act_summary)
  
}
