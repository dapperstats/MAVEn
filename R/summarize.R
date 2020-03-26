summarize_metabolism <- function(fly_metabolism, type = "") {
    
    n_cycles <- as.numeric(max(fly_metabolism$cycle))
    
    met_summary <- fly_metabolism %>% 
        group_by(Chamber, cycle) %>% 
        summarize(median_co2_ul.h = co2_convertion_median(result), 
        median_time = median(Seconds), median_temp = median(TempC))
    
    if (type == "by_chamber") {
        met_summary <- met_summary %>% 
            group_by(Chamber) %>% 
            summarize(mean = mean(median_co2_ul.h), 
                sd = sd(median_co2_ul.h), 
                n = n_cycles, 
                sem = sem(median_co2_ul.h, n), 
                lower.ci = lower.ci(median_co2_ul.h, n), 
                upper.ci = upper.ci(median_co2_ul.h, n))
    }
    
    return(met_summary)
    
}


summarize_activity <- function(fly_activity, type = "", 
    activity_threshold = "0.5") {
    
    n_cycles <- as.numeric(max(fly_activity$cycle))
    
    act_summary <- fly_activity %>% 
        group_by(Chamber, cycle) %>% 
        summarize(mean_activity = mean(result, na.rm = T), 
            median_activity = median(result, na.rm = T)) %>% 
        mutate(activity_state = ifelse(mean_activity >= 
            activity_threshold, "Active", "Inactive"))
    
    if (type == "by_chamber") {
        act_summary <- act_summary %>% 
            group_by(Chamber) %>% 
            summarize(mean = mean(mean_activity, na.rm = T), 
                sd = sd(mean_activity, na.rm = T), 
                n = n_cycles, 
                sem = sem(mean_activity, n), 
                lower.ci = lower.ci(mean_activity, n), 
                upper.ci = upper.ci(mean_activity, n))
    }
    
    return(act_summary)
    
}
