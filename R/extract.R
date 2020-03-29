#' Extract fly metabolism data from MAVEn without baseline
#'
#' @param maven.cycle MAVEn dataset with cycles assigned. Must apply \code{assign_cyclenumbers} to MAVEn dataset without baseline data.
#'
#' @return Extracted fly metabolism data.
#'
#' @examples extract_metabolism(maven)
extract_metabolism <- function(maven.cycle) {
  
  met <- maven.cycle %>% 
    select(Seconds:BP_kPa, c_FRC_mlmin:CO2_mlmin, cycle, 
           CO2_mlminFly1:CO2_mlminFly16) %>%
    pivot_longer(cols = CO2_mlminFly1:CO2_mlminFly16, 
                 names_to = "parameter", values_to = "result") %>%
    filter(result > 0) %>% ## can we make this assumption?
    arrange(Seconds) %>% 
    group_by(Chamber, cycle) %>%
    mutate(measurement_number = Seconds - min(Seconds) + 1) %>%
    filter(cycle != "NA") # filter out data that do not have cycle assignment
    
  return(met)
}


#' Extract fly activity data from MAVEn without baseline
#'
#' @param maven.cycle MAVEn dataset with cycles assigned. Must apply \code{assign_cyclenumbers} to MAVEn dataset without baseline data.
#' @param metabolism_summary_cycle Summary dataset created by \code{summarize_metabolism}.
#' @param interval Measurement interval for activity evaluation. Must be in seconds. Recommend value less than 60 to stay within the instrument metabolism measurement. 
#' @param activity_baseline Baseline value for activity. 
#'
#' @return Extracted fly activity dataset.
#'
#' @examples extract_activity(maven.cycle, metabolism_summary_cycle, interval = 60, activity_baseline = 0.01)
extract_activity <- function(maven.cycle, 
                             metabolism_summary_cycle, 
                             interval = "", activity_baseline = "") {
  
  met <- metabolism_summary_cycle %>%
    mutate(act_start = median_time - interval,
           act_end = median_time + interval)
  
  act <- maven.cycle %>% 
    select(Seconds:BP_kPa, cycle, c_FRC_mlmin:CO2_mlmin, Act_1:Act_16) %>%
    pivot_longer(cols = Act_1:Act_16, 
                 names_to = "parameter", 
                 values_to = "result") %>%
    left_join(met, by = c("Chamber", "cycle")) %>%
    group_by(Chamber, cycle) %>%
    filter(Seconds > median_time - interval & 
             Seconds < median_time + interval) %>%
    group_by(Chamber, cycle) %>%
    mutate(measurement_number = Seconds - min(Seconds) + 1) %>%
    filter(result >= activity_baseline, # remove data that are not above threshold
           cycle != "NA") # filter out data that do not have cycle assignment
  
  return(act)
}

