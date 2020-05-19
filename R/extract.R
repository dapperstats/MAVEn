#'Extract animal metabolism data from MAVEn without baseline
#'
#'\code{extract_metabolism} extracts animal metabolism data from the cycle
#'integrated MAVEn dataset. It applies a measurement adjustment to the time
#'series for visualization with \code{metabolism_trend}.
#'
#' @param maven_cycle MAVEn dataset with cycles assigned. Must apply
#'  \code{assign_cyclenumbers} to MAVEn dataset without baseline data.
#'
#' @return Extracted animal metabolism data.
#'
#' @importFrom dplyr select filter arrange group_by mutate
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#'
#' @export
extract_metabolism <- function(maven_cycle) {
  
  met <- maven_cycle %>% 
    select(Seconds:BP_kPa, c_FRC_mlmin:CO2_mlmin, cycle, 
           CO2_mlminFly1:CO2_mlminFly16) %>%
    pivot_longer(cols = CO2_mlminFly1:CO2_mlminFly16, 
                 names_to = "parameter", values_to = "result") %>%
    mutate(parameter = as.numeric(gsub("CO2_mlminFly","", parameter))) %>%
    filter(Chamber == parameter) %>%
    filter(result > 0) %>% ## can we make this assumption?
    arrange(Seconds) %>% 
    group_by(Chamber, cycle) %>%
    mutate(measurement_number = Seconds - min(Seconds) + 1) %>%
    filter(cycle != "NA") # filter out data that do not have cycle assignment
    
  return(met)
}


#'Extract animal activity data from MAVEn without baseline
#'
#'\code{extract_activity} extracts animal activity data from the cycle integrated
#'MAVEn dataset and calculated animal metabolism. It applies a measurement
#'adjustment to the time series for visualization with \code{activity_trend}.
#'
#' @param maven_cycle MAVEn dataset with cycles assigned. Must apply
#'   \code{assign_cyclenumbers} to MAVEn dataset without baseline data.
#' @param metabolism_summary_cycle Summary dataset created by
#'   \code{summarize_metabolism}.
#' @param interval Measurement interval for activity evaluation. Must be in
#'   seconds. Recommend value less than 60 to stay within the instrument
#'   metabolism measurement.
#' @param activity_baseline Baseline value for activity.
#'
#' @return Extracted animal activity dataset with median time and activity
#'   start/end times.
#'
#' @importFrom dplyr mutate select left_join group_by filter 
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' 
#' @export
extract_activity <- function(maven_cycle, 
                             metabolism_summary_cycle, 
                             interval = "", 
                             activity_baseline = "") {
  
  met <- metabolism_summary_cycle %>%
    mutate(act_start = median_time - interval,
           act_end = median_time + interval)
  
  act <- maven_cycle %>% 
    select(Seconds:BP_kPa, cycle, c_FRC_mlmin:CO2_mlmin, Act_1:Act_16) %>%
    pivot_longer(cols = Act_1:Act_16, 
                 names_to = "parameter", 
                 values_to = "result") %>%
    mutate(parameter = as.numeric(gsub("Act_","", parameter))) %>%
    filter(Chamber == parameter) %>% #select the activity data that matches the metabolism chamber reading
    left_join(met, by = c("Chamber", "cycle")) %>%
    group_by(Chamber, cycle) %>%
    filter(Seconds > median_time - interval & 
             Seconds < median_time + interval) %>%
    group_by(Chamber, cycle) %>%
    mutate(measurement_number = Seconds - min(Seconds) + 1,
           result_flag = ifelse(result < activity_baseline, "bth", "")) #%>%
    #lter(result >= activity_baseline, # remove data that are not above threshold
           #cycle != "NA") # filter out data that do not have cycle assignment
  
  return(act)
}

