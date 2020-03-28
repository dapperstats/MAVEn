## Creates a dataframe for the fly metabolism
#' Title
#'
#' @param maven 
#'
#' @return
#' @export
#'
#' @examples
extract_metabolism <- function(maven) {
  
  met <- maven %>% 
    select(Seconds:BP_kPa, c_FRC_mlmin:CO2_mlmin, cycle, 
      CO2_mlminFly1:CO2_mlminFly16) %>%
    pivot_longer(cols = CO2_mlminFly1:CO2_mlminFly16, 
      names_to = "parameter", values_to = "result") %>%
    filter(result > 0) %>% ## can we make this assumption?
    arrange(Seconds) %>% 
    group_by(Chamber, cycle) %>%
    mutate(measurement_number = Seconds - min(Seconds) + 1) %>%
    filter(cycle != "NA") # remove data with an unassigned cycle
    
  return(met)
}

# Creates a dataframe for the fly activity
#' Title
#'
#' @param maven.cycle 
#' @param metabolism_summary_cycle 
#' @param interval 
#' @param activity_baseline 
#'
#' @return
#' @export
#'
#' @examples
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
    filter(result >= activity_baseline)
  
  return(act)
}

