#' Create final MAVEn datatable for an experimental run
#'
#' @param metabolism_summary_cycle Dataframe calculated by
#'   \code{summarize_metabolism} with the "by_cycle" type indicated.
#'@param fly_activity Fly activity dataframe extracted from the MAVEn without
#'  baseline using \code{extract_activity}. 
#' @param outdir Output directory. Defaults to 'output'
#' @param out_filename File name. Defaults to "ExperimentSummaryTable"
#' @param maven_experiment MAVEn experiment name.
#'
#' @return
#' @export
#'
#' @examples maven_datatable(metabolism_summary_cycle, activity_summary_cycle, 
#' outdir = "output", out_filename = "ExperimentSummaryTable", maven_experiment = "")
maven_datatable <- function(metabolism_summary_cycle, activity_summary_cycle,
                            outdir = "output", 
                            out_filename = "ExperimentSummaryTable", maven_experiment = "") {
    
    table <- metabolism_summary_cycle %>% 
        left_join(activity_summary_cycle, by = c("Chamber", "cycle"))
    
    outpath <- file.path(outdir, 
                         out_filename = paste0(Sys.Date(),"_", 
                                               maven_experiment, "_",
                                               out_filename, ".csv"))
    write.csv(table, file = outpath, row.names = F)
    
    return(table)
}

#' Summarize metabolism data.
#'
#' @param fly_metabolism Fly metabolism dataframe extracted from the MAVEn
#'  without baseline using \code{extract_metabolism}.
#' @param type Summarize data by Chamber (="by_chamber") or by Cycle (="by_cycle").
#'
#' @return
#' @export
#'
#' @examples 
#' summarize_metabolism(fly_metabolism, type = "by_cycle")
#' summarize_metabolism(fly_metabolism, type = "by_chamber")
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


#' Summarize animal activity data.
#'
#' @param fly_activity Animal activity dataframe extracted from the MAVEn without
#'  baseline using \code{extract_activity}.
#' @param type Summarize data by Chamber (="by_chamber") or by Cycle (="by_cycle").
#' @param activity_threshold Threshold value to establish activity state.
#'
#' @return
#' @export
#'
#' @examples 
#' summarize_activity(fly_activity, type = "by_cycle", activity_threshold = 0.5)
#' summarize_activity(fly_activity, type = "by_chamber", activity_threshold = 0.5)
summarize_activity <- function(fly_activity, type = "", 
                               activity_threshold = "0.5") {
    
    n_cycles <- as.numeric(max(fly_activity$cycle))
    
    act_summary <- fly_activity %>% 
        group_by(Chamber, cycle) %>% 
        summarize(mean_activity = mean(result, na.rm = T), 
                  median_activity = median(result, na.rm = T)) %>% 
        mutate(activity_state = ifelse(mean_activity >= activity_threshold, 
                                       "Active", "Inactive"))
    
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
