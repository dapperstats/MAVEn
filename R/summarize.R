#' Create final MAVEn datatable for an experimental run
#'
#' @param metabolism_summary_cycle Dataframe calculated by
#'   \code{summarize_metabolism} with the "by_cycle" type indicated.
#' @param activity_summary_cycle Dataframe calculated by
#'   \code{summarize_activity} with the "by_cycle" type indicated.
#' @param outdir Output directory. Defaults to 'output'
#' @param out_filename File name. Defaults to "ExperimentSummaryTable"
#' @param maven_experiment MAVEn experiment name.
#'
#' @return Table with calculated median metabolism  and activity by cycle.
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr left_join
#' @importFrom utils write.csv
#'
#' @examples #maven_datatable(metabolism_summary_cycle, activity_summary_cycle, 
#' #outdir = "output", out_filename = "ExperimentSummaryTable", 
#' #maven_experiment = "experiment")
maven_datatable <- function(metabolism_summary_cycle, activity_summary_cycle,
                            outdir = NULL, 
                            out_filename = "ExperimentSummaryTable", 
                            maven_experiment = "") {
    
    table <- metabolism_summary_cycle %>% 
        left_join(activity_summary_cycle, by = c("Chamber", "cycle"))
    
    if(!is.null(outdir)){
    outpath <- file.path(outdir, 
                         out_filename = paste0(Sys.Date(),"_", 
                                               maven_experiment, "_",
                                               out_filename, ".csv"))
    write.csv(table, file = outpath, row.names = F)}
    
    return(table)
}

#' Summarize metabolism data.
#'
#' @param animal_metabolism Animal metabolism dataframe extracted from the MAVEn
#'  without baseline using \code{extract_metabolism}.
#' @param type Summarize data by Chamber (="by_chamber") or by Cycle (="by_cycle").
#'
#' @return Table of values with calculated median metabolism by cycle or chamber.
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarize
#' @importFrom stats median sd
#'
#' @examples 
#' #summarize_metabolism(animal_metabolism, type = "by_cycle")
#' #summarize_metabolism(animal_metabolism, type = "by_chamber")
summarize_metabolism <- function(animal_metabolism, type = "") {
    
    n_cycles <- as.numeric(max(animal_metabolism$cycle))
    
    met_summary <- animal_metabolism %>% 
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
#' @param animal_activity Animal activity dataframe extracted from the MAVEn without
#'  baseline using \code{extract_activity}.
#' @param type Summarize data by Chamber (="by_chamber") or by Cycle (="by_cycle").
#' @param activity_threshold Threshold value to establish activity state.
#'
#' @return Table of values with calculated activity by cycle or chamber.
#' @export
#' 
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarize mutate
#' @importFrom stats median sd
#'
#' @examples 
#' #summarize_activity(animal_activity, type = "by_cycle", activity_threshold = 0.5)
#' #summarize_activity(animal_activity, type = "by_chamber", activity_threshold = 0.5)
summarize_activity <- function(animal_activity, type = "", 
                               activity_threshold = "0.5") {
    
    n_cycles <- as.numeric(max(animal_activity$cycle))
    
    act_summary <- animal_activity %>% 
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
