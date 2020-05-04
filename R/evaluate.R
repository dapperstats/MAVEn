#' Evaluate entire MAVEn dataset
#'
#' @param datadir data directory
#' @param outdir name of output directory. Defaults to "output"
#' @param maven_datafile name of data file to be evaluated. Must be ".csv".
#' @param maven_experiment name of experiment
#' @param interval specified interval for activity calculations. Must be in secoonds and defaults to 60.
#' @param activity_baseline Activity measurement baseline. Used to exclude values from dataset.
#' @param activity_threshold Activity threshold used for calculating activity status (e.g. inactive vs. active)
#' @param figures figures to be produced. Select from "overview", "diag", or "trend". Supply all three in list if you want all to be created. 
#'  
#' @export
#'
evaluate_maven <- function(datadir = "", outdir = NULL,
                           maven_datafile = "./maven_output.csv", 
                           maven_experiment = "", 
                           interval = 60, 
                           activity_baseline = 0.01,
                           activity_threshold = 1, 
                           figures = c("trend","diag","overview")){
  
  fpath <- file.path(outdir)
  
  if(!dir.exists(fpath)){
    dir.create(fpath)
  }
  
  # Load data
  maven_raw <- read_maven(datadir = datadir,
                          maven_datafile = maven_datafile, baseline = T)
  maven <- read_maven(datadir = datadir,
                      maven_datafile = maven_datafile, baseline = F)
  
  # assign a cycle
  maven.cycle <- assign_cyclenumber(maven)
  
  # animal metabolism
  animal_metabolism <- extract_metabolism(maven.cycle)
  metabolism_summary_cycle <- summarize_metabolism(animal_metabolism, 
                                                   type = "by_cycle")
  
  #animal activity
  animal_activity <- extract_activity(maven.cycle, metabolism_summary_cycle, 
                                      interval = interval, 
                                      activity_baseline = activity_baseline)
  activity_summary_cycle <- summarize_activity(animal_activity, 
                                               type = "by_cycle", 
                                               activity_threshold = 
                                                 activity_threshold)
  

  if ("overview" %in% figures) {
    plot_maven_overview(maven_raw, maven_experiment = maven_experiment, 
                        outdir = outdir)
  } 
  
  if ("trend" %in% figures){
    metabolism_trend(animal_metabolism, maven_experiment = maven_experiment, 
                    outdir = outdir)
    activity_trend(animal_activity, maven_experiment = maven_experiment,
                   outdir = outdir)
  } 
  
  if ("diag" %in% figures){
    metabolism_diag(maven_raw, metabolism_summary_cycle, 
                    maven_experiment = maven_experiment,
                    outdir = outdir)
    activity_diag(maven_raw, metabolism_summary_cycle, 
                  activity_summary_cycle,
                  maven_experiment = maven_experiment,
                  interval = interval,
                  outdir = outdir)
  }
  
  # final data table
  out <- maven_datatable(outdir = outdir,
                         metabolism_summary_cycle, activity_summary_cycle, 
                         maven_experiment = maven_experiment)   
  return(out)
}
