## This script is to run the full suite of functions in a single step

evaluate_maven <- function(datadir = "data", outdir = "output",
                           maven_datafile = "./maven_output.csv", 
                           maven_experiment = "", 
                           interval = 60, 
                           activity_baseline = 0.01,
                           activity_threshold = 1, 
                           figures = c("trend","diag","overview")){
  # Load data
  maven_raw <- read_maven(maven_datafile = maven_datafile, baseline = T)
  maven <- read_maven(maven_datafile = maven_datafile, baseline = F)
  
  # assign a cycle
  maven.cycle <- assign_cyclenumber(maven)
  
  # animal metabolism
  animal_metabolism <- extract_metabolism(maven.cycle)
  metabolism_summary_cycle <- summarize_metabolism(animal_metabolism, type = "by_cycle")
  
  #animal activity
  animal_activity <- extract_activity(maven.cycle, metabolism_summary_cycle, 
                                   interval = interval, 
                                   activity_baseline = activity_baseline)
  activity_summary_cycle <- summarize_activity(animal_activity, type = "by_cycle", 
                                               activity_threshold = activity_threshold)
  
  # final data table
  out <- maven_datatable(metabolism_summary_cycle, activity_summary_cycle, 
                         maven_experiment = maven_experiment) 
  
  if ("overview" %in% figures) {
    maven_raw <- read_maven(maven_datafile = maven_datafile, baseline = T)
    plot_maven_overview(maven_raw, maven_experiment = maven_experiment)
  } 
  
  if ("trend" %in% figures){
    metablism_trend(animal_metabolism, maven_experiment = maven_experiment)
    activity_trend(animal_activity, maven_experiment = maven_experiment)
  } 
  
  if ("diag" %in% figures){
    metabolism_diag(maven_raw, metabolism_summary_cycle, 
                    maven_experiment = maven_experiment)
    activity_diag(maven_raw, metabolism_summary_cycle, 
                  activity_summary_cycle,
                  maven_experiment = maven_experiment,
                  interval = interval)
  }
  
  return(out)
}
