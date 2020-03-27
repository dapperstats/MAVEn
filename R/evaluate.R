## This script is to run the full suite of functions in a single step

evaluate_maven <- function(datadir = "data", outdir = "output",
                           maven_datafile = "./maven_output.csv", 
                           maven_experiment = "", 
                           interval = 60, threshold = 0.01, 
                           figures = c("trend","diag","overview")){
  
  maven <- read_maven(maven_datafile = maven_datafile, baseline = F)
  maven.cycle <- assign_cyclenumber(maven)
  
  fly_metabolism <- extract_metabolism(maven.cycle)
  metabolism_summary_cycle <- summarize_metabolism(fly_metabolism, type = "by_cycle")
  
  fly_activity <- extract_activity(maven.cycle, metabolism_summary_cycle, 
                                   interval = interval, threshold = threshold)
  
  
  activity_summary_cycle <- summarize_activity(fly_activity, type = "by_cycle")
  out <- maven_datatable(metabolism_summary_cycle, activity_summary_cycle) 
  
  if ("overview" %in% figures) {
    maven_raw <- read_maven(maven_datafile = maven_datafile, baseline = T)
    plot_maven_overview(maven_raw, maven_experiment = maven_experiment)
  } 
  
  if ("trend" %in% figures){
    metablism_trend(fly_metabolism, maven_experiment = maven_experiment)
    activity_trend(fly_activity, maven_experiment = maven_experiment)
  } 
  
  if ("diag" %in% figures){
    metabolism_diag(maven_raw, metabolism_summary_cycle, 
                    maven_experiment = maven_experiment)
  }
  
  return(out)
}
