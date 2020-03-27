maven_datatable <- function(metabolism_summary_cycle, activity_summary_cycle,
  outdir = "output", out_filename = NULL, maven_experiment = "") {
    
  table <- metabolism_summary_cycle %>% 
    left_join(activity_summary_cycle, by = c("Chamber", "cycle"))
  
  outpath <- file.path(outdir, 
    out_filename = paste0(Sys.Date(),"_",maven_experiment, "_",
      out_filename, ".csv"))
  if(!is.null(out_filename)){  
    write.csv(table, file = outpath, row.names = F)}

  return(table)
}
