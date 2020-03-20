read_maven <- function(datadir = "data", 
                       maven_file = "maven_output.csv",
                       baseline = F){
  
  fpath = file.path(datadir, maven_file)
  
  if(baseline == F){
    out <- read.csv(fpath, header = T) %>% filter(Chamber > 0)
  } else {
    out <- read.csv(fpath, header = T)
  }
  
  return(out)
}
