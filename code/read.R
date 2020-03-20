read_maven <- function(datadir = "data", maven_file = "maven_output.csv") {
  
  fpath = file.path(datadir, maven_file)
  out <- read.csv(fpath, header = T) %>% filter(Chamber > 0)
  
  return(out)
}
