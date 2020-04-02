#' Read MAVEn output into R.
#' 
#' \code{read_maven} imports a MAVEn data with or without the baseline data (i.e. Chamber = 0). 
#' 
#' @param datadir Directory where data is stored
#' @param maven_datafile MAVEn file in .csv format
#' @param baseline Toggle option to include baseline data. Default is `FALSE`
#'
#' @return MAVEn dataset with (baseline = T) or without (baseline = F) data.
#' 
#' @importFrom utils read.csv
#' @importFrom magrittr %>%
#' @importFrom dplyr filter rename
#' 
#' @examples 
#' # import with baseline data
#' maven_raw <- read_maven(datadir = "data", maven_datafile = "maven_output.csv", baseline = T)
#' 
#' # import without baseline data
#' maven <- read_maven(datadir = "data", maven_datafile = "maven_output.csv", baseline = F)
read_maven <- function(datadir = "data", 
                        maven_datafile = "maven_output.csv", 
                        baseline = F) {
    
    fpath <- file.path(datadir, maven_datafile)
    
    if (baseline == F) {
        out <- read.csv(fpath, header = T) %>% filter(Chamber > 0)
    } else {
        out <- read.csv(fpath, header = T)
    }
    
    if("Volts_4" %in% names(out)) {
        out <- out %>% rename("TC1" = "Volts_4")
    }
    
    return(out)
}
