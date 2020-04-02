## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")


options(repos = c(CRAN = "http://cran.rstudio.com"))

depend <- c("ggplot2", "pander")
ndepend <- length(depend)
present <- installed.packages()[, "Package"]
needed <- depend[!(depend %in% present)]
n_needed <- length(needed)

if (n_needed > 0) {
    install.packages(needed)
    library(needed)
}

for (i in 1:ndepend) {
    suppressMessages(eval(bquote(library(.(depend[i])))))
}



## -----------------------------------------------------------------------------
library(MAVEn)
library(pander)
library(ggplot2)

## ----loadRawData--------------------------------------------------------------

maven_raw <- read_maven(datadir = "../data", 
                        maven_datafile = "./maven_output.csv", baseline = T)
maven_raw


## ----plotOverview, fig.height = 6, fig.width = 6, out.width="70%"-------------

plot_maven_overview(maven_raw, maven_experiment = "maven.example1")


## ----loadData-----------------------------------------------------------------

maven <- read_maven(datadir = "../data", 
                    maven_datafile = "./maven_output.csv", baseline = F)
maven

## ----assign-cyclenumber-------------------------------------------------------

maven.cycle <- assign_cyclenumber(maven)

maven.cycle

## ----exract-metabolism--------------------------------------------------------

animal_metabolism <- extract_metabolism(maven.cycle)
animal_metabolism

## ----plot-MetabolismTrend, fig.height = 6, fig.width = 7, out.width="70%"-----

metabolism_trend(animal_metabolism, maven_experiment = "maven.example1")


## ----plot-MetabolismTrend-options, fig.height = 6, fig.width = 7, out.width = "70%"----

p <- metabolism_trend(animal_metabolism, maven_experiment = "maven.example1")
p + scale_color_brewer(palette = "Dark2")


## ----summarize-metabolismcycle, echo = 1--------------------------------------
metabolism_summary_cycle <- summarize_metabolism(animal_metabolism, 
                                                 type = "by_cycle")


## ----table-summarize-metabolismcycle, echo = FALSE----------------------------
pander(head(metabolism_summary_cycle), table.split.table = Inf)

## ----summarize-metabolismchamber----------------------------------------------

metabolism_summary_chamber <- summarize_metabolism(animal_metabolism, 
                                                   type = "by_chamber")

## ----table-summarize-metabolismchamber, echo = FALSE--------------------------
pander(head(metabolism_summary_chamber), table.split.table = Inf)

## ----plot-MetabolismDiag, fig.height = 10, fig.width = 7, out.width="70%"-----

metabolism_diag(maven_raw, metabolism_summary_cycle, 
                maven_experiment = "maven.example1")


## -----------------------------------------------------------------------------

animal_activity <- extract_activity(maven.cycle, metabolism_summary_cycle,
                                interval = 60, activity_baseline = 0.01)
animal_activity 

## ----fig.height = 6, fig.width = 7, out.width="70%"---------------------------

activity_trend(animal_activity, maven_experiment = "maven.example1")


## ----summarize-activitycycle--------------------------------------------------

activity_summary_cycle <- summarize_activity(animal_activity, 
                                             type = "by_cycle",
                                             activity_threshold = 1)

## ----table-summarize-activitycycle, echo = FALSE------------------------------

pander(head(activity_summary_cycle), table.split.table = Inf)


## ----summarize-activitychamber------------------------------------------------

activity_summary_chamber <- summarize_activity(animal_activity, 
                                               type = "by_chamber")

## ----table-summarize-activitychamber, echo = FALSE----------------------------

pander(head(activity_summary_chamber), table.split.table = Inf)


