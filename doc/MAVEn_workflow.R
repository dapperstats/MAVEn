## ---- include = FALSE---------------------------------------------------------

knitr::opts_chunk$set(collapse = TRUE, comment = "#>")


## ---- include = FALSE---------------------------------------------------------

options(repos = c(CRAN = "http://cran.rstudio.com"))

depend <- c("ggplot2", "pander", "tidyr")
ndepend <- length(depend)
present <- installed.packages()[, "Package"]
needed <- depend[!(depend %in% present)]
n_needed <- length(needed)

if (n_needed > 0) {
    install.packages(needed)
}

library(ggplot2); library(pander); library(tidyr)


## -----------------------------------------------------------------------------

library(MAVEn)


## ----analysis_defaults--------------------------------------------------------

experiment_name <- "maven_analysis_output"
activity_interval_value <- 60
activity_baseline_value <- 0.01
activity_threshold_value <- 1

## ----loadRawData--------------------------------------------------------------

maven_raw <- read_maven(datadir = "../inst/extdata", 
                        maven_datafile = "./maven_output.csv", baseline = T)
tibble(maven_raw)


## ----plotOverview, fig.height = 6, fig.width = 6, out.width="70%"-------------

plot_maven_overview(maven_raw, maven_experiment = experiment_name)


## ----loadData-----------------------------------------------------------------

maven <- read_maven(datadir = "../inst/extdata", 
                    maven_datafile = "./maven_output.csv", baseline = F)
tibble(maven)

## ----assign-cyclenumber-------------------------------------------------------

maven_cycle <- assign_cyclenumber(maven)

tibble(maven_cycle)

## ----exract-metabolism--------------------------------------------------------

animal_metabolism <- extract_metabolism(maven_cycle)

tibble(animal_metabolism)

## ----plot-MetabolismTrend, fig.height = 6, fig.width = 7, out.width="70%"-----

metabolism_trend(animal_metabolism, maven_experiment = experiment_name)


## ----plot-MetabolismTrend-options, fig.height = 6, fig.width = 7, out.width = "70%"----

p <- metabolism_trend(animal_metabolism, maven_experiment = experiment_name)
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
                maven_experiment = experiment_name)


## -----------------------------------------------------------------------------

animal_activity <- extract_activity(maven_cycle, metabolism_summary_cycle,
                                interval = activity_interval_value, 
                                activity_baseline = activity_baseline_value)
tibble(animal_activity) 

## ----fig.height = 6, fig.width = 7, out.width="70%"---------------------------

activity_trend(animal_activity, maven_experiment = experiment_name, 
               activity_baseline = activity_baseline_value)


## ----summarize-activitycycle--------------------------------------------------

activity_summary_cycle <- summarize_activity(animal_activity, 
                                             type = "by_cycle",
                                             activity_threshold = 
                                               activity_threshold_value)

## ----table-summarize-activitycycle, echo = FALSE------------------------------

pander(head(activity_summary_cycle), table.split.table = Inf)


## ----summarize-activitychamber------------------------------------------------

activity_summary_chamber <- summarize_activity(animal_activity, 
                                               type = "by_chamber")

## ----table-summarize-activitychamber, echo = FALSE----------------------------

pander(head(activity_summary_chamber), table.split.table = Inf)


## ----plot-ActivityDiag, fig.height = 11, fig.width = 7, out.width="70%"-------

activity_diag(maven_raw, metabolism_summary_cycle, activity_summary_cycle,
              maven_experiment = experiment_name, 
              interval = activity_interval_value)


## ----create-datafile----------------------------------------------------------
maven_datatable(metabolism_summary_cycle, activity_summary_cycle,
                            maven_experiment = experiment_name)

## ---- eval = T, fig.width = 5, fig.height=5, out.width="70%"------------------

test.out <- maven_datatable(metabolism_summary_cycle, activity_summary_cycle,
                            maven_experiment = experiment_name)


ggplot(test.out, aes(x = activity_state, y = median_co2_ul.h, col = cycle)) +
  geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 0.2, 
                                             dodge.width = 0.7)) +
  labs(title = "Activity State", x = "", 
       y = expression(Median~CO[2]~(mu*L~h^-1)))

## ---- eval = F----------------------------------------------------------------
#  evaluate_maven(datadir = "../data",
#                 maven_datafile = "maven_output.csv",
#                 maven_experiment = "test.evaluate",
#                 activity_baseline = 0.01, activity_threshold = 1)

