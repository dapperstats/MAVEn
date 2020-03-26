maven_datatable <- function(metabolism_summary_cycle, activity_summary_cycle) {
    metabolism_summary_cycle %>% 
    left_join(activity_summary_cycle, by = c("Chamber", "cycle"))
}