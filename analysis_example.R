
# load the raw MAVEn dataset with baseline information
# This is useful for plotting the overall experimental summary
maven_raw <- read_maven(maven_datafile = "./maven_output.csv", baseline = T)
plot_maven_overview(maven_raw)

### The TC1 values from this file do not appear to be the same as those in the other. ###
maven_raw2 <- read_maven(maven_datafile = "./MAVEn 129 2019-11-22_8WT-8mettl4b-eclOct23_males-R.csv", baseline = T)
plot_maven_overview(maven_raw2)

# load the modified MAVEn dataset without baseline for workflow processing
### Do we need the baseline data for analysis? ###
maven <- read_maven(maven_datafile = "./maven_output.csv", baseline = F)
maven <- read_maven(datadir = "data", 
  maven_datafile = "./MAVEn 129 2019-11-22_8WT-8mettl4b-eclOct23_males-R.csv", baseline = F)

# assign a cycle number to the dataset
### update function name when other questions are answered ###
maven.cycle <- assign_cyclenumber_at1_variable(maven)

# Extract the metabolism data
fly_metabolism <- extract_metabolism(maven.cycle)

# visualize the trend
metablism_trend(fly_metabolism, outdir = "output", 
  out_filename = "maven_test", out_filetype = ".png")

# generate summary table by cycle
metabolism_summary_cycle <- summarize_metabolism(fly_metabolism, type = "by_cycle")

# Metabolism diagnostic
metabolism_diag(maven_raw, metabolism_summary_cycle)

# generate summary table by chamber
metabolism_summary_chamber <- summarize_metabolism(fly_metabolism, type = "by_chamber")

# extract activity data 
fly_activity<- extract_activity(maven, metabolism_summary_cycle, interval= 60, threshold = 0.01)
ggplot(fly_activity, aes(measurement_number, result, col = cycle)) +
  geom_point() + facet_wrap(~Chamber)

activity_summary_cycle <- summarize_activity(fly_activity, type = "by_cycle")
activity_summary_chamber <- summarize_activity(fly_activity, type = "by_chamber")

# create data table for analysis purposes
test.out <- maven_datatable(metabolism_summary_cycle, activity_summary_cycle) %>%
  mutate(mean_activity = replace_na(mean_activity,0),
         activity_state = ifelse(mean_activity >= activity_threshold, "Active", "Inactive"))
ggplot(test.out, aes(x = activity_state, y = median_co2_ul.h)) +
  geom_boxplot() + geom_point(position = position_jitter(width = .2))