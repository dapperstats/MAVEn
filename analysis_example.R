
# load the raw MAVEn dataset
maven_raw <- read_maven(maven_file = "./maven_output.csv", baseline = T)
plot_maven_overview(maven_raw)


# load the modified MAVEn dataset
maven <- read_maven(maven_file = "./maven_output.csv", baseline = F)

# assign a cycle number to the dataset
maven <- cycle_number(maven)


# Extract the metabolism data
fly_metabolism <- extract_metabolism(maven)

# visualize the trend
metablism_trend(fly_metabolism)

# generate summary table by cycle
metabolism_summary_cycle <- summarize_metabolism(fly_metabolism, type = "by_cycle")

# Metabolism diagnostic
metabolism_diag(maven_raw, metabolism_summary_cycle)

# generate summary table by chamber
metabolism_summary_chamber <- summarize_metabolism(fly_metabolism, type = "by_chamber")

# extract activity data 
fly_activity<- extract_activity(maven, metabolism_summary_cycle, interval= 60, threshold = 0.01)
activity_summary_cycle <- summarize_activity(fly_activity, type = "by_cycle")
activity_summary_chamber <- summarize_activity(fly_activity, type = "by_chamber")

test.out <- create_data_table(metabolism_summary_cycle, activity_summary_cycle) %>%
  mutate(mean_activity = replace_na(mean_activity,0),
         activity_state = ifelse(mean_activity >= activity_threshold, "Active", "Inactive"))
ggplot(test.out, aes(x = activity_state, y = median_co2_ul.h)) +
  geom_boxplot() + geom_point(position = position_jitter(width = .2))