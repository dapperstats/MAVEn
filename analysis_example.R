
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

