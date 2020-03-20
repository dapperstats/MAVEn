
# load the MAVEn dataset
maven <- read_maven(maven_file = "./maven_output.csv")

# assign a cycle number to the dataset
maven <- cycle_number(maven)

fly_metabolism <- extract_metabolism(maven)

## generate summary table by cycle
metabolism_summary <- summarize_metabolism(fly_metabolism, type = "by_cycle")

## generate summary table by chamber
metabolism_summary <- summarize_metabolism(fly_metabolism, type = "by_chamber")
