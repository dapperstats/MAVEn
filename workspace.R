code <- sapply(list.files(file.path(".", "R"), full.names = TRUE), source)

# remove baseline measurements
maven <- read_maven() %>% filter(Chamber > 0)

# assume that met measurements are > 0
#This is the animal metabolism data


cycle_starttime_init = maven$Seconds[maven$Chamber == 1][1]
chamber_measure_duration = 120
n_chambers = 16
#baseline_measurement_duration = 180
cycle_duration = n_chambers * chamber_measure_duration
n_cycles = nrow(maven)/(cycle_starttime_init + cycle_duration + 180)

test <- matrix(ncol = 3, nrow = 4)
colnames(test) <- c("cycle", "start", "end")






flychamber <- maven %>% 
  select(Seconds:BP_kPa, c_FRC_mlmin:CO2_mlmin, FlyChamber1:FlyChamber16) %>%
  pivot_longer(cols = FlyChamber1:FlyChamber16, names_to = "parameter", values_to = "result") %>%
  arrange(Seconds)



act <- maven %>% 
  select(Seconds:BP_kPa, c_FRC_mlmin:CO2_mlmin, cycle, Act_1:Act_16) %>%
  pivot_longer(cols = Act_1:Act_16, names_to = "parameter", values_to = "result") %>%
  arrange(Seconds)

met <- maven %>% 
  select(Seconds:BP_kPa, c_FRC_mlmin:CO2_mlmin, cycle, CO2_mlminFly1:CO2_mlminFly16) %>%
  pivot_longer(cols = CO2_mlminFly1:CO2_mlminFly16, names_to = "parameter", values_to = "result") %>%
  filter(result > 0) %>% ## can we make this assumption?
  arrange(parameter) %>% 
  group_by(Chamber, cycle) %>%
  mutate(measurement_number = Seconds - min(Seconds)+1)

met_summary <- met %>%
  group_by(Chamber, cycle) %>%
  summarize(median_co2_ul.h = co2_convertion(result)) %>%
  group_by(Chamber) %>%
  summarize(mean = mean(median_co2_ul.h), 
            sd = sd(median_co2_ul.h),
            n = round(n_cycles), 
            sem = sem(median_co2_ul.h, n),
            lower.ci = lower.ci(mean, n, sem),
            #lower.ci = lower.ci(median_co2_ul.h, n),
            upper.ci = upper.ci(median_co2_ul.h, n))


ggplot(data = met, aes(measurement_number, result, col = cycle)) +
  geom_point() +
  facet_wrap( ~ Chamber, scales = "free_y")


met$Seconds[met$Chamber == 1]

measurement_duration = 120
n_chambers = 16
cycle_length = measurement_duration*n_chambers # number of rows in cycle
n_cycles = nrow(met)/(n_chambers * measurement_duration)



met$cycle_number <- NA

cycle_start_time <- met$Seconds[1]
cycle_end_time <- cycle_start_time + cycle_length

for(i in 1:round(n_cycles)){
  
  met$cycle_number[met$Seconds >= cycle_start_time * i & met$Seconds < cycle_end_time * i] <- i
  
}

baseline_time = 180

for(i in 1:round(n_cycles)){
  
test <- met %>% 
  filter(Seconds >= cycle_start_time & Seconds < cycle_end_time * i) %>% 
  mutate(cycle_number = i) %>%
  select(Seconds, cycle_number)

cycle_start_time = cycle_start_time + cycle_end_time * i + baseline_time

}

test <- met %>%
  mutate(cycle_number = ifelse(Seconds <= cycle_length+start_time, 1, 2)) %>% ## this is a brut force method. it will not work with more than 2 cycles
  group_by(Chamber, cycle_number) %>%
  mutate(measurement_number = Seconds - min(Seconds)+1)

ggplot(data = test, aes(measurement_number, result, col = as.factor(cycle_number))) + 
  geom_point() + facet_grid(~Chamber)

test <- as.data.frame(test)

summarize_metabolism <- function(df=""){
  df %>%
    group_by(Chamber, cycle_number) %>%
    summarise(mean = mean(result), sd = sd(result))
}




maven %>% group_by(Chamber, cycle) %>% slice(c(1,n())) %>% select(Seconds, Chamber, cycle)


  met <- metabolism_summary_cycle %>%
    mutate(act_start = median_time - interval,
           act_end = median_time + interval)
  
  act <- maven %>% 
    select(Seconds:BP_kPa, cycle, c_FRC_mlmin:CO2_mlmin, Act_1:Act_16) %>%
    pivot_longer(cols = Act_1:Act_16, names_to = "parameter", values_to = "result") %>%
    left_join(met, by = c("Chamber", "cycle")) %>%
    group_by(Chamber, cycle) %>%
    filter(Seconds > median_time - interval & Seconds < median_time + interval) %>%
    group_by(Chamber, cycle) %>%
    mutate(measurement_number = Seconds - min(Seconds) + 1) %>%
    fulter(result > threshold)
  
  ggplot(fly_activity, aes(measurement_number, result, col = cycle)) +
    geom_point() + facet_wrap(~Chamber)
  

  
metabolism_summary_cycle %>% left_join(activity_summary_cycle, by = c("Chamber","cycle"))
test.out <- create_data_table(metabolism_summary_cycle, activity_summary_cycle) %>%
  mutate(mean_activity = replace_na(mean_activity,0),
         activity_state = ifelse(mean_activity >= activity_threshold, "Active", "Inactive"))
ggplot(test.out, aes(x = activity_state, y = median_co2_ul.h)) +
  geom_boxplot() + geom_point(position = position_jitter(width = .2))

as.Date.numeric(maven_raw$Date_Time)


###########
# test case with truc file

maven_raw <- read_maven(maven_datafile = "./truc_MAVEn 129 2019-11-22_8WT-8mettl4b-eclOct23_males-R.csv", baseline = T)
plot_maven_overview(maven_raw, maven_experiment = "test.case")

##  Data processing pipeline ----

# Step 1: load the MAVEn dataset without baseline ----
#  for workflow processing by toggling the baseline parameter.
maven <- read_maven(maven_datafile = "./truc2_MAVEn 129 2019-11-22_8WT-8mettl4b-eclOct23_males-R.csv", baseline = F)
maven <- read_maven(maven_datafile = "./truc_MAVEn 129 2019-11-22_8WT-8mettl4b-eclOct23_males-R.csv", baseline = F)
maven <- read_maven(maven_datafile = "./MAVEn 129 2019-11-22_8WT-8mettl4b-eclOct23_males-R.csv", baseline = F)


# Step 2: Assign a cycle number to the data ----
#
# This function is responsive to where the instrument begins its readings
#  and will assign the start of a cycle to Chamber 1

maven.cycle <- assign_cyclenumber(maven)


# Step 3a: Extract the metabolism data from the dataset ----

fly_metabolism <- extract_metabolism(maven.cycle)


# Step 3b: Visualize the trend data to check for issues in measurements ----
#
# The `extract_metabolism` function has standardized the time course to a 
#  measurement number (Second - min(Second)) to produce a consistent 
#  visualization of the data by cycle. 
#
# There are additional parameters added for saving the graphical output if 
#  needed. The default is to save a png named `MetabolismTrends.png`.
#
# Because these plots are generated with the ggplot2 package, you can save each
#  as an object and modify as you wish with themes, colors, etc. 

metablism_trend(fly_metabolism, maven_experiment = "test.case")

p <- metablism_trend(fly_metabolism, maven_experiment = "maven.example1")

p + theme_bw() + geom_line()


# Step 4: Produce a summary table for fly metabolism ----
#
# `summarize_metabolism` currently allows the user to create a table for all 
#  data ("by_cycle") or summarized by chamber ("by_chamber"). 
#
# We can add additional functionality that would automatically save the data 
#  output.

metabolism_summary_cycle <- summarize_metabolism(fly_metabolism, type = "by_cycle")

# generate summary table by chamber

metabolism_summary_chamber <- summarize_metabolism(fly_metabolism, type = "by_chamber")


# Step 5: Visual diagnostic of calculated data on raw data ----
#
# Using the MAVEn output with baseline (generated by baseline = T) as the base, 
#  `metabolism_diag` plots the median measurement time and median metabolism 
#  value onto the raw data for quick visual confirmation that the data 
#  calculated match the raw data.
#
# This figure also saves the graphic by default as `MetabolismDiagnostics.png`

metabolism_diag(maven_raw, metabolism_summary_cycle, maven_experiment = "maven.example1")


# Step 6a: Extract fly activity data based on metabolism calculations ----
#
# `extract_activity` requires the user to input an interval (measured in 
#   seconds) and a threshold activity level. Given the variability in  
#   where the instrument starts measurements, it is recommended to 
#   select a value no longer than 60 seconds (within the CO2 measurement 
#   interval)

fly_activity <- extract_activity(maven.cycle, metabolism_summary_cycle, 
                                 interval = 60, activity_baseline = 0.01)

# Step 6b: Plot fly activity ----
#
# These plots are again, standardized by the measurement number

activity_trend(fly_activity, maven_experiment = "maven.example1")


# Step 7: Generate fly activity summary tables ----
#
# There is currently no calculation for the abs difference sum, but that can be 
# added into the pipeline.
activity_summary_cycle <- summarize_activity(fly_activity, type = "by_cycle", 
                                             activity_threshold = 5)
activity_summary_chamber <- summarize_activity(animal_activity, type = "by_chamber")


# Step 8: Visual diagnostic of animal activity ----
activity_diag(maven_raw, metabolism_summary_cycle, activity_summary_cycle,
              maven_experiment = "maven_test", interval = 60)


# Step 9: Create the finalized data table ----
#
# create data table for analysis purposes

test.out <- maven_datatable(metabolism_summary_cycle, activity_summary_cycle) 


ggplot(test.out, aes(x = activity_state, y = median_co2_ul.h)) +
  geom_boxplot() + 
  geom_point(position = position_jitter(width = .2)) +
  labs(title = "Activity State", x = "", y = expression(Median~CO[2]~(mu*L~h^-1)))


animal_metabolism %>% select(Chamber, CO2ppm:BP_kPa) %>% group_by(Chamber) %>% summarize_if(is.numeric, funs(mean, median))
