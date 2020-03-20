code <- sapply(list.files(file.path(".", "code"), full.names = TRUE), source)

# remove baseline measurements
maven <- read_maven() %>% filter(Chamber > 0)

# assume that met measurements are > 0
#This is the fly metabolism data


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
