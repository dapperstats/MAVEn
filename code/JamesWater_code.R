#	MAVEn analysis 
#	Larvae
#	Jan 16, 2020
#	James Waters

#	Prior to this script, run the first two Expedata macros
#	and import/export the output as a .csv file from Excel

#	libraries
library(tidyverse)

#	import data
setwd("~/Desktop/R/MAVEn/Lizzie/")
f1 <- read.csv("~/Desktop/R/MAVEn/Lizzie/Marla Maven 1-17-20 WT and mut larva.csv", header=T)


#	Determining the time of the start of the first cycle
chamber.1.t1 <- f1$Seconds[f1$FlyChamber1 == 1][1]
chamber.1.t1

#	Determining the time of the start of the second cycle
chamber.1.t2 <- f1$Seconds[f1$FlyChamber1 == 1 & f1$Seconds > chamber.1.t1+500][1]
chamber.1.t2

# How long is a cycle?
t.interval <- chamber.1.t2 - chamber.1.t1
t.interval

#	Validate first two times
plot(f1$Seconds, f1$FlyChamber1, type="l", col="grey")
abline(v=c(chamber.1.t1, chamber.1.t2), col="red", lty=2)

#	How many cycles were recorded in all?
max.t <- max(f1$Seconds)
cycles <- floor((max.t-chamber.1.t1)/t.interval)
cycles


#	When did each cycle start?
#	Useful because some runs have 3-6 cycles
cycle.times <- rep(NA, cycles)
cycle.times
cycle.times[1] <- chamber.1.t1
for(i in 2:cycles){
	cycle.times[i] <- f1$Seconds[f1$FlyChamber1 == 1 & f1$Seconds > cycle.times[i-1]+500][1]
	}
cycle.times

#	Cycle labels
cycle.ids <- 1:cycles

#	Adding a column to store the cycle id's
f1$cycle <- NA

#	How much time elapsed between cycles
sec.per.cycle <- diff(cycle.times)
sec.per.cycle <- c(sec.per.cycle, sec.per.cycle[1])
sec.per.cycle

#	Vector of cycle IDs
cycle.by.sec <- rep(1:cycles, sec.per.cycle)

#	Adding the cycle IDs to the data frame
end.time <- cycle.times[1] + sum(sec.per.cycle) - 1
f1$cycle[cycle.times[1]:end.time] <- cycle.by.sec

#	Validation that cycle IDs match start times
plot(f1$Seconds, f1$FlyChamber1*7, type="l", col="grey")
lines(f1$Seconds, f1$cycle)

#	Alt validation using pipes and ggplot2
# f1 %>%
#	select(Seconds, FlyChamber1, cycle) %>%
#		gather("key", "value", -Seconds) %>%
#			ggplot(aes(x=Seconds, y=value, color=key)) + geom_path()

#
##
###  ~
##
#

#	Filter and summarize data by chamber and cycle
fly.all <- f1 %>%
	filter(Chamber %in% 1:16) %>%
	filter(cycle %in% c(1:cycles)) %>%
	group_by(cycle, Chamber) %>%
	summarise(median.co2 = median(CO2_mlmin), time = median(Seconds), temp=median(TC1))
fly.all

head(fly.all)

# #	Plot median CO2 measurements for each fly x cycle
# ggplot(fly.all, aes(x=cycle, y=median.co2)) + geom_point() + geom_path() + facet_wrap(~Chamber)

#	Experimental conditions plot
f1 %>% select(Seconds, TC1, FRC_mlmin, CO2ppm, Chamber) %>% gather("Measurement", "value", -Seconds) %>%
	ggplot(aes(x=Seconds/60, y=value)) + facet_wrap(~Measurement, ncol=1, scales="free_y") + geom_path() + xlab("Time (min)") + ylab("")


#	VALIDATION (new method)
#	Compare CO2 median calculations with the raw CO2 traces

#	Select, rename, and gather the CO2 traces for each fly
f2 <- f1 %>%
	select(Seconds, CO2_mlminFly1, CO2_mlminFly2, CO2_mlminFly3, CO2_mlminFly4, CO2_mlminFly5, CO2_mlminFly6, CO2_mlminFly7, CO2_mlminFly8, CO2_mlminFly9, CO2_mlminFly10, CO2_mlminFly11, CO2_mlminFly12, CO2_mlminFly13, CO2_mlminFly14, CO2_mlminFly15, CO2_mlminFly16) %>%
	rename(CO2_mlminFly01 = CO2_mlminFly1, CO2_mlminFly02 = CO2_mlminFly2, CO2_mlminFly03 = CO2_mlminFly3, CO2_mlminFly04 = CO2_mlminFly4, CO2_mlminFly05 = CO2_mlminFly5, CO2_mlminFly06 = CO2_mlminFly6, CO2_mlminFly07 = CO2_mlminFly7, CO2_mlminFly08 = CO2_mlminFly8, CO2_mlminFly09 = CO2_mlminFly9) %>%
	gather("fly", "CO2", -Seconds)

#	Simplify the chamber/fly label
f2$fly <- as.factor(f2$fly)
levels(f2$fly) <- 1:16
names(f2)[2] <- "Chamber"

#	Make the chamber id's a factor (this helps the graph below)
fly.all$Chamber <- factor(fly.all$Chamber, levels=unique(fly.all$Chamber))

#	Plot all of the CO2 traces
ggplot(f2, aes(x=Seconds, y=CO2, color=Chamber)) + geom_path() + facet_grid(Chamber~.) + guides(color=F) + theme_void() 

#	Make the base for a plot of all of the CO2 traces with summary points annotation
q <- ggplot(f2, aes(x=Seconds, y=CO2, color=Chamber)) + geom_path(aes(group=Chamber)) + facet_grid(Chamber~.) + guides(color=F) + theme_void()

#	Plot traces with crosses
q + geom_point(data=fly.all, aes(time, median.co2, group=NULL), pch=3, size=1, color="black") 

#	Plot traces with circles
q + geom_point(data=fly.all, aes(time, median.co2, group=NULL), pch=1, size=2, color="black")


#
##
###  ~
##
#

#	Collapse to one data summary point per fly
fly.summary <- fly.all %>%
	group_by(Chamber)%>%
	  mutate(median.co2.uL.h = median.co2*1000*60) %>%
	  summarise(mean.co2 = mean(median.co2.uL.h, na.rm = TRUE),
            sd.co2 = sd(median.co2.uL.h, na.rm = TRUE),
            n.co2 = n()) %>%
	  mutate(se.co2 = sd.co2 / sqrt(n.co2),
		lower.ci.co2 = mean.co2 - qt(1 - (0.05 / 2), n.co2 - 1) * se.co2,
		upper.ci.co2 = mean.co2 + qt(1 - (0.05 / 2), n.co2 - 1) * se.co2)

#	Note: Is the confidence interval code above correct? It gives very wide intervals, but perhaps expected given small sample sizes (n=2 or n=3 per fly)...

#	Code by treatment/type of fly
#	*** EDITS REQUIRED ***
fly.summary$treatment <- NA
fly.summary$treatment[fly.summary$Chamber %in% 1:2] <- "N=5"
fly.summary$treatment[fly.summary$Chamber %in% 3:4] <- "N=4"
fly.summary$treatment[fly.summary$Chamber %in% 5:6] <- "N=3"

ggplot(fly.summary, aes(x=Chamber, y=mean.co2)) + geom_errorbar(aes(ymin=lower.ci.co2, ymax=upper.ci.co2), colour="black", width=.1) + geom_point(size=2, shape=21, fill="white") + xlab("") + ylab("CO2 (µL/hour)") 
# + facet_grid(~treatment, scales="free_x")
# + scale_x_continuous(breaks=1:16, labels=1:16) 

#	Add additional experimental details
date <- "17.Jan.2020"
fly.summary$date <- date
fly.summary$sex <- NA
# fly.summary$sex[fly.summary$Chamber %in% 1:16] <- "female"	
# fly.summary$sex[fly.summary$Chamber %in% 1:16] <- "male"	
fly.summary$TempC <- median(f1$TempC)
fly.summary$co2.units <- "microliters CO2 per hour"
fly.summary


# ACTIVITY

#	Pulling raw activity data from the main data file
a1 <- f1 %>% 
	select("Seconds", "Act_1", "Act_2", "Act_3", "Act_4", "Act_5", "Act_6", "Act_7", "Act_8", "Act_9", "Act_10", "Act_11", "Act_12", "Act_13", "Act_14", "Act_15", "Act_16") %>%
	rename(Act_01=Act_1, Act_02=Act_2, Act_03=Act_3, Act_04=Act_4, Act_05=Act_5, Act_06=Act_6, Act_07=Act_7, Act_08=Act_8, Act_09=Act_9) %>%
	gather("Chamber", "activity", -Seconds)

#	Plotting each chamber's activity trace
ggplot(a1, aes(x=Seconds, y=activity, color=Chamber)) + geom_path() + facet_grid(Chamber~.)
ggplot(a1[a1$Seconds <3000,], aes(x=Seconds, y=activity, color=Chamber)) + geom_path() + facet_grid(Chamber~.) 

#	Integrating activity by mean deviation
a.summary <- a1 %>%
	group_by(Chamber) %>%
	summarise(activity = mean(activity, na.rm=T))

#	Plot of activity sums
ggplot(a.summary, aes(x=Chamber, y=activity)) + geom_point()
ggplot(a.summary, aes(x=Chamber, y=activity)) + geom_bar(stat="identity")

#	Adding activity to summary data frame
fly.summary$activity.chamber <- a.summary$Chamber
fly.summary$activity <- a.summary$activity

str(fly.summary)

#	View final summary data frame
fly.summary


#	Saving data file
#	Make sure to give file new/appropriate name
setwd("~/Desktop/R/MAVEn/marla")
write.table(fly.summary, file="maven.17-jan-results.csv", sep=",", row.names=F)


