install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)
library(dplyr)

activities_data = read_csv('~/hector/da-r/case-study/fitbit-data/dailyActivity_merged.csv')
calories_data = read_csv('~/hector/da-r/case-study/fitbit-data/hourlyCalories_merged.csv')
weight_data = read_csv('~/hector/da-r/case-study/fitbit-data/weightLogInfo_merged.csv')
intensities = read_csv('~/hector/da-r/case-study/fitbit-data/hourlyIntensities_merged.csv')
sleep = read_csv('~/hector/da-r/case-study/fitbit-data/sleepDay_merged.csv')



intensities$ActivityHour=as.POSIXct(intensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
intensities$time <- format(intensities$ActivityHour, format = "%H:%M:%S")
intensities$date <- format(intensities$ActivityHour, format = "%m/%d/%y")


calories_data$ActivityHour=as.POSIXct(calories_data$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
calories_data$time <- format(calories_data$ActivityHour, format = "%H:%M:%S")
calories_data$date <- format(calories_data$ActivityHour, format = "%m/%d/%y")activities_data


activities_data$ActivityDate=as.POSIXct(activities_data$ActivityDate, format="%m/%d/%Y", tz=Sys.timezone())
activities_data$date <- format(activities_data$ActivityDate, format = "%m/%d/%y")


sleep$SleepDay=as.POSIXct(sleep$SleepDay, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep$date <- format(sleep$SleepDay, format = "%m/%d/%y")

weight_data$Date=as.POSIXct(weight_data$Date, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight_data$Date=as.POSIXct(weight_data$Date,  format = "%m/%d/%y")

# activity
activities_data %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, Calories) %>%
  summary()

# explore num of active minutes per category
activities_data %>%
  select(VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes) %>%
  summary()

# sleep
sleep %>%
  select(TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>%
  summary()
# weight
weight_data %>%
  select(WeightKg, BMI) %>%
  summary()

merged_data <- merge(activities_data, sleep, by=c('Id', 'date')) %>%
  merge(calories_data, by=c('Id', 'date'))

head(merged_data)


ggplot(data=activities_data, aes(x=TotalSteps, y=TotalTimeInBed)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")

ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y= TotalSteps)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Sleep")



ggplot(data=sleep, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point()+ labs(title="Total Minutes Asleep vs. Total Time in Bed")


int_new <- intensities %>%
  group_by(time) %>%
  drop_na() %>%
  summarise(mean_total_int = mean(TotalIntensity))

ggplot(data=int_new, aes(x=time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")


ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point(color='darkblue') + geom_smooth() +
  labs(title="Minutes Asleep vs. Sedentary Minutes")
