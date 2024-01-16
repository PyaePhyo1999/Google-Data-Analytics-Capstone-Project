

#installing packages
install.packages("skimr")
install.packages("janitor")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lubridate")
install.packages("hms")
install.packages("tidyr")
install.packages("ggplot2")

#Usage library
library(dplyr)
library(skimr)
library(janitor)
library(tidyverse)
library(lubridate)
library(hms)
library(tidyr)
library(ggplot2)


#Read 10 csv files
ct01 <- read.csv("Cyclistic Trip data/202301-divvy-tripdata.csv")
ct02 <- read.csv("Cyclistic Trip data/202302-divvy-tripdata.csv")
ct03 <- read.csv("Cyclistic Trip data/202303-divvy-tripdata.csv")
ct04 <- read.csv("Cyclistic Trip data/202304-divvy-tripdata.csv")
ct05 <- read.csv("Cyclistic Trip data/202305-divvy-tripdata.csv")
ct06 <- read.csv("Cyclistic Trip data/202306-divvy-tripdata.csv")
ct07 <- read.csv("Cyclistic Trip data/202307-divvy-tripdata.csv")
ct08 <- read.csv("Cyclistic Trip data/202308-divvy-tripdata.csv")
ct09 <- read.csv("Cyclistic Trip data/202309-divvy-tripdata.csv")
ct10 <- read.csv("Cyclistic Trip data/202310-divvy-tripdata.csv")

#bind all csv files into one data frame
ctbind <- bind_rows(ct01,ct02,ct03,ct04,ct05,ct06,ct07,ct08,ct09,ct10)


#Cleaning Process
#delete missing rows in data frame
ctbind[ctbind==""]<-NA
ctbind_clean <-na.omit(ctbind)

#delete duplicates in data frame
ctfinal <- ctbind_clean[!duplicated(ctbind_clean$ride_id), ]


ctfinal <- ctfinal %>%
  filter(ended_at > started_at)

#change data format character into date_time
ctfinal$started_at <- as.POSIXct(ctfinal$started_at, format = "%Y-%m-%d %H:%M:%S")
ctfinal$ended_at <- as.POSIXct(ctfinal$ended_at, format = "%Y-%m-%d %H:%M:%S")

# Calculate the length of each ride by subtracting the column “started_at” from the column “ended_at” 
ride_length <- ctfinal$ended_at - ctfinal$started_at
#change second into time_format h:m:s
ctfinal$ride_length <- as_hms(ride_length)

# Calculate the day of the week as a number noting that 1 = Sunday and 7 = Saturday
ctfinal$day_of_week <- as.Date(ctfinal$started_at)
ctfinal$day_of_week <- paste(wday(ctfinal$day_of_week),"-",weekdays(as.Date(ctfinal$started_at)))

#Save as txt file
write.table(ctfinal,"Cyclistic Trip data/ctfinal.txt",fileEncoding = "UTF-8", quote = FALSE)

#Analyze
cyclistic <- ctfinal
#explore data in dataframe
head(cyclistic)

#Summary of data set
summary(cyclistic)


#Set up Mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#max of the ride length
max_ride_length <- max(ride_length)
max_ride_length <- seconds_to_period(max_ride_length)  
max_ride_length <- sprintf('%2d %02d:%02d:%02d', 
                           day(max_ride_length), max_ride_length@hour, 
                           minute(max_ride_length), second(max_ride_length))

mean_ride_length =as_hms(as.integer(mean(ride_length)))
#mean and max of the ride length, mode of the day of week
rl_df <- cyclistic %>%
  mutate(ride_length = as.numeric(ride_length)) %>%
  summarize(mean_ride_length, max_ride_length,
            mode_of_dayofweek= Mode(day_of_week))
View(rl_df)

#average_ride_length of each user type seperate with day of week
pv_avg <- cyclistic %>%
  group_by(day_of_week,member_casual) %>%
  summarise(average_ride_length = as.numeric(ceiling(mean(ride_length)))) %>%
  spread( key = day_of_week, value = average_ride_length)
#total of average_ride_length by each day
total_avg <- pv_avg %>%
     summarise(member_casual = "Total", across(where(is.numeric), ~sum(ceiling(.))))
#summerize into one data frame
pv_avg_total <- bind_rows(pv_avg,total_avg)
View(pv_avg_total)


#count_ride_id of each user type seperate with day of week
pv_count <- cyclistic %>%
  group_by(day_of_week,member_casual) %>%
  summarise(count_ride_id = length(ride_id)) %>%
  spread( key = day_of_week, value = count_ride_id)
#total of count_ride_id by each day
total_count <- pv_count %>%
  summarise(member_casual = "Total", across(where(is.numeric), sum))
#summerize into one data frame
pv_count_total <- bind_rows(pv_count,total_count)
View(pv_count_total)

#Join two table as a whole
join <- cbind(pv_avg_total,pv_count_total[,-1]) 
View(join)

#extract year and month from start_at
cyclistic <- cyclistic %>%
  mutate(year_month = paste(strftime(cyclistic$started_at, "%Y"),
                            "-",
                            strftime(cyclistic$started_at, "%m"),
                            paste("(",strftime(cyclistic$started_at, "%b"), ")", sep="")))
View(cyclistic)

cyclistic <- cyclistic %>%
  mutate(start_hour = paste(strftime(cyclistic$started_at, "%H")))
View(cyclistic)


#figure percentage of member-casual by day of week
cyclistic01 <- cyclistic %>%
  group_by(day_of_week) %>%
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(cyclistic)) * 100,
            member = (sum(member_casual == "member") / length(ride_id)) * 100,
            casual = (sum(member_casual == "casual") / length(ride_id)) * 100,
            '% diff' = member-casual)
View(cyclistic01)

#Chart-1 member and casual distribution
max_year_month <- max(cyclistic$year_month)
min_year_month <- min(cyclistic$year_month)

ggplot(cyclistic, aes(day_of_week, fill= member_casual )) +
  geom_bar() +
  facet_wrap(~member_casual)+
  labs(title="Chart 01 - Casuals x Members distribution by day of week",
       caption = paste0("Data from: ", min_year_month, "  to  ", max_year_month),
       x = "Day of Week",
       y = "Member-Casual")+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))

#figure percentage of member-casual by months
cyclistic02 <- cyclistic %>%
  group_by(year_month) %>%
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(cyclistic)) * 100,
            member = (sum(member_casual == "member") / length(ride_id)) * 100,
            casual = (sum(member_casual == "casual") / length(ride_id)) * 100,
            '% diff' = member-casual
  )
View(cyclistic02)

#Chart-2 member and casual distribution
max_year_month <- max(cyclistic$year_month)
min_year_month <- min(cyclistic$year_month)
ggplot(cyclistic, aes(year_month, fill= member_casual)) +
  geom_bar() +
  facet_wrap(~member_casual)+
  labs(title="Chart 02 - Casuals x Members distribution by months",
       caption = paste0("Data from: ", min_year_month, "  to  ", max_year_month),
       x = "Month",
       y = "Member-Casual")+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))

#figure percentage of member-casual by rideable type
cyclistic03 <- cyclistic %>%
  group_by(rideable_type) %>%
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(cyclistic)) * 100,
            member = (sum(member_casual == "member") / length(ride_id)) * 100,
            casual = (sum(member_casual == "casual") / length(ride_id)) * 100,
            '% diff' = member-casual
  )
View(cyclistic03)

#Chart-3 member and casual distribution by rideable type
max_year_month <- max(cyclistic$year_month)
min_year_month <- min(cyclistic$year_month)
ggplot(cyclistic, aes(rideable_type, fill= rideable_type)) +
  geom_bar() +
  facet_wrap(~member_casual)+
  labs(title="Chart 03 - Casuals x Members distribution by rideable types",
       caption = paste0("Data from: ", min_year_month, "  to  ", max_year_month),
       x = "Rideable Type",
       y = "Member-Casual")+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))

#figure percentage of member-casual by ride-id 
cyclistic04 <- cyclistic %>%
  group_by(member_casual) %>%
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(cyclistic)) * 100)
View(cyclistic04)


#Chart-4 member and casual distribution
max_year_month <- max(cyclistic$year_month)
min_year_month <- min(cyclistic$year_month)
ggplot(cyclistic, aes(member_casual, fill= member_casual)) +
  geom_bar() +
  labs(title="Chart 04 - Casuals x Members distribution",
       caption = paste0("Data from: ", min_year_month, "  to  ", max_year_month),
       x = "Member-Casual",
       y = "Count")+
  theme(
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))


#figure percentage of member-casual by hour of the day 
cyclistic05 <- cyclistic %>%
  group_by(start_hour) %>%
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(cyclistic)) * 100,
            member = (sum(member_casual == "member") / length(ride_id)) * 100,
            casual = (sum(member_casual == "casual") / length(ride_id)) * 100,
            '% diff' = member-casual
  )
View(cyclistic05)


#Chart-5 member and casual distribution by hour of the day
max_year_month <- max(cyclistic$year_month)
min_year_month <- min(cyclistic$year_month)
ggplot(cyclistic, aes(start_hour, fill= member_casual)) +
  geom_bar() +
  labs(title="Chart 05 - Casuals x Members distribution by hour of the day",
       caption = paste0("Data from: ", min_year_month, "  to  ", max_year_month),
       y = "Member-Casual",
       x = "Hour of the day")+
  theme(axis.text.x = element_text(angle = 90),
    axis.title.x = element_text(face="bold"),
    axis.title.y = element_text(face="bold"),
    plot.caption = element_text(face="bold"),
    plot.title =element_text(face="bold"))


#Chart-6 member and casual distribution by hour of the day divided by week-day
max_year_month <- max(cyclistic$year_month)
min_year_month <- min(cyclistic$year_month)
ggplot(cyclistic, aes(start_hour, fill= member_casual)) +
  geom_bar() +
  facet_wrap(~day_of_week)+
  labs(title="Chart 06 - Casuals x Members distribution by hour of the day divided by week-day",
       caption = paste0("Data from: ", min_year_month, "  to  ", max_year_month),
       y = "Member-Casual",
       x = "Hour of the day") +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))


#Distinguish weekdays and weekends
cyclistic <- cyclistic %>%
  mutate(type_of_weekday = ifelse(day_of_week == '7 - Saturday' | day_of_week == '1 - Sunday',
                                  'weekends',
                                  'weekdays'))
View(cyclistic)

cyclistic07 <- cyclistic %>%
  group_by(type_of_weekday,start_hour) %>%
  summarise(count = length(ride_id),
            '%' = (length(ride_id) / nrow(cyclistic)) * 100,
            member = (sum(member_casual == "member") / length(ride_id)) * 100,
            casual = (sum(member_casual == "casual") / length(ride_id)) * 100,
            '% diff' = member-casual
  )
View(cyclistic07)

#Chart-7 member and casual distribution by hour of the day between weekdays and weekends
max_year_month <- max(cyclistic$year_month)
min_year_month <- min(cyclistic$year_month)
ggplot(cyclistic, aes(start_hour, fill= member_casual)) +
  geom_bar() +
  facet_wrap(~type_of_weekday)+
  labs(title="Chart 07 - Casuals x Members distribution by hour of the day between weekdays and weekends",
       caption = paste0("Data from: ", min_year_month, "  to  ", max_year_month),
       y = "Member-Casual",
       x = "Hour of the day")+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))


#ride_length into minutes
cyclistic08 <- cyclistic
cyclistic08$ride_length_min = as.numeric(cyclistic08$ride_length)/60

#show in percentage of having ride length data
ventiles = quantile(cyclistic08$ride_length_min, seq(0, 1, by=0.05))
print(ventiles)


#filter out very low number of ride length in 0% and 100%
cyclistic08 <- cyclistic08 %>% 
  filter(ride_length_min > as.numeric(ventiles['5%'])) %>%
  filter(ride_length_min < as.numeric(ventiles['95%']))

#figure out quartites in boxplot
cyclistic08 %>%
  group_by(member_casual) %>%
  summarise(mean = mean(ride_length_min),
            first_quarter = quantile(ride_length_min,.25),
            'median' = median(ride_length_min),
            third_quarter = quantile(ride_length_min, .75),
            'IR' = third_quarter - first_quarter)


#Chart-8  distribution of riding time for member and casual by day of week
max_year_month <- max(cyclistic$year_month)
min_year_month <- min(cyclistic$year_month)
ggplot(cyclistic08, aes(x=day_of_week, y= ride_length_min, fill=member_casual)) +
  geom_boxplot()+
  facet_wrap(~member_casual)+
  labs(title="Chart 08 - distribution of riding time for member and casual by day of week",
       caption = paste0("Data from: ", min_year_month, "  to  ", max_year_month),
       y = "Riding Time",
       x = "Day of week")+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))


#Chart-9  distribution of riding time for member and casual by rideable type
max_year_month <- max(cyclistic$year_month)
min_year_month <- min(cyclistic$year_month)
ggplot(cyclistic08, aes(x=rideable_type, y= ride_length_min, fill=member_casual)) +
  geom_boxplot()+
  facet_wrap(~member_casual)+
  labs(title="Chart 09 - distribution of riding time for member and casual by rideable type",
       caption = paste0("Data from: ", min_year_month, "  to  ", max_year_month),
       y = "Riding Time",
       x = "Rideable Type")+
  theme(axis.text.x = element_text(angle = 90),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        plot.caption = element_text(face="bold"),
        plot.title =element_text(face="bold"))















































http://127.0.0.1:8561/graphics/deb20a98-893d-4d65-89f0-fa7e13044c88.png