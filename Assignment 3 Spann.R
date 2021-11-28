# I was born in 1997 so I downloaded the 1997 dataset
# The below code will double check the working directory,
# load the data.table library, and then read in the gz file
getwd()
library(data.table)
data <- fread("StormEvents_details-ftp_v1.0_d1997_c20210803.csv.gz", data.table = FALSE)


# Now I will limit the dataframe to only a subset of columns.
subset_data <- data[, c("BEGIN_YEARMONTH", "BEGIN_DAY", "BEGIN_TIME",
                             "END_YEARMONTH", "END_DAY", "END_TIME",
                             "BEGIN_DATE_TIME", "END_DATE_TIME", "EPISODE_ID",
                             "EVENT_ID", "STATE", "STATE_FIPS", "CZ_NAME",
                             "CZ_TYPE", "CZ_FIPS", "EVENT_TYPE", "SOURCE",
                             "BEGIN_LAT", "BEGIN_LON", "END_LAT", "END_LON")]

# This code is used to covert 2 fields to the date-time class
library(lubridate)
subset_data <- subset_data %>% mutate(BEGIN_DATE_TIME=dmy_hms(BEGIN_DATE_TIME))
subset_data <- subset_data %>% mutate(END_DATE_TIME=dmy_hms(END_DATE_TIME))
str(subset_data$BEGIN_DATE_TIME)
str(subset_data$END_DATE_TIME)

# This chunk is used to changed the state and county names to title
library(tidyverse)
subset_data$STATE = str_to_title(subset_data$STATE, locale = "en")
subset_data$CZ_NAME = str_to_title(subset_data$CZ_NAME, locale = "en")

#To limit to CZ_TYPE of C only and then remove the CZ_TYPE column,
#I will use the filter and select functions.
subset_data <- filter(subset_data, CZ_TYPE == "C")
subset_data <- select(subset_data, -CZ_TYPE)

#Pad the state and county FIPS with a "0" at the beginning and then unite
#the two columns to make one fips column with the 5 or 6-digit county FIPS code
subset_data$STATE_FIPS = str_pad(subset_data$STATE_FIPS, width=3, side="left", pad="0")
subset_data$CZ_FIPS = str_pad(subset_data$CZ_FIPS, width=3, side="left", pad="0")
library(dplyr)
subset_data <- unite(subset_data, "FIPS", c("STATE_FIPS","CZ_FIPS"))

#Change all the column names to lower case
subset_data <- rename_all(subset_data, tolower)

#Create a dataframe with these three columns: state name, area, and region
#from the state dataset in base R
state_info<-data.frame(state=state.name, area=state.area, region=state.region)

#Create a dataframe with the number of events per state in the year of your birth
#Merge in the state information dataframe you just created in step 8
#Remove any states that are not in the state information dataframe
events_by_state <- data.frame(table(subset_data$state))
events_by_state <- rename(events_by_state, c("state"="Var1"))
merger <- merge(x=events_by_state, y=state_info, by.x = "state", by.y = "state")

#Plotting storm info
library(ggplot2)
plot <- ggplot(merger, aes(x=area, y=Freq)) +
  geom_point(aes(color = region)) +
  labs(x = "Land area (square miles)", y = "# of storm events in 1997")
plot

