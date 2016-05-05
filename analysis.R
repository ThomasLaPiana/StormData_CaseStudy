## Set working directory and load required packages
setwd("~/JHU DataScience Courses/R Directory/StormData_CaseStudy")
require(data.table)
require(dplyr)
require(lubridate)

## Load in the dataset and take a quick look
raw_storm_data = data.table(read.csv('repdata-data-StormData.csv'))
head(raw_storm_data)

## Reform the data set to answer questions about population health as a result of storms
harmful_weather = raw_storm_data %>% 
    select(EVTYPE,FATALITIES,INJURIES) %>% 
    group_by(EVTYPE) %>% 
    summarise_each(funs(sum)) %>% 
    mutate(Human_Harm = FATALITIES + INJURIES) %>%
    arrange(desc(Human_Harm))

## Grab the ten most harmful to make a meaningful plot of the findings and scale it for readability
most_harmful = head(select(harmful_weather,EVTYPE,Human_Harm),10)
print(most_harmful)

## Plot the findings showing human harm by type of weather (log scaled and non-scaled)
par(mfrow=c(2,1))

with(most_harmful,
     barplot(Human_Harm, names = EVTYPE,log='y',
             main = 'Sum of humans injured or killed by Weather Type',
             xlab = 'Weather Type',
             ylab = 'People Injured of Killed (Log Scaled)'))

with(most_harmful,
     barplot(Human_Harm, names = EVTYPE,
             main = 'Sum of humans injured or killed by Weather Type',
             xlab = 'Weather Type',
             ylab = 'People Injured of Killed'))



## Reform the data set to answer questions about economic damage as a result of storms
damaging_weather = raw_storm_data %>% 
    select(EVTYPE,PROPDMG,CROPDMG) %>% 
    group_by(EVTYPE) %>% 
    summarise_each(funs(sum)) %>% 
    mutate(Total_Damage = PROPDMG + CROPDMG) %>%
    arrange(desc(Total_Damage))

## Grab the ten most harmful to make a meaningful plot of the findings and scale it for readability
most_damaging = head(select(damaging_weather,EVTYPE,Total_Damage),10)
print(most_damaging)

## Plot the findings showing human harm by type of weather (log scaled)
with(most_damaging,
     barplot(Total_Damage, names = EVTYPE,
             main = 'Sum of economic damage by Weather Type',
             xlab = 'Weather Type',
             ylab = 'Damage done in Dollars $'))