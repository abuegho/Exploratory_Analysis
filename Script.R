library(tidyverse)
library(chron)
library(Hmisc)
library(ggmap)
library(lubridate)

## Combine date and time into one column and arrange df by this new variable
W = Traffic_Violations %>% 
  unite(DateTime, Date, Time, sep = " ", remove = F)

# W$DateTime = strptime(W$DateTime, "%m/%d/%Y %H:%M:%S", tz = "EST")

W = W[order(W$DateTime), ]
W$Time = chron(times = W$Time)

W = W %>% 
  mutate(UI = 
           as.numeric(grepl("alcohol", W$Description, ignore.case = T)))

## Turn Yes/No values to binary form for easier summary
W$Belts =
  ifelse(grepl(pattern = "Yes", x = W$Belts),
         yes = print(1), no = print(0))

W$`Personal Injury` =
  ifelse(grepl(pattern = "Yes", x = W$`Personal Injury`),
         yes = print(1), no = print(0))

W$`Property Damage` =
  ifelse(grepl(pattern = "Yes", x = W$`Property Damage`),
         yes = print(1), no = print(0))

W$Fatal =
  ifelse(grepl(pattern = "Yes", x = W$Fatal),
         yes = print(1), no = print(0))

W$`Work Zone` =
  ifelse(grepl(pattern = "Yes", x = W$`Work Zone`),
         yes = print(1), no = print(0))

W$`Contributed To Accident` =
  ifelse(grepl(pattern = "Yes", x = W$`Contributed To Accident`),
         yes = print(1), no = print(0))

## Create a second data frame containing unique events (since original data set
## can have multiple observations referring to the same person)

Single = distinct(W, DateTime, Location, Color, Model, Race, Gender, `Driver City`, .keep_all = T)
Single = filter(Single, !is.na(Color), Color != "NA")
Single = Single %>% 
  select(-Alcohol) %>% 
  rename(Alcohol = "UI")

## Create another data frame that only accounts for Accidents

Single_Acc = Single %>% 
  filter(`Contributed To Accident` == 1) %>% 
  select(-`Contributed To Accident`)

## Create a data frame accounting for only alcohol-involved incidents

Alcohol = filter(Single, grepl(".*alcohol.*|influence", Single$Description, ignore.case = T))

## Standardise times & dates
W$DateTime = strptime(W$DateTime, "%m/%d/%Y %H:%M:%S", tz = "EST")
Alcohol$DateTime = strptime(Alcohol$DateTime, "%m/%d/%Y %H:%M:%S", tz = "EST")
Single$DateTime = strptime(Single$DateTime, "%m/%d/%Y %H:%M:%S", tz = "EST")
Single_Acc$DateTime = strptime(Single_Acc$DateTime, "%m/%d/%Y %H:%M:%S", tz = "EST")

## Make year columns
Single$year = year(Single$DateTime)
Single_Acc$year = year(Single_Acc$DateTime)

## Summary of different Races
Single_Acc %>% 
  select(-DateTime) %>% 
  group_by(Race) %>% 
  select(c(Belts, `Personal Injury`, `Property Damage`, Fatal, Alcohol, `Work Zone`)) %>% 
  summarise_all(funs(mean))

## Distribution of accidents over the span of the day
ggplot(Single_Acc, 
       aes(Color, Time, col = Color)) + 
       geom_jitter(alpha = .3, width = .1) + 
       scale_y_chron(format = "%H:%M") +
       theme(axis.text.x = element_text(angle = 75), 
             legend.position = "bottom")

## representation of different Races committing violations
quickplot(Race, data = Single, fill = Race)

## Time of the day against which different Races are in accidents
ggplot(Single_Acc,
       aes(Race, Time, col = Gender)) +
  geom_jitter(alpha = .7, width = .3) +
  scale_y_chron(format = "%H:%M") + 
  facet_grid(Gender~year) +
  theme(axis.text.x = element_text(angle = 75))

ggplot(Alcohol,
       aes(Race, Time, col = Gender)) +
  geom_jitter(alpha = .7, width = .3) +
  scale_y_chron(format = "%H:%M") + 
  facet_grid(Gender~Belts) +
  theme(axis.text.x = element_text(angle = 75))

## Intoxication vs Seatbelts
ggplot(Single, aes(factor(Alcohol), factor(Belts), col = Race)) +
  geom_jitter(alpha = .3, size = 4)

## Map representation
Montgomery = get_map(location = geocode("Montgomery County"), 
                source = "google", 
                maptype = "roadmap", 
                crop = F, zoom = 10.5)

ggmap(Montgomery) +
  geom_point(aes(x = Longitude, y = Latitude), 
             data = Single_Acc_T,
             alpha = .1,
             size = 2,
             color = "Red")

ggmap(Montgomery) +
  geom_point(aes(Longitude, Latitude),
             data = Alcohol,
             alpha = .3,
             size = 2)