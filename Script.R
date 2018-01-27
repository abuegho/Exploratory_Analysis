library(tidyverse)
library(chron)
library(Hmisc)
library(ggmap)

## Combine date and time into one column and arrange df by this new variable
W = Traffic_W %>% 
  unite(DateTime, Date, Time, sep = " ") %>% 
  arrange(as.POSIXct(DateTime, format = "%m/%d/%Y %H:%M:%S"))

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

W$Alcohol =
  ifelse(grepl(pattern = "Yes", x = W$Alcohol),
         yes = print(1), no = print(0))
W$`Work Zone` =
  ifelse(grepl(pattern = "Yes", x = W$`Work Zone`),
         yes = print(1), no = print(0))

W$`Contributed To Accident` =
  ifelse(grepl(pattern = "Yes", x = W$`Contributed To Accident`),
         yes = print(1), no = print(0))

## Create a second data frame containing unique events (since original data set
## can have multiple observations referring to the same person)

Single = distinct(W, DateTime, .keep_all = T)
Single = filter(Single, !is.na(Color), Color != "NA")

## Create another data frame that only accounts for Accidents

Single_Acc = Single %>% 
  filter(`Contributed To Accident` == 1) %>% 
  select(-`Contributed To Accident`)

Single_Acc_T = separate(Single_Acc, DateTime, c("Date", "Time"), sep = " ")

Single_Acc_T$Time = chron(times = Single_Acc_T$Time)

## Summary of different Races
Single_Acc_T %>% 
  group_by(Race) %>% 
  select(c(Belts, `Personal Injury`, 
           `Property Damage`, Fatal, Alcohol, `Work Zone`)) %>% 
  summarise_all(funs(mean))

## Distribution of accidents over the span of the day
ggplot(Single_Acc_T, 
       aes(Color, Time, col = Color)) + 
       geom_jitter(alpha = .3, width = .1) + 
       scale_y_chron(format = "%H:%M:%S") +
       theme(axis.text = element_text(angle = 45))

## representation of different Races committing violations
quickplot(Race, data = Single, fill = Race)

## Time of the day against which different Races are in accidents
ggplot(Single_Acc_T,
       aes(Race, Time, col = Color)) +
       geom_jitter(alpha = .4, width = .2) +
       scale_y_chron(format = "%H:%M:%S")

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
