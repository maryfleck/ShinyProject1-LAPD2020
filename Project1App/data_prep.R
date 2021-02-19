library(tidyverse)

LAPD <- read_csv('Arrest_Data_from_2020_to_Present.csv')
View(head(LAPD))

unique(LAPD$`Area Name`)


#things I want to do to pre-filter

#limit descent to BWHO
#limit dates?

test <- filter(LAPD, `Booking Date` < '02/01/2020')
range(test$`Booking Date`)

pie_data <- LAPD %>% group_by(`Charge Group Description`) %>% summarize(count=n())

ggplot(data=LAPD, aes(x="", fill=`Charge Group Description`)) +
  geom_bar(width=1) +
  coord_polar("y", start=0)

ggplot(data=LAPD, aes(x=`Booking Date`))

#create day, week and month columns
#user can choose which
#group by and summarize for that column
#plus date range

library(lubridate)

LAPD <- mutate(LAPD, week = isoweek(LAPD$`Arrest Date`))
LAPD$month <- month(as.POSIXlt(LAPD$`Arrest Date`))
LAPD$date <- substr(LAPD$`Arrest Date`,1,10)
LAPD$date <- as.Date(LAPD$date, format='%m/%d/%Y')
LAPD <- mutate(LAPD, week = isoweek(LAPD$date))
LAPD$month <- month(as.POSIXlt(LAPD$date))

time_data <- LAPD %>% group_by(`Arrest Date`) %>% summarise(count=n())


# the week after COVID was declared a state of emergency - March 3 or 4 - weeks 9/10/11 (mid 10)
# the week after George Floyd was murdered - 21/22/23 (early 22)
#vs the weeks before

View(head(LAPD))

new <- filter(LAPD, `Descent Code` %in% c('B', 'W', 'H'))

new <- filter(new, date < '2021-01-01')

new <- filter(new, week %in% c(9:11,21:23))

write.csv(new, 'LAPD_updated.csv')

test <- filter(new, week==1)
unique(test$date)
