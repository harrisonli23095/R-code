PERMID <- 9664343
library(tidyverse)
#1
airbnb<-read_csv("assign_3.csv")
#2
##a
airbnb_properties <- airbnb %>% select(id, 
                                       property_type, 
                                       price, 
                                       host_is_superhost, 
                                       room_type)
##b
AptHs <- airbnb_properties %>% filter(property_type == "Apartment" |
                                        property_type == "House")
AptHs %>% count(price, sort = T) %>% head()
most_common_price4 <- 75
##c
airbnb_properties <- airbnb_properties %>% mutate(is_superhost = 
                              ifelse(host_is_superhost == "TRUE", 1, 0))
##d
airbnb_properties %>% count(is_superhost)
superhosts <- 204
nonsuperhosts <- 795
other <- 1

##e
airbnb_properties$price <- as.numeric(airbnb_properties$price)
mean(airbnb_properties$price)
average_price <- 123.969

##f
averages_by_host <- airbnb_properties %>% group_by(is_superhost) %>%
  summarise(average_prices = mean(price))

#3
##a
top20_neighbourhoods <- airbnb %>% 
  count(neighbourhood) %>%
  filter(!is.na(neighbourhood)) %>% 
  arrange(desc(n)) %>% 
  top_n(20,n)

##b
top5 <- airbnb %>% 
  filter(neighbourhood == "Capitol Hill"| 
           neighbourhood == "Ballard"|
           neighbourhood == "Queen Anne"|
           neighbourhood == "Belltown"|
           neighbourhood == "Minor") %>%
  group_by(neighbourhood) %>%
  summarise(average_price = mean(price))

##c 
airbnb <- airbnb %>% mutate(adjusted_price = price + cleaning_fee)

##d
top5_adjusted <- airbnb %>% 
  filter(neighbourhood == "Capitol Hill"| 
           neighbourhood == "Ballard"|
           neighbourhood == "Queen Anne"|
           neighbourhood == "Belltown"|
           neighbourhood == "Minor") %>%
  group_by(neighbourhood) %>%
  summarise(average_price = mean(price, na.rm = T),
            average_adjusted_price = mean(adjusted_price, na.rm = T))

##e
top5_adjusted <- airbnb %>% 
  filter(neighbourhood == "Capitol Hill"| 
           neighbourhood == "Ballard"|
           neighbourhood == "Queen Anne"|
           neighbourhood == "Belltown"|
           neighbourhood == "Minor") %>%
  group_by(neighbourhood) %>%
  summarise(average_price = mean(price, na.rm = T), 
            average_adjusted_price = mean(adjusted_price, na.rm = T),
            average_difference = abs(average_price - average_adjusted_price))

##Write up
OneBed <- airbnb %>% filter(neighbourhood == "Capitol Hill" & bedrooms == 1 & property_type == "Apartment") %>%
  group_by(property_type) %>%
  summarise(average_price = mean(price, na.rm = T),
            average_adjusted_price = mean(adjusted_price, na.rm = T),
            average_difference = abs(average_price - average_adjusted_price))

TwoBed <- airbnb %>% filter(neighbourhood == "Capitol Hill" & bedrooms == 2 & property_type == "Apartment") %>%
  group_by(property_type) %>%
  summarise(average_price = mean(price, na.rm = T),
            average_adjusted_price = mean(adjusted_price, na.rm = T),
            average_difference = abs(average_price - average_adjusted_price))

airbnb %>% filter(neighbourhood == "Capitol Hill") %>%
  count(property_type) %>%
  arrange(desc(n))

airbnb$host_response_rate <- as.numeric(airbnb$host_response_rate)

airbnb %>% mean(host_response_rate, na.rm = T)

NumApt <- airbnb %>% filter(neighbourhood == "Capitol Hill" & 
                    property_type == "Apartment") %>%
  group_by(bedrooms) %>%
  count()

OneBed
TwoBed
NumApt
