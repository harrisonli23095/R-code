PERMID <- 9664343
library(tidyverse)
library(janitor)

#1
calls <- read_csv("fcc.csv")

#2
calls <- clean_names(calls)

#3
##a
calls <- calls %>% 
  extract(col = location_center_point_of_the_zip_code, 
          into = "state", 
          regex = "([A-Z]{1,2})", 
          remove = F)

##b
calls_zip <- calls %>% 
  extract(col = caller_id_number, 
          into = "area_code", 
          regex = "(\\d\\d\\d)", 
          remove = F)

##c
calls_zip_sep <- calls_zip %>%
  separate(col = time_of_issue, into = c("time_of_issue","am_pm"), sep = " ") 

##d
calls_zip_sep <- calls_zip_sep %>% 
  mutate(am = ifelse(str_detect(am_pm, "A|a"),1,0))

#4
##a
delay <- calls %>% select(ticket_id,
                          ticket_created,
                          date_of_issue)

##b
delay_sep <- delay %>% separate(col = ticket_created,
                                into = c("month_created", 
                                         "day_created", 
                                         "year_created"),
                                sep = "\\/") %>%
  extract(col = year_created,
          into = "year_created",
          regex = "(\\d{4,})")

##c
delay_sep2 <- delay_sep %>%
  separate(col = date_of_issue,
           into = c("month_occurred",
                    "day_occurred",
                    "year_occurred"),
           sep = "\\/")

##d
time_elapsed <- delay_sep2 %>%
  filter(as.numeric(year_created) == as.numeric(year_occurred) &
           as.numeric(month_created) == as.numeric(month_occurred)) %>% 
  mutate(day_delay = as.numeric(day_created) - as.numeric(day_occurred))

##e
monthly_delay <- time_elapsed %>%
  group_by(month_occurred) %>%
  summarise(monthly_delay = mean(as.numeric(day_delay), na.rm = T))

##f
july_delay <- 0.79

x <- monthly_delay %>% 
  filter(month_occurred == 7) %>%
  summarize(round(monthly_delay, digits = 2))

##Write up
time_elapsed %>% count(month_occurred) %>%
  arrange(desc(n))#most common month: May



AM <- calls_zip_sep %>% filter(am == 1)
PM <- calls_zip_sep %>% filter(am == 0)

#9AM - 11:30AM, 12PM - 1PM most common
AM %>% count(time_of_issue) %>%
  arrange(desc(n))
PM %>% count(time_of_issue) %>% 
  arrange(desc(n))

#75758 most common zip code
calls %>% count(zip) %>%
  arrange(desc(n))

calls %>% count(issue) %>%
  arrange(desc(n))

#Prerecorded Voice most common, live Voice second
calls %>% count(type_of_call_or_messge) %>%
  arrange(desc(n))

calls %>% filter(issue == "Telemarketing (including do not call and spoofing)") %>%
  count(type_of_call_or_messge) %>%
  arrange(desc(n))

calls %>% filter(issue == "Robocalls") %>%
  count(type_of_call_or_messge) %>%
  arrange(desc(n))

calls %>% filter(issue == "Telemarketing (including do not call and spoofing)" &
                   time_of_issue == "10:00 AM") %>%
  count(time_of_issue) %>%
  arrange(desc(n))

calls %>% filter(issue == "Robocalls" &
                   time_of_issue == "10:00 AM") %>%
  count(time_of_issue) %>%
  arrange(desc(n))
