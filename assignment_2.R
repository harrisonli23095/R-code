PERMID <- 9664343

library(tidyverse)
library(janitor)
library(magrittr)
assign_2_dat <- read_csv("assign_2.csv")

#1
View(assign_2_dat)

a2_unique <- assign_2_dat %>%
  distinct()

total_response <- 283

#2
typeof(a2_unique$visits)
typeof(a2_unique$days_attend)

a2_unique$visits <- gsub("\\-.*","",a2_unique$visits) %>%
  type.convert(a2_unique$visits)

a2_unique$days_attend <- gsub("\\-.*","",a2_unique$days_attend) %>%
  type.convert(a2_unique$days_attend, na.string = "NA")

#3
a2_unique <- a2_unique %>%
  mutate(local = ifelse(zip %in% c(80305,80309,80302,80304,80303,80306),
                        1, 0))

a2_unique$local

#4
local_perc <- mean(a2_unique$local)

#5
gender <- a2_unique$gender
female_perc <- mean(gender=="Female", na.rm = TRUE)

#6
gend_age_loc <- a2_unique %>%
  group_by(gender, age, local)%>%
  select(local, age, gender) %>%
  summarize(num_obs = n(), .groups = "keep") %>%
  arrange(desc(local), age, gender)

#7
big_spender_zip <- a2_unique %>% 
  filter(spend_food_drink_total == "$1,000-$1,999") %>%
  group_by(zip) %>%
  summarize(num_obs = n(), .groups = "keep")
