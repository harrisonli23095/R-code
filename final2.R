PERMID <- 9664343
library(janitor)
library(tidyverse)
complaints <- read_csv("complaints.csv")
hotness <- read_csv("hotness.csv")

#1
complaints <- clean_names(complaints)
hotness <- clean_names(hotness)

#2
complaints_cleaned <- complaints %>% 
  separate(col = date, into = c("month", "day", "year"), sep = "/")

#3
hotness_cleaned <- hotness %>% 
  separate(col = month_date_yyyymm, into = c("year", "month"), sep = "(?<=6)")

#4
clean_merge <- complaints_cleaned %>% 
  inner_join(hotness_cleaned, 
             by = c("year" = "year", "month" = "month", "zip" = "postal_code"))

#5
mean(clean_merge$hotness_rank)
median(clean_merge$hotness_rank)
avg_hot <- 5730.578
med_hot <- 4682

#6
comp_type <- clean_merge %>% group_by(issue) %>% 
  summarise(mean = round(mean(hotness_rank), 1), 
                        median = round(median(hotness_rank),1), 
                        min = round(min(hotness_rank),1), 
                        max = round(max(hotness_rank),1))
comp_type
#7
meth_type <- clean_merge %>% group_by(method) %>% 
  summarise(mean = round(mean(hotness_rank), 1), 
            median = median(hotness_rank),
            max = max(hotness_rank),
            min = min(hotness_rank))
meth_type
#8
last_tib <- clean_merge %>% 
  separate(col = zip_name, into = c("city", "state"), sep = ", ")

#9
last_tib %>% group_by(state) %>%
  summarise(n = n()) %>% 
  arrange(desc(n))
most_calls <- "ca"

#Write up
ggplot(last_tib) +
  geom_boxplot(aes(x = issue, y = hotness_rank)) +
  xlab("Issue") +
  ylab("Hotness Rank")

uwu <- last_tib %>% filter(issue == "Robocalls")
ggplot(uwu) +
  geom_density(aes(x = hotness_rank)) +
  xlab("Hotness Rank") +
  ylab("Frequency") +
  labs(title = "Hotness Rank of Robocall Occurances")

ggplot(last_tib) +
  geom_density(aes(x = hotness_rank, fill = issue), alpha = 0.4) +
  xlab("Hotness Rank") +
  ylab("Frequency") +
  labs(title = "Hotness Rank of Robocall & Telemarketing Occurances", fill = "Issue")






















