PERMID <- 9664343
library(tidyverse)
library(janitor)
ibt <- read_csv("ibt_testdata.csv")

##PART 1
#1
ibtclean <- ibt[-c(1,2),]

#2
unique(ibtclean$nationality)
ibtclean <- ibtclean %>% 
  mutate(natgroups = ifelse(nationality == "American"|
                              nationality == "USA"|
                              nationality == "US"|
                              nationality == "African American"|
                              nationality == "United States of America"|
                              nationality == "usa"|
                              nationality == "american"|
                              nationality == "Asian American"|
                              nationality == "Taiwanese American"|
                              nationality == "US-American", 1, 0))
ibtclean <- ibtclean %>% 
  mutate(natgroups = ifelse(nationality == "China"|
                              nationality == "Mexican"|
                              nationality == "German"|
                              nationality == "Pacific Islander"|
                              nationality == "syrian"|
                              nationality == "Cambodian"|
                              nationality == "hong kong"|
                              nationality == "china"|
                              nationality == "Republic of Korea"|
                              nationality == "Chinese"|
                              nationality == "chinese"|
                              nationality == "Filipino"|
                              nationality == "Brazilian"|
                              nationality == "CHINA"|
                              nationality == "Hispanic"|
                              nationality == "Taiwanese"|
                              nationality == "Polish"|
                              nationality == "Taiwan"|
                              nationality == "Peruvian", 2, natgroups))
ibtclean <- ibtclean %>% 
  mutate(natgroups = ifelse(nationality == "White"|
                            nationality == "white"|
                              nationality == "chinese/american"|
                              nationality == "Middle-Eastern"|
                              nationality == "American? of European descent", 3, natgroups))
ibtclean$natgroups[is.na(ibtclean$natgroups)] <- 3
ibtclean$natgroups
unique(ibtclean$nationality)
unique(ibtclean$natgroups)
ibtclean %>% count(as.numeric(natgroups))
natgroups <- tibble(US = 36, NotUS = 44, Other = 20)

#3 Barplot
ibtclean$Q65_Page.Submit ##first part
ibtclean$Q66_Page.Submit ##second part

ibtclean$Q65_Page.Submit <- as.numeric(ibtclean$Q65_Page.Submit)
mean(ibtclean$Q65_Page.Submit, na.rm = T)

ibtclean$Q66_Page.Submit <- as.numeric(ibtclean$Q66_Page.Submit)
mean(ibtclean$Q66_Page.Submit, na.rm = T)

num3 <- c(Q65 = 102, Q66 = 64)

barplot(num3,
        main = "Average time to submit",
        xlab = "Questions",
        ylab = "Seconds",
        col = c("red","green")
        )

##PART 2
ibt <- read_csv("ibt_testdata.csv")
ibtclean <- ibt[-c(1,2),]

#1
unique(ibtclean$nationality)

ibtclean <- ibtclean %>% 
  mutate(natnew = ifelse(nationality == "China"|
                           nationality == "hong kong"|
                           nationality == "china"|
                           nationality == "Chinese"|
                           nationality == "chinese"|
                           nationality == "CHINA"|
                           nationality == "Taiwanese"|
                           nationality == "Taiwan", 1, 0))

OWO <- ibtclean %>% 
  filter(race_ethnicity == "Asian"|
           race_ethnicity == "asian"|
           race_ethnicity == "South Asian"|
           race_ethnicity == "Filipino"|
           race_ethnicity == "Chinese"|
           race_ethnicity == "asian/chinese/american born chinese"|
           race_ethnicity == "Cambodians are typically considered very traditional and tend to be more family oriented. They have deep roots in the religion of Buddhism."|
           race_ethnicity == "south asian"|
           race_ethnicity == "Pacific Islander"|
           race_ethnicity == "Indian"|
           race_ethnicity == "White and Japanese"|
           race_ethnicity == "Asia"|
           race_ethnicity == "Asian American"|
           race_ethnicity == "Asian/Caucasian"|
           race_ethnicity == "Asian / Korean"|
           race_ethnicity == "White & Asian") %>% 
  select(nationality) %>% 
  group_by(nationality) %>% 
  summarise(n())

ibtclean <- ibtclean %>% 
  mutate(natnew = ifelse(nationality == "White"|
                           nationality == "white"|
                           nationality == "Middle-Eastern"|
                           nationality == "American? of European descent"| 
                           nationality == "Mexican"|
                           nationality == "German" |
                           nationality == "Pacific Islander" |
                           nationality == "syrian" |
                           nationality == "Republic of Korea"|
                           nationality == "Filipino"|
                           nationality == "Brazilian"|
                           nationality == "Hispanic"|
                           nationality == "Polish"|
                           nationality == "Cambodian"|
                           nationality == "Peruvian", 4, natnew))
ibtclean$natnew[is.na(ibtclean$natnew)] <- 4

unique(ibtclean$race_ethnicity)
ibtclean %>% group_by(nationality, race_ethnicity) %>% 
  summarise()
ibtclean %>% count(as.numeric(natnew))


natnew <- tibble(grchina = 30, usnonasian = 23, usasian = 14, other = 33)

#2
##ggplot
ibt <- read_csv("ibt_testdata.csv")
ibtclean <- ibt[-c(1,2),]

ibtclean$Q65_Page.Submit <- as.numeric(ibtclean$Q65_Page.Submit)
ibtclean$Q66_Page.Submit <- as.numeric(ibtclean$Q66_Page.Submit)
ibtclean <- ibtclean %>%  mutate(USorNot = ifelse(natgroups == 1, "US", NA))
ibtclean <- ibtclean %>%  mutate(USorNot = ifelse(natgroups == 2, "US", USorNot))
Q65 <- ibtclean %>% select(USorNot, Q65_Page.Submit)
Q66 <- ibtclean %>% select(USorNot, Q66_Page.Submit)
plot <- NULL


ggplot(data = boxplot) +
  geom_boxplot(aes(x = group, y = Question, fill = group)) +
  ylab("# seconds to complete") +
  xlab("") +
  labs(title = "Difference in Times By Citizenship and Difficulty", fill="Nationality/Difficulty") +
  theme_light()

##box plot
USEasy <- ibtclean %>% 
  filter(natgroups == 1) %>% 
  select(Q66_Page.Submit) %>% 
  summarise(Question = Q66_Page.Submit)
USHard <- ibtclean %>% 
  filter(natgroups == 1) %>% 
  select(Q65_Page.Submit) %>% 
  summarise(Question = Q65_Page.Submit)
NotUSEasy <- ibtclean %>% 
  filter(natgroups == 2) %>% 
  select(Q66_Page.Submit) %>% 
  summarise(Question = Q66_Page.Submit)
NotUSHard <- ibtclean %>% 
  filter(natgroups == 2) %>% 
  select(Q65_Page.Submit) %>% 
  summarise(Question = Q65_Page.Submit)

a <- data.frame(group = "US Easy", value = USEasy)
b <- data.frame(group = "US Hard", value = USHard)
c <- data.frame(group = "Not US Easy", value = NotUSEasy)
d <- data.frame(group = "Not US Hard", value = NotUSHard)
boxplot <- rbind(a,b,c,d)










