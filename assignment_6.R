PERMID <- 9664343
library(tidyverse)
library(ggplot2)
airbnb <- read_csv("airbnb_small.csv")
#1
set.seed(9664343)
practice_vector <- runif(10, min = 0, max = 25)
one_hundred <- seq(100)

#2
##a
for (i in 1:5) {
  print(practice_vector[i])
}

##b
squared_numbers <- rep(NA, 10)

##c
for (i in 1:10) {
  squared_numbers[i] <- practice_vector[i]^2
}

##d
even_numbers <- vector()
odd_numbers <- vector()

##e
for (i in 1:100) {
  if((one_hundred[i] %% 2) == 0){
    even_numbers[i] = one_hundred[i]
  }
  else{
    odd_numbers[i] = one_hundred[i]
  }
}
even_numbers <- even_numbers[!is.na(even_numbers)]
odd_numbers <- odd_numbers[!is.na(odd_numbers)]

#3
##a
maximum <- function(vector) {
  for(i in 1:length(vector)) {
    return(print(max(vector)))
  }
}
maximum(vector = practice_vector)

##b
hist_gg <- function(tibble, column, xlabel, title){
  ggplot(data = tibble) + 
    geom_histogram(aes(x = column)) +
    xlab(xlabel) +
    labs(title = title) +
    theme_light()
}

#4
##a
top10 <- airbnb %>% 
  count(neighbourhood) %>%
  filter(!is.na(neighbourhood)) %>%
  top_n(10)
top10

##b
top10_reordered <- airbnb %>%
  select(neighbourhood) %>%
  filter(!is.na(neighbourhood)) %>%
  group_by(neighbourhood) %>%
  count() %>%
  ungroup(neighbourhood) %>%
  top_n(10, n) %>%
  mutate(neighbourhood_reordered = fct_reorder(neighbourhood, n, .desc = TRUE))

##c
ggplot(top10_reordered) +
  geom_col(aes(x = neighbourhood_reordered, y = n)) +
  coord_flip() +
  xlab("") +
  ylab("") +
  labs(title = "Most Frequently Listed Neighbourhoods in Seattle for Airbnb") +
  theme_light()

##d
stacked_gg <- function(tibble, column, n, title){
  ggplot(data = tibble) +
    geom_col(aes(x = column, y = n)) +
    coord_flip() +
    xlab("") +
    ylab("") +
    labs(title = title) +
    theme_light()
}
stacked_gg(top10_reordered, 
           top10_reordered$neighbourhood_reordered,
           top10_reordered$n,
           "Most Frequently Listed Neighbourhoods in Seattle for Airbnb")

#5
my_distinct <- function(x){
  a <- NULL
  for (i in 1:length(x)) {
    if(typeof(x)=="double" | typeof(x)=="integer"){
      if(x[i] %in% a){
        a <- a
      } else {
        a <- c(a, x[i])
      }
    } else{
      return("Type Error! Try again.")
    }
  }
  return(a)
}

#Write-up
my_distinct <- function(x){ ##Names the function and set the input for the function
  a <- NULL ##sets a empty variable/vector to be filled later
  for (i in 1:length(x)) { ##set up the for loop and i variable equal to length of input vector. 
    if(typeof(x)=="double" | typeof(x)=="integer"){ ##This line checks if the input is a double or integer
      if(x[i] %in% a){ ##Checks if the input is already in the 'a' vector so there are no repeats
        a <- a ##if there are repeats, this line will not add it to the vector
      } else { ##if the input is not a repeat it will come here
        a <- c(a, x[i]) ##This adds the input to the vector
      }
    } else{ ##if the input is not a double or integer, it will come here
      return("Type Error! Try again.") ##This prints out "Type Error! Try again." and ends the function
    }
  }
  return(a) ##if the function has not ended already, the function will print out the 'a' vector
}           ##with all the unique values of the input vector

