# This clears the environment
rm(list = ls())

# Load wd
getwd()
setwd("C:/Users/yunus/OneDrive/Documenti/Desktop")
getwd()

# Load the data
ratings_data = read.csv("C:/Users/yunus/Downloads/archive/rotten_tomatoes_movies.csv",100)
critic_reviews = read.csv("C:/Users/yunus/Downloads/archive (2)/IMDB_dataset_320.000_reviews.csv",100)

# data exploration (customer_reviews)

install.packages("tidyverse")
library(tidyverse)
dim(ratings_data) #The "ratings_data" Data Frame is composed by 17712 observations and 22 variables
glimpse(ratings_data)#The variables are characters and integers
view(ratings_data)
names(ratings_data)#the variables are about the characteristics of the films (movie_title,movie_info, ...), about the characteristics of the tomatometer critics(...) and about the characteristics of the audience (...)


# data exploration (critic_reviews)

dim(critic_reviews) 
glimpse(critic_reviews)
view(critic_reviews)
names(critic_reviews)

###################################


#compare critic and audience reviews

t.test <- t.test(ratings_data$tomatometer_rating, ratings_data$audience_rating)
t.test

#content rating and genres

#creating a vector, to have the unique values 

(content_rating <- unique(ratings_data$content_rating))

(content.rating <- unique(ratings_data$content_rating))

#for loop t.test

list.a <- list()

for (i in content_rating) {
  a <- t.test(ratings_data$tomatometer_rating[ratings_data$content_rating == i],
              ratings_data$audience_rating[ratings_data$content_rating == i])
  list.a[[i]] <- a
}

list.a


#replacing values --> with this code with replaced NR with G 

(ratings_data$content_rating[ratings_data$content_rating == 'NR'] <- 'G')

#########SENTIMENT ANALYSIS PART############
getwd()
setwd("C:/Users/yunus/Downloads/archive (7)")
getwd()

# Load the data
ratings_data = read.csv("C:/Users/yunus/Downloads/archive/rotten_tomatoes_movies.csv")
library(tidyverse)
glimpse(ratings_data)
movie_names_data = read.csv("C:/Users/yunus/Downloads/archive (7)/rotten_tomatoes_movies.csv")
glimpse(movie_names_data)
rottentomatoesreviews = read.csv("C:/Users/yunus/Downloads/archive (7)/rotten_tomatoes_movie_reviews.csv")
glimpse(rottentomatoesreviews)   

(rottentomatoesreviews$scoreSentiment[rottentomatoesreviews$scoreSentiment == 'POSITIVE'] <- 1)
(rottentomatoesreviews$scoreSentiment[rottentomatoesreviews$scoreSentiment == 'NEGATIVE'] <- 0)
rottentomatoesreviews$scoreSentiment <- as.numeric(rottentomatoesreviews$scoreSentiment)


# Creating mean sentiment per movie
library(dplyr)
a <- rottentomatoesreviews %>% 
  group_by(id) %>%
  summarise (mean_sentiment = mean(scoreSentiment)) %>%
  arrange(desc(mean_sentiment)) 

# Checking for duplicate movie names
length(movie_names_data$id)
length(unique(movie_names_data$id))

# Proceeding with only unique movie names
movie_names_data <- unique(movie_names_data)

# creating dataset with mean sentiment and movie name
sentiment_data <- movie_names_data %>% 
  select(id, title) %>%
  left_join(a, by = c("id"))

# Removing duplicate movie ids (not names,as some different movies can have the same name)
length(sentiment_data$id) - length(unique(sentiment_data$id))
sentiment_data <- sentiment_data %>% distinct(id, mean_sentiment)

# Creating final dataset for regression, contains sentiment scores
rotten_tomatoes_movies <- movie_names_data %>% 
  left_join(sentiment_data, by = c("id"))

###############REGRESSION PART##################


print(rotten_tomatoes_movies$boxOffice)


## Converting box office var to numerical

rotten_tomatoes_movies$boxOffice <- gsub("M", "000000", rotten_tomatoes_movies$boxOffice)
rotten_tomatoes_movies$boxOffice <- gsub("k", "000", rotten_tomatoes_movies$boxOffice)
rotten_tomatoes_movies$boxOffice <- gsub("\\.", "", rotten_tomatoes_movies$boxOffice)
rotten_tomatoes_movies$boxOffice <- gsub("\\$", "", rotten_tomatoes_movies$boxOffice)

rotten_tomatoes_movies$boxOffice <- as.numeric(rotten_tomatoes_movies$boxOffice)
options(scipen = 999)

#regression 

regression1 <- lm(boxOffice ~ audienceScore + tomatoMeter + mean_sentiment, data = rotten_tomatoes_movies)
summary(regression1) 


#conclusion --> audience affect more the boxoffice, people rely more on the public opinion instead of critics 

#histograms to see the visualization 

library(ggplot2)


#Histogram boxoffice 

summary(rotten_tomatoes_movies$boxOffice)

ggplot(rotten_tomatoes_movies, aes(x = boxOffice)) +
  geom_density(fill = "lightblue", color = "black") +
  labs(title = "Histogram of boxoffice",
       x = "Box Office",
       y = "Frequency") +
  theme_bw()

# Histogram of audienceScore 

ggplot(rotten_tomatoes_movies, aes(x = audienceScore)) +
  geom_density(fill = "purple", color = "black") +
  labs(title = "Histogram of Audience Score",
       x = "Audience Score",
       y = "Frequency") +
  theme_bw()


# Histogram of tomatoMeter 

ggplot(rotten_tomatoes_movies, aes(x = tomatoMeter)) +
  geom_density(fill = "lightcoral", color = "black") +
  labs(title = "Histogram of Tomato Meter",
       x = "Tomato Meter",
       y = "Frequency") +
  theme_bw()

#regression line and scatter plot of audience score

ggplot(rotten_tomatoes_movies, aes(x = audienceScore, y = boxOffice)) +
  geom_point(color = "red", alpha = 0.5, size = 0.5 ) +  
  geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  labs(title = "Box Office vs Audience Score with Regression Line",
       x = "Audience Score",
       y = "Box Office",) +
  theme_bw()

#scatter plot with regression line, tomatometer and audience score together 

ggplot(rotten_tomatoes_movies, aes(x = audienceScore, y = boxOffice)) +
  geom_point(alpha = 0.5, size = 1, color = "blue") +
  geom_point(aes(x = tomatoMeter, y = boxOffice), alpha = 0.2, size = 1, color = "lightcoral") +
  geom_smooth(aes(x = tomatoMeter), method = "lm", se = FALSE, color = "lightgreen") +  # Regression line for tomatoMeter
  geom_smooth(method = "lm", se = FALSE, color = "lightblue") +  # Regression line for audienceScore
  labs(title = "Box Office vs Audience Score and Tomato Meter with Regression Lines",
       x = "Audience Score and Tomato Meter",
       y = "Box Office") +
  theme_bw()




summary(rotten_tomatoes_movies$tomatoMeter)












