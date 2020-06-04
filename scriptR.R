###########################################################
# Create edx set, validation set
###########################################################

# Note: this process could take a couple of minutes

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

##### library used
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
library(lubridate)

options(digits = 5)
library(knitr)
library(rmarkdown)

################################
# Download source file
################################

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")


########################################
# Dividing into Edx set (for training) and Validation set (for validating RMSE) 
########################################

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)


#################################################################################
# Methods and Analysis
################################################################################

######################
## Data Description
######################

### Exploring the datatype and type of variables in the dataset.
str(edx)


# To find out the number of unique movies and unique users are in the edx dataset?
edx%>%
  summarize(n_users = n_distinct(userId), 
            n_movies = n_distinct(movieId))


# Positive skwed distribution, showing that there are many movies with few ratings
edx %>%
  group_by(movieId)%>%
  summarize(count = n()) %>%
  arrange(desc(count))%>%
  ggplot(aes(count))+geom_histogram(fill="blue2",color="goldenrod")+ylab("Number of Movies")+xlab("Number of Ratings")



##### Relationship of number of movie ratings with rating received
##### Movie with the most number of movie ratings appears to have have higher rating
edx %>% 
  group_by(movieId, title) %>%
  summarize(count = n(), rating = mean(rating)) %>%
  arrange(desc(count))%>%
  top_n(10, count)

#### Movie with the least number of movie ratings appears to have lower rating
edx %>% group_by(movieId, title) %>%
  summarize(count = n(), rating = mean(rating)) %>%
  arrange(count)%>%
  top_n(10, count)

##### Distribution graph reveals that movie with better rating = more reviews, and movies with poor rating = fewer review
##### possible effect of ratings per movie on movie rating
edx %>% 
  group_by(movieId, title) %>%
  summarize(count = n(), rating = mean(rating)) %>%
  ggplot(aes(x=rating,y=count))+
  geom_smooth()+
  xlab("Movie Rating")+
  ylab("Number of user ratings received")

# This plot reveals that most users provide few movie ratings
edx %>% 
  group_by(userId) %>%
  summarize(count = n()) %>%
  arrange(desc(count))%>%
  ggplot(aes(count))+
  geom_histogram (fill="blue2",color="goldenrod")+
  ylab("Number of Users")+
  xlab("Number of Movies Ratings")

# User who provide the most reviews appears to give movie ratings closer to 3 
edx %>% 
  group_by(userId) %>%
  summarize(count = n(), rating = mean(rating)) %>%
  arrange(desc(count))%>%
  top_n(count, 10)

# User who provide the least reviews appear to give more diverse ratings
edx %>% 
  group_by(userId) %>%
  summarize(count = n(), rating = mean(rating)) %>%
  arrange(count)%>%
  top_n(count, 10)

# Finding the distribution of movie ratings given the number of reviews received
edx %>% 
  group_by(userId) %>%
  summarize(count = n(), rating = mean(rating)) %>%
  arrange(desc(count))%>%
  with(quantile(count, c(0.25,0.5,0.75)))

#Users who provide the most number of reviews (top 75th percentile) tend to give rating that is normally distributed around 3.5
edx %>% 
  group_by(userId) %>%
  summarize(count = n(), rating = mean(rating)) %>%
  filter(count > 141)%>%
  ggplot(aes(x = rating))+
  geom_histogram()+
  ylab("Numbers of Users")+
  ggtitle("Ratings given by Users with the most number of reviews")

#Users who provide the least number of reviews (bottom 25th percentile) also tend to give rating that is normally distributed around 3.5
edx %>% 
  group_by(userId) %>%
  summarize(count = n(), rating = mean(rating)) %>%
  filter(count <= 32)%>%
  ggplot(aes(x = rating))+
  geom_histogram()+
  ylab("Numbers of Users")+
  ggtitle("Ratings given by Users with the least number of reviews")


# Users who are between 25th and 75th percentile also tend to give rating that is normally distributed around 3.5
# Rating per users have no effect on movie rating
edx %>% 
  group_by(userId) %>%
  summarize(count = n(), rating = mean(rating)) %>%
  filter(count > 32 | count <= 141)%>%
  ggplot(aes(x = rating))+
  geom_histogram()+
  ylab("Numbers of Users")+
  ggtitle("Ratings given by Users within the 25th to 75th percentile")


# Distribution of ratings
edx %>% 
  group_by(rating) %>% 
  summarize(count = n()) %>% 
  top_n(5) %>%
  arrange(desc(count))

#This plot reveals discretization effect in movie rating
options(scipen=6)
edx %>%
  group_by(rating) %>%
  ggplot(aes(x = rating)) +
  geom_bar(fill="blue2", color="goldenrod")+
  ylab("NUmbers of Movies")

##########################################
# Mutating a variable called "release_year" (ie., the movie's year of release )
#########################################

# String process "movie title" to extract the year of release of movie
pattern<-"[(]+(\\d{4})[)]+$"
edx_with_year<- edx %>% 
  mutate(release_year=parse_number(str_extract(title, pattern)))

# check for accuracy of string processing
head(edx_with_year)
range(edx_with_year$release_year)
str(edx_with_year)

# This plot shows that movies released in the 1990s has the highest number of movie ratings
edx_with_year%>%
  group_by(movieId) %>%
  summarize(n = n(), year = mean(release_year))%>%
  qplot(as.character(year), n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  xlab("Year of Movie Release")+
  ylab("Number of Movie Ratings")

# The year with the highest median number of ratings
edx_with_year_and_n <- edx_with_year%>%
  group_by(movieId) %>%
  summarize(n = n(), year = mean(release_year))
edx_with_year_index <- edx_with_year%>%
  group_by(movieId) %>%
  summarize(n = n(), year = mean(release_year))%>%
  with(max(median(n)))
edx_with_year_and_n$year[edx_with_year_index]

## Top 25 movies with the most ratings per year, along with thir average ratings
edx_with_year%>% 
  filter(release_year >= 1992) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - mean(release_year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))%>%print.data.frame()

## This plot shows that movies with more review/year are likely to have higher movie rating
edx_with_year %>% 
  filter(release_year >= 1992) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - mean(release_year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()+
  xlab("Number of Reviews per Year")+
  ylab("Movie Rating")

##########################################
# Mutating relevant variables 
#########################################

## Mutate variables (rate_date, time_btw, rate, total_r)
edx_with_year_date <- edx_with_year%>%
  mutate(date = as_datetime(timestamp))%>%
  mutate(rate_date = round_date(date, unit = "week"),
         time_btw = 2018 - release_year)

edx_with_year_week <- edx_with_year_date%>%
  group_by(movieId)%>%
  summarize(rate = n()/mean(time_btw),
            total_r = n())

edx_with_year_week_2 <- edx_with_year_date %>%
  left_join(edx_with_year_week, by='movieId')

str(edx_with_year_week_2)

## Compute the average rating for each week and plot this average against date.
edx_with_year_week_2 %>% 
  group_by(rate_date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(rate_date, rating)) +
  geom_point() +
  geom_smooth() +
  xlab("Week of rating")+
  ggtitle("Time and Movie rating")

## To check for genre effect 
edx_with_year_week_2 %>% 
  group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 10000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################
# Analysing edx dataset
################################

#################
# Partitioning edx dataset into test set and train set
#################
test_index <- createDataPartition(y = edx_with_year_week_2$rating, times = 1, p = 0.5, list = FALSE)
edx_train <- edx_with_year_week_2[-test_index,]
temp <- edx_with_year_week_2[test_index,]

# Make sure userId and movieId in validation set are also in edx set
edx_test <- temp %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, edx_test)
edx_train <- rbind(edx_train, removed)
rm(test_index, temp,removed)



# create a RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## Linear modelling approach ###

#mean movie ratings in train set
mu <- mean(edx_train$rating)
mu

just_mean_effect<-RMSE(mu, edx_test$rating)
just_mean_effect

## Movie Individual Effect ##
movie_ind_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + edx_test %>%
  left_join(movie_ind_avgs, by='movieId') %>%
  pull(b_i)

movie_ind_effect<-RMSE(predicted_ratings, edx_test$rating)
movie_ind_effect

## User Individual Effect ##
user_ind_avgs <- edx_train %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu))

predicted_ratings <- mu + edx_test %>%
  left_join(user_ind_avgs, by='userId') %>%
  pull(b_u)

user_ind_effect<-RMSE(predicted_ratings, edx_test$rating)
user_ind_effect

## Year of Release individual effect ##
release_yr_ind_avgs <- edx_train %>%
  group_by(release_year) %>%
  summarize(b_rel_yr = mean(rating - mu))

predicted_ratings <- mu + edx_test %>%
  left_join(release_yr_ind_avgs, by='release_year') %>%
  pull(b_rel_yr)

release_yr_ind_effect<-RMSE(predicted_ratings, edx_test$rating)
release_yr_ind_effect

## Date of User Rating individual effect ##
date_ind_avgs <- edx_train %>%
  group_by(rate_date) %>%
  summarize(b_dt = mean(rating - mu))

predicted_ratings <- mu + edx_test %>%
  left_join(date_ind_avgs, by='rate_date') %>%
  pull(b_dt)

date_ind_effect<-RMSE(predicted_ratings, edx_test$rating)
date_ind_effect

## Number of ratings received per year (ie., rate) individual effect ##
rate_ind_avgs <- edx_train %>%
  group_by(rate) %>%
  summarize(b_r = mean(rating - mu))

predicted_ratings <- mu + edx_test %>%
  left_join(rate_ind_avgs, by='rate') %>%
  pull(b_r)

rate_ind_effect<-RMSE(predicted_ratings, edx_test$rating)
rate_ind_effect

## Genre individual effect ##
genre_ind_avgs <- edx_train %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu))

predicted_ratings <- mu + edx_test %>%
  left_join(genre_ind_avgs, by='genres') %>%
  pull(b_g)

genre_ind_effect<-RMSE(predicted_ratings, edx_test$rating)
genre_ind_effect

## Time between individual effect ##

time_ind_avgs <- edx_train %>%
  group_by(time_btw) %>%
  summarize(b_time = mean(rating - mu))

qplot(b_time, data = time_ind_avgs, bins = 10, color = I("black"))

predicted_ratings <- mu + edx_test %>%
  left_join(time_ind_avgs, by='time_btw') %>%
  pull(b_time)

time_ind_effect<-RMSE(predicted_ratings, edx_test$rating)
time_ind_effect

rmse_results <- tibble(method = c("Just the average",
                                  "Just Movie Model", 
                                  "Just User Model", 
                                  "Just Release Year Model", 
                                  "Just Timestamp Model",
                                  "Just Between Time Model",
                                  "Just Rate per Year Model",
                                  "Just Genre Model"), 
                       RMSE = c(just_mean_effect, 
                                movie_ind_effect,
                                user_ind_effect,
                                release_yr_ind_effect,
                                date_ind_effect,
                                time_ind_effect,
                                rate_ind_effect,
                                genre_ind_effect))
rmse_results<-rmse_results%>%arrange(RMSE)
rmse_results

#######################################
## Building a linearised model consisting of movie effect,user effect,release year, and time between.
######################################

###1) Movie + rate model 

movie_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

rate_avgs <- edx_train %>%
  left_join(movie_avgs, by='movieId')%>%
  group_by(rate)%>%
  summarize(b_r = mean(rating - mu - b_i))

predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by = 'movieId')%>%
  left_join(rate_avgs, by = 'rate')%>%
  mutate(pred = mu + b_i + b_r)%>%
  pull(pred)

movie_rate_model <- RMSE(predicted_ratings, edx_test$rating)
movie_rate_model


####2) movie + user model

movie_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

user_avgs <- edx_train %>%
  left_join(movie_avgs, by='movieId')%>%
  group_by(userId)%>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by = 'movieId')%>%
  left_join(user_avgs, by = 'userId')%>%
  mutate(pred = mu + b_i + b_u)%>%
  pull(pred)

movie_user_model <- RMSE(predicted_ratings, edx_test$rating)
movie_user_model



####3) movie + user + genre model

movie_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

user_avgs <- edx_train %>%
  left_join(movie_avgs, by ='movieId')%>%
  group_by(userId)%>%
  summarize(b_u = mean(rating - mu - b_i))

genres_avgs<-edx_train%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  group_by(genres)%>%
  summarize(b_g=mean(rating - mu - b_i - b_u))

predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by = 'movieId')%>%
  left_join(user_avgs, by = 'userId')%>%
  left_join(genres_avgs, by='genres')%>%
  mutate(pred = mu + b_i + b_u + b_g)%>%
  pull(pred)

movie_user_genres_model <- RMSE(predicted_ratings, edx_test$rating)
movie_user_genres_model


####4) movie + user + genre + rate_date model

movie_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

user_avgs <- edx_train %>%
  left_join(movie_avgs, by ='movieId')%>%
  group_by(userId)%>%
  summarize(b_u = mean(rating - mu - b_i))

genres_avgs<-edx_train%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  group_by(genres)%>%
  summarize(b_g=mean(rating - mu - b_i - b_u))

rate_date_avgs<-edx_train%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  left_join(genres_avgs, by='genres')%>%
  group_by(rate_date)%>%
  summarize(b_t=mean(rating - mu - b_i - b_u - b_g))

predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by = 'movieId')%>%
  left_join(user_avgs, by = 'userId')%>%
  left_join(genres_avgs, by='genres')%>%
  left_join(rate_date_avgs,by='rate_date')%>%
  mutate(pred = mu + b_i + b_u + b_g + b_t)%>%
  pull(pred)

movie_user_genres_ratedate_model <- RMSE(predicted_ratings, edx_test$rating)
movie_user_genres_ratedate_model

rmse_results_model <- tibble(method = c("Movie + Rate Model",
                                        "Movie + User Model",
                                        "Movie + User + Genres Model",
                                        "Movie + User + Genres + Rate_date Model"), 
                             RMSE = c(movie_rate_model, 
                                      movie_user_model,
                                      movie_user_genres_model,
                                      movie_user_genres_ratedate_model))


rmse_results_model <- rmse_results_model%>%
  arrange(RMSE)%>%print.data.frame()


#########################
## Finding lambdas value for movie and user bias
########################
######## 1) Finding movie lambda value (ie., the value is 4)

lambdas <- seq(3.5, 5, 0.25) # Multiple small brackets were used to isolate the lambda in order to save processing time

rmses <- sapply(lambdas, function(l){
  
  movie_avgs <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  user_avgs <- edx_train %>%
    left_join(movie_avgs, by ='movieId')%>%
    group_by(userId)%>%
    summarize(b_u = mean(rating - mu - b_i))
  
  genres_avgs<-edx_train%>%
    left_join(movie_avgs,by='movieId')%>%
    left_join(user_avgs,by='userId')%>%
    group_by(genres)%>%
    summarize(b_g=mean(rating - mu - b_i - b_u))
  
  rate_date_avgs<-edx_train%>%
    left_join(movie_avgs,by='movieId')%>%
    left_join(user_avgs,by='userId')%>%
    left_join(genres_avgs, by='genres')%>%
    group_by(rate_date)%>%
    summarize(b_t=mean(rating - mu - b_i - b_u - b_g))
  
  predicted_ratings <- edx_test %>%
    left_join(movie_avgs, by = 'movieId')%>%
    left_join(user_avgs, by = 'userId')%>%
    left_join(genres_avgs, by='genres')%>%
    left_join(rate_date_avgs,by='rate_date')%>%
    mutate(pred = mu + b_i + b_u + b_g + b_t)%>%
    pull(pred)
  return(RMSE(predicted_ratings, edx_test$rating))
})

qplot(lambdas, rmses)
lambda_movie<-lambdas[which.min(rmses)]



######## 2) Finding user lambda value (ie., the value is 5.25)

lambdas <- seq(4.75, 5.5, 0.25) # Multiple small brackets were used to isolate the lambda in order to save processing time

rmses <- sapply(lambdas, function(l){
  movie_avgs <- edx_train %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+lambda_movie))
  
  user_avgs <- edx_train %>%
    left_join(movie_avgs, by ='movieId')%>%
    group_by(userId)%>%
    summarize(b_u = sum(rating - mu - b_i)/(n()+l))
  
  genres_avgs<-edx_train%>%
    left_join(movie_avgs,by='movieId')%>%
    left_join(user_avgs,by='userId')%>%
    group_by(genres)%>%
    summarize(b_g=mean(rating - mu - b_i - b_u))
  
  rate_date_avgs<-edx_train%>%
    left_join(movie_avgs,by='movieId')%>%
    left_join(user_avgs,by='userId')%>%
    left_join(genres_avgs, by='genres')%>%
    group_by(rate_date)%>%
    summarize(b_t=mean(rating - mu - b_i - b_u - b_g))
  
  predicted_ratings <- edx_test %>%
    left_join(movie_avgs, by = 'movieId')%>%
    left_join(user_avgs, by = 'userId')%>%
    left_join(genres_avgs, by='genres')%>%
    left_join(rate_date_avgs,by='rate_date')%>%
    mutate(pred = mu + b_i + b_u + b_g + b_t)%>%
    pull(pred)
  
  return(RMSE(predicted_ratings, edx_test$rating))
})

qplot(lambdas, rmses)
lambda_user <- lambdas[which.min(rmses)]


######################
##fitting lambda value into linearlised model (FINAL TRAINING MODEL), RMSE (edx_test) = 0.8679
######################

movie_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda_movie))

user_avgs <- edx_train %>%
  left_join(movie_avgs, by ='movieId')%>%
  group_by(userId)%>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_user))

genres_avgs<-edx_train%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  group_by(genres)%>%
  summarize(b_g=mean(rating - mu - b_i - b_u))

rate_date_avgs<-edx_train%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  left_join(genres_avgs, by='genres')%>%
  group_by(rate_date)%>%
  summarize(b_t=mean(rating - mu - b_i - b_u - b_g))

predicted_ratings <- edx_test %>%
  left_join(movie_avgs, by = 'movieId')%>%
  left_join(user_avgs, by = 'userId')%>%
  left_join(genres_avgs, by='genres')%>%
  left_join(rate_date_avgs,by='rate_date')%>%
  mutate(pred = mu + b_i + b_u + b_g + b_t)%>%
  pull(pred)

RMSE(predicted_ratings, edx_test$rating)

####################################################################

###FINAL TEST on Validation set

###################################################################


validation_mutate <- validation %>% 
  mutate(date = as_datetime(timestamp))%>%
  mutate(rate_date = round_date(date, unit = "week"))

mu <- mean(edx_train$rating)

movie_avgs <- edx_train %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda_movie))

user_avgs <- edx_train %>%
  left_join(movie_avgs, by ='movieId')%>%
  group_by(userId)%>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+lambda_user))

genres_avgs<-edx_train%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  group_by(genres)%>%
  summarize(b_g=mean(rating - mu - b_i - b_u))

rate_date_avgs<-edx_train%>%
  left_join(movie_avgs,by='movieId')%>%
  left_join(user_avgs,by='userId')%>%
  left_join(genres_avgs, by='genres')%>%
  group_by(rate_date)%>%
  summarize(b_t=mean(rating - mu - b_i - b_u - b_g))

predicted_ratings <- validation_mutate %>%
  left_join(movie_avgs, by = 'movieId')%>%
  left_join(user_avgs, by = 'userId')%>%
  left_join(genres_avgs, by='genres')%>%
  left_join(rate_date_avgs,by='rate_date')%>%
  mutate(pred = mu + b_i + b_u + b_g + b_t)%>%
  pull(pred)

RMSE(predicted_ratings, validation_mutate$rating)


