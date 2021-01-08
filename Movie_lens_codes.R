#loading libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dslabs)) install.packages("dslabs", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(tidyverse)
library(ggplot2)
library(dslabs)
library(knitr)
library(kableExtra)
library(lubridate)
library(Matrix)
library(scales)
library(dplyr)
options(digits=4)
options(warn=-1)
options(dplyr.summarise.inform = FALSE)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#Loading data, provided in the course

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
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

#***************************************************************************************

# edx summary and check for NAs
summary(edx)
head(edx)%>% kable %>% kable_styling()
sum(is.na(edx))
nrow(edx)

#distinct user and distinct movies
edx%>% summarise(distinct_user = n_distinct(userId),
                 distinct_movies = n_distinct(movieId))


#rating plot as histogram

edx %>% ggplot(aes(rating)) +
  geom_histogram(binwidth = 0.25, color = "black", fill = "royalblue3")+
  scale_y_continuous(breaks = seq(0,2500000,500000))+
  ggtitle("Movie rating distribution")+
  theme(plot.title = element_text(hjust = 0.5,size=15,face = "italic"), axis.text = element_text(size=12,face = "bold"),panel.border = element_rect(colour = "gray80", fill=NA, size=1))+
  labs(x="Rating", y="count")

# distribution chart of rating
edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram( bins=25, color = "white", fill ="gray20") +
  scale_x_log10()+ggtitle("Movie rating distribution")+
  theme(plot.title = element_text(hjust = 0.5,size=15,face = "italic"), axis.text = element_text(size=12,face = "bold"),panel.border = element_rect(colour = "gray80", fill=NA, size=1))+
  labs(x="n", y="count")

#sparse chart
users<- sample(unique(edx$userId),100)
edx%>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")


#user distribution
edx %>%
  dplyr::count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 25, color = "white",fill = "cyan4") + 
  scale_x_log10() +
  ggtitle("User Distribution")+theme(plot.title = element_text(hjust = 0.5,size=15,face = "italic"), axis.text = element_text(size=12,face = "bold"))+
  labs(x="No_of_users", y="Rating_count")


# Genres
edx %>% 
  summarise(n_Genre = n_distinct(genres))


#separating genres
edx %>%
  separate_rows(genres, sep = "\\|") %>% # separate genres with '|'
  group_by(genres) %>%
  summarise(n = n()) %>%
  arrange(desc(n))%>%kable(col.names = c("Genre","Total rating"),format.args = list(big.mark = ",", 
                                                                                    scientific = FALSE))%>%kable_styling()
            
#display genres having more than or equal to 100000 rating
edx %>% group_by(genres) %>%
  summarise(n=n(), avg = mean(rating)) %>%
  filter ( n >=100000) %>%
  mutate(genres = reorder(genres,avg)) %>%
  ggplot(aes(avg,genres))+
  geom_point(color = "royalblue",size = 3)+labs(x="Avg Rating",y="Genre")+
  theme(axis.text = element_text(size=12,face = "bold"))


#splitting yer from title, first with brackets and then without
edx <- edx %>%
  mutate(first_split=str_extract(title,regex("\\((\\d{4})\\)")),
         r_year=str_extract(first_split, regex("(\\d{4})")),
         r_year =as.numeric(r_year))%>%
  select(-first_split)


# display plot with ratings over years
mean = 3.51
edx %>% group_by(r_year) %>%
  summarise(n =n(), avg = mean(rating)) %>%
  ggplot(aes(r_year, avg))+
  geom_point()+geom_hline(yintercept = mean, color = "red")+
  labs(x="Release year",y="Avg rating")+
  theme(axis.text = element_text(size=12,face = "bold"))

edx %>% group_by(r_year) %>% summarise(n = n())%>%
  kable%>%kable_styling()


#count of ratings over years
edx %>% group_by(r_year) %>%
  summarise(ratings = n()) %>%
  ggplot(aes(r_year, ratings)) +
  geom_bar(stat = "identity", fill = "gray1", color = "white")+
  scale_y_continuous(labels = comma)+labs(x="Release year",y="no_rating")+
  theme(axis.text = element_text(size=12,face = "bold"))

#***************************************************************************#

#setting edx to edx_mod

edx_mod <- edx

#setting validation to validation_mod
validation_mod <- validation

#first splitting year with brackets from title and removing the temp column in the final data set

validation_mod <- validation_mod %>%
  mutate(first_split = str_extract(title, regex("\\((\\d{4})\\)")),
         r_year= str_extract(first_split, regex("(\\d{4})")),
         r_year =as.numeric(r_year)) %>%
  select(-first_split)


#creating test and training set from edx
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx_mod$rating, times = 1, p = 0.1, list = FALSE) #90-10 split

train_edx <- edx_mod[-test_index,]
temp <- edx_mod[test_index,]


# Check to ensure all 'users' and 'movies' from test are in training set
test_edx <- temp %>% 
  semi_join(train_edx, by = "movieId") %>%
  semi_join(train_edx, by = "userId")

nrow(test_edx)
nrow(train_edx)

# First Model - Naïve bias:
mu_hat <- mean(train_edx$rating)
mu_hat

#prediction
naive_rmse <- RMSE(test_edx$rating, mu_hat)
naive_rmse
#table of results
rmse_results <- data_frame(method = "Naive_RMSE", RMSE = naive_rmse)
rmse_results %>%kable%>%kable_styling(position ="center")

# 1.060054


#summarize mean with movie_bias
movie_bias <- train_edx %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu_hat))

sum(is.na(movie_bias))
#prediction
predicted_b_i <- mu_hat + test_edx %>%
  left_join(movie_bias, by = "movieId")%>%
              .$b_i

#calculate RMSE with movie bias

movie_rmse <- RMSE(predicted_b_i, test_edx$rating)
movie_rmse

#binding result to previous table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Effect Model",
                                     RMSE = movie_rmse ))
rmse_results %>%kable%>%kable_styling(position="center")


# user effect (b_u)
user_bias <- train_edx %>%
  left_join(movie_bias, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_hat - b_i))
# predicting user effects

predicted_b_u <- test_edx %>%
  left_join(movie_bias, by = "movieId") %>%
  left_join(user_bias, by = "userId") %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  .$pred

#Calculate RMSE off user effect b_u

user_rmse <- RMSE(predicted_b_u, test_edx$rating)
user_rmse

#table of results
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + User Effects Model",  
                                     RMSE = user_rmse ))
rmse_results %>% kable%>%kable_styling(position="center")


# release year effect
year_bias <- train_edx %>%
  left_join(movie_bias, by = "movieId")%>%
  left_join(user_bias, by = "userId") %>%
  group_by(r_year)%>%
  summarise(b_yr = mean(rating - mu_hat - b_i-b_u))

#prediction
predicted_b_yr <- test_edx %>%
  left_join(movie_bias, by = "movieId") %>%
  left_join(user_bias, by = "userId") %>%
  left_join(year_bias, by ="r_year")%>%
  mutate(pred = mu_hat + b_i + b_u + b_yr) %>%
  .$pred

#rmse for year
release_year_rmse <- RMSE(predicted_b_yr, test_edx$rating)
release_year_rmse

#results table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie + user+Release year Effects Model",  
                                     RMSE = release_year_rmse ))
rmse_results %>%kable%>%kable_styling(position="center")


# g_bias <- train_edx %>%
#   left_join(movie_bias, by = "movieId")%>%
#   left_join(user_bias, by = "userId") %>%
#   left_join(year_bias, by ="r_year")%>%
#   group_by(genres)%>%
#   summarize(b_g = mean(rating - mu_hat - b_i-b_u-b_yr))
# 
# predicted_b_g <- test_edx %>%
#   left_join(movie_bias, by = "movieId") %>%
#   left_join(user_bias, by = "userId") %>%
#   left_join(year_bias, by ="r_year")%>%
#   left_join(g_bias, by = "genres")%>%
#   mutate(pred = mu_hat + b_i + b_u + b_yr+b_g) %>%
#   .$pred
# sum(is.na(predicted_b_g))
# #rmse for bias
# genre_rmse <- RMSE(predicted_b_g, test_edx$rating)
# genre_rmse


# Predicting on the 'Validation' data
predicted_val <- validation_mod %>%
  left_join(movie_bias, by = "movieId") %>%
  left_join(user_bias, by = "userId") %>%
  left_join(year_bias, by ="r_year")%>%
  mutate(pred = mu_hat + b_i + b_u + b_yr) %>%
  .$pred
predicted_val[is.na(predicted_val)] <- 0 #setting 4NA's to 0
# prediction
rmse_val <- RMSE(predicted_val, validation_mod$rating)
rmse_val

#displaying titles of top movies with least 5 star reviews

movie_titles <- edx_mod %>% 
  select(movieId, title) %>%
  distinct()

movie_b <- movie_bias %>% left_join(movie_titles, by="movieId")

movie_b%>% arrange(desc(b_i)) %>% slice(1:10)  %>% 
  pull(title)


# predicting lambda value
lambdas <- seq(3,6,0.1) #sequence to run lambdas

#function for lambda
rmses <- sapply(lambdas, function(l){
  
  mu_hat <- mean(train_edx$rating)
  
  b_i <- train_edx %>%
    group_by(movieId) %>%
    summarise(b_i =sum(rating - mu_hat)/(n()+l))
  b_u <- train_edx %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - b_i - mu_hat)/(n()+l))
  b_yr <- train_edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by ="userId") %>%
    group_by(r_year) %>%
    summarise(b_yr = sum(rating - b_i - b_u - mu_hat)/(n()+l))
  predicted_ratings <- test_edx %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_yr, by="r_year") %>%
    mutate(pred = mu_hat + b_i +b_u+b_yr) %>%
    .$pred
  return(RMSE(predicted_ratings, test_edx$rating))
})


qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]
lambda


#including lambda(regularization) with predictions
b_i <- edx_mod %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu_hat)/(n()+lambda))

b_u <- edx_mod %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - b_i - mu_hat)/(n()+lambda))

b_yr <- edx_mod %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(r_year) %>%
  summarise(b_yr = sum(rating - b_i - b_u - mu_hat)/(n()+lambda))

#final prediction on validation set
predicted_ratings_regularized <- validation_mod %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_yr, by="r_year") %>%
  mutate(pred = mu_hat + b_i + b_u + b_yr) %>%
  pull(pred)

#prediction on validation(hand out) set
validation_rmse <- RMSE(validation_mod$rating, predicted_ratings_regularized)
validation_rmse




