
##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
#if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

#install.packages("lubridate")
#install.packages("tidyr")
#install.packages("caret")
#install.packages("purrr")

library(purrr)
library(tidyverse)
library(caret)
#library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)
#
dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")

movies <- movies %>%
  mutate(movieId = as.integer(movieId) )


movielens <- left_join(ratings, movies, by = "movieId")
#head(movielens)


# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)
#head(edx)


edx1 <- edx %>% mutate(ratingdate=as_date(as_datetime(timestamp)),
                      ratingyear=as.numeric(format(ratingdate,"%Y")),
                      ratingmonth=as.numeric(format(ratingdate,"%m")),
                      releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}")),title = str_remove(title, "[/(]\\d{4}[/)]$")),
                      nostalgialevel=ratingyear-releaseyear,
                      nostalgiasplit=case_when(nostalgialevel >=10 ~  "NostalgialMovies",TRUE ~"inDecade"),
                      release_year_era=case_when(releaseyear >=1910 & releaseyear <=1919 ~  "1910s",                # define the decades of the release date
                                                 releaseyear >=1920 & releaseyear <=1929~  "1920s", 
                                                 releaseyear >=1930 & releaseyear <=1939 ~  "1930s",
                                                 releaseyear >=1940 & releaseyear <=1949 ~  "1940s",
                                                 releaseyear >=1950 & releaseyear <=1959 ~  "1950s",
                                                 releaseyear >=1960 & releaseyear <=1969 ~  "1960s",
                                                 releaseyear >=1970 & releaseyear <=1979~  "1970s",
                                                 releaseyear >=1980 & releaseyear <=1989 ~  "1980s",
                                                 releaseyear >=1990 & releaseyear <=1999 ~  "1990s",
                                                 releaseyear >=2000 & releaseyear <=2009~  "2000s",
                                                 TRUE ~"2010s"),
                      release_year_era_group=case_when (releaseyear >=1910 & releaseyear <=1979~ "1970s & earlier",       # group the decades to : prior 1980s, 1980s,1990s,2000s
                                                       releaseyear >=1980 & releaseyear <=1989~ "1980s",
                                                       releaseyear >=1990 & releaseyear <=1999~ "1990s",
                                                       releaseyear >=2000 & releaseyear <=2009~ "2000s" ,
                                                        TRUE ~"2010s")
                      )

 
                     
 head(edx1)

 rating_mean<-round(mean(edx1$rating), 2)
count_full_star_rating<- format(sum(edx1$rating==1 | edx1$rating==2 | edx1$rating==3 | edx1$rating==4 | edx1$rating==5),big.mark=",",scientific=F) 
count_half_star_rating<- format(sum(edx1$rating==0.5 | edx1$rating==1.5 | edx1$rating==2.5 | edx1$rating==3.5 | edx1$rating==4.5),big.mark=",",scientific=F) 
percent_full_star_rating<-  round((sum(edx1$rating==1 | edx1$rating==2 | edx1$rating==3 | edx1$rating==4 | edx1$rating==5)/nrow(edx1)),3)*100
percent_half_star_rating<- round(sum(edx1$rating==0.5 | edx1$rating==1.5 | edx1$rating==2.5 | edx1$rating==3.5 | edx1$rating==4.5)/nrow(edx1), 3) *100


 # Plot distribution of ratings in the edx dataset
 edx1 %>% ggplot(aes(rating)) +
   geom_histogram(bins=10,col="white", fill = "darkblue") +
   scale_y_continuous(breaks = c(1000000, 2000000), labels = c("1", "2")) +
   labs(x = "Rating", y = "Count (in millions)", caption = "Source: edx dataset") 
 

 #TOP Bottom movies
 
 movies_count_top_m<-edx1  %>%
   group_by(title,movieId) %>%
   summarize(n_rating_of_movie = n(), 
             mu_movie = mean(rating),
             sd_movie = sd(rating)) %>%
   arrange(desc(n_rating_of_movie))
 
 movies_count_bottom_m<-edx1  %>%
   group_by(title,movieId) %>%
   summarize(n_rating_of_movie = n(), 
             mu_movie = mean(rating),
             sd_movie = sd(rating)) %>%
   arrange((n_rating_of_movie))

  movies_count_top_m[1:5,]
 movies_count_bottom_m[1:5,]

 # histogram of distribution of movie ratings
 edx1 %>% 
   dplyr::count(movieId) %>% 
   ggplot(aes(n)) + 
   geom_histogram(bins=20,col="white", fill = "darkblue") + 
   scale_x_log10() + 
   ggtitle("Distribution of Number of Ratings Given to Movies") +
   xlab("Number of Ratings") + 
   ylab("Number of movies")
 
 
 #decades distributed over the star ratings
 
   edx1 %>% group_by (rating,release_year_era) %>%
   count(release_year_era) %>% 
   ggplot(aes(x = release_year_era, y = n/1000, fill= n)) +  # Plot with values on top
   geom_bar(stat = "identity") +
   ggtitle("Release by Decade") +
   labs(fill = "number of ratings", subtitle="Movie release decades distributed over the star ratings", caption ="Note: Release decade category extricate from the title") +
   
   facet_wrap(~ rating,ncol=2)   
   
  # smoothing the rating over time
 
 rating_decade<-edx1 %>% 
   group_by(ratingyear,ratingmonth, release_year_era_group) %>%
   summarize(avgrating = mean(rating))
 #head(rating_decade)
 
 rating_decade%>%
   ggplot(aes(ratingyear, avgrating, colour = release_year_era_group)) +
   geom_point() +
   geom_smooth() +
   theme_bw() + theme(panel.grid = element_blank(),axis.title = element_blank()) +
   labs(colour = "Release decades", 
        title="Timestamp (unit in month)", 
        subtitle="Nostalgia movies recieve more star ratings than recent movies")
   
   # ratingyear distributed over the star ratings splits to nostalgia
   
   movies_per_year <- edx1 %>%
     select(movieId, ratingyear,nostalgiasplit,rating) %>% # select columns we need
     group_by(ratingyear,nostalgiasplit,rating) %>% # group by year
     summarise(countMovies = n())
   

 movies_per_year %>%
   ggplot(aes(x = ratingyear, y = countMovies/1000, fill = nostalgiasplit, colour = nostalgiasplit)) +
  geom_bar(stat = 'identity') +  
   ggtitle("Cunt Movies by Rating Year")+
   facet_wrap(~ rating,ncol=2)
 
 
 
 #genres
 
 genres_count_m<-edx1  %>%
   group_by(genres,movieId) %>%
   summarize(n_rating_of_movie = n(), 
             mu_movie = mean(rating),
             sd_movie = sd(rating)) 
 
 genres_count_g<-edx1  %>%
   group_by(genres) %>%
   summarize(n_rating_of_genres = n())
 
 genres_count<-genres_count_m %>% left_join(genres_count_g, by="genres")%>%
   mutate(rank=rank(n_rating_of_genres))%>%
   arrange(desc(n_rating_of_genres))

 test<-genres_count%>% filter(genres=="Horror")%>%
   arrange(desc(n_rating_of_movie))
 
 
  head(genres_count)
 
 genres_count %>% 
   ggplot( aes(x=reorder(genres,n_rating_of_genres), y=n_rating_of_movie)) +
   geom_segment( data = . %>% filter(rank>40), aes(xend=genres, yend=0)) +
   geom_point(data = . %>% filter(rank>40), size=4,color="darkblue") +
   coord_flip() +
   ggtitle("Movies per Genre") +
   labs(fill = "Number of Ratings", 
        subtitle="Distribute for each genre when each point represent a movie ",
        caption ="Note: movies with genre combinations act as genre of its own ") +
    theme(axis.text.x = element_text(size = 10),
                   axis.text.y = element_text(size = 10),
                   legend.position = "none")
 
 
 # Drama rating distribution   
 genres_count%>%filter(genres=="Drama") %>% ggplot(aes(x = mu_movie )) + 
   geom_histogram(bins=10,col="white", fill = "darkblue") +
   scale_y_continuous(limits = c(0, 1000))+
   ggtitle("Drama Movie Rating Distribution ") +
   theme_minimal()+ theme(panel.grid = element_blank(),axis.title.y = element_blank())
 
 # Horror rating distribution 
 genres_count%>%filter(genres=="Horror") %>% ggplot(aes(x = mu_movie )) + 
   geom_histogram(bins=10,col="white", fill = "darkblue") +
   scale_y_continuous(limits = c(0, 1000))+
   ggtitle("Horror Movie Rating Distribution ") +
   theme_minimal()+ theme(panel.grid = element_blank(),axis.title.y = element_blank())


 # Create train set and test sets from edx1
# set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
 test_index <- createDataPartition(y = edx1$rating, times = 1, p = 0.1, list = FALSE)
 train_set <- edx1[-test_index,]
 temp <- edx1[test_index,]
 
 # Make sure userId and movieId in test set are also in train set
 test_set <- temp %>%
   semi_join(train_set, by = "movieId") %>%
   semi_join(train_set, by = "userId")
 
 # Add rows removed from test set back into train set
 removed <- anti_join(temp, test_set) 
 train_set <- rbind(train_set, removed)
 
 # Remove temporary files to tidy environment
 rm(test_index, temp, removed) 
 
 head(train_set)
 head(movie_avgs)
 

 # Methods - develop, train and test various iterations of the algorithm

 # Calculate the overall average rating across all movies included in train set
 mu_hat <- mean(train_set$rating)
 # Calculate RMSE between each rating included in test set and the overall average
 simple_rmse <- RMSE(test_set$rating, mu_hat)
 

 
 # Estimate movie effect (b_i)
 movie_avgs <- train_set %>%
   group_by(movieId) %>%
   summarise(b_i = mean(rating - mu_hat))
 # Predict ratings adjusting for movie effects
 predicted_b_i <- mu_hat + test_set %>%
   left_join(movie_avgs, by = "movieId") %>%
   pull(b_i)
 # Calculate RMSE based on movie effects model
 movie_rmse <- RMSE(predicted_b_i, test_set$rating)
 movie_rmse

  # Estimate user effect (b_u)
 user_avgs <- train_set %>%
   left_join(movie_avgs, by = "movieId") %>%
   group_by(userId) %>%
   summarise(b_u = mean(rating - mu_hat - b_i))
 # Predict ratings adjusting for movie and user effects
 predicted_b_u <- test_set %>%
   left_join(movie_avgs, by="movieId") %>%
   left_join(user_avgs, by="userId") %>%
   mutate(pred = mu_hat + b_i + b_u) %>%
   pull(pred)
 # Calculate RMSE based on user effects model
 user_rmse <- RMSE(predicted_b_u, test_set$rating)
 user_rmse
 
 # Estimate release year effect (b_y)
 year_avgs <- train_set %>%
   left_join(movie_avgs, by = "movieId") %>%
   left_join(user_avgs, by = "userId") %>%
   group_by(releaseyear) %>%
   summarise(b_y = mean(rating - mu_hat - b_i - b_u  ))
 # Predict ratings adjusting for movie, user and release year effects
 predicted_b_y <- test_set %>%
   left_join(movie_avgs, by = "movieId") %>%
   left_join(user_avgs, by = "userId") %>%
   left_join(year_avgs, by = "releaseyear") %>%
   mutate(pred = mu_hat + b_i + b_u + b_y) %>%
   pull(pred)
 # Calculate RMSE based on year effects model
 year_rmse <- RMSE(predicted_b_y, test_set$rating)
 year_rmse
 
 # Estimate rating date effect (b_r)
 date_avgs <- train_set %>%
   left_join(movie_avgs, by = "movieId") %>%
   left_join(user_avgs, by = "userId") %>%
   left_join(year_avgs, by = "releaseyear") %>%
   group_by(ratingyear) %>%
   summarise(b_r = mean(rating - mu_hat - b_i - b_u - b_y))
 # Predict ratings adjusting for movie, user, release year and rating date effects
 predicted_b_r <- test_set %>%
   left_join(movie_avgs, by = "movieId") %>%
   left_join(user_avgs, by = "userId") %>%
   left_join(year_avgs, by = "releaseyear") %>%
   left_join(date_avgs, by = "ratingyear") %>%
   mutate(pred = mu_hat + b_i + b_u + b_y + b_r) %>%
   pull(pred)
 # Calculate RMSE based on review date effects model
 ratingdate_rmse <- RMSE(predicted_b_r, test_set$rating)
 ratingdate_rmse
 
 # Estimate nostalgia level  effect (b_n)
 nostalgia_avgs <- train_set %>%
   left_join(movie_avgs, by = "movieId") %>%
   left_join(user_avgs, by = "userId") %>%   
   left_join(year_avgs, by = "releaseyear") %>%
   left_join(date_avgs, by = "ratingyear") %>%
   group_by(nostalgialevel) %>%
   summarise(b_n = mean(rating - mu_hat - b_i - b_u - b_y - b_r ))
 # Predict ratings adjusting for movie, user, release year, rating date and nostalgia effects
 predicted_b_n <- test_set %>%
   left_join(movie_avgs, by = "movieId") %>%
   left_join(user_avgs, by = "userId") %>%
   left_join(year_avgs, by = "releaseyear") %>%
   left_join(date_avgs, by = "ratingyear") %>%
   left_join(nostalgia_avgs, by = "nostalgialevel") %>%
   mutate(pred = mu_hat + b_i + b_u + b_y + b_r + b_n) %>%
   pull(pred)
 # Calculate RMSE based on review date effects model
 nostalgia_rmse <- RMSE(predicted_b_n, test_set$rating)
 nostalgia_rmse
 
 # Estimate genre effect (b_g)
 genre_avgs <- train_set %>%
   left_join(movie_avgs, by = "movieId") %>%
   left_join(user_avgs, by = "userId") %>%
   left_join(year_avgs, by = "releaseyear") %>%
   left_join(date_avgs, by = "ratingyear") %>%
   left_join(nostalgia_avgs, by = "nostalgialevel") %>%
   group_by(genres) %>%
   summarise(b_g = mean(rating - mu_hat - b_i - b_u - b_y - b_n))
 # Predict ratings adjusting for movie, user and genre effects
 predicted_b_g <- test_set %>%
   left_join(movie_avgs, by = "movieId") %>%
   left_join(user_avgs, by = "userId") %>%
   left_join(year_avgs, by = "releaseyear") %>%
   left_join(date_avgs, by = "ratingyear") %>%
   left_join(nostalgia_avgs, by = "nostalgialevel") %>%
   left_join(genre_avgs, by = "genres") %>%
   mutate(pred = mu_hat + b_i + b_u + b_y  + b_n + b_g ) %>%
   pull(pred)
  # Calculate RMSE based on genre effects model
  genre_rmse <- RMSE(predicted_b_g, test_set$rating)
  genre_rmse
 

 # Generate a sequence of values for lambda ranging from 3 to 6 with 0.1 increments (inc)
 lambdas <- seq(4, 6, 0.2)
 # Regularise model, predict ratings and calculate RMSE for each value of lambda
 rmses <- sapply(lambdas, function(l){
   b_i <- train_set %>%
     group_by(movieId) %>%
     summarise(b_i = sum(rating - mu_hat)/(n()+l))
   b_u <- train_set %>%
     left_join(b_i, by="movieId") %>%
     group_by(userId) %>%
     summarise(b_u = sum(rating - b_i - mu_hat)/(n()+l)) 
   b_y <- train_set %>%
     left_join(b_i, by="movieId") %>%
     left_join(b_u, by="userId") %>%
     group_by(releaseyear) %>%
     summarise(b_y = sum(rating - b_i - b_u - mu_hat)/(n()+l))
   b_r <- train_set %>%
     left_join(movie_avgs, by = "movieId") %>%
     left_join(user_avgs, by = "userId") %>%
     left_join(year_avgs, by = "releaseyear") %>%
     group_by(ratingyear) %>%
     summarise(b_r = sum(rating - b_i - b_u - b_y - mu_hat)/(n()+l))
   b_n <- train_set %>%
     left_join(b_i, by="movieId") %>%
     left_join(b_u, by="userId") %>%
     left_join(b_y, by="releaseyear") %>%
     left_join(b_r, by="ratingyear") %>%
     group_by(nostalgialevel) %>%
     summarise(b_n = sum(rating - b_i - b_u - b_y - b_r - mu_hat)/(n()+l))
   b_g <- train_set %>%
     left_join(b_i, by="movieId") %>%
     left_join(b_u, by="userId") %>%
     left_join(b_y, by="releaseyear") %>%
     left_join(b_r, by="ratingyear") %>%
     left_join(b_n, by="nostalgialevel") %>%
     group_by(genres) %>%
     summarise(b_g = sum(rating - b_i - b_u  - b_y - b_r - b_n - mu_hat)/(n()+l))
   predicted_ratings <- test_set %>%
     left_join(b_i, by="movieId") %>%
     left_join(b_u, by="userId") %>%
     left_join(b_y, by="releaseyear") %>%
     left_join(b_r, by="ratingyear") %>%
     left_join(b_n, by="nostalgialevel") %>%
     left_join(b_g, by="genres") %>%
     mutate(pred = mu_hat + b_i + b_u + b_y + b_r + b_n + b_g) %>%
     pull(pred)
   return(RMSE(predicted_ratings, test_set$rating))
 })
 
 #plot lambdas
 qplot(lambdas, rmses)  
 
 # Assign optimal tuning parameter (lambda)
 lambda <- lambdas[which.min(rmses)]
 # Minimum RMSE achieved
 regularised_rmse <- min(rmses) 
 
 # Methods - mutate validation dataset to reflect changes to edx1 (year, date of review) and run final hold-out test

 
 # Use mutate function to update validation dataset in line with changes made to edx
 final_holdout_test1 <- final_holdout_test%>% mutate(ratingdate=as_date(as_datetime(timestamp)),
                                                    ratingyear=as.numeric(format(ratingdate,"%Y")),
                                                    ratingmonth=as.numeric(format(ratingdate,"%m")),
                                                    releaseyear = as.numeric(str_extract(str_extract(title, "[/(]\\d{4}[/)]$"), regex("\\d{4}")),title = str_remove(title, "[/(]\\d{4}[/)]$")),
                                                    nostalgialevel=ratingyear-releaseyear
                                                       )
 
 # Use full edx dataset to model all effects + regularised with chosen value for lambda
 b_i <- edx1 %>%
   group_by(movieId) %>%
   summarise(b_i = sum(rating - mu_hat)/(n()+lambda))
 
 b_u <- edx1 %>%
   left_join(b_i, by="movieId") %>%
   group_by(userId) %>%
   summarise(b_u = sum(rating - b_i - mu_hat)/(n()+lambda)) 

 b_y <- edx1 %>%
   left_join(b_i, by="movieId") %>%
   left_join(b_u, by="userId") %>%
   group_by(releaseyear) %>%
   summarise(b_y = sum(rating - b_i - b_u - mu_hat)/(n()+lambda))

 b_r <- edx1 %>%
   left_join(movie_avgs, by = "movieId") %>%
   left_join(user_avgs, by = "userId") %>%
   left_join(year_avgs, by = "releaseyear") %>%
   group_by(ratingyear) %>%
   summarise(b_r = sum(rating - b_i - b_u - b_y - mu_hat)/(n()+lambda))
 
 b_n <- edx1 %>%
   left_join(b_i, by="movieId") %>%
   left_join(b_u, by="userId") %>%
   left_join(b_y, by="releaseyear") %>%
   left_join(b_r, by="ratingyear") %>%
   group_by(nostalgialevel) %>%
   summarise(b_n = sum(rating - b_i - b_u - b_y - b_r - mu_hat)/(n()+lambda))

 b_g <- edx1 %>%
   left_join(b_i, by="movieId") %>%
   left_join(b_u, by="userId") %>%
   left_join(b_y, by="releaseyear") %>%
   left_join(b_r, by="ratingyear") %>%
   left_join(b_n, by="nostalgialevel") %>%
   group_by(genres) %>%
   summarise(b_g = sum(rating - b_i - b_u  - b_y - b_r - b_n - mu_hat)/(n()+lambda))

  predicted_ratings <- final_holdout_test1 %>%
   left_join(b_i, by="movieId") %>%
   left_join(b_u, by="userId") %>%
   left_join(b_y, by="releaseyear") %>%
   left_join(b_r, by="ratingyear") %>%
   left_join(b_n, by="nostalgialevel") %>%
   left_join(b_g, by="genres") %>%
   mutate(pred = mu_hat + b_i + b_u + b_y + b_r + b_n + b_g) %>%
   pull(pred)
 
 # Calculate final validation RMSE
 valid_rmse <- RMSE(final_holdout_test1$rating, predicted_ratings)
 valid_rmse
