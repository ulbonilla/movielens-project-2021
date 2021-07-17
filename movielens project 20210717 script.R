
###### setup #########

rm(list=ls())

# the path directory contains all working files for this project
path <- "//MyCloudEX2Ultra/HDulises/backup1/trabajo/IFT/cursos/EDX/harvard x/edx data science capstone/movielens/"
#path <- "C:/Users/User/OneDrive/Documentos"  # change if needed
setwd(path)

Sys.setlocale("LC_TIME", "English")

###### libraries ######

# libraries used in this project are authomatically installed
if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
library("lubridate")
if (!require("FactoMineR")) install.packages("FactoMineR")
library("FactoMineR")# for MCA
if (!require("factoextra")) install.packages("factoextra")
library("factoextra") # for MCA
if (!require("caret")) install.packages("caret")
library("caret")
if (!require("knitr")) install.packages("knitr")
library("knitr")
if (!require("rlang")) install.packages("rlang")
library("rlang")
if (!require("Hmisc") ) install.packages("Hmisc")
library("Hmisc") # for correlation table
if (!require("e1071") ) install.packages("e1071")
library("e1071") # for skewness and kurtossis functions

###### Introduction ######

edx_data <- read_csv(file = paste0(path, "edx_dataset.csv")) 

str(edx_data)

# table 1: original data set
kable(head(edx_data[-1]), align = "ccc")


###### Analysis ######

#graph 1: bar chart of ratings

edx_data$rating %>% 
  table %>% 
  as.data.frame() %>% 
  set_names(c("Rating", "Total")) %>% 
  mutate(Percentage = Total/sum(Total, na.rm = TRUE)*100,
         Percentage_format = format(round( Percentage,2), nsmall =2, big.mark = ","),
         Rating = factor(Rating)) %>%
  ggplot(aes(x= Rating, y = Percentage)) +
  geom_bar(aes(fill = Rating), stat="identity") +
  geom_text(aes(label=Percentage_format),  vjust = -.3, size=2.5) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Ratings") +
  ylab("Percentage (%)") +
  ggtitle(label = "Percentage of movie reviews by rating")

rm(edx_data, temp)

# feature engineering I: first 5 features

file <- str_c(path, "edx_data1.csv") # file exported in this chunk
# I wrote the code such that the sections that take the long are not run every time

if (!file.exists(file)) {
  
  edx_data <- read_csv(file = paste0(path, "edx_dataset.csv")) 
  
  # from userId and movieId (and rating) get movie_mean, user_mean and movie_user
  
  mean_total <- mean(edx_data$rating)
  
  saveRDS(mean_total, str_c(path,"mean_total.rds"))
  
  edx_data <- edx_data %>% 
              group_by(movieId) %>% 
              mutate(movie_mean = round(mean(rating - mean_total), digits = 4)) %>% # first feature
              ungroup() %>% 
              group_by(userId) %>% 
              mutate(user_mean = round(mean(rating - mean_total), digits = 4), # second feature
                     movie_user = movie_mean*user_mean) %>% # fifth feature
              ungroup()
  
  # from title and timestamp get year_dif_mean and year_mean
  
  edx_data <- edx_data %>% 
              mutate(year_m = str_extract(string = title, pattern = "\\([:digit:]{4}\\)"),
                     year_m = str_replace_all(string = year_m, pattern = "\\(", replacement = ""),
                     year_m = str_replace_all(string = year_m, pattern = "\\)", replacement = ""),
                     year_m = as.numeric(year_m),
                     timestamp = as_datetime(timestamp), 
                     year_dif = as.numeric(year(timestamp)) - year_m,
                     year_dif = ifelse(year_dif<0,0,year_dif)) %>%
              group_by(year_dif) %>% 
              mutate(year_dif_mean = round(mean(rating - mean_total), digits = 4)) %>% # third feature
              ungroup() %>% 
              group_by(year_m) %>% 
              mutate(year_mean = round(mean(rating - mean_total), digits = 4)) %>% # fourth feature
              ungroup()  
  
  write.csv(edx_data, file = str_c(path, "edx_data1.csv"))
  
  rm(edx_data)  # from now on, the data to be read is edx_data1
  
}

# feature engineering II: genre features

file <- str_c(path, "genres_df.rds") # file exported in this chunk

if (!file.exists(file)) {
  
  # the following function is used to collect all the different genres that appear in the data
  genres_fun <- function(data) {
    
    genres_df <-  data$genres %>% 
                  str_split(pattern = "\\|") %>% 
                  unlist %>% 
                  as.vector() %>% 
                  unique() %>% 
                  as.data.frame() %>% 
                  set_names(nm = "genre") %>% 
                  mutate(genre = str_replace_all(string = genre, 
                                                 pattern = "\\(", 
                                                 replacement = ""),
                         genre = str_replace_all(string = genre, 
                                                 pattern = "\\)", 
                                                 replacement = "")) %>% 
                  arrange(genre)
    
    return(genres_df)
    
  }
  
  saveRDS(genres_fun, "genres_fun.rds") # i will use again this function
  
  edx_data <- read_csv(file = paste0(path, "edx_data.csv")) # original data
  
  # genres_df is a list that contains all genres in the sample
  genres_df <- genres_fun(edx_data) 
  
  rm(edx_data, genres_fun)
  
  genres_list <-  genres_df$genre %>% 
                  as.vector()  %>% 
                  as.list() %>% 
                  saveRDS(str_c(path, "genres_list.rds"))
  
  # Once i have the list of existing genres i can create the categorical columns
  genres_df <-  genres_df %>% 
                mutate(genre = str_replace_all(genre, "-| ", ".")) %>% 
                saveRDS(str_c(path, "genres_df.rds"))
  
  rm(edx_data, genres_df, genres_list)
  
}

genres_df <- readRDS(str_c(path, "genres_df.rds"))

#  feature engineering IV: genre dummies

file <- str_c(path, "genres_data.csv") # file exported in this chunk

if (!file.exists(file)) {
  
  # the following function creates binary values for each gender identified in the data
  gendata_fun <- function(x,y) {# x is every single genre in the data
   
     temp <- str_detect(y$genres, x) %>% 
            as.data.frame() %>% 
            set_names(unlist(x)) %>% 
            mutate()
    
    temp[,1] <- temp[,1]*1 # from boolean to numeric
    
    # temp[,1] <- factor(temp[,1], levels = c("0", "1"))
    
    return(temp)
    
  }
  
  saveRDS(gendata_fun, str_c(path, "gendata_fun.rds"))
  
  genres_list <- readRDS(str_c(path, "genres_list.rds")) # from previous chunk
  
  edx_data <- read_csv(file = paste0(path, "edx_data.csv")) %>% # original data
              select(genres)
  
  ind <- map( genres_list,  gendata_fun, edx_data) # create dummies
  
  genres_data <- bind_cols(lapply(ind, as.data.frame.list))
  
  write.csv(genres_data, file = str_c(path, "genres_data.csv"))
  
  rm(edx_data, genres_data, genres_list, gendata_fun, ind)
  
}

# feature engineering V: genre effect feature

file <- str_c(path, "genres_data2.csv")

if (!file.exists(file)) {
  
  genres_df <- readRDS(str_c(path, "genres_df.rds")) %>% 
               unlist(use.names = FALSE)
  
  mean_total <- read_rds(str_c(path, "mean_total.rds"))
  
  genres_data <- read_csv(str_c(path, "genres_data.csv")) %>% 
                 select(-X1)
  
  ratings <- read_csv(file = paste0(path, "edx_data1.csv")) %>% 
             select( rating)
  
  # function to create the mean of each genre
  genre_means_fun <- function(genr, genres_data, ratings, mean_total) {
    
    temp <- sum(genres_data[genr]*(ratings-mean_total), na.rm = TRUE)/sum(genres_data[genr], na.rm = TRUE) %>% 
            unlist(use.names = FALSE)
    
    genres_data <- genres_data %>% 
                   select(!!genr) %>% 
                   mutate(!!genr := !!as.name(genr)*temp) 
    
  }
  
  genres_data <- map(genres_df, genre_means_fun, genres_data, ratings, mean_total) %>% 
                 bind_cols(lapply(genres_df0, as.data.frame.list)) %>% 
                 #select(-IMAX, -no.genres.listed) %>% 
                 summarise(genre_mean = rowSums(.)/rowSums(.!=0)) # feature
  
  write.csv(genres_data, file = str_c(path, "genres_data2.csv"))
  
  rm(ratings,genres_df, genres_data)
  
}

# table 2: correlation table 

temp <- read_csv( str_c(path, "edx_data1.csv")) %>% 
        select(rating, user_mean, movie_mean, year_dif_mean, year_mean, movie_user)

# table 2: correlation betwen numerical variables
table2 <- rcorr(as.matrix(temp))[[1]] %>% 
          as.data.frame()

knitr::kable(table2, 
             align = "ccc")

# table 3: statistics table

getmode_fun <- function(v) {
              uniqv <- unique(v)
              uniqv[which.max(tabulate(match(v, uniqv)))]
}

mysummary_fun <- function(vector, na.rm = FALSE, round = 2){
                  results <- c(summary(vector), 
                               'Skewness' = skewness(vector, na.rm = FALSE, type = 3),
                               'Kurtosis' = kurtosis(vector, na.rm = FALSE, type = 3),
                               'Std. Dev' = sd(vector, na.rm))
                  return(results)
}

table3 <- do.call(cbind, lapply(temp, mysummary_fun)) %>% 
          as.data.frame() %>% 
          tibble::rownames_to_column("Statistic") %>% 
          mutate_if(is.numeric, round, digits=3) %>% 
          select(-movie_user)

knitr::kable(table3, 
             align = "ccc")

# graph 2: density chart

temp %>% 
  ggplot() + 
  geom_density(aes(x=user_mean, color= "firebrick")) +
  geom_density(aes(x=movie_mean, color = "deepskyblue1")) +
  scale_color_manual(name = "Features:", 
                     values = c("firebrick" = "firebrick", 
                                "deepskyblue1" = "deepskyblue1"),
                     labels = c('user_mean','movie_mean')) +
  theme_bw() +
  xlab("Deviation from the mean") +
  ylab("Density") +
  ggtitle(label = "Densities of user_mean and movie_mean") 

# graph 3: density chart

temp %>% 
  ggplot() + 
  geom_density(aes(x=year_mean, color = "goldenrod2")) + 
  geom_density(aes(x=year_dif_mean, color = "darkgreen")) +
  scale_color_manual(name = "Features:", 
                     values = c("goldenrod2" = "goldenrod2", 
                                "darkgreen" = "darkgreen"),
                     labels = c('year_mean','year_dif_mean')) +
  theme_bw() +
  xlab("Deviation from the mean") +
  ylab("Density") +
  ggtitle(label = "Densities of year_mean and year_dif_mean")

# graph 4: genre´s bar chart

temp <- read_csv(str_c(path, "genres_data.csv")) %>%   
        select(-X1) %>% 
        map_dbl(sum) %>% 
        as.data.frame() %>% 
        set_names(c("Total")) %>% 
        tibble::rownames_to_column("Genre") %>% 
        mutate(Total = Total/10^3,
               Total_format = format(round( Total,0), nsmall =0, big.mark = ",")) %>%
        arrange(Total)

temp  %>%
  filter(Genre != "no.genres.listed") %>% 
  mutate(Genre = factor(Genre, levels = unique(temp$Genre))) %>% 
  ggplot(aes(x= Genre, y = Total)) +
  geom_bar(aes(fill = Genre), stat="identity") +
  geom_text(aes(label=Total_format),  hjust = -.01, size = 2.5) +
  theme_bw() +
  theme(legend.position = "none") +
  xlab("Genres") +
  ylab("Number of reviews (thousands)") +
  ggtitle(label = "Number of movie reviews by genre") + 
  coord_flip()

rm(temp)

###### models ######

# linear model I caret

file <- str_c(path, "rmse_caret.rds") # file exported in this chunk

if (!file.exists(file)) {
  
  edx_data <- read_csv(file = paste0(path, "edx_data1.csv")) %>% 
              select(rating, user_mean, movie_mean, year_dif_mean, year_mean, movie_user) 
  
  model_LM <- caret::train(rating ~ ., 
                           data = edx_data,
                           method = "lm",
                           trControl = trainControl( method = "cv", 
                                                     number = 5,
                                                     verboseIter = TRUE))
  
  saveRDS(model_LM, "model_LM1.rds")
  
  rmse_caret <- model_LM$results[[2]]
  
  saveRDS(rmse_caret, str_c(path, "rmse_caret.rds"))
  
  rm(edx_data, model_LM)
  
}

rmse_caret <- readRDS(str_c(path, "rmse_caret.rds"))

# linear model I: 5 features

file <- str_c(path, "model_LM2.rds") # file exported in this chunk

if (!file.exists(file)) {
  
  edx_data <- read_csv(file = paste0(path, "edx_data1.csv")) %>% 
              select(rating, user_mean, movie_mean, year_dif_mean, year_mean, movie_user)
  
  model_LM <- lm(rating ~ ., data = edx_data)
  
  saveRDS(model_LM, str_c(path, "model_LM2.rds") )
  
  rm(edx_data, model_LM)
  
}

# linear model II: 25 features


file <- str_c(path, "model_LM3.rds") # file exported in this chunk

if (!file.exists(file)) {
  
  edx_data <- read_csv(file = paste0(path, "edx_data1.csv")) %>% # n_max
              select(rating, user_mean, movie_mean, year_dif_mean, year_mean, movie_user) %>% 
              bind_cols(read_csv(file = paste0(path, "genres_data.csv"))) %>% 
              select(-X1)
  
  model_LM <- lm(rating ~ ., data = edx_data) 
  
  saveRDS(model_LM, str_c(path, "model_LM3.rds"))
  
  rm(edx_data, model_LM)
  
}

# linear model III caret

file <- str_c(path, "model_LM4.rds") # file exported in this chunk

if (!file.exists(file)) {
  
  edx_data <- read_csv(file = paste0(path, "edx_data1.csv")) %>% # n_max
              select(rating, user_mean, movie_mean, year_dif_mean, year_mean, movie_user) %>% 
              bind_cols(read_csv(file = paste0(path, "genres_data2.csv"))) %>% 
              select(-X1)
  
  model_LM <- caret::train(rating ~ ., 
                           data = edx_data,
                           method = "lm",
                           trControl = trainControl( method = "cv", 
                                                     number = 5,
                                                     verboseIter = TRUE))
  
  saveRDS(model_LM, "model_LM4.rds")
  
  rmse_caret <- model_LM$results[[2]]
  
  saveRDS(rmse_caret, str_c(path, "rmse_caret2.rds"))
  
  rm(edx_data, model_LM)
  
}

rmse_caret <- readRDS(str_c(path, "rmse_caret2.rds"))

# linear model III: 6 features

file <- str_c(path, "model_LM5.rds") # file exported in this chunk

if (!file.exists(file)) {
  
  edx_data <- read_csv(file = paste0(path, "edx_data1.csv")) %>% # n_max
              select(rating, user_mean, movie_mean, year_dif_mean, year_mean, movie_user) %>% 
              bind_cols(read_csv(file = paste0(path, "genres_data2.csv"))) %>% 
              select(-X1)
  
  model_LM <- lm(rating ~ ., data = edx_data) 
  
  saveRDS(model_LM, str_c(path, "model_LM5.rds"))
  
  rm(edx_data, model_LM)
  
}

######### Results ##########

# preparing validation data I

file <- paste0(path, "val_data2.csv") # file exported in this chunk

if (!file.exists(file)) {
  
  # first, apply the initial transformations of features
  val_data <- read_csv(file = paste0(path, "validation.csv"))  %>% 
    mutate(year_m = str_extract(string = title, pattern = "\\([:digit:]{4}\\)"),
           year_m = str_replace_all(string = year_m, pattern = "\\(", replacement = ""),
           year_m = str_replace_all(string = year_m, pattern = "\\)", replacement = ""),
           year_m = as.numeric(year_m),
           timestamp = as_datetime(timestamp), 
           year_dif = as.numeric(year(timestamp)) - year_m,
           year_dif = ifelse(year_dif<0,0,year_dif))
  
  # recovering the features is done by matching vectors old vs new:
  old <- c("userId", "movieId", "year_m", "year_dif")
  
  new <- c("user_mean", "movie_mean", "year_mean", "year_dif_mean")
  
  for (i in 1:4) {
    
    edx_data <- read_csv(file = paste0(path, "edx_data1.csv")) %>% 
                select( old[i],new[i]) %>% 
                mutate(!!new[i] := round(!!sym(new[i]), digits = 4)) %>% 
                unique()
    
    val_data <- val_data %>% 
      left_join(edx_data, by = old[i])
    
  }
  
  val_data <- val_data %>% 
              select(rating, user_mean, movie_mean, year_dif_mean, year_mean) %>%
              mutate(movie_user = movie_mean*user_mean)
  
  write.csv(x =val_data, file = paste0(path, "val_data2.csv"))
  
  rm(edx_data, val_data)
  
}

rmse_caret <- read_rds(str_c(path, "rmse_caret.rds"))

# preparing validation data II

# genre column in validation is also split

file <- paste0(path, "val_data3.csv") # file exported in this chunk

if (!file.exists(file)) {
  
  genres_list <- read_rds(str_c(path, "genres_list.rds"))
  gendata_fun <- read_rds(str_c(path, "gendata_fun.rds"))
  
  val_data <- read_csv(file = paste0(path, "validation.csv")) 
  
  ind <- map( genres_list,  gendata_fun, val_data) # create dummies
  
  genres_data <- bind_cols(lapply(ind, as.data.frame.list))
  
  val_data <- read_csv(file = paste0(path, "val_data2.csv"))%>% 
              select(rating, user_mean, movie_mean, year_dif_mean, year_mean, movie_user) %>% 
              bind_cols(genres_data)
  
  write.csv(x =val_data, file = paste0(path, "val_data3.csv"))
  
  rm(genres_data, val_data, ind, genres_list, gendata_fun)
  
}

# preparing validation data III

# for the last model I use only 6 features

file <- paste0(path, "val_data4.csv") # file exported in this chunk

if (!file.exists(file)) {
  
  edx_data <- read_csv(file = paste0(path, "edx_data1.csv")) %>% 
              select( movieId) %>% 
              bind_cols(read_csv(file = paste0(path, "genres_data2.csv"))) %>% 
              select(-X1) %>% 
              unique()   
  
  val_data <- read_csv(file = paste0(path, "val_data2.csv"))%>% 
              select(rating, user_mean, movie_mean, year_dif_mean, year_mean, movie_user) %>% 
              bind_cols(select(read_csv(file = paste0(path, "validation.csv")), movieId)) %>% 
              left_join(edx_data, by = "movieId") %>% 
              select(-movieId)
  
  write.csv(x =val_data, file = paste0(path, "val_data4.csv"))
  
  rm(edx_data, val_data)
  
}

# calculating the RMSE for each model:

# RMSE I

val_data <- read_csv(file = paste0(path, "val_data2.csv")) %>% 
            select(rating, user_mean, movie_mean, year_dif_mean, year_mean, movie_user) 

model <- readRDS(file = paste0(path, "model_LM2.rds"))

RMSE <- RMSE( predict(model, val_data), val_data$rating)

# RMSE II
val_data <- read_csv(file = paste0(path, "val_data3.csv")) %>% 
            select(-X1) 

model <- readRDS(file = paste0(path, "model_LM3.rds"))

RMSE <- c(RMSE, RMSE( predict(model, val_data), val_data$rating))


# RMSE III
val_data <- read_csv(file = paste0(path, "val_data4.csv")) %>% 
            select(-X1) 

model <- readRDS(file = paste0(path, "model_LM5.rds"))


RMSE <- c(RMSE, RMSE( predict(model, val_data), val_data$rating))


rm(model)

# table 4: the ultimate results
Models <- c("Linear model 1", "Linear model 2", "Linear model 3")

number <- c(5, 25, 6)

table4 <- data.frame(Models, RMSE, number)

# this chunk is always evaluated
kable(table4, 
      align = "ccc", 
      col.names = c("", 
                    "RMSE", 
                    "Number of variables"))






