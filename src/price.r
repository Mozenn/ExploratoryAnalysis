library(dplyr)
library(purrr)
library(ggplot2)

data <- read.csv(file=file.path("..", "data","games_features.csv", fsep = .Platform$file.sep), sep=",", encoding="UTF-8")
data <- data %>% filter(genreisnongame == "false")

data_f <- data %>% select(steamspyowners,pricefinal)
colnames(data_f) <- c("nbOwner","price")
data_f <- data_f %>% filter(nbOwner != 0) 
data_f <- data_f %>% filter(nbOwner < 4000000)

ggplot(data_f, aes(nbOwner,price)) + geom_point(aes(color= price)) 
# TODO improve title, axis name, ... 

data_free <- data %>% filter(genreisfreetoplay == "true" & categoryinapppurchase == "true" & steamspyowners != 0)
free_medians <- data_free %>% summarise(type = "freetoplay", medianOwners = median(steamspyowners))
free_means <- data_free %>% summarise(type = "freetoplay", meanOwners = mean(steamspyowners))

data_premium <- data %>% filter(genreisfreetoplay == "false" & steamspyowners != 0)
premium_medians <- data_premium %>% summarise(type = "premium", medianOwners = median(steamspyowners))
premium_means <- data_premium %>% summarise(type = "premium", meanOwners = mean(steamspyowners))

res_mean <- bind_rows(free_means,premium_means)
res_median <- bind_rows(free_medians,premium_medians)

ggplot(res_mean, aes(type,meanOwners,fill=type)) + geom_col(color= "black")
ggplot(res_median, aes(type,medianOwners,fill=type)) + geom_col(color= "black")
# TODO improve title, axis name, ... 