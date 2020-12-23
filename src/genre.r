library(dplyr)
library(purrr)
library(ggplot2)

clean_name <- function(str){
  res <- unlist(strsplit(str, split='is', fixed=TRUE))[2]
  res
}

clean_names <- function(str_list){
  res <- map_chr(str_list,clean_name)
  res
}

create_genre_means <- function(data,name){
  res <- data %>% filter(data[name] == "true") %>% summarise(genre = name, meanOwners = mean(steamspyowners)) 
  res
}

create_genre_medians <- function(data,name){
  res <- data %>% filter(data[name] == "true") %>% summarise(genre = name, medianOwners = median(steamspyowners)) 
  res
}

data <- read.csv(file=file.path("..", "data","games_features.csv", fsep = .Platform$file.sep), sep=",", encoding="UTF-8")

data_f <- data %>% select(steamspyowners,starts_with("genreis"))
data_f <- data_f %>% select(-one_of(c("genreisearlyaccess","genreisfreetoplay","genreisnongame","genreisindie")))
data_f <- data_f %>% rename_with(clean_names,starts_with("genreis")) %>% rename(massmulti = massivelymultiplayer )
names <- colnames(data_f)[2:10]

dfs_means <- names %>% map(~ create_genre_means(data_f,.x))
res_means <- reduce(dfs_means,bind_rows)

dfs_medians <- names %>% map(~ create_genre_medians(data_f,.x))
res_medians <- reduce(dfs_medians,bind_rows)


ggplot(res_means, aes(genre,meanOwners,fill=genre)) + geom_col(color= "black")
ggplot(res_medians, aes(genre,medianOwners,fill=genre)) + geom_col(color= "black")


# TODO genre of top 10 most owned games 
