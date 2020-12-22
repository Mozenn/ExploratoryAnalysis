library(dplyr)
library(ggplot2)

data <- read.csv(file=file.path("..", "data","games_features.csv", fsep = .Platform$file.sep), sep=",", encoding="UTF-8")

data_f <- data %>% select(steamspyowners,metacritic)
colnames(data_f) <- c("nbOwner","rating")
data_f <- data_f %>% filter(nbOwner != 0 & rating != 0) 
data_alt <- data_f %>% filter(nbOwner < 4000000)

ggplot(data_alt, aes(nbOwner,rating)) + geom_point(aes(color= rating)) + scale_colour_gradientn(colours=c("red","orange","green"))

