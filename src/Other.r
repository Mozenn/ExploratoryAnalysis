library(dplyr)
library(tidyr)
library(ggplot2)

extract_year <- function(date){
  res <- as.numeric(unlist(strsplit(date, split=' ', fixed=TRUE))[3])
  res
}

data <- read.csv(file=file.path("..", "data","games_features.csv", fsep = .Platform$file.sep), sep=",", encoding="UTF-8")

data_f <- data %>% filter(genreisnongame == "false" & steamspyowners != 0)
data_f <- data_f %>% select(releasedate,steamspyowners)

data_f <- data_f %>% rowwise() %>% mutate(releaseyear = extract_year(releasedate)) %>% drop_na() %>% select(-releasedate)

res <- data_f %>% group_by(releaseyear) %>% summarise(meanOwners = mean(steamspyowners))

ggplot(res, aes(releaseyear,meanOwners)) + geom_line(color="#2C32AA", size=1) + geom_point(color="#2C32AA",size=2)