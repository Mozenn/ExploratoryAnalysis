library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)

data <- read.csv(file=file.path("..", "data","games_features.csv", fsep = .Platform$file.sep), sep=",", encoding="UTF-8")


owner_distribution <- table(data$steamspyowners)
owner_distribution_df <- as.data.frame(table(data$steamspyowners))
colnames(owner_distribution_df) <- c("NbOwner","Freq")
owner_distribution_df$NbOwner <- as.numeric(levels(owner_distribution_df$NbOwner))
owner_distribution_df <- owner_distribution_df %>% filter(NbOwner != 0) 

res <- owner_distribution_df %>% filter(NbOwner < 10000) 
      
res_f <- res %>% summarise(sum = sum(Freq))