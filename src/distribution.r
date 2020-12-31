library(dplyr)
library(ggplot2)


get_owner_count_by_categories <- function(data,category_name,min,max){
  res <- data %>% filter(NbOwner > min & NbOwner <= max) 
  res_count <- res %>% summarise(owners_count = category_name, games_count = sum(Freq)) 
  res_count
}

data <- read.csv(file=file.path("..", "data","games_features.csv", fsep = .Platform$file.sep), sep=",", encoding="UTF-8")

# transform & clean dataframe 
data_df <- as.data.frame(table(data$steamspyowners))
colnames(data_df) <- c("NbOwner","Freq")
data_df$NbOwner <- as.numeric(levels(data_df$NbOwner))

data_df <- data_df %>% filter(NbOwner != 0) 

# compute owners count by category 

categories = c("<=1000","1000-10000","10000-100000","100000-100000",">1000000")

very_small <- get_owner_count_by_categories(data_df,"<=1000",0,1000)
small <- get_owner_count_by_categories(data_df,"1000-10000",1000,10000)
average <- get_owner_count_by_categories(data_df,"10000-100000",10000,100000)
large <- get_owner_count_by_categories(data_df,"100000-100000",100000,1000000)
very_large <- get_owner_count_by_categories(data_df,">1000000",1000000,.Machine$integer.max)

# aggregate counts 

res <- bind_rows(very_small,small,average,large,very_large)

res$owners_count <- factor(res$owners_count,levels = categories, ordered = TRUE)

# plot 

plot <- ggplot(res, aes(owners_count,games_count)) + geom_col(color= "black",fill="#FF9999")
plot + ggtitle("Distribution of the number of owners per game") + labs(x = "owners count", y="games count")