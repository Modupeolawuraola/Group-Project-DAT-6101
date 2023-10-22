df_train <- read.csv("C:/Users/Asus/Downloads/bike-sharing-demand/train.csv", stringsAsFactors = FALSE)
head(df_train)

df_test <- read.csv("C:/Users/Asus/Downloads/bike-sharing-demand/test.csv", stringsAsFactors = FALSE)
head(df_test)


library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)

library(corrplot)
# library(caTools)
library(lubridate)
library(caret)






num.cols <- sapply(df_train, is.numeric)

ggplot(df_train, aes(temp, count)) + geom_point(alpha = 0.3, aes(color = temp)) + theme_bw()




ggplot(df_train, aes(x = humidity)) + geom_histogram(bins=20,alpha=0.5,fill='blue')


ggplot(df_train, aes(x = registered)) + geom_histogram(bins=20,alpha=0.5,fill='red')


ggplot(df_train, aes(x = casual)) + geom_histogram(bins=20,alpha=0.5,fill='red')



df_corr <- cor(df_train)

# Create the correlation plot
corrplot(df_corr, method = "color", type = "lower", tl.cex = 0.7)

# Add correlation values as text
text_col <- colorRampPalette(c("blue", "white", "red"))(100)
corrplot(df_corr, type = "upper", tl.col = "black", tl.srt = 45, col = text_col)

# Add a title to the plot
title("Correlation Heatmap", line = 2)



#setting Seed


# 
# sample <- sample.split(df$count, SplitRatio = 0.7)
# 
# train <- subset(df, sample == TRUE)
# 
# test <- subset(df, sample == FALSE)


# na_count <- sum(is.na(df_train))
# 
# if (na_count > 0){
#   df <- na.omit(df_train)
# }

df_train$datetime <- as.POSIXct(df_train$datetime)


extract_feature <- function(df) {
  df <- df %>%
    mutate(
      year = year(datetime),
      day = day(datetime),
      month = month(datetime),
      hour = hour(datetime),
      dayofweek = wday(datetime) - 1  
    )
  
  return(df)
}



df_train <- extract_feature(df_train)

print(df_train)

#Filtering for 2011
df_2011 <- df_train %>% filter(year(datetime) == 2011)


daily_counts <- df_2011 %>%
  group_by(date = as.Date(datetime)) %>%
  summarise(count = sum(count))


ggplot(data = daily_counts, aes(x = date, y = count)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "Date", y = "Rental Count", title = "Daily Rentals for 2011") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme_minimal()


df_2012 <- df_train %>% filter(year(datetime) == 2012)


ggplot() +
  geom_line(data = df_2011, aes(x = datetime, y = count, color = "2011"), size = 1) +
  geom_line(data = df_2012, aes(x = datetime, y = count, color = "2012"), size = 1) +
  scale_color_manual(values = c("2011" = "navy", "2012" = "brown")) +
  labs(x = "Date", y = "Rental Count", title = "2011 and 2012 Rentals") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_datetime(date_labels = "%Y-%m-%d", date_breaks = "1 month") +
  guides(color = guide_legend(title = "Year"))


df_train <- df_train %>% select(-datetime)


training_df <- df_train[, !colnames(df_train) %in% "count"]

label <- df_train$count


# install.packages("caret")



set.seed(101)



index <- createDataPartition(y = label, p = 0.2, list = FALSE)

X_train <- training_df[index, ]
X_val <- training_df[-index, ]
y_train <- label[index]
y_val <- label[-index]




lm_model <- lm(y_train ~ ., data = X_train)

print(summary(lm_model))


predictions <- predict(lm_model, newdata = X_val)

rmse <- sqrt(mean((predictions - y_val)^2))

cat("Root Mean Squared Error (RMSE):", rmse, "\n")


results <- cbind(predictions, y_val)






df_test$datetime <- as.POSIXct(df_test$datetime)









df_test <- extract_feature(df_test)

datetime <- df_test$datetime



df_test <- df_test %>% select(-datetime)

casual <- df_train$casual
registered <- df_train$registered



df_test$casual <- casual
df_test$registered <- registered

df_test <- df_test %>% select(season, holiday, workingday, weather, temp, atemp, humidity, windspeed, casual, registered, year, day, month, hour, dayofweek)







predictions_test <- predict(lm_model, newdata = df_test)




