
  #Reading CSV file and Displaying Data Dimensions
  
#Loading libraries
library(tidyverse)
library(psych)
library(ggthemes)
library(ggplot2) #for data visualization
library(dplyr) #for data manipulation
library(tidyr) #for tidying the data
library(corrgram)
library(lubridate)
library(caret)
library(corrplot)

#Reading the CSV file into data frame
train_data <- read.csv('C:/Users/Asus/Downloads/bike-sharing-demand/train.csv', stringsAsFactors = FALSE)
test_data <- read.csv("C:/Users/Asus/Downloads/bike-sharing-demand/test.csv", stringsAsFactors =  FALSE)

## Getting Data Frame dimensions
#displaying shape, number of columns, rows and first few rows of the data set
head(train_data)
nrow(train_data)  # Number of rows, 10866
ncol(train_data)  # Number of columns, 12

#Basic information about Data Frame
str(train_data)


summary(train_data) #summary statistics for the data frame


describe(train_data)


str(train_data)




### The goal of normalization is to transform features to be on a similar scale. This improves the performance and training stability of the model.

#  Conducting Univariate Analysis 


ggplot(train_data, aes(temp, count)) + geom_point(alpha = 0.3) + theme_bw() +
  labs(title = "Number of Bikers by Temperature") +
  xlab("Temperature (Celcius)") +
  ylab("Number of Bikers")



ggplot(train_data, aes(humidity, count)) + geom_point(alpha = 0.3) + theme_bw() +
  labs(title = "Number of Bikers by Humidity") +
  xlab("Humidity (%)") +
  ylab("Number of Bikers")





##When do casual bikers increase the most?

ggplot(train_data, aes(casual, temp))+ geom_point() + theme_light() +
  labs(x = "Number of Casual Bike Users", 
       y = "Temperature (celcius)",
       title = "Casual Bikers by Temperature")



##Showing Correlation Heatmap among the Numeric Virables 

numeric_train_data <- train_data %>% select_if(is.numeric)
cor_matrix <- cor(numeric_train_data)

# Create the correlation plot
corrplot(cor_matrix, method = "color", type = "lower", tl.cex = 0.7)




#Create a joint scatter plot for 'temp' vs 'atemp'

# Create a box plot for 'cnt' by 'season'
ggplot(train_data, aes(x = season, y = count, fill = season, group = season)) +
  geom_boxplot() +
  labs(title = "Total # Bikes Rented By Season") +
  scale_fill_brewer(palette = "Set2") +
  xlab("Season") +
  ylab("Total # Bikes Rented")

###Feature Selection for modeling  

train_data$datetime <- as.POSIXct(train_data$datetime)


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





train_data <- extract_feature(train_data)

print(train_data)




train_data <- train_data %>% select(-datetime)


training_df <- train_data[, !colnames(train_data) %in% c("casual","registered")]


lm_model <- lm(count ~ ., data = training_df)

print(summary(lm_model))




test_data <- extract_feature(test_data)

datetime <- test_data$datetime



test_data <- test_data %>% select(-datetime)


par(mfrow = c(2,2))
plot(lm_model, which = c(1,2,3))
# In the residuals vs fitted plot, the variance of residuals increased as the values increased. So we tested a log transformation.

lm_model2 <- lm(log(count) ~ ., data = training_df)
summary(lm_model2)



par(mfrow = c(2,2))
plot(lm_model2, which = c(1,2,3))


## Model Selection
lm_model3 <- lm(log(count) ~ .-holiday -day, data = training_df) # no holiday and day
anova(lm_model3,lm_model2)



summary(lm_model3)
anova(lm_model3)


par(mfrow = c(2,2))
plot(lm_model3, which = c(1,2,3))


drops <- c("holiday","day")
test_data[ , !(names(test_data) %in% drops)]


predictions <- predict(lm_model3, newdata = test_data)



test_data$final_count = predictions

