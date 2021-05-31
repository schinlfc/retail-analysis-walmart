### Required R libraries
load_libraries <- function(){
  library(openair)
  library(caret)
  library(ggplot2)
  library(lubridate)
  library(dplyr)
  library(tidyverse)
  library(tidyselect)
  library(vctrs)
  library(scales)
  library(zoo)
  library(forecast)
  library(Hmisc)
  library(corrplot)
  library(PerformanceAnalytics)
  library(ggfortify)
  library(car)
  library(patchwork)
  library(knitr)
  print("The libraries have been loaded .")
}
load_libraries()

### 1. Data preprocessing
# ++++++++++++++++++++++++++++
# DSet working directory
# ++++++++++++++++++++++++++++
setwd("/Users/schinlfc/data-science-R/retail-analysis-walmart/data")

# ++++++++++++++++++++++++++++
# Read csv file and assigned as df
# ++++++++++++++++++++++++++++
df <- read.csv("Walmart_Store_sales.csv")
# Get a summary of the data
summary(df)
# Check for missing values
sum(!complete.cases(df))
# Rename columns
df <- 
  df %>% 
  rename(
    store = Store,
    date = Date,
    weekly_sales = Weekly_Sales,
    holiday_flag = Holiday_Flag,
    temperature = Temperature,
    fuel_price = Fuel_Price,
    cpi = CPI,
    unemployment = Unemployment)
# Convert date to d-m-Y
df$date=as.Date(df$date,format="%d-%m-%Y")

### 2. Basic statistical results
## Which store has maximum sales?
df %>% 
  group_by(store) %>% summarise(weekly_sales =
  max(weekly_sales)) %>% arrange(desc(weekly_sales)) %>% head(1)

## Which store has maximum standard deviation?
df %>% 
  group_by(store) %>% summarise(weekly_sales = sd(weekly_sales)) %>%
  arrange(desc(weekly_sales)) %>% head(1)

## Which store/s has good quarterly growth rate in the third quarter of 2012
# Defining the start and end date of Q3
Q3 <- 
  selectByDate( 
    df,
    start = "2012-07-01",
    end = "2012-09-30")
# Defining the start and end date of Q3
Q2 <- 
  selectByDate( 
    df,
    start = "2012-04-01",
    end = "2012-06-30")
# Finding the total weekly sales of each store in Q3
Q3_weekly_sale <- 
  data.frame(Q3 %>% group_by(store) %>% 
  summarise(Q3_weekly_sales = sum(weekly_sales)))
# Finding the total weekly sales of each store in Q2
Q2_weekly_sale <- 
  data.frame(Q2 %>% group_by(store) %>% 
  summarise(Q2_weekly_sale = sum(weekly_sales)))
# Merging Q3 and Q2 data frame on store as a common column
Q3_Q2_merged <- 
  merge(Q3_weekly_sale,Q2_weekly_sale, by="store")
# Calculating growth rate in the third quarter of 2012
Q3_Q2_merged$growth_rate <-
  (((Q3_Q2_merged$Q3_weekly_sales)-(Q2_weekly_sale$Q2_weekly_sale))
   / (Q2_weekly_sale$Q2_weekly_sale)) * 100
# Round the growth rate to two decimal places
Q3_Q2_merged$growth_rate <- 
  round(Q3_Q2_merged$growth_rate, digits = 2)
# Get stores that have good quarterly growth rate in the third quarter of 2012
Q3_Q2_merged %>% 
  group_by(store) %>% arrange(desc(growth_rate)) %>% head(20)

## Some holidays have a negative impact on sales. Find out holidays which have 
# higher sales than the mean sales in non-holiday season for all stores together
# Weekly sales between nonholidays and holidays
df %>% group_by(holiday_flag) %>%
  summarise(mean_sales_of_nonholiday_and_holiday = 
  mean(weekly_sales))
# Create dummy variable for the dates of Super Bowl
df$super_bowl <- 
  ifelse(df$date == "2010-02-12" | df$date == "2011-02-11" | 
  df$date == "2012-02-10" | df$date == "2013-02-08", 1, 0)
# Create dummy variable for the dates of Labour Day
df$labour_day <- 
  ifelse(df$date == "2010-09-10" | df$date == "2011-09-09" | 
  df$date == "2012-09-07" | df$date == "2013-09-06", 1, 0)
# Create dummy variable for the dates of Thanksgiving
df$thanksgiving <- 
  ifelse(df$date == "2010-11-26" | df$date == "2011-11-25" | 
  df$date == "2012-11-23" | df$date == "2013-11-29", 1, 0)
# Create dummy variable for the dates of Christmas
df$christmas <- 
  ifelse(df$date == "2010-12-31" | df$date == "2011-12-30" | 
  df$date == "2012-12-28" | df$date == "2013-12-27", 1, 0)
# Mean weekly sales during Super Bowl
df %>% 
  group_by(super_bowl) %>% summarise(super_bowl_mean_sales = 
  mean(weekly_sales)) 
# Mean weekly sales during Labour Day
df %>% 
  group_by(labour_day) %>% summarise(labour_day_mean_sales = 
  mean(weekly_sales)) 
# Mean weekly sales during Thanksgiving
df %>% 
  group_by(thanksgiving) %>% summarise(thanksgiving_mean_sales = 
  mean(weekly_sales)) 
# Mean weekly sales during Christmas
df %>% 
  group_by(christmas) %>% summarise(christmas_mean_sales = 
  mean(weekly_sales)) 

### Provide a monthly and semester view of sales in units and give insights
# Split date into year, month and day.
df <- 
  df %>%
  separate(date, sep="-", into = c("year", "month", "day"))

# Plotting weekly sale distribution by month
ggplot(df, aes(x = month, y = weekly_sales/10000)) + # Scale by 10000
  geom_col() +
  geom_col(fill = "#00abff") +
  theme(text = element_text(size = 10, hjust = 0.5)) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~year) +
  ggtitle("Weekly sales distribution by month for the years of 2010-2012") +
  xlab("Month") +
  ylab("Weekly Sales (in 10000)")

# Create a date column
df <-
  df %>%
  mutate(date = make_date(year, month, day))

# Plotting weekly sale distribution by semester
ggplot(df, aes(x = semester(date, with_year = TRUE), y = weekly_sales/100000)) + 
  geom_col() +
  geom_col(fill = "#00abff") +
  theme(text = element_text(size = 10, hjust = 0.5)) +
  theme_update(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Weekly sales distribution by semester") +
  xlab("Semester") +
  ylab("Weekly Sales (in 100000)")

### 3. Data visualisation and statistical models
# First, I need to subset the dataset to only include Store #1 information 
# by the following code:
df1 <- df[df$store == 1, ]
# Create week from date
df1$week <- lubridate::week(ymd(df1$date))
# Second, I need to split into train/test dataset and set seed for 
# reproducible results
set.seed(123) 
# Test side of 25%
sample_size <- floor(0.75 * nrow(df1))
train_index <- sample(seq_len(nrow(df1)), size = sample_size)
train <- df1[train_index, ]
test <- df1[-train_index, ]

#### 3.1 Data visualisation
ggplot(train, aes(x=temperature)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Histogram plot of temprature") +
  xlab("Temperature") +
  ylab("Density")

ggplot(train, aes(x=fuel_price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Histogram plot of fuel price") +
  xlab("Fuel Price") +
  ylab("Density")

ggplot(train, aes(x=cpi)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Histogram plot of consumer price index (CPI)") +
  xlab("CPI") +
  ylab("Density")

ggplot(train, aes(x=unemployment)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Histogram plot of unemployment rate") +
  xlab("Unemployment (%)") +
  ylab("Density")

ggplot(train, aes(x=weekly_sales/1000)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Histogram plot of weekly sales") +
  xlab("Weekly Sales (in 1000)") +
  ylab("Density")

ggplot(train, aes(x=log(weekly_sales))) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") +
  ggtitle("Histogram plot of log weekly_sales") +
  xlab("Log Weekly Sales") +
  ylab("Density")

ggplot(train, aes(x=temperature, y=weekly_sales/1000)) +
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
  color="darkred", fill="blue") +
  labs(title = "Scatter plot of temperature vs. weekly sales") +
  ylab("Weekly Sales (in 1000)") +
  xlab("Temperature")

ggplot(train, aes(x=temperature, y=log(weekly_sales))) +
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
  color="darkred", fill="blue") +
  labs(title = "Scatter plot of temperature vs. log weekly sales") +
  ylab("Log Weekly Sales") +
  xlab("Temperature")
ggplot(train, aes(x=unemployment, y=log(weekly_sales))) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
  color="darkred", fill="blue") +
  labs(title = "Scatter plot of unemployment rate vs. log weekly 
  wales") +
  ylab("Log Weekly Sales") +
  xlab("Unemployment (%)")

ggplot(train, aes(x=cpi, y=weekly_sales/1000)) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
  color="darkred", fill="blue") +
  labs(title = "Scatter plot of CPI vs. weekly sales") +
  ylab("Weekly Sales (in 1000)") +
  xlab("CPI")

ggplot(train, aes(x=cpi, y=log(weekly_sales))) + 
  geom_point(shape=18, color="blue")+
  geom_smooth(method=lm,  linetype="dashed",
  color="darkred", fill="blue") +
  labs(title = "Scatter plot of CPI vs. log weekly sales") +
  ylab("Log Weekly Sales") +
  xlab("CPI")

#### 3.2 Linear regression
##### 3.2.1 Model I
lm_fit1 <- 
  lm(weekly_sales ~ temperature + cpi + thanksgiving + 
  fuel_price + unemployment + super_bowl, data = train)
# Show results of the fitted regression
summary(lm_fit1) 

par(mfrow = c(2,2))
# Diagnostic analysis
autoplot(lm_fit1)

##### 3.2.2 Model II 
lm_fit2 <- lm(weekly_sales ~ cpi + thanksgiving, data = train)
# Show results of the fitted regression
summary(lm_fit2) 
# Diagnostic analysis
par(mfrow = c(2,2))
autoplot(lm_fit2) 

##### 3.2.3 Model III
lm_fit3 <- lm(log(weekly_sales) ~ temperature + cpi + 
  thanksgiving + fuel_price + unemployment + 
  super_bowl,data = train)
# Show results of the fitted regression
summary(lm_fit3)
# Diagnostic analysis
par(mfrow = c(2,2))
autoplot(lm_fit3)

##### 3.2.4 Model IV
lm_fit4 <- lm(log(weekly_sales) ~ cpi + thanksgiving, data = train)
# Show results of the fitted regression
summary(lm_fit4)
# Diagnostic analysis
par(mfrow = c(2,2))
autoplot(lm_fit4)

### 3. Model evaluation
#### 3.1 Evaluation of Model I
#Fit the test dataset to the training Model I
lm_fit_pred1 <- predict(lm_fit1, newdata = test, type = "response")

##### 3.1.1 Model I correlation accuracy
# Actuals and predicteds 
act_pred1 <- data.frame(cbind(actuals=test$weekly_sales,
  predicteds=lm_fit_pred1))
# Correlation accuracy
cor(act_pred1) 

# Here an overview of the first 10 rows of the new dataframe composed by actual 
# values and predicted values:
head(act_pred1, n=10)

##### 3.1.2 Model I min-max accuracy
min_max1 <- 
  mean(apply(act_pred1, 1, min) / apply(act_pred1, 1, max))
# Show the result
print(min_max1) 

##### 3.1.3 Model I mean absolute percentage deviation
mape1 <- 
  mean(abs((act_pred1$predicteds -
  act_pred1$actuals))/act_pred1$actuals)
# show the result
print(mape1) 

#### 3.1 Evaluation of Model I
# Fit the test dataset to the training Model I
lm_fit_pred1 <- predict(lm_fit1, newdata = test, type = "response")

##### 3.1.1 Model I correlation accuracy
# Actuals and predicteds 
act_pred1 <- data.frame(cbind(actuals=test$weekly_sales,
  predicteds=lm_fit_pred1))
# Correlation accuracy
cor(act_pred1) 

# Here an overview of the first 10 rows of the new dataframe composed by actual 
# values and predicted values:
head(act_pred1, n=10)

##### 3.1.2 Model I min-max accuracy
min_max1 <- 
  mean(apply(act_pred1, 1, min) / apply(act_pred1, 1, max))
# Show the result
print(min_max1) 

##### 3.1.3 Model I mean absolute percentage deviation
mape1 <- 
  mean(abs((act_pred1$predicteds -
  act_pred1$actuals))/act_pred1$actuals)
# show the result
print(mape1) 

#### 3.2 Evaluation of Model II
# Fit the test dataset to the training Model II
lm_fit_pred2 <- predict(lm_fit2, newdata = test, type = "response")

##### 3.2.1 Model II correlation accuracy
# Actuals and predicteds 
act_pred2 <- data.frame(cbind(actuals=test$weekly_sales, 
  predicteds=lm_fit_pred2)) 
# Correlation accuracy
cor(act_pred2) 

# Here an overview of the first 10 rows of the new dataframe composed by actual
# values and predicted values:
# Actuals and predicteds
head(act_pred2, n=10)

##### 3.2.2 Model II min-max accuracy
min_max2 <- 
  mean(apply(act_pred2, 1, min) / apply(act_pred2, 1, max))
# Show the result
print(min_max2) 

##### 3.2.3 Model II mean absolute percentage deviation
mape2 <- 
  mean(abs((act_pred2$predicteds
  -act_pred2$actuals))/act_pred2$actuals)
# Show the result
print(mape2) 

#### 3.3 Evaluation of Model III
# Fit the test dataset to the training Model III
lm_fit_pred3 <- predict(lm_fit3, newdata = test, type = "response")

##### 3.3.1 Model III correlation accuracy
# Actuals and predicteds 
act_pred3 <- 
  data.frame(cbind(actuals=test$weekly_sales,
  predicteds=exp(lm_fit_pred3))) 
# Correlation accuracy
cor(act_pred3) 

# Here an overview of the first 10 rows of the new dataframe composed by actual 
# values and predicted values:
head(act_pred3, n=10)

##### 3.3.2 Model III min-max accuracy
min_max3 <- 
  mean(apply(act_pred3, 1, min) / apply(act_pred3, 1, max))
# Show the result
print(min_max3) 

##### 3.3.3 Model III mean absolute percentage deviation
# Mean absolute percentage deviation is so defined:
mape3 <- 
  mean(abs((act_pred3$predicteds
  -act_pred3$actuals))/act_pred3$actuals)
# Show the result
print(mape3)

#### 3.4 Evaluation of Model IV
# Fit the test dataset to the training Model IV
lm_fit_pred4 <- predict(lm_fit4, newdata = test, type = "response")

##### 3.4.1 Model IV correlation accuracy
# Actuals and predicteds 
act_pred4 <- 
  data.frame(cbind(actuals=test$weekly_sales,
  predicteds=exp(lm_fit_pred4))) 
# Correlation accuracy
cor(act_pred4) 

# Here an overview of the first 10 rows of the new dataframe composed by actual 
# values and predicted values:
head(act_pred4, n=10)

##### 3.4.2 Model IV min-max accuracy
min_max4 <- 
  mean(apply(act_pred4, 1, min) / apply(act_pred4, 1, max))
# Show the result
print(min_max4) 

##### 3.4.3 Model IV mean absolute percentage deviation
mape4 <- 
  mean(abs((act_pred4$predicteds
  -act_pred4$actuals))/act_pred4$actuals)
# show the result
print(mape4) 

##### Plot actuals and predicteds using test data set
# Create week column for each actuals and predicteds data frame
act_pred1$week <- test$week
act_pred2$week <- test$week
act_pred3$week <- test$week
act_pred4$week <- test$week

# Creat color themems
Color1 <- "#69b3a2"
Color2 <- rgb(0.2, 0.6, 0.9, 1)

# Plot actuals and predicted for Model I
ggplot(act_pred1, aes(x=week)) +
  geom_point(aes(y=actuals), size=1.5, color=Color1, group=1) + 
  geom_line(aes(y=predicteds), size=0.5, color=Color2, group=2) +
  ggtitle("Actual vs Prediction for Model I") +
  xlab("Week") +
  ylab("Weekly Sales")

# Plot actuals and predicted for Model II
ggplot(act_pred2, aes(x=week)) +
  geom_point(aes(y=actuals), size=1.5, color=Color1, group=1) + 
  geom_line(aes(y=predicteds), size=0.5, color=Color2, group=2) +
  ggtitle("Actual vs Prediction for Model II") +
  xlab("Week") +
  ylab("Weekly Sales")

# Plot actuals and predicted for Model III
ggplot(act_pred3, aes(x=week)) +
  geom_point(aes(y=actuals), size=1.5, color=Color1, group=1) + 
  geom_line(aes(y=predicteds), size=0.5, color=Color2, group=2) +
  ggtitle("Actual vs Prediction for Model II") +
  xlab("Week") +
  ylab("Weekly Sales")

# Plot actuals and predicted for Model IV
ggplot(act_pred4, aes(x=week)) +
  geom_point(aes(y=actuals), size=1.5, color=Color1, group=1) + 
  geom_line(aes(y=predicteds), size=0.5, color=Color2, group=2) +
  ggtitle("Actual vs Prediction for Model II") +
  xlab("Week") +
  ylab("Weekly Sales")







dev.off()
