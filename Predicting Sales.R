# Load the data
icecream <- read.csv("http://bit.ly/2OMHgFi")

# Attach objects in the database; can be accessed by simply giving their names
attach(icecream) 

# View the data and see the structure of this dataframe
icecream
str(icecream)

# Checking for null values per each variable
sum(is.na(icecream$icecream_sales)) 
sum(is.na(icecream$income))
sum(is.na(icecream$price))
sum(is.na(icecream$temperature))
sum(is.na(icecream$country))
sum(is.na(icecream$season))

# All packages that are used on this study have been installed; eg install.packages("dplyr") for dplyr package to investigate data
# Load packages dplyr and ggplot2 for investigating and visualising the data respectively
library(dplyr)
library(ggplot2)

# Just glimpse of how the data looks like and check the classes of the variables
glimpse(icecream)


class(icecream$icecream_sales)
class(icecream$price)
class(icecream$income)
class(icecream$temperature)
class(icecream$season)
class(icecream$country)

# Summarise variables in this dataset
summary(icecream)

# Summary of descriptive statistics of sales by country 
icecream %>%
  group_by(country) %>%
  summarise(count=n(),
            mu = mean(icecream_sales), pop_med = median(icecream_sales),
            sigma = sd(icecream_sales), pop_iqr = IQR(icecream_sales),
            pop_min = min(icecream_sales), pop_max = max(icecream_sales),
            pop_q1 = quantile(icecream_sales, 0.25), # first quartile, 25th percentile
            pop_q3 = quantile(icecream_sales, 0.75)) # third quartile, 75th percentile

#Summary of descriptive statistics of sales grouped by season
icecream %>%
  group_by(season) %>%
  summarise(count=n(),
            mu = mean(icecream_sales), pop_med = median(icecream_sales),
            sigma = sd(icecream_sales), pop_iqr = IQR(icecream_sales),
            pop_min = min(icecream_sales), pop_max = max(icecream_sales),
            pop_q1 = quantile(icecream_sales, 0.25), # first quartile, 25th percentile
            pop_q3 = quantile(icecream_sales, 0.75)) # third quartile, 75th percentile

# Scatter plots for all combinations of variables 
pairs(icecream)

#Analysis for the sales; Density plot of sales, box plot of sales per country, box plot of sales per season
ggplot(icecream, aes(x=icecream_sales)) + geom_density() + xlab("Ice cream Sales") + ylab("Density") + ggtitle("Density plot of Ice cream Sales")
ggplot(data = icecream, aes(x = country, y = icecream_sales)) + geom_boxplot() + xlab("Country") + ylab("Ice cream sales") + ggtitle("Box plot of ice cream sales")
ggplot(data = icecream, aes(x = season, y = icecream_sales)) + geom_boxplot() + xlab("Season") + ylab("Ice cream sales") + ggtitle("Box plot of ice cream sales")

#A histogram of the ice cream sales instead of density plot of Sales
hist(icecream$icecream_sales)

#Analysis for the Income; Density plot of Income, box plot of Income per country, box plot of Income per season
ggplot(icecream, aes(x=income)) + geom_density() + xlab("Income") + ylab("Density") + ggtitle("Density plot of Income")
ggplot(data = icecream, aes(x = country, y = income)) + geom_boxplot()  + xlab("Season") + ylab("Income") + ggtitle("Box plot of Income per Season")
ggplot(data = icecream, aes(x = season, y = income)) + geom_boxplot()  + xlab("Season") + ylab("Income") + ggtitle("Box plot of Income per Season")

#Analysis for the Price; Density plot of Price, box plot of Price per country
ggplot(icecream, aes(x=price)) + geom_density() + xlab("Price") + ylab("Density") + ggtitle("Density plot of Price")
ggplot(data = icecream, aes(x = country, y = price)) + geom_boxplot() + xlab("Country") + ylab("Price") + ggtitle("Box plot of Price")

#Analysis of Temperature; box plot of temperature per country
ggplot(data = icecream, aes(x = country, y = temperature)) + geom_boxplot() + xlab("Country") + ylab("Temperature") + ggtitle("Box plot of Temperature")

#Size of observations in each category of country variable
by(icecream$icecream_sales, icecream$country, length)

#Hypothesis test
#The box plots show how the medians of the two distributions compare, 
#but we can also compare the means of the distributions which the last line in that code does by adding the means on boxplot using stat_summary(). 
#We see some differences in means and test if this difference is statistically significant.

ggplot(data = na.omit(icecream),
       aes(x = country, y= icecream_sales, colour=country)) +
  geom_boxplot() + xlab("Country") +
  ylab("Ice ceam sales") +
  ggtitle("Box plot of ice cream sales") +
  stat_summary(fun.y=mean, colour="darkred", geom="point", shape=1, size=3)

#Collection of a simple random sample of size 100 from the icecream dataset, which is assigned to samp1
sales <- icecream$icecream_sales 
samp1 <- sample(sales, 100)
mean(samp1) # mean of the sample distribution for sales
glimpse(samp1)
hist(samp1)  

# For use of inference(), I have installed "statsr" package with the following command; install.packages("statsr")
library(statsr)
inference(y= icecream_sales, x = country, data = icecream,
          statistic = c("mean"),
          type = c("ht"),
          null = 0,
          alternative = c("twosided"), method = c("theoretical"), conf_level = 0.95,
          order = c("A","B"))

#Computation of the correlation coefficient between pairs of our numerical variables, this returns the correlation matrix
icecream %>%
  select(icecream_sales, income, price, temperature) %>% 
  cor() %>%
  knitr::kable(
    digits = 3,
    caption = "Correlations between icecream_sales, income, price and temperature", booktabs = TRUE
  )

#Visualize the association between the outcome variable with each of the explanatory variables
library(ggplot2)
icecream <- read.csv("http://bit.ly/2OMHgFi")
p1 <- ggplot(icecream, aes(x = income, y = icecream_sales)) +
  geom_point() +
  labs(x = "Income (in £)", y = "Ice cream sales (in £)", title = "Relationship between ice cream sales and income") + 
  geom_smooth(method = "lm", se = FALSE)
p2 <- ggplot(icecream, aes(x = price, y = icecream_sales)) +
  geom_point() +
  labs(x = "Price (in £)", y = "Ice cream sales (in £)", title = "Relationship between ice cream sales and price") + 
  geom_smooth(method = "lm", se = FALSE)
p3 <- ggplot(icecream, aes(x = temperature, y = icecream_sales)) +
  geom_point() +
  labs(x = "Temperature (in Celsius °C)", y = "Ice cream sales (in £)", title = "Relationship between ice cream sales and temperature") + 
  geom_smooth(method = "lm", se = FALSE)
library(gridExtra)
grid.arrange(p1, p2, p3)

# Scatter plot; relationship between temperature and ice cream sales by country, adding x, y axis labels, title and a different regression line for each country
ggplot(data = icecream, aes(x = temperature, y = icecream_sales, colour = country)) + geom_point() + 
  xlab("Temperature (in °C)") + ylab("Sales of ice cream (in £)") + 
  ggtitle("Ice cream Sales vs Temperature by Country") +
  geom_smooth(method = "lm", se = FALSE)

# Scatter plot; relationship between income and ice cream sales by country, adding x, y axis labels, title and a different regression line for each country
ggplot(data = icecream, aes(x = income, y = icecream_sales, colour = country)) + geom_point() + 
  xlab("Income (in £)") + ylab("Sales of ice cream (£)") + 
  ggtitle("Ice cream Sales vs Income by Country") +
  geom_smooth(method = "lm", se = FALSE)

# Scatter plot; relationship between price and ice cream sales by country, adding x, y axis labels, title and a different regression line for each country
ggplot(data = icecream, aes(x = price, y = icecream_sales, colour = country)) + geom_point() + 
  xlab("Price (in £)") + ylab("Sales of ice cream (in £)") + 
  ggtitle("Ice cream Sales vs Price by Country") +
  geom_smooth(method = "lm", se = FALSE)

#Fit the model; multiple regression model 
icecream <- read.csv("http://bit.ly/2OMHgFi")
Sales_model <- lm(icecream_sales ~ income + price + temperature + country + season, data = icecream) 
summary(Sales_model)

#Computation of the t-critical value
qt(0.025, df=992)

#Confidence intervals of coefficients on explanatory variables at a 90% confidence level
confint(Sales_model, level = 0.90)


# Model understanding 
country_a_20 <- data.frame(income = 20000, price = 3, temperature = 20, country = "A", season = "Winter")
predict(Sales_model, country_a_20, interval = "prediction", level = 0.95)
country_b_30 <- data.frame(income = 30000, price = 3, temperature = 20, country = "B", season = "Winter")
predict(Sales_model, country_b_30, interval = "prediction", level = 0.95)

country_ice_temp1 <- data.frame(income = 30000, price = 3, temperature = 20, country = "B", season = "Winter")
predict(Sales_model, country_ice_temp1, interval = "prediction", level = 0.95)
country_ice_temp2 <- data.frame(income = 30000, price = 3.5, temperature = 22, country = "B", season = "Winter")
predict(Sales_model, country_ice_temp2, interval = "prediction", level = 0.95)


#Testing conditions, first condition is linearity 
par(mfrow=c(1,2))
plot(Sales_model$residuals ~ icecream$income)
plot(Sales_model$residuals ~ icecream$price)
plot(Sales_model$residuals ~ icecream$temperature)

#Second condition; nearly normally distributed error terms
hist(Sales_model$residuals) 
qqnorm(Sales_model$residuals)
qqline(Sales_model$residuals)

#Constant variability of residuals
plot(Sales_model$residuals ~ Sales_model$fitted)  

#Independent residuals
plot(Sales_model$residuals) 

# Prediction
pred <- data.frame(income = 30000, price = 3, temperature = 23, country = "A", season = "Spring")
predict(Sales_model, pred, interval = "prediction", level = 0.95)
