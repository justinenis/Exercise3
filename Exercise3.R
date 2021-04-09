library(ggplot2)
library(tidyverse)
library(rsample)
library(caret)
library(foreach)
library(modelr)
library(parallel)
library(lubridate)
library(dplyr)
library(data.table)
library(ggrepel)
library(ggmap)
library(equatiomatic)


# Question 1.1
# 1. Why can’t I just get data from a few different cities and run the regression 
# of “Crime” on “Police” to understand how more cops in the streets affect crime? 
# (“Crime” refers to some measure of crime rate and “Police” measures the number 
# of cops in a city.)

# Taking the data from a handful of cities would only establish correlation and 
# not caustion. To derive a casual relationship between number of police and the 
# amount of crime, there would needed to be data taken of one variable that is 
# unrelated to the other varibale. 

#Question 1.2
# The researchers are UPenn were able to isolate the effect by using the Terror 
# Alert System and data from Washington, D.C. Since Washington, D.C. is considered a 
# high target for terrorism, as the threat level increased to orange on the Terror 
# Alert System, more police officers are added around the city, regardless of crime.
# Researchers were then able to observe a causal relationship between police and crime.


# At the 5% level of significance, daily crime decreases by 7.316 occurrences when
# the Terror Alert System is high, all else equal. When controlling for midday ridership, daily 
# crime decreases by 6.046 occurrences, all else equal. Additionally, at the 1% 
# level of significance, midday ridership increases by 17.34% when the Terror Alert
# System is high. In all cases, the R^2 is close to zero, which would be expected
# since police officers were added as a result of a high terrorist threat. 



# Question 1.3
# The researchers were attempting to capture the effect of tourism in Washington, D.C. 
# when the terror threat level was orange so as to understand if there would be fewer 
# victims in a terrorist attack. 


# Question 1.4
# The model being estimated here is the dependent variable of daily total number 
# of crimes regressed on two interaction variables and a log taken for midday 
# ridership. The two interactions are both interacting with the variable High 
# Alert, with District 1 referring to a dummy variable. 

# All else equal, daily total number of crimes decreases by 2.621 occurrences in 
# District 1 when the terror threat level is high. All else equal, daily total number
# of crime decreases by 0.571 occurrences in other districts when the terror threat
# level is high. All else equal, midday ridership increases 2.477% when the terror
# threat level is high. All else equal, daily total number of crimes decreases by 
# 11.058 occurrences when the terror threat level is high.






# Question 2 
greenbuildings = read.csv('/Users/franklinstudent/Desktop/GitHub/Exercise3/greenbuildings.csv')


greenbuildings = greenbuildings %>%
  mutate(Annual_revenue_per_sqft = Rent*leasing_rate)%>%
  filter(green_rating == 1) %>%
  drop_na()


new_green = greenbuildings %>%
  select(1:4, 7:11, 15:24)


X = as.matrix(new_green[,1:18])
y = new_green[,19]

pc_greenbuildings = prcomp(X, scale=TRUE)
greendata = pc_greenbuildings$x[,1:18]
pcr1 = lm(y ~ greendata)

summary(pcr1)


plot(fitted(pcr1), y)



# Question 3

housing = read.csv('/Users/franklinstudent/Desktop/GitHub/Exercise3/CAhousing.csv')


housing = housing %>%
  mutate(totalRooms = totalRooms/households) %>%
  mutate(totalBedrooms = totalBedrooms/households) %>%
  mutate(across(totalRooms, scale)) %>%
  mutate(across(totalBedrooms, scale))


housing_split = initial_split(housing, prop = 0.8)
housing_train = training(housing_split)
housing_test = testing(housing_split)

lm2 = lm(medianHouseValue ~ longitude + latitude + housingMedianAge + 
           totalRooms+ totalBedrooms + population + households + 
           medianIncome, data=housing_train)
lm0 = lm(medianHouseValue ~ 1, data = housing_train)
lm_forward = step(lm0, direction = 'forward',
                  scope=~(longitude + latitude + housingMedianAge + 
                            totalRooms+ totalBedrooms + population + households + 
                            medianIncome)^2)

AIC(lm2)
AIC(lm_forward)

coef(lm_forward)
summary(lm_forward)

getCall(lm_forward)

mod1 <- lm_forward
extract_eq(mod1)


  
  

new_housing = housing %>%
  mutate(predicted =  predict(lm_forward, housing, type="response"))%>%
  mutate(resid = medianHouseValue - predicted)
  


#out-of-sample error
sqrt(mean((predicted - actual)^2))



predicted2 <- predict(lm_forward, housing_train, type = "response")
actual2 <- housing_train[, "medianHouseValue"]

#in_sample error
sqrt(mean((predicted2 - actual2)^2))

rmse_lm2 = rmse(lm2, housing_test)
rmse_lm_forward = rmse(lm_forward, housing_test)

rmse_lm2
rmse_lm_forward

knn_lm_forward = knnreg(medianHouseValue ~ medianIncome + housingMedianAge + 
                          households + population + latitude + longitude + 
                          totalBedrooms + totalRooms, data = housing_train, k = 20)



K_folds = 30
k_grid = seq(2, 80, by=2)
House_folds = crossv_kfold(housing, k=K_folds)

cv_grid = foreach(k = k_grid, .combine='rbind') %do% {
  models = map(House_folds$train, ~ knnreg(medianHouseValue ~ medianIncome + housingMedianAge + 
                                             households + population + latitude + longitude + 
                                             totalBedrooms + totalRooms, k=k, data = housing, use.all=FALSE))
  errs = map2_dbl(models, House_folds$test, modelr::rmse)
  c(k=k, err = mean(errs), std_err = sd(errs)/sqrt(K_folds))
} %>% as.data.frame

lm_forward_rmse_plot = ggplot(cv_grid) +
  geom_point(aes(x=k, y=err)) +
  geom_errorbar(aes(x=k, ymin = err-std_err, ymax = err+std_err)) + 
  labs(y="RMSE", title="RMSE vs k for KNN regression")

lm_forward_rmse_plot






ggmap::register_google(key = "AIzaSyAA9IwVuC5w1x2QtoM6KLXchxGMaETnQnM")


p <- ggmap(get_googlemap(center = c(lon =  -119.417931, lat = 36.778259),
                         zoom = 6, scale = 2,
                         maptype ='terrain',
                         color = 'color'))
figure1 = p + geom_point(aes(x = longitude, y = latitude, color = medianHouseValue), 
                       data = housing, size = 0.5)+ xlab("Longitude") + ylab("Latitude") + ggtitle("Figure 1") +
  theme(plot.title = element_text(hjust = 0.5))
figure1



figure2 = p + geom_point(aes(x = longitude, y = latitude, color = medianHouseValue), 
                       data = housing_test, size = 0.5) + xlab("Longitude") + ylab("Latitude") + ggtitle("Figure 2") +
  theme(plot.title = element_text(hjust = 0.5))
figure2



figure3 = p + geom_point(aes(x = longitude, y = latitude, color = resid), 
                         data = new_housing, size = 0.5) + xlab("Longitude") + ylab("Latitude") + ggtitle("Figure 3") +
  theme(plot.title = element_text(hjust = 0.5))
figure3



