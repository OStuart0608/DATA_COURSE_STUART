## 1. loads the “/Data/mushroom_growth.csv” data set
library(ggplot2)
library(tidyverse)
library(modelr)
library(dplyr)
library(broom)
library(kableExtra)
library(easystats)

read.csv("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Data/mushroom_growth.csv")

Assingment_8_data <- read.csv("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Data/mushroom_growth.csv")


view(Assingment_8_data)


## 2. creates several plots exploring relationships between the response and predictors

summary(Assingment_8_data)


Assingment_8_data %>%
  ggplot(aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  facet_wrap(~Species)
 

Assingment_8_data %>%
  ggplot(aes(x = Light, y = GrowthRate)) +
  geom_point() +
  facet_wrap(~ Species)

Assingment_8_data %>%
  ggplot(aes(x = Temperature, y = GrowthRate)) +
  geom_point() +
  facet_wrap(~ Species)
## 3.defines at least 4 models that explain the dependent variable “GrowthRate”

mol_8 <- glm(data = Assingment_8_data,
             formula = GrowthRate ~ Light)

mol_9 <- glm(data = Assingment_8_data,
             formula = GrowthRate ~ Nitrogen)

mol_10 <- glm(data = Assingment_8_data,
             formula = GrowthRate ~ Temperature)

mol_11 <- glm(data = Assingment_8_data,
              formula = GrowthRate ~ Temperature + Light + Nitrogen)

mol_12 <- glm(data = Assingment_8_data,
              formula = GrowthRate ~ Temperature * Light * Nitrogen)
## 4.calculates the mean sq. error of each model
summary(mol_8)

mean(mol_8$residuals^2)

mean(mol_9$residuals^2)

mean(mol_10$residuals^2)


mean(mol_11$residuals^2)


mean(mol_12$residuals^2)

## 5.selects the best model you tried




compare_performance(mol_8, mol_9, mol_10, mol_11, mol_12) %>% plot()

## mol_8 is the best model


## 6. adds predictions based on new hypothetical values for the independent variables used in your model


Assignment_8_pre <- add_predictions(Assingment_8_data, mol_8)
 
view(Assignment_8_pre)




## 7. plots these predictions alongside the real data
par(mfrow = c(1, 2))

plot(Assignment_8_pre$pred, Assignment_8_pre$GrowthRate, xlab = "Prediction",
     ylab = "Growth Rate")
plot(Assignment_8_pre$Light, Assignment_8_pre$GrowthRate, xlab = "Light",
     ylab = "Growth Rate")



## Upload responses to the following as a numbered plaintext document to Canvas:
##1. Are any of your predicted response values from your best model scientifically meaningless?Explain.
# It seems that our predicited values almost match our real data values which most likley means they are scientifically meaningless. 


## 2. In your plots, did you find any non-linear relationships? Do a bit of research online and give a link to at least one resource explaining how to deal with modeling non-linear relationships in R.


# In my plots, the only non-linear relationship that I found was the nitorgen and growthrate plots by species. Link: https://www.datacamp.com/tutorial/introduction-to-non-linear-model-and-insights-using-r


## 3. Write the code you would use to model the data found in “/Data/non_linear_relationship.csv” with a linear model (there are a few ways of doing this)


read.csv("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Data/non_linear_relationship.csv")

Non_linear_Data <- read.csv("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Data/non_linear_relationship.csv")

view(Non_linear_Data)

Non_linear_Data %>%
  ggplot(aes(x = predictor, y = response)) +
  geom_point() +
  geom_smooth()

names(Non_linear_Data)

log.model <- lm(predictor~log(response), data = Non_linear_Data)
summary(log.model)
view(log.model)

names(log.model)

Non_linear_Data %>%
  ggplot(aes(x =response, y = predictor)) +
  geom_point() +
  stat_smooth(method = "lm",
              formula = y~log(x))


















































