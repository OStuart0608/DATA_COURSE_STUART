library(tidyverse)
library(readxl)
library(gapminder)
library(ggimage)
library(gganimate)
library(patchwork)
library(easystats)
library(ggplot2)
library(MASS)

Final_Project_Data <- read.csv("../OStuart0608.github.io/IHME_GBD_2010_MORTALITY_AGE_SPECIFIC_BY_COUNTRY_1970_2010 (1).csv")
view(Final_Project_Data)


Final_Project_Data6 <- Final_Project_Data5 %>%
  filter(Country.Name == c('Australia', 'Brazil', 'Central Africa', 'Canada', 'China', 'France', 'United Kingdom', 'Japan', 'Mexico', 'United States')) %>%
  view()

Final_Project_Data1 <- Final_Project_Data
view(Final_Project_Data1)
Final_Project_Data1$Number.of.Deaths <- as.numeric(gsub(",", "", Final_Project_Data1$Number.of.Deaths)) %>%
  view()

sapply(Final_Project_Data5, class)

Final_Project_Data5 <-  Final_Project_Data1$Number.of.Deaths <- as.numeric(gsub(",", "", Final_Project_Data1$Number.of.Deaths)) %>%
  view()

write.csv(Final_Project_Data6, "Final_Project_Data6.csv", row.names = FALSE)



Final_Project_Data5 <- Final_Project_Data1 %>%
  mutate(Number.of.Deaths = as.numeric(gsub(",", "", Number.of.Deaths)),
         Death.Rate.Per.100.000 = as.numeric(gsub(",", "", Death.Rate.Per.100.000))) %>%
  view()

## code for 1st plot on on my website




# Save to a CSV file
write.csv(Final_Project_Data2, "Final_Project_Data2(2).csv", row.names = FALSE)




# Clean the Number.of.Deaths column (remove commas and convert to numeric)
Final_Project_Data2 <- aggregate(Number.of.Deaths ~ Country.Name, data = Final_Project_Data1, FUN = max)
view(Final_Project_Data2) 


sapply(Final_Project_Data2, class)

Final_Project_Data2$Number.of.Deaths <- as.numeric(gsub(",", "", Final_Project_Data2$Number.of.Deaths))

# Create a bar plot

ggplot(Final_Project_Data2, aes(x = Country.Name, y = Number.of.Deaths)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Number of Deaths by Country", x = "Country", y = " Max Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Code for 2nd plot on website




Final_Project_Data3 <- aggregate(Number.of.Deaths ~ Year, data = Final_Project_Data1, FUN = max)
view(Final_Project_Data3) 


sapply(Final_Project_Data3, class)

Final_Project_Data3$Number.of.Deaths <- as.numeric(gsub(",", "", Final_Project_Data3$Number.of.Deaths))

# Create a bar plot

ggplot(Final_Project_Data3, aes(x = Year, y = Number.of.Deaths)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = " Max Number of Deaths Per Year", x = "Year", y = " Max Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
library(knitr)



## Code for third graph on website

Final_Project_Data4 <- aggregate(Number.of.Deaths ~ Sex, data = Final_Project_Data1, FUN = max)
view(Final_Project_Data4) 


sapply(Final_Project_Data4, class)

Final_Project_Data4$Number.of.Deaths <- as.numeric(gsub(",", "", Final_Project_Data4$Number.of.Deaths))

# Create a bar plot

ggplot(Final_Project_Data4, aes(x = Sex, y = Number.of.Deaths)) +
  geom_bar(stat = "identity", fill = "brown1") +
  labs(title = " Max Number of Deaths Per Sex", x = "Sex", y = " Max Number of Deaths") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


## Creating a Statistical model

names(Final_Project_Data6)

Final_Project_mod <- glm(data = Final_Project_Data6,
                         formula = Death.Rate.Per.100.000 ~ Country.Name + Year)
summary(Final_Project_mod)



Final_Project_mod1 <- glm(data = Final_Project_Data6,
                          formula = Death.Rate.Per.100.000 ~ Country.Name + Age.Group)
summary(Final_Project_mod1)



Final_Project_mod2 <- glm(data = Final_Project_Data6,
                          formula = Death.Rate.Per.100.000 ~ Country.Name  + Sex)
summary(Final_Project_mod2)


Final_Project_mod3 <- glm(data = Final_Project_Data6,
                          formula = Death.Rate.Per.100.000 ~ Country.Name  + Year + Age.Group + Sex)
summary(Final_Project_mod3)


compare_performance(Final_Project_mod, Final_Project_mod1, Final_Project_mod2, Final_Project_mod3) %>% plot()

library(broom)

broom::tidy(Final_Project_mod3) 


## Creating prediction values

library(modelr)
Final_Project_Predictions <- add_predictions(Final_Project_Data6, Final_Project_mod3)

view(Final_Project_Predictions)




## 7. plots these predictions alongside the real data
library(ggplot2)
library(tidyverse)
library(modelr)
library(dplyr)
library(broom)
library(kableExtra)
library(easystats)






par(mfrow = c(1, 1))

# Plot 1: Predictions vs Death ate
plot(Final_Project_Predictions$pred, 
     Final_Project_Predictions$Death.Rate.Per.100.000, 
     xlab = "Prediction", 
     ylab = "Death Rate Per 100,000")

broom

summary(Final_Project_Predictions)
broom::tidy(Final_Project_Predictions)






write.csv(Final_Project_Predictions, "Final_Project_Predictions.csv", row.names = FALSE)


# Check the structure of the object
str(Final_Project_Predictions)

# Convert factors/characters to numeric if necessary
Final_Project_Predictions2 <- lapply(Final_Project_Predictions, function(x) {
  if (is.character(x) || is.factor(x)) as.numeric(as.character(x)) else x
})

# Try again with broom::tidy()
broom::tidy(Final_Project_Predictions2)