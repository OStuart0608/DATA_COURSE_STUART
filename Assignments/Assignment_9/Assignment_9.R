library(tidyverse)
library(modelr)
library(easystats)
library(GGally)

Assignment_9_data <- read.csv("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Data/GradSchool_Admissions.csv")

view(Assignment_9_data)

Assignment_9_data1 <- Assignment_9_data %>%
  mutate(admit = as.character(admit)) %>%
  mutate(rank = as.character(rank)) %>%
  view()

# Replace values using logical indexing
Assignment_9_data1$admit[Assignment_9_data1$admit == 1] <- "Yes"
Assignment_9_data1$admit[Assignment_9_data1$admit == 0] <- "No"


Assignment_9_data1 %>%
  ggplot(mapping = aes(x = gpa, 
                       y = rank, colour = admit
  )) + 
  geom_point() +
  labs(title = 'Grad gpa and rank admitions',
       x = 'GPA',
       y = 'Rank') + 
  theme(axis.text.x = element_text(angle=90, hjust = 1 ))

Assignment_9_data1 %>%
  ggplot(mapping = aes(x = gre, 
                       y = rank, colour = admit
  )) + 
  geom_point() +
  labs(title = 'Grad gre and rank admitions',
       x = 'GRE',
       y = 'Rank') + 
  theme(axis.text.x = element_text(angle=90, hjust = 1 ))

Assignment_9_data1 %>%
  ggplot(mapping = aes(x = rank, 
                       y = gpa, colour = gre
  )) + 
  geom_point() +
  labs(title = 'Grad Rank GPA with Gre',
       x = 'Rank',
       y = 'GPA') + 
  theme(axis.text.x = element_text(angle=90, hjust = 1 ))



## Modeling the data

mod1 <- glm(data=Assignment_9_data,
            formula=admit ~ gpa)

mod2 <- glm(data=Assignment_9_data,
            formula= admit ~ gre)

mod3 <- glm(data=Assignment_9_data,
            formula= admit ~ rank)

mod4 <- glm(data=Assignment_9_data,
            formula=admit ~ gpa * gre)

full_mod <- glm(data=Assignment_9_data,
                formula=admit ~ gpa * gre * rank)

model_comparison <- compare_performance(mod1,mod2,mod3,mod4,full_mod,
                                        rank=TRUE)

model_comparison

compare_performance(mod1,mod2,mod3,mod4,full_mod,
                    rank=TRUE) %>% plot()

## Predictions

library(modelr)
Assignment9_Predictions <- add_predictions(Assignment_9_data, full_mod)

Assignment9_Predictions


plot(Assignment9_Predictions$pred, 
     Assignment9_Predictions$admit, 
     xlab = "Prediction", 
     ylab = "Admit")

plot(Assignment9_Predictions$pred, 
     Assignment9_Predictions$gpa, 
     xlab = "Prediction", 
     ylab = "Admit")

broom

summary(Assignment9_Predictions)
broom::tidy(Assignment9_Predictions)