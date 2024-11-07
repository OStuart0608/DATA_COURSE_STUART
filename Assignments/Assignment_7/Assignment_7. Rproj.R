library(tidyverse)
library(easystats)
library(ggplot2)

## 1.Import the Assignment_7/Utah_Religions_by_County.csv data set

 read.csv("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Assignments/Assignment_7/Utah_Religions_by_County.csv")
 
Utah_7 <- read.csv("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Assignments/Assignment_7/Utah_Religions_by_County.csv") 






## 2.Clean it up into “tidy” shape

view(Utah_7)
head(Utah_7)

Utah_7_1 <- Utah_7 %>%
  pivot_longer(cols = c("Assemblies.of.God", "Episcopal.Church", "Pentecostal.Church.of.God", "Greek.Orthodox", "LDS", "Southern.Baptist.Convention", "United.Methodist.Church", "Buddhism.Mahayana", "Catholic", "Evangelical", "Muslim", "Non.Denominational", "Orthodox"),
               names_to ='Religion',
               values_to = 'Religion_Percent') %>%
view()





## 3.Explore the cleaned data set with a series of figures (I want to see you exploring the data set)

Utah_7_1 %>%
  ggplot(aes(x = Religion, y = Religion_Percent, colour = County)) +
  geom_point(stat = "identity") +
  labs(title = 'Utah religious population by County',
       x = 'Religion',
       y = 'Religion Percentage') +
  theme(axis.text.x = element_text(angle = 90)) 
 
  
Utah_7_1 %>%
  ggplot(mapping = aes(x = County , 
                       y = Pop_2010,
  )) + 
  geom_bar(stat = "identity") +
  labs(title = 'Utah population Graph',
       x = 'County',
       y = 'Population') +
  theme(axis.text.x = element_text(angle = 90))

Utah_7_1 %>%
  ggplot(mapping = aes(x = County , 
                       y = Non.Religious, colour = Pop_2010
  )) + 
  geom_point(stat = "identity") +
  labs(title = 'Utah Non Religious population',
       x = 'County',
       y = 'Non Religious Percentage') +
  theme(axis.text.x = element_text(angle = 90))




## 4 & 5.Address the questions:“Does population of a county correlate with the proportion of any specific religious group in that county?”
###“Does proportion of any specific religion in a given county correlate with the proportion of non-religious people?”
###Just stick to figures and maybe correlation indices…no need for statistical tests yet, Add comment lines that show your thought processes _____________

head(Utah_7_1)
names(Utah_7_1)

view(Utah_7_1)
mod6 <- glm(data = Utah_7_1,
            formula = Pop_2010 ~ Religion_Percent)
  summary(mod6)


mod6 %>%
  ggplot(aes(x = Pop_2010, y = Religion_Percent)) +
  geom_point() +
  geom_smooth()
## From this plot based on the model mod6, while the blue line may be straight, there is a lot of deviation from the points suggesting that population of a county does not correlate with the proportion of any specific religious group in that county.

mod7 <- glm(data = Utah_7_1,
            formula = Religion_Percent ~ Non.Religious)
  summary(mod7)

mod7 %>%
  ggplot(aes(x = Religion_Percent, y =Non.Religious )) +
  geom_point() +
  geom_smooth()

## From this plot based on the model mod7, the blue line is curved and there is a lot of deviation from the points on the graph suggesting the proportion of any specific religion in a given county does not correlate with the proportion of non-religious people.












