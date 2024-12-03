## 1. Read in the unicef data (10 pts) 
library(tidyverse)

read_csv("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Exams/Exam 2/unicef-u5mr.csv")

Exam_2_data <- read_csv("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Exams/Exam 2/unicef-u5mr.csv")

view(Exam_2_data)

##2. Get it into tidy format (10 pts) 


Exam_2_data_1 <- Exam_2_data %>%
  pivot_longer(starts_with('U5MR'),
               names_to ='Year',
               values_to = 'U5MR') %>%
  transform(Year=str_replace(Year,"U5MR.","")) %>%
  view()




##3. Plot each country’s U5MR over time (20 points)

Exam_2_data_1$Year <- as.numeric(Exam_2_data_1$Year)

Exam_2_data_1 %>%
  filter(!is.na(U5MR)) %>%
  ggplot(aes(x = Year, y =U5MR)) + 
  geom_path()+
  facet_wrap(~ Continent)







## 4. Save this plot as LASTNAME_Plot_1.png (5 pts) 


ggsave("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Exams/Exam 2/LASTNAME_Plot_1.png")




##5. Create another plot that shows the mean U5MR for all the countries within a given continent at each year (20 pts)

Mean_U5MR = Exam_2_data_1 %>%
  group_by(Continent, Year) %>%
  summarize(Mean_U5MR = mean(U5MR, na.rm = T))
view(Mean_U5MR)

Mean_U5MR %>%
  ggplot(aes(x = Year,
             y = Mean_U5MR,
             color = Continent)) + 
  geom_line()










##6. Save that plot as LASTNAME_Plot_2.png (5 pts) 


ggsave("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Exams/Exam 2/LASTNAME_Plot_2.png")






##7. Create three models of U5MR (20 pts)

### mod1 should account for only Year
install.packages("easystats")
library(easystats)


view(Exam_2_data_1)



mod1 <- glm(data = Exam_2_data_1 ,
            formula = U5MR ~ Year)
### mod2 should account for Year and Continent


mod2 <- glm(data = Exam_2_data_1,
            formula = U5MR ~ Year + Continent)



### mod3 should account for Year, Continent, and their interaction term


mod3 <- glm(data = Exam_2_data_1,
            formula = U5MR ~ Year * Continent)




## 8. Compare the three models with respect to their performance

compare_performance(mod1, mod2, mod3)

compare_performance(mod1, mod2, mod3) %>% plot()


#Based on the plot of each proformance I conclude that mod3 is the best. 




## 9. Plot the 3 models’ predictions like so: (10 pts)


Exam_2_data_1$mod1 <- predict(mod1, Exam_2_data_1)
Exam_2_data_1$mod2 <- predict(mod2, Exam_2_data_1)
Exam_2_data_1$mod3 <- predict(mod3, Exam_2_data_1)


Exam_2_data_3 <- Exam_2_data_1 %>%
  pivot_longer(starts_with('mod')) %>%
  view()

Exam_2_data_4 <- Exam_2_data_3 %>%
  rename(Predicted_U5MR = value) %>%
  view()

Exam_2_data_4 %>%
  ggplot(aes(x = Year, y = Predicted_U5MR , colour = Continent))+
  geom_line() +
  facet_wrap(~ name) +
  labs(title = 'Model Predictions',
       x = 'Year',
       y = 'Predicted U5MR')



## 10. BONUS - Using your preferred model, predict what the U5MR would be for Ecuador in the year 2020. The real value for Ecuador for 2020 was 13 under-5 deaths per 1000 live births. How far off was your model prediction???
