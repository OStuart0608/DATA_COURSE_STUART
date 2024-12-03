library(tidyverse)
library(ggplot2)
library(modelr)
library(dplyr)
library(broom)
library(kableExtra)
library(easystats)

Exam_3_data <- read.csv("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Exams/Exam 3/FacultySalaries_1995.csv")

view(Exam_3_data)

names(Exam_3_data)


Exam_3_data_1  = select(Exam_3_data, AvgFullProfSalary, AvgAssocProfSalary, AvgAssistProfSalary, NumFullProfs, NumAssocProfs, NumAssistProfs, State, Tier  )

view(Exam_3_data_1)

Exam_3_data_2 <- rename(Exam_3_data_1, PFull = AvgFullProfSalary, PAssoc = AvgAssocProfSalary, PAssist = AvgAssistProfSalary )
view(Exam_3_data_2)


Exam_3_data_3 <- Exam_3_data_2 %>%
  pivot_longer(starts_with('P'),
               names_to = 'Rank',
               values_to = 'Salary') %>%
  view()


Exam_3_data_4 <- subset(Exam_3_data_3, Tier != "VIIB")
view(Exam_3_data_4)


Exam_3_data_4 %>%
  ggplot(aes(x = Rank , 
             y = Salary, colour = Rank
  )) + 
  geom_boxplot()+
  facet_wrap(~Tier)

## Question 2

aova_Exam_data <- Exam_3_data_4 %>%
  aov(data = .,
      formula = Salary ~ State + Tier + Rank)
summary(aova_Exam_data)




## Question 3

Exam_3_2ndData <- read.csv("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Exams/Exam 3/Juniper_Oils.csv")
view(Exam_3_2ndData)



Exam_3_2ndData_1  = select(Exam_3_2ndData, alpha.pinene,para.cymene,alpha.terpineol,cedr.9.ene,alpha.cedrene,beta.cedrene,cis.thujopsene,alpha.himachalene,beta.chamigrene,cuparene,compound.1,alpha.chamigrene,widdrol,cedrol,beta.acorenol,alpha.acorenol,gamma.eudesmol,beta.eudesmol,alpha.eudesmol,cedr.8.en.13.ol,cedr.8.en.15.ol,compound.2,thujopsenal,YearsSinceBurn)

view(Exam_3_2ndData_1)

Exam_3_2ndData_2 <- Exam_3_2ndData_1 %>%
  pivot_longer(cols = c(alpha.pinene,para.cymene,alpha.terpineol,cedr.9.ene,alpha.cedrene,beta.cedrene,cis.thujopsene,alpha.himachalene,beta.chamigrene,cuparene,compound.1,alpha.chamigrene,widdrol,cedrol,beta.acorenol,alpha.acorenol,gamma.eudesmol,beta.eudesmol,alpha.eudesmol,cedr.8.en.13.ol,cedr.8.en.15.ol,compound.2,thujopsenal),
               names_to = 'Compound',
               values_to = 'Concentration') %>%
  view()


## Question 4


Exam_3_2ndData_2 %>%
  ggplot(aes(x = YearsSinceBurn , 
             y = Concentration
  )) + 
  geom_smooth()+
  facet_wrap(~Compound, scales = "free")

## Question 5

Exam_3_mod <- glm(data = Exam_3_2ndData_2,
                  formula = Concentration ~ YearsSinceBurn * Compound)
summary(Exam_3_mod)

broom::tidy(Exam_3_mod) %>%
  filter(p.value < 0.05)