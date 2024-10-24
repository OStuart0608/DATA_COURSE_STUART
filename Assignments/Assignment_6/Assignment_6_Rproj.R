library(tidyverse)

assignment_6_data <- read_csv("C:/Users/Owner/Desktop/Biol 3100/DATA_COURSE_STUART/Data/BioLog_Plate_Data.csv")
view(assignment_6_data)


# Instructions 1. Cleans this data into tidy (long) form

assignment_6_data_1 <- assignment_6_data %>%
  pivot_longer(starts_with('Hr'),
               names_to = 'Time_in_Hours',
               values_to = 'Absorbance') %>%
  mutate(Time_in_Hours= case_when(Time_in_Hours =='Hr_24'~24,
                          Time_in_Hours =='Hr_48'~48,
                          Time_in_Hours == 'Hr_144'~144,)) %>%
  view()




# 2. Creates a new column specifying whether a sample is from soil or water
assignment_6_data_2 <- assignment_6_data_1 %>%
  mutate( Sample_Type = case_when(`Sample ID` == 'Clear_Creek' ~ "Water",
                                  `Sample ID` == 'Soil_1' ~ "Soil",
                                  `Sample ID` == 'Soil_2' ~ "Soil",
                                  `Sample ID` == 'Waste_Water' ~ "Water"  )) %>%
  view()



# 3. Generates a plot that matches this one (note just plotting dilution == 0.1)

assignment_6_data_2 %>%
  filter(Dilution == 0.1) %>%
  ggplot(aes(x = Time_in_Hours,
             y = Absorbance,
             colour =Sample_Type)) + 
  geom_smooth(se = F) +
  facet_wrap(~ Substrate) +
  ylim (0, 2) + 
  labs (x = 'Time_in_Hours',
        y = 'Absorbance') + 
  theme_minimal()


# 4. Generates an animated plot that matches this one (absorbance values are mean of all 3 replicates for each group) This plot is just showing values for the substrate “Itaconic Acid”
assignment_6_data_3 = assignment_6_data_2 %>%
  filter(Substrate == 'Itaconic Acid')%>%
  group_by(`Sample ID`, Rep, Time_in_Hours, Dilution) %>%
  summarize(assignment_6_data_3 = mean(Absorbance, na.rm = TRUE))
view(assignment_6_data_3)


Mean_Absorbance = assignment_6_data_3 %>%
  group_by(`Sample ID`, Time_in_Hours, Dilution) %>%
  summarize(Mean_Absorbance = mean(assignment_6_data_3, na.rm = T))
view(Mean_Absorbance)

animation_plot_1 = Mean_Absorbance %>%
  ggplot(aes(x = Time_in_Hours,
             y = Mean_Absorbance,
             color = `Sample ID`)) + 
  geom_line() +
  facet_wrap('Dilution') + 
  labs(x = 'Time',
       y = 'Mean_Absorbance',
       color = 'Type') + 
  transition_reveal(Time_in_Hours)
animate(animation_plot_1, nframes = 100, fps = 10)



library(gapminder)
library(ggimage)
library(gganimate)
library(patchwork)
library(ggminder)
library(tidyverse)

 