# Loading the packages

library(tidyverse)
library(afex) # for running ANOVA
library(emmeans) # for running pairwise comparisons
library(visdat) # for visualising missing values 


##### Challenge 1: (between-participants ANOVA) IV = lexical frequency (high, low)
# DV - RT 


# Import data 

Q1_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data1.csv")

head(Q1_data)

# Data tidying - Recoding our IV 

Q1_data_tidied <- Q1_data %>% 
  mutate(Condition = factor(Condition))

head(Q1_data_tidied)


View(Q1_data_tidied)


# Summarising the data 

Q1_data_tidied %>% 
  group_by(Condition) %>% 
  summarise(mean = mean(RT), sd = sd(RT))


# Visualising the data 

set.seed(1234) # to make visualisations reproducible - setting the starting point for 
# this algorithm to work - do it each time before you do something that involves an element of randomness (e.g., visualisaiton with geom_jitter())

Q1_data_tidied %>% 
  ggplot(aes(x = Condition, y = RT, colour = Condition)) + 
  geom_violin() +
  geom_jitter(width = .1, alpha = .5) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) + 
  labs(x = "Lexical Frequency", y = "Reaction Time (ms)")
  

# Building the ANOVA model 

Q1_model <- aov_4(RT ~ Condition + (1 | Subject), data = Q1_data_tidied)

summary(Q1_model)

# There is a significant effect of lexical frequency, F(1, 22) = 91.22, p < .001
# No need for post hoc tests as only 2 levels - just look at summary stats / visualisation


##### Challenge 2: (between-participants ANOVA) IV = lexical frequency (very low, low, high, very high) = Condition
# DV = RT (reaction time)


Q2_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data2.csv")

head(Q2_data)

# Tidying the data - recoding into factor 

Q2_data_tidied <- Q2_data %>% 
  mutate(Condition = factor(Condition))

head(Q2_data_tidied)


# Summary statistics 

Q2_data_tidied %>% 
  group_by(Condition) %>% 
  summarise(mean = mean(RT), sd = sd(RT))



# Visualising the data 

Q2_data_tidied %>% 
  ggplot(aes(x = fct_reorder(Condition, RT, .desc = TRUE), y = RT, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .25) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot") +
  theme_minimal() +
  theme(text = element_text(size = 13)) + 
  labs(title = " Reaction time across levels of lexical frequency", 
       x = "Lexical Frequency", y = "Reaction Time (ms)")

# Building the ANOVA model 

Q2_model <- aov_4(RT ~ Condition + (1 | Subject), data = Q2_data_tidied)

summary(Q2_model)

# F(3,44) = 203.21, p < .001, generalised η2 = .93

# Post-hoc tests 

emmeans(Q2_model, pairwise ~ Condition)


###### Challenge 3 - 2 x 2 factorial (repeated measures)
# IV(1) - Size (large, small), IV(2) - colour (B&W, Colour)
# DV - RT 

# Import data 

Q3_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data3.csv")

head(Q3_data)


# Tidying the data - recoding both IVs into factors 

Q3_data_tidied <- Q3_data %>% 
  mutate(Size = factor(Size), Colour = factor(Colour))

head(Q3_data_tidied)


# Summary statistics 

library(visdat)
vis_miss(Q3_data_tidied) # no missing data 


Q3_summary <- Q3_data_tidied %>% 
  group_by(Size, Colour) %>% 
  summarise(mean_rt = mean(RT), sd_rt = sd(RT))


# Visualising the data 

Q3_data_tidied %>% 
  ggplot(aes(x = Size:Colour, y = RT, colour = Size:Colour)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .25) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Size x Colour", y = "Reaction Time (ms)")


Q3_data_tidied %>% 
  ggplot(aes(x = Size, y = RT, colour = Colour)) +
  facet_wrap(~ Colour) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .25) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Size", y = "Reaction Time (ms)")



Q3_summary %>% 
  ggplot(aes(x = Size, y = mean_rt, colour = Colour, group = Colour)) +
  geom_path() 

# From Duncan (on Slack)

Q3_data_tidied %>% 
  ggplot(aes(x = Size, y = RT, group = Colour, colour = Colour)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") 



# Building the ANOVA model 

Q3_model <- aov_4(RT ~ Size * Colour + (1 + Size * Colour | Subject),
                           data = Q3_data_tidied)

anova(Q3_model) 
# Significant main effect (Size) - F (1, 23) = 198.97, p < .001, generalised η2 = .623
# Also sig. main effect (Colour) - F(1, 23) = 524.27, p < .001, generalised η2 = .866
# But importantly, sig. interaction effect. F(1, 23) = 11.08, p < .01, generalised η2 = .137


# Post-hoc tests 

emmeans(Q3_model, pairwise ~ Size * Colour , adjust = "Bonferroni")

# Sig. differences for all  



##### Challenge 4 - 2 x 2 x 3 mixed design 
# IV(1) = Difficulty (Easy, Hard) [repeated measures]
# IV(2) = Time_Pressure (Time Pressure, No Time Pressure) [repeated measures]
# IV(3) = Group (Psychology Students, Maths Students, Arts Students)



# Import data 

Q4_data <- read.csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/ANOVA_data4.csv")

head(Q4_data)

str(Q4_data)

View(Q4_data)


# Tidying the data - recoding 

Q4_data_tidied <- Q4_data %>% 
  mutate(Difficulty = factor(Difficulty), Time_Pressure = factor(Time_Pressure),
         Group = factor(Group)) %>% 
  mutate(Time_Pressure = recode(Time_Pressure, 
                                "NoPressure" = "No Pressure")) %>% 
  mutate(Group = recode(Group, 
                        "Arts_Students" = "Arts Students",
                        "Maths_Students" = "Maths Students",
                        "Psychology_Students" = "Psychology Students"))

str(Q4_data_tidied)


# Summary statistics 

vis_miss(Q4_data_tidied) # No missing data 


Q4_summary <- Q4_data_tidied %>% 
  group_by(Difficulty, Time_Pressure, Group) %>% 
  summarise(mean_rt = mean(RT), sd_rt = sd(RT))


# Visualising the data 

Q4_data_tidied %>% 
  ggplot(aes(x = Difficulty:Time_Pressure, y = RT, color = Group)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .1) +
  stat_summary(fun.data = "mean_cl_boot") +
  facet_wrap(~ Group, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, vjust = .5)) +
  labs(x = "Difficulty x Time Pressure", y = "Reaction Time (ms)")


Q4_data_tidied %>% 
  ggplot(aes(x = Difficulty, y = RT, color = Time_Pressure)) +
  geom_boxplot() + 
  facet_wrap(~ Group, scales = "free") +
  labs(x = "Difficulty", y = "Reaction Time (ms)", color = "Time Pressure") +
  theme(legend.position = "top")

# Adapted Duncan's code for Question 3 here: 

Q4_data_tidied %>% 
  ggplot(aes(x = Difficulty:Time_Pressure, y = RT, group = Group, 
             colour = Group)) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line") +
  labs(x = "Difficulty x Pressure", y = "Reaction Time (ms)")




# Building the ANOVA model 


Q4_model<- aov_4(RT ~ Difficulty * Time_Pressure * Group + 
                   (1 + Difficulty * Time_Pressure | Subject),
                 data = Q4_data_tidied)


anova(Q4_model) # Significant 3-way interaction Difficulty * Time_Pressure * Group
# F(2, 69) = 4.63, p < .05, generalised n2 = .041

emmeans(Q4_model, pairwise ~ Difficulty * Time_Pressure * Group, 
        adjust = "Bonferroni")



# Looking at interaction between Difficulty & Time_Pressure separately for each level of our group factor
# Building a 2 * 2 ANOVA model for each of your student Groups 


# Building the 2 x 2 Anova for Psych students


psych_model<- aov_4(RT ~ Difficulty * Time_Pressure + 
                   (1 + Difficulty * Time_Pressure | Subject),
                 data = Q4_data_tidied %>% 
                   filter(Group == "Psychology_Students"))


anova(psych_model)

emmeans(psych_model, pairwise ~ Difficulty * Time_Pressure)



# Building the 2 x 2 Anova for Maths students

maths_model<- aov_4(RT ~ Difficulty * Time_Pressure + 
                        (1 + Difficulty * Time_Pressure | Subject),
                      data = Q4_data_tidied %>% 
                        filter(Group == "Maths_Students"))


anova(maths_model)

# Building the 2 x 2 Anova for Arts students 

arts_model<- aov_4(RT ~ Difficulty * Time_Pressure + 
                        (1 + Difficulty * Time_Pressure | Subject),
                      data = Q4_data_tidied %>% 
                        filter(Group == "Arts_Students"))


anova(arts_model)

