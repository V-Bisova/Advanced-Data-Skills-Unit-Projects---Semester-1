# Loading the packages

library(tidyverse)
library(afex) # for running ANOVA
library(emmeans) # for running pairwise comparisons

######### Between-participants ANOVA ############################################

#looking at the effect of beverage consumed on motor task ability 
# IV = beverage type (water, single espresso, double espresso)
# DV = ability (continuous scale)


# Importng the data 

my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/cond.csv")
head(my_data) # inspecting the data - does it look like we want it to look 

# our condition variable not yet read as a factor - need to tidy data set 

#### Data tidying ####

my_data_tidied <- my_data %>%
  mutate(Condition = factor(Condition)) # recoding the Condition column 

head(my_data_tidied)


###### Summarising data - generating summary statistics #####

my_data_tidied %>% 
  group_by(Condition) %>% 
  summarise(mean = mean(Ability), sd = sd(Ability))


#### Visualising the data #####

set.seed(1234)

my_data_tidied %>% 
  ggplot(aes(x = Condition, y = Ability, colour = Condition)) +
  geom_violin() + # t give us an understanding of the shape of the distribution for all 3 conditions
  geom_jitter(width = .1) + # plotting the raw data points 
  guides(colour = FALSE) + 
  stat_summary(fun.data = "mean_cl_boot", colour = "black") + # adding summary statistics in black
  theme_minimal() +
  theme(text = element_text(size = 13))


##### ANOVA #####

# Building the ANOVA Model with the aov_4() function from the afex package
# Ability is predicted by Condition + specifying random effect, then specifing which data set 

model <- aov_4(Ability ~ Condition + (1 | Participant), data = my_data_tidied)

summary(model) # to get the ANOVA output 

# F(2,42) = 297.05, p < .001, generalised η2 = .93


# Examining where the difference is - posthoc tests 
#emmeans() uses Tukey as default, can adjust if we want to use another (adjust = "bonferroni")

emmeans(model, pairwise ~ Condition) # will tell us what means differ from what other means across conditions


################## Repeated Measures ANOVA ############################

# Importing the data 

rm_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/rm_data.csv")
head(rm_data)

# Tidying the data 

rm_data_tidied <- rm_data %>%
  mutate(Condition = factor(Condition))

head(rm_data_tidied)  

# Summarising data 

rm_data_tidied %>% 
  group_by(Condition) %>% 
  summarise(mean = mean(RT), sd = sd(RT))

# Visualising our data 

rm_data_tidied %>% 
  ggplot(aes(x = fct_reorder(Condition, RT), y = RT, colour = Condition)) +
  geom_violin() +
  geom_jitter(width = .1) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Condition", y = "Reaction time (s)")


# Building the model
# Specifying the repeated measures design in the random effect term: our random effect we define as (1 + Condition | Participant)

rm_model <- aov_4(RT ~ Condition + (1 + Condition | Participant),
                  data = rm_data_tidied)

summary(rm_model) # also output for tests for sphericity - if significant, it's violated - report 

anova(rm_model) # To get the effect size & for adjusted degrees of freedom (1.97, 61)

# F(1.97, 61) = 238.23, p < .001, generalised η2 = .84


# Post-hoc tests - where is the difference? 

emmeans(rm_model, pairwise ~ Condition, adjust = "Bonferroni")


################### Factorial ANOVA ###############################

factorial_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/factorial_data.csv")

head(factorial_data)


# Recoding so IVs read as factors not characters to fit our experimental design

factorial_data_tidied <- factorial_data %>% 
  mutate(Sentence = factor(Sentence), Context = factor(Context))

head(factorial_data_tidied)

# Summarising data 

factorial_data_tidied %>% 
  group_by(Context, Sentence) %>% 
  summarise(mean_rt = mean(RT), sd_rt = sd(RT))

# But missing values - need to check for missing values and then remove
# Install and load the visdat package 

library(visdat)

vis_miss(factorial_data_tidied)

# There are 0.1% missing values - add na.rm to summarise function 

factorial_data_tidied %>% 
  group_by(Context, Sentence) %>% 
  summarise(mean_rt = mean(RT, na.rm = TRUE), sd_rt = sd(RT, na.rm = TRUE))


# Visualising our data 

factorial_data_tidied %>% 
  filter(!is.na(RT)) %>% 
  ggplot(aes(x = Context:Sentence, y = RT, colour = Context:Sentence)) +
  geom_violin() +
  geom_jitter(width = .1, alpha = .25) +
  guides(colour = FALSE) +
  stat_summary(fun.data = "mean_cl_boot", colour = "black") +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Context x Sentence", y = "Reaction Time (ms)")

# Building our ANOVA model


# F1 ANOVA- a by-subjects ANOVA (i.e., treat Subjects as a random effect)

model_subjects <- aov_4(RT ~ Context * Sentence + 
                          (1 + Context * Sentence | Subject), 
                        data = factorial_data_tidied, na.rm = TRUE)
anova(model_subjects)
# not sig. main effect of factor 1 "Context" p = .080
# not sig. main effect of factor 2 "Sentence", p = .431
# A significant interaction effect (Context x Sentence), F(1,59) = 4.60, p < 0.05



# F2 ANOVA - by-items analysis to ensure any effects aren't limited to just a subset of experimental items

model_items <- aov_4(RT ~ Context * Sentence + (1 + Context * Sentence | Item),
                     data = factorial_data_tidied, na.rm = TRUE)
anova(model_items)

# No sig. main effects for Context or Sentence 
# Significant interaction effect for Context x Sentence, F(1,27) = 5.77, p < .05


# Interpreting by-subjects ANOVA model 

emmeans(model_subjects, pairwise ~ Context * Sentence, adjust = "none")

# The two key comparisons are: Negative-Negative vs Positive- Negative
# and Negative-Positive vs Positive-Positive
# Adjust p-value manually by multiplying their p-values by 2 (for the 2 comparisons that are theoretically meaningful)

# An interaction interaction effect being driven by negative sentences being read more qickly 
# following a negative context than a positive context 











