# Loading the packages ----

library(tidyverse) # Load the tidyverse packages
library(afex) # ANOVA functions
library(emmeans) # Needed for pairwise comparisons


# Importing the data ----

my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/12_glm_anova_pt2/master/data/ancova_data.csv")


head(my_data)

colnames(my_data) <- c("participant", "condition", "ability", "gaming")

my_data <- my_data %>% 
  mutate(condition = factor(condition))

head(my_data)

# Summary Statistics ----

my_data %>% 
  group_by(condition) %>% 
  summarise(mean_ability = mean(ability), sd_ability = sd(ability))

# Data visualisations ----

set.seed(1234)

ggplot(my_data, aes(x = gaming, y = ability,  colour = condition)) + 
  geom_point(size = 3, alpha = .9) +
  labs(x = "Gaming Frequency (hours per week)", 
       y = "Motor Ability") +
  theme_minimal() +
  theme(text = element_text(size = 11)) 

# Building the ANOVA model (ignoring the covariate, i.e., gaming frequency)

anova_model <- aov_4(ability ~ condition + (1 | participant),
                     data = my_data)

anova(anova_model) # F (2,42) = 53.43, p < .001, generalised n2 = .718

# Pairwise comparisons

emmeans(anova_model, pairwise ~ condition)
# On the basis of this, we might conclude that we have an effect of Condition, 
# and that each of our three groups differs significantly from the others. 
# But would this be right? No, because we haven’t taken account of our covariate.


# Building the ANCOVA model (taking into account the covariate, gaming frequency)

model_ancova <- aov_4(ability ~ gaming + condition + (1 | participant), 
                      data = my_data, factorize = FALSE)
anova(model_ancova) # [F(2,41) = 0.88, p = .4234 - Condition]
# Gaming - sig. F(1,41) = 53.56, p < .001, gen.n2 = .566


# Producing the adjusted means 

emmeans(model_ancova, pairwise ~ condition)
# Double Espresso   6.32
# Single Espresso   6.87
# Water             7.33 



# ANOVA as a special case of REGRESSION # ----

# Data Visualisation ----
my_data %>%
  ggplot(aes(x = condition, y = ability, colour = condition)) +
  geom_violin() +
  geom_jitter(width = .05, alpha = .8) +
  labs(x = "Condition", 
       y = "Motor Ability") +
  stat_summary(fun.data = mean_cl_boot, colour = "black") +
  guides(colour = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 12))

# Relevel the data - so water is the intercept ----

my_data <- my_data %>%
  mutate(condition = fct_relevel(condition,
                                 c("Water", "Double Espresso", "Single Espresso")))
head(my_data)
View(my_data)

# Setting up our contrasts ----

contrasts(my_data$condition) 

# ANOVA as a Linear Model ----

model_lm <- lm(ability ~ condition, data = my_data)
model_lm # we get the coefficients 
# To work out the mean Ability of our Double Espresso Group, 
# we use the coding for the Double Espresso group (1, 0) with our equation:
# Ability = Intercept + β1(Double Espresso) + β2(Single Espresso)


# ANCOVA as a Linear Model ----

ancova_lm <- lm(ability ~ gaming + condition, data = my_data)
ancova_lm # we get the coefficients

# We can work out the mean of our reference group (Water) by plugging in the values
# to our equation - note that Gaming is not a factor and we need to enter the mean of this variable.

mean(my_data$gaming) # to compute the mean for the covariate, Gaming 
# We add this mean (12.62296) to our equation alongside the co-efficients for 
# each of our predictors. With our dummy coding scheme, we can work out the adjusted mean of our Water group.

# Centering our covariate ----
# -- standardises the variable (with the mean centred on zero) and removes the need 
# to multiply the linear model coeffficient for the covariate by the covariate’s mean

my_scaled_data <- my_data %>%
  mutate(centred_gaming = scale(gaming))

plot(density(my_scaled_data$gaming))

plot(density(my_scaled_data$centred_gaming))

# Re-building the ancova linear model with our scaled & centred covariate ----

model_ancova_centred <- lm(ability ~ centred_gaming + condition, data = my_scaled_data)
model_ancova_centred

# We see that the Intercept now corresponds to the adjusted mean for the Water group. 
#  We can calculate the adjusted mean for the Double Espresso group by 
# subtracting 1.0085 from 7.3280, and we can calculate the adjusted mean for the Single Espresso group by subtracting 0.4563 from 7.3280.






