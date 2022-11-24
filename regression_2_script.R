install.packages("ggcorrplot")
# Loading Packages ############################################################
# Load the tidyverse packages

library(tidyverse)

# Needed for correlation

library(Hmisc)

# Needed for maths functions

library(MASS) 

# Needed for VIF calculation

library(car) 

# Needed for stepwise regression 

library(olsrr)

# Needed to check model assumptions

library(performance) 

# Needed to build correlation matrix 

library(ggcorrplot) 



# Importing the data ##########################################################

reg_2 <- read_csv("https://raw.githubusercontent.com/ajstewartlang/10_glm_regression_pt2/master/data/MRes_tut2.csv")

View(reg_2)

# The data - what are our variables? 
# Predictors: age - children's chronological age (in months), RA - reading age,
# std_RA - standardised reading age, std_SPELL -  standardised spelling score
# Outcome - corr_spell = percentage correct spelling score using the list of 48 words



# Examining possible relationships ############################################

# Correlation matrix - this is from the lecture (not sure if needed?)
# -case (used to exclude variables, in this case as this is just the participant number)

corr <- cor(dplyr::select(reg_2, -case))

corr_matrix <- ggcorrplot(corr, hc.order = TRUE,
           type = "lower", lab = TRUE)

corr_matrix

# We’ll plot test performance against each of our four predictors in turn 

# Plot: age (predictor) vs corr_spell (outcome)

age_scatterplot <- ggplot(reg_2, aes(x = age, y = corr_spell)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

age_scatterplot

# Plot: RA (predictor) vs corr_spell (outcome)

RA_scatterplot <- ggplot(reg_2, aes(x = RA, y = corr_spell)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) + 
  labs(title = "Reading ability and correct spelling scores", 
       x = "Reading ability", 
       y = "Correct spelling scores (in %)")

RA_scatterplot

# Plot: std_RA (predictor) vs corr_spell (outcome)

std_RA_scatterplot <- ggplot(reg_2, aes(x = std_RA, y = corr_spell)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) + 
  labs(title = "Standardised reading ability and correct spelling scores", 
       x = "Standardised reading ability", 
       y = "Correct spelling scores (in %)")

std_RA_scatterplot

# Plot: std_SPELL (predictor) vs corr_spell (outcome)

std_SPELL_scatterplot <- ggplot(reg_2, aes(x = std_SPELL, y = corr_spell)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) + 
  labs(title = "Standardised spelling and correct spelling scores", 
       x = "Standardised spelling scores", 
       y = "Correct spelling scores (in %)")

std_SPELL_scatterplot


# Model the data #############################################################
# model_0 - the mean of our outcome variable 
# model_1 - the regression model, containing all predictors 

model_0 <- lm(corr_spell ~ 1, data = reg_2)
model_1 <- lm(corr_spell ~ age + RA + std_RA + std_SPELL, data = reg_2)

# Comparing the mean model (model_0) to our regression model (model_1)

anova(model_0, model_1)

# F (4, 42) = 60.42, p < .001 - the models significantly differ from each other
# Need to look at the RSS values and compare
# lower RSS value means there is less error between the model and the observed data
# model_1 has the lower RSS value = better 


# Checking the assumptions ####################################################

str(reg_2)

check_outliers(reg_2) #Tells me that case 10 is an outlier and for which variables 

outliers_list <- check_outliers(reg_2)

as.data.frame(outliers_list)

cooks.distance(lm(model_1))

plot(cooks.distance(lm(model_1)))


check_model(model_1) 

# Dealing with the outlier(s) - removing case 10

reg_2_drop10 <- filter(reg_2, case != "10")
View(reg_2_drop10)

#Remodel the data - model_2 which doesn't include case 10

model_2 <- lm(corr_spell ~ age + RA + std_RA + std_SPELL, data = reg_2_drop10)

check_model(model_2)

# Look at the multicollinearity values measured by VIF

vif(model_2) #RA and std_RA - problematic

rcorr(reg_2_drop10$RA, reg_2_drop10$std_RA) # strong positive correlation r = .88

#Building a new model, model_3 which excludes the predictor with the highest VIF value - RA

model_3 <- lm(corr_spell ~ age + std_RA + std_SPELL, data = reg_2_drop10)

check_model(model_3)

summary(model_3) # a = - 209.44, b(age) = 1.10, b(std_RA) = 0.38, b(std_SPELL) = 1.21
#Regression equation: 
# Spelled correct = -209.44 + (1.10 x age) + (0.38 x std_RA) + (1.21 x std_SPELL) + residual

# Fit model_0 to the same size dataset then comapre both models

model_0 <- lm(corr_spell ~ 1, data = reg_2_drop10)
anova(model_0, model_3) # F(3,42) = 72.87, p < .001


########### Stepwise Regression ##################

# Model the data 

model0 <- lm(corr_spell ~ 1, data = reg_2_drop10)
model1 <- lm(corr_spell ~ age + std_RA + std_SPELL, data = reg_2_drop10)

# Stepwise-forwards - when you start with the null model and predictors are added until they don’t explain any more variance

steplimitsf <- step(model0, scope = list(lower = model0, upper = model1),
                    direction = "forward")

summary(steplimitsf)

# Stepwise-backwards -  when you start with the full model and remove predictors until removal starts affecting your model’s predictive ability

steplimitsb <- step(model1, direction = "back")

summary(steplimitsb)

# Stepwise using both forwards and backwards procedures

steplimitboth <- step(model0, scope = list(upper = model1), direction = "both")

check_model(steplimitboth)
summary(steplimitboth)

# Entering predictors based on their p-values (olsrr package)

pmodel <- ols_step_forward_p(model1)
pmodel




