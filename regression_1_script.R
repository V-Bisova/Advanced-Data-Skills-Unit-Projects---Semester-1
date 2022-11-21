# Installing packages and loading 

install.packages("Hmisc")
install.packages("performance")

library(tidyverse)
library(Hmisc)
library(performance)

# Importing the data 

crime <- read_csv("https://raw.githubusercontent.com/ajstewartlang/09_glm_regression_pt1/master/data/crime_dataset.csv")

head(crime)

# Tidying the data 
# Splitting the City, State column into two new columns called City and State
# Using the separate() function
# We’ll also rename the columns to change the name of the “index_nsa”
# column to “House_price” and get rid of the space in the “Violent Crimes” heading.

crime_tidied <- crime %>% 
  separate(col = "City, State", into = c("City", "State")) %>% 
  rename(House_price = index_nsa) %>% 
  rename(Violent_Crimes = "Violent Crimes")

head(crime_tidied)

# Plot the data - scatterplot 

scatterplot <- crime_tidied %>% 
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "Violent Crimes")

scatterplot

# Calculating Pearson's r - r = .81, p < .001, so 64% of the variance in our
# Violent_Crimes variable is explained by our Population size variable

rcorr(crime_tidied$Population, crime_tidied$Violent_Crimes)


# From the plot we may conclude that the r/ship is being overly influenced by crime
# in a small number of very large cities (top right of the plot above). 
# Let's exclude cities with populations > 2,000,000

crime_filtered <- filter(crime_tidied, Population < 2000000)

# Redoing the scatterplot As there are still likely to be quite a lot of points
# (and thus overplotting with many points appearing roughly in the same place),
# we can set the alpha parameter to be < 1 in the geom_point() line of code.
# This parameter corresponds to the translucency of each point

scatterplot_filtered <- crime_filtered %>% 
  ggplot(aes(x = Population, y = Violent_Crimes)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "Violent Crimes")

scatterplot_filtered

# Calculating Pearson's r with the filtered data - r = .69, p < .001

rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)


# Building a linear model (only focusing on year 2015 so observations = independent)

crime_filtered <- filter(crime_filtered, Year == 2015)

# Building another plot - using geom_text() to plot the City names and set the
# check_overlap parameter to TRUE to ensure the labels don't overlap 

scatterplot_3 <- crime_filtered %>% 
  ggplot(aes(x = Population, y = Violent_Crimes, label = City)) +
  geom_point() +
  geom_text(nudge_y = 500, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 1800000) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population", 
       y = "Violent Crimes")

scatterplot_3

# Working out Pearson's r - r = .65, p < .001

rcorr(crime_filtered$Population, crime_filtered$Violent_Crimes)


# Model the data - predicting how violent crime rate is predicted by population size
# Building two models: model1 = using the mean of out outcome variable as predictor
# model2 = using the Population size to predict Violent Crimes outcome

model1 <- lm(Violent_Crimes ~ 1, data = crime_filtered)
model2 <- lm(Violent_Crimes ~ Population, data = crime_filtered)

# Checking Assumptions

check_model(model2)

# Using the anova() function to see if our model with Population as predictor 
# is better than the one using just the mean 

anova(model1, model2)

# Getting the parameter estimates of model2 - Interpreting our model

summary(model2)

### Population size & robberies in 2015 - same relationship? ###

# Filter data

crime_filtered_2 <- filter(crime_tidied, Population < 2000000, Year == 2015) 

# Plots

scatterplot_rob <- crime_filtered_2 %>% 
  ggplot(aes(x = Population, y = Robberies)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "Robberies")

scatterplot_rob2 <- crime_filtered_2 %>% 
  ggplot(aes(x = Population, y = Robberies, label = City)) +
  geom_point() +
  geom_text(nudge_y = 500, check_overlap = TRUE) +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(0, 1800000) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population", 
       y = "Robberies")

scatterplot_rob
scatterplot_rob2

# Pearson's r - r = .63, p < .001

rcorr(crime_filtered_2$Population, crime_filtered_2$Robberies)

# Building the mean model and our model (model3 = mean model, model4 = our regression model)

model3 <- lm(Robberies ~ 1, data = crime_filtered_2)
model4 <- lm(Robberies ~ Population, data = crime_filtered_2)

# Checking assumptions 

check_model(model4)

# F-test - p < .001

anova(model3, model4)

# Interpreting our model 

summary(model4)

### Are house prices predicted by the number of violent crimes in 2015? ###

scatterplot_hp <- crime_filtered %>% 
  ggplot(aes(x = House_price, y = Violent_Crimes)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "House Price",
       y = "Violent Crimes")

scatterplot_hp

# r = -.18, p = 0.2796 

rcorr(crime_filtered$House_price, crime_filtered$Violent_Crimes)

model5 <- lm(Violent_Crimes ~ 1, data = crime_filtered)
model6 <- lm(Violent_Crimes ~ House_price, data = crime_filtered)

check_model(model6)

anova(model5, model6)

summary(model6)

### Are house prices predicted by population size in 2015? ###

scatterplot_pop_hp <- crime_filtered %>% 
  ggplot(aes(x = Population, y = House_price)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "House Price")

scatterplot_pop_hp

# r = .14, p = .372
rcorr(crime_filtered$House_price, crime_filtered$Population)

model7 <- lm(House_price ~ 1, data = crime_filtered)
model8 <- lm(House_price ~ Population, data = crime_filtered)

check_model(model8)

anova(model7, model8)

summary(model8)

### What happens if you remove method = “lm” from geom_smooth()? ###

scatterplot_test <- crime_filtered %>% 
  ggplot(aes(x = Population, y = House_price)) +
  geom_point(alpha = .25) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(x = "Population",
       y = "House Price")

scatterplot_test

### What house price would you predict for a city with a population of 1 million? ###

### b = 0.00002314, a = 204.5, x = 1000000 --> 
# y = bx + a = (0.00002314)1000000 + 204.5 = 227.64

