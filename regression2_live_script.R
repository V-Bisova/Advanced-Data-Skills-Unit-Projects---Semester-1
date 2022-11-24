
# Loading packages

library(tidyverse)
library(Hmisc)
library(performance)
library(MASS) 
library(car) 
library(olsrr)
library(ggcorrplot)
library(gridExtra)

# Importing data

coffee_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

View(coffee_ratings)

# Predictors: aroma, aftertaste, acidity, body, balance
# sweetness, uniformity, clean_cup, and moisture
# Outcome - flavour

coffee_filtered <- dplyr::select(coffee_ratings, aroma, aftertaste, acidity,
                                 body, balance, sweetness, uniformity, clean_cup,
                                 moisture, flavor) %>% 
  filter(flavor != 0 & aroma != 0)

View(coffee_filtered)

# Scatterplot - aroma 

s1 <- ggplot(coffee_filtered, aes(x = aroma, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

# Scatterplot - aftertaste 

s2 <- ggplot(coffee_filtered, aes(x = aftertaste, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

# Scatterplot - acidity 

s3 <- ggplot(coffee_filtered, aes(x = acidity, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

# Scatterplot - body 

s4 <- ggplot(coffee_filtered, aes(x = body, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))


# Scatterplot - balance

s5 <- ggplot(coffee_filtered, aes(x = balance, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))


# Scatterplot - sweetness

s6 <- ggplot(coffee_filtered, aes(x = sweetness, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

# Scatterplot - uniformity 

s7 <- ggplot(coffee_filtered, aes(x = uniformity, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

# Scatterplot - clean_cup 

s8 <- ggplot(coffee_filtered, aes(x = clean_cup, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))


# Scatterplot - moisture 

s9 <- ggplot(coffee_filtered, aes(x = moisture, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13))

# Plot the scatterplots together 

all_scatterplots <- gridExtra::grid.arrange(s1, s2, s3, s4, s5, s6, s7, s8, s9, ncol=3)



### Model the data 

model0 <- lm(flavor ~ 1, data = coffee_filtered)
model1 <- lm(flavor ~ aroma + aftertaste + acidity + body + balance + sweetness 
             + uniformity + moisture + clean_cup, data = coffee_filtered)

# Comparing the models

anova(model0, model1)

check_model(model1)
cooks.distance(lm(model1))


# Stepwise regression (backwards)

steplimitsb <- step(model1, direction = "back")

summary(steplimitsb)

# Stepwise regression (forwards)

steplimitsf <- step(model0, scope = list(lower = model0, upper = model1),
                    direction = "forward")



# Additional Challenge 
# Use the predict() function to predict what the flavour rating would be for the
# following values of the predictors aroma = 8, acidity = 7, body = 5, 
# balance = 9, uniformity = 6, clean_cup = 9

### My way - added values for aftertaste (6), sweetness (6), and moisture (6)

# Create a new model first that only includes aroma , acidity, body, 
# balance uniformity , clean_cup as predictors 

model2 <- lm(flavor ~ aroma + acidity + body + balance + uniformity + clean_cup,
             data = coffee_filtered)

new <- data.frame(aroma = c(8), acidity = c(7), body = c(5),
                  balance = c(9), uniformity = c(6), clean_cup = c(9))

predict(model2, newdata = new)

# y = 7.37003 (7.37 to 2dp)

### Duncan's way ### 

new2 <- tibble("aroma" = c(8), "acidity" = c(7), "body" = c(5), "balance" = c(9),
               "uniformity" = c(6), "clean_cup" = c(9))











