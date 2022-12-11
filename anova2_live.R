# Loading the packages ----

library(tidyverse) # Load the tidyverse packages
library(afex) # ANOVA functions
library(emmeans) # Needed for pairwise comparisons
library(ggplot2)

# Challenge 1 - water/ espresso data set ----
# Importing the data

my_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/cond.csv")
head(my_data)

# But we already did this in the lecture??? 

# Challenge 2  - RM data from ANOVA 1 ---- 
#Import Data
rm_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/11_glm_anova_pt1/master/data/rm_data.csv")
head(rm_data)

# Data Tidy
colnames(rm_data) <- c("participant", "condition", "rt")

rm_data <- rm_data %>% 
  mutate(condition = tolower(condition)) %>%
  mutate(condition = factor(condition))

head(rm_data)


rm_data <- rm_data %>%
  mutate(condition = fct_relevel(condition,
                                 c("very easy", "easy", "hard", "very hard")))

contrasts(rm_data$condition)

# Building the ANOCA as Regression 

rm_model <- lm(rt ~ condition, data = rm_data)

rm_model

# Response Time = Intercept + β1(Easy) + β2(Hard) + β3(Very Hard)

very_easy = 1.19975 
very_easy
easy = 1.199975 + 0.02846 * 1 + 0.19174 * 0 + 0.67147 * 0
easy # 1.228435
hard = 1.199975 + 0.02846 * 0 + 0.19174 * 1 + 0.67147 * 0
hard # 1.391715
very_hard = 1.199975 + 0.02846 * 0 + 0.19174 * 0 + 0.67147 * 1
very_hard # 1.871445







# Challenge 3 - Tidy a dataset ----

# Import data

library(tidytuesdayR)

dr_who_data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv")
head(dr_who_data)


worldcup_data <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv")
head(worldcup_data)

str(worldcup_data)
  


# Interested in looking at goals scored over time - i.e., over the years the competition has run 

library(dygraphs)
library(tidyverse)
library(lubridate)


goals_data <- worldcup_data %>% 
  select(year, goals_scored)

head(goals_data)



# Data visualisations

ggplot(data = goals_data, aes(x = year, y = goals_scored)) +
  geom_point() +
  geom_smooth()




