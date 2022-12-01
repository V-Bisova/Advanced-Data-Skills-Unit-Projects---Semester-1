# Getting started - loading packages, reading in data, exploring data set 

# Loading initial packages ----------------------------------------------------

install.packages("here")
install.packages("kableExtra")
install.packages("gganimate")
install.packages("ggthemes")
install.packages("gapminder")
install.packages("knitr")

library(tidyverse)
library(patchwork)
library(Hmisc)
library(ggridges)
library(ggplot2)
library(gganimate)
library(ggthemes)

# Reading in the data set -----------------------------------------------------

A1 <- read_csv("assignment_1_data.csv")

#Exploring the data ----------------------------------------------------------- 

str(A1)
head(A1)

# DATA WRANGLING --------------------------------------------------------------

# Tidying data - reshaping the data from wide to long format (using the pivot_longer() function)
tidy_A1 <- A1 %>% 
  pivot_longer(cols = c(Condition1, Condition2, Condition3, Condition4),
               names_to = "Condition", 
               values_to = "RT")

View(tidy_A1)

# Recoding variables - giving conditions meaningful names ----------------------

head(tidy_A1)

tidy_data <- tidy_A1 %>% 
  mutate(Condition = recode(Condition, 
                            "Condition1" = "Expect Number_Target Number",
                            "Condition2" = "Expect Number_Target Letter",
                            "Condition3" = "Expect Letter_Target Number",
                            "Condition4" = "Expect Letter_Target Letter")) %>% 
  mutate(Condition = factor(Condition))


# The first mutate() function renames the conditions with meaninful labels. 
# Separating the Condition column into two new columns for factor "Expectation"
# and factor "Target". The second mutate() function transforms the two new variables 
# (Expectation, Target) into factors

tidy_recoded <- tidy_A1 %>% 
  mutate(Condition = recode(Condition, 
                            "Condition1" = "Expect Number_Target Number",
                            "Condition2" = "Expect Number_Target Letter",
                            "Condition3" = "Expect Letter_Target Number",
                            "Condition4" = "Expect Letter_Target Letter")) %>%
  separate(col = "Condition", into = c("Expectation", "Target"), sep = "_") %>% 
  mutate(Expectation = factor(Expectation), Target = factor(Target))

View(tidy_recoded)

# GENERATING SUMMARY STATISTICS -----------------------------------------------

# Calculating mean, standard deviation and count of observations for each of 
# the four conditions. Ensuring missing values are disregarded with !is.na() function.
# Also arranging in order of smallest mean RT to largest mean RT using the arrange() function 

# Will need to covert to "summary_data" during visualisations as otherwise it does not appear in markdown
summary_data <- tidy_recoded %>% 
  group_by(Expectation, Target) %>% 
  filter(!is.na(RT)) %>% 
  summarise(mean_RT = mean(RT), sd_RT = sd(RT), number = n()) %>% 
  arrange(mean_RT)

install.packages("papeR", repos = "http://cran.us.r-project.org" )
library(papeR)

summarize(tidy_recoded, type = "numeric", group = "Expectation", test = FALSE)


# Additional summary statistics, including - min, max, median 

summary_data_2 <- tidy_recoded %>% 
  group_by(Expectation, Target) %>%
  filter(!is.na(RT)) %>% 
  summarise(median_RT = median(RT), min_RT = min(RT), max_RT = max(RT)) %>% 
  arrange(median_RT)

View(summary_data_2)
View(summary_data)

# VISUALISING THE DATA --------------------------------------------------------

fig1 <- tidy_recoded %>%
  ggplot(aes(x = Expectation, y = RT, 
             color = Target)) +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  geom_boxplot(alpha = .5, show.legend = FALSE) +
  theme_minimal() +
  theme(text = element_text(size = 13), legend.position = "none") +
  labs(x = "Expected Stimulus", y = "Reaction Time (ms)",
       color = "Target Stimulus")  +
  scale_x_discrete(labels = c('Expected Letter', 'Expected Number'))
fig1

# Plotly - Installing package ----------------------------------------------------------
# https://statisticsglobe.com/plotly-line-plot-r

install.packages("plotly", repos = "http://cran.us.r-project.org")
library(plotly)


# Boxplot 

plot_ly(
  data = tidy_recoded,
  y = ~RT,
  x = ~Expectation,
  type = "box", 
  boxpoints = "all",
  boxmean = TRUE,
  notched = TRUE, 
  color = ~Target,
  showlegend = TRUE
)


# Bar Plot - will have to get rid of this

plot_ly(
  data = tidy_recoded,
  x = ~Expectation,
  y = ~RT,
  color = ~Target,
  type = "bar"
)

# Properly constructed bar plot (using summary data - mean RT)

plot_ly(
  data = summary_data,
  x = ~Expectation,
  y = ~mean_RT,
  type = "bar",
  color = ~Target
)

# Stacked bar plot (Plotly)

plot_ly(
  data = summary_data,
  x = ~Expectation,
  y = ~mean_RT,
  color = ~Target,
  type = "bar"
) %>% 
  layout(barmode = "stack")



