# Homework 3 - Linear Regression
# name(s): Benedict W. & Catalina L.


# load all packages here:
library(tidyverse)
library(here)
library(effects)
library(rgl)
library(car)



# 1) Load the data: associations.csv and inspect the data frame - how many data points and variables are there? What types of variables are there?
association <- read_csv(here("associations.csv")) #%>% 
  #mutate(SYLL = as.factor(SYLL)) %>% 
  #mutate(LETTERS = as.factor(LETTERS))
glimpse(association)


# 2.a) Explore all the variables in your data set: provide summary statistics (measures of central tendency) and plot each one of them (except WORD). Describe what you see in the plots and the summary statistics. Is there anything striking? Are some of the variables very skewed and need to be transformed? If so, try some of the transformations we learned about and see if that helps. Only use them if they really improve things a lot.
summary(association$SYLL)
plot(association$SYLL)
mean(association$SYLL)

SYLL_stat <- ggplot(association,
                    aes(x = SYLL)) +
  geom_bar()
SYLL_stat
#reasons

summary(association$LETTERS)
plot(association$LETTERS)

LETT_stat <- ggplot(association,
                    aes(x = LETTERS)) +
  geom_bar()
LETT_stat
#reasons2

summary(association$IMAGE)
plot(association$IMAGE)

IMAGE_stat <- ggplot(association,
                    aes(x = IMAGE)) +
  geom_histogram(binwidth = 0.1)
IMAGE_stat
#reason3

summary(association$CONCR)
plot(association$CONCR)

CONCR_stat <- ggplot(association,
                    aes(x = CONCR)) +
  geom_histogram(binwidth = 0.1)
CONCR_stat

#reason4

summary(association$ASSOC)
plot(association$ASSOC)

ASSOC_stat <- ggplot(association,
                    aes(x = ASSOC)) +
  geom_histogram(binwidth = 0.1)
ASSOC_stat
#omgsomanyreasons


# 2.b) Plot the dependent variable (ASSOC) with each of the predictors (all the other variables) and give a brief explanation of what you see.



# 3.a) Make a linear model with ASSOC as the dependent variable and all other variables as predictors. Start with the maximal model including all interactions and perform manual model selection until you arrive at the final model.
# For each model, display the summary and briefly explain what it means.


# 3.b) Plot the effects of the final model and give a brief summary of what the plot shows.


# 3.c) Plot the residuals of the final model. Is there a discernible pattern or not? Are they (more or less) normally distributed?


