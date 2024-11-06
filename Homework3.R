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
SYLL_stat <- ggplot(association,
                    aes(x = SYLL)) +
  geom_bar()
SYLL_stat
SYLL_qq <- ggplot(association, aes(sample = SYLL)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
SYLL_qq
#The mean syllable count of words in the dataset is 2,158. Together with the plot it becomes clear that most words in the data have 2 syllables, but tend to have slightly more.
#That the 3rd quartile is at 3 syllables indicates that there are very few words with 4 or more syllables in the dataset, which again is supported by the visualisation.
#The QQ plot isn't very telling not all to useful here, as we only have integer values. We could have transformed this column, but decided against it as it does not seem useful to turn amount of Syllables (and letters for that matter) into continuous values.


summary(association$LETTERS)
LETT_stat <- ggplot(association,
                    aes(x = LETTERS)) +
  geom_bar()
LETT_stat
LETT_qq <- ggplot(association, aes(sample = LETTERS)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
LETT_qq
#The summary states the mean letter count of words in the data is 6, with the mean being only slightly higher at 4.65. 
#The bar plot supports this finding and shows an abvious peak at a lettercount of 6, with the amount tapering off to both sides.
#This also makes it look vaguely normal, but the QQ test would somewhat disagree. We think, for the same reason as stated above, that it wasn't useful to transform it.


summary(association$IMAGE)
plot(association$IMAGE)

IMAGE_stat <- ggplot(association,
                    aes(x = IMAGE)) +
  geom_histogram(binwidth = 0.1)
IMAGE_stat
hist(association$IMAGE)
#This doesnt seem very normal, so lets apply a log to the IMAGE column
association <- association %>% 
  mutate(logIMG = log10(IMAGE))
hist(association$logIMG)
#already looks a lil better
#we now test for normalityness
IMAGE_qq <- ggplot(association, aes(sample = logIMG)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
IMAGE_qq
#this looks to be reasonably normal, though the higher values definitely taper off
#reason3

summary(association$CONCR)
hist(association$CONCR)

CONCR_stat <- ggplot(association,
                    aes(x = CONCR)) +
  geom_histogram(binwidth = 0.1)
CONCR_stat
#Here we tried a log transformation, but that didnt seem to improve it, so we try something else idk what tho
#lessss try square root transformation

association <- association %>% 
  mutate(sqrtCONCR = sqrt(CONCR))
hist(association$sqrtCONCR)
#this looks very slightly better/more normal
#lets test this thooooooooo
CONCR_qq <- ggplot(association, aes(sample = sqrtCONCR)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
CONCR_qq

#reason4

summary(association$ASSOC)
hist(association$ASSOC)

ASSOC_stat <- ggplot(association,
                    aes(x = ASSOC)) +
  geom_histogram(binwidth = 0.1)
ASSOC_stat
#this is the only plot with continuous values that looked inherently "normal"
ASSOC_qq <- ggplot(association, aes(sample = ASSOC)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
ASSOC_qq
#this test confirms that the data truly is quite normal

# 2.b) Plot the dependent variable (ASSOC) with each of the predictors (all the other variables) and give a brief explanation of what you see.



# 3.a) Make a linear model with ASSOC as the dependent variable and all other variables as predictors. Start with the maximal model including all interactions and perform manual model selection until you arrive at the final model.
# For each model, display the summary and briefly explain what it means.


# 3.b) Plot the effects of the final model and give a brief summary of what the plot shows.


# 3.c) Plot the residuals of the final model. Is there a discernible pattern or not? Are they (more or less) normally distributed?


