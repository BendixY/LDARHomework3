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
#There are 101 data points for 6 different variables.
#One column, "word", is saved as characters, while all others are represented as doubles.
#We initially tried to mutate the Syllable count and Letter count into factors to prevent us from treating them like continuous numbers like the ones in the columns for IMAGE, CONCR and ASSOC,
#but reverted that, as it made the initial analysis unecessarily complicated
#we made sure to keep in mind that these two columns should only ever contain integers, so results, like means or later model analysis, which refer to them with decimal points were treated with a grain of salt.



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
#The bar plot supports this finding and shows an obvious peak at a letter count of 6, with the amount tapering off to both sides.
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

ASSOC_SYLL_plot <- ggplot(data = association, aes(x = ASSOC, y = SYLL)) +
  geom_point()
ASSOC_SYLL_plot
#points seem to be more frequent 
ASSOC_LETTERS_plot <- ggplot(data = association, aes(x = ASSOC, y = LETTERS)) +
  geom_point()
ASSOC_LETTERS_plot
#mh yes quite

ASSOC_IMG_plot <- ggplot(data = association, aes(x = ASSOC, y = IMAGE)) +
  geom_point()
ASSOC_IMG_plot
#the higher the image score, the higher the assoc score, on average

ASSOC_CONCR_plot <- ggplot(data = association, aes(x = ASSOC, y = CONCR)) +
  geom_point()
ASSOC_CONCR_plot
#words with very low assoc score tend to have low concr score as well. average assoc score can hav either very high or low concr scores (and some medium concr scores), high assoc score words correlate with high concr scores


# 3.a) Make a linear model with ASSOC as the dependent variable and all other variables as predictors. Start with the maximal model including all interactions and perform manual model selection until you arrive at the final model.
# For each model, display the summary and briefly explain what it means.

max_model <- lm(ASSOC ~ SYLL*LETTERS*logIMG*sqrtCONCR,
                data = association)
summary(max_model)
#The expected value of ASSOC given the baseline of the other variables is 21.6.
#The adjusted R^2 is significantly lower than the multiple R^2, which would indicate overfitting. This makes sense given that we use the max model

drop1(max_model,
      test = "F")
#The Pr(>F) of the 4 way interaction is quite high, which would indicate that it is not contributing much to the model
#So we limit our next model to only 3 way interactions
model02 <- lm(ASSOC ~ (SYLL+LETTERS+logIMG+sqrtCONCR)^3, data = association)
summary(model02)
#Once again the mutliple R^2 and adjusted R^2 are quite far apart and only the intercept is indicated with some amount of significance, so we can keep dropping interactions
drop1(model02, test = "F")
#The Syll:logIMG:sqrtCONCR interaction has the highest PR(>F), meaning it's the next one we drop like its hot
model03 <- update(model02, ~. -SYLL:logIMG:sqrtCONCR)
summary(model03)
#our R^2 values are slowly trending closer. But in terms of significance, there still isnt anything of high interest to report. so we go on droppin
drop1(model03, test = "F")
#This time it's the interaction between SYLL, LETTERS and sqrtCONCR that we will drop
model04 <- update(model03, ~. -SYLL:LETTERS:sqrtCONCR)
summary(model04)
#This didnt really improve anything. we keep droppin, bois
drop1(model04, test = "F")
#all three interactions that pop up are insignifiacnt, so we drop the highest again
model05 <- update(model04, ~. -SYLL:sqrtCONCR)
summary(model05)
#uh-oh the r^2 values seem to be drifting apart again.though the summary now indicates some signifiacne when it comes to the interaction with Letters. We keep dropping1
drop1(model05, test = "F")
#we now drop the interaction between SYLL:LETTERS:logIMG
model06 <- update(model05, ~. -SYLL:LETTERS:logIMG)
summary(model06)
#the R squared values are converging a bit, but none of the coefficients seem significant, so we drop again
drop1(model06, test = "F")
#we drop the interaction between Syll and logIMG now. The SYLL:LETTERS interaction also has a pretty high Pr(>F) and can hopefully be dropped next
model07 <- update(model06, ~. -SYLL:logIMG)
summary(model07)
#again the R squared valeus are trending closer, but there still isnt any significance within the results, so we drop drop drop
drop1(model07, test = "F")
#SYLL:LETTERS time to go. That words with more letters tend to have more syllables is straightforward anyways, so having a signifiacnt interaction there would have been very confusing
model08 <- update(model07, ~. -SYLL:LETTERS)
summary(model08)
#no big impact to the R-squared values, still no significance in any coefficients
drop1(model08, test = "F")
#SYLL be gone
model09 <- update(model08, ~. -SYLL)
summary(model09)
#r2 moving closer, we now have 2 coeficcients that R rates with a . in significance. not high, but we might be gettign somewhere now...
drop1(model09, test = "F")
#this 3 way interaction has one of the lowest Pr(>F) we dropped yet, but we do what we gotta do
model10 <- update(model09, ~. -LETTERS:logIMG:sqrtCONCR)
summary(model10)
#wow now we have a * at the intercept but not really anywhere else. So we keep going
drop1(model10, test = "F")
#LETTERS:logIMG has incredibly high Pr(>F), so away it goes
model11 <- update(model10, ~. -LETTERS:logIMG)
summary(model11)
#some new significance when considering letters now. but still all very low, so we gogogo again

# 3.b) Plot the effects of the final model and give a brief summary of what the plot shows.


# 3.c) Plot the residuals of the final model. Is there a discernible pattern or not? Are they (more or less) normally distributed?


