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
#but reverted that, as it made the initial analysis unnecessarily complicated
#we made sure to keep in mind that these two columns should only ever contain integers, so results, like means or later model analysis, which refer to them with decimal points were treated with a grain of salt.



# 2.a) Explore all the variables in your data set: provide summary statistics (measures of central tendency) and plot each one of them (except WORD). Describe what you see in the plots and the summary statistics. Is there anything striking? Are some of the variables very skewed and need to be transformed? If so, try some of the transformations we learned about and see if that helps. Only use them if they really improve things a lot.
summary(association$SYLL)
SYLL_stat <- ggplot(association,
                    aes(x = SYLL)) +
  geom_bar()
SYLL_stat
SYLL_qq <- ggplot(association,
                  aes(sample = SYLL)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
SYLL_qq
#The mean syllable count of words in the dataset is 2,158. Together with the plot it becomes clear that most words in the data have 2 syllables, but tend to have slightly more.
#That the 3rd quartile is at 3 syllables indicates that there are very few words with 4 or more syllables in the dataset, which again is supported by the visualization. 
#The QQ plot isn't very telling and not all to useful here, as we only have integer values. We could have transformed this column, but decided against it as it does not seem useful to turn amount of Syllables (and letters for that matter) into continuous values.


summary(association$LETTERS)
LETT_stat <- ggplot(association,
                    aes(x = LETTERS)) +
  geom_bar()
LETT_stat
LETT_qq <- ggplot(association,
                  aes(sample = LETTERS)) +
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
#This doesn't seem very normal, so we applied a log to the IMAGE column
association <- association %>% 
  mutate(logIMG = log10(IMAGE))
hist(association$logIMG)
#already looks a better
#we now test for normality
IMAGE_qq <- ggplot(association,
                   aes(sample = logIMG)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles",
       y = "Sample Quantiles")
IMAGE_qq
#this looks to be reasonably normal, though the higher values definitely taper off

#reason3

summary(association$CONCR)
hist(association$CONCR)

CONCR_stat <- ggplot(association,
                    aes(x = CONCR)) +
  geom_histogram(binwidth = 0.1)
CONCR_stat
#Here we tried a log transformation, but that didn't seem to improve it, so we tried something else.
#trying the square root transformation

association <- association %>% 
  mutate(sqrtCONCR = sqrt(CONCR))
hist(association$sqrtCONCR)
#this looks more normal
#So we tested it.
CONCR_qq <- ggplot(association,
                   aes(sample = sqrtCONCR)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles",
       y = "Sample Quantiles")
CONCR_qq

#The graph below shows that the 2nd and 3rd quartiles are both positiv which is also true for the theoretical quartiles.
#same as above the data is not very normal.
summary(association$ASSOC)
hist(association$ASSOC)
#In the histogram the association around 5 to 6 has the highest frequency. 
ASSOC_stat <- ggplot(association,
                    aes(x = ASSOC)) +
  geom_histogram(binwidth = 0.1)
ASSOC_stat
#this is the only plot with continuous values that looked inherently "normal" and well readable.
ASSOC_qq <- ggplot(association,
                   aes(sample = ASSOC)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles",
       y = "Sample Quantiles")
ASSOC_qq
#this test confirms that the data truly is quite normal.

# 2.b) Plot the dependent variable (ASSOC) with each of the predictors (all the other variables) and give a brief explanation of what you see.

ASSOC_SYLL_plot <- ggplot(data = association, aes(x = ASSOC, y = SYLL)) +
  geom_point()
ASSOC_SYLL_plot
#The points seem to be more frequent  around the variable 6. 
#As aready discussed, words with more than 3 syllables are less likely to often be associated with something.
ASSOC_LETTERS_plot <- ggplot(data = association, aes(x = ASSOC, y = LETTERS)) +
  geom_point()
ASSOC_LETTERS_plot
#The distrubition of the variable letters is similar to the one with syllables.

ASSOC_IMG_plot <- ggplot(data = association, aes(x = ASSOC, y = IMAGE)) +
  geom_point()
ASSOC_IMG_plot
#the higher the image score, the higher the assoc score, on average.

ASSOC_CONCR_plot <- ggplot(data = association, aes(x = ASSOC, y = CONCR)) +
  geom_point()
ASSOC_CONCR_plot
#words with very low assoc score tend to have low concr score as well. Average assoc score can have either very high or low concr scores (and some medium concr scores), high assoc score words correlate with high concr scores.


# 3.a) Make a linear model with ASSOC as the dependent variable and all other variables as predictors. Start with the maximal model including all interactions and perform manual model selection until you arrive at the final model.
# For each model, display the summary and briefly explain what it means.

max_model <- lm(ASSOC ~ SYLL*LETTERS*logIMG*sqrtCONCR,
                data = association)
summary(max_model)
#The expected value of ASSOC given the baseline of the other variables is 21.6.
#The adjusted R^2 is significantly lower than the multiple R^2, which would indicate overfitting. This makes sense given that we use the max model.

drop1(max_model,
      test = "F")
#The Pr(>F) of the 4 way interaction is quite high, which would indicate that it is not contributing much to the model.
#We limit our next model to only 3 way interactions.
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
drop1(model11, test = "F")
#we drop logIMG:sqrt:CONCR. this is a bit surprising, as this is where we thought could have been a decently significant interaction ¯\_(ツ)_/¯
model12 <- update(model11, ~. -logIMG:sqrtCONCR)
summary(model12)
#R2 are getting cloooser. we have high significance in the intercept ans with logIMG
#We'll see what the drop1 changes.
drop1(model12, test = "F")
#There's no visible significance between the two values. We also don't get an intercept here.
#Howecer, the Pr(>F) value is definetely lower!
model13 <- update(model12, ~. -LETTERS:sqrtCONCR)
summary(model13)
#Same observation here as above, there's significance between the intercept and logIMG.
#As shown in model12 removing the LETTERS takes away the significance.
drop1(model13, test = "F")
#No intrecept but significance in ligIMG.
#Here the Pr(>F) level of logIMG starts getting bigger again.
model14 <- update(model12, ~. -sqrtCONCR)
summary(model14)

drop1(model14, test = "F")
#same result here, significance between the intercept and logIMG.
model15 <- update(model13, ~. -sqrtCONCR)
summary(model15)
#LETTERS is a lot smaller but oh well, not significant..
drop1(model15, test = "F")
#There are stars behind logIMG again :D
model16 <- update(model15, ~. -LETTERS)
summary(model16)

#We reduced the model down to ASSOC ~ logIMG
#The Intercept at 2.6918 means that is the expected value for ASSOC when logIMG is 0.
#For each unit increase in logIMG, the ASSOC value is expected to rise by 5.0149 units.
#The R2 values are very close to each other, implying that it's unlikely there's any overfitting.




# 3.b) Plot the effects of the final model and give a brief summary of what the plot shows.

association$predicted_ASSOC <- predict(model16)
#this adds the predictions for datapoints into our dataset
EffectPlot <- ggplot(association,
                     aes(x = logIMG,
                         y = ASSOC)) +
  geom_point(color = "blue") +
  geom_line(aes(y = predicted_ASSOC),
            color = "red",
            size = 1) + #Here we add the predictions as a line to help the visualisation
  labs(title = "Effect of logIMG on ASSOC",
       x = "logIMG",
       y = "ASSOC")
EffectPlot
#The plot shows us the expected rise of ASSOC for each unit of logIMG
#As no data points go below ~0.26logIMG, we do not get to see the intercept.
#The same applies for values of logIMG > 0.84
#In the range of 0.6<logIMG<0.85 the data points veer off the prediction quite strongly in both directions, but the upwards trend is still apparent.



# 3.c) Plot the residuals of the final model. Is there a discernible pattern or not? Are they (more or less) normally distributed?

association$residuals <- residuals(model16)
#with this we add the residuals back into our data

ggplot(association, aes(x = logIMG, y = residuals)) +
  geom_point(color = "purple") + 
  geom_hline(yintercept = 0, #add horizontal line at y = 0
             color = "black",
             linetype = "dashed") + #for increased visibility
  labs(title = "Residuals of model16",
       x = "logIMG",
       y = "Residuals")
#The residual plot shows clear pattern, though it is bunched up a bit more in the higher values, which corresponds to our analysis of the QQ test of logIMG above.
#we check again for normality by making a QQ test of the residuals themselves

RESI_qq <- ggplot(association,
                  aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Theoretical Quantiles", y = "Sample Quantiles")
RESI_qq
#the values align somewhat neatly with the theoretical quantiles

#an additonal histogram of the residuals might also help
RESI_histo <- ggplot(association, aes(x = residuals(model16))) +
  geom_histogram(binwidth = 0.2) +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Frequency")
RESI_histo
#This obviously isn't perfectly normal, but given our initial data, it's relatively close

shapiro.test(residuals(model16))
#this test can also be used to test for normality. our resulting p-value of 0.2272 is higher than 0.05, which would again suggest that the residuals do not deviate significantly from a normal distribution