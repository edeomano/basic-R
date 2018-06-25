############################################
# 6b Statistical Analysis
# Look
# Summarise
# Model
# Predict
# Check
############################################
rm(list=ls())

# load libraries and set theme for plots
library(tidyverse)
library(pander)
library(reshape2)
theme_set(theme_bw())



############################################
# ANOVA
# - compare multiple groups
############################################
str(iris)

# 6b.1 look
ggplot(iris, aes(Species, Petal.Length)) +
  geom_boxplot() + 
  ylab("Petal Length")

iris2 <- iris %>%
  melt(id.vars = "Species", variable.name = "Type", value.name = "Measurement")
head(iris2)
ggplot(iris2, aes(Species, Measurement, fill = Type)) +
  geom_boxplot() + 
  facet_wrap(~ Type) +
  ylab("Petal Length")


############################################
# 6b.2 summarise
summary(iris)
table(iris$Species)
table(iris$Species, )

iris %>%
  group_by(Species) %>%
  summarise(avgSepal.Length = mean(Sepal.Length), 
            avgSepal.Width = mean(Sepal.Width),
            avgPetal.Length = mean(Petal.Length),
            avgPetal.Width = mean(Petal.Width),
            n = n())



############################################
# 6b.3 model
# - significant differences detected
model_iris <- aov(Petal.Length ~ Species, data = iris)
summary(model_iris)

# or
model2 <- lm(Petal.Length ~ Species, data = iris)
anova(model2)

summary(model2)
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)        1.46200    0.06086   24.02   <2e-16 ***
#  Speciesversicolor  2.79800    0.08607   32.51   <2e-16 ***
#  Speciesvirginica   4.09000    0.08607   47.52   <2e-16 ***

# gives cumulative estimates
# Intercept equals the estimate for setosa
# Speciesversicolor is 2.79800 more than setosa
# Speciesvirginica is 4.09000 more than setosa


############################################
# 6b.4 do post-hoc tests
# 6b.4.1 using Tukey
TukeyHSD(model_iris)
# or
library(agricolae)
h <- HSD.test(model_iris, "Species")
h


# 6b.4.2 using all pairwise comparisons
# using airquality data
# Month is coded as numbers
# if you use as.factor(Month) in the model,
# the lm object it will create will not work in HSD.test
# create another variable as a factor
?pairwise.t.test
pairwise.t.test(airquality$Ozone, as.factor(airquality$Month))

str(airquality)
air <- airquality %>% mutate(fMonth = factor(Month))
air_lm <- lm(Ozone ~ fMonth, data = air)
air_lm
anova(air_lm)
h_air <- HSD.test(air_lm, "fMonth")
h_air


# another tool for multiple comparisons
library(multcomp)
air_lm <- lm(Ozone ~ factor(Month), data = airquality)
air_glht <- glht(air_lm, linfct = mcp(`factor(Month)` = "Tukey"))
air_glht
plot(confint(air_glht))   # plot the confidence intervals




############################################
# 6b.5 data with blocking
# 6b.5.1 turkey data
# - fully factorial experiment
# - 48 turkeys
# - 2 housing types x 3 diets x 8 blocks
# - sex, initial weight and weight were recorded
# - weight is the variable of interest
############################################
turkey <- read.csv("data/turkey.csv")
turkey$block <- as.factor(turkey$block)
turkey$diet <- as.factor(turkey$diet)
turkey$sex <- factor(turkey$sex, labels = c("Male", "Female"))
str(turkey)

# housing is significant
model3 <- aov(wtgain ~ housing, data = turkey)
summary(model3)

# housing is still significant but block is NS
model4 <- aov(wtgain ~ block + housing, data = turkey)
summary(model4)

# housing is still significant but block and sex is NS
model5a <- aov(wtgain ~ block + housing + sex, data = turkey)
summary(model5a)
model5b <- aov(wtgain ~ block + housing*sex, data=turkey)
summary(model5b)


# 6b.5.2 cars data
# use cylinders as the blocking factor
summary(aov(mpg ~ factor(vs), data = mtcars))
summary(aov(mpg ~ factor(cyl) + factor(vs), data = mtcars))




############################################
# 6b.6 REPEATED MEASURES
# - 5 people
# - 3 levels of Valence
# - variable of interest: how many word can
#   a person recall
#
# Subject/Valence
# - valence is nested within subject
#
# Error(Subject/Valence)
# - specify error strata
# - fit appropriate model within each error
#   stratum
# - Subject and Valence set as random effects
############################################
valence <- read.csv("data/valance.csv")
val.aov <- aov(Recall ~ Valence + Error(Subject/Valence), valence)
summary(val.aov)


val.aov2 <- aov(Recall ~ Valence + (Subject/Valence), valence)
summary(val.aov2)


############################################
# 6b.7 LOGISTIC REGRESSION
# turtle data
# - length, width and height measured
# - determine gender using L, W and H
############################################
turtle <- read.csv("data/turtle.csv")
head(turtle)

ggplot(data = turtle, aes(x = Length, y = Height, shape = Sex, colour=Sex)) + 
  geom_point(size=2.5)

# width is NS
model_turtle <- glm(Sex ~ Length + Width + Height,
                    data = turtle, 
                    family = "binomial")
summary(model_turtle)

# remove width
model_turtle <- glm(Sex ~ Length + Height,
                    data = turtle, 
                    family = "binomial")
summary(model_turtle)

# interpret
# log(p/(1+p)) = intercept + B1*Length + B2*Height
# to get the p
model_turtle$coefficients
# (Intercept)      Length      Height 
# -23.8158108  -0.4970195   1.8868415

predict(model_turtle, type = "response")[1:5]


# classify a new turtle
newdata <- data.frame(Length = 135, Height = 50)
predict(model_turtle, newdata, type="response")

newdata2 <- data.frame(Length = 135, Height = 46)
predict(model_turtle, newdata2, type="response")




############################################
# 6b.8 CHECK ANOVA ASSUMPTIONS
############################################
# NS p-value means the residuals are normally
# distributed
shapiro.test(model_iris$residuals)


# looking at the raw data
ggplot(data=iris, aes(x=Petal.Length, fill=Species)) + 
  geom_histogram(col="black") + 
  facet_wrap(~Species, scales = "free_x")


# try transformation
model_irisLog <- aov(log(Petal.Length) ~ Species, data = iris)
summary(model_irisLog)
shapiro.test(model_irisLog$residuals)

plot(model_iris)


# Equal variances of the groups
# using the raw data
# p-value is highly significant so
# the variances are not equal
bartlett.test(Petal.Length ~ Species, data = iris)



# Independence of errors
# - subject knowledge
# -- Pseudo replication
# -- Repeated measurements
#    - correlated measurements
#    - ether in time or space





############################################
# MODEL SELECTION
############################################
library(MASS)
full_model <- lm(mpg ~ ., data = mtcars)
reduced_model <- stepAIC(full_model)

# Non-significant p-value means they give the
# same information
anova(full_model, reduced_model)




