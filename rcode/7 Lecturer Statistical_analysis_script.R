
## ANOVA

str(iris)
ggplot(iris, aes(Species, Petal.Length)) + geom_boxplot() + 
  ylab("Petal Length")

##ANOVA

# Two ways:
  
model_iris <- aov(Petal.Length~Species, data=iris)
summary(model_iris)

model2 <- lm(Petal.Length~Species, data=iris)
anova(model2)

##ANOVA | Output

summary(model_iris) #aov version

##ANOVA | Multiple Comparisons
  
TukeyHSD(model_iris)

library(agricolae)
h <- HSD.test(model_iris, "Species")
h

##Multiple `t` tests

pairwise.t.test(airquality$Ozone, as.factor(airquality$Month))


## ANOVA with blocks

# setwd("~/Documents/Work/SRA May 2018/SRA_Training/notes")
turkey <- read.csv("data/turkey.csv")
turkey$block <- as.factor(turkey$block)
turkey$diet <- as.factor(turkey$diet)
turkey$sex <- factor(turkey$sex, labels = c("Male", "Female"))
str(turkey)

##ANOVA with Blocks

model3 <- aov(wtgain~housing, data=turkey)
summary(model3)

model4 <- aov(wtgain~block+housing, data=turkey)
summary(model4)

##ANOVA with Blocks
summary(aov(mpg~factor(vs), data=mtcars))

# Let's "Block" by number of cylinders. 

summary(aov(mpg~factor(cyl)+factor(vs), data=mtcars))

## Interaction effects

model5 <- aov(wtgain~block+housing+sex, data=turkey)
summary(model5)

## ANOVA for repeated measures

valence <- read.csv("data/valance.csv")
val.aov <- aov(Recall~Valence+Error(Subject/Valence), valence)
summary(val.aov)      

##Statistical tests | More complex models

turtle <- read.csv("data/turtle.csv")
head(turtle)

ggplot(data = turtle, aes(x = Length, y = Height, shape = Sex, colour=Sex)) + 
  geom_point(size=2.5)


##Logistic regresssion

model_turtle <- glm(Sex~Length+Width+Height, data = turtle, 
                    family = "binomial")
summary(model_turtle)

##Logistic regresssion | Update our model

model_turtle <- glm(Sex~Length+Height, data = turtle, 
                    family = "binomial")
summary(model_turtle)

##Logistic Regression | Interpreting model

model_turtle$coefficients
predict(model_turtle, type = 'response')[1:5]

##Predicition

newdata <- data.frame(Length = 135, Height = 50)
predict(model_turtle, newdata, type="response")
newdata2 <- data.frame(Length = 135, Height = 46)
predict(model_turtle, newdata2, type="response")

turtle_plot + geom_point(aes(x=135, y=50), size=2.5) + geom_point(aes(x=135, y=46), size=2.5)

##ANOVA Assumptions | Normality of Errors

#From the iris dataset in model_iris
shapiro.test(iris$Petal.Length)

shapiro.test(model_iris$residuals)

##ANOVA Assumptions | Normality of Errors

ggplot(data=iris, aes(x=Petal.Length, fill=Species)) + 
geom_histogram(col="black") + 
facet_wrap(~Species, scales = "free_x")

##ANOVA Assumptions | Normality of Errors

plot(model_iris, which=2)

##ANOVA Assumptions | Equal variance

bartlett.test(Petal.Length~Species, data=iris)
plot(model_iris,which=1)


##Model Selection

library(MASS)
full_model <- lm(mpg~., data=mtcars)
reduced_model <- stepAIC(full_model)


# Compare models with `anova`

#Non-significant p-value means they give the same information
anova(full_model, reduced_model)


## References

# - https://sites.ualberta.ca/~lkgray/uploads/7/3/6/2/7362679/slides_-_anova_assumptions.pdf
# - Notes and Turkey data from Helena Oakey, Biomathematics and Statistics Scotland
# - Valance data: http://www.personality-project.org/r/r.guide.html#withinone
