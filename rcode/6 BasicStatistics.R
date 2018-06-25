############################################
# 6 Basic Statistics
############################################
rm(list=ls())

# load libraries and set theme for plots
library(tidyverse)
library(pander)
library(reshape2)
theme_set(theme_bw())


############################################
# 6.1 statistical distributions
?Distributions

randomSamples <- data_frame(
  Normal = rnorm(1000, mean = 1, sd = 1),
  Uniform = runif(1000, min = -0.732, max = 2.732),
  Exponential = rexp(1000, rate = 1),
  Poisson = rpois(1000, lambda = 1)
)
randomSamples


############################################
# 6.2 load data file
# set dose as a factor and set the order of the factor levels
# Note: if you use "Medium" you will get NA when you convert
# it to numeric
toothData <- file.path("data", "toothData.csv") %>%
  read_csv %>%
  mutate(dose = factor(dose, levels = c("Low", "Med", "High")))
toothData
as.numeric(toothData$dose)

# 6.3 plot
toothData %>%
  ggplot(aes(x = dose, y = len, fill = supp)) +
  geom_boxplot()



############################################
# HYPOTHESIS TESTS
# 6.4 do t-test
# - assume that the dataset is normally distributed
# 6.4.1 Ho: mean of length = 0
?t.test
t.test(toothData$len)

# 6.4.2 Ho: mean length of VC = mean length of OJ
# note by default this t-test is for unpaired samples
x <- subset(toothData, supp == "VC")$len
y <- subset(toothData, supp == "OJ")$len
t.test(x, y)
# or
t.test(len ~ supp, data = toothData)


# 6.5 for non-normal data
# - Wilcoxon Rank-Sum Tests aka Mann-Whitney test
wilcox.test(len ~ supp, data = toothData)


# 6.6 Chi-Squared test
pass <- matrix(c(25, 8, 6, 15), nrow = 2)
colnames(pass) <- c("Pass", "Fail")
rownames(pass) <- c("Attended", "Skipped")
pass
chisq.test(pass)


# 6.7 Fisher's Exact Test
# use this when you expect some low counts for some
# categories
fisher.test(pass)




############################################
# 6.8 LINEAR REGRESSION
# by default an intercept term is included
lmTooth <- lm(len ~ supp, data = toothData)
summary(lmTooth)
anova(lmTooth)


lmToothDose <- lm(len ~ supp + dose, data = toothData)
summary(lmToothDose)
anova(lmToothDose)

lmToothInteraction <- lm(len ~ supp + dose + supp:dose, data = toothData)
# or use ^2 to include 2-way interaction, ^3 for 3-way interaction
lmToothInteraction <- lm(len ~ (supp + dose)^2, data = toothData)
# or
lmToothInteraction <- lm(len ~ supp*dose, data = toothData)
summary(lmToothInteraction)
anova(lmToothInteraction)

# compare the models
anova(lmTooth, lmToothDose, lmToothInteraction)


############################################
# 6.9 PRINCIPAL COMPONENTS ANALYSIS
# sample data
# - 50 genes, from 2 types of T cell types
############################################
# 6.9.1 read the data
genes <- file.path("data", "geneExpression.csv") %>%
  read_csv() %>%
  as.data.frame() %>%
  column_to_rownames("X1") %>%
  as.matrix() %>%
  t()
dim(genes)
genes[, 1:10]

# 6.9.2 do pca
?prcomp
# by default data is centered
# data is approximately in the same scale so
# scaling is not needed
pcaGenes <- prcomp(genes)
summary(pcaGenes)
biplot(pcaGenes)
screeplot(pcaGenes)


# 6.9.3 plot
names(pcaGenes)

# co-ordinates for each sample
# useful for checking the data
plot(as.data.frame(pcaGenes$x)[1:3])

# 6.9.4 make a better plot
samples <- data_frame(name = rownames(genes)) %>%
  mutate(CellType = str_extract(name, "(Th|Treg)"),   # look for Th or Treg
         Stim = str_detect(name, "\\+"))              # look for a + and set to TRUE if it's detected
samples

pcaPlot <- pcaGenes$x %>%
  as.data.frame() %>%
  rownames_to_column("name") %>%
  left_join(samples) %>%
  ggplot(aes(x = PC1, y = PC2,
             colour = CellType, shape = Stim,
             label = name)) +
    geom_point(size = 3)
pcaPlot

library(plotly)
ggplotly(pcaPlot)
