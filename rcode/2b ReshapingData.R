###############################################################
# SESSION 2 reshape the data
###############################################################
rm(list=ls())
library(reshape2)
library(tidyverse)


# 2.1 load data
wideData <- read_csv("data/wide.csv")

# 2.2 convert to long format, set column names
wideData %<% 
  melt(id.vars = c("Name", "Tx"),
       value.name = "Change",
       variable.name = "Day")

# 2.3 get group summaries and save to a data frame
wideMeans <- wideData %>% 
  melt(id.vars = c("Name", "Tx"),
       value.name = "Change",
       variable.name = "Day") %>%
  group_by(Tx, Day) %>%
  summarise(avgChange = mean(Change))

# 2.4 convert long to wide
wideMeans %>% dcast(Tx ~ Day)

# 2.5 read data with 3 genes, 2 cell types across 3 times
PCR <- read_csv("data/PCR.csv")
PCR

# 2.6 reshape, name variables and split the values for
#     the cell type into cell type and time point
PCR %>%
  melt(id.vars = "Gene",
       value.name = "Ct",
       variable.name = "CellType") %>%
  separate(col = CellType, into = c("CellType", "TimePoint"))

# specify a character to split celltype
PCR %>%
  melt(id.vars = "Gene",
       value.name = "Ct",
       variable.name = "CellType") %>%
  separate(col = CellType,
           into = c("CellType", "TimePoint"),
           sep = "i")


# 2.7 output back to wide format
PCR %>%
  melt(id.vars = "Gene",
       value.name = "Ct",
       variable.name = "CellType") %>%
  separate(col = CellType, into = c("CellType", "TimePoint")) %>%
  dcast(Gene + CellType ~ TimePoint, value.var = "Ct")


###############################################################
# Text manipulation
# . : wild card
# + : any number of characters after that
PCR %>%
  melt(id.vars = "Gene",
       value.name = "Ct",
       variable.name = "CellType") %>%
  mutate(TimePoint = str_extract(CellType, "(0hr|12hr|24hr)"),
         CellType = str_replace(CellType, "_(0hr|12hr|24hr)", ""))
  

