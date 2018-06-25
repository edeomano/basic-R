############################################
# Types of Atomic Vectors
############################################
# logical vector
logi_vec <- c(TRUE, FALSE, FALSE)
log_vec

# integer vector
int_vec <- 1:5
str(int_vec)

# double vector
dbl_vec <- c(0.618, 1.414, 2)
dbl_vec

# character vector
char_vec <- c("blue", "red", "green")
char_vec


############################################
# Properties of vectors
############################################
# typeof is preferable to class
typeof(char_vec)
typeof(dbl_vec)
typeof(int_vec)
typeof(logi_vec)
length(logi_vec)
attributes(euro)  # euro is a built-in vector

############################################
# Combine vectors
############################################
# 2 vectors with different data types
# - combining logical and integer
# - logical values are coerced into integers
new_vec <- c(logi_vec, int_vec)
typeof(new_vec)
new_vec


############################################
# coercion functions
############################################
new_vec <- c(as.character(logi_vec), as.character(int_vec))
typeof(new_vec)
new_vec



############################################
# subsetting vectors
# brackets
# double brackets: can be used to return
#   single elements only
############################################
char_vec[2:3]

head(euro)
tail(euro)
euro["ESP"]
euro[int_vec]
euro[c("ATS", "DEM", "IEP")]


length(euro)
euro[9:12]  # R should give an error but it didn't



############################################
# vector operations
############################################
dbl_vec > 1
dbl_vec[dbl_vec > 1]
dbl_vec %in% int_vec

# gives the index of dbl_vec that is in int_vec
which(dbl_vec %in% int_vec)



############################################
# matrices: 2D equivalent of vectors
# attributes
# - dim, ncol, nrow
############################################
int_mat <- matrix(1:6, ncol = 2)
int_mat
length(int_mat)
dim(int_mat)
nrow(int_mat)
ncol(int_mat)
int_mat[2, 2]
int_mat[1, ]
int_mat[, 1]


############################################
# homogenous structures
# - vector
# - arrays
# heterogenous structures
# - dataframe
# - lists
############################################
# lists
tResults <- t.test(dbl_vec)
class(tResults)
typeof(tResults)
length(tResults)
names(tResults)
attributes(tResults)
str(tResults)
tResults

# list of 1
tResults$statistic
tResults$p.value
tResults$method

# list of 2
tResults$conf.int

tResults["p.value"]

# subsetting using brackets gives you a list
tResults[1]
typeof(tResults[1])

# subsetting using double brackets gives you s single component
# of the list
tResults[[1]]
typeof(tResults[[1]])

tResults[c("statistic", "p.value")]
tResults[["statistic"]]


############################################
# data frames
############################################
library(tidyverse)
toothData <- read.csv("data/toothData.csv")
dim(toothData)
colnames(toothData)
rownames(toothData)

toothData[1:2, 1]
toothData[1:2, "len"]
toothData$len[1:2]
toothData[1:2, c("supp","dose")]


# compare these
## returns a vector
toothData$len    
toothData[[1]]
toothData[["len"]]
## returns a list
toothData["len"]
toothData[1]


toothData[5]  # gives an error



############################################
# assigning names
############################################
named_vec <- c(a=1, b=2, c=3) 
named_vec
names(named_vec) <- c()   # remove names
named_vec
names(named_vec) <- c("a", "b", "c")  # bring back the names
named_vec



############################################
# lists
############################################
my_list <- list(int_vec, dbl_vec)
names(my_list) <- c("integers", "doubles")
# or
my_list <- list(integers = int_vec, doubles = dbl_vec)
my_list


# add another item in the list
my_list$logical <- logi_vec
my_list
