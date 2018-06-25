###############################################################
# SESSION 1: Introduction
# SESSION 1.2: Reading data into R
###############################################################
library(tidyverse)
library(readr)  # for read_csv
library(readxl)

# 1.2.1 read csv file
toothData <- read_csv("data/toothData.csv")

# 1.2.2 read Excel file with several sheets
#       read sheet 1
RealTimeData <- read_excel("data/RealTimeData.xlsx", 
                           sheet = "Sheet1")

# 1.2.3 read each sheet
RealTimeData1 <- read_excel("data/RealTimeData.xlsx", 
                           sheet = "Sheet1")
RealTimeData2 <- read_excel("data/RealTimeData.xlsx", 
                           sheet = "Sheet2")
RealTimeData3 <- read_excel("data/RealTimeData.xlsx", 
                           sheet = "Sheet3")

###############################################################
# SESSION 2.1 Data Cleaning
###############################################################
# read transport data
# - first row has a comment
# - has no column headers
# - data begins in the second row
# - missing data coded as "-"
# - first column has a sequential number, probably row numbers

# 2.1.1 using default values will give an error
transport <- read_csv("data/transport.csv")

# 2.1.2 use arguments for comment and column names
#       column 5 is read as a character because of the dash
transport <- read_csv("data/transport.csv",
                      comment = "#",
                      col_names = FALSE)
transport

# 2.1.3 use arguments for comment and column names
#       column 5 is read as a character because of the dash
transport <- read_csv("data/transport.csv",
                      comment = "#",
                      col_names = FALSE,
                      na = "-")
transport

# 2.1.4 let R figure out the data types
# -????? : don't read the first column, read the next 5 columns
#          and let R figure out the data types
transport <- read_csv("data/transport.csv",
                      comment = "#",
                      col_names = FALSE,
                      na = "-",
                      col_types = "-?????")
transport

# 2.1.5 assign the column names stored in a vector
colnames(transport)
colnames(transport) <- c("gender", "name", "weight", "height", "method") 
colnames(transport)
transport


# 2.1.6 export the data
write_csv(transport, "data/transport_clean.csv")



###############################################################
# SESSION 2.2 Data Munging/Manipulation
# use dplyr functions
# - starts_with, ends_with, contains, one_of, everything
#   filter, arrange
###############################################################
# 2.2.1 check dimensions of the data
dim(transport)

# 2.2.2 select or remove columns by name or index
select(transport, gender, weight)
select(transport, 2:4)
select(transport, -name)
select(transport, -2)
select(transport, ends_with("ght"))
select(transport, contains("t"))

# 2.2.3 filter the data
filter(transport, method == "car")
filter(transport, method == "car", gender == "female")

# 2.2.4 sort the data
arrange(transport, weight)
arrange(transport, desc(weight))
arrange(transport, method, height)

# 2.2.5 combine functions using the magrittr %>%
transport %>% 
  filter(method == "bike")

transport %>% 
  filter(method == "bike") %>% 
  arrange(weight)

# 2.2.6 rename a column and create new columns
transport %>%
  rename(height_cm = height) %>%
  mutate(height_m = height_cm/100,
         BMI = weight / height_m^2) %>%
  filter(method == "bike", BMI > 25) %>% 
  arrange(weight)
  

# 2.2.7 get summaries, remove NAs
# will drop the whole row with NAs
transport %>%
  filter(!is.na(height)) %>%
  summarise(mean(weight), mean(height))

# will drop row within a column with NAs
transport %>%
  summarise(mean(weight), mean(height, na.rm = TRUE))


# 2.2.8 get group summaries and number of observations
transport %>%
  group_by(gender, method) %>%
  summarise(avgWeight = mean(weight), 
            avgHeight = mean(height, na.rm = TRUE),
            n = n())

# 2.2.9 applying multiple functions
transport %>%
  filter(!is.na(height)) %>%
  group_by(gender, method) %>%
  summarise_at(vars(ends_with("ght")),
               funs(mean, sd))


transport %>%
  filter(!is.na(height)) %>%
  group_by(gender, method) %>%
  summarise_at(vars(ends_with("ght")),
               funs(mean, sd)) %>%
  select(gender, method, starts_with("weight"), starts_with("heigth"))


# 2.2.10 combine data frames
transport <- read_csv("data/transport_clean.csv")
contacts <- read_csv("data/contacts.csv")
dim(transport)
dim(contacts)

left_join(transport, contacts, by = "name")
right_join(transport, contacts, by = "name")
full_join(transport, contacts, by = "name")


