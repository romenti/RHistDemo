# Basic course in R for historical demography ####

# First Lecture: introduction to R ####

# R language essentials

# This is a comment - it describes the code that follows
# in the following line we perform the simple operation: 2 + 4
2 + 4

# traditionally the first executed command should be the one that 
# prints to the screen: "Hello World" 
print("Hello World")

# Using R as a calculator
3 + 2
3 - 2
3 * 2
3 / 2
3 > 2
3 < 2
3 == 2
2 != 2
# an example: calculate the number of minutes in a year
365 * 24 * 60 

# let us create a variable to store the previous result
minutes <- 365 * 24 * 60
# calculate the number of minutes in half a year
minutes / 2
# since it is a "number", that is a numeric data, I can play with it

minutes ^ 2
minutes * 10

# ok, let us see the second data type: Character strings
variable2 <- "Welcome to this course"
variable2

# you cannot perform numerical operation on character strings
# try this
variable2 / 2
variable2 * 5

# third type: Logical data
Logi1 <- 23^2 < 12345

# mathematical functions
sqrt(2)
sqrt(minutes)
exp(minutes)
log(minutes)

# a variable is very boring
# with vectors we can collect more objects
# numeric vector
vector1 <- c(2, -4, 1/2, 2^3)
vector1

vector1 / 2
vector1 ^ 3

# character vector
vector2 <- c("Welcome", "to", "this", "course")
vector2

# logical vector
vector3 <- c(TRUE, FALSE, FALSE, TRUE)
vector3
# or, for example
vector4 <- vector1 < 1

#Operations with vectors
vn <- 1:6

# vector filtering
vn[2]
vn[1:3]
vn[-3]
vn[3:1]

# elementwise arithmetic
vn * 2
vn * vn

# using functions
mean(vn)

# can we calculate the variance?
mean(vn^2) - mean(vn)^2

# but what happens if we combine more types of data into
# a single vector?

v.comb <- c("Today is the", 9, "th of march")
v.comb / 4

v.comb1 <- c(-6, 34, FALSE, TRUE)
v.comb1 / 4

# matrices
d <- 1:12

e <- matrix(d, nrow = 4, ncol = 3)
e1 <- matrix(d, nrow = 4, ncol = 3, byrow = TRUE)

# matrix filtering
e[1,2]
e[3,1]
e[1:2, 2:3]

# some other examples of matrix filtering
e[1, ]
e[ ,1]
e[c(TRUE, FALSE,  TRUE, FALSE), 1:2]
e[ ,c(TRUE, FALSE, TRUE)]

# elementwise and matrix operation
e * 2
e * e

# lists 
# a list is a "collection" of possibly heterogeneous objects
# let us see an example
friends <- list(
  name = c("Annie", "Bob", "Carl"),
  age = c(20, 25, 19),
  single = c(TRUE, FALSE, TRUE)
)
# sub-selecting with names
# select the names
friends$age # call the name of the variable
friends[[2]] # use the position
friends[["age"]] # hybrid approach
# what if you do not know the names of the elements 
# inside your list?
names(friends)

# and finally: data.frame!
# data frame, they look like (and act as) matrices
# but they can collect data of different
# types in well organized way

friends.df <- as.data.frame(friends)

# alternative way to create data.frame
# starting from data

friends1 <- data.frame(
  name = c("Annie", "Bob", "Carl"),
  age = c(20, 25, 19),
  single = c(TRUE, FALSE, TRUE)
)

# Describing the Structure of a data frame
# Some useful functions

dim(friends.df)
ncol(friends.df)
nrow(friends.df)

# check the structure of the data frame
str(friends.df)

# Accessing elements in a data frame
friends.df[1,1]
friends.df[1,]
friends.df[,1]
friends.df[1:2,2:3]

# but there is more
friends.df$age
friends.df$age[c(1,3)]
friends.df[friends.df$single == TRUE,]
friends.df[friends.df$single != FALSE,]

# save to another object all the singles
friends.single <- friends.df[friends.df$single == TRUE,]

# what if we obtain some new info?
friends.df$town <- c("Milan", "Rome", "Bologna")
dim(friends.df)

# change columns position
friends.df[ , c(1, 4, 3, 2)]

# to cancel the variable: set to NULL
friends.df$town <- NULL

# dealing with missin values NA
age <- c(25, 31, NA, 41)
age / 2
mean(age)
mean(age, na.rm = T) # remove missing values

# check for missing values
is.na(age)
sum(is.na(age))

age[is.na(age)] <- 23

# working with available dataset
data()
data("swiss")
?swiss
str(swiss)





