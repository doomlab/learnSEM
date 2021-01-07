## ---- include = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE--------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## --------------------------------------------------------------------------
X <- 4

## --------------------------------------------------------------------------
library(palmerpenguins)
data(penguins)
attributes(penguins)

## --------------------------------------------------------------------------
str(penguins)

names(penguins) #ls(penguins) provides this as well 

## --------------------------------------------------------------------------
X

## --------------------------------------------------------------------------
penguins$species

## --------------------------------------------------------------------------
A <- 1:20
A

B <- seq(from = 1, to = 20, by = 1)
B

C <- c("cheese", "is", "great")
C

D <- rep(1, times = 30)
D

## --------------------------------------------------------------------------
class(A)
class(C)
class(penguins)
class(penguins$species)

## --------------------------------------------------------------------------
dim(penguins) #rows, columns
length(penguins)
length(penguins$species)

## --------------------------------------------------------------------------
output <- lm(flipper_length_mm ~ bill_length_mm, data = penguins)
str(output)
output$coefficients

## --------------------------------------------------------------------------
myMatrix <- matrix(data = 1:10,
                   nrow = 5,
                   ncol = 2)
myMatrix

## --------------------------------------------------------------------------
penguins[1, 2:3]
penguins$sex[4:25] #why no comma?

## --------------------------------------------------------------------------
X <- 1:5
Y <- 6:10
# I can use either because they are the same size 
cbind(X,Y)
rbind(X,Y)

## --------------------------------------------------------------------------
ls()
ls(penguins)

## --------------------------------------------------------------------------
newDF <- as.data.frame(cbind(X,Y))
str(newDF)
as.numeric(c("one", "two", "3"))

## --------------------------------------------------------------------------
penguins[1:2,] #just the first two rows 
penguins[penguins$bill_length_mm > 54 , ] #how does this work?
penguins$bill_length_mm > 54

## --------------------------------------------------------------------------
#you can create complex rules
penguins[penguins$bill_length_mm > 54 & penguins$bill_depth_mm > 17, ]
#you can do all BUT
penguins[ , -1]
#grab a few columns by name
vars <- c("bill_length_mm", "sex")
penguins[ , vars]

## --------------------------------------------------------------------------
#another function
#notice any differences? 
subset(penguins, bill_length_mm > 54)
#other functions include filter() in tidyverse

## --------------------------------------------------------------------------
head(complete.cases(penguins)) #creates logical
head(na.omit(penguins)) #creates actual rows
head(is.na(penguins$body_mass_g)) #for individual vectors

## --------------------------------------------------------------------------
getwd()

## ----eval = F--------------------------------------------------------------
#  setwd("/Users/buchanan/OneDrive - Harrisburg University/Teaching/ANLY 580/updated/1 Introduction R")

## --------------------------------------------------------------------------
library(rio)
myDF <- import("data/assignment_introR.csv")
head(myDF)

## ----eval = F--------------------------------------------------------------
#  install.packages("car")

## --------------------------------------------------------------------------
library(car)

## ----eval = F--------------------------------------------------------------
#  ?lm
#  help(lm)

## --------------------------------------------------------------------------
args(lm)
example(lm)

## --------------------------------------------------------------------------
pizza <- function(x){ x^2 }
pizza(3)

## --------------------------------------------------------------------------
table(penguins$species)
summary(penguins$bill_length_mm)

## --------------------------------------------------------------------------
mean(penguins$bill_length_mm) #returns NA
mean(penguins$bill_length_mm, na.rm = TRUE)

cor(penguins[ , c("bill_length_mm", "bill_depth_mm", "flipper_length_mm")])
cor(penguins[ , c("bill_length_mm", "bill_depth_mm", "flipper_length_mm")],
    use = "pairwise.complete.obs")

