## ----include = FALSE----------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = F, message = F, warning = F---------
knitr::opts_chunk$set(echo = TRUE)

## ----eval = F-----------------------------------
# install.packages("lavaan")
# install.packages("semPlot")

## -----------------------------------------------
library(rio)
eval.data <- import("data/lecture_evals.csv")

## ----echo=FALSE, out.width = "25%", fig.align="center"----
knitr::include_graphics("pictures/lecture_evals.png")

## ----echo=FALSE, out.width = "25%", fig.align="center"----
knitr::include_graphics("pictures/lecture_evals.png")

## -----------------------------------------------
library(lavaan)
eval.model <- '
q4 ~ q12 + q2
q1 ~ q4 + q12
'

## -----------------------------------------------
eval.model

## -----------------------------------------------
eval.output <- sem(model = eval.model,
                   data = eval.data)

## -----------------------------------------------
summary(eval.output)

## -----------------------------------------------
summary(eval.output, 
        standardized = TRUE, # for the standardized solution
        fit.measures = TRUE, # for model fit
        rsquare = TRUE) # for SMCs

## -----------------------------------------------
library(semPlot)
semPaths(eval.output, # the analyzed model 
         whatLabels = "par", # what to add as the numbers, std for standardized
         edge.label.cex = 1, # make the font bigger
         layout = "spring") # change the layout tree, circle, spring, tree2, circle2

## -----------------------------------------------
regression.cor <- lav_matrix_lower2full(c(1.00,
                                         0.20,1.00,
                                         0.24,0.30,1.00,
                                         0.70,0.80,0.30,1.00))

# name the variables in the matrix
colnames(regression.cor) <-
  rownames(regression.cor) <-
  c("X1", "X2", "X3", "Y") 

## -----------------------------------------------
regression.model <- '
# structural model for Y
Y ~ a*X1 + b*X2 + c*X3 
# label the residual variance of Y
Y ~~ z*Y 
'

## -----------------------------------------------
regression.fit <- sem(model = regression.model,
                      sample.cov = regression.cor, # instead of data
                      sample.nobs = 1000) # number of data points

## -----------------------------------------------
summary(regression.fit, 
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

## -----------------------------------------------
semPaths(regression.fit, 
         whatLabels="par", 
         edge.label.cex = 1,
         layout="tree")

## -----------------------------------------------
beaujean.cov <- lav_matrix_lower2full(c(648.07, 
                                        30.05, 8.64, 
                                        140.18, 25.57, 233.21))
colnames(beaujean.cov) <-
  rownames(beaujean.cov) <-
  c("salary", "school", "iq")

## -----------------------------------------------
beaujean.model <- '
salary ~ a*school + c*iq
iq ~ b*school # this is reversed in first printing of the book 
ind:= b*c # this is the mediation part 
'

## -----------------------------------------------
beaujean.fit <- sem(model = beaujean.model, 
                    sample.cov = beaujean.cov, 
                    sample.nobs = 300)

## -----------------------------------------------
summary(beaujean.fit, 
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

## -----------------------------------------------
semPaths(beaujean.fit, 
         whatLabels="par", 
         edge.label.cex = 1,
         layout="tree")

## ----echo=FALSE, out.width = "50%", fig.align="center"----
knitr::include_graphics("pictures/srmr_formula.png")

## -----------------------------------------------
chi_difference <- 12.6 - 4.3
df_difference <- 14 - 12
pchisq(chi_difference, df_difference, lower.tail = F)

## -----------------------------------------------
compare.data <- lav_matrix_lower2full(c(1.00,
                                        .53,	1.00,	
                                        .15,	.18,	1.00,		
                                        .52,	.29,	-.05,	1.00,	
                                        .30,	.34,	.23,	.09,	1.00))

colnames(compare.data) <- 
  rownames(compare.data) <- 
  c("morale", "illness", "neuro", "relationship", "SES") 

## -----------------------------------------------
#model 1
compare.model1 = '
illness ~ morale
relationship ~ morale
morale ~ SES + neuro
'

#model 2
compare.model2 = '
SES ~ illness + neuro
morale ~ SES + illness
relationship ~ morale + neuro
'

## -----------------------------------------------
compare.model1.fit <- sem(compare.model1, 
                          sample.cov = compare.data, 
                          sample.nobs = 469)

summary(compare.model1.fit, 
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

## -----------------------------------------------
compare.model2.fit <- sem(compare.model2, 
                          sample.cov = compare.data, 
                          sample.nobs = 469)

summary(compare.model2.fit, 
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

## -----------------------------------------------
semPaths(compare.model1.fit, 
         whatLabels="par", 
         edge.label.cex = 1,
         layout="spring")

## -----------------------------------------------
semPaths(compare.model2.fit, 
         whatLabels="par", 
         edge.label.cex = 1,
         layout="spring")

## -----------------------------------------------
anova(compare.model1.fit, compare.model2.fit)
fitmeasures(compare.model1.fit, c("aic", "ecvi"))
fitmeasures(compare.model2.fit, c("aic", "ecvi"))

