## ---- include = FALSE-------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = F, message = F, warning = F-------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(lavaan)
library(semPlot)

## ----echo=FALSE, out.width = "75%", fig.align="center"----------------
knitr::include_graphics("pictures/example_lgm.png")

## ----echo=FALSE, out.width = "75%", fig.align="center"----------------
knitr::include_graphics("pictures/random_fixed.png")

## ----echo=FALSE, out.width = "75%", fig.align="center"----------------
knitr::include_graphics("pictures/random_fixed.png")

## ----echo=FALSE, out.width = "75%", fig.align="center"----------------
knitr::include_graphics("pictures/random_fixed.png")

## ---------------------------------------------------------------------
##load the data
crime.cov <- lav_matrix_lower2full(c(.63, 
                                    .50, .60, 
                                    .48, .48, .58, 
                                    .47, .48, .51, .67))

crime.mean <- c(5.17, 5.32, 5.40, 5.52)

names(crime.mean) <- 
  rownames(crime.cov) <- 
  colnames(crime.cov) <- c("Time1", "Time2", "Time3", "Time4")

## ---------------------------------------------------------------------
crime.model1 <- '
# intercept
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
i~~0*i
# residual variances
Time1~~r*Time1
Time2~~r*Time2
Time3~~r*Time3
Time4~~r*Time4
'

## ---------------------------------------------------------------------
crime.fit1 <- growth(crime.model1,
                    sample.cov=crime.cov, 
                    sample.mean=crime.mean, 
                    sample.nobs=952)
summary(crime.fit1,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

## ---------------------------------------------------------------------
semPaths(crime.fit1,
         whatLabels = "par",
         edge.label.cex = 1,
         layout = "tree")

## ---------------------------------------------------------------------
library(knitr)
fit.table <- matrix(NA, nrow = 5, ncol = 6)
colnames(fit.table) <- c("Model", "X2", "df", "RMSEA", "SRMR", "CFI")
fit.table[1, ] <- c("Intercept Only", round(fitmeasures(crime.fit1, c("chisq", "df", "rmsea", "srmr", "cfi")),3))
kable(fit.table)

## ---------------------------------------------------------------------
#save the parameter estimates
crime.fit1.par <- parameterestimates(crime.fit1)

#make table
par.table <- matrix(NA, nrow = 5, ncol = 7)
colnames(par.table) <- c("Model", "Intercept Mean", "Intercept Variance", "Residual Variance", "Slope Mean", "Slope Variance", "Covariance")

#put data in table
par.table[1, ] <- c("Intercept Only", 
                    round(crime.fit1.par$est[crime.fit1.par$lhs == "i" & crime.fit1.par$op == "~1"], 3),
                    "X", 
                    round(crime.fit1.par$est[crime.fit1.par$lhs == "Time1" & crime.fit1.par$op == "~~"], 3),
                    "X", 
                    "X", 
                    "X")
kable(par.table)

## ---------------------------------------------------------------------
crime.model2 <- '
# intercept
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
# residual variances
Time1~~r*Time1
Time2~~r*Time2
Time3~~r*Time3
Time4~~r*Time4
'

## ---------------------------------------------------------------------
crime.fit2 <- growth(crime.model2,
                    sample.cov=crime.cov, 
                    sample.mean=crime.mean, 
                    sample.nobs=952)
summary(crime.fit2,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

## ---------------------------------------------------------------------
fit.table[2, ] <- c("Random Intercept", round(fitmeasures(crime.fit2, c("chisq", "df", "rmsea", "srmr", "cfi")),3))
kable(fit.table)

## ---------------------------------------------------------------------
#save the parameter estimates
crime.fit2.par <- parameterestimates(crime.fit2)

#put data in table
par.table[2, ] <- c("Random Intercept", 
                    round(crime.fit2.par$est[crime.fit2.par$lhs == "i" & crime.fit2.par$op == "~1"], 3),
                    round(crime.fit2.par$est[crime.fit2.par$lhs == "i" & crime.fit2.par$op == "~~"], 3), 
                    round(crime.fit2.par$est[crime.fit2.par$lhs == "Time1" & crime.fit2.par$op == "~~"], 3),
                    "X", 
                    "X", 
                    "X")
kable(par.table)

## ---------------------------------------------------------------------
crime.model3 <- '
# intercept
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
# slope
s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4
s~0*1
s~~0*i
# residual variances
Time1~~r*Time1
Time2~~r*Time2
Time3~~r*Time3
Time4~~r*Time4
'

## ---------------------------------------------------------------------
crime.fit3 <- growth(crime.model3,
                    sample.cov=crime.cov, 
                    sample.mean=crime.mean, 
                    sample.nobs=952)
summary(crime.fit3,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

## ---------------------------------------------------------------------
fit.table[3, ] <- c("Random Slope", round(fitmeasures(crime.fit3, c("chisq", "df", "rmsea", "srmr", "cfi")),3))
kable(fit.table)

## ---------------------------------------------------------------------
#save the parameter estimates
crime.fit3.par <- parameterestimates(crime.fit3)

#put data in table
par.table[3, ] <- c("Random Slope", 
                    round(crime.fit3.par$est[crime.fit3.par$lhs == "i" & crime.fit3.par$op == "~1"], 3),
                    round(crime.fit3.par$est[crime.fit3.par$lhs == "i" & crime.fit3.par$op == "~~" & crime.fit3.par$rhs == "i"], 3), 
                    round(crime.fit3.par$est[crime.fit3.par$lhs == "Time1" & crime.fit3.par$op == "~~"], 3),
                    "X", 
                    round(crime.fit3.par$est[crime.fit3.par$lhs == "s" & crime.fit3.par$op == "~~"], 3), 
                    "X")
kable(par.table)

## ---------------------------------------------------------------------
crime.model4 <- '
# intercept
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
# slope
s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4
# residual variances
Time1~~r*Time1
Time2~~r*Time2
Time3~~r*Time3
Time4~~r*Time4
'

## ---------------------------------------------------------------------
crime.fit4 <- growth(crime.model4,
                    sample.cov=crime.cov, 
                    sample.mean=crime.mean, 
                    sample.nobs=952)
summary(crime.fit4,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

## ---------------------------------------------------------------------
fit.table[4, ] <- c("Full Slope", round(fitmeasures(crime.fit4, c("chisq", "df", "rmsea", "srmr", "cfi")),3))
kable(fit.table)

## ---------------------------------------------------------------------
#save the parameter estimates
crime.fit4.par <- parameterestimates(crime.fit4)

#put data in table
par.table[4, ] <- c("Full Slope", 
                    round(crime.fit4.par$est[crime.fit4.par$lhs == "i" & crime.fit4.par$op == "~1"], 3),
                    round(crime.fit4.par$est[crime.fit4.par$lhs == "i" & crime.fit4.par$op == "~~" & crime.fit4.par$rhs == "i"], 3), 
                    round(crime.fit4.par$est[crime.fit4.par$lhs == "Time1" & crime.fit4.par$op == "~~"], 3),
                    round(crime.fit4.par$est[crime.fit4.par$lhs == "s" & crime.fit4.par$op == "~1"], 3), 
                    round(crime.fit4.par$est[crime.fit4.par$lhs == "s" & crime.fit4.par$op == "~~"], 3), 
                    round(crime.fit4.par$est[crime.fit4.par$lhs == "i" & crime.fit4.par$op == "~~" & crime.fit4.par$rhs == "s"], 3))
kable(par.table)

## ---------------------------------------------------------------------
crime.model5 <- '
# intercept
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
# slope
s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4
'

## ---------------------------------------------------------------------
crime.fit5 <- growth(crime.model5,
                    sample.cov=crime.cov, 
                    sample.mean=crime.mean, 
                    sample.nobs=952)
summary(crime.fit5,
        standardized = TRUE,
        fit.measures = TRUE,
        rsquare = TRUE)

## ---------------------------------------------------------------------
fit.table[5, ] <- c("Unconstrained", round(fitmeasures(crime.fit5, c("chisq", "df", "rmsea", "srmr", "cfi")),3))
kable(fit.table)

## ---------------------------------------------------------------------
#save the parameter estimates
crime.fit5.par <- parameterestimates(crime.fit5)

residual_numbers <- paste(round(crime.fit5.par$est[crime.fit5.par$lhs == "Time1" & crime.fit5.par$op == "~~"], 3), 
                          round(crime.fit5.par$est[crime.fit5.par$lhs == "Time2" & crime.fit5.par$op == "~~"], 3),
                          round(crime.fit5.par$est[crime.fit5.par$lhs == "Time3" & crime.fit5.par$op == "~~"], 3),
                          round(crime.fit5.par$est[crime.fit5.par$lhs == "Time4" & crime.fit5.par$op == "~~"], 3))

#put data in table
par.table[5, ] <- c("Unconstrained", 
                    round(crime.fit5.par$est[crime.fit5.par$lhs == "i" & crime.fit5.par$op == "~1"], 3),
                    round(crime.fit5.par$est[crime.fit5.par$lhs == "i" & crime.fit5.par$op == "~~" & crime.fit5.par$rhs == "i"], 3), 
                    residual_numbers,
                    round(crime.fit5.par$est[crime.fit5.par$lhs == "s" & crime.fit5.par$op == "~1"], 3), 
                    round(crime.fit5.par$est[crime.fit5.par$lhs == "s" & crime.fit5.par$op == "~~"], 3), 
                    round(crime.fit5.par$est[crime.fit5.par$lhs == "i" & crime.fit5.par$op == "~~" & crime.fit5.par$rhs == "s"], 3))
kable(par.table)

