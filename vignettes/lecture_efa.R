## ---- include = FALSE------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = F--------------------------------------------------------------
options(scipen = 999)
knitr::opts_chunk$set(echo = TRUE)

## ----echo = F, warning = F, message = F------------------------------------
library(lavaan)
library(semPlot)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939)
semPaths(fit,
         whatLabels = "std",
         edge.label.cex = 1)

## ----echo = F, warning = F, message = F------------------------------------
library(lavaan)
library(semPlot)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939)
semPaths(fit,
         whatLabels = "std",
         edge.label.cex = 1)

## ----message = F-----------------------------------------------------------
library(rio)
library(psych)
master <- import("data/lecture_efa.csv")
head(master)

## ----scree, echo=FALSE, out.height="500px", out.width="800px", fig.align="center"----
knitr::include_graphics("pictures/scree.png")

## --------------------------------------------------------------------------
number_items <- fa.parallel(master, #data frame
                            fm="ml", #math
                            fa="fa") #only efa

## --------------------------------------------------------------------------
 
sum(number_items$fa.values > 1)
sum(number_items$fa.values > .7)

## ----rotation, echo=FALSE, out.height="500px", out.width="800px", fig.align="center"----
knitr::include_graphics("pictures/rotate.png")

## --------------------------------------------------------------------------
EFA_fit <- fa(master, #data
              nfactors = 2, #number of factors
              rotate = "oblimin", #rotation
              fm = "ml") #math

## --------------------------------------------------------------------------
EFA_fit

## --------------------------------------------------------------------------
EFA_fit2 <- fa(master[ , -23], #data
              nfactors = 2, #number of factors
              rotate = "oblimin", #rotation
              fm = "ml") #math

EFA_fit2

## --------------------------------------------------------------------------
 fa.plot(EFA_fit2, 
     labels = colnames(master[ , -23]))

## --------------------------------------------------------------------------
fa.diagram(EFA_fit2)

## --------------------------------------------------------------------------
EFA_fit2$rms #Root mean square of the residuals
EFA_fit2$RMSEA #root mean squared error of approximation
EFA_fit2$TLI #tucker lewis index
1 - ((EFA_fit2$STATISTIC-EFA_fit2$dof)/
       (EFA_fit2$null.chisq-EFA_fit2$null.dof)) #CFI 

## --------------------------------------------------------------------------
factor1 = c(1:7, 9:10, 12:16, 18:22)
factor2 = c(8, 11, 17)
##we use the psych::alpha to make sure that R knows we want the alpha function from the psych package.
##ggplot2 has an alpha function and if we have them both open at the same time
##you will sometimes get a color error without this :: information. 
psych::alpha(master[, factor1], check.keys = T)
psych::alpha(master[, factor2], check.keys = T)

