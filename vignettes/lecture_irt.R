## ---- include = FALSE-----------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = F, message = F, warning = F-----------------
knitr::opts_chunk$set(echo = TRUE)
library(lavaan)
library(semPlot)

## ----echo=FALSE, out.width = "75%", fig.align="center"----
knitr::include_graphics("pictures/icc_example.png")

## ----echo=FALSE, out.width = "75%", fig.align="center"----
knitr::include_graphics("pictures/item_difficulty.png")

## ----echo=FALSE, out.width = "75%", fig.align="center"----
knitr::include_graphics("pictures/ability.png")

## ----echo=FALSE, out.width = "75%", fig.align="center"----
knitr::include_graphics("pictures/ability.png")

## -------------------------------------------------------
library(ltm)
library(mirt)
data(LSAT)
head(LSAT)

## -------------------------------------------------------
# Data frame name ~ z1 for one latent variable
#irt.param to give it to you standardized
LSAT.model <- ltm(LSAT ~ z1,
                  IRT.param = TRUE)

## -------------------------------------------------------
coef(LSAT.model)

## -------------------------------------------------------
plot(LSAT.model, type = "ICC") ## all items at once

## -------------------------------------------------------
plot(LSAT.model, type = "IIC", items = 0) ## Test Information Function

## -------------------------------------------------------
factor.scores(LSAT.model)

## -------------------------------------------------------
LSAT.model2 <- tpm(LSAT, #dataset
                   type = "latent.trait",
                   IRT.param = TRUE)

## -------------------------------------------------------
coef(LSAT.model2)

## -------------------------------------------------------
plot(LSAT.model2, type = "ICC") ## all items at once

## -------------------------------------------------------
plot(LSAT.model2, type = "IIC", items = 0) ## Test Information Function

## -------------------------------------------------------
factor.scores(LSAT.model2)

## -------------------------------------------------------
anova(LSAT.model, LSAT.model2)

## -------------------------------------------------------
library(rio)
poly.data <- import("data/lecture_irt.csv")
poly.data <- na.omit(poly.data)

#reverse code
poly.data$Q99_9 = 8 - poly.data$Q99_9

#separate factors
poly.data1 = poly.data[ , c(1, 4, 5, 6, 9)]
poly.data2 = poly.data[ , c(2, 3, 7, 8, 10)]

## -------------------------------------------------------
gpcm.model1 <- mirt(data = poly.data1, #data
                    model = 1, #number of factors
                    itemtype = "gpcm") #poly model type

## -------------------------------------------------------
summary(gpcm.model1) ##standardized coefficients 

## -------------------------------------------------------
coef(gpcm.model1, IRTpars = T) ##coefficients

head(fscores(gpcm.model1)) ##factor scores

## -------------------------------------------------------
plot(gpcm.model1, type = "trace") ##curves for all items at once
itemplot(gpcm.model1, 5, type = "trace")

## -------------------------------------------------------
itemplot(gpcm.model1, 4, type = "info") ##IIC for each item
plot(gpcm.model1, type = "info") ##test information curve

## -------------------------------------------------------
plot(gpcm.model1) ##expected score curve

