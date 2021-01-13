## ---- include = FALSE--------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = F, message = F, warning = F--------------------------
options(scipen = 999)
knitr::opts_chunk$set(echo = TRUE)
library(lavaan, quietly = T)
library(semPlot, quietly = T)
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

fit <- cfa(HS.model, data = HolzingerSwineford1939)

HS.model2 <- 'visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 
              visual ~ speed'
fit2 <- cfa(HS.model2, data = HolzingerSwineford1939)

HS.model3 <- 'visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 
              visual ~ speed
              speed ~ textual
              textual ~ visual'
fit3 <- cfa(HS.model3, data = HolzingerSwineford1939)

## ----exo, echo=FALSE, out.width="75%", fig.align="center"--------
knitr::include_graphics("pictures/exo_endo.png")

## ----endo, echo=FALSE, out.width="75%", fig.align="center"-------
knitr::include_graphics("pictures/exo_endo.png")

## ----echo = F----------------------------------------------------
semPaths(fit,
         whatLabels = "std",
         edge.label.cex = 1)

## ----echo = F----------------------------------------------------
semPaths(fit2,
         whatLabels = "std",
         edge.label.cex = 1)

## ----full, out.width="75%", echo=FALSE, fig.align="center"-------
knitr::include_graphics("pictures/full_sem.png")

## ----echo = F----------------------------------------------------
semPaths(fit2,
         whatLabels = "std",
         edge.label.cex = 1)

## ----echo = F----------------------------------------------------
semPaths(fit3,
         whatLabels = "std", 
         edge.label.cex = 1)

## ----echo = F----------------------------------------------------
summary(fit2)

## ----echo = F----------------------------------------------------
summary(fit2, standardized = T, rsquare = T)

## ----model-steps, echo=FALSE, out.width="75%",  fig.align="center"----
knitr::include_graphics("pictures/model_steps.png")

## ----echo = F----------------------------------------------------
semPaths(fit)

## ----echo = F----------------------------------------------------
summary(fit)

## ----echo = F----------------------------------------------------
summary(fit, standardized = T)

