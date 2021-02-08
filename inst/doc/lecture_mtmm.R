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
knitr::include_graphics("pictures/model1_mtmm.png")

## ---------------------------------------------------------------------
library(lavaan)
library(semPlot)
library(rio)

meaning.data <- import("data/lecture_mtmm.csv")
str(meaning.data)

## ---------------------------------------------------------------------
methods.model <- '
mlq =~ m1 + m2 + m3 + m4 + m5 + m6 + m8 + m9 + m10
pil =~ p3 + p4 + p8 + p12 + p17 + p20
'

traits.model <- '
meaning =~ m1 + m2 + m5 + m10 + p4 + p12 + p17
purpose =~ m3 + m4 + m6 + m8 + m9 + p3 + p8 + p20
'

## ---------------------------------------------------------------------
methods.fit <- cfa(model = methods.model, 
                   data = meaning.data,
                   std.lv = TRUE)
traits.fit <- cfa(model = traits.model,
                  data = meaning.data,
                  std.lv = TRUE)

lavInspect(traits.fit, "cor.lv")

## ---------------------------------------------------------------------
summary(methods.fit, 
        rsquare = TRUE, 
        standardized = TRUE,
        fit.measures = TRUE)

summary(traits.fit, 
        rsquare = TRUE, 
        standardized = TRUE,
        fit.measures = TRUE)

## ---------------------------------------------------------------------
semPaths(methods.fit, 
         whatLabels = "std", 
         layout = "tree", 
         edge.label.cex = 1)

semPaths(traits.fit, 
         whatLabels = "std", 
         layout = "tree", 
         edge.label.cex = 1)

## ---------------------------------------------------------------------
step1.model <- '
mlq =~ m1 + m2 + m3 + m4 + m5 + m6 + m8 + m9 + m10
pil =~ p3 + p4 + p8 + p12 + p17 + p20
meaning =~ m1 + m2 + m5 + m10 + p4 + p12 + p17
purpose =~ m3 + m4 + m6 + m8 + m9 + p3 + p8 + p20

##fix the covariances
mlq ~~ 0*meaning
pil ~~ 0*meaning
mlq ~~ 0*purpose
pil ~~ 0*purpose
'

## ---------------------------------------------------------------------
step1.fit <- cfa(model = step1.model, 
                 data = meaning.data,
                 std.lv = TRUE)

summary(step1.fit, 
        rsquare = TRUE, 
        standardized = TRUE,
        fit.measures = TRUE)

## ---------------------------------------------------------------------
semPaths(step1.fit, 
         whatLabels = "std", 
         layout = "tree", 
         edge.label.cex = 1)

## ----echo=FALSE, out.width = "75%", fig.align="center"----------------
knitr::include_graphics("pictures/model2_mtmm.png")

## ---------------------------------------------------------------------
##model 2 is the methods model 
##we've already checked it out
anova(step1.fit, methods.fit)

fitmeasures(step1.fit, "cfi") 
fitmeasures(methods.fit, "cfi")

## ----echo=FALSE, out.width = "75%", fig.align="center"----------------
knitr::include_graphics("pictures/model3_mtmm.png")

## ---------------------------------------------------------------------
step3.model <- '
mlq =~ m1 + m2 + m3 + m4 + m5 + m6 + m8 + m9 + m10
pil =~ p3 + p4 + p8 + p12 + p17 + p20
meaning =~ m1 + m2 + m5 + m10 + p4 + p12 + p17
purpose =~ m3 + m4 + m6 + m8 + m9 + p3 + p8 + p20

##fix the covariances
mlq ~~ 0*meaning
pil ~~ 0*meaning
mlq ~~ 0*purpose
pil ~~ 0*purpose
meaning ~~ 1*purpose
'

## ---------------------------------------------------------------------
step3.fit <- cfa(model = step3.model,
                 data = meaning.data,
                 std.lv = TRUE)

summary(step3.fit, 
        rsquare = TRUE, 
        standardized = TRUE, 
        fit.measure = TRUE)

## ---------------------------------------------------------------------
semPaths(step3.fit, 
         whatLabels = "std", 
         layout = "tree", 
         edge.label.cex = 1)

## ---------------------------------------------------------------------
anova(step1.fit, step3.fit)

fitmeasures(step1.fit, "cfi")
fitmeasures(step3.fit, "cfi")

## ----echo=FALSE, out.width = "75%", fig.align="center"----------------
knitr::include_graphics("pictures/model4_mtmm.png")

## ---------------------------------------------------------------------
step4.model <- '
mlq =~ m1 + m2 + m3 + m4 + m5 + m6 + m8 + m9 + m10
pil =~ p3 + p4 + p8 + p12 + p17 + p20
meaning =~ m1 + m2 + m5 + m10 + p4 + p12 + p17
purpose =~ m3 + m4 + m6 + m8 + m9 + p3 + p8 + p20

##fix the covariances
mlq ~~ 0*meaning
pil ~~ 0*meaning
mlq ~~ 0*purpose
pil ~~ 0*purpose
pil ~~ 0*mlq
'

## ---------------------------------------------------------------------
step4.fit <- cfa(model = step4.model, 
                  data = meaning.data,
                  std.lv = TRUE)

summary(step4.fit, 
        rsquare = TRUE, 
        standardized = TRUE, 
        fit.measure = TRUE)

## ---------------------------------------------------------------------
semPaths(step4.fit, 
         whatLabels = "std", 
         layout = "tree", 
         edge.label.cex = 1)

## ---------------------------------------------------------------------
anova(step1.fit, step4.fit)

fitmeasures(step1.fit, "cfi")
fitmeasures(step4.fit, "cfi")

## ---------------------------------------------------------------------
parameterestimates(step1.fit, standardized = T)

## ---------------------------------------------------------------------
parameterestimates(step1.fit, standardized = T)

