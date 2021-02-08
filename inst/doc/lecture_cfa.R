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
knitr::include_graphics("pictures/diagram_sem.png")

## ---------------------------------------------------------------------
# a famous example, build the model
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

# fit the model 
HS.fit <- cfa(HS.model, data = HolzingerSwineford1939)

# diagram the model
semPaths(HS.fit, 
         whatLabels = "std", 
         layout = "tree",
         edge.label.cex = 1)

## ---------------------------------------------------------------------
# a famous example, build the model
HS.model <- ' visual  <~ x1 + x2 + x3'

# fit the model 
HS.fit <- cfa(HS.model, data = HolzingerSwineford1939)

# diagram the model
semPaths(HS.fit, 
         whatLabels = "std", 
         layout = "tree",
         edge.label.cex = 1)

## ---------------------------------------------------------------------
wisc4.cor <- lav_matrix_lower2full(c(1,
                                     0.72,1,
                                     0.64,0.63,1,
                                     0.51,0.48,0.37,1,
                                     0.37,0.38,0.38,0.38,1))
# enter the SDs
wisc4.sd <- c(3.01 , 3.03 , 2.99 , 2.89 , 2.98)

# give everything names
colnames(wisc4.cor) <- 
  rownames(wisc4.cor) <-
  names(wisc4.sd) <- 
  c("Information", "Similarities", 
    "Word.Reasoning", "Matrix.Reasoning", "Picture.Concepts")

# convert
wisc4.cov <- cor2cov(wisc4.cor, wisc4.sd)

## ---------------------------------------------------------------------
wisc4.model <- '
g =~ Information + Similarities + Word.Reasoning + Matrix.Reasoning + Picture.Concepts
'

## ---------------------------------------------------------------------
wisc4.fit <- cfa(model = wisc4.model, 
                sample.cov = wisc4.cov, 
                sample.nobs = 550,  
                std.lv = FALSE)

## ---------------------------------------------------------------------
summary(wisc4.fit,
        standardized=TRUE, 
        rsquare = TRUE,
        fit.measures=TRUE)

## ---------------------------------------------------------------------
parameterestimates(wisc4.fit,
                   standardized=TRUE)

## ---------------------------------------------------------------------
fitted(wisc4.fit) ## estimated covariances
wisc4.cov ## actual covariances

## ---------------------------------------------------------------------
fitmeasures(wisc4.fit)

## ---------------------------------------------------------------------
modificationindices(wisc4.fit, sort = T)

## ---------------------------------------------------------------------
semPaths(wisc4.fit, 
         whatLabels="std", 
         what = "std",
         layout ="tree",
         edge.color = "blue",
         edge.label.cex = 1)

## ---------------------------------------------------------------------
wisc4.model2 <- '
V =~ Information + Similarities + Word.Reasoning 
F =~ Matrix.Reasoning + Picture.Concepts
'

# wisc4.model2 <- '
# V =~ Information + Similarities + Word.Reasoning 
# F =~ a*Matrix.Reasoning + a*Picture.Concepts
# '

## ---------------------------------------------------------------------
wisc4.fit2 <- cfa(wisc4.model2, 
                  sample.cov=wisc4.cov, 
                  sample.nobs=550,
                  std.lv = F)

## ---------------------------------------------------------------------
summary(wisc4.fit2,
        standardized=TRUE, 
        rsquare = TRUE,
        fit.measures=TRUE)

## ---------------------------------------------------------------------
semPaths(wisc4.fit2, 
         whatLabels="std", 
         what = "std",
         edge.color = "pink",
         edge.label.cex = 1,
         layout="tree")

## ---------------------------------------------------------------------
anova(wisc4.fit, wisc4.fit2)
fitmeasures(wisc4.fit, c("aic", "ecvi"))
fitmeasures(wisc4.fit2, c("aic", "ecvi"))

## ---------------------------------------------------------------------
#install.packages("parameters")
library(parameters)
model_parameters(wisc4.fit, standardize = TRUE)

## ---------------------------------------------------------------------
library(broom)
tidy(wisc4.fit)
glance(wisc4.fit)

