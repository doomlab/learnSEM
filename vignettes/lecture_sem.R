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
knitr::include_graphics("pictures/full_sem2.png")

## ----echo=FALSE, out.width = "75%", fig.align="center"----
knitr::include_graphics("pictures/indicators.png")

## ----echo=FALSE, out.width = "75%", fig.align="center"----
knitr::include_graphics("pictures/kline_model.png")

## -------------------------------------------------------
library(lavaan)
library(semPlot)

family.cor <- lav_matrix_lower2full(c(1.00, 
                                      .74,	1.00,	
                                      .27,	.42,	1.00,	
                                      .31,	.40,	.79,	1.00,	
                                      .32,	.35,	.66,	.59,	1.00))
family.sd <- c(32.94,	22.75, 13.39,	13.68,	14.38)
rownames(family.cor) <- 
  colnames(family.cor) <-
  names(family.sd) <- c("father", "mother", "famo", "problems", "intimacy")

family.cov <- cor2cov(family.cor, family.sd)

## -------------------------------------------------------
family.model <- '
adjust =~ problems + intimacy
family =~ father + mother + famo'

## -------------------------------------------------------
family.fit <- cfa(model = family.model,
                  sample.cov = family.cov,
                  sample.nobs = 203)

## -------------------------------------------------------
inspect(family.fit, "cov.lv")
inspect(family.fit, "cor.lv")

## -------------------------------------------------------
family.fit <- cfa(model = family.model,
                  sample.cov = family.cor,
                  sample.nobs = 203)

## -------------------------------------------------------
summary(family.fit, 
        rsquare = TRUE, 
        standardized = TRUE,
        fit.measures = TRUE)

## -------------------------------------------------------
modificationindices(family.fit, sort = T)

## -------------------------------------------------------
family.model2 <- '
adjust =~ problems + intimacy
family =~ father + mother + famo
father ~~ mother'

family.fit2 <- cfa(model = family.model2,
                  sample.cov = family.cov,
                  sample.nobs = 203)

inspect(family.fit2, "cor.lv")

## -------------------------------------------------------
semPaths(family.fit, 
         whatLabels="std", 
         layout="tree", 
         edge.label.cex = 1)

## -------------------------------------------------------
predict.model <- '
adjust =~ problems + intimacy
family =~ father + mother + famo
adjust~family'

## -------------------------------------------------------
predict.fit <- sem(model = predict.model,
                   sample.cov = family.cor,
                   sample.nobs = 203)

## -------------------------------------------------------
summary(predict.fit, 
        rsquare = TRUE, 
        standardized = TRUE,
        fit.measures = TRUE)

## -------------------------------------------------------
semPaths(predict.fit, 
         whatLabels="std", 
         layout="tree", 
         edge.label.cex = 1)

## ----echo=FALSE, out.width = "75%", fig.align="center"----
knitr::include_graphics("pictures/full_example.png")

## -------------------------------------------------------
family.cor <- lav_matrix_lower2full(c(1.00, 
                                     .42,	1.00, 
                                    -.43,	-.50,	1.00, 
                                    -.39,	-.43,	.78,	1.00,	
                                    -.24,	-.37,	.69,	.73,	1.00, 
                                    -.31,	-.33,	.63,	.87,	.72,	1.00,	
                                    -.25,	-.25,	.49,	.53,	.60,	.59,	1.00, 
                                     -.25,	-.26,	.42,	.42,	.44,	.45,	.77,	1.00,	
                                     -.16,	-.18,	.23,	.36,	.38,	.38,	.59,	.58, 1.00))

family.sd <- c(13.00,	13.50,	13.10,	12.50,	13.50,	14.20,	9.50,	11.10,	8.70)

rownames(family.cor) <- 
  colnames(family.cor) <-
  names(family.sd) <- c("parent_psych","low_SES","verbal",
                        "reading","math","spelling","motivation","harmony","stable")

family.cov <- cor2cov(family.cor, family.sd)

## -------------------------------------------------------
composite.model <- '
risk <~ low_SES + parent_psych + verbal
achieve =~ reading + math + spelling
adjustment =~ motivation + harmony + stable
risk =~ achieve + adjustment
'

## -------------------------------------------------------
composite.fit <- sem(model = composite.model, 
                      sample.cov = family.cov, 
                      sample.nobs = 158)

## -------------------------------------------------------
summary(composite.fit, 
        rsquare = TRUE, 
        standardized = TRUE,
        fit.measures = TRUE)

## -------------------------------------------------------
modificationindices(composite.fit, sort = T)

## -------------------------------------------------------
semPaths(composite.fit, 
         whatLabels="std", 
         layout="tree",
         edge.label.cex = 1)

