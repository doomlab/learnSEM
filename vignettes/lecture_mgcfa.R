## ----include = FALSE----------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----echo = F, message = F, warning = F---------
knitr::opts_chunk$set(echo = TRUE)
library(lavaan)
library(semPlot)

## -----------------------------------------------
library(lavaan)
library(rio)
res.data <- import("data/assignment_mgcfa.csv")

head(res.data)

## -----------------------------------------------
table(res.data$Sex)
res.data$Sex <- factor(res.data$Sex, 
                       levels = c(1,2),
                       labels = c("Men", "Women"))
res.data <- subset(res.data, !is.na(Sex))
nrow(res.data)

## -----------------------------------------------
overall.model <- '
RS =~ RS1 + RS2 + RS3 + RS4 + RS5 + RS6 + RS7 + RS8 + RS9 + RS10 + RS11 + RS12 + RS13 + RS14
'

overall.fit <- cfa(model = overall.model,
                   data = res.data, 
                   meanstructure = TRUE) ##this is important 

## -----------------------------------------------
summary(overall.fit, 
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

## -----------------------------------------------
library(knitr)
table_fit <- matrix(NA, nrow = 9, ncol = 6)
colnames(table_fit) = c("Model", "X2", "df", "CFI", "RMSEA", "SRMR")
table_fit[1, ] <- c("Overall Model", round(fitmeasures(overall.fit, 
                                                 c("chisq", "df", "cfi",
                                                   "rmsea", "srmr")),3))
kable(table_fit)

## ----semplot, messages = F----------------------
library(semPlot)

semPaths(overall.fit, 
         whatLabels = "std", 
         edge.label.cex = 1,
         layout = "tree")

## -----------------------------------------------
men.fit <- cfa(model = overall.model,
               data = res.data[res.data$Sex == "Men" , ], 
               meanstructure = TRUE)
summary(men.fit,
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

## -----------------------------------------------
women.fit <- cfa(model = overall.model,
                 data = res.data[res.data$Sex == "Women" , ], 
                 meanstructure = TRUE)
summary(women.fit,
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

## -----------------------------------------------
table_fit[2, ] <- c("Men Model", round(fitmeasures(men.fit, 
                                                 c("chisq", "df", "cfi",
                                                   "rmsea", "srmr")),3))

table_fit[3, ] <- c("Women Model", round(fitmeasures(women.fit, 
                                                 c("chisq", "df", "cfi",
                                                   "rmsea", "srmr")),3))

kable(table_fit)

## -----------------------------------------------
configural.fit <- cfa(model = overall.model,
                      data = res.data,
                      meanstructure = TRUE,
                      group = "Sex")
summary(configural.fit,
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

## -----------------------------------------------
table_fit[4, ] <- c("Configural Model", round(fitmeasures(configural.fit, 
                                                 c("chisq", "df", "cfi",
                                                   "rmsea", "srmr")),3))

kable(table_fit)

## -----------------------------------------------
metric.fit <- cfa(model = overall.model,
                  data = res.data,
                  meanstructure = TRUE,
                  group = "Sex",
                  group.equal = c("loadings"))
summary(metric.fit,
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

## -----------------------------------------------
table_fit[5, ] <- c("Metric Model", round(fitmeasures(metric.fit, 
                                                 c("chisq", "df", "cfi",
                                                   "rmsea", "srmr")),3))

kable(table_fit)

## -----------------------------------------------
scalar.fit <- cfa(model = overall.model,
                  data = res.data,
                  meanstructure = TRUE,
                  group = "Sex",
                  group.equal = c("loadings", "intercepts"))
summary(scalar.fit,
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

## -----------------------------------------------
table_fit[6, ] <- c("Scalar Model", round(fitmeasures(scalar.fit, 
                                                 c("chisq", "df", "cfi",
                                                   "rmsea", "srmr")),3))

kable(table_fit)

## -----------------------------------------------
strict.fit <- cfa(model = overall.model,
                  data = res.data,
                  meanstructure = TRUE,
                  group = "Sex",
                  group.equal = c("loadings", "intercepts", "residuals"))
summary(strict.fit,
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

## -----------------------------------------------
table_fit[7, ] <- c("Strict Model", round(fitmeasures(strict.fit, 
                                                 c("chisq", "df", "cfi",
                                                   "rmsea", "srmr")),3))

kable(table_fit)

## -----------------------------------------------
##write out partial codes
partial_syntax <- paste(colnames(res.data)[3:16], #all the columns
                        "~~", #residuals
                        colnames(res.data)[3:16]) #all columns again 
partial_syntax

CFI_list  <- 1:length(partial_syntax)
names(CFI_list) <- partial_syntax

for (i in 1:length(partial_syntax)){
  
  temp <- cfa(model = overall.model, 
              data = res.data,
              meanstructure = TRUE,
              group = "Sex",
              group.equal = c("loadings", "intercepts", "residuals"),
              group.partial = partial_syntax[i])
  
  CFI_list[i] <- fitmeasures(temp, "cfi")
}

CFI_list
which.max(CFI_list)

## -----------------------------------------------
strict.fit2 <- cfa(model = overall.model,
                  data = res.data,
                  meanstructure = TRUE,
                  group = "Sex",
                  group.equal = c("loadings", "intercepts", "residuals"), 
                  group.partial = c("RS9 ~~ RS9"))
summary(strict.fit2,
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

## -----------------------------------------------
table_fit[8, ] <- c("Strict Model RS9", round(fitmeasures(strict.fit2, 
                                                 c("chisq", "df", "cfi",
                                                   "rmsea", "srmr")),3))

kable(table_fit)

## -----------------------------------------------
for (i in 1:length(partial_syntax)){
  
  temp <- cfa(model = overall.model, 
              data = res.data,
              meanstructure = TRUE,
              group = "Sex",
              group.equal = c("loadings", "intercepts", "residuals"),
              group.partial = c("RS9 ~~ RS9", partial_syntax[i]))
  
  CFI_list[i] <- fitmeasures(temp, "cfi")
}

CFI_list
which.max(CFI_list)

## -----------------------------------------------
strict.fit3 <- cfa(model = overall.model,
                  data = res.data,
                  meanstructure = TRUE,
                  group = "Sex",
                  group.equal = c("loadings", "intercepts", "residuals"), 
                  group.partial = c("RS9 ~~ RS9", "RS13 ~~ RS13"))
summary(strict.fit3,
        standardized = TRUE, 
        rsquare = TRUE, 
        fit.measure = TRUE)

## -----------------------------------------------
table_fit[9, ] <- c("Strict Model RS9 + 13", round(fitmeasures(strict.fit3, 
                                                 c("chisq", "df", "cfi",
                                                   "rmsea", "srmr")),3))

kable(table_fit)

## -----------------------------------------------
predicted_scores <- lavPredict(strict.fit3, type = "ov")

table(res.data$Sex)

predicted_scores <- as.data.frame(do.call(rbind, predicted_scores))
predicted_scores$Sex <- c(rep("Women", 266), rep("Men", 244))

predicted_scores$sum <- apply(predicted_scores[ , 1:14], 1, sum)
head(predicted_scores)

tapply(predicted_scores$sum, predicted_scores$Sex, mean)

## -----------------------------------------------
res.data$sum <- apply(res.data[ , 3:16], 1, sum)

tapply(res.data$sum, res.data$Sex, mean)

latent_means <- lavPredict(strict.fit3)

latent_means <- as.data.frame(do.call(rbind, latent_means))
latent_means$Sex <- c(rep("Women", 266), rep("Men", 244))

options(scipen = 999)
tapply(latent_means$RS, latent_means$Sex, mean) 

tapply(latent_means$RS, latent_means$Sex, mean) * #latent mean
  tapply(res.data$sum, res.data$Sex, sd, na.rm = T) + #real sum
  tapply(res.data$sum, res.data$Sex, mean, na.rm = T) #real sd

## ----effectsize, message = F--------------------
library(MOTE)
M <- tapply(predicted_scores$sum, predicted_scores$Sex, mean)
SD <- tapply(predicted_scores$sum, predicted_scores$Sex, sd)
N <- tapply(predicted_scores$sum, predicted_scores$Sex, length)

effect_size <- d.ind.t(M[1], M[2], SD[1], SD[2], N[1], N[2], a = .05)
effect_size$estimate
effect_size$statistic

