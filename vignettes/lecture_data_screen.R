## ---- include = FALSE-----------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include=FALSE-------------------------------
knitr::opts_chunk$set(echo = TRUE)

## -------------------------------------------------------
library(rio)
master <- import("data/lecture_data_screen.csv")
names(master)

## -------------------------------------------------------
#summary(master)
table(master$JOL_group)

table(master$type_cue)

## -------------------------------------------------------
no_typos <- master
no_typos$JOL_group <- factor(no_typos$JOL_group,
                             levels = c("delayed", "immediate"),
                             labels = c("Delayed", "Immediate"))

no_typos$type_cue <- factor(no_typos$type_cue, 
                            levels = c("cue only", "stimulus pairs"),
                            labels = c("Cue Only", "Stimulus Pairs"))

## -------------------------------------------------------
summary(no_typos)

## -------------------------------------------------------
# how did I get 3:22?
# how did I get the rule?
# what should I do? 
no_typos[ , 3:22][ no_typos[ , 3:22] > 100 ]

no_typos[ , 3:22][ no_typos[ , 3:22] > 100 ] <- NA

no_typos[ , 3:22][ no_typos[ , 3:22] < 0 ] <- NA

## -------------------------------------------------------
no_missing <- no_typos
summary(no_missing)

## -------------------------------------------------------
percent_missing <- function(x){sum(is.na(x))/length(x) * 100}
missing <- apply(no_missing, 1, percent_missing)
table(missing)

## -------------------------------------------------------
replace_rows <- subset(no_missing, missing <= 5)
no_rows <- subset(no_missing, missing > 5)

## -------------------------------------------------------
missing <- apply(replace_rows, 2, percent_missing)
table(missing)

replace_columns <- replace_rows[ , 3:22]
no_columns <- replace_rows[ , 1:2]

## -------------------------------------------------------
library(mice)
tempnomiss <- mice(replace_columns)

## -------------------------------------------------------
fixed_columns <- complete(tempnomiss)
all_columns <- cbind(no_columns, fixed_columns)
all_rows <- rbind(all_columns, no_rows)
nrow(no_missing)
nrow(all_rows)

## -------------------------------------------------------
mahal <- mahalanobis(all_columns[ , -c(1,2)], #take note here 
  colMeans(all_columns[ , -c(1,2)], na.rm=TRUE),
  cov(all_columns[ , -c(1,2)], use ="pairwise.complete.obs"))

cutoff <- qchisq(p = 1 - .001, #1 minus alpha
                 df = ncol(all_columns[ , -c(1,2)])) # number of columns

## -------------------------------------------------------
cutoff

summary(mahal < cutoff) #notice the direction 

no_outliers <- subset(all_columns, mahal < cutoff)

## -------------------------------------------------------
library(corrplot)
corrplot(cor(no_outliers[ , -c(1,2)]))

## -------------------------------------------------------
random_variable <- rchisq(nrow(no_outliers), 7)
fake_model <- lm(random_variable ~ ., 
                 data = no_outliers[ , -c(1,2)])
standardized <- rstudent(fake_model)
fitvalues <- scale(fake_model$fitted.values)

## -------------------------------------------------------
plot(fake_model, 2)

## -------------------------------------------------------
hist(standardized)

## -------------------------------------------------------
{plot(standardized, fitvalues)
  abline(v = 0)
  abline(h = 0)
}

