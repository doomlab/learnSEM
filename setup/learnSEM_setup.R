library(devtools)
library(usethis)

usethis::use_dev_package("lavaan")
usethis::use_dev_package("semPlot")
#usethis::use_dev_package("rio")

library(rio)
#introR <- import("data/introR.csv")
#usethis::use_data(introR, overwrite = T)

library(roxygen2)
roxygenize()
devtools::check()
