library(devtools)
library(usethis)

#usethis::use_dev_package("lavaan")
#usethis::use_dev_package("semPlot")
#usethis::use_dev_package("rio")
#usethis::use_dev_package("learnr")
#usethis::use_dev_package("learnrhash")
#usethis::use_dev_package("corrplot")
#usethis::use_dev_package("shiny")
#usethis::use_dev_package("psych")
#usethis::use_dev_package("GPArotation")
#usethis::use_dev_package("parameters")
#usethis::use_dev_package("broom")

#usethis::build_readme()

library(rio)
#efa <- import("data/assignment_efa.csv")
#usethis::use_data(caafidata, overwrite = T)

#usethis::use_tutorial("cfabasics", "CFA: Basics", open = interactive())


library(roxygen2)
roxygenize()
devtools::check()

#dir.create("inst")
dir.create("inst/doc")
file.copy(dir("vignettes", full.names=TRUE), "inst/doc", overwrite=TRUE)
