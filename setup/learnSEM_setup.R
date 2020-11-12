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

#usethis::build_readme()

library(rio)
#efa <- import("data/assignment_efa.csv")
#usethis::use_data(efa, overwrite = T)

#usethis::use_tutorial("efa", "Your First Lesson", open = interactive())


library(roxygen2)
roxygenize()
devtools::check()
