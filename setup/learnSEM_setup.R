library(devtools)
library(usethis)

#usethis::use_dev_package("lavaan")
#usethis::use_dev_package("semPlot")
#usethis::use_dev_package("rio")
#usethis::use_dev_package("learnr")
#usethis::use_dev_package("learnrhash")
#usethis::use_dev_package("corrplot")
#usethis::use_dev_package("shiny")

#usethis::build_readme()

library(rio)
#datascreen <- import("data/introR.csv")
#usethis::use_data(datascreen, overwrite = T)

#usethis::use_tutorial("datascreen", "Your First Lesson", open = interactive())


library(roxygen2)
roxygenize()
devtools::check()
