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
#usethis::use_dev_package("mirt")
#usethis::use_dev_package("ltm")
#usethis::use_dev_package("MOTE")
#usethis::use_news_md()
#usethis::use_pkgdown()

#usethis::build_readme()

library(rio)
#efa <- import("data/assignment_efa.csv")
#usethis::use_data(mirtdata, overwrite = T)

#usethis::use_tutorial("irt", "Item Response Theory", open = interactive())


library(roxygen2)
roxygenize()
devtools::check()
tools::buildVignettes(dir = ".", tangle=TRUE)

#dir.create("inst")
dir.create("inst/doc")
file.copy(dir("vignettes", full.names=TRUE), "inst/doc", overwrite=TRUE)
pkgdown::build_site()

# pull and then reinstall
devtools::install_github("doomlab/learnSEM")
