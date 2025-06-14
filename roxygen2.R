#
# (1) Make R packages available
#

library("devtools")
library("roxygen2")


#
# (2) Create documentation file(s)
#

document()


#
# (3) Build R package and PDF file with help pages
#

build(args = "--compact-vignettes=gs+qpdf")
build_manual()


#
# (4) Install R package
#

install(build_vignettes = TRUE)


#
# (5) Check R package
#

check(build_args = "--compact-vignettes=gs+qpdf")


#
# (6) Check examples
#

setwd("..")
run_examples("netmeta", fresh = TRUE,
             run_dontrun = TRUE, run_donttest = TRUE)
warnings()
