unlink("PA1_template.html")
unlink("PA1_template_files", recursive=TRUE)
unlink("PA1_template.md")
unlink("figure", recursive=TRUE)
unlink("PA1_template.R")
library(knitr)
# create an html version along with the md version
knit2html("PA1_template.Rmd")
# create a tangled version also, so we can run it stand-alone
knit("PA1_template.Rmd", output = "PA1_template.R", tangle = TRUE)
