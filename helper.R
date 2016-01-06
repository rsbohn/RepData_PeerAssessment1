#helper.R
#Randall Bohn
#2016-01-06

# build the project using knit2html
setwd("/users/rsboh/coursera/peer1")
library(knitr)
knit2html("PA1_template.Rmd")

# now you should commit the files
# > git add PA1_template.html PA1_template.md figure
# > git commit ...