PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/afis_fuzzysis"
setwd(PATH)


library(plumber)
r <- plumb("plumber.R")  # Where 'plumber.R' is the location of the file shown above
r$run(port=8000)
