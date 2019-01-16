PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/afis_fuzzysis"
setwd(PATH)

library(plumber)
library(rjson)

requesecao <- 0

r <<- plumb("write.R")  # Where 'plumber.R' is the location of the file shown above
r$run(port=8000)


