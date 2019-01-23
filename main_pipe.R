PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/afis_fuzzysis"
setwd(PATH)

options(warn=-1) # Comment for debug, uncomment for production (supress warnings)

source(file="include/functions.R")

# data <- read.csv("data/Xeon.EURUSD.p60.M30.csv")
data <- read.csv("data.csv")
# data$lucro <- ifelse(data$delta > 0.00015,1,0)
# Get Col Numbers by name
# print(col_names_func(data))

possb_feat <- c(5:ncol(data)) # To work with auto select features, define the possible features (col numbers)
nbin <- 3 # Target binary column (factor 0;1)

weights_type <- "dynamic"
################################################################################

print(paste("Start!:",Sys.time()))

res_mat100 <- evaluate_afis(100,(nrow(data)-200),data,possb_feat,eval_method = "sc")
summary(res_mat100)
