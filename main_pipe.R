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

# only_1 <- evaluate_afis(100,(nrow(data)-100),data,possb_feat,eval_method = "only_1", eval_return = "matrix")
# conservative <- evaluate_afis(100,(nrow(data)-100),data,possb_feat,eval_method = "conservative", eval_return = "matrix")
# conservative2 <- evaluate_afis(100,(nrow(data)-100),data,possb_feat,eval_method = "conservative2", eval_return = "matrix")
# sc <- evaluate_afis(100,(nrow(data)-100),data,possb_feat,eval_method = "sc", eval_return = "matrix")
# sc2 <- evaluate_afis(100,(nrow(data)-100),data,possb_feat,eval_method = "sc2", eval_return = "matrix")
fuzzy50 <- evaluate_afis(100,(nrow(data)-100),data,possb_feat,eval_method = "fuzzy55", eval_return = "matrix")
# fuzzy60 <- evaluate_afis(100,(nrow(data)-100),data,possb_feat,eval_method = "fuzzy60", eval_return = "matrix")
# fuzzy70 <- evaluate_afis(100,(nrow(data)-100),data,possb_feat,eval_method = "fuzzy70", eval_return = "matrix")
