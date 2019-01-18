PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/afis_fuzzysis"
setwd(PATH)

options(warn=-1) # Comment for debug, uncomment for production (supress warnings)

source(file="include/functions.R")

# data <- read.csv("data/Xeon.EURUSD.M30.P60.Venda.csv")
data <- read.csv("data/Xeon.compraWIN.M5.P60.csv")

# Get Col Numbers by name
# print(col_names_func(data))

possb_feat <- c(5:ncol(data)) # To work with auto select features, define the possible features (col numbers)
nbin <- 3 # Target binary column (factor 0;1)

weights_type <- "dynamic"

################################################################################

print(paste("Start!:",Sys.time()))

# res_mat20 <- evaluate_afis(20,(nrow(data)-500),data,possb_feat)
# res_mat40 <- evaluate_afis(40,(nrow(data)-500),data,possb_feat)
# res_mat60 <- evaluate_afis(60,(nrow(data)-500),data,possb_feat)
# res_mat80 <- evaluate_afis(80,(nrow(data)-100),data,possb_feat)
res_mat100 <- evaluate_afis(100,(nrow(data)-500),data,possb_feat)
# res_mat500 <- evaluate_afis(500,(nrow(data)-500),data,possb_feat)
