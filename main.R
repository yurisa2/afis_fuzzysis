PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/afis_fuzzysis"
setwd(PATH)

# options(warn=-1) # Comment for debug, uncomment for production (supress warnings)

source(file="include/functions.R")

data <- read.csv("data/Xaptur.WINN.p20.7863.PERIOD_M10.Stats.csv")

# Simple Feature Engineering (because already did on source data generator)
direcao_1 <- data[which(data$direcao == -1),]
summary(direcao_1$lucro)

direcao_1$lucro <- ifelse (direcao_1$lucro <= 0,1,0)
direcao_1$lucro <- factor(direcao_1$lucro)

# Get Col Numbers by name
# print(col_names_func(direcao_1))

possb_feat <- c(7:70) # To work with auto select features, define the possible features (col numbers)
nbin <- 6 # Target binary column (factor 0;1)

weights_type <- "dynamic"

################################################################################

print(paste("Start!:",Sys.time()))

# res_mat20 <- evaluate_afis(20,(nrow(training_data)-2000),training_data,possb_feat)
# res_mat40 <- evaluate_afis(40,(nrow(training_data)-2000),training_data,possb_feat)
# res_mat60 <- evaluate_afis(60,(nrow(training_data)-2000),training_data,possb_feat)
# res_mat80 <- evaluate_afis(80,(nrow(training_data)-2000),training_data,possb_feat)
res_mat100 <- evaluate_afis(60,(nrow(direcao_1)-100),direcao_1,possb_feat)
# rm(res_mat100)
