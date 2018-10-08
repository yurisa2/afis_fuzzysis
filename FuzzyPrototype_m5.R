PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/FuzzySystem"
setwd(PATH)

source(file="include.R")

library(useful)

options(warn=-1)


winm5 <- read.csv("winm5.csv")
winm5 <- winm5[sample(1:nrow(winm5),2000),]
# colnames(winm5)


winm5 <- shift.column(data=winm5, columns="lucro", newNames="lucro_1",len=1)

winm5 <- winm5[which(winm5$direcao == 1),]


winm5$lucro_1_bin <- ifelse(winm5$lucro_1 > 0,1 ,0)

# Data Setting
n_col_features <- c(7:40,46:73) # Define colunas para estudo;

nbin <- 75 # Define a Coluna Binária



data_input <- winm5[sample(1:nrow(winm5),10),]
# str(data_test)

data_input <- data.matrix(data_input)


result <- result_matrix(winm5,data_input,n_col_features,nbin, plots = T)
# acc <- accuracy_fis(winm5,data_input,n_col_features,nbin)
