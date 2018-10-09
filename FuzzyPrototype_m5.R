PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/FuzzySystem"
setwd(PATH)

source(file="include.R")

library(useful)

options(warn=-1)

winm5 <- read.csv("winm5.csv")
# winm5 <- winm5[sample(1:nrow(winm5),2000),]
# colnames(winm5)

winm5 <- shift.column(data=winm5, columns="lucro", newNames="lucro_1",len=1)

winm5$lucro_1_bin <- ifelse(winm5$lucro_1 > 0, 1, 0)

# Data Setting
winm5 <- winm5[which(winm5$direcao == 1),] #choose direction
winm5_input <- winm5

# Cols for model
n_col_features <- c(7:40,46:73) # Define colunas para estudo;
remove_cols <- c(10,22,33,34,59,70)
n_col_features <- setdiff(n_col_features,remove_cols)
nbin <- 75 # Define a Coluna Binária

# Train Set for model
train_size <- 200
rand_start <- sample(train_size:nrow(winm5),1)
rand_end <- rand_start + train_size
# winm5 <- winm5[rand_start:rand_end,]

# Test Set for model
input_size <- 50
input_start <- rand_end
input_end <- input_size + input_start
data_input <- winm5_input[input_start:input_end,]

rm(acc1)
rm(acc0)
rm(acc_c)
rm(acc_sc)
# result <- result_matrix(winm5,data_input,n_col_features,nbin, plots = F)
acc1 <- accuracy_fis(winm5,data_input,n_col_features,nbin, method = "only_1")
acc0 <- accuracy_fis(winm5,data_input,n_col_features,nbin, method = "only_0")
acc_c <- accuracy_fis(winm5,data_input,n_col_features,nbin, method = "conservative")
acc_sc <- accuracy_fis(winm5,data_input,n_col_features,nbin, method = "sc")
