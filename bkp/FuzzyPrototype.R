PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/FuzzySystem"
# PATH <- "/Applications/mampstack-5.6.21-2/apache2/htdocs/afis_fuzzysis"
setwd(PATH)

source(file="include.R")

library(useful)

options(warn=-1)

data <- read.csv("dias_horas_full.csv")
data$radiacao_global_kjm2 <- NULL


# data <- data[4000:5000,] # para nao fritar CPU

 # Adiciona a direcao do vento decomposta
data <- cbind(data,wind_dir(data[,"vento_dir_graus"]))

# Data Por dia
data_per_day_sum <- aggregate(data[,4:ncol(data)], by=list(data$Dia), FUN=sum,na.rm=TRUE)
data_per_day_mean <- aggregate(data[,4:ncol(data)], by=list(data$Dia), FUN=mean,na.rm=TRUE)

colnames(data_per_day_sum) <- paste0("sum_",colnames(data_per_day_sum))
colnames(data_per_day_mean) <- paste0("mean_",colnames(data_per_day_mean))

data_per_day_max <- aggregate(data[,4:ncol(data)], by=list(data$Dia), FUN=max)
data_per_day_min <- aggregate(data[,4:ncol(data)], by=list(data$Dia), FUN=min)
data_per_day_deltas <- data_per_day_max - data_per_day_min

data_per_day_deltas[,1]  <- NULL

colnames(data_per_day_deltas) <- paste0("delta_",colnames(data_per_day_deltas))

data_per_day <- cbind(data_per_day_mean,data_per_day_sum,data_per_day_deltas)

# Shifts de resultado
data_per_day <- shift.column(data=data_per_day, columns="sum_precipitacao_mm", newNames="sum_precipitacao_mm_1",len=1)
data_per_day <- shift.column(data=data_per_day, columns="sum_precipitacao_mm", newNames="sum_precipitacao_mm_1_bin",len=1)

rownames(data_per_day) <- data_per_day[,1]
data_per_day[,1] <- NULL

data_per_day$sum_precipitacao_mm_1_bin <- ifelse(data_per_day$sum_precipitacao_mm_1_bin > 0 , 1, 0)
data_per_day$sum_precipitacao_mm_1_bin <- factor(data_per_day$sum_precipitacao_mm_1_bin)

# str(data_per_day)

# for (i in 1:ncol(data_per_day)){ print(paste(i,colnames(data_per_day)[i])) }

#############


# summary(data_per_day)
# for (i in 1:ncol(data_per_day)){ print(paste(i,colnames(data_per_day)[i])) }
# str(data_per_day[complete.cases(data_per_day),])
#
# # Data Por Mes
#
# data_per_month <- cbind(data,format(as.Date(data$Dia), "%m"))
# colnames(data_per_month)[ncol(data_per_month)] <- c("mes")
# data_per_month$mes <- factor(data_per_month$mes)
# data_per_month <- aggregate(data_per_month[,4:ncol(data_per_month)-1], by=list(data_per_month$mes), FUN=mean,na.rm=TRUE)

# Data Setting
n_col_features <- c(1,2,3,10,11,17,18,19,20,22,23,24,31,32,34,38,39,40,41,
42,43,44,51,52,54,58,59,60,61) # Define colunas para estudo;
nbin <- 63 # Define a Coluna Binária

# summary(data_per_day[complete.cases(data_per_day),n_col_features])

data_per_day <- data_per_day[complete.cases(data_per_day),]
data_per_day_orig <- data_per_day[complete.cases(data_per_day),]
# data_per_day_orig[rand_start:rand_end,]

data_per_day_input <- data_per_day
result_ma <- NULL
train_size <- nrow(data_per_day)
# rand_start <- sample(train_size:nrow(data_per_day),1)


# for(i in 200:(nrow(data_per_day_orig)-train_size)) {
for(i in 200:300) {
  # Train Set for model

  rand_start <- 1
  rand_end <- rand_start + train_size
  # rand_end <- nrow(data_per_day_orig)
  data_per_day <- data_per_day_orig[rand_start:rand_end,]

  # Test Set for model
  input_size <- 0
  input_start <- rand_end
  input_end <- input_size + input_start
  data_input <- data_per_day_input[input_start:input_end,]

  result_ma_now <- result_matrix(data_per_day,data_input,n_col_features,nbin)

  result_ma <- rbind(result_ma,result_ma_now)
}

prove <- NULL
# for(i in nrow(result_ma)) prove[i] <- ifelse(result_ma[i,5] == 1 && result_ma[i,4] == 0, 1,0)
# str(data_per_day)
prove <- factor(result_ma$Eval1)

# result_ma$col_sum <- result_ma$Eval0 + result_ma$Eval1
# if(result_ma$col_sum == 2) {
#   result_ma$col_sum <- 0
#   prove <- factor(result_ma$col_sum)
# } else {
#   prove <- factor(result_ma$Eval1)
# }

confusionMatrix(prove,factor(result_ma$Benchmark),positive = "1")

# rm(acc1); rm(acc0); rm(acc_c); rm(acc_sc)

# acc1 <- accuracy_fis(data_per_day,data_input,n_col_features,nbin, method = "only_1")
# acc0 <- accuracy_fis(data_per_day,data_input,n_col_features,nbin, method = "only_0")
# acc_c <- accuracy_fis(data_per_day,data_input,n_col_features,nbin, method = "conservative")
# acc_sc <- accuracy_fis(data_per_day,data_input,n_col_features,nbin, method = "sc")
