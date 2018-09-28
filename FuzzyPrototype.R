PATH <- 'C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/FuzzySystem'
# PATH <- '/Applications/mampstack-5.6.21-2/apache2/htdocs/afis_fuzzysis'
setwd(PATH)


source(file="include.R")

# install.packages("shiny")

library(ggplot2)
library(useful)
library(corrplot)
library(FuzzyR)

options(warn=-1)


data <- read.csv("dias_horas_full.csv")

data$Dia <- as.Date(data$Dia)
data <- data[40:50,] # para nao fritar CPU

# Data Por dia
data_per_day <- aggregate(data[,4:ncol(data)], by=list(data$Dia), FUN=sum,na.rm=TRUE)

data_per_day$temp_max_c <- NULL
data_per_day$temp_min_c  <- NULL
data_per_day$tem_max_pto_orvalho_c  <- NULL
data_per_day$tempo_min_pto_orvalho_c  <- NULL
data_per_day$umid_rel_ar_max_pctg  <- NULL
data_per_day$umid_rel_ar_min_pctg  <- NULL
data_per_day$pressao_atmosferica_max_hpa  <- NULL
data_per_day$pressao_atmosferica_min_hpa  <- NULL

data_per_day_max <- aggregate(data[,4:ncol(data)], by=list(data$Dia), FUN=max,na.rm=TRUE)
data_per_day_min <- aggregate(data[,4:ncol(data)], by=list(data$Dia), FUN=min,na.rm=TRUE)
data_per_day_deltas <- data_per_day_max - data_per_day_min

data_per_day_deltas$temp_max_c <- NULL
data_per_day_deltas$temp_min_c  <- NULL
data_per_day_deltas$tem_max_pto_orvalho_c  <- NULL
data_per_day_deltas$tempo_min_pto_orvalho_c  <- NULL
data_per_day_deltas$umid_rel_ar_max_pctg  <- NULL
data_per_day_deltas$umid_rel_ar_min_pctg  <- NULL
data_per_day_deltas$pressao_atmosferica_max_hpa  <- NULL
data_per_day_deltas$pressao_atmosferica_min_hpa  <- NULL
data_per_day_deltas$precipitacao_mm  <- NULL
data_per_day_deltas$delta_vento_raj_max_ms  <- NULL
data_per_day_deltas[,1]  <- NULL

colnames(data_per_day_deltas) <- paste0("delta_",colnames(data_per_day_deltas))


data_per_day <- cbind(data_per_day,data_per_day_deltas)
# str(data_per_day)
data_per_day <- tail(data_per_day, n=400)
# Shifts de resultado
data_per_day <- shift.column(data=data_per_day, columns="precipitacao_mm", newNames="precipitacao_mm_1",len=1)
data_per_day <- shift.column(data=data_per_day, columns="precipitacao_mm", newNames="precipitacao_mm_1_bin",len=1)

rownames(data_per_day) <- data_per_day[,1]
data_per_day[,1] <- NULL

data_per_day$precipitacao_mm_1_bin <- ifelse(data_per_day$precipitacao_mm_1_bin > 0 , 1, 0)
data_per_day$precipitacao_mm_1_bin <- factor(data_per_day$precipitacao_mm_1_bin)

# Data Por Mes

data_per_month <- cbind(data,format(as.Date(data$Dia), "%m"))
colnames(data_per_month)[ncol(data_per_month)] <- c("mes")
data_per_month$mes <- factor(data_per_month$mes)
data_per_month <- aggregate(data_per_month[,4:ncol(data_per_month)-1], by=list(data_per_month$mes), FUN=mean,na.rm=TRUE)
data_per_month <- data_per_month[,c(1,3,4,5,12,13,14,16)]

# Data Setting
n_col_features <- 1:17 # Define colunas para estudo;
# str(data_per_day)
# n_col_features <- c(1,2) # Define colunas para estudo;
nbin <- 19 # Define a Coluna Binária
# ncoluna <- 12

# Create Rules

# Conc

data_test <- data_per_day[,n_col_features]
data_test2 <- data_per_day[,nbin]

data_input <- data_test

data_input <- data.matrix(data_input)
#
evaluation <- fuz_sis(data_per_day,data_input,n_col_features,nbin)



EVresult_fis <- ifelse(EVone > 50,1,0)

total <- cbind(EVzero,EVone,as.character(data_test2),EVresult_fis)

result_total <- ifelse(total[,3] == total[,4],1,0)
result_1 <- ifelse(total[which(total[,3] == 1),3] == total[which(total[,3] == 1),4],1,0)
result_0 <- ifelse(total[which(total[,3] == 0),3] == total[which(total[,3] == 0),4],1,0)

# mean(result_total, na.rm=T) #0.579227388387694 | MEAN
# mean(result_1)#0.399916422900125 | MEAN
# mean(result_0)#0.810950413223141 | MEAN
# 0.58004158004158
# 0.393230254910155
# 0.810950413223141


result_matrix(data_per_day,data_input,n_col_features,nbin)
accuracy_fis(data_per_day,data_input,n_col_features,nbin)


# accuracy_fis(data_per_day,data_input,n_col_features,nbin)
#


#
# str(total)
#
# total <- data.frame(as.numeric(total))
# boxplot(as.numeric(total[,2])~total[,3]) #Hangover Analysis
