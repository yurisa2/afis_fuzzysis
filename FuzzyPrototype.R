# PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/FuzzySystem"
PATH <- "/Applications/mampstack-5.6.21-2/apache2/htdocs/afis_fuzzysis"

setwd(PATH)

source(file="include.R")

library(ggplot2)
library(useful)
library(corrplot)
library(FuzzyR)

options(warn=-1)

data <- read.csv("dias_horas_full.csv")
data$radiacao_global_kjm2  <- NULL

data$Dia <- as.Date(data$Dia)
# data <- data[4000:5000,] # para nao fritar CPU

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

data_per_day_max <- aggregate(data[,4:ncol(data)], by=list(data$Dia), FUN=max)
data_per_day_min <- aggregate(data[,4:ncol(data)], by=list(data$Dia), FUN=min)
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
# data_per_day <- tail(data_per_day, n=400)
# Shifts de resultado
data_per_day <- shift.column(data=data_per_day, columns="precipitacao_mm", newNames="precipitacao_mm_1",len=1)
data_per_day <- shift.column(data=data_per_day, columns="precipitacao_mm", newNames="precipitacao_mm_1_bin",len=1)

rownames(data_per_day) <- data_per_day[,1]
data_per_day[,1] <- NULL

data_per_day$precipitacao_mm_1_bin <- ifelse(data_per_day$precipitacao_mm_1_bin > 0 , 1, 0)
data_per_day$precipitacao_mm_1_bin <- factor(data_per_day$precipitacao_mm_1_bin)

# summary(data_per_day)
# str(data_per_day[complete.cases(data_per_day),])

# Data Por Mes

data_per_month <- cbind(data,format(as.Date(data$Dia), "%m"))
colnames(data_per_month)[ncol(data_per_month)] <- c("mes")
data_per_month$mes <- factor(data_per_month$mes)
data_per_month <- aggregate(data_per_month[,4:ncol(data_per_month)-1], by=list(data_per_month$mes), FUN=mean,na.rm=TRUE)
data_per_month <- data_per_month[,c(1,3,4,5,12,13,14,16)]

# Data Setting
n_col_features <- 1:15 # Define colunas para estudo;
nbin <- 17 # Define a Coluna Binária

data_input <- data_per_day[sample(1:nrow(data_per_day),10),]

result <- result_matrix(data_per_day,data_input,n_col_features,nbin, plots = F)
