PATH <- 'C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/FuzzySystem'
setwd(PATH)


library(ggplot2)
library(useful)
library(corrplot)
library(FuzzyR)


data <- read.csv("dias_horas_full.csv")

data$Dia <- as.Date(data$Dia)
# data <- data[1:5000,] # para nao fritar CPU


# Data Por dia
data_per_day <- aggregate(data[,4:ncol(data)], by=list(data$Dia), FUN=mean,na.rm=TRUE)

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
n_col_features <- c(1,2,3,10,11,12,13,14,15) # Define colunas para estudo;
nbin <- 19 # Define a Coluna Binária
# ncoluna <- 12


#Functions
normalize <- function(x) {
  return ((x - min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE) - min(x,na.rm=TRUE)))
}

bx_values <- function(obj_data, ncoluna ){
  bp_0 <- boxplot(obj_data[which(obj_data[,nbin]==0) ,ncoluna], plot=F)
  bp_1 <- boxplot(obj_data[which(obj_data[,nbin]==1) ,ncoluna], plot=F)
  bp_0$stats
  bp_1$stats
  min
  bp <- cbind(bp_0$stats,bp_1$stats)
  colnames(bp) <- c("zero","one")

  return(data.frame(bp))
}

weight_list_n <- function(dataset, nbin_func){
  for(i in n_col_features) {

    if(!exists("list_w")){ list_w <- ""}

    data_0 <- dataset[which(dataset[,nbin_func]==0),i]
    data_1 <- dataset[which(dataset[,nbin_func]==1),i]
    rect <- ks.test(data_0,data_1)$statistic
    list_w[i] <- rect
  }

  list_w <- normalize(as.numeric(list_w));

  return(as.numeric(list_w))
}

lista_basica <- weight_list_n(data_per_day,nbin)

nfuzzy_col <- 2

length(n_col_features)

#Create Model and OutPut
modelo_fuzzy <- newfis("modelo_zero")

modelo_fuzzy <- addvar(modelo_fuzzy,"output","OutPutZERO", 0:100)
modelo_fuzzy <- addmf(modelo_fuzzy,"output",1,"Low","trimf", c(0, 0, 50))
modelo_fuzzy <- addmf(modelo_fuzzy,"output",1,"Medium","trimf", c(0, 50, 100))
modelo_fuzzy <- addmf(modelo_fuzzy,"output",1,"High","trimf", c(50, 100, 100))

# Create Inputs
i_mf <- 1
for(i in 1:length(n_col_features)) {
  bx <- bx_values(data_per_day, n_col_features[i])

  modelo_fuzzy <- addvar(modelo_fuzzy,"input", paste("InputsZERO_",colnames(data_per_day)[n_col_features[i]]), min(bx$zero):max(bx$zero))
  modelo_fuzzy <- addmf(modelo_fuzzy,"input",i_mf,"a3","trimf", c(bx$zero[1], bx$zero[1],bx$zero[2]))
  modelo_fuzzy <- addmf(modelo_fuzzy,"input",i_mf,"a2","trimf", c(bx$zero[1], bx$zero[2],bx$zero[3]))
  modelo_fuzzy <- addmf(modelo_fuzzy,"input",i_mf,"1","trimf", c(bx$zero[2], bx$zero[3],bx$zero[4]))
  modelo_fuzzy <- addmf(modelo_fuzzy,"input",i_mf,"b2","trimf", c(bx$zero[3], bx$zero[4],bx$zero[5]))
  modelo_fuzzy  <- addmf(modelo_fuzzy,"input",i_mf,"b3","trimf", c(bx$zero[4], bx$zero[5],bx$zero[5]))

  i_mf <- i_mf + 1
}

# Create Rules



showfis(modelo_fuzzy)
