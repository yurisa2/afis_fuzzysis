PATH <- 'C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/FuzzySystem'
PATH <- '/Applications/mampstack-5.6.21-2/apache2/htdocs/afis_fuzzysis'
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
# n_col_features <- c(1,2) # Define colunas para estudo;
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

create_fuzzy_rules <- function(dataset) {

  total_col <- length(n_col_features) + 3
  m <- matrix(0L, nrow = 5*length(n_col_features), ncol = total_col)
  m[,total_col] <- 1 # Add 1 to last col as in AND

  j <- 1
  for(i in 1:length(n_col_features))
  {
    feature_weight <- weight_list_n(data_per_day,nbin)[n_col_features[i]]
    m[j:(j+4),i] <- c(1,2,3,4,5) #input MFs
    m[j:(j+4),(total_col-1)] <- ifelse(feature_weight == 0,0.000000001,feature_weight) #Calculate Weights IF For not 0
    m[j:(j+4),(total_col-2)] <- c(1,2,3,2,1) #Output MFs
    j <- j + 5 # Goto Next 5 Lines
  }

  return(m)
}

ma_test <-matrix(0L, nrow = 5, ncol = 9)  # @nrussell
ma_test[,2] <- c(1,2,3,4,5)



#Create Model and OutPut
modelo_fuzzy <- newfis("modelo_zero")

create_fuzzy_outputs <- function(fuzzy_model){
  fuzzy_model <- addvar(fuzzy_model,"output","Output MODEL", c(0,100))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Low","trimf", c(0, 0, 50))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Medium","trimf", c(0, 50, 100))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"High","trimf", c(50, 100, 100))
 return(fuzzy_model)
}



# Create Inputs

create_fuzzy_inputs <- function(fuzzy_model,dataset,bx_class = "zero"){
  i_mf <- 1
  for(i in 1:length(n_col_features)) {
    bx <- bx_values(dataset, n_col_features[i])
    fuzzy_model <- addvar(fuzzy_model,"input", colnames(dataset)[n_col_features[i]], c(min(bx$zero),max(bx$zero)))
    fuzzy_model <- addmf(fuzzy_model,"input",i_mf,paste("a3",colnames(dataset)[n_col_features[i]]),"trimf", c(bx[1,bx_class], bx[1,bx_class],bx[2,bx_class]))
    fuzzy_model <- addmf(fuzzy_model,"input",i_mf,paste("a2",colnames(dataset)[n_col_features[i]]),"trimf", c(bx[1,bx_class], bx[2,bx_class],bx[3,bx_class]))
    fuzzy_model <- addmf(fuzzy_model,"input",i_mf,paste("1",colnames(dataset)[n_col_features[i]]),"trimf", c(bx[2,bx_class], bx[3,bx_class],bx[4,bx_class]))
    fuzzy_model <- addmf(fuzzy_model,"input",i_mf,paste("b2",colnames(dataset)[n_col_features[i]]),"trimf", c(bx[3,bx_class], bx[4,bx_class],bx[5,bx_class]))
    fuzzy_model  <- addmf(fuzzy_model,"input",i_mf,paste("b3",colnames(dataset)[n_col_features[i]]),"trimf", c(bx[4,bx_class], bx[5,bx_class],bx[5,bx_class]))

    i_mf <- i_mf + 1
  }

 return(fuzzy_model)
}


# Create Rules

rules <- create_fuzzy_rules(data_per_day)

modelo_fuzzy <- addrule(modelo_fuzzy,rules)

# Conc

data_test <- data_per_day[2200:2319,n_col_features]

modelo_zero <- newfis("modelo_zero")
modelo_zero <- create_fuzzy_inputs(modelo_zero,data_per_day,"zero")
modelo_zero <- create_fuzzy_outputs(modelo_zero)
rules_teste <- create_fuzzy_rules(data_per_day)
modelo_zero <- addrule(modelo_zero,rules_teste)

modelo_one <- newfis("modelo_one")
modelo_one <- create_fuzzy_inputs(modelo_one,data_per_day,"one")
modelo_one <- create_fuzzy_outputs(modelo_one)
rules_teste <- create_fuzzy_rules(data_per_day)
modelo_one <- addrule(modelo_one,rules_teste)


data_input <- data_test


data_input <- data.matrix(data_input)
# data_input <- matrix(c(25.5,75.5),2,1)
# data_input <- matrix(data_input,2)
EVzero <- evalfis(data_input,modelo_zero)
EVone <- evalfis(data_input,modelo_one)

total <- cbind(EVzero,EVone)

# fis_show <- showfis(modelo_zero)

# Input_data <- matrix((1:2),1,2)
# fis <- tipper()
# showfis(fis)
# evalfis(Input_data, fis
