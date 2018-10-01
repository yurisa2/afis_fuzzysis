#Functions
normalize <- function(x) {
  return ((x - min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE) - min(x,na.rm=TRUE)))
}

bx_values <- function(obj_data, ncoluna, nbin){
  bp_0 <- boxplot(obj_data[which(obj_data[,nbin]==0) ,ncoluna], plot=F)
  bp_1 <- boxplot(obj_data[which(obj_data[,nbin]==1) ,ncoluna], plot=F)
  bp_0$stats
  bp_1$stats
  min
  bp <- cbind(bp_0$stats,bp_1$stats)
  colnames(bp) <- c("zero","one")

  return(data.frame(bp))
}

weight_list_n <- function(dataset, nbin,features){
  for(i in features) {

    if(!exists("list_w")){ list_w <- ""}

    data_0 <- dataset[which(dataset[,nbin]==0),i]
    data_1 <- dataset[which(dataset[,nbin]==1),i]
    rect <- ks.test(data_0,data_1)$statistic
    list_w[i] <- rect
  }

  list_w <- normalize(as.numeric(list_w));

  return(as.numeric(list_w))
}

create_fuzzy_rules <- function(dataset,features) {

  total_col <- length(features) + 3
  m <- matrix(0L, nrow = 5*length(features), ncol = total_col)
  m[,total_col] <- 1 # Add 1 to last col as in AND

  j <- 1
  for(i in 1:length(features))
  {
    feature_weight <- weight_list_n(dataset,nbin,features)[features[i]]
    m[j:(j+4),i] <- c(1,2,3,4,5) #input MFs
    # m[j:(j+4),(total_col-1)] <- ifelse(feature_weight == 0,0.000000001,feature_weight) #Calculate Weights IF For not 0;
    m[j:(j+4),(total_col-1)] <- feature_weight
    # m[j:(j+4),(total_col-1)] <- 1
    m[j:(j+4),(total_col-2)] <- c(1,2,3,2,1) #Output MFs
    j <- j + 5 # Goto Next 5 Lines
  }

  return(m)
}

ma_test <-matrix(0L, nrow = 5, ncol = 9)  # @nrussell
ma_test[,2] <- c(1,2,3,4,5)

#Create Model and OutPut

create_fuzzy_outputs <- function(fuzzy_model){
  fuzzy_model <- addvar(fuzzy_model,"output","Output MODEL", c(0,100))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Low","trimf", c(0, 0, 50))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Medium","trimf", c(0, 50, 100))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"High","trimf", c(50, 100, 100))
  return(fuzzy_model)
}

# Create Inputs
create_fuzzy_inputs <- function(fuzzy_model,dataset,bx_class = "zero",features,nbin){
  i_mf <- 1
  for(i in 1:length(features)) {
    bx <- bx_values(dataset, features[i],nbin)
    fuzzy_model <- addvar(fuzzy_model,"input", colnames(dataset)[features[i]], c(min(bx$zero),max(bx$zero)))
    fuzzy_model <- addmf(fuzzy_model,"input",i_mf,paste0("a3",colnames(dataset)[features[i]]),"trimf", c(bx[1,bx_class], bx[1,bx_class],bx[2,bx_class]))
    fuzzy_model <- addmf(fuzzy_model,"input",i_mf,paste0("a2",colnames(dataset)[features[i]]),"trimf", c(bx[1,bx_class], bx[2,bx_class],bx[3,bx_class]))
    fuzzy_model <- addmf(fuzzy_model,"input",i_mf,paste0("1",colnames(dataset)[features[i]]),"trimf", c(bx[2,bx_class], bx[3,bx_class],bx[4,bx_class]))
    fuzzy_model <- addmf(fuzzy_model,"input",i_mf,paste0("b2",colnames(dataset)[features[i]]),"trimf", c(bx[3,bx_class], bx[4,bx_class],bx[5,bx_class]))
    fuzzy_model  <- addmf(fuzzy_model,"input",i_mf,paste0("b3",colnames(dataset)[features[i]]),"trimf", c(bx[4,bx_class], bx[5,bx_class],bx[5,bx_class]))
    plotmf(fuzzy_model, "input", i_mf)
    i_mf <- i_mf + 1
  }

  return(fuzzy_model)
}


######## RETURNS 2col, [FIS0,FIS1] ########
fuz_sis <- function(dataset,data_test,features,nbin){
  model_zero <- newfis("model_zero")
  model_zero <- create_fuzzy_inputs(model_zero,dataset,"zero",features,nbin)
  model_zero <- create_fuzzy_outputs(model_zero)
  rules <- create_fuzzy_rules(dataset,features)
  model_zero <- addrule(model_zero,rules)

  model_one <- newfis("model_one")
  model_one <- create_fuzzy_inputs(model_one,dataset,"one",features,nbin)
  model_one <- create_fuzzy_outputs(model_one)
  rules <- create_fuzzy_rules(dataset,features)
  model_one <- addrule(model_one,rules)

  EVzero <- evalfis(data_test,model_zero)
  EVone <- evalfis(data_test,model_one)


  total <- cbind(EVzero,EVone)
  colnames(total) <- c("FIS0","FIS1")

  return(data.frame(total))
}
############################################

result_matrix <- function(dataset,data_test,features,nbin) {

  evaluation <- fuz_sis(dataset,data_test,features,nbin)
  # EVresult_fis <- ifelse(evaluation$one > 50,1,0)

  return_result_matrix <- cbind(evaluation,
    Benchmark=as.character(dataset[,nbin]),
    Eval0=ifelse(evaluation$FIS0 > 50,1,0),
    Eval1=ifelse(evaluation$FIS1 > 50,1,0)
  )
  # return_result_matrix <- data.frame(return_result_matrix)
  return(data.frame(return_result_matrix))
  # return(total)
}

accuracy_fis <- function(dataset,data_test,features,nbin) {
  total <- result_matrix(dataset,data_test,features,nbin)

# FAZER A PORRA DA ESTATISTICA DIREITO
  daB <- data_test

  total <- cbind(total,as.character(daB),ifelse(total[,2] > 50,1,0))

  zero_rows = which(total$Eval0 == 0)

  result_total <- ifelse(total[,3] == total[,4],1,0)
  result_1 <- ifelse(,1,0)
  result_0 <- ifelse(,1,0)


  mean_total <- mean(result_total, na.rm=T)
  mean1 <- mean(result_1)
  mean0 <- mean(result_0)

  el_return <- cbind(mean_total,mean0,mean1)
  el_return <- data.frame(el_return)
  colnames(el_return) <- c("% Total","% 0","% 1")

  return(el_return)
}



library(ggplot2)
library(useful)
library(corrplot)
library(FuzzyR)

options(warn=-1)


data <- read.csv("../input/dias_horas_full.csv")
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

summary(data_per_day)
str(data_per_day[complete.cases(data_per_day),])

# Data Por Mes

data_per_month <- cbind(data,format(as.Date(data$Dia), "%m"))
colnames(data_per_month)[ncol(data_per_month)] <- c("mes")
data_per_month$mes <- factor(data_per_month$mes)
data_per_month <- aggregate(data_per_month[,4:ncol(data_per_month)-1], by=list(data_per_month$mes), FUN=mean,na.rm=TRUE)
data_per_month <- data_per_month[,c(1,3,4,5,12,13,14,16)]

# Data Setting
n_col_features <- 1:15 # Define colunas para estudo;
# str(data_per_day)
# n_col_features <- c(1,2) # Define colunas para estudo;
nbin <- 17 # Define a Coluna Binária
# ncoluna <- 12

# Create Rules

# Conc

data_test <- data_per_day[,n_col_features]
data_test2 <- data_per_day[,nbin]

data_input <- data_test

data_input <- data.matrix(data_input)
# #
# evaluation <- fuz_sis(data_per_day,data_input,n_col_features,nbin)
#
#
#
# EVresult_fis <- ifelse(EVone > 50,1,0)
#
# total <- cbind(EVzero,EVone,as.character(data_test2),EVresult_fis)
#
# result_total <- ifelse(total[,3] == total[,4],1,0)
# result_1 <- ifelse(total[which(total[,3] == 1),3] == total[which(total[,3] == 1),4],1,0)
# result_0 <- ifelse(total[which(total[,3] == 0),3] == total[which(total[,3] == 0),4],1,0)

# mean(result_total, na.rm=T) #0.579227388387694 | MEAN
# mean(result_1)#0.399916422900125 | MEAN
# mean(result_0)#0.810950413223141 | MEAN
# 0.58004158004158
# 0.393230254910155
# 0.810950413223141


result_matrix(data_per_day,data_input,n_col_features,nbin)
# accuracy_fis(data_per_day,data_input,n_col_features,nbin)

# accuracy_fis(data_per_day,data_input,n_col_features,nbin)
#

# str(total)
#
# total <- data.frame(as.numeric(total))
# boxplot(as.numeric(total[,2])~total[,3]) #Hangover Analysis
