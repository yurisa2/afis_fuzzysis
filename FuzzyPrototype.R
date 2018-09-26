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

nbin <- 19 #Define a Coluna Binária
# ncoluna <- 12



bx_values <- function(obj_data, ncoluna ){

bp_0 <- boxplot(obj_data[which(obj_data[,nbin]==0) ,ncoluna], plot=F)
bp_1 <- boxplot(obj_data[which(obj_data[,nbin]==1) ,ncoluna], plot=F)
bp_0$stats
bp_1$stats


bp <- cbind(bp_0$stats,bp_1$stats)
colnames(bp) <- c("zero","one")
return(bp)
}

bp_1 <- boxplot(data_per_day[which(data_per_day[,nbin]==1) ,10])


bx_values(data_per_day, 9)

genmf("rule3a", 0:50, c(0, 25, 50, 50))



modelo_fuzzy <- newfis("modelo")

fis_MF <- addvar(modelo_fuzzy,"input", 9, 0:100)
mf3a <- addmf(fis_MF,"input",1,"emeefe","trimf", c(0, 25, 50))
mf3b <- addmf(fis_MF,"input",2,"emeefe","trimf", c(0, 25, 50))
mf2a <- addmf(fis_MF,"input",3,"emeefe","trimf", c(0, 25, 50))
mf2b <- addmf(fis_MF,"input",4,"emeefe","trimf", c(0, 25, 50))
mf1  <- addmf(fis_MF,"input",5,"emeefe","trimf", c(0, 25, 50))
