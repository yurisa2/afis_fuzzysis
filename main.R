PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/afis_fuzzysis"
setwd(PATH)

# options(warn=-1) # Comment for debug, uncomment for production (supress warnings) # EDITAVEL


source(file="include/functions.R")


data <- read.csv("data/winm5.csv")

direcao_1 <- data[which(data$direcao == 1),]
direcao_1$lucro <- ifelse (direcao_1$lucro>0,1,0)
direcao_1$lucro <- factor(direcao_1$lucro)

str(direcao_1)

print(col_names_func(direcao_1))


possb_feat <- c(7:72) # Define colunas para estudo; # EDITAVEL
nbin <- 6 # Define a Coluna Binaria (resultado) # EDITAVEL



training_data <- direcao_1[(nrow(direcao_1)-1000):(nrow(direcao_1)-100),]
testing_data <- direcao_1[(nrow(direcao_1)-101):(nrow(direcao_1)),]

str(training_data)
str(testing_data)

features <- weight_list_n(training_data,nbin,c(7:72))

# starting_point <- nrow(direcao_1)-100 # Onde comecar a analise (ln dataset), observare que deve ser maior que trailing_size # EDITAVEL
################################################################################

print(paste("Inicio Laco cumulativo:",Sys.time()))

# nALLt0 <- evaluate_afis(20,starting_point,direcao_1,eval_plots=F) # 0 reseta o trailing size para = i
source(file="include/functions.R")


res_mat20 <- evaluate_afis(20,(nrow(training_data)-2000),training_data,possb_feat)
res_mat40 <- evaluate_afis(40,(nrow(training_data)-2000),training_data,possb_feat)
res_mat60 <- evaluate_afis(60,(nrow(training_data)-2000),training_data,possb_feat)
res_mat80 <- evaluate_afis(80,(nrow(training_data)-2000),training_data,possb_feat)
res_mat100 <- evaluate_afis(100,(nrow(training_data)-2000),training_data,possb_feat)
