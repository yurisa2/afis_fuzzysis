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


n_col_features <- c(7:72) # Define colunas para estudo; # EDITAVEL
nbin <- 6 # Define a Coluna Binaria (resultado) # EDITAVEL



training_data <- direcao_1[(nrow(direcao_1)-1000):(nrow(direcao_1)-100),]
testing_data <- direcao_1[(nrow(direcao_1)-101):(nrow(direcao_1)),]

str(training_data)
str(testing_data)

features <- weight_list_n(training_data,nbin,c(7:72))

above_weights <- NULL

for(i in c(7:72)) {
 if(features[i] > 0.5) above_weights <- c(above_weights,i)
}
# starting_point <- nrow(direcao_1)-100 # Onde comecar a analise (ln dataset), observare que deve ser maior que trailing_size # EDITAVEL
################################################################################

print(paste("Inicio Laco cumulativo:",Sys.time()))

# nALLt0 <- evaluate_afis(20,starting_point,direcao_1,eval_plots=F) # 0 reseta o trailing size para = i
source(file="include/functions.R")

res_mat <- evaluate_afis(20,(nrow(training_data)-200),training_data,above_weights,eval_method = "only_1")


objects(res_mat)
res_mat$table