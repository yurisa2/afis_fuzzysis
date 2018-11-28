PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/afis_fuzzysis"
setwd(PATH)

options(warn=-1) # Comment for debug, uncomment for production (supress warnings) # EDITAVEL


source(file="include/functions.R")

data <- read.csv("data/winm5.csv")

direcao_1 <- data[which(data$direcao == 1),]
direcao_1$lucro <- ifelse (direcao_1$lucro>0,1,0)
direcao_1$lucro <- factor(direcao_1$lucro)

str(direcao_1)

print(col_names_func(direcao_1))


n_col_features <- c(7:10) # Define colunas para estudo; # EDITAVEL
nbin <- 6 # Define a Coluna Binaria (resultado) # EDITAVEL


starting_point <- nrow(direcao_1)-100 # Onde comecar a analise (ln dataset), observare que deve ser maior que trailing_size # EDITAVEL
################################################################################

print(paste("Inicio Laco cumulativo:",Sys.time()))
nALLt0 <- evaluate_afis(20,starting_point,direcao_1,eval_plots=T) # 0 reseta o trailing size para = i
