PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/afis_fuzzysis"
setwd(PATH)

options(warn=-1) # Comment for debug, uncomment for production (supress warnings) # EDITAVEL

library("parallel")
library("doSNOW")

cl <- makeCluster(4) #change the 2 to your number of CPU cores
registerDoSNOW(cl)

source(file="include/functions.R")

arq <- list.files(path = "data", pattern = "*.csv")

list_full <- NULL


foreach(file_idx=arq) %dopar% {

data <- read.csv(paste0("data/",file_idx))

direcao_1 <- data[which(data$direcao == 1),]
direcao_1$lucro <- ifelse (direcao_1$lucro>0,1,0)
direcao_1$lucro <- factor(direcao_1$lucro)

# str(direcao_1)

# print(col_names_func(direcao_1))


possb_feat <- c(7:72) # Define colunas para estudo; # EDITAVEL
nbin <- 6 # Define a Coluna Binaria (resultado) # EDITAVEL


features <- weight_list_n(direcao_1,nbin,c(7:72))

# starting_point <- nrow(direcao_1)-100 # Onde comecar a analise (ln dataset), observare que deve ser maior que trailing_size # EDITAVEL
################################################################################

print(paste("Inicio Laco cumulativo:",Sys.time()))

# nALLt0 <- evaluate_afis(20,starting_point,direcao_1,eval_plots=F) # 0 reseta o trailing size para = i
source(file="include/functions.R")


res_mat100 <- evaluate_afis(100,(nrow(direcao_1)-2000),direcao_1,possb_feat)

list_line <- c(file_idx,res_mat100$byClass[3],res_mat100$byClass[4])

list_full <- rbind(list_full,list_line)

}

stopCluster(cl)

                write.csv(list_full,file="compra_multiCPU.csv")
