PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/afis_fuzzysis"
setwd(PATH)

source(file="include/functions.R")

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /writecsv
function(req){
  requesecao <<- req
  
  data_json <<- fromJSON(requesecao$postBody)
  
  cool_data <<- 0
  
  for(i in 1:length(data_json)) {
    temp_json <- as.data.frame(data_json[[i]])
    
    cool_data <<- rbind(cool_data,temp_json)
  }
  
  cool_data <<- cool_data[complete.cases(cool_data),]
  
  cool_data[] <<- lapply(cool_data, function(x) {
    if(is.factor(x)) as.numeric(as.character(x)) else x
  })
  
  for(i in 1:ncol(cool_data)) cool_data[,i] <<- as.numeric(as.character(cool_data[,i]))
  
  cool_data$lucro <<- as.numeric(cool_data$lucro) * as.numeric(cool_data$Hilo_Direcao)
  cool_data[,2] <<- ifelse (cool_data[,2] > 0,1,0)
  #  cool_data$lucro <<- factor(cool_data$lucro)
  
  possb_feat <- c(4:61)
  nbin <- 2 # Target binary column (factor 0;1)
  
  weights_type <- "dynamic"
  
  training_data <<- cool_data[2:nrow(cool_data),]
  
  data_input <<- cool_data[1,]
  
  n_col_features <- auto_feature_selector(training_data,nbin,possb_feat)
  
  
  
  result_ma_now <<- result_matrix(training_data,
                                  data_input,
                                  n_col_features,
                                  nbin,
                                  plots=F,
                                  method = "only_1")
  
  
 print(result_ma_now$Eval1)
 #  print(1)
}


