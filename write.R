#PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/afis_fuzzysis"
#setwd(PATH)

#source(file="include/functions.R")


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
  

  print("requesecao$postBody")
}
 