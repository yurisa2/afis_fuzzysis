library(FuzzyR)
library(caret)

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

weight_list_n <- function(dataset, nbin, features){
  dataset <- dataset[complete.cases(dataset),]
  for(i in features) {
    # for(i in ncol(dataset)) {

    if(!exists("list_w")){ list_w <- ""}

    data_0 <- dataset[which(dataset[,nbin]==0),i]
    data_1 <- dataset[which(dataset[,nbin]==1),i]

    rect <- ks.test(data_0,data_1)$statistic
    list_w[i] <- rect
  }

  list_w <- normalize(as.numeric(list_w));

  # print(list_w)

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

create_fuzzy_outputs <- function(fuzzy_model, plots = F){
  fuzzy_model <- addvar(fuzzy_model,"output","Output MODEL", c(0,100))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Low","trimf", c(0, 0, 50))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Medium","trimf", c(0, 50, 100))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"High","trimf", c(50, 100, 100))
  return(fuzzy_model)
}

# Create Inputs
create_fuzzy_inputs <- function(fuzzy_model,dataset,bx_class = "zero",features,nbin, plots = F){

  i_mf <- 1
  for(i in 1:length(features)) {
    bx <- bx_values(dataset, features[i],nbin)
    # feature_weight <- weight_list_n(dataset,nbin,features)[features[i]]
    if(bx_class == "zero") max_min <- c(min(bx$zero),max(bx$zero))
    if(bx_class == "one") max_min <- c(min(bx$one),max(bx$one))

    fuzzy_model <- addvar(fuzzy_model,"input", colnames(dataset)[features[i]],max_min )
    fuzzy_model <- addmf(fuzzy_model,"input",i_mf,paste0("a3",colnames(dataset)[features[i]]),"trimf", c(bx[1,bx_class], bx[1,bx_class],bx[2,bx_class]))
    fuzzy_model <- addmf(fuzzy_model,"input",i_mf,paste0("a2",colnames(dataset)[features[i]]),"trimf", c(bx[1,bx_class], bx[2,bx_class],bx[3,bx_class]))
    fuzzy_model <- addmf(fuzzy_model,"input",i_mf,paste0("1",colnames(dataset)[features[i]]),"trimf", c(bx[2,bx_class], bx[3,bx_class],bx[4,bx_class]))
    fuzzy_model <- addmf(fuzzy_model,"input",i_mf,paste0("b2",colnames(dataset)[features[i]]),"trimf", c(bx[3,bx_class], bx[4,bx_class],bx[5,bx_class]))
    fuzzy_model  <- addmf(fuzzy_model,"input",i_mf,paste0("b3",colnames(dataset)[features[i]]),"trimf", c(bx[4,bx_class], bx[5,bx_class],bx[5,bx_class]))

    i_mf <- i_mf + 1
  }

  return(fuzzy_model)
}


######## RETURNS 2col, [FIS0,FIS1] ########
fuz_sis <- function(dataset,data_test,features,nbin, plots = F){
  if(missing(plots)) plots <- F

  data_test <- data.matrix(data_test)
  data_test <- data_test[,features]

  model_zero <- newfis("model_zero")
  model_zero <- create_fuzzy_inputs(model_zero,dataset,"zero",features,nbin, plots)
  model_zero <- create_fuzzy_outputs(model_zero)
  rules <- create_fuzzy_rules(dataset,features)
  model_zero <- addrule(model_zero,rules)

  # print(rules) # DEBUG

  model_one <- newfis("model_one")
  model_one <- create_fuzzy_inputs(model_one,dataset,"one",features,nbin, plots)
  model_one <- create_fuzzy_outputs(model_one, plots = F)
  rules <- create_fuzzy_rules(dataset,features)
  model_one <- addrule(model_one,rules)

  # print(rules) # DEBUG

  EVzero <- evalfis(data_test,model_zero)
  EVone <- evalfis(data_test,model_one)


  total <- cbind(EVzero,EVone)
  colnames(total) <- c("FIS0","FIS1")


  if(plots == T) plots_afis(dataset,features,nbin,model_zero,model_one)

  return(data.frame(total))
}
############################################

result_matrix <- function(dataset,data_test,features,nbin, plots = F) {
  if(missing(plots)) plots <- F
  d_bench <- data_test

  evaluation <- fuz_sis(dataset,data_test,features,nbin, plots)
  # EVresult_fis <- ifelse(evaluation$one > 50,1,0)

  return_result_matrix <- cbind(evaluation,
    Benchmark=as.character(d_bench[,nbin]),
    Eval0=ifelse(evaluation$FIS0 > 50,1,0),
    Eval1=ifelse(evaluation$FIS1 > 50,1,0)
  )
  # return_result_matrix <- data.frame(return_result_matrix)
  return(data.frame(return_result_matrix))
  # return(total)
}

accuracy_fis <- function(
  dataset,
  data_test,
  features,
  nbin,
  plots = F,
  method = "only_1") {
    if(missing(plots)) plots <- F

    total <- result_matrix(dataset,data_test,features,nbin,plots)

    if(method == "only_1") prove <- factor(total$Eval1)
    if(method == "only_0") prove <- factor(total$Eval0)
    if(method == "conservative")
    {
      total$col_sum <- total$Eval0 + total$Eval1
      if(total$col_sum == 2) {
        total$col_sum <- 0
        prove <- factor(total$col_sum)
      } else {
        prove <- factor(total$Eval1)
      }
    }
    if(method == "sc") # Super Conservative
    {
      total$cond_0 <- ifelse(total$Eval0 < 50,1,0)
      total$cond_1 <- ifelse(total$Eval1 > 50,1,0)
      if(total$cond_0 == 1 && total$cond_1 == 1) {
        total$result <- 1
        prove <- factor(total$result)
      } else {
        total$result <- 0
        prove <- factor(total$result)
      }
    }

    conf_mat <- confusionMatrix(prove,factor(total$Benchmark))
    return(conf_mat)
  }

plots_afis <- function(dataset,features,nbin,model_zero,model_one) {
    for(i in 1:length(model_one$input)) {
      col_name_var <- model_one$input[[i]]$name
      col_num_var <- which( colnames(dataset)==col_name_var )

      par(mfrow=c(2,2))

      feature_weight <- round(weight_list_n(dataset,nbin,features)[col_num_var],3)

      plotmf(model_one, "input", i,main =paste(col_num_var,"ONE",model_one$input[[i]]$name))
      plotmf(model_zero, "input", i,main =paste(col_num_var,"ZERO",model_zero$input[[i]]$name))
      boxplot(dataset[,col_num_var]~dataset[,nbin],main =paste(col_num_var,"Weight",feature_weight))
      hist(dataset[,col_num_var],main = paste(col_num_var,"Dist.",model_one$input[[i]]$name))
    }
  }
