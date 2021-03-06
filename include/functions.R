
################################################################################
# FILENAME :        functions.R
#
# DESCRIPTION :
#       Function repository for system
#
# PUBLIC FUNCTIONS :
#       To-Do
#
#
# NOTES :
#
#
#
#
#
# AUTHOR :    @yurisa2        START DATE :    01 Oct 18
#
# CHANGES :
#
#
#
#
################################################################################

library("FuzzyR")
library("caret")

# Functions

# Simple normalization, 0..1 - input is vector
normalize <- function(x) {
  return ((x - min(x,na.rm=TRUE)) / (max(x,na.rm=TRUE) - min(x,na.rm=TRUE)))
}

# Expand grid based on reps
new.expand.grid <- function(input, reps) {
  new_grid <- expand.grid(replicate(reps, input, simplify = FALSE))
  return(new_grid)
}

# Get the values from the boxplot() functions, to build the MF curves
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

# Return the weights normalized using the ks.test for the fuzzy rules
weight_list_n <- function(dataset, nbin, features){
  dataset <- dataset[complete.cases(dataset),]
  # DEBUG
  # data_0_ <- dataset[which(dataset[,nbin]==0),4]
  # data_1_ <- dataset[which(dataset[,nbin]==1),4]

  # print(length(data_0_))
  # print(length(data_1_))
  # /DEBUG


  for(i in features) {
    # for(i in ncol(dataset)) {

    if(!exists("list_w")){ list_w <- ""}

    data_0 <- dataset[which(dataset[,nbin]==0),i]
    data_1 <- dataset[which(dataset[,nbin]==1),i]

    #print(length(data_0))
    #print(length(data_1))

    if((length(data_1) <= 1) || (length(data_0) <= 1)) { rect <- 0
    } else {
    #  print("ks") # DEBUG
    rect <- ks.test(data_0,data_1)$statistic }
    #

    list_w[i] <- rect
  }

  list_w <- normalize(as.numeric(list_w));

  # print(list_w)

  return(as.numeric(list_w))
}

# Analyze and create the fuzzy rules, using weights from previous functions

create_fuzzy_rules <- function(dataset,features, rule_set = "partial", weights_type = "fixed") {
  if(rule_set=="partial") {
      total_col <- length(features) + 3
      m <- matrix(0L, nrow = 5*length(features), ncol = total_col)
      m[,total_col] <- 1 # Add 1 to last col as in AND
      m_test <- m

      j <- 1
      for(i in 1:length(features))
      {
        if(weights_type == "fixed") { feature_weight <- 1 # Fixed and equal weights
        } else {
          feature_weight <- weight_list_n(dataset,nbin,features)[features[i]]
        }
        m[j:(j+4),i] <- c(1,2,3,4,5) #input MFs
        # m[j:(j+4),(total_col-1)] <- ifelse(feature_weight == 0,0.000000001,feature_weight) #Calculate Weights IF For not 0;
        m[j:(j+4),(total_col-1)] <- feature_weight
        # m[j:(j+4),(total_col-1)] <- 1
        m[j:(j+4),(total_col-2)] <- c(1,2,3,2,1) #Output MFs
        j <- j + 5 # Goto Next 5 Lines
      }
    } else {
      # Fazer com que os pesos sejam calculados, normalizados e depois DISTRIBUIDOS (provavelmente media ponterada) para todo o rolê
      # embora até hoje nao tenha feito muita coisa

      total_col_test <- new.expand.grid(c(1,2,3,4,5),as.integer(length(features)))
      total_col_test_soma <- total_col_test
      total_col_test_soma[total_col_test_soma == 4] = 2
      total_col_test_soma[total_col_test_soma == 5] = 1

      res_row <- NULL
      for(j in 1:nrow(total_col_test_soma)) {
        res_row[j] = sum(total_col_test_soma[j,])
      }


      res_row <- normalize(res_row)

      res_row[res_row >= 0.66] = 3
      res_row[res_row > 0.33 & res_row < 0.66 ] = 2
      res_row[res_row <= 0.33 ] = 1

      total_col_test <- cbind(total_col_test,res_row)
      total_col_test <- cbind(total_col_test,1,1)

      m <- as.matrix(total_col_test)

    }

    # write(feature_weight,"feature_weight.txt",append = T) # DEBUG


return(m)
}
#
# ma_test <-matrix(0L, nrow = 5, ncol = 9)  # @nrussell
# ma_test[,2] <- c(1,2,3,4,5)

# Create Model and OutPut
# Add outputs to modem (pre-defined scale), 3 MFs
create_fuzzy_outputs <- function(fuzzy_model, plots = F){
  fuzzy_model <- addvar(fuzzy_model,"output","Output MODEL", c(0,100))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Low","trimf", c(0, 0, 50))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"Medium","trimf", c(0, 50, 100))
  fuzzy_model <- addmf(fuzzy_model,"output",1,"High","trimf", c(50, 100, 100))
  return(fuzzy_model)
}

# Create Inputs
# It uses data from boxplots, creates MF curves and add them to the models
create_fuzzy_inputs <- function(fuzzy_model,
                                dataset,
                                bx_class = "zero",
                                features,
                                nbin,
                                plots = F){
  i_mf <- 1
  for(i in 1:length(features)) {
    bx <- bx_values(dataset, features[i],nbin)
    # feature_weight <- weight_list_n(dataset,nbin,features)[features[i]]
    if(bx_class == "zero") max_min <- c(min(bx$zero),max(bx$zero))
    if(bx_class == "one") max_min <- c(min(bx$one),max(bx$one))

    fuzzy_model <- addvar(fuzzy_model,
                          "input",
                          colnames(dataset)[features[i]],
                          max_min )

    fuzzy_model <- addmf(fuzzy_model,
                        "input",
                        i_mf,
                        paste0("a3",colnames(dataset)[features[i]]),
                        "trimf",
                        c(bx[1,bx_class], bx[1,bx_class], bx[2,bx_class]))
    fuzzy_model <- addmf(fuzzy_model,
                         "input",
                         i_mf,
                         paste0("a2",colnames(dataset)[features[i]]),
                         "trimf",
                         c(bx[1,bx_class], bx[2,bx_class],bx[3,bx_class]))
    fuzzy_model <- addmf(fuzzy_model,
                         "input",
                         i_mf,
                         paste0("1",colnames(dataset)[features[i]]),
                         "trimf",
                         c(bx[2,bx_class], bx[3,bx_class],bx[4,bx_class]))
    fuzzy_model <- addmf(fuzzy_model,
                         "input",
                         i_mf,
                         paste0("b2",colnames(dataset)[features[i]]),
                         "trimf",
                         c(bx[3,bx_class], bx[4,bx_class],bx[5,bx_class]))
    fuzzy_model  <- addmf(fuzzy_model,
                          "input",
                          i_mf,
                          paste0("b3",colnames(dataset)[features[i]]),
                          "trimf",
                          c(bx[4,bx_class], bx[5,bx_class],bx[5,bx_class]))
    i_mf <- i_mf + 1 # Increases the index for the MFs
  }
  return(fuzzy_model)
}

auto_feature_selector <- function(training_data,nbin,cols_features){
  features <- weight_list_n(training_data,nbin,cols_features)
  above_weights <- NULL
  while_1 <- 1
  orderer <- NULL

  for(i in 1:length(features)) {
    # print(paste(i,features[i])) # DEBUG

    if(!is.na(features[i])) orderer <- rbind(orderer,c(i,features[i]))

   if(!is.na(features[i]) && features[i] > 0.5) above_weights <- c(above_weights,i)
  }

if (length(above_weights) < 3){
orderer <- orderer[order(orderer[,2]),1]
orderer <- tail(orderer,n=3)
above_weights <- orderer
}


  # write(features,"features.txt",append = T) # DEBUG
  # write(length(above_weights),"lenght_weights.txt",append = T) # DEBUG
  #
  # write("NL","DEBUG_orderer.txt",append = T) # DEBUG
  # write(orderer,"DEBUG_orderer.txt",append = T) # DEBUG
  #
  write("NL","DEBUG_above_weights.txt",append = T) # DEBUG
  write(above_weights,"DEBUG_above_weights.txt",append = T) # DEBUG
  #
  # write("NL","DEBUG_features.txt",append = T) # DEBUG
  # write(features,"DEBUG_features.txt",append = T) # DEBUG

 return(above_weights)
}


######## RETURNS 2col, [FIS0,FIS1] ########
# Creates two models, one for 0 and one for 1 and evaluate them.
# the RETURN is a DF with Eval0 and Eval1 (ranks for the probability of 0 or 1)
fuz_sis <- function(dataset,data_test,features,nbin, plots = F, weights = "fixed", rule_set = "partial"){
  if(missing(plots)) plots <- F
  if(missing(rule_set)) rule_set <- "partial"

  data_test <- data.matrix(data_test)
  data_test <- data_test[,features]

  model_zero <- newfis("model_zero")
  model_zero <- create_fuzzy_inputs(model_zero,
                                    dataset,
                                    "zero",
                                    features,
                                    nbin,
                                    plots)
  model_zero <- create_fuzzy_outputs(model_zero)
  rules <- create_fuzzy_rules(dataset,features,weights_type = weights, rule_set = rule_set)
  model_zero <- addrule(model_zero,rules)

  # print(rules) # DEBUG

  model_one <- newfis("model_one")
  model_one <- create_fuzzy_inputs(model_one,dataset,"one",features,nbin, plots)
  model_one <- create_fuzzy_outputs(model_one, plots = F)
  rules <- create_fuzzy_rules(dataset,features, weights_type = weights, rule_set = rule_set)
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
# Returns a matrix (DF) of results, using original data for benchmarking
result_matrix <- function(dataset,
                          data_test,
                          features,
                          nbin,
                          plots = F,
                          method = "only_1",
                          weights = "fixed",
                          rule_set = "partial")
                          {

  if(missing(plots)) plots <- F
  if(missing(method)) method <- "only_1"

  Eval0 <- 0
  Eval1 <- 0

  d_bench <- data_test

  evaluation <- fuz_sis(dataset,data_test,features,nbin, plots, weights = weights, rule_set = rule_set)
  # EVresult_fis <- ifelse(evaluation$one > 50,1,0)

  if(method == "only_1") {
    Eval0=ifelse(evaluation$FIS1 > 50,0,1)
    Eval1=ifelse(evaluation$FIS1 > 50,1,0)
  }

  if(method == "conservative" || method == "conservative1") {
    Eval0=ifelse(evaluation$FIS0 < evaluation$FIS1 & evaluation$FIS1 > 50,0,1)
    Eval1=ifelse(evaluation$FIS0 < evaluation$FIS1 & evaluation$FIS1 > 50,1,0)

  }

  if(method == "conservative2") {
    Eval0=ifelse(evaluation$FIS0 < 50 & evaluation$FIS1 > 50,0,1)
    Eval1=ifelse(evaluation$FIS0 < 50 & evaluation$FIS1 > 50,1,0)
  }

  if(method == "sc") {
    Eval0=ifelse(evaluation$FIS0 < 50 & evaluation$FIS1 > 55,0,1)
    Eval1=ifelse(evaluation$FIS0 < 50 & evaluation$FIS1 > 55,1,0)
  }

  if(method == "sc2") {
    Eval0=ifelse(evaluation$FIS0 < 45 & evaluation$FIS1 > 50,0,1)
    Eval1=ifelse(evaluation$FIS0 < 45 & evaluation$FIS1 > 50,1,0)
  }

  if(method == "fuzzy50") {
    fuzzy_res <- fuzzy_result(evaluation)
    Eval0=ifelse(fuzzy_res$ret > 50,0,1)
    Eval1=ifelse(fuzzy_res$ret > 50,1,0)
  }

  if(method == "fuzzy55") {
    fuzzy_res <- fuzzy_result(evaluation)
    Eval0=ifelse(fuzzy_res$ret > 55,0,1)
    Eval1=ifelse(fuzzy_res$ret > 55,1,0)
  }

  if(method == "fuzzy60") {
    fuzzy_res <- fuzzy_result(evaluation)
    Eval0=ifelse(fuzzy_res$ret > 60,0,1)
    Eval1=ifelse(fuzzy_res$ret > 60,1,0)
  }

  if(method == "fuzzy65") {
    fuzzy_res <- fuzzy_result(evaluation)
    Eval0=ifelse(fuzzy_res$ret > 65,0,1)
    Eval1=ifelse(fuzzy_res$ret > 65,1,0)
  }

  if(method == "fuzzy70") {
    fuzzy_res <- fuzzy_result(evaluation)
    Eval0=ifelse(fuzzy_res$ret > 70,0,1)
    Eval1=ifelse(fuzzy_res$ret > 70,1,0)
  }

  return_result_matrix <- cbind(evaluation,
    Benchmark=as.character(d_bench[,nbin]),
    Eval0,
    Eval1)

  # return_result_matrix <- data.frame(return_result_matrix)
  return(data.frame(return_result_matrix))
  # return(total)
}

# plots 4 different graphs for each variable (feature)
plots_afis <- function(dataset,features,nbin,model_zero,model_one) {
  for(i in 1:length(model_one$input)) {
    col_name_var <- model_one$input[[i]]$name
    col_num_var <- which( colnames(dataset)==col_name_var )

    feature_weight <- round(weight_list_n(dataset,nbin,features)[col_num_var],3)

    par(mfrow=c(2,2))

    plotmf(model_zero, "input", i,main =paste(col_num_var,
                                              "ZERO",
                                              model_zero$input[[i]]$name))
    plotmf(model_one, "input", i,main =paste(col_num_var,
                                              "ONE",
                                              model_one$input[[i]]$name))
    hist(dataset[,col_num_var],main = paste(col_num_var,
                                            "Dist.",
                                            model_one$input[[i]]$name))
    boxplot(dataset[,col_num_var]~dataset[,nbin],main =paste(col_num_var,
                                                              "Weight",
                                                              feature_weight))
  }
}

# Return col names and its indices
col_names_func <- function(dataset) {
  rect <- c("Index","Col Name")
  for(i in 1:ncol(dataset) ) {

    rect_temp <- c(i,colnames(dataset)[i])
    rect <- rbind(rect, rect_temp)
    rect_temp <- NULL
  }
  return(rect)
}

evaluate_afis <- function(trailing_size,
                          starting_point,
                          dataset,
                          possible_features,
                          eval_plots =F,
                          eval_method = "only_1",
                          eval_return = "matrix") {

  if(missing(eval_plots)) eval_plots <- F
  if(missing(eval_method)) eval_method <- "only_1"
  if(missing(eval_return)) eval_return <- "matrix"

  result_ma <- NULL # Inicializando
  conf_ma <- NULL # Inicializandoa

  print(paste("Start Evaluating:",Sys.time()))

  # Esta rotina FOR testa TODOS os dias a partir do starting_point com um modelo Fuzzy Movel (trailing_size) em dias
  # O modelo também faz previsões com dataframes de varias linhas, porém a proposta é somente um dia.
  for (i in starting_point:nrow(dataset)) {
    if(trailing_size == 0) trailing_size <- i # # Atropela trailing_size para resetar em cada iteracao # EDITAVEL

    training_data <- dataset[(i-trailing_size+1):(i-1),]

    n_col_features <- auto_feature_selector(training_data,nbin,possible_features)


    data_input <- dataset[i,]

    result_ma_now <- result_matrix(training_data,
      data_input,
      n_col_features,
      nbin,
      plots=F,
      method = eval_method
      ) # EDITAVEL

      result_ma <- rbind(result_ma,result_ma_now)

      # Necessario no minimo dois niveis de fator para a conf_matrix
      if(nlevels(factor(result_ma$Eval1)) > 1 && nlevels(factor(result_ma$Benchmark)) > 1) {
        conf_ma_now <- confusionMatrix(factor(result_ma$Eval1),
        factor(result_ma$Benchmark),
        positive = "1")

        conf_ma <- rbind(conf_ma,c(i,
          conf_ma_now$byClass[3],
          conf_ma_now$byClass[4],
          conf_ma_now$byClass[11]))
        }

        if(i %% 100 == 0)   print(paste(Sys.time(),
        "Current:",
        i,
        "ln. of",
        nrow(dataset)))
      }

      print(paste("Evaluating:",Sys.time()))

      cumulative <- conf_ma

      if(eval_plots) {
        plot(cumulative)
        grid(NA, 5, lwd = 2) # grid only in y-direction
      }

conf_matrix_return <- result_ma

      if(eval_return == "matrix") {
        conf_matrix_return <- confusionMatrix(factor(result_ma$Eval1),
            factor(result_ma$Benchmark),
            positive = "1")
        }



      return(conf_matrix_return)
}

fuzzy_result <- function(result_mat_f) {

result_mat_full <- result_mat_f
result_mat_f <- as.matrix(result_mat_f[,1:2])

model_result <- newfis("model_result")
model_result <- addvar(model_result,"input","FIS0",c(15.33,84.67))
model_result <- addmf(model_result,"input",1,"baixo","trimf",c(15.33,16.33,50))
model_result <- addmf(model_result,"input",1,"medio","trimf",c(16.33,50,84.67))
model_result <- addmf(model_result,"input",1,"alto","trimf",c(50,84.67,84.67))

model_result <- addvar(model_result,"input","FIS1",c(15.33,84.67))
model_result <- addmf(model_result,"input",2,"baixo","trimf",c(15.33,16.33,50))
model_result <- addmf(model_result,"input",2,"medio","trimf",c(16.33,50,84.67))
model_result <- addmf(model_result,"input",2,"alto","trimf",c(50,84.67,84.67))

model_result <- addvar(model_result,"output","Eval1",c(0,100))
model_result <- addmf(model_result,"output",1,"minimo","trimf",c(0,0,25))
model_result <- addmf(model_result,"output",1,"baixo","trimf",c(0,25,50))
model_result <- addmf(model_result,"output",1,"medio","trimf",c(25,50,75))
model_result <- addmf(model_result,"output",1,"alto","trimf",c(50,75,100))
model_result <- addmf(model_result,"output",1,"maximo","trimf",c(75,100,100))


      total_col_test <- new.expand.grid(c(1,2,3),2)
      total_col_test[,1] <- (total_col_test[,1] * -1)

      total_col_test_soma <- total_col_test


      res_row <- NULL
      for(j in 1:nrow(total_col_test_soma)) {
        res_row[j] = sum(total_col_test_soma[j,])
      }

      res_row <- normalize(res_row)

      res_row[res_row >= 0.80] = 5
      res_row[res_row > 0.60 & res_row < 0.80 ] = 4
      res_row[res_row > 0.4 & res_row < 0.6 ] = 3
      res_row[res_row > 0.2 & res_row < 0.4 ] = 2
      res_row[res_row <= 0.20 ] = 1

      total_col_test <- cbind(total_col_test,res_row)
      total_col_test <- cbind(total_col_test,1,1)
      total_col_test <- abs(total_col_test)
      total_col_test <- as.matrix(total_col_test)

model_result <- addrule(model_result,total_col_test)

ret <- evalfis(result_mat_f[,1:2],model_result)

ret <- cbind(result_mat_full,ret)

return(ret)
}
