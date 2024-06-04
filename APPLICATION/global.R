library(plotly)

model_final <- function(X,type=1){
  
  model <- X
  
  if(type==1){
    for(cov in names(X)){
      
      model[,cov] <- X[,cov]*table_resultats[cov,"AVG"]
      RES_tmp <- RES
      TIME_tmp <- TIME
      
    }
  }
  
  if(type==2){
    for(cov in names(X)){
      
      model[,cov] <- X[,cov]*table_resultats_SP[cov,"AVG"]
      RES_tmp <- RES_SP
      TIME_tmp <- TIME_SP
    }
  }
  
  sum.model <- rowSums(model, na.rm = TRUE)
  
  pred.temp <- 0
  
  for(i in 1:(nb_ech_bootstrap*nb_BDD_completes)){
    
    tmp <- exp(matrix(exp(sum.model))%*%t(as.matrix(-1*RES_tmp$H0B[,i+1])))
    tmp[is.na(tmp)] <- 1
    pred.temp <- pred.temp + tmp
    
  }
  
  pred <- pred.temp/(nb_ech_bootstrap*nb_BDD_completes)
  
  
  table.temp <- data.frame(time = c(0,TIME_tmp$time), pred = c(1,pred[1,]))
  
  list(model = sum.model, table = table.temp)
  
}


graph <- function(X,type=1){
  
  if(type==1){
    t <- "Survival"
  }
  
  if(type==2){
    t <- "Free-progression survival"
  }
  
  graphic <- plot_ly(data = model_final(X,type)$table, x = ~ round(time,digits = 2), y = ~ round(pred,digits = 3)) %>%
    layout(title = "Predicted survival curve",
           xaxis = list(title = "Time (months)"),
           yaxis = list(title = t)) %>% config(
             toImageButtonOptions = list(
               format = "svg",
               filename = "myplot",
               width = 600,
               height = 700))
  add_lines(graphic, line= list(shape = 'hv'))
  
}