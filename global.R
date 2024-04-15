#tata
model_final <- function(X){
  
  model <- X
  
  for(cov in names(X)){
    
    model[,cov] <- X[,cov]*table_resultats[cov,"AVG"]
    
  }
  
  sum.model <- rowSums(model, na.rm = TRUE)
  
  pred.temp <- 0
  
  for(i in 1:(nb_ech_bootstrap*nb_BDD_completes)){
    
    tmp <- exp(matrix(exp(sum.model))%*%t(as.matrix(-1*RES$H0B[,i+1])))
    tmp[is.na(tmp)] <- 1
    pred.temp <- pred.temp + tmp
    
  }
  
  pred <- pred.temp/(nb_ech_bootstrap*nb_BDD_completes)
  
  
  table.temp <- data.frame(time = c(0,TIME$time), pred = c(1,pred[1,]))
  
  median <- table.temp[table.temp$pred==max(table.temp[table.temp$pred<=0.5,]$pred),]$time
  
  list(model = sum.model, table = table.temp, median = median)
  
}


graph <- function(X, median){
  
  if(median == TRUE){
    
    ggplot(model_final(X)$table, aes(x = time, y = pred)) +
      geom_step(color ="dodgerblue1" )+
      labs(
        x = "Survival time (months)",
        y = "Progression-free survival") +
      geom_segment(aes(x=model_final(X)$median, xend=model_final(X)$median, y=-Inf,  yend= 0.5),  linetype=2) +
      geom_segment(aes(x=-Inf, xend=model_final(X)$median, y=0.5, yend=0.5),  linetype=2) +
      theme_minimal() +
      ylim(c(0,1))
    
  }else{
    
    ggplot(model_final(X)$table,aes(x = time, y = pred)) +
      geom_step(color ="dodgerblue1" )+
      labs(
        x = "Survival time (months)",
        y = "Progression-free survival") + 
      theme_minimal() +
      ylim(c(0,1))
    
  }
  
}

