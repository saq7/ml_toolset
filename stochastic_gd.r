
## Stochastic GRADIENT DESCENT ALGORITHM
stochastic_gd<-function(design_matrix,response,
                        gradient_fn,cost_fn,threshold=0.01,
                        max_pass=1,
                        init_weights=matrix(rep(0,ncol(design_matrix)),ncol=1),
                        learning_rate=0.1){
  theta<-init_weights
  alpha<-learning_rate
  n<-nrow(design_matrix)
  cost<-numeric()
  nrows<-nrow(X)
  for(k in 1:max_pass){
    index<-(1:nrows)[order(runif(nrows))]
    if(!is.null(cost_fn)){
      cost<-c(cost,cost_fn(response,design_matrix%*%theta))
    }    
    if(tail(cost,n=1)<threshold){
      break
    }
    for (i in index){
      theta<-theta-alpha*gradient_fn(theta,design_matrix[i,],response[i,])  
    }
  }
  list(weights=theta,cost=cost)
}
