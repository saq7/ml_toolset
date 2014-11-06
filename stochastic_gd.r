
## Stochastic GRADIENT DESCENT ALGORITHM
stochastic_gd<-function(design_matrix,response,gradient_fn,cost_fn,threshold=0.01,
                        max_pass=1,init_weights=matrix(rep(0,ncol(design_matrix)),ncol=1),learning_rate=0.1){
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

## testing
# set.seed(3122)
# data_size<-1000
# slope<-runif(1,min=0,max=1)
# intercept<-runif(1,min=1,max=4)
# X<-matrix(c(rep(1,data_size),runif(2*data_size,min=-5,max=5)),ncol=3,byrow=FALSE)
# labels<- matrix(X[,2] > (slope*X[,1]+intercept),ncol=1)

system.time(stochastic_gd(design_matrix=X,response=labels,gradient_fn=ls_gradient_fn,
              cost_fn=ls_cost_fn,max_pass=200,learning_rate=0.001)->a)
a$weights->theta

pred<-X%*%theta
pred<-replace(pred,pred<0.5,0)
pred<-replace(pred,pred>0.5,1)
sum(labels!=pred)/nrow(labels)