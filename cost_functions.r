## Loss/cost functions and their gradients

## the least squares cost function - matrix operations
ls_cost_fn<-function(actual,prediction){
  n<-nrow(actual)
  (1/n)*t(prediction-actual)%*%(prediction-actual)
}

## least squares gradient - matrix operations
ls_gradient_fn<-function(w,X,y){
  if(is.null(dim(X)[2])){ 
    #single obs case - for stochastic alg
    X*(X%*%w-y)
  }else{ 
    #complete cases
    t(X)%*%(X%*%w-y)
  }  
}

## Log loss function
ll_cost_fn<-function(actual, prediction) {
  epsilon <- .000000000000001
  yhat <- pmin(pmax(prediction, epsilon), 1-epsilon)
  logloss <- -mean(actual*log(yhat)
                   + (1-actual)*log(1 - yhat))
  return(logloss)
}

## -gradient of the log loss function
ll_gradient_fn<-function(w,X,y){
  n<-nrow(X)
  if(is.null(n)){
    n<-1
  }
  cost<-0
  for(i in 1:n){
    cost<-cost+(-(y*X*(1/(1+exp(X%*%w))) - 
                       (1-y)*X*(1/(1+exp(-X%*%w)))))
  }
  cost/n
}
