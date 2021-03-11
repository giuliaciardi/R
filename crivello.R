crivello <- function(n){
  v <- (2:n)
   
  i <- 1
  to_k <- NULL
  
  repeat{
    
    to_k <- c(v[i])
    
    log <- as.logical(v %% v[i])
    to_k <- c(to_k,v[log])
      
    v <- to_k
    
    i <- i+1
    
    if (v[length(v)]>n){
      break
    }
    }
    
  
  return(v)
}

crivello(n = 15)
