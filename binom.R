#Probability

p <- function(q, dist = "binomial", lower.tail = TRUE, rounding = 4, porcentage = FALSE, gui = "plot", ...){
  if(dist == "binomial"){
argnames <- names(formals(p))
  # if(!any(argnames == "size")) stop("Insira argumento 'size'!", call. = FALSE)
    # if(!any(argnames == "prob")) stop("Insira argumento 'prob'!", call. = FALSE)
     if (lower.tail){
       prob <- pbinom(q = x, size = size, )
     }
  }
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)
    
}

 
 
 
 