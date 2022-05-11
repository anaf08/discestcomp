#Gerando números aleatorios de uma distribuição

rexponencial <- function(n, lambda){
  if (!is.numeric(lambda) & lambda < 0) stop("O argumento lambda deve ser numérico e maior que 0!", call. = FALSE)
  nunif <- runif(n)
  x <- (-log(1 - nunif)) / lambda
  return(x)
}

  
#Aproximação de distribuições
x <- SMR:::GaussLegendre(2)
fx <- function(x) x^3 - 5 * x
fx2 <- function(x) x^2

#O resultado fica assim
sum(x$weights * fx(x$nodes))

#Para fx2 temos
sum(x$weights * fx2(x$nodes))

