# Calculando o devsio padrão no leem

sdev <- function(x, rounding = 2, na.rm = FALSE, details = FALSE,
                 grouped = TRUE) {
  if (class(x) != "leem") stop("Use the 'new_leem()' function to create an object of class leem!", call. = FALSE)
  if (class(x) == "leem" & is.null(attr(x, "table"))) x <- tabfreq(x)
  if (attr(x, "variable") == "discrete") {
    numchar <- is.numeric(x$estat$raw_data)
    if (numchar) {
      desvpad <- round(sd(x = x$estat$raw_data,
                          na.rm = na.rm), digits = rounding)
      resume <- list(sdeviation = desvpad, table = x$tabela, rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      } else {
        return(desvpad)
      }
      
    } else {
      stop("Measure not used for this data type!", call. = FALSE,
           domain = "R-leem")
    }
  }
  if (attr(x, "variable") == "continuous") {
    if (grouped == TRUE) {
      desvpad <- sqrt((variance(x)))
      resume <- list(sdeviation = desvpad, table = x$tabela,
                     rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(desvpad)
      }
    } else {
      desvpad <- round(sd(x = x$estat$raw_data, na.rm = na.rm),
                       digits = rounding)
      resume <- list(sdeviation = desvpad, table = x$tabela,
                     rawdata = x$estat$raw_data)
      if (details) {
        return(resume)
      }
      else {
        return(desvpad)
      }
    }
  }
}

set.seed(10)
x <- rnorm(36, 100, 50)
x <- new_leem(x, variable = 2)
sdev(x)

