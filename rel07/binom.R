# Distribuição Binomial

p <- function(q, dist = "binomial", lower.tail = TRUE, rounding = 4, porcentage = FALSE, gui = "plot", ...) {
  argaddit$size <- list(...)
  argdef <- formals(p)
  if (dist == "binomial") {
    if (!any(names(argaddit) == "size")) stop("Insira o argumento 'size'!", call. = FALSE)
    if (!any(names(argaddit) == "prob")) stop("Insira o argumento 'prob'!", call. = FALSE)
    if (lower.tail) {
        plotcurve <- function(q, size, prob){
          rmin <- size * prob - 4 * sqrt(size * prob*(1 - prob))
          if (rmin < 0) rmin < 0 else rmin <- round(rmin)
          x <- rmin:size
          x1 <- rmin:q
          x2 <- (q + 1):size
          probx <- dbinom(x, size = size, prob = prob)
          probx1 <- dbinom(x1, size = size, prob = prob)
          probx2 <- dbinom(x2, size = size, prob = prob)
  
  # Area de plotagem
    xlim <- c(rmin, size)
    ylim <- c(min(probx), max(probx) + 0.1)
    
  # Area de plotagem
    plot.new()
    plot.window(xlim, ylim)
    
  # Eixos
    axis(1)
    axis(2)
        
  # Titulo e lables
    title(ylab = expression(p[X](x)), xlab="X")
      
    lines (x1, probx1, type = "h", panel.first = grid(),lwd = 2, col = "red")
    points(x1, probx1, lwd = 2, col = "red", pch = 19)
    
    lines (x2, probx2, type = "h",lwd = 2)
    points(x2, probx2, lwd = 2, pch = 19)
          

          abline(v= size*prob, lty=2)
          qq <- round(q, digits=2)
          qqaux <- round(q, digits=2)
          Pr <- round(pbinom(q, size = size, prob = prob, lower.tail = T), rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(X<=q)==Pr~"\n\n"~size == n~p==prob , list(q=qq, Pr=Pr, n = size, prob = prob)))
        }

        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(size, prob),
                                 size = manipulate::slider(-6, 6, q),
                                 prob = manipulate::slider(mu, mu + 200, mu))
        }
        
      } else{
      plotcurve <- function(q, size, prob) {
          curve(dbinom(x, size = , prob =sigma), -6, 6, ylab = expression(f[T](t)), xlab="T")
          x <- seq(q, 6, by=0.01)
          y <- seq(-6, q, by=0.01)
          fx <- dbinom(x, size = mu, prob = sigma)
          fy <- dbinom(y, size = mu, prob = sigma)
          polygon(c(x, rev(x)),
                  c(fx, rep(0, length(fx))),
                  col="red")
          polygon(c(y, rev(y)),
                  c(fy, rep(0, length(fy))),
                  col="gray90")
          abline(v=0, lty=2)
          qq <- round(q, digits=2)
          qqaux <-round(q, digits=2)
          Pr <- round(pbinom(qq, size = mu, prob =sigma, lower.tail = F), digits=rounding)
          Pr <- gsub("\\.", ",", Pr)
          qq <- gsub("\\.", ",", qq)
          axis(side=1, at=qqaux, labels=qqaux,
               col="red", font = 2)
          abline(v = qqaux, lty=2, col = "red")
          legend("topleft", bty="n", fill="red",
                 legend=substitute(P(X~`>`~q)==Pr~"\n\n"~size==mu, list(q=qq, Pr=Pr, mu = mu)))
      }
      if (gui == "plot")
          # Probability
          size <- argaddit$size
          sucesso <- argaddit$prob
          prob <- pbinom(q = q,  = mu, prob =sigma)
          # Plot
          plotcurve(q, size, prob)
        }
        if (gui == "rstudio") {
          manipulate::manipulate(plotcurve(size, prob),
                                 size = manipulate::slider(-6, 6, q),
                                 prob = manipulate::slider(mu, mu + 200, mu))
        }
        
      }
    }
    prob <- round(prob, rounding)
    if (porcentage == TRUE) prob <- prob * 100
    return(prob)
    
}

 
 
 
 