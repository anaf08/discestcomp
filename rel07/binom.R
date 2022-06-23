# Distribuição Binomial

p <- function(q, dist = "binomial", lower.tail = TRUE, rounding = 4, porcentage = FALSE, gui = "plot", ...) {
  argaddit <- list(...)
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
      if (gui == "plot"){
        # Probability
        size <- argaddit$size
        sucesso <- argaddit$prob
        prob <- pbinom(q = q, size = size, prob = sucesso)
        # Plot
        plotcurve(q, size, prob = sucesso)
      }
      if (gui == "rstudio") {
        size <- argaddit$size
        sucesso <- argaddit$prob
        prob <- pbinom(q = q, size = size, prob = sucesso)
        manipulate::manipulate(plotcurve(q, size, prob),
                               q = manipulate::slider(0, size, q),
                               size = manipulate::slider(size, size + 30, size),
                               prob = manipulate::slider(sucesso, 1, sucesso))
      }

    } else{
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
        
        lines (x1, probx1, type = "h", panel.first = grid(),lwd = 2)
        points(x1, probx1, lwd = 2, pch = 19)
        
        lines (x2, probx2, type = "h",lwd = 2, col = "red")
        points(x2, probx2, lwd = 2, pch = 19, col = "red")
        
        
        abline(v= size*prob, lty=2)
        qq <- round(q, digits=2)
        qqaux <- round(q, digits=2)
        Pr <- round(pbinom(q, size = size, prob = prob, lower.tail = F), rounding)
        Pr <- gsub("\\.", ",", Pr)
        qq <- gsub("\\.", ",", qq)
        axis(side=1, at=qqaux, labels=qqaux,
             col="red", font = 2)
        abline(v = qqaux, lty=2, col = "red")
        legend("topleft", bty="n", fill="red",
               legend=substitute(P(X>q)==Pr~"\n\n"~size == n~p==prob , list(q=qq, Pr=Pr, n = size, prob = prob)))
      }
      if (gui == "plot"){
        # Probability
        size <- argaddit$size
        sucesso <- argaddit$prob
        prob <- pbinom(q = q, size = size, prob = sucesso,lower.tail = FALSE)
        # Plot
        plotcurve(q, size, prob = sucesso)
      }
      if (gui == "rstudio") {
        # Probability
        size <- argaddit$size
        sucesso <- argaddit$prob
        prob <- pbinom(q = q, size = size, prob = sucesso, lower.tail = FALSE)
        manipulate::manipulate(plotcurve(q, size, prob),
                               q = manipulate::slider(q, size, q),
                               size = manipulate::slider(size, size + 30, size),
                               prob = manipulate::slider(sucesso, 1, sucesso)
                               )
      }

    }
  }
  prob <- round(prob, rounding)
  if (porcentage == TRUE) prob <- prob * 100
  return(prob)

}

