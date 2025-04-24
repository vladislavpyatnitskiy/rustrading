lapply(c("fGarch","timeSeries","moexer","xts"),require,character.only=T) # Libs

rus.VaR.plt <- function(x, VaR = c(95), s=NULL, e=NULL){ # Plot of VaR 
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  
  getData <- function(A, s, e) { 
    if (is.null(s) && is.null(e))
      return(get_candles(A, from = "2007-07-20", interval = 'daily')) 
    if (is.null(e)) return(get_candles(A, from = s, interval = 'daily')) 
    if (is.null(s)) return(get_candles(A, till = e, interval = 'daily')) 
    return(get_candles(A, from = s, till = e, interval = 'daily')) 
  }
  for (A in x){ D <- as.data.frame(getData(A, s, e)[,c(3,8)])
    
    D <- D[!duplicated(D),] # Remove duplicates
    
    p <- cbind(p, xts(D[, 1], order.by = as.Date(D[, 2]))) }
    
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Assign column names
  
  p <- as.timeSeries(p) # Display time series
  
  if (isTRUE(lg)) { p = diff(log(p))[-1,] } # log returns 
  
  for (n in 1:ncol(p)){ s <- p[,n]  # For each column in data set
    
    t <- seq(nrow(s)) # Set index
    
    gm <- garchFit( ~ garch(1, 1), data = coredata(s), trace = F) # GARCH model
    
    plot(t, s, type="l", xlab = "Trading Days", ylab = Returns, las = 1,
         main = sprintf("%s VaR GARCH (1,1)", colnames(s)),
         sub = "Data Source: Moscow Exchange") # Plot graph
    
    for (v in seq(VaR)){ lines(t,mean(s)+qnorm(1-VaR[v]*.01)*gm@sigma.t,
                               col=v+1) }  
    
    grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
    
    abline(h = 0)
    
    axis(side = 4, las = 2) # Right y-axis
    
    par(mar = c(5, 4, 4, 4)) } # Define borders of the plot to fit right y-axis
}
rus.VaR.plt("LKOH", VaR = 95) # Test
