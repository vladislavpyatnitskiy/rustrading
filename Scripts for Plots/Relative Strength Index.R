lapply(c("moexer", "xts", "timeSeries"), require, character.only = T) # lib

RSI.rus <- function(x, s=NULL, e=NULL, split=F, all=F){
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  
  getData <- function(A, s, e) { 
    if (is.null(s) && is.null(e))
      return(get_candles(A, from = "2007-07-20", interval = 'daily')) 
    if (is.null(e)) return(get_candles(A, from = s, interval = 'daily')) 
    if (is.null(s)) return(get_candles(A, till = e, interval = 'daily')) 
    return(get_candles(A, from = s, till = e, interval = 'daily')) 
  }
  for (A in x){ D <- as.data.frame(getData(A, s, e)[,c(3,8)])
    
    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        A, which(x == A), length(x)
      )
    ) # Download message
    
    D <- D[!duplicated(D),] # Remove duplicates
    
    D <- xts(D[,1], order.by = as.Date(D[,2])) # Move dates to row names
    
    colnames(D) <- A # Put the tickers in data set
    
    D <- as.timeSeries(D) # Make it time series
    
    if (A == "BELU" & isTRUE(split) &
        (isTRUE(s < "2024-08-15" | e < "2024-08-15") |
         isTRUE(is.null(s) | is.null(e)))){
      
      f <- which(rownames(D) == "2024-08-15")
      
      D[c(1:f),] <- D[c(1:f),] / 8 } # Adjustments for Novabev stock
    
    D <- D[apply(D, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    D <- diff(log(as.timeSeries(D)))[-1,]
    
    if (is.null(p)){ p <- list(D) } else { p[[A]] <- D } }

  df <- NULL
  d <- NULL

  for (m in 1:length(p)){ y <- p[[m]]
  
    gains <- ifelse(y > 0, y, 0)
    losses <- ifelse(y < 0, -y, 0)
  
    G <- NULL
    L <- NULL
  
    for (n in 1:(nrow(y)-14)){
      
      G <- rbind.data.frame(G, mean(gains[n:(n+13),]))
      L <- rbind.data.frame(L, mean(losses[n:(n+13),]))
    }
  
    I <- 100 - (100 / (1 + G/L))
  
    dates <- rownames(y)[15:nrow(y)]
  
    rownames(I) <- dates
    colnames(I) <- colnames(y)
  
    I <- as.timeSeries(I)
  
    plot(
      I,
      ylab = "RSI",
      xlab = "Trading Days",
      main = sprintf("Relative Strength Index of %s", colnames(y)),
      las = 1
    )
  
    axis(side = 4, las = 2) # Right Y-Axis Values
  
    grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
  
    abline(h = 30, col = "green", lwd = 3) #
    abline(h = 70, col = "red", lwd = 3) #
  
    A <- I[nrow(I),]
  
    d <- c(d, if (A < 30) "Buy" else if (A > 70) "Sell" else "Hold")
  
    df <- c(df, A)
  }

  D <- cbind.data.frame(x, df, d)

  colnames(D) <- c("Ticker", "RSI", "Signal")

  D[order(-D$RSI), ] # Sort by price change level
}
RSI.rus(c("GMKN", "BELU"), s="2025-01-01")
