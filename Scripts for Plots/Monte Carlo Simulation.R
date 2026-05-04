lapply(c("moexer", "xts", "ggplot2", "data.table", "timeSeries"),
       require, character.only = T) # Libraries

rus.monte.carlo <- function(x, ndays, m){ # Monte Carlo Simulation
  
  redom = list(
    c("AGRO", "RAGR"), c("CIAN", "CNRU"), c("HHRU", "HEAD"), c("FIVE", "X5"),
    c("FIXP", "FIXR"), c("YNDX", "YDEX"))
  
  from = "2007-01-01"
  
  J <- NULL
  R <- NULL
  L <- NULL
  Av <- NULL
  Plots <- NULL
  
  for (n in 1:length(x)){
    
    if (any(sapply(redom, function(redom_item) x[n] %in% redom_item))){
      
      f <- which(sapply(redom, function(redom_item) x[n] %in% redom_item))
      
      for (k in 1:length(redom[[f]])){
        
        a = as.data.frame(
          get_candles(redom[[f]][k], from=from, interval='daily')[,c(3,8)]
        )
        
        if (k == 2){ 
          
          message(
            sprintf(
              "%s is downloaded; %s from %s", x[n], which(x == x[n]), length(x)
            )
          )
        }
        
        a <- a[!duplicated(a),] # Remove duplicates
        
        a <- xts(a[, 1], order.by = as.Date(a[, 2]))
        
        if (x[n] == "AGRO") a <- a / 7.01
        if (x[n] == "FIXP") a <- a / 100     
        
        colnames(a) <- redom[[f]][2]
        
        if (is.null(R)) R <- data.frame(a) else R <- rbind.data.frame(R, a)
      }
    } else {
      
      a = as.data.frame(get_candles(x[n], from=from, interval='daily')[,c(3,8)])
      
      message(
        sprintf(
          "%s is downloaded; %s from %s", 
          x[n], which(x == x[n]), length(x)
        )
      )
      
      a <- a[!duplicated(a),] # Remove duplicates
      
      a <- xts(a[, 1], order.by = as.Date(a[, 2]))
      
      colnames(a) <- x[n]
      
      R <- data.frame(a) 
    }
    
    R <- as.timeSeries(R) # Make it time series
    
    if (x[n] == "BELU"){ j <- which(rownames(R) == "2024-08-15")
    
      R[c(1:j),] <- R[c(1:j),]/8 } # Adjustments for Novabev stock
    
    c <- R
    R <- NULL  # Reset R for next iteration
    r <- as.numeric(c / lag(c)) # Calculate returns
    r[1] <- 1 # Assign first observation as 1
    set.seed(0) # Calculate various scenarios of Stock Performance
    
    # Mimic Historical Performance using log returns
    p <- data.table(
      apply(
        replicate(m, expr = round(sample(r, ndays, replace=T), 2)), 2, cumprod
      )
    )
    
    p$days <- 1:nrow(p)
    p <- melt(p, id.vars = "days")
    
    # Make Line Charts with all scenarios
    plt <- ggplot(p, aes(x = days, y = (value - 1) * 100, col = variable)) +
      geom_line() +
      theme_bw() +
      theme(legend.position = "none") +
      ggtitle(
        sprintf("%s Performance by Monte Carlo Simulation", colnames(c))
      ) +
      xlab("Days Invested") + 
      ylab("Return (%)")
    
    summary <- as.vector(summary((p$value[p$days == ndays] - 1) * 100)) 
    
    names(summary) <- c("Min.", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max.")
    
    L <- rbind(L, t(as.data.frame(summary))) # Join
    
    Av <- c(Av, as.vector(mean((p$value[p$days] - 1) * 100 < 0))) # Join
    
    if (is.null(Plots)) Plots <- list(plt) else Plots[[n]] <- plt }
  
  names(Plots) <- x
  names(Av) <- x # Assign names
  rownames(L) <- x # Assign row names
  
  L <- as.data.frame(L)
  
  L <- L[order(-L$`Median`), ] # Sort by yield level
  Av <- sort(Av, decreasing = F)
  
  DF <- list(Plots, L, Av) # Output
  
  names(DF) <- c("Plots", "Yield", "Means")
  
  DF
}
rus.monte.carlo(c("DIOD", "LKOH"), 1000, 100) # Test
