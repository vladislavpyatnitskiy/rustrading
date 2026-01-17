lapply(c("moexer", "xts", "ggplot2", "data.table", "timeSeries"),
       require, character.only = T) # Libraries

rus.monte.carlo <- function(x, ndays, n){ # Monte Carlo Simulation
  
  P <- NULL # When Data from Yahoo! Finance needed
  
  for (A in x){ 
    
    D <- as.data.frame(
      get_candles(
        A, 
        from = "2007-01-01",
        interval = 'daily'
        )[,c(3,8)]
      )
    
    message(
      sprintf(
        "%s is downloaded (%s / %s)", 
        A, which(x == A), length(x)
      )
    ) # Download message
    
    D <- D[!duplicated(D),] # Remove duplicates
    
    P <- cbind(P, xts(D[, 1], order.by = as.Date(D[, 2]))) }
    
    P <- P[apply(P, 1, function(x) all(!is.na(x))),] # Reduce NA
    
    colnames(P) <- x 
    
    P <- as.timeSeries(P) # Make data Time Series
    
    L <- NULL
    Av <- NULL
    Plots <- NULL
    
    for (m in 1:ncol(P)){ c <- P[,m]
    
    r <- as.numeric(c / lag(c)) # Calculate returns
    r[1] <- 1 # Assign first observation as 1
    set.seed(0) # Calculate various scenarios of Stock Performance
    
    # Mimic Historical Performance using log returns
    p <- data.table(
      apply(
        replicate(n, expr = round(sample(r, ndays, replace=T), 2)), 2, cumprod
        )
      )
    
    p$days <- 1:nrow(p)
    p <- melt(p, id.vars = "days")
    
    # Make Line Charts with all scenarios
    plt <- ggplot(p, aes(x=days,y=(value - 1) * 100, col=variable)) +
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
    
    if (is.null(Plots)){ Plots <- plt } else { Plots <- list(Plots, plt) } }
    
  names(Av) <- x # Assign names
  
  rownames(L) <- x # Assign row names
  
  list(Plots, L, Av) # Output
}
rus.monte.carlo(c("DIOD", "LKOH"), 1000, 100) # Test
