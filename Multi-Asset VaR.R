lapply(
  c("xts", "timeSeries", "moexer"), 
  require, character.only = T) # lib

# Function to calculate Multi-Asset Value-at-Risk
rus.MVaR <- function(
    x, s=NULL, e=NULL, VaR = 0.01, p.weights, ndays = 1, split=T
    ){
  
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  
  getData <- function(A, s, e) { 
    if (is.null(s) && is.null(e))
      return(get_candles(A, from = "2007-07-20", interval = 'daily')) 
    if (is.null(e)) return(get_candles(A, from = s, interval = 'daily')) 
    if (is.null(s)) return(get_candles(A, till = e, interval = 'daily')) 
    return(get_candles(A, from = s, till = e, interval = 'daily')) 
  }
  for (A in x){ D <- as.data.frame(getData(A, s, e)[,c(3,8)])
    
    message(sprintf("%s is downloaded; %s from %s", A, which(x==A), length(x)))
    
    D <- D[!duplicated(D),] # Remove duplicates
    
    D <- xts(D[,1], order.by = as.Date(D[,2])) # Move dates to row names
    
    colnames(D) <- A # Put the tickers in data set
    
    D <- as.timeSeries(D) # Make it time series
    
    if (A == "BELU" & isTRUE(split) &
        (isTRUE(s < "2024-08-15" | e < "2024-08-15") |
         isTRUE(is.null(s) | is.null(e)))){
      
      f <- which(rownames(D) == "2024-08-15")
      
      D[c(1:f),] <- D[c(1:f),] / 8 } # Adjustments for Novabev stock
    
    p <- cbind(p, D) } # Merge
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),]
  
  p <- as.timeSeries(p) # Make it time series and display
  
  p = diff(log(p))[-1,] # Make logs if needed
  
  # Calculate sum of products for means and weights
  R = crossprod(
    x=as.matrix(apply(p, 2, function(col) mean(col))),
    y=p.weights
  )

  # Calculate sum of products for variances and square weights
  V = crossprod(
    x=as.matrix(apply(p, 2, function(col) var(col))),
    y=p.weights ^ 2
  )

  a.names <- x # Assign names

  # Create a matrix to store the products of weights
  weight_product_matrix <- matrix(
    0,
    nrow=length(a.names),
    ncol=length(a.names)
  )

  # Fill the matrix with weight products
  for (i in 1:length(a.names)) { for (j in 1:length(a.names)) {
    weight_product_matrix[i, j] <- p.weights[i] * p.weights[j] } }

  # Set row and column names
  rownames(weight_product_matrix) <- a.names
  colnames(weight_product_matrix) <- a.names

  # Put weights and covariances in nested list
  list_wghts_cov <- list(weight_product_matrix, cov(p))

  wghts_cov <- NULL # Create an empty variable to put covariances and weights

  for (n in 1:length(list_wghts_cov)){ # For each table in tables

    # Extract unique pairs and their correlations
    cor_pairs <- which(
      upper.tri(
        list_wghts_cov[[n]],
        diag = T
      ),
      arr.ind = T
    )

    # Put them into one data frame
    unique_pairs <- data.frame(
      Variable1 = rownames(list_wghts_cov[[n]])[cor_pairs[, 1]],
      Variable2 = rownames(list_wghts_cov[[n]])[cor_pairs[, 2]],
      Correlation = (list_wghts_cov[[n]])[cor_pairs]
    )

    # Filter out pairs where the tickers are not the same
    different_tickers_pairs <- unique_pairs[
      unique_pairs$Variable1 != unique_pairs$Variable2,
    ]

    # Concatenate First_Name and Last_Name with a space in between
    different_tickers_pairs$Pair <- paste(
      different_tickers_pairs$Variable1,
      different_tickers_pairs$Variable2
    )

    # Create new tables where put pairs in first column and values in second
    new_data_set <- as.data.frame(different_tickers_pairs[,4:3])

    # When working with first table assign to empty variable
    if (is.null(wghts_cov)){ wghts_cov <- new_data_set } else {

      # Otherwise perform inner join
      wghts_cov_join <- merge(x = wghts_cov, y = new_data_set, by = "Pair")} }

  # Sum of products for covariances and weights
  CV <- crossprod(
    x = wghts_cov_join[,2],
    y = wghts_cov_join[,3]
  ) * 2

  RISK <- (V + CV) ^ .5 # Portfolio's standard deviation

  # Final calculation
  MVaR = round(
    (1 - qnorm(VaR,mean=1 + R, sd = RISK)) * ndays ^ .5, 4
  ) * 100

  # Prepare it for presentation
  sprintf("Portfolio MVaR is %s%% for the next %s days.", MVaR, ndays)
}
rus.MVaR(
  x = c("LKOH", "SBER", "BELU", "OZON"), 
  VaR=.01, 
  p.weights=c(.4, .3, .2, .1), 
  ndays = 10
) # Test
