trade <- function(current, target, price_cur) {
  # Take the current asset, and add whatever needs to be added to make
  # the asset's holdings equivalent to the target. Next, record changes in cash
  # and return everything.
  change = target-current
  cashflow = change*price_cur
  #print(paste("Traded ", change, " for a total of $", cashflow))

  return(cashflow)
}

trade_determine <- function(df, stdevs=1, cash = 1000, scale = 1) {
  y = df$wti
  x = df$brent

  if( dim(joc@V)[1] > 2) {
    const = joc@V[3,1]
  } else {
    const = 0
  }

  y_val = joc@V[1,1]
  x_val = joc@V[2,1]

  coint = y*y_val + x*x_val + const
  colnames(coint) <- "spread"

  error <- spread_jo - mean(spread_jo)
  threshold <- stdevs*(sd(spread_jo) - mean(spread_jo))

  coint$long_wti <- as.integer((coint$spread) < (-threshold))
  coint$long_brent <- as.integer((coint$spread) > (threshold))
  coint$neither <- (coint$long_wti == coint$long_brent)

  coint$position_wti <- 0
  coint$position_brent <- 0

  coint$cash <- cash
  coint$trade_made <- 0

  coint$brent_paid <- NA
  coint$wti_paid <- NA

  ### CONTROL TRADES ###


  # Iterate through each day in the series, and determine what to trade,
  # if at all.
  for(i in 1:dim(coint)[1]) {

    # First we need to check if we should do anything today.
    # If neither is 1, skip to the next day.
    if (coint$neither[[i]] == 1) next
    # Otherwise, do something else.
    else {
      # Do this if we're supposed to be long brent.
      if(coint$long_brent[[i]] == 1) {
        # Now, we check to see if we have some already.
        if(coint$position_brent >= 0) {

          ### BUY MORE
          ### BRENT, SELL
          ### MORE WTI

        }

        # If we aren't already long, we need to close out our positions.
        # This is in contrast to the previous interation of this algorithm,
        # which would invert the position.
        else {
          bt = 0
          wt = 0

          cb = coint$position_brent[[i]]
          cw = coint$position_wti[[i]]

          # trade(current, target, price_cur)

          # Update brent holdings, trade it away.
          cashflow = trade(cb, bt, df$brent[[i]]) # Next, record trade
          coint$trade_made[i] <- 1 # Then we update our cash.
          coint$cash[i] <- coint$cash[[i-1]] + cashflow
        }

      }
      if(coint$long_wti[[i]] == 1) y = y+1
    }
  }

  ### END CONTROL ###

  # Carry forward Brent/WTI paid
  coint$brent_paid <- na.locf(coint$brent_paid)
  coint$wti_paid <- na.locf(coint$wti_paid)

  # Calculate portfolio value
  coint$wti_value <- coint$position_wti*y
  coint$brent_value <- coint$position_brent*(x*-x_val)
  coint$portfolio_value <- coint$wti_value + coint$brent_value

  # Calculate change in value
  coint$wti_delta <- diff.xts(coint$wti_value)
  coint$brent_delta <- diff.xts(coint$brent_value)

  # Calculate returns
  coint$brent_returns <- diff.xts(coint$brent_value)/lag.xts(coint$brent_value)
  coint$wti_returns <- diff.xts(coint$wti_value)/lag.xts(coint$wti_value)
  coint$portfolio_returns <- coint$wti_returns - coint$brent_returns

  return(coint)
}

trades <- trade_determine(price, 0.5, 10000)
