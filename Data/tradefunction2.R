hpr <- function(coint) {
  first = coint$portfolio_value[[1]]
  last = coint$portfolio_value[[dim(coint)[1]]]
  return ((last-first)/first)
}

sharp <- function(returns) {
  ri = mean(returns, na.rm = TRUE)
  s = sd(returns, na.rm = TRUE)
  return(ri/s)
}

trade <- function(current, target, price_cur, scale = 1) {
  # Take the current asset, and add whatever needs to be added to make
  # the asset's holdings equivalent to the target. Next, record changes in cash
  # and return everything.
  change = target-current
  cashflow = -change*price_cur
  #print(paste("Traded ", change, " for a total of $", cashflow))

	# We give trade (for example)
	# cb = -12
	# bt = 0
	# price_cur = 51.54
 
	# So change = 0-(-12) = 12
	# cashflow = 12*51.54*1 = 618.48

  return(cashflow)
}

trade_determine2 <- function(df, stdevs=1, cash = 1000, scale = 1) {
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
  threshold <- stdevs

  coint$long_wti <- as.integer((coint$spread) < (-threshold))
  coint$long_brent <- as.integer((coint$spread) > (threshold))
  coint$neither <- (coint$long_wti == coint$long_brent)

  coint$position_wti <- 0
  coint$position_brent <- 0

  coint$cash <- cash
  coint$trade_made <- 0

	coint$brentcf <- 0
	coint$wticf <- 0

  coint$brent_paid <- NA
  coint$wti_paid <- NA

	coint$brent <- df$brent
	coint$wti <- df$wti

	######################
  ### CONTROL TRADES ###
	######################

  # Iterate through each day in the series, and determine what to trade,
  # if at all.
  for(i in 2:dim(coint)[1]) {
    coint$position_brent[i] <- coint$position_brent[[i-1]]
    coint$position_wti[i] <- coint$position_wti[[i-1]]
    
    coint$cash[i] = coint$cash[[i-1]]
    # First we need to check if we should do anything today.
    # If neither is 1, skip to the next day.
    if(i==1) {
      next
    }
    else if(i == dim(coint)[1]) {
      bt = 0
      wt = 0
      
      cb = coint$position_brent[[i-1]]
      cw = coint$position_wti[[i-1]]
      
      # trade(current, target, price_cur)
      
      # Update brent holdings, trade it away.
      cashflow = trade(cb, bt, coint$brent[[i]]) # Next, record trade
      # We give trade (for example)
      # cb = -12
      # bt = 0
      # price_cur = 51.54
      coint$trade_made[i] <- 1 # Then we update our cash.
      coint$cash[i] <- coint$cash[[i]] + cashflow
      coint$brent_paid[i] <- coint$brent[[i]]
      coint$brentcf[i] <- cashflow
      coint$position_brent[i] <- bt
      
      # Update WTI holdings.
      cashflow = trade(cw, wt, coint$wti[[i]]) # Next, record trade
      coint$trade_made[i] <- 1 # Then we update our cash.
      coint$cash[i] <- coint$cash[[i]] + cashflow
      coint$wti_paid[i] <- coint$wti[[i]]
      coint$wticf[i] <- cashflow
      coint$position_wti[i] <- wt
      next
    }
    else if( (coint$long_brent[[i]] ==0) & (coint$long_wti[[i]] == 0))  {
			
      next
		}
    # Otherwise, do something else.
    else {
      # Do this if we're supposed to be long brent.
      if((coint$long_brent[[i]] == 1) & (coint$long_wti[[i]] == 0)) {
        # Now, we check to see if we have some already.
        if(coint$position_brent[[i]] >= 0) {
					bt = coint$position_brent[[i-1]] - scale*x_val
					wt = coint$position_wti[[i-1]] - scale

					cb = coint$position_brent[[i]]
					cw = coint$position_wti[[i]]

					# trade(current, target, price_cur)

					# Update brent holdings, trade it away.
					cashflow = trade(cb, bt, coint$brent[[i]]) # Next, record trade
					coint$trade_made[i] <- 1 # Then we update our cash.
					coint$cash[i] <- coint$cash[[i]] + cashflow
					coint$brent_paid[i] <- coint$brent[[i]]
					coint$position_brent[i] <- bt
					coint$brentcf[i] <- cashflow

					# Update WTI holdings.
					cashflow = trade(cw, wt, coint$wti[[i]]) # Next, record trade
					coint$trade_made[i] <- 1 # Then we update our cash.
					coint$cash[i] <- coint$cash[[i]] + cashflow
					coint$wti_paid[i] <- coint$wti[[i]]
					coint$position_wti[i] <- wt
					coint$wticf[i] <- cashflow
					next
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
          cashflow = trade(cb, bt, coint$brent[[i]]) # Next, record trade
					# We give trade (for example)
					# cb = -12
					# bt = 0
					# price_cur = 51.54
          coint$trade_made[i] <- 1 # Then we update our cash.
          coint$cash[i] <- coint$cash[[i]] + cashflow
					coint$brent_paid[i] <- coint$brent[[i]]
					coint$brentcf[i] <- cashflow
					coint$position_brent[i] <- bt

					# Update WTI holdings.
					cashflow = trade(cw, wt, coint$wti[[i]]) # Next, record trade
          coint$trade_made[i] <- 1 # Then we update our cash.
          coint$cash[i] <- coint$cash[[i]] + cashflow
					coint$wti_paid[i] <- coint$wti[[i]]
					coint$wticf[i] <- cashflow
					coint$position_wti[i] <- wt

					next
        }

      }
      if((coint$long_wti[[i]] == 1) & (coint$long_brent[[i]] == 0)) {
        # If we're already long, great! Be more long.
        if(coint$position_wti[[i]] >= 0) {
          bt = coint$position_brent[[i-1]] + scale*x_val
          wt = coint$position_wti[[i-1]] + scale
          
          cb = coint$position_brent[[i]]
          cw = coint$position_wti[[i]]
          
          # trade(current, target, price_cur)
          
          # Update Brent holdings
          cashflow = trade(cb, bt, coint$brent[[i]]) # Next, record trade
          coint$trade_made[i] <- 1 # Then we update our cash.
          coint$cash[i] <- coint$cash[[i]] + cashflow
          coint$brent_paid[i] <- coint$brent[[i]]
          coint$position_brent[i] <- bt
          coint$brentcf[i] <- cashflow
          
          # Update WTI holdings.
          cashflow = trade(cw, wt, coint$wti[[i]]) # Next, record trade
          coint$trade_made[i] <- 1 # Then we update our cash.
          coint$cash[i] <- coint$cash[[i]] + cashflow
          coint$wti_paid[i] <- coint$wti[[i]]
          coint$position_wti[i] <- wt
          coint$wticf[i] <- cashflow
          next
        }
        else {
          bt = 0
          wt = 0
          
          cb = coint$position_brent[[i]]
          cw = coint$position_wti[[i]]
          
          # trade(current, target, price_cur)
          
          # Update brent holdings, trade it away.
          cashflow = trade(cb, bt, coint$brent[[i]]) # Next, record trade
          coint$trade_made[i] <- 1 # Then we update our cash.
          coint$cash[i] <- coint$cash[[i]] + cashflow
          coint$brent_paid[i] <- coint$brent[[i]]
          coint$brentcf[i] <- cashflow
          coint$position_brent[i] <- bt
          
          # Update WTI holdings.
          cashflow = trade(cw, wt, coint$wti[[i]]) # Next, record trade
          coint$trade_made[i] <- 1 # Then we update our cash.
          coint$cash[i] <- coint$cash[[i]] + cashflow
          coint$wti_paid[i] <- coint$wti[[i]]
          coint$wticf[i] <- cashflow
          coint$position_wti[i] <- wt
          next
        }
      }
      
    }
  }

	###################
  ### END CONTROL ###
	###################

  # Carry forward Brent/WTI paid
  coint$brent_paid <- na.locf(coint$brent_paid)
  coint$wti_paid <- na.locf(coint$wti_paid)

  # Calculate portfolio value
  coint$wti_value <- coint$position_wti*coint$wti
  coint$brent_value <- coint$position_brent*coint$brent
  coint$portfolio_value <- coint$wti_value + coint$brent_value + coint$cash

  # Calculate change in value
  coint$wti_delta <- diff.xts(coint$wti_value)
  coint$brent_delta <- diff.xts(coint$brent_value)

  # Calculate returns
  # (Not needed, returns are calculated by portfolio value)
  #coint$brent_returns <- diff.xts(coint$brent_value)/lag.xts(coint$brent_value)
  #coint$wti_returns <- diff.xts(coint$wti_value)/lag.xts(coint$wti_value)
  
  coint$portfolio_returns <- diff.xts(coint$portfolio_value)/lag.xts(coint$portfolio_value)
  coint$log_portfolio_returns <- log(coint$portfolio_value/lag.xts(coint$portfolio_value))
  coint$simplemod_portfolio_returns <- coint$portfolio_value/lag(coint$portfolio_value) - 1

  # Net Cash
  coint$netcash <- coint$brentcf+coint$wticf
  return(coint)
}
