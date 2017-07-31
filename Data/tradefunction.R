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
  
  for(i in 1:dim(coint)[1]){
    # There's two possible choices for each asset -
    # 1. We are long the asset we're supposed to be, so buy more.
    # 2. We are short when we're supposed to be long, so buy enough to make
    #    portfolio holdings the beginning of the holding, i.e. 1 for wti
    #    or 0.85 or whatever for Brent.
    
    if(identical(trades$neither[[i]],1)) {
      next
    } else {
      # If we're supposed to be long WTI, do this:
      if(identical(trades$long_wti[[i]],1)) {
        # If we are supposed to be long WTI and we're already long, buy more.
        if(sign(coint$position_wti[[i-1]]) >= 0) {
          # trade(current, target, price)
          # return(list[change,cashflow])
          wti_target = coint$position_wti[i-1] + 1*scale
          brent_target = coint$position_brent[i-1] + x_val*scale
          
          wti_price = y[[i]]
          brent_price = x[[i]]
          
          cashflow = trade(coint$position_wti[i-1], wti_target, wti_price)
          coint$position_wti[i] <- wti_target
          coint$cash[i] <- coint$cash[i-1] + cashflow
          coint$wti_paid[i] <- cashflow
          
          cashflow = trade(coint$position_brent[i-1], brent_target,brent_price)
          coint$position_brent[i] <- brent_target
          coint$cash[i] <- coint$cash[i-1] + cashflow
          coint$brent_paid[i] <- cashflow
          
          coint$trade_made[i] <- 1
        }
        
        # Otherwise, we need to buy enough to make our holdings long. WTI needs
        # to be set to +1, Brent to -x_val.
        if((sign(coint$position_wti[[i-1]]) == -1)) {
          wti_target = 1*scale
          brent_target = x_val*scale
          
          wti_price = y[[i]]
          brent_price = x[[i]]
          
          cashflow = trade(coint$position_wti[i-1], wti_target,wti_price)
          coint$position_wti[i] <- wti_target
          coint$cash[i] <- coint$cash[i-1] + cashflow
          coint$wti_paid[i] <- cashflow
          
          cashflow = trade(coint$position_brent[i-1], brent_target,brent_price)
          coint$position_brent[i] <- brent_target
          coint$cash[i] <- coint$cash[i-1] + cashflow
          coint$brent_paid[i] <- cashflow
          
          coint$trade_made[i] <- 1
        }
        
      }
      
      # If we're supposed to be long Brent, do this.
      if(identical(trades$long_brent[[i]],1) & identical(trades$neither[[i]],0)) {
        if(sign(coint$position_brent[[i-1]]) >= 0) {
          
          wti_target = coint$position_wti[i-1] - 1*scale
          brent_target = coint$position_brent[i-1] - x_val*scale
          
          wti_price = y[[i]]
          brent_price = x[[i]]
          
          cashflow = trade(coint$position_wti[i-1], wti_target,
                           wti_price)
          coint$position_wti[i] <- wti_target
          coint$cash[i] <- coint$cash[i-1] + cashflow
          coint$wti_paid[i] <- cashflow
          
          cashflow = trade(coint$position_brent[i-1], brent_target,
                           brent_price)
          coint$position_brent[i] <- brent_target
          coint$cash[i] <- coint$cash[i-1] + cashflow
          coint$brent_paid[i] <- cashflow
          
          coint$trade_made[i] <- 1
        }
        
        # If we're short brent, flip the position.
        if((sign(coint$position_brent[[i-1]]) == -1)) {
          wti_target = -1*scale
          brent_target = -x_val*scale
          
          wti_price = y[[i]]
          brent_price = x[[i]]
          
          cashflow = trade(coint$position_wti[i-1], wti_target,
                           wti_price)
          coint$position_wti[i] <- wti_target
          coint$cash[i] <- coint$cash[i-1] + cashflow
          coint$wti_paid[i] <- cashflow
          
          cashflow = trade(coint$position_brent[i-1], brent_target,
                           brent_price)
          coint$position_brent[i] <- brent_target
          coint$cash[i] <- coint$cash[i-1] + cashflow
          coint$brent_paid[i] <- cashflow
          
          coint$trade_made[i] <- 1
        }
      }
    }
    
    
  }
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
