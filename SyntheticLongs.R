require("quantmod");require("data.table")
RAW = Sys.Date()
daytoday = format(RAW,"%Y%m%d")
FILES = list.files("/Volumes/3TB/CBOE/ALL",full.names = TRUE)
# FILES = tail(FILES,1) # recent
# FILES= FILES[50] # Falling
# FILES= FILES[110] # Rising

# read in options
ops = readRDS(FILES)

# calculates the Synthetic Longs for all expirations available
getSyntheticLongs = function(ticker){
    
  # subset options
  tmp = subset(ops, ops$Symbol == ticker)
  # save the date/timestamp
  DATE = unique(tmp$Date)[1]
  
  # get all expirations
  EXP = unique(tmp$expiry)
  
  synth = lapply(as.list(EXP), function(xp){
    OPT = subset(tmp, tmp$expiry == xp)
    STRK = unique(OPT$strike)
    # subset strikes / form straddle form
    ALL = lapply(as.list(STRK), function(strk){
      # subset strike then divide by Call/Put
      df = subset(OPT, OPT$strike == strk)  
      CALL = subset(df,df$flag == "C")
      PUT = subset(df,df$flag == "P")
      # straddle view
      CALL = cbind(CALL$flag,round(as.numeric(CALL$Mid),2), round(as.numeric(CALL$change),2),
                   round(as.numeric(CALL$percent_change),2),round(as.numeric(CALL$Delta),2),
                   round(as.numeric(CALL$Gamma),2),round(as.numeric(CALL$Theta),2),
                   round(as.numeric(CALL$calc_IV),2),round(as.numeric(CALL$volume),2),
                   round(as.numeric(CALL$open_interest),2),round(as.numeric(CALL$strike),2)) %>% as.data.frame()
      colnames(CALL) <- c("cFlag","cMid","cChange","cPctChange","cDelta","cGamma",
                          "cTheta","cIV","cVol","cOI","Strike")
      PUT = cbind(PUT$expiry,PUT$flag,round(as.numeric(PUT$Mid),2), round(as.numeric(PUT$change),2),
                   round(as.numeric(PUT$percent_change),2),round(as.numeric(PUT$Delta),2),
                   round(as.numeric(PUT$Gamma),2),round(as.numeric(PUT$Theta),2),
                   round(as.numeric(PUT$calc_IV),2),round(as.numeric(PUT$volume),2),
                   round(as.numeric(PUT$open_interest),2),as.numeric(PUT$days2Exp),
                   round(as.numeric(PUT$stkClose),2)) %>% as.data.frame()
      colnames(PUT) <- c("Expiration","pFlag","pMid","pChange","pPctChange","pDelta",
                         "pGamma","pTheta","pIV","pVol","pOI","days2exp","stkClose")
      # combine & divide by expiry
      ALL = cbind(CALL,PUT)
      # Synthetic Long - Buy Call & Sell Put
      ALL$Synthetic = round((as.numeric(ALL$cMid)-as.numeric(ALL$pMid))+
                              as.numeric(ALL$Strike),4)
      # difference between current Stock Price & Synthetic
      ALL$Diff = as.numeric(ALL$Synthetic) - as.numeric(ALL$stkClose)
      # Premium, Discount, Par
      if(as.numeric(ALL$Diff)<0){SUB="Discount"}
      if(as.numeric(ALL$Diff)>0){SUB="Premium"}
      if(as.numeric(ALL$Diff)==0){SUB="Par"}
      ALL$SUB = SUB
      
      ALL
    })
    # row bind all strikes in straddle view
    ALL = rbindlist(ALL,use.names = TRUE,fill = TRUE)
    
    
    # return ALL
    ALL
  })
  # rowbind all expirations
  synth = rbindlist(synth,use.names = TRUE,fill = TRUE)
  # add cost (Buy Call[Pay Premium] + Sell Put[Margin @ Strike]) x 100
  synth$Debit = (as.numeric(synth$cMid)+as.numeric(synth$Strike))*100
  # Credit[The Put Premium]
  synth$Credit = as.numeric(synth$pMid)*100
  # NET
  synth$NET =  synth$Debit - synth$Credit
  # add ticker (in case multiple tickers are combined)
  synth$Symbol = ticker
  # add date/timestamp
  synth$Date = DATE
  
  
  # return synthetic longs
  synth
}
# get SL for:
SS = getSyntheticLongs(ticker="F")

# results at expiration function
getSLreturn = function(SS, expiration, strike, nContracts){
  # subset data to required expiration/strike
  df = subset(SS,SS$Expiration == expiration & SS$Strike == strike)
  # where did the stock closed on expiration
  stk = getSymbols(paste(df$Symbol), from="2021-01-01",auto.assign = FALSE)
  # subset stk to days held
  stk = stk[paste0(df$Date,"/",expiration)]
  # # MAX/MIN price during time
  # MIN = min(Lo(stk))
  # MAX = max(Hi(stk))
  # RANGE = MAX - MIN
  stkClose = Cl(stk[nrow(stk)]) %>% as.numeric
  # If the stk closes below the call strike price, the call will have no intrinsic value
  # if the stk is above the call strike price then it will be worth something at expiration
  callPRC = ifelse(stkClose < as.numeric(df$Strike), 0, stkClose-as.numeric(df$Strike))
  # If the stk closes below the put strike price, the put will be worth something at expiration
  # if the stk is above the put strike price, the call will have no intrinsic value
  putPRC = ifelse(stkClose > as.numeric(df$Strike), 0, as.numeric(df$Strike)-stkClose)
  # calculate PnL for stock
  stkPnL = -(stkClose - as.numeric(df$stkClose))*100*nContracts
  stkRet = round(stkClose/as.numeric(df$stkClose)-1,4)
  # calculate the PnL for the options per contract (x100)
  callPnL   = (callPRC - as.numeric(df$cMid))*100*nContracts
  putPnL    = (as.numeric(df$pMid) - putPRC)*100*nContracts
  
  # aggregate cost of trade = Short 100 shares + NET cost of options (cost of calls + margin for put)
  tradeCost = as.numeric(df$stkClose)*100*nContracts + as.numeric(df$NET)*nContracts
  # gross PnL should be the amount it was discounted (in df)
  grossPnL = (stkPnL + callPnL + putPnL)
  # minus commissions (x2 for round lot i.e. opening & closing)
  #netPnL   = grossPnL - (nContracts*tradeFee*2)
  # Percentage return
  #pctRet = round(netPnL/tradeCost,4)
  # opening prices
  b4 = as.data.frame(cbind(as.numeric(df$stkClose),as.numeric(df$Strike),as.numeric(df$cMid),as.numeric(df$pMid),
                      nContracts,-as.numeric(df$Debit)*nContracts, as.numeric(df$Credit)*nContracts,
                      as.numeric(df$stkClose)*100*nContracts,tradeCost))
  # debit (call price + margin for put)
  # credit (put premium for selling)
  colnames(b4) = c("stkPrice","strike","callPrc","putPrc",'nContracts',"debit",'credit','shortStk','net')
  
  af = as.data.frame(cbind(as.numeric(stkClose),as.numeric(df$Strike),as.numeric(callPRC),as.numeric(putPRC),
                           nContracts,(as.numeric(callPRC)*100*nContracts)+as.numeric(df$Strike)*100*nContracts,
                           putPRC*100*nContracts,-as.numeric(stkClose)*100*nContracts,0
                     ))
  colnames(af) = c("stkPrice","strike","callPrc","putPrc",'nContracts',"debit",'credit','shortStk','net')
  
  dt = rbind(b4,af)
  dt = rbind(dt,cbind((dt[2,1:5]-dt[1,1:5]),(dt[2,6:9]+dt[1,6:9])))
  dt$credit[3] = dt$credit[1]-dt$credit[2]
  # add gross
  dt$net[3] = grossPnL
  # return results
  data.frame(dt,row.names = c("start","expiration","PnL"))
}



# Falling 
fall = getSLreturn(SS,expiration = "2021-07-23",strike = 5, nContracts = 10)

# Rising
rise = getSLreturn(SS,expiration = "2021-10-15",strike = 8, nContracts = 1)  # PUT OTM
rise = getSLreturn(SS,expiration = "2021-10-15",strike = 30, nContracts = 1) # PUT ITM

