setwd("C:/Users/acher/crypto/")

#Pick Which Coin
coin = "ETH" #"BTC"

#Read in exchange files and sample them at the given interval
ex_read <- function(exchange,interval,crypto){
  raw=read.csv(file=paste0("raw/",exchange,"_",crypto,"_Minute.csv"),header=TRUE)
  raw <- raw[, -c(1)]
  df = raw[, seq(1, ncol(raw), interval)]
  df
}

#Stdev Calculation
Vol_calc <- function (raw){
  mat=data.matrix(raw, rownames.force = NA) 
  mat=t(mat)
  #mat=log(mat)
  #calculate RV
  dif=diff(mat)
  RV=colSums((dif)^2)
  vol_df=data.frame(RV)
  vol_df
} 


#Exchanges and Sample Intervals
intervals = c(1,5,10)
intervals_t <- c("1 min", "5 min", "10 min")
exchange_list = c("Binance","Bitfinex","Bitstamp","Coinbase","Exmo","Gemini","HitBTC","Kraken","Okcoin") 


#Create the Empty Table
df <- data.frame(matrix(ncol = length(intervals), nrow = length(exchange_list)))
col1 <- paste0(intervals[1]," min")
col2 <- paste0(intervals[2]," min")
col3 <- paste0(intervals[3]," min")
col = do.call(c, list(col1,col2,col3))
col = as.character(col)
colnames(df) <- col
rownames(df) <- exchange_list

#Populate the Empty Table
for (i in 1:length(exchange_list)){
  for (j in 1:length(intervals)){
    raw=ex_read(exchange_list[i],intervals[j],coin)
    RV = Vol_calc(log(raw))
    df[i,j] <- sd(RV[[1]])
  }
}

View(df)

###############################################################

#Export the Tables
write.csv(df,paste0(coin,"stdev_table.csv"), row.names = TRUE)
