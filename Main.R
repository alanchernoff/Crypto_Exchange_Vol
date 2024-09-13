setwd("C:/Users/acher/crypto/")

exchange_list = c("Binance","Bitfinex","Bitstamp","Coinbase","Exmo","Gemini","HitBTC","Kraken","Okcoin") 
exchange_list22 = c("Bitfinex","Bitstamp","Coinbase","Exmo","FTX","HitBTC","OKcoin")
#note: Kucoin is available for 2022 but the data is incomplete
#note: Binance is in USDT for BTC and ETH
#note: Kraken is in USDT for ETH only
#note: kucoin and FTX stop in 11/22
#note: kucoin is in USDT for btc and ETH

Vol_list <- data.frame(Name = rep(NA, 365))

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

for(i in 1:length(exchange_list)){
  exchange=exchange_list[i]
  raw=read.csv(file=paste0("raw/",exchange,"_ETH_Minute.csv"),header=TRUE)
  raw <- raw[, -c(1)]
  RV=Vol_calc(log(raw))
  name_str=exchange_list[i]
  Vol_list[name_str] <- RV
}

Vol_list<- Vol_list[, -c(1)]
View(Vol_list)

