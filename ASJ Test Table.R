setwd("C:/Users/acher/crypto/")

#Pick Which Coin
coin = "ETH" #"BTC"

#Read in exchange files and sample them at the given interval
ex_read <- function(exchange,interval){
  raw=read.csv(file=paste0("raw/",exchange,"_",coin,"_Minute.csv"),header=TRUE)
  raw <- raw[, -c(1)]
  df = raw[, seq(1, ncol(raw), interval)]
  df
}

#Jump Calculation
jump_calc <- function (exchange,interval){
  dfraw=ex_read(exchange,interval)
  dfmat=data.matrix(dfraw, rownames.force = NA)
  dfmat=t(dfmat)
  dfmat=log(dfmat)
  dfdiff=diff(dfmat)
  delta=1/nrow(dfmat)
  alpha=matrix(0, (nrow(dfmat)-1), ncol(dfmat))
  for (j in 1: ncol(dfmat)){for (i in 1: (nrow(dfmat)-1)){if (abs(dfdiff[i,j]) <= sqrt(delta)){alpha[i,j]=abs((dfdiff[i,j]))^2} else {alpha[i,j]=0}}} 
  alph_fin=5*sqrt(colSums(alpha)) 
  omega=0.47 
  BPD = colSums((abs(dfdiff))^4) 
  kfreq = (nrow(dfmat)+1)/2 
  data_10 = matrix(0, kfreq, ncol(dfmat)) 
  for (j in 1: ncol(dfmat)){for (i in 1:kfreq){data_10[i,j] = dfmat[(i-1)*2+1,j]}} 
  BPK = colSums((abs(diff(data_10)))^4) 
  SPK = BPK/BPD 
  trun_4 = matrix(0,nrow(dfmat)-1,ncol(dfmat)) 
  for (j in 1: ncol(dfmat)){for (i in 1:(nrow(dfmat)-1)){ 
    if (abs(dfdiff[i,j]) <= alph_fin[j]*delta^omega){trun_4[i,j] = abs((dfdiff[i,j]))^4}
    else {trun_4[i,j] = 0}}} 
  mp = pi^(-0.5)*4*gamma(5/2) 
  AP = (delta^(-1)/mp)*colSums(trun_4) 
  trun_8 = matrix(0, (nrow(dfmat)-1), ncol(dfmat)) 
  for (j in 1: ncol(dfmat)){for (i in 1: (nrow(dfmat)-1)){ 
    if (abs(dfdiff[i,j]) <= alph_fin[j]*delta^omega){trun_8[i,j] = abs((dfdiff[i,j]))^8} 
    else {trun_8[i,j] = 0}}} 
  mp_8 = pi^(-0.5)*16*gamma(9/2) 
  AP_8 = (delta^(-3)/mp_8)*colSums(trun_8) 
  Var = (delta* AP_8*160)/(3*AP^2) 
  ASJ = (2 - SPK)/sqrt(Var) 
  normASJ=pnorm(ASJ)
  jumpday = ifelse(normASJ > 0.95, 1, 0)
  jumpday[is.na(jumpday)] <- 0
  sum(jumpday/length(jumpday))
}

#Exchanges and Sample Intervals
intervals = c(1,5,10)
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
    df[i,j] <- jump_calc(exchange_list[i],intervals[j])
    }
}

View(df)

#Export the Table
write.csv(df,paste0(coin,"ASJ_table.csv"), row.names = TRUE)


df2 <- data.frame(A = c(0.50, 0.70, 0.30), B = c(0.80, 0.60, 0.30))

# Function to test if proportions in a column are equal
test_proportions <- function(proportions) {
  # Convert proportions to observed counts (relative to a fixed total)
  total <- 365
  observed <- proportions * total
  
  # Expected counts if proportions are equal
  expected <- rep(mean(observed), length(observed))
  
  # Perform chi-squared test
  chisq.test(x = observed, p = expected / sum(expected))
}

# Test for each column
min1_result <- test_proportions(df$`1 min`)
min1_result$p.value

min5_result <- test_proportions(df$`5 min`)
min5_result$p.value

min10_result <- test_proportions(df$`10 min`)
min10_result$p.value

write.csv(df,paste0(coin,"_Chi.csv"), row.names = TRUE)

prop.test(successes, sample_sizes)
