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
  jumpday
}

#chi-squared proportion test
test_proportions <- function(proportions) {
  # Convert proportions to observed counts (relative to a fixed total)
  total <- 365
  observed <- proportions * total
  
  # Expected counts if proportions are equal
  expected <- rep(mean(observed), length(observed))
  
  # Perform chi-squared test
  chisq.test(x = observed, p = expected / sum(expected))
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
    jumpday = jump_calc(exchange_list[i],intervals[j])
    df[i,j] <- sum(jumpday/length(jumpday))
  }
}

View(df)


#############################################################################################3


# Test for each column
min1_result <- test_proportions(df$`1 min`)
min5_result <- test_proportions(df$`5 min`)
min10_result <- test_proportions(df$`10 min`)

# Extract chi-squared statistic and p-value
chi_values <- c(min1_result$statistic, min5_result$statistic, min10_result$statistic)
p_values <- c(min1_result$p.value, min5_result$p.value, min10_result$p.value)

# Create a data frame to store the results
chi_df <- data.frame(
  Time_Interval = c("1 min", "5 min", "10 min"),
  Chi_Squared = chi_values,
  P_Value = p_values
)

View(chi_df)

#############################################################################################3



Binance <- data.frame("1 min" = numeric(365), "5 min" = numeric(365), "10 min" = numeric(365))
Coinbase <- data.frame("1 min" = numeric(365), "5 min" = numeric(365), "10 min" = numeric(365))

col1 <- paste0(intervals[1]," min")
col2 <- paste0(intervals[2]," min")
col3 <- paste0(intervals[3]," min")
col = do.call(c, list(col1,col2,col3))
col = as.character(col)
colnames(Binance) <- col
colnames(Coinbase) <- col

for (j in 1:length(intervals)){
  Binance[,j] = jump_calc("Binance",intervals[j])
  Coinbase[,j] = jump_calc("Coinbase",intervals[j])
  }

# Count the number of 1s (successes) in Binance and Coinbase for each time interval
successes_1min <- c(sum(Binance$`1 min`), sum(Coinbase$`1 min`))
successes_5min <- c(sum(Binance$`5 min`), sum(Coinbase$`5 min`))
successes_10min <- c(sum(Binance$`10 min`), sum(Coinbase$`10 min`))

# Total trials (365 for each exchange)
trials <- c(365, 365)

# Perform two-sample proportion tests
prop_min1 <- prop.test(successes_1min, trials)
prop_min5 <- prop.test(successes_5min, trials)
prop_min10 <- prop.test(successes_10min, trials)


# Extract t-statistic and p-value
t_stats <- c(prop_min1$statistic, prop_min5$statistic, prop_min10$statistic)
p_values <- c(prop_min1$p.value, prop_min5$p.value, prop_min10$p.value)

# Create a data frame for results
prop_df <- data.frame(
  Time_Interval = c("1 min", "5 min", "10 min"),
  T_statistic = t_stats,
  P_Value = p_values
)


# View resulting data frame
View(prop_df)
View(chi_df)
View(df)

###############################################################

#Export the Tables
write.csv(df,paste0(coin,"ASJ_table.csv"), row.names = TRUE)
write.csv(chi_df,paste0(coin,"_Chi.csv"), row.names = TRUE)
write.csv(prop_df,paste0(coin,"_t_prop.csv"), row.names = TRUE)
