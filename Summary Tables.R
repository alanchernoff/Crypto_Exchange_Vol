setwd("C:/Users/acher/crypto/")

library(stargazer)

exchange_list <- c("Binance", "Bitfinex", "Bitstamp", "Coinbase",
                   "Exmo", "Gemini", "HitBTC", "Kraken", "Okcoin")

# ── helper: compute per-exchange summary stats for one coin ───────────────────
load_coin <- function(coin) {
  result <- list()
  
  for (exchange in exchange_list) {
    path <- paste0("raw/", exchange, "_", coin, "_Minute.csv")
    
    tryCatch({
      raw <- read.csv(file = path, header = TRUE)
      
      # Drop the date column (col 1), unroll all minute prices into one vector
      prices <- unlist(raw[, -1], use.names = FALSE)
      prices <- as.numeric(prices)
      prices <- prices[!is.na(prices)]
      
      result[[exchange]] <- prices
      
    }, error = function(e) {
      message("Skipping ", exchange, " (", coin, "): ", e$message)
    })
  }
  
  if (length(result) == 0) stop("No files loaded for ", coin)
  
  # Pad to equal length so data frame construction works
  lengths <- vapply(result, length, integer(1))
  max_len <- max(lengths)
  result  <- lapply(result, function(x) c(x, rep(NA, max_len - length(x))))
  
  as.data.frame(result, check.names = FALSE)
}

# ── build data frames ─────────────────────────────────────────────────────────
eth_df <- load_coin("ETH")
btc_df <- load_coin("BTC")

# ── output tables ─────────────────────────────────────────────────────────────
stargazer(eth_df,
          type         = "latex",
          title        = "Summary Statistics: ETH/USD Minute-Level Data",
          label        = "tab:eth_summary",
          digits       = 2,
          summary.stat = c("n", "mean", "median", "sd", "min", "max"))

stargazer(btc_df,
          type         = "latex",
          title        = "Summary Statistics: BTC/USD Minute-Level Data",
          label        = "tab:btc_summary",
          digits       = 2,
          summary.stat = c("n", "mean", "median", "sd", "min", "max"))
