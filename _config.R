Sys.setlocale("LC_TIME", "English")
Sys.setenv(LANG = "en")

###############################################################################
# Cutoff dates to align time-series plots
cutoff <- '2018-01-01-00-00-00'    
cutoff_end <- '2019-10-31-23-59-59'   

fig_width <- 140*4/3
fig_height <- 80*4/3

# mapping of exchange names -----------------------------------------------
replace_exchange_labels <- function(data, exchange) {
  data |> 
    filter(exchange != "bitMEX" & exchange != "bitmex" & exchange != "btcc") |> 
    mutate(exchange = if_else(exchange == "binance", "Binance", exchange),
                  exchange = if_else(exchange == "bitfinex", "Bitfinex", exchange),
                  exchange = if_else(exchange == "bitflyer", "bitFlyer", exchange),
                  exchange = if_else(exchange == "bitmex", "bitMEX", exchange),
                  exchange = if_else(exchange == "bitstamp", "Bitstamp", exchange),
                  exchange = if_else(exchange == "bittrex", "Bittrex", exchange),
                  exchange = if_else(exchange == "btcc", "BTCC", exchange),
                  exchange = if_else(exchange == "cex", "CEX.IO", exchange),
                  exchange = if_else(exchange == "cex.io", "CEX.IO", exchange),
                  exchange = if_else(exchange == "gate", "Gate", exchange),
                  exchange = if_else(exchange == "gate.io", "Gate", exchange),
                  exchange = if_else(exchange == "gatecoin", "Gatecoin", exchange),
                  exchange = if_else(exchange == "gdax", "Coinbase Pro", exchange),
                  exchange = if_else(exchange == "gemini", "Gemini", exchange),
                  exchange = if_else(exchange == "hitbtc", "HitBTC", exchange),
                  exchange = if_else(exchange == "kraken", "Kraken", exchange),
                  exchange = if_else(exchange == "liqui", "Liqui", exchange),
                  exchange = if_else(exchange == "lykke", "Lykke", exchange),
                  exchange = if_else(exchange == "poloniex", "Poloniex", exchange),
                  exchange = if_else(exchange == "xbtce", "xBTCe", exchange))
}
