# load packages
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(XML))
suppressPackageStartupMessages(library(stringr))

# set parameter options
options(stringsAsFactors = FALSE)
options("scipen" = 100)

# Set the current date parameter
Today <- str_replace_all(as.character(today()),
                         "-",
                         "")
# Get BTC data
btc <- GET(paste0("https://coinmarketcap.com/currencies/bitcoin/historical-data/?start=20150807&end=",
                  Today))
btc <- as.data.frame(readHTMLTable(doc = content(btc,
                                                 "text")))

# Format
colnames(btc) <- str_replace_all(colnames(btc),
                                 "NULL.",
                                 "")
btc$Date <- as.Date(btc$Date,
                    format = "%b %d, %Y")
btc$Open <- as.numeric(btc$Open)
btc$High <- as.numeric(btc$High)
btc$Low <- as.numeric(btc$Low)
btc$Close <- as.numeric(btc$Close)
btc$Volume <- str_replace_all(btc$Volume,
                              ",",
                              "")
btc$Volume <- as.numeric(btc$Volume)
btc$Market.Cap <- str_replace_all(btc$Market.Cap,
                                  ",",
                                  "")
btc$Market.Cap <- as.numeric(btc$Market.Cap)

# Calculate stats
MU <- mean(btc$Close)
SD <- sd(btc$Close)
btc$Zclose <- (btc$Close - MU) / SD

# Get ETH data
eth <- GET(paste0("https://coinmarketcap.com/currencies/ethereum/historical-data/?start=20150807&end=",
                  Today))
eth <- as.data.frame(readHTMLTable(doc = content(eth,
                                                 "text")))

# Format
colnames(eth) <- str_replace_all(colnames(eth),
                                 "NULL.",
                                 "")
eth$Date <- as.Date(eth$Date,
                    format = "%b %d, %Y")
eth$Open <- as.numeric(eth$Open)
eth$High <- as.numeric(eth$High)
eth$Low <- as.numeric(eth$Low)
eth$Close <- as.numeric(eth$Close)
eth$Volume <- str_replace_all(eth$Volume,
                              ",",
                              "")
eth$Volume <- as.numeric(eth$Volume)
eth$Market.Cap <- str_replace_all(eth$Market.Cap,
                                  ",",
                                  "")
eth$Market.Cap <- as.numeric(eth$Market.Cap)

# Calculate stats
MU <- mean(eth$Close)
SD <- sd(eth$Close)
eth$Zclose <- (eth$Close - MU) / SD

# set parameters for visualization
if (max(btc$Zclose) > max(eth$Zclose)) {
  Max <- max(btc$Zclose)
} else {
  Max <- max(eth$Zclose)
}
N <- which(btc$Date == (btc$Date[1] - years(1)))

# Plot visualizaton
print(ggplot(btc[1:N,],
             aes(x = Date,
                 y = Zclose)) +
        geom_line(color = "blue") +
        geom_line(aes(y = eth$Zclose[1:N]),
                  color = "red") +
        ylab("Z-score Transformation of Daily Close") +
        xlab(paste0(month(btc$Date[N],
                          label = TRUE,
                          abbr = FALSE),
                    " ",
                    year(btc$Date[N]),
                    " through ",
                    month(btc$Date[1],
                          label = TRUE,
                          abbr = FALSE),
                    " ",
                    year(btc$Date[1]))) +
        scale_x_date(breaks = pretty_breaks(n = 10),
                     date_labels = "%b") +
        annotate("text",
                 x = (btc$Date[1] - months(11)),
                 y =  (Max * 0.95),
                 label = "Bitcoin   (BTC)",
                 color = "blue") +
        annotate("text",
                 x = (btc$Date[1] - months(11)),
                 y = (Max * 0.9),
                 label = "Ethereum (ETH)",
                 color = "red") +
        annotate("text",
                 x = btc$Date[1] + days(9),
                 y = btc$Zclose[1],
                 label = paste0("$",
                                format(btc$Close[1],
                                       big.mark = ",")),
                 color = "blue") +
        annotate("text",
                 x = eth$Date[1] + days(9),
                 y = eth$Zclose[1],
                 label = paste0("$",
                                format(eth$Close[1],
                                       big.mark = ",")),
                 color = "red")
)
