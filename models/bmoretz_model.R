library(data.table)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(GGally)
library(ggthemes)
library(formattable)
library(scales)
library(reshape2)
library(skimr)
library(gridExtra)
library(lessR)
library(ggiraphExtra)
library(olsrr)
library(caret)
library(sjPlot)
library(sjmisc)
library(car)
library(WVPlots)
library(MASS)
library(Metrics)
library(stringr)
library(Rtsne)
library(plotly)
library(Quandl)
library(tidyverse)
library(quantmod)
library(ggcorrplot)
library(tidyquant)
library(timetk)
library(tidyverse)
library(tibbletime)
library(forecast)

#####################################################################
######################### EDA #######################################
#####################################################################

data.path <- "D:/Projects/MSDS-Capstone/data"

setwd(data.path)

theme_set(theme_light())

# Theme Overrides
theme_update(plot.title = element_text(hjust = 0.5),
             axis.text.x = element_text(size = 10),
             axis.text.y = element_text(size = 10),
             axis.title = element_text(face = "bold", size = 12, colour = "steelblue4"),
             legend.position = "top", legend.title = element_blank())

# Data files
data.energy <- read_csv( file = "northwestern_energy_all_energy_ohlc.csv") 
data.symbology <- read_csv( file = "symbol_description.csv")

# Global Symbol look-up
trading.symbols <- colnames(data.energy)[
  sapply(colnames(data.energy), FUN = function(symbol) {
    !str_contains(symbol, "_")
  })]

data <- data.energy

# Utility Functions

getDataForSymbol <- function( symbol, data = data.energy ) {
  
  base.cols <- c("open", "high", "low", "close", "volume")
  
  symbol.cols <- c("nymex_date", as.vector(sapply(base.cols, function( c ) { 
    paste0(symbol, "_", c) }, simplify = T)))
  
  data.symbol <- data[, symbol.cols]
  data.symbol <- data.symbol[order(data.symbol$nymex_date),]
  
  colnames(data.symbol) <- as.vector(sapply(colnames(data.symbol), FUN = function(c) { 
    str_replace(c, paste0(symbol, "_"), "") }, simplify = T))
  
  data.symbol <- data.symbol[complete.cases(data.symbol),]
  data.symbol$spotPrice <- data.symbol[["close"]]

  # calculate returns from prices
  prices <- data.symbol$spotPrice
  n <- length(prices)
  ret <- prices[-1] / prices[-n] - 1
  
  # store it
  data.symbol$return <- c(0, ret)
  data.symbol$logReturn <- log(1 + data.symbol$return)
  
  colnames(data.symbol)
  
  data.symbol[-c(1:2),] # throw away the first record that has no return data.
}

plotReturns <- function( symbol, energy_data, start_date = "2019-1-1" ){ 
  
  data <- energy_data[[symbol]]
  data.plot <- data[data$nymex_date >= start_date, c("nymex_date", "spotPrice", "return", "logReturn")]
  
  p1 <- ggplot(data.plot, aes(nymex_date, return)) +
    geom_line()
  
  p2 <- ggplot(data.plot, aes(nymex_date, logReturn)) +
    geom_line()
  
  p3 <- ggplot(data.plot, aes(return, fill = ..count..)) +
    geom_histogram()
  
  p4 <- ggplot(data.plot, aes(logReturn, fill = ..count..)) +
    geom_histogram()
  
  grid.arrange(p1, p2, p3, p4, nrow = 2, top = paste("Returns for", symbol, " since ", start_date))
}

getRetVsT <- function(returns, df_canidates = c(1, 2, 4, 6, 10, 20)) {
  plots <- lapply(df_canidates, function(df) {
    
    n <- length(returns)
    q_range <- (1:n) / (n+1)
    
    data <- data.table(ret = returns, theoretical = qt(q_range, df))
    data$theoretical <- sort(data$theoretical)
    
    model <- lm(qt(c(0.25,0.75), df = df) ~ quantile(data$ret,c(0.25,0.75)))
    
    ggplot(data, aes(x = sort(ret), y = theoretical)) +
      geom_abline(col = 'cornflowerblue', lwd = 1.3, slope = model$coefficients[2], intercept = model$coefficients[1]) +
      geom_point() +
      labs(title = paste("df = ", df), 
           x = "returns",
           y = "theoretical")
  })
  
  do.call(grid.arrange, c(plots, top = "QQ-Plot: returns vs t-distribution"))
}

getRetDensityVsNorm <- function(returns) {
  data <- data.table(x = seq(min(returns), max(returns), length.out = length(returns)), y = returns)
  
  ggplot(data, aes(y)) +
    geom_density(aes(col = "KDE"), lwd = 1) +
    geom_line(aes(x, dnorm(x, mean = mean(returns), sd = sd(returns)), col = "normal(mean, sd)"), lwd = 1.1, linetype = "longdash") +
    geom_line(aes(x, dnorm(x, mean = median(returns), sd = mad(returns)), col = "normal(median, mad)"), lwd = 1.1, linetype = "dashed") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = paste("KDE: Returns Vs Normal, n=", nrow(data)), y = "density", x = "") +
    theme(legend.position = "right")
}

getRetNormQuantiles <- function(returns, quantiles = c(0.25,0.1,0.05,0.025,0.01,0.0025), desc = "") {
  plots <- lapply(quantiles, function(p) {
    
    p_value <- p
    n <- length(returns)
    q_range <- (1:n) / (n+1)
    
    data <- data.table(ret = returns, theoretical = qnorm(q_range))
    data$theoretical <- sort(data$theoretical)
    
    model <- lm(qnorm(c(p_value, 1-p_value)) ~ quantile(data$ret, c(p_value, 1-p_value)))
    
    ggplot(data, aes(x = sort(ret), y = theoretical)) +
      geom_abline(col = 'cornflowerblue', lwd = 1.3, slope = model$coefficients[2], intercept = model$coefficients[1]) +
      geom_point() +
      labs(title = paste("p = ", p_value), 
           x = "returns",
           y = "theoretical")
  })
  
  do.call(grid.arrange, c(plots, top = paste(desc, "vs Normal Quantiles")))
}

getCorr <- function( data = commodites ) {
  ret <- data.table()
  
  for(symbol in names(data)) {
    s <- data.table(return = data[[symbol]]$logReturn)
    colnames(s) <- symbol
    
    ret <- cbind(ret, s)
  }
  
  ggcorrplot(cor(ret),
             type = "lower",
             method = "circle",
             colors = c("tomato2", "white", "springgreen3"),
             lab_size = 3,
             title = "Energy Commodity Correlations")
}

candlestick <- function(symbol, start_date = "2019-1-1", data = commodites ) {

  desc <- data.symbology[data.symbology$Symbol == toupper(symbol),]$Description
  
  d <- data[[symbol]][data[[symbol]]$nymex_date >= start_date,]
  p <- d %>% 
    plot_ly(x = ~nymex_date, type = "candlestick",
            open = ~open, close = ~spotPrice,
            high = ~high, low = ~low) %>%
    add_lines(x = ~nymex_date, y = ~open, line = list(color = 'black', width = 0.75), inherit = F) %>%
    layout(title = paste(symbol, "activity since", start_date))
  
  v <- d %>% 
    plot_ly(x = ~nymex_date, y = ~volume, type='bar', name = "Volume",
            colors = c('#17BECF','#7F7F7F'))
  
  rs <- list(visible = TRUE, x = 0.5, y = -0.055,
             xanchor = 'center', yref = 'paper',
             font = list(size = 9),
             buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward')
             ))
  
  pp <- subplot(p, v, heights = c(0.7,0.2), nrows=2,
               shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste( desc, " : ", format(as.Date(start_date), "%b %d %Y"), "-", format(as.Date(max(d$nymex_date)), "%b %d %Y")),
           xaxis = list(rangeselector = rs),
           legend = list(orientation = 'h', x = 0.5, y = 1,
                         xanchor = 'center', yref = 'paper',
                         font = list(size = 10),
                         bgcolor = 'transparent'))
  pp
}

pretty_kable <- function(data, title, dig = 2) {
  kable(data, caption = title, digits = dig) %>%
    kable_styling(bootstrap_options = c("striped", "hover"))
}

getCorTable <- function( values ) {
  p.table <- data.table(round(values, 4), keep.rownames = T)
  
  formattable(p.table, align = c("l", "c", "c", "c", "r"),
              list(`Indicator Name` = formatter("span", style = ~style(color = "grey", font.weight = "bold"))
              ))
}

###############################################################
# Indexes
##############################################################

indices <- colnames(data.energy)[colnames(data.energy) %in% data.symbology$Symbol]


###############################################################
# EDA
##############################################################

energy.commodities <- c("cl", "sc", "hp", "mt", "ng", "qa", "qg", "ve", "rb", "ho")

commodites <- lapply(energy.commodities, FUN = function( s ) {
  getDataForSymbol(s)
})

names(commodites) <- energy.commodities

getRetNormQuantiles(rnorm(2000), desc = "RNORM (baseline)") # for reference

## Symbol Simple Correlations (Pearson)

getCorr(commodites)


#### RB

plotReturns("rb", commodites)

rb.ret <- commodites$rb$logReturn

getRetVsT(rb.ret) # t-distribution, df = 6

getRetDensityVsNorm(rb.ret) # strogest fit: median / mad

getRetNormQuantiles(rb.ret, desc = "RO")

symbol <- "rb"
start_date <- "2019-6-1"

candlestick(symbol, start_date, commodites)

#### HO

plotReturns("ho", commodites)

ho.ret <- commodites$ho$logReturn

getRetVsT(ho.ret) # t-distribution, df = 4, watch out for the heavy tail

getRetDensityVsNorm(ho.ret) # strogest fit: median / mad

getRetNormQuantiles(ho.ret, desc = "HO")

symbol <- "ho"
start_date <- "2019-1-1"

candlestick(symbol, start_date, commodites)

#### CL

plotReturns("cl", commodites)

cl.ret <- commodites$cl$logReturn

getRetVsT(cl.ret) # t-distribution, df = 4, watch out for the heavy tail

getRetDensityVsNorm(cl.ret) # strogest fit: median / mad

getRetNormQuantiles(cl.ret, desc = "CL")

symbol <- "cl"
start_date <- "2019-6-1"

candlestick(symbol, start_date, commodites)

########
###### Crude

getReturnsForSymbol <- function( symbol, data = commodites ) {
  r <- as.data.table(commodites[[symbol]][, c("nymex_date", "logReturn")])[, .(Date = nymex_date, Return = logReturn)]
  colnames(r)[2] <- symbol
  
  r
}

brent <- getReturnsForSymbol("sc")
wti <- getReturnsForSymbol("cl")
gulf <- getReturnsForSymbol("mt")
ve <- getReturnsForSymbol("ve")
gas <- getReturnsForSymbol("rb")
natgas <- getReturnsForSymbol("ng")
ho <- getReturnsForSymbol("ho")

crude.wide <- merge(merge(merge(brent, wti, on = c("Date")), gulf, on = c("Date")), ve, on = c("Date"))

commodity.wide <- merge(merge(merge(crude.wide, gas, on = c("Date")), natgas, on = c("Date")), ho, on = c("Date"))

### Correlations

p <- cor(crude.wide[, 2:5], method = c("pearson"))
getCorTable(p)

k <- cor(crude.wide[, 2:5], method = c("kendall"))
getCorTable(k)

s <- cor(crude.wide[, 2:5], method = c("spearman"))
getCorTable(s)

crude.dt <- crude.wide[crude.wide$Date >= "2018-1-1" & crude.wide$Date <= "2018-12-31",]

ggplot(crude.dt, aes(x = Date)) +
  geom_line(aes(y = cl), lwd = .8, col = "cornflowerblue") +
  geom_line(aes(y = mt), lwd = .8, col = "orange", alpha = .7) +
  labs(title = "WTI vs Gulf Sour", y = "Return")

crude.dt <- crude.long[crude.wide$Date >= "2018-6-1" & crude.wide$Date <= "2018-8-31",]

ggplot(crude.dt, aes(x = Date)) +
  geom_line(aes(y = cl), lwd = .8, col = "cornflowerblue") +
  geom_line(aes(y = mt), lwd = .8, col = "orange", alpha = .7) +
  labs(title = "WTI vs Gulf Sour", y = "Return")


ggplot(crude.dt, aes(x = cl, y = mt)) +
  geom_point(col = "black") +
  geom_smooth() +
  labs(title = "WTI vs Gulf Sour", x = "WTI", y = "Gulf Sour")

##### Wide Format
## Dist of Returns

commodity.long <- melt(commodity.wide, id.var = c("Date"),
                   variable.name = "Symbol",
                   value.name = "Return")
commodity.long$Symbol <- toupper(commodity.long$Symbol)

data.symbology <- data.symbology[, 1:2]
commodity.long <- merge(commodity.long, data.symbology, on = c("Symbol"))

commodity.long %>%
  ggplot(aes(x = Return, fill = Description)) +
  #geom_histogram(aes(y = ..density..),alpha = 0.45, binwidth = 0.005) +
  geom_density(aes(y = ..density..), alpha = 0.35) +
  ggtitle("Monthly Returns Since 2014") +
  theme_update(plot.title = element_text(hjust = 0.5))

commodity.long %>%
  ggplot(aes(x = Return, fill = Description)) +
  geom_histogram(aes(y = ..density..),alpha = 0.45, binwidth = 0.005) +
  ggtitle("Monthly Returns Since 2014") +
  theme_update(plot.title = element_text(hjust = 0.5))


getRetVsT(commodites$ve$logReturn, 1:6)

commodity.long %>%
  ggplot(aes(x = Return, y = ..density..)) +
  geom_density( alpha = 1) +
  geom_histogram(aes(fill = Description), alpha = 0.45, binwidth = 0.01) +
  facet_wrap(~Symbol) +
  ggtitle("Monthly Returns Density Since 2013") +
  xlab("monthly returns") +
  ylab("distribution") +
  theme_update(plot.title = element_text(hjust = 0.5))

# rolling sd

window <- 10

# rolling sd, tidyverse
sd_roll_10 <- rollify(sd, window = window)

commodity.rolling.sd <- 
  commodity.long %>%
  as_tbl_time(index = Date) %>%
  mutate(rolling_sd = sd_roll_10(Return)) %>%
  select(-Return) %>%
  na.omit()

commodity.rolling.sd[commodity.rolling.sd$Symbol != "VE",] %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = rolling_sd, color = Description)) +
  facet_wrap(aes(Description)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = pretty_breaks(n = 8)) +
  labs(title = "Rolling Volatility - 10 day", y = "") +
  theme(plot.title = element_text(hjust = 0.5))

#####################
#### TS ACF
####################

ggAcf(ts(commodites$sc$logReturn, frequency = 365)) +
  ggtitle("Series: WTI")

ggAcf(ts(commodites$ng$logReturn, frequency = 365)) +
  ggtitle("Series: Natural Gas")

ggAcf(ts(commodites$ng$logReturn, frequency = 365)) +
  ggtitle("Series: Natural Gas")

############
### TS Correlations
###########

ggAcf(brent) + 
  ggtitle("Series: Brent Crude")

ggAcf(wti) +
  ggtitle("Series: WTI")

ggAcf(commodites$sc$logReturn) +
  ggtitle("Series: WTI")

ggAcf(commodity.wide[, .(sc, cl)]) +
  ggtitle("WTI vs Brent ACF")

ggAcf(commodity.wide[, .(cl, mt)]) +
  ggtitle("WTI vs Gulf Sour ACF")

############
### TS Models
###########

# Brent Crude
cl.train.data <- commodity.long[Symbol == "CL" & Date < "2019-1-1"]
cl.test.data <- commodity.long[Symbol == "CL" & Date >= "2019-1-1"]

ggAcf(cl.train.data$Return)
ggAcf(cl.train.data$Return)$data

cl.train.model <- auto.arima(cl.train.data$Return, ic = "bic")

cl.test.model <- Arima(cl.test.data$Return, model = cl.train.model)

cl.forecasts = fitted(cl.test.model)

plot(forecast(cl.test.model, h = 4))


# WTI

sc.baseline <- as.data.table(commodites[["sc"]])
sc.baseline[, 
            Date := nymex_date][, 
                                nymex_date := NULL]

Box.test(sc.baseline$return, lag = 4, type = "Ljung-Box")

ggAcf(sc.train.data$return) +
  ggtitle("WTI Autocorrelation")

ggAcf(sc.train.data$return)$data

sc.train.data <- sc.baseline[Date < "2019-1-1"]
sc.test.data <- sc.baseline[Date >= "2019-1-1"]

sc.train.model <- auto.arima(sc.train.data$return, ic = "bic")

sc.test.model <- Arima(sc.test.data$return, model = sc.train.model)

sc.test.data$pred <- fitted(sc.test.model)

ggplot(sc.test.data, aes(x = Date)) +
  geom_line(aes(y = return), lwd = .5, col = "black") +
  geom_line(aes(y = pred), lwd = 1.5, col = "cornflowerblue", alpha = .7, linetype = 2) +
  geom_hline(aes(yintercept = .015), col = "green", lwd = .8, alpha = .7) +
  geom_hline(aes(yintercept = -.02), col = "red", lwd = .8, alpha = .7) +
  labs(title = "WTI Actual vs. Pred", y = "Return")

monthly <- sc.test.data[Date >= "2019-1-1" & Date < "2019-2-1"]

ggplot(monthly, aes(x = Date)) +
  geom_line(aes(y = return), lwd = .5, col = "black") +
  geom_line(aes(y = pred), lwd = 1.5, col = "cornflowerblue", alpha = .7, linetype = 2) +
  labs(title = "WTI Actual vs. Pred", y = "Return")

threshold <- 0.015

sc.test.data[, index := .I]

sc.enter <- sc.test.data[pred < -threshold | pred > threshold]
sc.enter$exit <- sc.enter$index + 4
sc.enter$side <- ifelse(as.numeric(sc.enter$pred) >= 0, "buy", "sell")

sc.exit <- sc.test.data[index %in% sc.enter$exit]
sc.exit$exit <- 0
sc.exit$side <- ifelse(sc.enter$side == "buy", "sell", "buy")

sc.transactions <- rbind(sc.enter, sc.exit)[ order(Date)][, .(Date, spotPrice, side)]

sc.transactions

write.csv(sc.transactions, file = "sc.trades.csv")

sc.disp.trans <- merge(sc.test.data, sc.transactions, by = c("Date"), all.x = T)
sc.disp.trans$color <- ifelse(sc.disp.trans$side == "buy", "green", ifelse(sc.disp.trans$side == "sell", "red", NA))

sc.disp.trans$spotPrice.y[is.na(sc.disp.trans$spotPrice.y)] <- NA # sc.disp.trans[is.na(sc.disp.trans$spotPrice.y)]$spotPrice.x

head(sc.disp.trans, 10)

ggplot(sc.test.data[Date < "2019-2-1"]) +
  geom_line(aes(x = Date, y = close), lwd = 1, col = "black") +
  geom_point(data = sc.disp.trans[Date < "2019-2-1"], aes(x = Date, y = spotPrice.y, col = color), lwd = 5) +
  labs(title = "WTI Actual vs. Pred", y = "Return") +
  theme(legend.position = "none")

symbol <- "sc"
start_date <- "2019-1-1"

candlestick(symbol, start_date, commodites)

candlestickPred <- function(data, symbol) {
  
  d <- data
  
  desc <- data.symbology[data.symbology$Symbol == toupper(symbol),]$Description
  
  start_date <- as.Date(min(d$Date))
  end_date <- as.Date(max(d$Date))
  
  p <- d %>% 
    plot_ly(x = ~Date, type = "candlestick",
            open = ~open, close = ~spotPrice.x,
            high = ~high, low = ~low) %>%
    add_paths(x = ~Date, y = ~spotPrice.y, line = list(color = "black", width = 3), inherit = F) %>%
    layout(title = paste(symbol, "activity since", format(start_date, "%b %d %Y")))
  
  v <- d %>% 
    plot_ly(x = ~Date, y = ~volume, type='bar', name = "Volume",
            colors = c('#17BECF','#7F7F7F'))
  
  rs <- list(visible = TRUE, x = 0.5, y = -0.055,
             xanchor = 'center', yref = 'paper',
             font = list(size = 9),
             buttons = list(
               list(count=1,
                    label='RESET',
                    step='all'),
               list(count=1,
                    label='1 YR',
                    step='year',
                    stepmode='backward'),
               list(count=3,
                    label='3 MO',
                    step='month',
                    stepmode='backward'),
               list(count=1,
                    label='1 MO',
                    step='month',
                    stepmode='backward')
             ))
  
  pp <- subplot(p, v, heights = c(0.7,0.2), nrows=2,
                shareX = TRUE, titleY = TRUE) %>%
    layout(title = paste( desc, " : ", format(start_date, "%b %d %Y"), "-", format(end_date, "%b %d %Y")),
           xaxis = list(rangeselector = rs),
           legend = list(orientation = 'h', x = 0.5, y = 1,
                         xanchor = 'center', yref = 'paper',
                         font = list(size = 10),
                         bgcolor = 'transparent'))
  pp
}

candlestickPred(sc.disp.trans, "WTI")
