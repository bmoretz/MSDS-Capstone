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

candlestick("ho")

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

