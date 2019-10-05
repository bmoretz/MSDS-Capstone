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
data.energy <- read_csv( file = "energy_2015year_master.csv") 
data.symbology <- read_csv( file = "global_index_description.csv")

# Global Symbol look-up
trading.symbols <- colnames(data.energy)[
  sapply(colnames(data.energy), FUN = function(symbol) {
  !str_contains(symbol, "_")
})]


# Utility Functions

getDataForSymbol <- function( symbol, data = data.energy ) {
  fwd.points <- c(1, 2, 3, 4, 5, 10, 15, 20, 30, 60, 90, 180, 365)
  
  price.points <- c(symbol, 
    sapply(fwd.points, FUN = function( fp ) { 
      paste0( symbol, "_", fp, "FD")
    })
  )
  
  calendar.days <- c("trade_date",
    sapply(fwd.points, FUN = function( fp ) { 
      paste0( "calendar_", fp)
    })
  )
  
  columns <- c(price.points, calendar.days)
  
  data[, columns]

  columns <- c(price.points, calendar.days)
  
  data.symbol <- data[, columns]
  data.symbol <- data.symbol[order(data.symbol$trade_date),]
  
  
  # calculate returns from prices
  prices <- data.symbol[[symbol]]
  n <- length(prices)
  ret <- prices[-1] / prices[-n] - 1
  
  # store it
  data.symbol$return <- c(0, ret)
  data.symbol$logReturn <- log(1 + data.symbol$return)
  
  colnames(data.symbol[symbol]) <- "spotPrice"
  data.symbol[-1,] # throw away the first record that has no return data.
}

plotReturns <- function( symbol, start_date = "2019-1-1", data ){ 

  data.plot <- data[data$trade_date >= start_date, c("trade_date", "RB", "return", "logReturn")]
  
  p1 <- ggplot(data.plot, aes(trade_date, return)) +
    geom_line()
  
  p2 <- ggplot(data.plot, aes(trade_date, logReturn)) +
    geom_line()
  
  p3 <- ggplot(data.plot, aes(return, fill = ..count..)) +
    geom_histogram()
  
  p4 <- ggplot(data.plot, aes(logReturn, fill = ..count..)) +
    geom_histogram()
  
  grid.arrange(p1, p2, p3, p4, nrow = 2)
}

symbol <- "RB"
data.rb <- getDataForSymbol(symbol)

plotReturns(symbol, data = data.rb)

energy.commodities <- c("CL", "HO", "RB", "SC")

commodites <- lapply(energy.commodities, FUN = function( s ) {
  getDataForSymbol(s)
})

commodites[[1]]
