library(rsconnect)

rsconnect::deployApp('D:/Projects/MSDS-RiskAnalytics/PortfolioAnalytics/apps/portfolio_return_dist.rmd')
rsconnect::deployApp('D:/Projects/MSDS-RiskAnalytics/PortfolioAnalytics/apps/portfolio_volatility.rmd')
rsconnect::deployApp('D:/Projects/MSDS-RiskAnalytics/PortfolioAnalytics/apps/portfolio_skewness.rmd')
rsconnect::deployApp('D:/Projects/MSDS-RiskAnalytics/PortfolioAnalytics/apps/commodity_predict.rmd')

