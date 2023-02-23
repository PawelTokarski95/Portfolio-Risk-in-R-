library(PortfolioAnalytics)
library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)


# Define function to calculate volatility of a stock
Volatility_stock_market <- function(x) {
rR = diff(x)
return(abs(mean(rR, na.rm=TRUE))/sd(rR, na.rm=TRUE))
}

# Define ETF and stock tickers
stock_ETF <- c('XWD.TO', 'EEM')


# Get data for ETFs
for(i in 1:length(stock_ETF)) {
getSymbols(stock_ETF[[i]], src='yahoo', from=Sys.Date()-365*3, to=Sys.Date())
}



# Calculate volatility for ETFs
Volatility_stock_market(XWD.TO$XWD.TO.Close)
Volatility_stock_market(EEM$EEM.Close)

#It seems that the Developed countries stocks are more profitable ones, so I get tickers for US.
stock_tickers <- c("META", "MSFT", "GOOGL", "WMT", "AMZN", "TSLA", "XOM", "KO", "ORCL", "DIS", "NKE", "ADBE", "MA")

# Get data for stocks
Stocks <- data.frame()
for(i in 1:length(stock_tickers)) {
Stocks[[i]] <- getSymbols.yahoo(stock_tickers[[i]], env=globalenv(), from=Sys.Date()-365*3, to=Sys.Date(), periodicity='weekly')[,4]
}
colnames(Stocks) <- stock_tickers

# Calculate volatility for stocks
volatilities <- sapply(Stocks, Volatility_stock_market)


# Calculate return on investment (ROI)
rR <- na.omit(ROC(to.weekly(as.xts(Stocks))))

# Create portfolio with stock names
portf <- portfolio.spec(colnames(rR))


# Add constraints to portfolio
portf <- add.constraint(portf, type = 'weight_sum', min_sum=1, max_sum=1)
portf <- add.constraint(portf, type = 'box', min=0.0, max=0.2)

# Add objectives to portfolio
portf <- add.objective(portf, type='return', name = 'mean')
portf <- add.objective(portf, type='risk', name = 'StdDev')

# Optimize portfolio using ROI method
optim <- optimize.portfolio(rR, portf, optimize_method='ROI', trace=TRUE)

# Example portfolio amount
Portfolio_value_to_invest <- 100000


# Define the optimal weights for the specific portfolio
Invested_return <- optim$weights%*%t(as.matrix(rR))

# Check if the data is properly distributed, so there would not be a problem with normal distribution plot
median(Invested_return)
mean(Invested_return)
max(Invested_return)
min(Invested_return)

# Boxplot to visualize the previous descriptive statistics
ggplot() + geom_boxplot(mapping = aes(y=Invested_return))


# Create profit for portfolio
Financial_result <- Invested_return*Portfolio_value_to_invest

# Calculate final VaR
VaR <- qnorm(0.05, mean(Financial_result), sd(Financial_result))

# GGplot -> VaR + additional histogram to show the data frequency accross distribution
ggplot(mapping = aes(x = Financial_result)) + ggtitle("Value at Risk") + geom_histogram(aes(y =..density..), binwidth = 1000, colour='black', fill='lightgray', lwd=0.85)+
  geom_density(color='darkblue', lwd=1.35) +
  geom_vline(xintercept = VaR, color = "red", linetype = "dashed", lwd=1.4)

# Expected Shortfall calculation
Expected_Shortfall <- mean(Financial_result[Financial_result < VaR])
