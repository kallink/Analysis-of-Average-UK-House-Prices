#' ---
#' title: Coursework 1 scratchpad
#' author: Kallinken Skantharuban
#' date: Spring term 2026
#' ---

# 1. Setup -------------------------------------------
library(zoo)
library(prophet)

# 2. Dataset ------------------------------
ukHousePrices <- read.csv("data/ukHousePrices.csv")
head(ukHousePrices)
tail(ukHousePrices)


# 3. Convert to Prophet Format -------------------------------

ukHousePrices_dataframe <- data.frame(
    ds = as.Date(ukHousePrices$Date,
                 format = "%d/%m/%Y"),
    y=as.numeric(ukHousePrices$Price))
head(ukHousePrices_dataframe)
tail(ukHousePrices_dataframe)
summary(ukHousePrices_dataframe)



# 4. Plot Data ---------------------------------------------------

plot(ukHousePrices_dataframe$ds, ukHousePrices_dataframe$y,
     type = "l",
     col = "blue",
     main = "UK Average House Prices 1990-2025",
     xlab = "Year",
     ylab = "Average Price (£)")

ukHousePrices_ts <- ts(ukHousePrices_dataframe$y,
                       start = c(1990, 1),
                       frequency = 12)
ukHousePrices_decomp <- decompose(ukHousePrices_ts)
plot(ukHousePrices_decomp)

# 5. Linear Regression Model ----------------------------------------

ukHousePrices_lm <- lm(y ~ as.numeric(ds),
                       data = ukHousePrices_dataframe)
summary(ukHousePrices_lm)

plot(ukHousePrices_dataframe$ds, ukHousePrices_dataframe$y,
     type = "l",
     col  = "blue",
     main = "UK House Prices with Trend Line 1990-2025",
     xlab = "Year",
     ylab = "Average Price (£)")
abline(ukHousePrices_lm, col = "red", lwd = 2)

legend("topleft",
       legend = c("Actual", "Trend"),
       col    = c("blue", "red"),
       lty    = 1)
# 6. Prophet Model -------------------------------------------------
ukHousePrices_model <- prophet(ukHousePrices_dataframe)

ukHousePrices_future1 <- make_future_dataframe( ukHousePrices_model,
                                                periods = 120,
                                                freq = "month")

ukHousePrices_future2 <- make_future_dataframe( ukHousePrices_model,
                                                periods = 240,
                                                freq = "month")

ukHousePrices_forecast1 <- predict(ukHousePrices_model, ukHousePrices_future1)

ukHousePrices_forecast2 <- predict(ukHousePrices_model, ukHousePrices_future2)

# 7. Plot Results -------------------------------------------------------

plot(ukHousePrices_model, ukHousePrices_forecast1,
     xlab = "Year",
     ylab = "Average Price (£)")

plot(ukHousePrices_model, ukHousePrices_forecast2,
     xlab = "Year",
     ylab = "Average Price (£)")

prophet_plot_components(ukHousePrices_model, ukHousePrices_forecast1)
prophet_plot_components(ukHousePrices_model, ukHousePrices_forecast2)