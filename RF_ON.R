# load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(randomForest)
library(caret)
library(Metrics)

# import indices 
index <- read_excel("~/school/JYC lab/machine learning/OxCGRT_CAN_Predictors.xlsx", 2)

# import case outcomes
cases <- read_excel("~/school/JYC lab/machine learning/COVID19 Timeline Canada.xlsx", 1)


# INDEX DATAFRAME CLEANING

# reformat dates 
index$Date <- as.Date(as.character(index$Date), format = "%Y%m%d")
print(index)

# specify start & end dates
start_date_index <- as.Date("2020-02-20")
end_date_index <- as.Date("2022-12-17")

# split data into selected province and trim date ranges 
index_ON <- index %>%
  subset(RegionCode == "CAN_ON" & Date >= start_date_index & Date <= end_date_index)

print(index_ON)
tail(index_ON)


# CASES DATAFRAME CLEANING

# convert the date-time column to a date column
cases$Date <- as.Date(cases$date)

# specify start & end dates
start_date_cases <- as.Date("2020-03-05")
end_date_cases <- as.Date("2022-12-31")

# split data into selected province & trim date ranges 
cases_ON <- cases %>%
  subset(region == "ON" & Date >= start_date_cases & Date <= end_date_cases)

print(cases_ON)
tail(cases_ON)


# INDEX AND CASES DATAFRAME MERGING 

nrow(index_ON) # 1032 rows
nrow(cases_ON) # 1032 rows

index_cases_raw_ON <- bind_cols(index_ON, cases_ON)

View(index_cases_raw_ON)

# remove unnecessary columns 
index_cases_ON <- index_cases_raw_ON %>%
  select(-CountryCode, -CountryName, -RegionCode, -CityName, -CityCode, -Jurisdiction, -ConfirmedCases, -ConfirmedDeaths, -MajorityVaccinated, -PopulationVaccinated, -name, -region, -Date...22)

View(index_cases_ON)

# CLEAN UP COLUMN NAMES 

colnames(index_cases_ON) <- c("region_name", "index_date", "stringency_index", "govresp_index", "contamhealth_index", "econ_index", "cases_date", "total_cases", "new_cases")

View(index_cases_ON)


# RANDOM FOREST (ONTARIO)

# split data into training (70%) and test (30%)
index_split <- createDataPartition(y = index_cases_ON$new_cases, p = 0.7, list = FALSE)
train_data_ON <- index_cases_ON[index_split, ]
test_data_ON <- index_cases_ON[-index_split, ]

nrow(train_data_ON) # 724
nrow(test_data_ON) # 308

# run random forest for all predictors 
rf_model_ON <- randomForest(new_cases ~ stringency_index + govresp_index + contamhealth_index + econ_index, data = train_data_ON, 
                            ntree = 100, 
                            importanc=TRUE,
                            nodesize = 7,
                            mtry = 4)
print(rf_model_ON)

plot(rf_model_ON)

# rank predictor importance
importance(rf_model_ON)

varImpPlot(rf_model_ON)

# predictions for test data
predicted_values_ON <- predict(rf_model_ON, newdata = test_data_ON)

# evaluate model with rmse
rmse(test_data_ON$new_cases, predicted_values_ON)


# cross validation 
actual_values_ON <- test_data_ON$new_cases

residuals <- actual_values_ON - predicted_values_ON

plot(predicted_values_ON, residuals, main = "Residuals vs Predicted Values for ON", xlab = "Predicted Values", ylab = "Residuals")
abline(h = 0, col = "red", lty = 2)


# FORECASTING

# plot time series 
ggplot(data = index_cases_ON, mapping = aes(x = index_date, y = new_cases))  +
  geom_line(aes(x = index_date, y = new_cases, color = "Original")) +
  geom_line(data = test_data_ON, aes(x = index_date, y = predicted_values_ON, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Time Series Forecasting New COVID-19 Cases in Ontario with Random Forest", x = "Date", y = "New Cases")
