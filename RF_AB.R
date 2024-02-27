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
index_AB <- index %>%
  subset(RegionCode == "CAN_AB" & Date >= start_date_index & Date <= end_date_index)

print(index_AB)
tail(index_AB)


# CASES DATAFRAME CLEANING

# convert the date-time column to a date column
cases$Date <- as.Date(cases$date)

# specify start & end dates
start_date_cases <- as.Date("2020-03-05")
end_date_cases <- as.Date("2022-12-31")

# split data into selected province & trim date ranges 
cases_AB <- cases %>%
  subset(region == "AB" & Date >= start_date_cases & Date <= end_date_cases)

print(cases_AB)
tail(cases_AB)


# INDEX AND CASES DATAFRAME MERGING 

nrow(index_AB) # 1032
nrow(cases_AB) # 1032 

index_cases_raw_AB <- bind_cols(index_AB, cases_AB)

View(index_cases_raw_AB)

# remove unnecessary columns 
index_cases_AB <- index_cases_raw_AB %>%
  select(-CountryCode, -CountryName, -RegionCode, -CityName, -CityCode, -Jurisdiction, -ConfirmedCases, -ConfirmedDeaths, -MajorityVaccinated, -PopulationVaccinated, -name, -region, -Date...22)

View(index_cases_AB)

# CLEAN UP COLUMN NAMES 

colnames(index_cases_AB) <- c("region_name", "index_date", "stringency_index", "govresp_index", "contamhealth_index", "econ_index", "cases_date", "total_cases", "new_cases")

View(index_cases_AB)


# RANDOM FOREST (ALBERTA)

# split data into training (70%) and test (30%)
index_split <- createDataPartition(y = index_cases_AB$new_cases, p = 0.7, list = FALSE)
train_data_AB <- index_cases_AB[index_split, ]
test_data_AB <- index_cases_AB[-index_split, ]

nrow(train_data_AB) # 724
nrow(test_data_AB) # 308

# run random forest for all predictors 
rf_model_AB <- randomForest(new_cases ~ stringency_index + govresp_index + contamhealth_index + econ_index, data = train_data_AB, 
                            ntree = 100, 
                            importanc=TRUE,
                            nodesize = 7,
                            mtry = 4)
print(rf_model_AB)

plot(rf_model_AB)

# rank predictor importance
importance(rf_model_AB)

varImpPlot(rf_model_AB)

# predictions for test data 
predicted_values_AB <- predict(rf_model_AB, newdata = test_data_AB)

# evaluate model with rmse
rmse(test_data_AB$new_cases, predicted_values_AB)


# cross validation 
actual_values_AB <- test_data_AB$new_cases

residuals <- actual_values_AB - predicted_values_AB

plot(predicted_values_AB, residuals, main = "Residuals vs Predicted Values for AB", xlab = "Predicted Values", ylab = "Residuals") +
  abline(h = 0, col = "red", lty = 2)


# FORECASTING

# plot time series 
ggplot(data = index_cases_AB, mapping = aes(x = index_date, y = new_cases))  +
  geom_line(aes(x = index_date, y = new_cases, color = "Original")) +
  geom_line(data = test_data_AB, aes(x = index_date, y = predicted_values_AB, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Time Series Forecasting New COVID-19 Cases in Alberta with Random Forest", x = "Date", y = "New Cases")

