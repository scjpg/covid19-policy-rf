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
index_BC <- index %>%
  subset(RegionCode == "CAN_BC" & Date >= start_date_index & Date <= end_date_index)

print(index_BC)
tail(index_BC)


# CASES DATAFRAME CLEANING

# convert the date-time column to a date column
cases$Date <- as.Date(cases$date)

# specify start & end dates
start_date_cases <- as.Date("2020-03-05")
end_date_cases <- as.Date("2022-12-31")

# split data into selected province & trim date ranges 
cases_BC <- cases %>%
  subset(region == "BC" & Date >= start_date_cases & Date <= end_date_cases)

print(cases_BC)
tail(cases_BC)


# INDEX AND CASES DATAFRAME MERGING 

nrow(index_BC) # 1032 rows
nrow(cases_BC) # 1032 rows

index_cases_raw_BC <- bind_cols(index_BC, cases_BC)

View(index_cases_raw_BC)

# remove unnecessary columns 
index_cases_BC <- index_cases_raw_BC %>%
  select(-CountryCode, -CountryName, -RegionCode, -CityName, -CityCode, -Jurisdiction, -ConfirmedCases, -ConfirmedDeaths, -MajorityVaccinated, -PopulationVaccinated, -name, -region, -Date...22)

View(index_cases_BC)

# CLEAN UP COLUMN NAMES 

colnames(index_cases_BC) <- c("region_name", "index_date", "stringency_index", "govresp_index", "contamhealth_index", "econ_index", "cases_date", "total_cases", "new_cases")

View(index_cases_BC)


# RANDOM FOREST (ONTARIO)

# split data into training (70%) and test (30%)
index_split <- createDataPartition(y = index_cases_BC$new_cases, p = 0.7, list = FALSE)
train_data_BC <- index_cases_BC[index_split, ]
test_data_BC <- index_cases_BC[-index_split, ]

nrow(train_data_BC) # 724
nrow(test_data_BC) # 308

# run random forest for all predictors 
rf_model_BC <- randomForest(new_cases ~ stringency_index + govresp_index + contamhealth_index + econ_index, data = train_data_BC, 
                            ntree = 100, 
                            importanc=TRUE,
                            nodesize = 7,
                            mtry = 4)
print(rf_model_BC)

plot(rf_model_BC)

# rank predictor importance
importance(rf_model_BC)

varImpPlot(rf_model_BC)

# predictions for test data
predicted_values_BC <- predict(rf_model_BC, newdata = test_data_BC)

# evaluate model with rmse
rmse(test_data_BC$new_cases, predicted_values_BC)


# cross validation 
actual_values_BC <- test_data_BC$new_cases

residuals <- actual_values_BC - predicted_values_BC

plot(predicted_values_BC, residuals, main = "Residuals vs Predicted Values for BC", xlab = "Predicted Values", ylab = "Residuals") +
  abline(h = 0, col = "red", lty = 2)

# plot test data predictions
ggplot(data = index_cases_BC, mapping = aes(x = index_date, y = new_cases))  +
  geom_line(aes(x = index_date, y = new_cases, color = "Original")) +
  geom_line(data = test_data_BC, aes(x = index_date, y = predicted_values_BC, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Test Data Predictions for COVID-19 Cases in British Columbia with Random Forest", x = "Date", y = "New Cases")

