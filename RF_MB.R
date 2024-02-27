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
start_date_index <- as.Date("2020-03-28")
end_date_index <- as.Date("2022-04-09")

# split data into selected province and trim date ranges 
index_MB <- index %>%
  subset(RegionCode == "CAN_MB" & Date >= start_date_index & Date <= end_date_index)

print(index_MB)
tail(index_MB)


# CASES DATAFRAME CLEANING

# convert the date-time column to a date column
cases$Date <- as.Date(cases$date)

# specify start & end dates
start_date_cases <- as.Date("2020-03-14")
end_date_cases <- as.Date("2022-03-26")

# split data into selected province & trim date ranges 
cases_MB <- cases %>%
  subset(region == "MB" & Date >= start_date_cases & Date <= end_date_cases)

print(cases_MB)
tail(cases_MB)


# INDEX AND CASES DATAFRAME MERGING 

nrow(index_MB) # 743 
nrow(cases_MB) # 743 

index_cases_raw_MB <- bind_cols(index_MB, cases_MB)

View(index_cases_raw_MB)

# remove unnecessary columns 
index_cases_MB <- index_cases_raw_MB %>%
  select(-CountryCode, -CountryName, -RegionCode, -CityName, -CityCode, -Jurisdiction, -ConfirmedCases, -ConfirmedDeaths, -MajorityVaccinated, -PopulationVaccinated, -name, -region, -Date...22)

View(index_cases_MB)

# CLEAN UP COLUMN NAMES 

colnames(index_cases_MB) <- c("region_name", "index_date", "stringency_index", "govresp_index", "contamhealth_index", "econ_index", "cases_date", "total_cases", "new_cases")

View(index_cases_MB)


# RANDOM FOREST (ONTARIO)

# split data into training (70%) and test (30%)
index_split <- createDataPartition(y = index_cases_MB$new_cases, p = 0.7, list = FALSE)
train_data_MB <- index_cases_MB[index_split, ]
test_data_MB <- index_cases_MB[-index_split, ]

nrow(train_data_MB) # 523
nrow(test_data_MB) # 220

# run random forest for all predictors 
rf_model_MB <- randomForest(new_cases ~ stringency_index + govresp_index + contamhealth_index + econ_index, data = train_data_MB, 
                            ntree = 100, 
                            importanc=TRUE,
                            nodesize = 7,
                            mtry = 4)
print(rf_model_MB)

plot(rf_model_MB)

# rank predictor importance
importance(rf_model_MB)

varImpPlot(rf_model_MB)

# predictions for test data
predicted_values_MB <- predict(rf_model_MB, newdata = test_data_MB)

# evaluate model with rmse
rmse(test_data_MB$new_cases, predicted_values_MB)


# cross validation 
actual_values_MB <- test_data_MB$new_cases

residuals <- actual_values_MB - predicted_values_MB

plot(predicted_values_MB, residuals, main = "Residuals vs Predicted Values for MB", xlab = "Predicted Values", ylab = "Residuals") +
  abline(h = 0, col = "red", lty = 2)

# plot test data predictions
ggplot(data = index_cases_MB, mapping = aes(x = index_date, y = new_cases))  +
  geom_line(aes(x = index_date, y = new_cases, color = "Original")) +
  geom_line(data = test_data_MB, aes(x = index_date, y = predicted_values_MB, color = "Forecast")) +
  scale_color_manual(values = c("Original" = "blue", "Forecast" = "red")) +
  labs(title = "Test Data Predictions for COVID-19 Cases in Manitoba with Random Forest", x = "Date", y = "New Cases")

