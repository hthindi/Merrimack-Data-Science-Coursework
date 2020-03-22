library(dplyr)

cat("\014")
rm(list=ls())


weather = read.csv("C:/Users/Hasan Hindi/Documents/merrimack/courses/MACHINE LEARNING/project data/weather.csv")
weather$Precipitation_logic = ifelse(weather$OBSERVED_DAILY_WATER_EQUIVALENT_inches>0,1,0)

#Data Splicing to check how many precipitation days there are for each station
w1 = weather %>% group_by(Station) %>% summarise(num_precip = sum(Precipitation_logic == 1))
w2 = weather %>% group_by(Station) %>% tally()
w3 = inner_join(w1,w2,by = "Station")
w3$fraction_of_precip = round(w3$num_precip/w3$n,2)
w3$Avg_Days_Rain = round(w3$fraction_of_precip*365,0)

#Feature engineering to change sunrise and sunset time to fraction of day
weather$Month  <- months(as.Date(weather$Date,format = "%m/%d/%Y"))
weather$SUNRISE_local_AM_hour <- as.numeric(substr(weather$SUNRISE_local_AM,1,1))*60
weather$SUNRISE_local_AM_minute = as.numeric(substr(weather$SUNRISE_local_AM,3,4))
weather$SUNRISE_local_AM_fraction_of_day = (weather$SUNRISE_local_AM_hour+weather$SUNRISE_local_AM_minute)/(24*60)
weather$SUNSET_local_PM_hour = as.numeric(substr(weather$SUNSET_local_PM,1,2))*60
weather$SUNSET_local_PM_minute = as.numeric(substr(weather$SUNSET_local_PM,4,5))
weather$SUNSET_local_PM_hour_fraction_of_day = (weather$SUNSET_local_PM_hour+weather$SUNSET_local_PM_minute)/(24*60)

# Creates a dataframe to filter out any features with missing values
# less than 1000 observations but more than zero. 

cols = as.array(colnames(weather))
stored = c()
for (i in 1:length(cols)){
  stored = c(stored,sum(is.na(weather[,cols[i]])))
}
cols_with_missing_to_fill = data.frame(colnames = cols,missing = stored)
cols_with_missing_to_fill = cols_with_missing_to_fill %>% filter(missing < 1000 & missing > 0 )

## Fills NA values for those missing values with mean of those features by each station.
for (i in as.array(cols_with_missing_to_fill$colnames)){
  for (j in which(is.na(weather[,i]))){
    station.name = weather[j,"Station"]
    row_num_station = which(weather$Station == station.name)
    mean_of_station_for_i = mean(weather[row_num_station,i],na.rm = TRUE)
    weather[j,i] <- mean_of_station_for_i
  }
}

#Removing any features with too many NA values or if those features have homogeneous data (like all zeroes)
weather$MINIMUM_SOLAR_RADIANCE_W_M_2 = NULL
weather$MINIMUM_SOLAR_RADIANCE_FT_CANDLES = NULL
weather$TOTAL_MINUTES_of_SUNSHINE_if_observed = NULL
weather$SUNRISE_local_AM = NULL
weather$SUNSET_local_PM = NULL
weather$FORECASTED_DFN_TOTAL_PRECIP_inches = NULL
weather$SUNRISE_local_AM_hour = NULL
weather$SUNRISE_local_AM_minute = NULL
weather$SUNSET_local_PM_hour = NULL
weather$SUNSET_local_PM_minute = NULL
weather$CALENDAR_DAY_TOTAL_HOURS_of_PRECIP = NULL
weather$HIGH_TEMP_30_YR_5th_PERCENTILE = NULL
weather$HIGH_TEMP_30_YR_30th_PERCENTILE = NULL
weather$HIGH_TEMP_30_YR_50th_PERCENTILE = NULL
weather$HIGH_TEMP_30_YR_70th_PERCENTILE = NULL
weather$HIGH_TEMP_30_YR_95th_PERCENTILE = NULL
weather$LOW_TEMP_30_YR_5th_PERCENTILE = NULL
weather$LOW_TEMP_30_YR_30th_PERCENTILE = NULL
weather$LOW_TEMP_30_YR_50th_PERCENTILE = NULL
weather$LOW_TEMP_30_YR_70th_PERCENTILE = NULL
weather$LOW_TEMP_30_YR_95th_PERCENTILE = NULL


weather$Month = as.factor(weather$Month)
weather$Precipitation_logic = ifelse(weather$Precipitation_logic == 1,"Precipitation","No Precipitation")
weather$Precipitation_Today = weather$Precipitation_logic
weather$Precipitation_logic = NULL


funGetStationWeatherPastAndPresent <- function(station_list){
  weather2 = weather[0,]
  for (i in station_list){
    df <- weather %>% filter(weather$Station == i)
    df$PrecipitationYesterday <- lag(df$Precipitation_Today,1)
    df$Precipitation2DaysAgo <- lag(df$Precipitation_Today,2)
    df$PrecipitationTomorrow <- lead(df$Precipitation_Today,1)
    weather2 <- rbind(weather2,df)
  }
  assign("weather2",weather2,.GlobalEnv) 
}

funGetStationWeatherPastAndPresent(levels(weather$Station))

weather2$Precipitation2DaysAgo <- as.factor(weather2$Precipitation2DaysAgo)
weather2$PrecipitationYesterday <- as.factor(weather2$PrecipitationYesterday)
weather2$PrecipitationTomorrow <- as.factor(weather2$PrecipitationTomorrow)
weather2$Precipitation_Today <- as.factor(weather2$Precipitation_Today)

weather2$year = as.factor(format(as.Date(weather2$Date, format="%m/%d/%Y"),"%Y"))
weather2$Date = NULL
weather2 = na.omit(weather2)
weather2$OBSERVED_DAILY_WATER_EQUIVALENT_inches = NULL
weather2$PREDOMINATE_WEATHER_TEXT_all_hours = NULL
weather2$SUNRISE_local_AM_fraction_of_day = NULL
weather2$SUNSET_local_PM_hour_fraction_of_day = NULL


library(caret)

set.seed(123)
train.years = sample(levels(weather2$year),2)
train <- weather2[which(weather2$year %in% train.years),]
test <- weather2[-which(weather2$year %in% train.years),]
train$year = NULL
test$year = NULL

levels(train$PrecipitationTomorrow)[levels(train$PrecipitationTomorrow)=="Precipitation"] <- "Positive"
levels(train$PrecipitationTomorrow)[levels(train$PrecipitationTomorrow)=="No Precipitation"] <- "Negative"

recommended_mtry <- floor(sqrt(ncol(train[,-which(colnames(train) == "PrecipitationTomorrow")])))
rfGrid <- expand.grid(mtry = c(recommended_mtry-2, recommended_mtry, recommended_mtry+2))
rfControl <- trainControl(method = "oob",classProbs = TRUE)

freq_enc <- function(variable, level) { nrow(train[train[, variable]==level, ]) }

station_freq <- mapply(freq_enc, variable = "Station", level = levels(weather2$Station), USE.NAMES = FALSE) 
names(station_freq) <- levels(weather2$Station)

Month_freq <- mapply(freq_enc, variable = "Month", level = levels(weather2$Month), USE.NAMES = FALSE) 
names(Month_freq) <- levels(weather2$Month)

Precipitation_Today_freq <- mapply(freq_enc, variable = "Precipitation_Today", level = levels(weather2$Precipitation_Today), USE.NAMES = FALSE) 
names(Precipitation_Today_freq) <- levels(weather2$Precipitation_Today)

Precipitation_Yesterday_freq <- mapply(freq_enc, variable = "PrecipitationYesterday", level = levels(weather2$PrecipitationYesterday), USE.NAMES = FALSE) 
names(Precipitation_Yesterday_freq) <- levels(weather2$PrecipitationYesterday)

Precipitation2DaysAgo_freq <- mapply(freq_enc, variable = "Precipitation2DaysAgo", level = levels(weather2$Precipitation2DaysAgo), USE.NAMES = FALSE) 
names(Precipitation2DaysAgo_freq) <- levels(weather2$Precipitation2DaysAgo)

train$station_freq <- 0
for (level in levels(train$Station)){
  train[train[, "Station"]==level, "station_freq"] <- station_freq[level] 
}

train$Month_freq <- 0
for (level in levels(train$Month)){
  train[train[, "Month"]==level, "Month_freq"] <- Month_freq[level] 
}

train$Precipitation_Today_freq <- 0
for (level in levels(train$Precipitation_Today)) {
  train[train[, "Precipitation_Today"]==level, "Precipitation_Today_freq"] <- Precipitation_Today_freq[level] 
}

train$Precipitation_Yesterday_freq <- 0
for (level in levels(train$PrecipitationYesterday)) {
  train[train[, "PrecipitationYesterday"]==level, "Precipitation_Yesterday_freq"] <- Precipitation_Yesterday_freq[level] 
}

train$Precipitation2DaysAgo_freq <- 0
for (level in levels(train$Precipitation2DaysAgo)) {
  train[train[, "Precipitation2DaysAgo"]==level, "Precipitation2DaysAgo_freq"] <- Precipitation2DaysAgo_freq[level] 
}

#test

test$station_freq <- 0
for (level in levels(train$Station)){
  test[test[, "Station"]==level, "station_freq"] <- station_freq[level] 
}

test$Month_freq <- 0
for (level in levels(train$Month)){
  test[test[, "Month"]==level, "Month_freq"] <- Month_freq[level] 
}

test$Precipitation_Today_freq <- 0
for (level in levels(train$Precipitation_Today)) {
  test[test[, "Precipitation_Today"]==level, "Precipitation_Today_freq"] <- Precipitation_Today_freq[level] 
}

test$Precipitation_Yesterday_freq <- 0
for (level in levels(train$PrecipitationYesterday)) {
  test[test[, "PrecipitationYesterday"]==level, "Precipitation_Yesterday_freq"] <- Precipitation_Yesterday_freq[level] 
}

test$Precipitation2DaysAgo_freq <- 0
for (level in levels(train$Precipitation2DaysAgo)) {
  test[test[, "Precipitation2DaysAgo"]==level, "Precipitation2DaysAgo_freq"] <- Precipitation2DaysAgo_freq[level] 
}


test[, 2:93] <- scale(test[, 2:93], center = apply(train[, 2:93], 2, mean), scale = apply(train[, 2:93], 2, sd)) 

train[, 2:93] <- scale(train[, 2:93])

test[, 99:103] <- scale(test[, 99:103], center = apply(train[, 99:103], 2, mean), scale = apply(train[, 99:103], 2, sd)) 
train[, 99:103] <- scale(train[, 99:103])


rf_model <- train(x = train[,c(2:93,99:103)],
                  y = train[,98],
                  method = "rf",
                  tuneGrid = rfGrid,
                  trControl = rfControl,
                  importance = TRUE,
                  trace = FALSE,ntrees = 500)




levels(test$PrecipitationTomorrow)[levels(test$PrecipitationTomorrow)=="Precipitation"] <- "Positive"
levels(test$PrecipitationTomorrow)[levels(test$PrecipitationTomorrow)=="No Precipitation"] <- "Negative"


rf_pred <- predict(rf_model,newdata = test[,c(2:93,99:103)])

rf_probs = predict(rf_model,newdata = test[,c(2:93,99:103)],
                   type = "prob")


confusionMatrix(rf_pred,test$PrecipitationTomorrow,positive = "Positive")



library(classifierplots)


library(ROCR)
library(pROC)

par(pty = "s")
roc(test$PrecipitationTomorrow,rf_probs[,"Positive"],plot = TRUE,legacy.axes = TRUE,
    lwd = 4,print.auc = TRUE,print.auc.x = 0.4,print.auc.y = 0.2, main = "Random Forest 2.0")


rf_varImp <- varImp(rf_model, type = 2)
plot(rf_varImp,top = 30)


nnGrid <- expand.grid(size = 8:10, decay = 0.2)
nnControl <- trainControl(method = "repeatedcv", repeats = 5, classProbs = TRUE)


nn_model <- train(x = train[,c(2:93,99:103)],
                  y = train[,98],
                  method = "nnet",
                  tuneGrid = nnGrid,
                  trControl = nnControl,
                  trace = FALSE)



nn_pred <- predict(nn_model,newdata = test[,c(2:93,99:103)])
nn_probs = predict(nn_model,newdata = test[,c(2:93,99:103)],
                   type = "prob")


confusionMatrix(nn_pred,test$PrecipitationTomorrow,positive = "Positive")

par(pty = "s")
roc(test$PrecipitationTomorrow,nn_probs[,"Positive"],plot = TRUE,legacy.axes = TRUE,
    lwd = 4,print.auc = TRUE,print.auc.x = 0.4,print.auc.y = 0.2, main = "Neural Network")


# onehot_encoder <- dummyVars(~ Station + Month + Precipitation_Today + PrecipitationYesterday + Precipitation2DaysAgo,
#                             weather2[, c("Station", "Month", "Precipitation_Today", "PrecipitationYesterday", "Precipitation2DaysAgo" )], levelsOnly = FALSE)
# 
# onehot_enc_train <- predict(onehot_encoder, train[, c("Station", "Month", "Precipitation_Today", "PrecipitationYesterday", "Precipitation2DaysAgo")]) 
# train <- cbind(train, onehot_enc_train)
# 
# onehot_enc_test <- predict(onehot_encoder, test[, c("Station", "Month", "Precipitation_Today", "PrecipitationYesterday", "Precipitation2DaysAgo")]) 
# test <- cbind(test, onehot_enc_test)
# 
# 
# rf_model_onehot <- train(x = train[,c(2:93,104:139)],
#                   y = train[,98],
#                   method = "rf",
#                   tuneGrid = rfGrid,
#                   trControl = rfControl,
#                   importance = TRUE,
#                   trace = FALSE,ntrees = 500)
# 
# rf_pred_onehot <- predict(rf_model_onehot,newdata = test[,c(2:93,104:139)])
# rf_probs_onehot <- predict(rf_model_onehot,newdata = test[,c(2:93,104:139)],
#                            type = "prob")
# 
# 
# confusionMatrix(rf_pred_onehot,test$PrecipitationTomorrow,positive = "Positive")
# 
# par(pty = "s")
# roc(test$PrecipitationTomorrow,rf_probs_onehot[,"Positive"],plot = TRUE,legacy.axes = TRUE,
#     lwd = 4,print.auc = TRUE,print.auc.x = 0.4,print.auc.y = 0.2)
# 
# 
# nn_model_onehot <- train(x = train[,c(2:93,104:139)],
#                   y = train[,98],
#                   method = "nnet",
#                   tuneGrid = nnGrid,
#                   trControl = nnControl,
#                   trace = FALSE)
# 
# 
# 
# nn_pred_onehot <- predict(nn_model_onehot,newdata = test[,c(2:93,104:139)])
# nn_probs_onehot = predict(nn_model_onehot,newdata = test[,c(2:93,104:139)],
#                    type = "prob")
# 
# 
# confusionMatrix(nn_pred_onehot,test$PrecipitationTomorrow,positive = "Positive")
# 
# par(pty = "s")
# roc(test$PrecipitationTomorrow,nn_probs_onehot[,"Positive"],plot = TRUE,legacy.axes = TRUE,
#     lwd = 4,print.auc = TRUE,print.auc.x = 0.4,print.auc.y = 0.2)