# load library
#install.packages('depmixS4')
#install.packages('quantmod')
library('depmixS4')
library('quantmod')
library(ggplot2)

# load data
setwd("C:/Users/lenovo/Desktop/Personal/down/11_08_12_13b_r隐马_T17307")
data_without_anomalies <- na.omit(read.table('Dataset_GroupAssignment3.txt',sep=',',header=1))
data_with_anomalies <- na.omit(read.table('DatasetWithAnomalies_GroupAssignment3.txt',sep=',',header=1))

data_without_anomalies$datetime <- strptime(paste(data_without_anomalies[, 1], data_without_anomalies[, 2]), format = "%d/%m/%Y %H:%M:%S")
data_with_anomalies$datetime <- strptime(paste(data_with_anomalies[, 1], data_with_anomalies[, 2]), format = "%d/%m/%Y %H:%M:%S")

# question 1: Data Exploration
## select time windows
data_without_anomalies_weekday <- data_without_anomalies[data_without_anomalies$datetime$year == 108 &
                                                         data_without_anomalies$datetime$wday == 4 & 
                                                         data_without_anomalies$datetime$hour >= 6 & 
                                                         data_without_anomalies$datetime$hour <= 9, ]
data_without_anomalies_weekend <- data_without_anomalies[data_without_anomalies$datetime$year == 108 &
                                                           data_without_anomalies$datetime$wday == 6 & 
                                                           data_without_anomalies$datetime$hour >= 6 & 
                                                           data_without_anomalies$datetime$hour <= 9, ]
## Visualization
ggplot(data=data_without_anomalies_weekday, 
       mapping=aes(x=Time,
                   y=Global_active_power))
ggplot() + 
  layer(data=data_without_anomalies_weekday, mapping=aes(x=Time, y=Global_active_power), stat="identity", geom = "point", position = "identity") + 
  xlab("Time") + 
  ylab("Average Global Intensity") + 
  labs(colour = "Time Frame") + 
  ggtitle("Weekay Scatter")

ggplot() + 
  layer(data=data_without_anomalies_weekend, mapping=aes(x=Time, y=Global_active_power), stat="identity", geom = "point", position = "identity") + 
  xlab("Time") + 
  ylab("Average Global Intensity") + 
  labs(colour = "Time Frame") + 
  ggtitle("Weekend Scatters")


# question 2: train model
## split train dataset and test dataset
set.seed(1)
data_without_anomalies_weekday$gp <- runif(dim(data_without_anomalies_weekday)[1])
weekday_train <- subset(data_without_anomalies_weekday,data_without_anomalies_weekday$gp <= 0.8)
weekday_test <- subset(data_without_anomalies_weekday,data_without_anomalies_weekday$gp > 0.8)

data_without_anomalies_weekend$gp <- runif(dim(data_without_anomalies_weekend)[1])
weekend_train <- subset(data_without_anomalies_weekend,data_without_anomalies_weekend$gp <= 0.8)
weekend_test <- subset(data_without_anomalies_weekend,data_without_anomalies_weekend$gp > 0.8)

## fit the best model
### weekday dataset
Nstates <- 2:16
BIC <- vector("list",15)
for (n in Nstates) {
  mod_mor <- depmix( offset = NULL,
                     response = weekday_train$Global_active_power~weekday_train$Sub_metering_1+weekday_train$Sub_metering_2+weekday_train$Sub_metering_3-1, 
                     data = weekday_train,
                     nstates = n)
  fm <- fit(mod_mor)
  print(fm)
  BIC[[n-1]] <- BIC(fm)
}
plot(Nstates, BIC, ty="b")
### weekend dataset
Nstates <- 2:16
BIC <- vector("list",15)
for (n in Nstates) {
  mod_mor <- depmix( offset = NULL,
                     response = weekend_train$Global_active_power~weekend_train$Sub_metering_1+weekend_train$Sub_metering_2+weekend_train$Sub_metering_3-1, 
                     data = weekend_train,
                     nstates = n)
  fm <- fit(mod_mor)
  print(fm)
  BIC[[n-1]] <- BIC(fm)
}# weekday dataset
mod_weekday_train <- depmix( offset = NULL,
                   response = weekday_train$Global_active_power~weekday_train$Sub_metering_1+weekday_train$Sub_metering_2+weekday_train$Sub_metering_3-1, 
                   data = weekday_train,
                   nstates = 10,
                   ntimes = 20)
fm <- fit(mod_weekday_train)

mod_weekday_test <- depmix( offset = NULL,
                        response = weekday_test$Global_active_power~weekday_test$Sub_metering_1+weekday_test$Sub_metering_2+weekday_test$Sub_metering_3-1, 
                        data = weekday_test,
                        nstates = 10,
                        ntimes = 20)
fm_new <- setpars(mod_weekday_test,getpars(fm))
summary(fm_new)
BIC(fm_new)
logLik(fm_new)
plot(Nstates, BIC, ty="b")
BIC

# question 3: mdoel test
#
## weekend dataset 
mod_weekend_train <- depmix( offset = NULL,
                             response = weekend_train$Global_active_power~weekend_train$Sub_metering_1+weekend_train$Sub_metering_2+weekend_train$Sub_metering_3-1, 
                             data = weekend_train,
                             nstates = 10,
                             ntimes = 20)
fm <- fit(mod_weekend_train)

mod_weekend_test <- depmix( offset = NULL,
                            response = weekend_test$Global_active_power~weekend_test$Sub_metering_1+weekend_test$Sub_metering_2+weekend_test$Sub_metering_3-1, 
                            data = weekend_test,
                            nstates = 10,
                            ntimes = 20)
fm_new <- setpars(mod_weekend_test,getpars(fm))
summary(fm_new)
BIC(fm_new)
logLik(fm_new)

## Anomaly Detection
data_with_anomalies_weekday <- data_with_anomalies[data_with_anomalies$datetime$year == 109 &
                                                           data_with_anomalies$datetime$wday == 4 & 
                                                           data_with_anomalies$datetime$hour >= 6 & 
                                                           data_with_anomalies$datetime$hour <= 9, ]
data_with_anomalies_weekend <- data_with_anomalies[data_with_anomalies$datetime$year == 109 &
                                                           data_with_anomalies$datetime$wday == 6 & 
                                                           data_with_anomalies$datetime$hour >= 6 & 
                                                           data_with_anomalies$datetime$hour <= 9, ]
weekday_test_ano <- data_with_anomalies_weekday
weekend_test_ano <- data_with_anomalies_weekend
## weekday dataset
mod_weekday_train <- depmix( offset = NULL,
                             response = weekday_train$Global_active_power~weekday_train$Sub_metering_1+weekday_train$Sub_metering_2+weekday_train$Sub_metering_3-1, 
                             data = weekday_train,
                             nstates = 10,
                             ntimes = 20)
fm <- fit(mod_weekday_train)
mod_weekday_test <- depmix( offset = NULL,
                            response = weekday_test_ano$Global_active_power~weekday_test_ano$Sub_metering_1+weekday_test_ano$Sub_metering_2+weekday_test_ano$Sub_metering_3-1, 
                            data = weekday_test_ano,
                            nstates = 10,
                            ntimes = 20)
fm_new <- setpars(mod_weekday_test,getpars(fm))
summary(fm_new)
BIC(fm_new)
logLik(fm_new)
## weekend dataset 
mod_weekend_train <- depmix( offset = NULL,
                             response = weekend_train$Global_active_power~weekend_train$Sub_metering_1+weekend_train$Sub_metering_2+weekend_train$Sub_metering_3-1, 
                             data = weekend_train,
                             nstates = 10,
                             ntimes = 20)
fm <- fit(mod_weekend_train)

mod_weekend_test <- depmix( offset = NULL,
                            response = weekend_test_ano$Global_active_power~weekend_test_ano$Sub_metering_1+weekend_test_ano$Sub_metering_2+weekend_test_ano$Sub_metering_3-1, 
                            data = weekend_test_ano,
                            nstates = 10,
                            ntimes = 20)
fm_new <- setpars(mod_weekend_test,getpars(fm))
summary(fm_new)
BIC(fm_new)
logLik(fm_new)

#
## scala
mod_weekend_train <- depmix( offset = NULL,
                             response = scale(weekend_train$Global_active_power)~scale(weekend_train$Sub_metering_1)+scale(weekend_train$Sub_metering_2)+scale(weekend_train$Sub_metering_3)-1, 
                             data = weekend_train,
                             nstates = 10,
                             ntimes = 20)
fm <- fit(mod_weekend_train)

mod_weekend_test <- depmix( offset = NULL,
                            response = scale(weekend_train$Global_active_power)~scale(weekend_train$Sub_metering_1)+scale(weekend_train$Sub_metering_2)+scale(weekend_train$Sub_metering_3)-1, 
                            data = weekend_test_ano,
                            nstates = 10,
                            ntimes = 20)
fm_new <- setpars(mod_weekend_test,getpars(fm))
summary(fm_new)
BIC(fm_new)
logLik(fm_new)
