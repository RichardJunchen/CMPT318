# load library
#install.packages('depmixS4')
#install.packages('quantmod')
library(depmixS4)
library(quantmod)
library(ggplot2)

# load data
data_project <- na.omit(read.table('TermProjectData.txt',sep=',',header=1))
data_anomalies1 <- na.omit(read.table('Data1(WithAnomalies).txt',sep=',',header=1))
data_anomalies2 <- na.omit(read.table('Data2(WithAnomalies).txt',sep=',',header=1))
data_anomalies3 <- na.omit(read.table('Data3(WithAnomalies).txt',sep=',',header=1))

# datetime format
data_project$datetime <- strptime(paste(data_project[, 1], data_project[, 2]), format = "%d/%m/%Y %H:%M:%S")

# select time window
data_project <- data_project[data_project$datetime$year == 108 &
                               data_project$datetime$wday == 4 & 
                               data_project$datetime$hour >= 6 & 
                               data_project$datetime$hour <= 9, ]


ggplot() + 
  layer(data=data_project, mapping=aes(x=Time, y=Global_active_power), stat="identity", geom = "line", position = "identity") + 
  xlab("Time") + 
  ylab("Average Global Intensity") + 
  labs(colour = "Time Frame") + 
  ggtitle("Weekay Scatter ~ Group 14")


# PCA
vars <- c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_2')
pca <- prcomp(data_project[vars])
summary(pca)

pca <- prcomp(data_project[vars],scale. = TRUE)
summary(pca)



# train model
# split data
data_project <- cbind(data_project, pca$x)
names(data_project)

set.seed(1)
data_project$gp <- runif(dim(data_project)[1])
data_train <- subset(data_project,data_project$gp <= 0.8)
data_test <- subset(data_project,data_project$gp > 0.8)

# single HMM
Nstates <- 2:16
BIC1 <- vector("list",15)
for (n in Nstates) {
  mod_mor <- depmix( offset = NULL,
                     response = scale(data_train$Global_active_power)~scale(data_train$Sub_metering_1)-1, 
                     data = data_train,
                     nstates = n)
  fm <- fit(mod_mor)
  print(fm)
  BIC1[[n-1]] <- BIC(fm)
}
plot(Nstates, BIC1, ty="b",main = 'Single model Plot')
# nstates = 12

# mult HMM
Nstates <- 2:16
BIC2 <- vector("list",15)
for (n in Nstates) {
  mod_mor <- depmix( offset = NULL,
                     response = scale(data_train$Global_active_power)~scale(data_train$Sub_metering_1)+scale(data_train$Sub_metering_2)+scale(data_train$Sub_metering_3)-1, 
                     data = data_train,
                     nstates = n)
  fm <- fit(mod_mor)
  print(fm)
  BIC2[[n-1]] <- BIC(fm)
}
plot(Nstates, BIC2, ty="b",main = "multiple model Plot")
# nstates = 12 

# pca single hmm 
Nstates <- 2:16
BIC3 <- vector("list",15)
for (n in Nstates) {
  mod_mor <- depmix( offset = NULL,
                     response = scale(data_train$Global_active_power)~data_train$PC1-1, 
                     data = data_train,
                     nstates = n)
  fm <- fit(mod_mor)
  print(fm)
  BIC3[[n-1]] <- BIC(fm)
}
plot(Nstates, BIC3, ty="b",main = 'PCA single model Plot')
# nstates = 12

# pca multiple hmm 
Nstates <- 2:16
BIC4 <- vector("list",15)
for (n in Nstates) {
  mod_mor <- depmix( offset = NULL,
                     response = scale(data_train$Global_active_power)~data_train$PC1+data_train$PC2-1, 
                     data = data_train,
                     nstates = n)
  fm <- fit(mod_mor)
  print(fm)
  BIC4[[n-1]] <- BIC(fm)
}
plot(Nstates, BIC4, ty="b",main = 'PCA multiple model Plot')
# nstates = 12



# train and test model
# single hmm
mod_mor <- depmix( offset = NULL,
                   response = scale(data_train$Global_active_power)~scale(data_train$Sub_metering_1)-1, 
                   data = data_train,
                   nstates = 13)
fm <- fit(mod_mor)
print(fm)

mod_mor <- depmix( offset = NULL,
                   response = scale(data_test$Global_active_power)~scale(data_test$Sub_metering_1)-1, 
                   data = data_test,
                   nstates = 13)
fm <- fit(mod_mor)
print(fm)

# multiple hmm
mod_mor <- depmix( offset = NULL,
                   response = scale(data_train$Global_active_power)~scale(data_train$Sub_metering_1)+scale(data_train$Sub_metering_2)+scale(data_train$Sub_metering_3)-1, 
                   data = data_train,
                   nstates = 14)
fm <- fit(mod_mor)
print(fm)

mod_mor <- depmix( offset = NULL,
                   response = scale(data_test$Global_active_power)~scale(data_test$Sub_metering_1)+scale(data_test$Sub_metering_2)+scale(data_test$Sub_metering_3)-1, 
                   data = data_test,
                   nstates = 14)
fm <- fit(mod_mor)
print(fm)


# PCA single model
mod_mor <- depmix( offset = NULL,
                   response = scale(data_train$Global_active_power)~data_train$PC1-1, 
                   data = data_train,
                   nstates = 12)
fm <- fit(mod_mor)
print(fm)

mod_mor <- depmix( offset = NULL,
                   response = scale(data_test$Global_active_power)~data_test$PC1-1, 
                   data = data_train,
                   nstates = 12)
ssfm <- fit(mod_mor)
print(fm)

# PCA multiple model
mod_mor <- depmix( offset = NULL,
                   response = scale(data_train$Global_active_power)~data_train$PC1+data_train$PC2-1, 
                   data = data_train,
                   nstates = 12)
fm <- fit(mod_mor)
print(fm)

mod_mor <- depmix( offset = NULL,
                   response = scale(data_test$Global_active_power)~data_test$PC1+data_test$PC2-1, 
                   data = data_test,
                   nstates = 12)
fm <- fit(mod_mor)
print(fm)


### anomaly
data_anomalies1 <- na.omit(read.table('Data1(WithAnomalies).txt',sep=',',header=1))
data_anomalies2 <- na.omit(read.table('Data2(WithAnomalies).txt',sep=',',header=1))
data_anomalies3 <- na.omit(read.table('Data3(WithAnomalies).txt',sep=',',header=1))


pca_1 <- prcomp(data_anomalies1[vars],scale. = TRUE)
pca_2 <- prcomp(data_anomalies2[vars],scale. = TRUE)
pca_3 <- prcomp(data_anomalies3[vars],scale. = TRUE)

data_anomalies1 <- cbind(data_anomalies1, pca_1$x)
data_anomalies2 <- cbind(data_anomalies2, pca_2$x)
data_anomalies3 <- cbind(data_anomalies3, pca_3$x)

# anomalies 1
# sigle model hmm
mod_mor <- depmix( offset = NULL,
                   response = scale(data_anomalies1$Global_active_power)~scale(data_anomalies1$Sub_metering_1)-1, 
                   data = data_anomalies1,
                   nstates = 13)
fm <- fit(mod_mor)
print(fm)
# multiple hmm
mod_mor <- depmix( offset = NULL,
                   response = scale(data_anomalies1$Global_active_power)~scale(data_anomalies1$Sub_metering_1)+scale(data_anomalies1$Sub_metering_2)+scale(data_anomalies1$Sub_metering_3)-1, 
                   data = data_anomalies1,
                   nstates = 14)
fm <- fit(mod_mor)
print(fm)
# PCA single model
mod_mor <- depmix( offset = NULL,
                   response = scale(data_anomalies1$Global_active_power)~data_anomalies1$PC1-1, 
                   data = data_anomalies1,
                   nstates = 12)
fm <- fit(mod_mor)
print(fm)
# PCA multiple model
mod_mor <- depmix( offset = NULL,
                   response = scale(data_anomalies1$Global_active_power)~data_anomalies1$PC1+data_anomalies1$PC2-1, 
                   data = data_anomalies1,
                   nstates = 12)
fm <- fit(mod_mor)
print(fm)





# anomalies 2
# sigle model hmm
mod_mor <- depmix( offset = NULL,
                   response = scale(data_anomalies2$Global_active_power)~scale(data_anomalies2$Sub_metering_1)-1, 
                   data = data_anomalies2,
                   nstates = 13)
fm <- fit(mod_mor)
print(fm)
# multiple hmm
mod_mor <- depmix( offset = NULL,
                   response = scale(data_anomalies2$Global_active_power)~scale(data_anomalies2$Sub_metering_1)+scale(data_anomalies2$Sub_metering_2)+scale(data_anomalies2$Sub_metering_3)-1, 
                   data = data_anomalies2,
                   nstates = 14)
fm <- fit(mod_mor)
print(fm)
# PCA single model
mod_mor <- depmix( offset = NULL,
                   response = scale(data_anomalies2$Global_active_power)~data_anomalies2$PC1-1, 
                   data = data_anomalies2,
                   nstates = 12)
fm <- fit(mod_mor)
print(fm)
# PCA multiple model
mod_mor <- depmix( offset = NULL,
                   response = scale(data_anomalies2$Global_active_power)~data_anomalies2$PC1+data_anomalies2$PC2-1, 
                   data = data_anomalies2,
                   nstates = 12)
fm <- fit(mod_mor)
print(fm)








# anomalies 3
# sigle model hmm
mod_mor <- depmix( offset = NULL,
                   response = scale(data_anomalies3$Global_active_power)~scale(data_anomalies3$Sub_metering_1)-1, 
                   data = data_anomalies3,
                   nstates = 13)
fm <- fit(mod_mor)
print(fm)
# multiple hmm
mod_mor <- depmix( offset = NULL,
                   response = scale(data_anomalies3$Global_active_power)~scale(data_anomalies3$Sub_metering_1)+scale(data_anomalies3$Sub_metering_2)+scale(data_anomalies3$Sub_metering_3)-1, 
                   data = data_anomalies3,
                   nstates = 14)
fm <- fit(mod_mor)
print(fm)
# PCA single model
mod_mor <- depmix( offset = NULL,
                   response = scale(data_anomalies3$Global_active_power)~data_anomalies3$PC1-1, 
                   data = data_anomalies3,
                   nstates = 12)
fm <- fit(mod_mor)
print(fm)
# PCA multiple model
mod_mor <- depmix( offset = NULL,
                   response = scale(data_anomalies3$Global_active_power)~data_anomalies3$PC1+data_anomalies3$PC2-1, 
                   data = data_anomalies3,
                   nstates = 12)
fm <- fit(mod_mor)
print(fm)
