#################### LIBRARIES USED ######################################

library(tidyverse)
library(lubridate)
library(PerformanceAnalytics)
library(outliers)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(caTools)
library(lattice)
library(caret)
library(xgboost)
library(data.table)
library(foreach)
library(e1071)
library(iterators)
library(parallel)
library(doParallel)
library(sp)
library(rgdal)
library(Formula)
library(sf)

###################  DATAFILE PATHS  #####################################

add_path <- '/Users/jvs22/Desktop/project/additional_variables.RData'
solar_path <- '/Users/jvs22/Desktop/project/solar_dataset.RData'
station_info_path <- '/Users/jvs22/Desktop/project/station_info.csv'

##########################    LOADING THE DATA    ###################################

add_var <- as.data.frame(readRDS(add_path))
solar <- as.data.frame(readRDS(solar_path))
station_info <- read.csv(station_info_path)

##########################  DIVIDING AND TRANSFORMING THE GENERAL DATA  ##################################

solar_farms <- solar[1:5113,1:99]
solar_farms_col_names <- colnames(solar_farms)[-1]  #extracting the 98 different stations
solar_farms$Date <- as.POSIXct(strptime(solar_farms$Date,format = '%Y%m%d', tz = 'us/central'))

PC_values <- solar[1:5113,100:ncol(solar)]    #extracting all of the PC_values from the data    

add_var$Date <- as.POSIXct(strptime(add_var$Date,format = '%Y%m%d', tz = 'us/central'))   #changing the date 

######################## ADDITIONAL VARIBALED DATA CLEANING & VISUALISATION ################################

add_var_scaled <- add_var

for(i in 2:ncol(add_var_scaled)){   #replacing all of the NA's of the additional variables with the median
  add_var_scaled[,i][is.na(add_var_scaled[,i])] <- median(as.numeric(add_var_scaled[,i]),na.rm = T)
  add_var_scaled[,i] <- (add_var_scaled[,i]-mean(as.numeric(add_var_scaled[,i])))/sd(as.numeric(add_var_scaled[,i])) # doing normalization of the variables
}

add_var_scaled_nout <- add_var_scaled

n <- 1
while (n <= 6){
  for (i in 2:ncol(add_var_scaled_nout)){       #replacing outliers with the median of the variable
    add_var_scaled_nout[,i] <- rm.outlier(as.numeric(add_var_scaled_nout[,i]),fill = T,median = T)
  }
  n <- n+1
}

### Boxplot of the scaled & scaled + outlier removed additional variables
add_var_box <- gather(add_var, key = 'Variables',value = 'Values',-Date)
add_var_scaled_box <- gather(add_var_scaled, key = 'Variables',value = 'Values',-Date)
add_var_scaled_nout_box <- gather(add_var_scaled_nout, key = 'Variables',value = 'Values',-Date)

ggplot(add_var_box,aes(Variables,Values)) + geom_boxplot(col="red") +
  labs(title="Additional Variables", subtitle="Without data cleaning",caption="add_var_dataset") +
  theme(axis.text.x = element_text(angle = 90),legend.position = 'none',panel.grid.major = element_blank())

ggplot(add_var_scaled_box,aes(Variables,Values)) + geom_boxplot(col ="orange") + 
  labs(title="Additional Variables", subtitle="Normalized + outliers",caption="add_var_dataset") +
  theme(axis.text.x = element_text(angle = 90),legend.position = 'none',panel.grid.major = element_blank())

ggplot(add_var_scaled_nout_box,aes(Variables,Values)) + geom_boxplot(col = "green") + 
  labs(title="Additional Variables", subtitle="Normalized + no outliers",caption="add_var_dataset") +
  theme(axis.text.x = element_text(angle = 90),legend.position = 'none',panel.grid.major = element_blank())

### Correlation Matrix of the additional variables
cormat <- signif(cor(add_var_scaled[,-1],use = "complete.obs"),2)  #creating correlation matrix between the normalized 100 variables
ord <- corrMatOrder(cormat, order="hclust")
cormat <- cormat[ord, ord]

par(mfrow = c(1,1))
ggcorrplot(cormat) + #plotting a correlation map for the normalized 100 variables
  theme(axis.text.x = element_text(color = "grey20", size = 5, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "grey20", size = 5, angle = 0, hjust = 1, face = "plain")) +
  ggtitle('Correlation map for the normalized 100 variables')


########################### SOLAR STATIONS DATA CLEANING & VISUALISATION #################################

# normalization for one particular station (ACME) for testing purposes
lapply(solar_farms[,-1], function(x) (x-mean(as.numeric(x)))/sd(as.numeric(x)))$ACME

solar_farms_scaled <- solar_farms

i <- 1
for(i in 1:nrow(solar_farms_scaled)){
  solar_farms_scaled[i,-1] <- (solar_farms_scaled[i,-1]-mean(as.numeric(solar_farms_scaled[i,-1])))/sd(as.numeric(solar_farms_scaled[i,-1]))
}

solar_farms_nout <- solar_farms

n <- 1
while (n <= 11){
  for (i in 1:nrow(solar_farms_nout)){       #replacing outliers with the median of the 98 Stations
    solar_farms_nout[i,-1] <- rm.outlier(as.numeric(solar_farms_nout[i,-1]),fill = T,median = T)
  }
  n <- n+1
}

solar_farms_scaled_box <- gather(solar_farms_scaled, key = 'Stations',value = 'Values',-Date)
ggplot(solar_farms_scaled_box,aes(x = Stations, y = Values)) + geom_boxplot(aes(fill = T)) + 
  theme(axis.text.x = element_text(angle = 90),legend.position = 'none',panel.grid.major = element_blank()) +
  ggtitle('Scaled Boxplot for the 98 solar stations in the State of Oklahoma')

solar_farms_scaled_clean_box <- gather(solar_farms_nout, key = 'Stations',value = 'Values',-Date)
ggplot(solar_farms_scaled_clean_box,aes(x = Stations, y = Values)) + geom_boxplot(aes(fill = T)) + 
  theme(axis.text.x = element_text(angle = 90),legend.position = 'none',panel.grid.major = element_blank()) +
  ggtitle('Scaled and cleaned Boxplot for the 98 solar stations in the State of Oklahoma')

# showing the production of each stations
title_plot <- paste('After running the rm.outlier',n-1,'times')

# line plot after removal
par(mfrow = c(2,1), mar = c(3,4,1,4))
for (i in 2:ncol(solar_farms)){
  plot(solar_farms$Date,solar_farms[,i], col = i, type = 'l',main = solar_farms_col_names[i-1], xlab = '',xaxt='n', ylab = 'Radiation')
  plot(solar_farms$Date,solar_farms_nout[,i], col = i, type = 'l',main = title_plot, ylab = 'Radiation')
}

### Density Plots for Solar Stations
x <- solar_farms[,2]
plot(density(as.numeric(x)),col = 1, ylim = c(0,6e-08),main = 'Density plot before cleaning')
for (i in 2:length(solar_farms)){
  x <- solar_farms[,i]
  lines(density(as.numeric(x)),col = i)
}

x <- solar_farms_nout[,2]
plot(density(as.numeric(x)),col = 1, ylim = c(0,6e-08),main = 'Density plot after cleaning')
for (i in 2:length(solar_farms_nout)){
  x <- solar_farms_nout[,i]
  lines(density(as.numeric(x)),col = i)
}

############################################  SUMMARY STATISITCS #############################################

summary(head(add_var)) #summary statistics of the additional variables before cleaning
summary(add_var_scaled) #summary statistics of the standardized additional variables
summary(add_var_scaled_nout) #summary statistics of the standardized + outlier removed additional variables

summary(solar_farms)  #summary statistics of solar_farms before cleaning
summary(solar_farms_scaled)  #summary statistics of solar_farms after normalization
summary(solar_farms_nout)  #summary statistics of solar_farms after outlier removal

################################## VISUALISATION OF THE STATIONS WITH SPATIAL DATA ############################

okh_shape <- st_read('/Users/jvs22/Desktop/project/tl_2010_40_county10/tl_2010_40_county10.shp')

ggplot(okh_shape) + 
  geom_sf(mapping = aes(fill = 'blue'),
          color = "white",
          size = 0.4) +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'none') +
  xlab('') + ylab('') +
  geom_point(data = station_info,  # Add and plot speices data
             shape=8, col = 'blue',
             size = 3,
             aes(x = elon, 
                 y = nlat, group = elev, col = elev)) +
  ggtitle("Localization of the Solar Stations in Oklahoma") +
  scale_fill_manual(values = 'grey')

####################################### VARIABLE IMPORTANCE ANALYSIS ######################################################
####################WE HAVE DECIDE AGAINST PCA SINCE VARIABLE IMPORTANCE IS MORE EFFECTIVE) ###############################

# Variable importance to identify the top 80 using solar_farms and additional variable without any cleaning
x <- filterVarImp(add_var[1:5113,-1],solar_farms[,2])
top_80 <- top_n(arrange(x,desc(x$Overall)),80)
y <- data_frame(Variable = rownames(top_80), Values = top_80$Overall, Station = rep(colnames(solar_farms)[2],80))

for(i in 3:ncol(solar_farms)){
  x <- filterVarImp(add_var_scaled[1:5113,-1],solar_farms[,i])
  top_80 <- top_n(arrange(x,desc(x$Overall)),80)
  x <- data_frame(Variable = rownames(top_80), Values = top_80$Overall, Station = rep(colnames(solar_farms)[i],80))
  y <- rbind(y,x)
}
view(group_by(y,Variable) %>% summarise(n = n()) %>% arrange(desc(n)))

# Variable importance to identify the top 80 using solar_farms and additional variable scaled
x1 <- filterVarImp(add_var_scaled[1:5113,-1],solar_farms[,2])
top_80_scaled <- top_n(arrange(x1,desc(x1$Overall)),80)
y1 <- data_frame(Variable = rownames(top_80_scaled), Values = top_80_scaled$Overall, Station = rep(colnames(solar_farms)[2],80))

for(i in 3:ncol(solar_farms)){
  x1 <- filterVarImp(add_var_scaled[1:5113,-1],solar_farms[,i])
  top_80_scaled <- top_n(arrange(x1,desc(x1$Overall)),80)
  x1 <- data_frame(Variable = rownames(top_80_scaled), Values = top_80_scaled$Overall, Station = rep(colnames(solar_farms)[i],80))
  y1 <- rbind(y1,x1)
}
view(group_by(y1,Variable) %>% summarise(n = n()) %>% arrange(desc(n)) %>% filter(n == 98))

# Variable importance to identify the top 80 using solar_farms + removed outliers and additional variable scaled + removed outliers
x3 <- filterVarImp(add_var_scaled_nout[1:5113,-1],solar_farms_nout[,2])
top_80_scaled_nout_both <- top_n(arrange(x3,desc(x3$Overall)),80)
y3 <- data_frame(Variable = rownames(top_80_scaled_nout_both), Values = top_80_scaled_nout_both$Overall, Station = rep(colnames(solar_farms_nout)[2],80))

for(i in 3:ncol(solar_farms_nout)){
  x3 <- filterVarImp(add_var_scaled_nout[1:5113,-1],solar_farms_nout[,i])
  top_80_scaled_nout_both <- top_n(arrange(x3,desc(x3$Overall)),80)
  x3 <- data_frame(Variable = rownames(top_80_scaled_nout_both), Values = top_80_scaled_nout_both$Overall, Station = rep(colnames(solar_farms_nout)[i],80))
  y3 <- rbind(y3,x3)
}
view(group_by(y3,Variable) %>% summarise(n = n()) %>% arrange(desc(n)))

# Variable importance to identify the top 80 using solar_farms and additional variable scaled + removed outliers
# best output with a limit of 95, getting 38 variables, used in ML section
x2 <- filterVarImp(add_var_scaled_nout[1:5113,-1],solar_farms[,2])
x2 <- rbind(x2,filterVarImp(PC_values,solar_farms[,2]) )
top_100_scaled_nout <- top_n(arrange(x2,desc(x2$Overall)),100)
y2 <- data_frame(Variable = rownames(top_100_scaled_nout), Values = top_100_scaled_nout$Overall, Station = rep(colnames(solar_farms)[2],100))

for(i in 3:ncol(solar_farms)){
  x2 <- filterVarImp(add_var_scaled_nout[1:5113,-1],solar_farms[,i])
  x2 <- rbind(x2,filterVarImp(PC_values,solar_farms[,i]) )
  top_100_scaled_nout <- top_n(arrange(x2,desc(x2$Overall)),100)
  x2 <- data_frame(Variable = rownames(top_100_scaled_nout), Values = top_100_scaled_nout$Overall, Station = rep(colnames(solar_farms)[2],100))
  y2 <- rbind(y2,x2)
}

z2 <- group_by(y2,Variable) %>% summarise(n = n()) %>% arrange(desc(n)) %>% top_n(80)
view(2)

############################################## ML PREDICTION ##############################################################
################################################# SVM MODEL ###############################################################


set.seed(11)

model_data <- cbind(Station = solar_farms[,2], add_var_scaled_nout[1:5113,colnames(add_var_scaled_nout) %in% z2$Variable])

# row indices for validation data (70%)
train_index <- sample(1:nrow(model_data),0.7*nrow(model_data))

# row indices for validation data (15%)
val_index <- sample(setdiff(1:nrow(model_data), train_index), 0.15*nrow(model_data)) 

# row indices for test data (15%)
test_index <- setdiff(1:nrow(model_data), c(train_index, val_index))

# split data
train <- model_data[train_index,]
val <- model_data[val_index,]
test  <- model_data[test_index,]

### Start cluster
stopImplicitCluster()
registerDoParallel(cores = detectCores())

### Define grid
c_values <- 10^seq(from = -2, to = 1, by = 0.5)
eps_values <- 10^seq(from = -2, to = 0, by = 0.5)
gamma_values <- 10^seq(from = -3, to = -1, by = 0.5)

### Compute grid search

grid_results <-  foreach (c = c_values, .combine = rbind)%:%
  foreach (eps = eps_values, .combine = rbind)%:%
  foreach (gamma = gamma_values, .combine = rbind)%dopar%{
    
    print(sprintf("Start of c = %s - eps = %s - gamma = %s", c, eps, gamma))
    
    # train SVM model with a particular set of hyperparamets
    model <- svm(Station ~ ., data = train,kernel = 'radial',
                 cost = c, epsilon = eps, gamma = gamma)
    
    # Get model predictions
    predictions_train <- predict(model, newdata = train)
    predictions_val <- predict(model, newdata = val)
    
    # Get errors
    errors_train <- predictions_train - train$Station
    errors_val <- predictions_val - val$Station
    
    # Compute Metrics
    mse_train <- round(mean(errors_train^2), 2)
    mae_train <- round(mean(abs(errors_train)), 2)
    
    mse_val <- round(mean(errors_val^2), 2)
    mae_val <- round(mean(abs(errors_val)), 2)
    
    # Build comparison table
    grid_results <- rbind(grid_results,
                          data.table(c = c, eps = eps, gamma = gamma, 
                                     mse_train = mse_train, mae_train = mae_train,
                                     mse_val = mse_val, mae_val = mae_val))
  }

# Order results by increasing mse and mae
grid_results <- grid_results[order(mae_val, mae_train)]

# Check results
best <- grid_results[1]

### Train final model
# train SVM model with best found set of hyperparamets
model <- svm(Station ~ ., data = train, kernel="radial",
             cost = best$c, epsilon = best$eps, gamma = best$gamma)

# Get model predictions
predictions_train <- predict(model, newdata = train)
predictions_val <- predict(model, newdata = val)
predictions_test <- predict(model, newdata = as.matrix(test))

# Get errors
errors_train <- predictions_train - train$Station
errors_val <- predictions_val - val$Station
errors_test <- predictions_test - test$Station

# Compute Metrics
mae_train <- round(mean(abs(errors_train)), 2)
mae_val <- round(mean(abs(errors_val)), 2)
mae_test <- round(mean(abs(errors_test)), 2);

## Summary
sprintf("MAE_train = %s - MAE_val = %s - MAE_test = %s", mae_train, mae_val, mae_test)

################################################### XGBOOST ###############################################################

set.seed(11)

#preparation of the data
model_data <- cbind(Station = solar_farms[,2], add_var_scaled[1:5113,colnames(add_var_scaled) %in% z2$Variable],solar[1:5113,100:101])

# row indices for validation data (70%)
train_index <- sample(1:nrow(model_data),0.7*nrow(model_data))

# row indices for validation data (15%)
val_index <- sample(setdiff(1:nrow(model_data), train_index), 0.15*nrow(model_data));  

# row indices for test data (15%)
test_index <- setdiff(1:nrow(model_data), c(train_index, val_index));

# split data
train <- model_data[train_index,]; 
val <- model_data[val_index,]; 
test  <- model_data[test_index,];

### Start cluster
stopImplicitCluster();
registerDoParallel(cores = 9);

### Define grid
eta_values <- seq(from = 0.006, to = 0.5, by = 0.001)
gamma_values <- seq(from = 0.1, to = 1, by = 0.1 )
nrounds <- seq(500,1500,10)


### Compute grid search
grid_results_1 <- data.table()

grid_results <-  for (eta in eta_values){
  for (gamma in gamma_values){
    for(nrounds in nrounds){
      
      #print(sprintf("Start of eta = %s - max_depth = %s - gamma = %s", eta, max_depth, gamma));
      
      # train xgboost model with a particular set of hyperparamets
      model <- xgboost(as.matrix(train[,-1]), label = train$Station,
                       eta = eta, max_depth = 1, nrounds = nrounds, 
                       subsample = 1, verbose = 0,
                       gamma = gamma);
      
      # Get model predictions
      predictions_train <- predict(model, newdata = as.matrix(train[,-1]))
      predictions_val <- predict(model, newdata = as.matrix(val[,-1]))
      
      # Get errors
      errors_train <- predictions_train - train$Station;
      errors_val <- predictions_val - val$Station;
      
      # Compute Metrics
      rmse_train <- round(sqrt(mean(errors_train^2)), 2);
      mae_train <- round(mean(abs(errors_train)), 2);
      
      rmse_val <- round(sqrt(mean(errors_val^2)), 2);
      mae_val <- round(mean(abs(errors_val)), 2);
      
      # Build comparison table
      grid_results_1 <<- rbind(grid_results_1,
                               data.table(eta = eta, 
                                          gamma = gamma, 
                                          nrounds = nrounds,
                                          mae_train = mae_train,
                                          mae_val = mae_val,
                                          diff = abs(mae_val-mae_train)));
      print(grid_results_1)
    }
  }
}

# Order results by increasing mse and mae
grid_results_1 <- mutate(grid_results_1,diff = abs(mae_train-mae_val))

grid_results_1 <- grid_results_1[order(diff)];
view(grid_results_1)

# Check results
best <- grid_results_1[2,1:2];

################################################## PREDICTION STATIONS ##################################################

#Predicting for the stations
prediction_stations_xgboost <- data.table(Date = solar[5114:nrow(solar),1])
names_solarfarms <- colnames(solar_farms)

#preparation of the data
model_data <- cbind(Station = solar_farms[,2], add_var_scaled[1:5113,colnames(add_var_scaled) %in% z2$Variable])

# split data
test  <- add_var_scaled[5114:6909,colnames(add_var_scaled) %in% z2$Variable]


for(i in 2:ncol(solar_farms)){
  
  #split data
  train <- cbind(Station = solar_farms[,i], add_var_scaled[1:5113,colnames(add_var_scaled) %in% z2$Variable])
  
  # train xgboost model with a particular set of hyperparamets
  model <- xgboost(as.matrix(train[,-1]), label = train$Station,
                   eta = best$eta, max_depth = 1, nrounds = 1000, 
                   subsample = 1, verbose = 0,
                   gamma = best$gamma);
  
  # Get model predictions
  predictions <- as.data.table(predict(model, newdata = as.matrix(test)))
  colnames(predictions) <-  names_solarfarms[i]
  
  # Build comparison table
  prediction_stations_xgboost <<- cbind(prediction_stations_xgboost,
                                        predictions);
  
  print(prediction_stations)
}

write.csv(prediction_stations, '/Users/jvs22/Desktop/project/Rprediction_stations.csv', row.names = F)

########################################## XGBOOST CROSS VALIDATION ###############################################


### Compute grid search
grid_results_1 <- data.table(eta = 0, 
                             gamma = 0, 
                             mae_train = 0,
                             mae_val = 0,
                             diff = 0)

grid_results <-  for (eta in eta_values){
  for (gamma in gamma_values){
    
    #print(sprintf("Start of eta = %s - max_depth = %s - gamma = %s", eta, max_depth, gamma));
    
    # train xgboost-CV model with a particular set of hyperparamets
    model <<- xgb.cv(as.matrix(model_data[,-1]), label = model_data$Station, 
                     params = list(eta = eta, max_depth = 1, subsample = 1, verbose = 0, gamma = gamma), 
                     nfold=5, nrounds = 10, prediction = T, metrics=list("rmse"),cb.cv.predict(save_models = T))
    print(model)
  }
}

############################################## GBM MODEL #################################################

#Predicting for the stations
prediction_stations_gbm <- data.table(Date = solar[5114:nrow(solar),1])
names_solarfarms <- colnames(solar_farms)

#preparation of the data
model_data <- cbind(Station = solar_farms[,2], add_var_scaled[1:5113,colnames(add_var_scaled) %in% z2$Variable])

# split data
test  <- add_var_scaled[5114:6909,colnames(add_var_scaled) %in% z2$Variable]



trainctrl <- trainControl(method = "LGOCV", number = 3,savePredictions = T, allowParallel = TRUE)

myGrid <- expand.grid(n.trees = c(150, 175, 200, 225),
                      interaction.depth = c(5, 6, 7, 8, 9),
                      shrinkage = c(0.075, 0.1, 0.125, 0.15, 0.2),
                      n.minobsinnode = c(7, 10, 12, 15))

gbm_tree_tune <- train(Station ~ ., data = model_data, method = "gbm", distribution = "gaussian",
                       trControl = trainctrl, verbose = TRUE,
                       tuneGrid = myGrid, metric = 'mae')

for(i in 2:ncol(solar_farms)){
  
  #split data
  train <- cbind(Station = solar_farms_nout[,i], add_var_scaled[1:5113,colnames(add_var_scaled) %in% z2$Variable])
  
  # train xgboost model with a particular set of hyperparamets
  model <- train(Station ~ ., data = model_data, method = "gbm", distribution = "gaussian",
                 trControl = trainctrl, verbose = TRUE,
                 tuneGrid = myGrid)
  
  # Get model predictions
  predictions <- as.data.table(predict(model, newdata = as.matrix(test)))
  colnames(predictions) <-  names_solarfarms[i]
  
  # Build comparison table
  prediction_stations_earth <<- cbind(prediction_stations_earth,
                                      predictions);
  
  print(prediction_stations)
}

write.csv(prediction_stations_earth, '/Users/jvs22/Desktop/project/Rprediction_stations_earth.csv', row.names = F)

prediction_stations_earth$Date <- solar[5114:nrow(solar),1]
prediction_stations_earth$Date <- as.POSIXct(strptime(prediction_stations_earth$Date,format = '%Y%m%d', tz = 'us/central'))   #changing the date 

all <- rbind(solar_farms,prediction_stations_earth)

ggplot(all,aes(Date,RETR)) + geom_line()

############################################### EARTH MODEL ###############################################

#Predicting for the stations
prediction_stations_earth <- data.table(Date = solar[5114:nrow(solar),1])
names_solarfarms <- colnames(solar_farms)

#preparation of the data
model_data <- cbind(Station = solar_farms_nout[,2], add_var_scaled[1:5113,colnames(add_var_scaled) %in% z2$Variable])

# split data
test  <- add_var_scaled[5114:6909,colnames(add_var_scaled) %in% z2$Variable]


for(i in 2:ncol(solar_farms)){
  
  #split data
  train <- cbind(Station = solar_farms_nout[,i], add_var_scaled[1:5113,colnames(add_var_scaled) %in% z2$Variable])
  
  # train xgboost model with a particular set of hyperparamets
  model <- earth(Station ~ ., data = train, pmethod = "cv", nfold = 5)
  
  # Get model predictions
  predictions <- as.data.table(predict(model, newdata = as.matrix(test)))
  colnames(predictions) <-  names_solarfarms[i]
  
  # Build comparison table
  prediction_stations_earth <<- cbind(prediction_stations_earth,
                                      predictions);
  
  print(prediction_stations)
}

write.csv(prediction_stations_earth, '/Users/jvs22/Desktop/project/Rprediction_stations_earth.csv', row.names = F)

prediction_stations_earth$Date <- solar[5114:nrow(solar),1]
prediction_stations_earth$Date <- as.POSIXct(strptime(prediction_stations_earth$Date,format = '%Y%m%d', tz = 'us/central'))   #changing the date 

all <- rbind(solar_farms,prediction_stations_earth)

ggplot(all,aes(Date,RETR)) + geom_line()


##################################################### NEURAL NETWORK ######################################################

#Predicting for the stations
prediction_stations_gbm <- data.table(Date = solar[5114:nrow(solar),1])
names_solarfarms <- colnames(solar_farms)

#preparation of the data
model_data <- cbind(Station = solar_farms[,2], add_var_scaled[1:5113,colnames(add_var_scaled) %in% z2$Variable])

# split data
test  <- add_var_scaled[5114:6909,colnames(add_var_scaled) %in% z2$Variable]

for(i in 2:ncol(solar_farms)){
  
  # neural network model 
  model <- neuralnet(Station ~ ., data = model_data, hidden = 4, stepmax = 4)
  
  # Get model predictions
  predictions <- as.data.table(predict(model, newdata = as.matrix(as.numeric)))
  colnames(predictions) <-  names_solarfarms[i]
  
  # Build comparison table
  prediction_stations_earth <<- cbind(prediction_stations_earth,
                                      predictions);
  
  print(prediction_stations)
}

write.csv(prediction_stations_earth, '/Users/jvs22/Desktop/project/Rprediction_stations_earth.csv', row.names = F)


################################################## PREDICTION STATIONS ##################################################

#Predicting for the stations
prediction_stations_xgboost <- data.table(Date = solar[5114:nrow(solar),1])
names_solarfarms <- colnames(solar_farms)

#preparation of the data
model_data <- cbind(Station = solar_farms[,2], add_var_scaled[1:5113,colnames(add_var_scaled) %in% z2$Variable],solar[1:5113,100:101])

# split data
test  <- cbind(add_var_scaled[5114:6909,colnames(add_var_scaled) %in% z2$Variable],solar[5114:6909,100:101])


for(i in 2:ncol(solar_farms)){
  
  #split data
  train <- cbind(Station = solar_farms[,i], add_var_scaled[1:5113,colnames(add_var_scaled) %in% z2$Variable],solar[1:5113,100:101])
  
  # train xgboost model with a particular set of hyperparamets
  model <- xgboost(as.matrix(train[,-1]), label = train$Station,
                   eta = 0.015, max_depth = 1, nrounds = 800, 
                   subsample = 1, verbose = 0,
                   gamma = 1);
  
  # Get model predictions
  predictions <- as.data.table(predict(model, newdata = as.matrix(test)))
  colnames(predictions) <-  names_solarfarms[i]
  
  # Build comparison table
  prediction_stations_xgboost <<- cbind(prediction_stations_xgboost,
                                        predictions);
  
  return(prediction_stations_xgboost)
}

write.csv(prediction_stations_xgboost, '/Users/jvs22/Desktop/project/Prediction_stations_xgboost.csv', row.names = F)

prediction_stations_xgboost$Date <- as.POSIXct(strptime(prediction_stations_xgboost$Date,format = '%Y%m%d', tz = 'us/central'))   #changing the date 
dim(solar_farms)
dim(prediction_stations_xgboost)
all <- rbind(solar_farms,prediction_stations_xgboost)
all$Values <- ifelse(all$Date < '2008-01-01', 'Given', 'Predicted')

ggplot(all,aes(Date,MIAM, group = Values, colour = Values)) + geom_line() + 
  scale_color_manual(values=c("#dfae00", "#aa151b")) +
  xlab('Years') + ylab('Daily incoming solar energy') + ggtitle('Prediction of daily incoming solar energy of solar station MIAM') + theme_light()

### EXTRA TESTING ###
summary(xgboost(as.matrix(model_data[,-1]), label = model_data$Station,
                eta = 0.4, nrounds = 1000, min_child_weight = 2,
                gamma = 2))


################################## ADDITIONAL ATTEMPS TO WORK WITH THE DATA ###############################################

# n <- 2
# for (i in 2:nrow(solar_farms)){       #matrix with the station, p-value and description of outliers
#   significance_matrix <- rbind(significance_matrix,c(solar_farms_col_names[n],chisq.out.test(as.numeric(solar_farms[i,-1]))$p.value,chisq.out.test(as.numeric(solar_farms[i,-1]))$alternative))
#   n <- n + 1
#   if (n %% 98 == 0){
#     n <- 1
#   }
# }

# significance_matrix <- data_frame(Variables = colnames(add_var)[2],p_value = chisq.out.test(as.numeric(solar_farms[1,-1]))$p.value,Comment = chisq.out.test(as.numeric(solar_farms[1,-1]))$alternative)
# significance_matrix <- cbind(Date = solar_farms$Date[1:5113],significance_matrix)

# farms_variables <- cbind(station =solar_farms[,2],add_var_scaled[1:5113,-1])

# lm_stations <- glm(station ~ .,farms_variables, family = 'gaussian') #glm model to predict the output of the station
# summary(lm_stations)
# plot(farms_variables$V6409,farms_variables$station) # correlation of station vs. V6409
# abline(lm_stations, col = 'red')

# y <- PC_values[,2]
# plot(density(as.numeric(y[!is.na(y)])),col = 1,ylim = c(0,0.2),xlim = c(-200,200))
# lines(density(as.numeric(PC_values[,170][!is.na(PC_values[,170])])),col = 3)
# for (i in 2:length(PC_value)){
#   x <- PC_values[,i]
#   lines(density(as.numeric(x[!is.na(x)])),col = i)
# }
#
# PC_values <- cbind(Date= solar$Date,PC_values)
# solar_farms <- gather(solar_farms, key = 'Stations',value = 'values',-Date)


