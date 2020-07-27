
# Step 1: Setting Working Directory

setwd("C:/............../Summer 2020/Forecasting/Assignments/2")

setwd("C:/Users/Pallavi/OneDrive/Documents/Summer 2020/Forecasting/Assignments/2")


#Step 2:  Loading the required library
library(ggplot2)
library(fpp2)
library(forecast)
library(corrplot)
library(ggcorrplot)


# Step 3: Loading Data into R

data <- read.csv("assignment2.csv", header = TRUE)


----------------------------------------------------------------------------------------------------------

  
#Step 4: Converting Data into ts() 

data_ts <- ts(data, start= 1977, frequency=4)

class(data_ts)

mytimeseries <- data_ts[, c('Mortgage','ERate', 'GDP')]

class(mytimeseries)
str(mytimeseries)


----------------------------------------------------------------------------------------------------------

  
# Step 5: Plotting Time series in single frame for better comparison

autoplot(mytimeseries, facets = TRUE) +
  ggtitle("REGRESSION TIME-SERIES ANALYSIS")+
  xlab("Years") +
  ylab("Rate of Change")


----------------------------------------------------------------------------------------------------------


# Step6: Dividing dataset into training and test

test <- tail(mytimeseries,12)
training <- window(mytimeseries, start= c(1977,1), end= c(2016,4))

autoplot(training, facets = TRUE)+
  ggtitle("Time Series Plot of Training Dataset")+
  xlab("Years") +
  ylab("Rate of Change")

autoplot(test, facets = TRUE)+
  ggtitle("Time Series Plot of Test Dataset")+
  xlab("Years") +
  ylab("Rate of Change")


----------------------------------------------------------------------------------------------------------


# Step 7: Running simple regression using tslm() on one predictor and one forecast variable from Training Dataset

modelData1 <- training[,c('Mortgage', 'GDP')]

model1 <- tslm(Mortgage~GDP, modelData1)




------------------------------------------------------------------------------------------------------------


# Step 8 for summary results and details regression results:

# get summary results and detailed regression results: 
summary(model1)
summary(influence.measures(model1))
anova(model1)              
coefficients(model1)
checkresiduals(model1)


# Plotting regression using ggpplot()


ggplot(as.data.frame(modelData1),aes(x=GDP, y=Mortgage)) +
  geom_point() +    
  geom_smooth(method="lm", se=TRUE)

# Checking Correlation

modelData1 %>%
  as.data.frame() %>%
  GGally::ggpairs()



  
----------------------------------------------------------------------------------------------------------

  
#Step 9 Predicting values and fit in the model

predvar1<- modelData1

fitted1 <- fitted(model1)   
fitted

predfinal1 <- cbind(predvar1, fitted)

# Plotting Fitted value with the Original Value


ggplot(as.data.frame(predfinal1),aes(x=predvar1.Mortgage, y=fitted1)) +
  xlab("Orignal Mortgage Value")+
  ylab("Fitted Mortgage Value")+
  ggtitle("Plot between Orignal and Fitted Value")+
  geom_point() +    
  geom_smooth(method="lm", se=TRUE)+
  theme(plot.title = element_text(hjust = 0.5))

# Checking Correlation Model 3

modelData3 %>%
  as.data.frame() %>%
  GGally::ggpairs()



---------------------------------------------------------------------------------------------------------------


# Step 10 Forecasting prediction

fcast1 <- forecast(model1, newdata= newdata, h= 15)

test

newdata <- data.frame(
  GDP= c(2797.35,2797.35,2797.35,2797.35)
)


# Plotting Forecasting

autoplot(modelData1[,'Mortgage'])+
  ylab('Change in Mortgage')+
  autolayer(fcast1, PI= TRUE, series= 'Simple Regression Forecasting')+
  ggtitle("Model1 Forecast")


------------------------------------------------------------------------------------------------------------

  
# Step 11: Adding Dummy Variables and remaining variables to the models.
  
model2 <- tslm(modelData1~ trend+ season)

summary(model2)
summary(influence.measures(model2))
anova(model2)
CV(model2)
accuracy(model2)
accuracy(model1)
accuracy(model3)



modelData3 <- training[, c('Mortgage','GDP', 'ERate')]
model3 <- tslm(Mortgage~ GDP+ERate, model1Data)

summary(model3)
summary(influence.measures(model3))
anova(model3)              
coefficients(model3)

----------------------------------------------------------------------------------------------------------

  
#Step 12 Predicting values and fit in model3
  
predvar3<- modelData3

fitted3 <- fitted(model3)   
fitted

predfinal3 <- cbind(predvar3, fitted3)

# Plotting Fitted value with the Original Value


autoplot(predvar3[,'Mortgage'], series= 'Orignal Mortgage')+
  autolayer(fitted3, series='Fitted Mortgage')+
  xlab('Time')+
  ylab('Mortgage Rate')+
  ggtitle('Fitted Data Series')+
  guides(color=guide_legend(title = ""))


ggplot(as.data.frame(predfinal3),aes(x=predvar3.Mortgage, y=fitted3)) +
  xlab("Mortgage Original Value")+
  ylab("Mortgage Fitted Value")+
  ggtitle("Model 3 Original vs Fitted Value")+
  geom_point() +    
  geom_smooth(method="lm", se=TRUE)+
  theme(plot.title = element_text(hjust = 0.5))


----------------------------------------------------------------------------------------------------------

  
# Step 13 Forecasting prediction for model3
  
fcast3 <- forecast(model3, newdata= newdata3)

newdata3 <- data.frame(
  GDP= c(2797.35,2797.35,2797.35,2797.35),
  ERate = c(74.17,74.17,74.17,74.17)
)


# Plotting Forecasting

autoplot(modelData3[,'Mortgage'])+
  ylab('Change in Mortgage')+
  autolayer(fcast3, PI= TRUE, series= 'Multi Variate Forecasting')+
  ggtitle("Model3 Forecast")  



-----------------------------------------------------------------------------------------------------------

  
# Step 14 :check correlation matrix to find relationships

corr<- cor(modelData1)

ggcorrplot(corr,legend.title = "correlation")+ggtitle("Correlation Plots: MORTGAGE & GDP")


corr3<- cor(modelData3)

ggcorrplot(corr3,legend.title = "correlation")+ggtitle("Correlation Plots: MORTGAGE, GDP, ERate")


---------------------------------------------------------------------------------------------------------

  
# Step 15: Checking residuals

checkresiduals(model1)

checkresiduals(model3)




----------------------------------------------------------------------------------------------------------


# Step 16: for comparing models against each others to find the best model

CV(model1)
accuracy(model1)


CV(model3)
accuracy(model3)


------------------------------------------------------------------------------------------------------------




































































































































































































































