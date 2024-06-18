#R_code for analysis of adm_data
data = read.csv("adm_data.csv", header = T, sep =",")
attach(data)

#analysis of variables wrt Chance of admit
regr1 = lm(Chance.of.Admit ~ GRE, data = data)
cor(Chance.of.Admit, GRE) #0.802
plot(GRE,Chance.of.Admit, xlab = "GRE score", ylab= "Chance of admission")
abline(regr1, col="red", lwd= 2)
summary(regr1) #p.value = 2e-16 Multiple R squared:0.644


regr2 = lm(Chance.of.Admit ~ TOEFL, data = data)
cor(Chance.of.Admit, TOEFL) #0.79
plot(TOEFL,Chance.of.Admit, xlab = "TEOFL score", ylab= "Chance of admission")
abline(regr2, col="red", lwd= 2)
summary(regr2) #p.value = 2e-16 Multiple R squared:0.6266

regr3 = lm(Chance.of.Admit ~ University.Rating, data = data)
cor(Chance.of.Admit, University.Rating) #0.71
plot(University.Rating,Chance.of.Admit, xlab = "University Rating", ylab= "Chance of admission")
abline(regr3, col="red", lwd= 2)
summary(regr3) #p.value = 2e-16 Multiple R squared:0.5059

regr4 = lm(Chance.of.Admit ~ SOP, data = data)
cor(Chance.of.Admit, SOP) #0.67
plot(SOP,Chance.of.Admit, xlab = "Statement of purpose", ylab= "Chance of admission")
abline(regr4, col="red", lwd= 2)
summary(regr4) #p.value = 2e-16 Multiple R squared:0.4566

regr5 = lm(Chance.of.Admit ~ LOR, data = data)
cor(Chance.of.Admit, LOR) #0.67
plot(LOR,Chance.of.Admit, xlab = "Letter of Recommendation", ylab= "Chance of admission")
abline(regr5, col="red", lwd= 2)
summary(regr5) #p.value = 2e-16 Multiple R squared: 0.4488

regr6 = lm(Chance.of.Admit ~ CGPA, data = data)
cor(Chance.of.Admit, CGPA) #0.87
plot(CGPA,Chance.of.Admit, xlab = "Undergraduate GPA", ylab= "Chance of admission")
abline(regr6, col="red", lwd= 2)
summary(regr6) #p.value = 2e-16 Multiple R squared: 0.7626

regr7 = lm(Chance.of.Admit ~ factor(Research), data = data)
cor(Chance.of.Admit, Research) #0.553
plot(Research,Chance.of.Admit, xlab = "Letter of Recommendation", ylab= "Chance of admission")
abline(regr7, col="red", lwd= 2)
summary(regr7) #p.value = 2e-16 Multiple R squared: 0.306

#Multiple Linear regression
final_regr_1 = lm(Chance.of.Admit ~ . - Serial.Number, data = data)
summary(final_regr_1)

#Model selection

#Step down method
final_regr_2 = lm(Chance.of.Admit ~ .- Serial.Number- SOP, data = data )
summary(final_regr_3)

final_regr_3 = lm(Chance.of.Admit ~ .- Serial.Number- SOP- University.Rating, data = data )
summary(final_regr_3)

# final_regr_3 is best model for step-down

#perform cross validation
set.seed(123)
install.packages("caret")
library(caret)
random_sample <- createDataPartition(Chance.of.Admit,p = 0.8, list = FALSE)
training_dataset <- data[random_sample, ]
testing_dataset <- data[-random_sample,]
final_regr_4 = lm(Chance.of.Admit ~ . - Serial.Number, data = training_dataset)
predictions_4 <- predict(final_regr_4, testing_dataset)
data.frame( R2 = R2(predictions_4, testing_dataset $ Chance.of.Admit),
            RMSE = RMSE(predictions_4, testing_dataset $ Chance.of.Admit))
final_regr_5 = lm(Chance.of.Admit ~ . - Serial.Number-SOP, data = training_dataset)
predictions_5 <- predict(final_regr_5, testing_dataset)
data.frame( R2 = R2(predictions_5, testing_dataset $ Chance.of.Admit),
            RMSE = RMSE(predictions_5, testing_dataset $ Chance.of.Admit))
final_regr_6 = lm(Chance.of.Admit ~ . - Serial.Number-University.Rating, data = training_dataset)
predictions_6 <- predict(final_regr_6, testing_dataset)
data.frame( R2 = R2(predictions_6, testing_dataset $ Chance.of.Admit),
            RMSE = RMSE(predictions_6, testing_dataset $ Chance.of.Admit))
final_regr_7 = lm(Chance.of.Admit ~ . - Serial.Number-University.Rating-SOP, data = training_dataset)
predictions_7 <- predict(final_regr_7, testing_dataset)
data.frame( R2 = R2(predictions_7, testing_dataset $ Chance.of.Admit),
            RMSE = RMSE(predictions_7, testing_dataset $ Chance.of.Admit))
#the best is  final_regr_5  for cross validation so different from step down
data.frame(R2= c(R2(predictions_4, testing_dataset $ Chance.of.Admit),R2(predictions_5, testing_dataset $ Chance.of.Admit),R2(predictions_6, testing_dataset $ Chance.of.Admit),R2(predictions_7, testing_dataset $ Chance.of.Admit)), 
           RMSE = c(RMSE(predictions_4, testing_dataset $ Chance.of.Admit),RMSE(predictions_5, testing_dataset $ Chance.of.Admit),RMSE(predictions_6, testing_dataset $ Chance.of.Admit),RMSE(predictions_7, testing_dataset $ Chance.of.Admit)))

#choose final_regr_5 as the best model

#Check assumptions
  #Normality of residuals
  res = residuals(final_regr_5)
  qqnorm(res, xlab = "Residuals")
  qqline(res, col = "red")
  histogram(res, xlab = "Residuals", ylab = "Frequency")
  #Homoscedeneity
  fi = fitted(final_regr_5)
  plot(fi, res, xlab = "Fitted", ylab = "Residuals")
  shapiro.test(res)
  ks.test(res, "pnorm")
#errors are not normally distributed and they do not have constant variance

