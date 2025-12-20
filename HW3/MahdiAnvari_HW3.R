# Question 1


library("NHANES")
data <- NHANES[, c("BPSysAve","Age","TotChol") ]  
data <- na.omit(data)
data

model <- lm(TotChol ~ Age + BPSysAve, data = data)
summary(model)

"
Call:
lm(formula = TotChol ~ Age + BPSysAve, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.3811 -0.6815 -0.0700  0.6163  8.6225 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 3.6968205  0.0819913  45.088  < 2e-16 ***
Age         0.0145014  0.0006653  21.796  < 2e-16 ***
BPSysAve    0.0050874  0.0007745   6.569 5.39e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.021 on 7990 degrees of freedom
Multiple R-squared:  0.1006,	Adjusted R-squared:  0.1003 
F-statistic: 446.6 on 2 and 7990 DF,  p-value: < 2.2e-16
"

RMSE <- sqrt(mean(model$residuals ^ 2))
RMSE
"1.020508"

library(ggplot2)
ggplot(data, aes(x = Age, y = TotChol)) +
  geom_point() +
  geom_smooth(method = "lm") + 
  labs(x = "Age",
       y = "TotChol")

data$AgeBin <- cut(data$Age,
                      breaks = c(0, 20, 40, 60, 80, 100),
                      labels = c("0–20", "20–40", "40–60", "60–80", "80–100"),
                      right = FALSE)

ggplot(data, aes(x = AgeBin, y = TotChol)) +
  geom_boxplot() +
  labs(x = "Age Bins",
       y = "TotChol")

# RMSE = 1.020508; The model predict by 1.021 units of error on average, approximately high error given the scale of total cholesterol.
# R2 = 0.1001; The model can explain only 10% of the variance in cholesterol values, indicating that they are not a powerful predictors for the cholesterol.
# P-value < 0.05; The overall P-value and the P-value for each predictor is below the threshold, indicating that they are statistically significant predictors. 
# Age coefficient = 0.0145014; 1 unit increase in Age -> 0.0145014 units increase in Total Cholesterol. (weak effect of Age on cholesterol, but still significant)
# BPSysAve coefficient = 0.005087; 1 unit increase in BPSysAve -> 0.005087 units increase in Total Cholesterol. (weak effect of BPSysAve on cholesterol, but still significant)


# Question 2


data("airquality")
data <- airquality[, c("Ozone","Wind","Temp") ] 
data <- na.omit(data)
data

model1 <- lm(Ozone~Temp + Wind, data = data)
summary(model1)
"
Call:
lm(formula = Ozone ~ Temp + Wind, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-41.251 -13.695  -2.856  11.390 100.367 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -71.0332    23.5780  -3.013   0.0032 ** 
Temp          1.8402     0.2500   7.362 3.15e-11 ***
Wind         -3.0555     0.6633  -4.607 1.08e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 21.85 on 113 degrees of freedom
Multiple R-squared:  0.5687,	Adjusted R-squared:  0.5611 
F-statistic:  74.5 on 2 and 113 DF,  p-value: < 2.2e-16
"

# RSE = 21.85; The model predict by 21.85 units of error on average, approximately high error given the scale of ozone values.
# R2 = 0.5687; The model can explain 57% of the variance in Ozone values, indicating that they are moderately strong predictors for the Ozone values.
# P-value < 0.05; The overall P-value and the P-value for each predictor is below the threshold, indicating that they are statistically significant predictors. 
# Temp coefficient = 1.8402; 1 unit increase in Temp -> 1.8402 units increase in the Ozone value.
# Wind coefficient = -3.0555; 1 unit increase in Wind -> 3.0555 units decrease in the Ozone value.

model2 <- lm(Ozone~Wind, data = data)
summary(model2)
"
Call:
lm(formula = Ozone ~ Wind, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-51.572 -18.854  -4.868  15.234  90.000 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  96.8729     7.2387   13.38  < 2e-16 ***
Wind         -5.5509     0.6904   -8.04 9.27e-13 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 26.47 on 114 degrees of freedom
Multiple R-squared:  0.3619,	Adjusted R-squared:  0.3563 
F-statistic: 64.64 on 1 and 114 DF,  p-value: 9.272e-13
"

# RSE = 26.47; The model predict by 26.47 units of error on average, performing worse compared to the model that considers both Temp and Wind Simultaneously. 
# R2 = 0.3619; The model can explain only 36% of the variance in Ozone values, having less power for prediction than the first model. 
# P-value < 0.05; The overall P-value is below the threshold, indicating that Wind statistically a significant predictor. 
# Wind coefficient = -5.5509; 1 unit increase in Wind -> 5.5509 units decrease in the Ozone value.

model3 <- lm(Ozone~Temp, data = data)
summary(model3)

"
Call:
lm(formula = Ozone ~ Temp, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-40.729 -17.409  -0.587  11.306 118.271 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) -146.9955    18.2872  -8.038 9.37e-13 ***
Temp           2.4287     0.2331  10.418  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 23.71 on 114 degrees of freedom
Multiple R-squared:  0.4877,	Adjusted R-squared:  0.4832 
F-statistic: 108.5 on 1 and 114 DF,  p-value: < 2.2e-16
"

# RSE = 23.71; The model predict by 23.71 units of error on average, performing worse compared to the model that considers both Temp and Wind Simultaneously. 
# R2 = 0.4877; The model can explain only 49% of the variance in Ozone values, having less power for prediction than the first model.  
# P-value < 0.05; The overall P-value is below the threshold, indicating that Temp is statistically a significant predictor. 
# Temp coefficient = 2.4287; 1 unit increase in Temp -> 2.4287 units increase in the Ozone value.

model4 <- lm(Ozone~Wind*Temp, data = data)
summary(model4)


"
Call:
lm(formula = Ozone ~ Wind * Temp, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-39.906 -13.048  -2.263   8.726  99.306 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -248.51530   48.14038  -5.162 1.07e-06 ***
Wind          14.33503    4.23874   3.382 0.000992 ***
Temp           4.07575    0.58754   6.937 2.73e-10 ***
Wind:Temp     -0.22391    0.05399  -4.147 6.57e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 20.44 on 112 degrees of freedom
Multiple R-squared:  0.6261,	Adjusted R-squared:  0.6161 
F-statistic: 62.52 on 3 and 112 DF,  p-value: < 2.2e-16
"
# RSE = 20.44; The model predict by 20.44 units of error on average, performing the best among the models fitted previously. 
# R2 = 0.6261; The model can explain only 63% of the variance in Ozone values, being the most powerful in prediction among all other models.
# P-value < 0.05; The overall P-value and the P-value for each predictor is below the threshold, indicating that they are statistically significant predictors. 
# Temp coefficient = 14.33503; 1 unit increase in Temp -> 14.33503 units increase in the Ozone value. however, this coefficient is not constant and change when the Wind changes, by the coefficient of Temp:Wind. 
# Wind coefficient = 4.07575; 1 unit increase in Wind -> 4.07575 units increase in the Ozone value.however, this coefficient is not constant and change when the Temp changes, by the coefficient of Temp:Wind. 



# The model that was fitted to the Temp and Wind interaction has the best performance, and I would choose that if I were to choose only one model.


#prediction with the first model:
predict(model1, data.frame(Wind = 10, Temp = 85))
"54.82707 "




# Question 3

library(MASS)

data("Boston")

data <- Boston[, c("rm", "nox", "age", "medv")]
data <- na.omit(data)
data

model <- lm(medv ~ nox + rm, data = data)
summary(model)

"
Call:
lm(formula = medv ~ nox + rm, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-17.889  -3.287  -0.636   2.518  39.638 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -18.2059     3.3393  -5.452 7.82e-08 ***
nox         -18.9706     2.5304  -7.497 2.97e-13 ***
rm            8.1567     0.4173  19.546  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 6.281 on 503 degrees of freedom
Multiple R-squared:  0.5354,	Adjusted R-squared:  0.5336 
F-statistic: 289.9 on 2 and 503 DF,  p-value: < 2.2e-16"


confint(model, level= 0.95)

"                2.5 %     97.5 %
(Intercept) -24.76666 -11.645106
nox         -23.94200 -13.999233
rm            7.33676   8.976551"


# RSE = 6.281; The model predict by 6281 USD of error on average, performing moderately given the scale of "medv"s in dataset. 
# R2 = 0.5354; The model can explain 53.54% of the variance in medv values, 
# P-value < 0.05; The overall P-value and the P-value for each predictor is below the threshold, indicating that they are statistically significant predictors. 
# nox coefficient = -18.9706; 1 part per 10 million increase in nox (Nitrogen Oxide Concentration) ->18971 USD decrease in medv (median value of owner-occupied homes)
# rm coefficient = 8.1567; 1 number increase in rm (average number of rooms per dwelling) -> 8157 USD increase in medv.

#nox confidence interval = (-23.9942, -13.999233); 1 part per 10 million  increase in nox -> decreases medv by between 13,999 and 23,994 USD with 95% confidence.
#rm confidence interval = (7.33676, 8.976551); 1 more rm -> increased medv by between 7,337 and 8,977 USD with 95% confidence.


ggplot(df_3, aes(x = rm, y = medv, color = age)) +
  geom_point() + 
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "rm(#)",
       y = "medv (1000$)",
       color = "Age (%)")




# Question 4

library(MASS)
data("biopsy")

data <- biopsy[, -1]
data <- na.omit(data)
data

model <- glm(class~., data = data, family = binomial)
summary(model)

"
Call:
glm(formula = class ~ ., family = binomial, data = data)

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -10.10394    1.17488  -8.600  < 2e-16 ***
V1            0.53501    0.14202   3.767 0.000165 ***
V2           -0.00628    0.20908  -0.030 0.976039    
V3            0.32271    0.23060   1.399 0.161688    
V4            0.33064    0.12345   2.678 0.007400 ** 
V5            0.09663    0.15659   0.617 0.537159    
V6            0.38303    0.09384   4.082 4.47e-05 ***
V7            0.44719    0.17138   2.609 0.009073 ** 
V8            0.21303    0.11287   1.887 0.059115 .  
V9            0.53484    0.32877   1.627 0.103788    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 884.35  on 682  degrees of freedom
Residual deviance: 102.89  on 673  degrees of freedom
AIC: 122.89

Number of Fisher Scoring iterations: 8
"

# The model improve the performance (comparing the Null deviance with the Residual deviance) 
# V1, V4, V6, V7 had P-value < 0.05;indicating that these factors are statistically significant predictors. 
# V2, V3, V5, V8, V9 had P-value > 0.05; indicating that these factors are not statistically significant predictors. 


probs <- predict(model, type = "response")

preds <- ifelse(probs > 0.5, "malignant", "benign")
preds <- factor(preds, levels = c("benign", "malignant"))


cm <- table(Predicted = preds, Actual = data$class)
cm

"           Actual
Predicted   benign malignant
  benign       434        11
  malignant     10       228"

# TP (Predicted malignant where it was malignant) : 228
# TN (Predicted benign where it was benign) : 434
# FP (Predicted malignant where it was benign) : 10
# FN (Predicted benign where it was malignant) : 11

# The model shows almost good performance.


ggplot(data, aes(x = V1, y = V3, color = probs)) +
  geom_jitter(alpha = 0.5, size=3) +
  scale_color_gradient()
labs(x = "V1",
     y = "V3",
     color = "P(malignancy)")


# Question 5

data("NHANES")

data <- NHANES[, c("AlcoholDay", "Age", "Weight")]
data <- na.omit(data)
data

model <- lm(Weight ~ AlcoholDay + Age, data = data)
summary(model)

"
Call:
lm(formula = Weight ~ AlcoholDay + Age, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-43.407 -14.791  -2.279  11.548 138.836 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 78.66558    0.98096  80.193  < 2e-16 ***
AlcoholDay   0.67207    0.09485   7.086 1.58e-12 ***
Age          0.03962    0.01835   2.159   0.0309 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 20.46 on 4886 degrees of freedom
Multiple R-squared:  0.0102,	Adjusted R-squared:  0.009798 
F-statistic: 25.18 on 2 and 4886 DF,  p-value: 1.315e-11"

RMSE <- sqrt(mean(model$residuals ^ 2))
RMSE
"20.45692"

# RMSE = 20.45692; The model predict by 20.45692 units of error on average, performing moderately given the scale of Weights in the dataset. 
# R2 = 0.0102; The model can explain 1% of the variance in Weight values, 
# P-value < 0.05; The overall P-value and the P-value for each predictor is below the threshold, indicating that they are statistically significant predictors. 
# AlcoholDay coefficient = 0.67207; 1 unit increase in AlcoholDay -> 0.67207 increase in weight. 
# Age coefficient = 0.03962; 1 unit increase in Age -> 0.03962 unit increase in weight.

#including interactions:
model_in<- lm(Weight ~ Age * AlcoholDay, data = data)
summary(model_in)
"
Call:
lm(formula = Weight ~ Age * AlcoholDay, data = data)

Residuals:
    Min      1Q  Median      3Q     Max 
-44.895 -14.446  -2.378  11.390 138.048 

Coefficients:
                Estimate Std. Error t value Pr(>|t|)    
(Intercept)    81.612009   1.112590  73.353  < 2e-16 ***
Age            -0.045251   0.023835  -1.899   0.0577 .  
AlcoholDay     -0.590034   0.246124  -2.397   0.0166 *  
Age:AlcoholDay  0.038536   0.006938   5.554 2.94e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 20.4 on 4885 degrees of freedom
Multiple R-squared:  0.01641,	Adjusted R-squared:  0.01581 
F-statistic: 27.17 on 3 and 4885 DF,  p-value: < 2.2e-16"

RMSE <- sqrt(mean(model_in$residuals ^ 2))
RMSE
"20.39264"

# After considering the interactions R2 increases to 1.6% and RMSE decreases to 20.39264.

data$AlcoholBins <- cut(data$AlcoholDay,
                        breaks = c(-Inf, 2, 4, +Inf),
                        labels = c("1-2", "3-4", "+5"))

ggplot(data, aes(x = AlcoholBins, y = Weight)) +
  geom_boxplot() +
  labs(x = "Alcohol Bins",
       y = "Weight")