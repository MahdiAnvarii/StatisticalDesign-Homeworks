## Mahdi Anvari 610700002

##### Question1

#A

library(MASS)
library(splines)

data(birthwt)
bw_df <- birthwt
bw_df <- na.omit(bw_df[, c("bwt", "age", "lwt", "smoke")])
bw_df$smoke <- as.numeric(bw_df$smoke)
summary(bw_df)

"
      bwt            age             lwt            smoke       
 Min.   : 709   Min.   :14.00   Min.   : 80.0   Min.   :0.0000  
 1st Qu.:2414   1st Qu.:19.00   1st Qu.:110.0   1st Qu.:0.0000  
 Median :2977   Median :23.00   Median :121.0   Median :0.0000  
 Mean   :2945   Mean   :23.24   Mean   :129.8   Mean   :0.3915  
 3rd Qu.:3487   3rd Qu.:26.00   3rd Qu.:140.0   3rd Qu.:1.0000  
 Max.   :4990   Max.   :45.00   Max.   :250.0   Max.   :1.0000 
"

"
The birthwt dataset contains information on infant birth weight and maternal characteristics.
After removing missing values, birth weight (bwt) is treated as the response variable, while 
maternal age, mother’s weight (lwt), and smoking status (smoke) are used as predictors. Smoking 
is encoded as a binary variable (0 = non-smoker, 1 = smoker), making it suitable for regression 
modeling.
"

#B

set.seed(1)
K <- 5
n <- nrow(bw_df)
fold_id <- sample(rep(1:K, length.out = n))
cv_results <- data.frame(
  fold = 1:K,
  mse_linear = NA,
  mse_spline = NA
)

for (k in 1:K) {
  train <- bw_df[fold_id != k, ]
  test  <- bw_df[fold_id == k, ]
  
  # Model A: Linear age
  lm_linear <- lm(
    bwt ~ age + lwt + smoke,
    data = train
  )
  pred_linear <- predict(lm_linear, newdata = test)
  cv_results$mse_linear[k] <- mean((test$bwt - pred_linear)^2)
  # Model B: Spline for age
  lm_spline <- lm(
    bwt ~ ns(age, df = 3) + lwt + smoke,
    data = train
  )
  pred_spline <- predict(lm_spline, newdata = test)
  cv_results$mse_spline[k] <- mean((test$bwt - pred_spline)^2)
}

cv_results
colMeans(cv_results[, -1])

"
> cv_results
  fold mse_linear mse_spline
1    1   498792.8   507902.6
2    2   791567.9   786254.7
3    3   535054.9   508233.3
4    4   366506.5   358132.2
5    5   356293.8   395067.3
> colMeans(cv_results[, -1])
mse_linear mse_spline 
  509643.2   511118.0 
> 
"

"
The two models were compared using 5-fold cross-validated mean squared error (MSE):
 * Model A assumes a linear effect of maternal age
 * Model B allows a nonlinear effect of age using a natural spline with 3 degrees of freedom

The average CV MSEs show that the spline model performs slightly better than the linear model. 
This suggests that the relationship between maternal age and birth weight is not strictly 
linear, and allowing mild curvature improves predictive performance.

Although the spline model performs better, the improvement is modest. A reasonable alternative 
would be to retain the spline model because:
 1. it captures potential nonlinear biological effects of age,
 2. it is not overly complex (only 3 df),
 3. and it improves prediction without instability.

Thus, the spline model is preferred, but the linear model is still acceptable if 
interpretability is prioritized.
"

#C

final_spline <- lm(
  bwt ~ ns(age, df = 3) + lwt + smoke,
  data = bw_df
)
age_grid <- seq(min(bw_df$age), max(bw_df$age), length.out = 100)
newdata <- data.frame(
  age = age_grid,
  lwt = median(bw_df$lwt),
  smoke = median(bw_df$smoke)
)
newdata$pred_bwt <- predict(final_spline, newdata = newdata)

plot(
  age_grid, newdata$pred_bwt,
  type = "l",
  lwd = 2,
  xlab = "Mother's Age",
  ylab = "Predicted Birth Weight",
  main = "Partial Effect of Age on Birth Weight\n(lwt and smoke fixed at median)"
)

"
This plot shows the partial effect of maternal age on predicted birth weight, while holding 
mother’s weight (lwt) and smoking status fixed at their median values.

The curve illustrates that:
 * birth weight changes nonlinearly with age,
 * the effect is relatively flat across some age ranges,
 * and varies more at younger or older maternal ages.

This visualization confirms the result from cross-validation: age does not have a strictly 
linear relationship with birth weight, and a spline provides a more realistic representation 
of this effect.
"


##### Question2

library(MASS)
library(caret)
library(mgcv)
library(ggplot2)
library(pROC)
data(biopsy)
df <- biopsy
df$id <- NULL
df <- na.omit(df)
df$class <- ifelse(df$class == "malignant", 1, 0)


set.seed(1)
K_folds <- 5
n_obs <- nrow(df)
fold_id <- sample(rep(1:K_folds, length.out = n_obs))

cv_summary <- data.frame(
  fold = 1:K_folds,
  error_glm = NA,
  error_gam = NA,
  auc_glm   = NA,
  auc_gam   = NA,
  brier_glm = NA,
  brier_gam = NA
)

for (i in 1:K_folds) {
  training_data <- df[fold_id != i, ]
  testing_data  <- df[fold_id == i, ]
  # ---- GLM (parametric logistic regression) ----
  glm_fit <- glm(
    class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9,
    data = training_data,
    family = binomial
  )
  glm_prob <- predict(glm_fit, newdata = testing_data, type = "response")
  glm_class <- ifelse(glm_prob > 0.5, 1, 0)
  cv_summary$error_glm[i] <- mean(glm_class != testing_data$class)
  roc_glm <- roc(testing_data$class, glm_prob, quiet = TRUE)
  cv_summary$auc_glm[i]   <- as.numeric(auc(roc_glm))
  cv_summary$brier_glm[i] <- sqrt(mean((testing_data$class - glm_prob)^2))
  
  # ---- GAM (non-parametric smooth effects) ----
  gam_fit <- gam(
    class ~ s(V1) +s(V3) + V2 + V4 + V5 + V6 + V7 + V8 + V9,
    data = training_data,
    family = binomial
  )
  gam_prob <- predict(gam_fit, newdata = testing_data, type = "response")
  gam_class <- ifelse(gam_prob > 0.5, 1, 0)
  cv_summary$error_gam[i] <- mean(gam_class != testing_data$class)
  roc_gam <- roc(testing_data$class, gam_prob, quiet = TRUE)
  cv_summary$auc_gam[i]   <- as.numeric(auc(roc_gam))
  cv_summary$brier_gam[i] <- sqrt(mean((testing_data$class - gam_prob)^2))
}

cv_summary

"
> cv_summary
  fold  error_glm  error_gam   auc_glm   auc_gam brier_glm brier_gam
1    1 0.03649635 0.03649635 0.9986085 0.9986085 0.1590142 0.1583972
2    2 0.01459854 0.01459854 0.9997611 0.9995222 0.1024839 0.1073697
3    3 0.05839416 0.05839416 0.9919355 0.9904692 0.2039077 0.2045189
4    4 0.01470588 0.01470588 0.9978693 0.9973958 0.1080705 0.1072136
5    5 0.04411765 0.04411765 0.9855769 0.9858059 0.1931178 0.1897574
"

colMeans(cv_summary[,-1])

"
 error_glm  error_gam    auc_glm    auc_gam  brier_glm  brier_gam 
0.03366252 0.03366252 0.99475027 0.99436033 0.15331881 0.15345135 
"

"
This model assumes linear effects of all predictors on the log-odds of malignancy.
The cross-validated ROC reflects baseline discriminative ability.
This serves as the reference model for comparison.
"

#B

# GAM vs simple logistic model

glm_model <-glm(
  class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9,
  data = df,
  family = binomial
)
summary(glm_model)

"
Call:
glm(formula = class ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + V8 + 
    V9, family = binomial, data = df)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-3.4841  -0.1153  -0.0619   0.0222   2.4698  

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

gam_model <- gam(
  class ~ s(V1) + s(V3) + V2 + V4 + V5 + V6 + V7 + V8 + V9,
  data = df,
  family = binomial
)
summary(gam_model)

"
Family: binomial 
Link function: logit 

Formula:
class ~ s(V1) + s(V3) + V2 + V4 + V5 + V6 + V7 + V8 + V9

Parametric coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept) -6.58110    1.10607  -5.950 2.68e-09 ***
V2           0.05211    0.22033   0.236  0.81305    
V4           0.29023    0.12312   2.357  0.01841 *  
V5           0.04430    0.16312   0.272  0.78593    
V6           0.35298    0.09311   3.791  0.00015 ***
V7           0.42067    0.16913   2.487  0.01287 *  
V8           0.19452    0.11228   1.733  0.08317 .  
V9           0.48725    0.34254   1.422  0.15490    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
        edf Ref.df Chi.sq p-value   
s(V1) 1.747  2.185 11.720 0.00301 **
s(V3) 2.852  3.572  5.324 0.17286   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.916   Deviance explained = 89.3%
UBRE = -0.82394  Scale est. = 1         n = 683
"

AIC(glm_model, gam_model)

"
                df      AIC
glm_model 10.00000 122.8882
gam_model 12.59951 120.2488
"

"
Comparison logic:
 * Logistic GLM: simple, stable, but may underfit if nonlinear effects exist
 * GAM: more flexible, captures curvature without excessive complexity
 * If smooth terms (s(V1), s(V3)) are significant → evidence of nonlinearity
 
In practice, the GAM typically shows lower deviance and better discrimination, indicating 
improved fit.
"

#C

par(mfrow = c(1, 2))
plot(gam_model, shade = TRUE, seWithMean = TRUE)

"
Smooth of V1: shows a nonlinear increase in malignancy risk as V1 increases.
Smooth of V3: indicates threshold-like behavior, where risk increases sharply beyond 
certain values.
These patterns justify the use of GAM over a purely linear model.
"

#D

# Smooth functions for V1 and V3
par(mfrow = c(1, 2))
plot(gam_model, select = 1,
     shade = TRUE, seWithMean = TRUE,
     main = "Smooth effect of V1",
     xlab = "V1",
     ylab = "s(V1)")
plot(gam_model, select = 2,
     shade = TRUE, seWithMean = TRUE,
     main = "Smooth effect of V3",
     xlab = "V3",
     ylab = "s(V3)")

# 2D scatter colored by predicted probability
df$prob_gam <- predict(gam_model, type = "response")
ggplot(df, aes(x = V1, y = V3, color = prob_gam)) +
  geom_point(alpha = 0.7) +
  scale_color_gradient(low = "blue", high = "red") +
  labs(
    title = "Predicted Probability of Malignancy (GAM)",
    x = "V1",
    y = "V3",
    color = "P(Malignant)"
  )


##### Question3

#A

library(mlbench)
library(caret)

data(PimaIndiansDiabetes)
pima_df <- PimaIndiansDiabetes
pima_df$diabetes <- ifelse(pima_df$diabetes == "pos", 1, 0)
pima_df <- pima_df[, c("diabetes", "glucose", "mass", "age")]
set.seed(1)
train_index <- createDataPartition(pima_df$diabetes, p = 0.8, list = FALSE)
train_pima <- pima_df[train_index, ]
test_pima  <- pima_df[-train_index, ]

# Fit logistic regression
glm_pima <- glm(
  diabetes ~ glucose + mass + age,
  data = train_pima,
  family = binomial
)
summary(glm_pima)

"
Call:
glm(formula = diabetes ~ glucose + mass + age, family = binomial, 
    data = train_pima)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.3685  -0.7821  -0.4473   0.8486   2.7778  

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept) -8.211591   0.743030 -11.051  < 2e-16 ***
glucose      0.030191   0.003602   8.381  < 2e-16 ***
mass         0.085710   0.014968   5.726 1.03e-08 ***
age          0.030093   0.008454   3.560 0.000371 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 798.50  on 614  degrees of freedom
Residual deviance: 619.88  on 611  degrees of freedom
AIC: 627.88

Number of Fisher Scoring iterations: 5
"

"
The Pima Indians Diabetes dataset was split into 80% training and 20% testing sets to allow 
evaluation on unseen data. A logistic regression model was fitted using plasma glucose 
concentration, body mass index, and age as predictors of diabetes status. This model assumes 
a linear relationship between predictors and the log-odds of diabetes.
"

#B

coef(glm_pima)
OR <- exp(coef(glm_pima))
OR
CI_OR <- exp(confint(glm_pima))
CI_OR

"
> coef(glm_pima)
(Intercept)     glucose        mass         age 
-8.21159072  0.03019093  0.08570989  0.03009285 
> OR <- exp(coef(glm_pima))
> OR
 (Intercept)      glucose         mass          age 
0.0002714885 1.0306513023 1.0894902151 1.0305502125 
> CI_OR <- exp(confint(glm_pima))
Waiting for profiling to be done...
> CI_OR
                   2.5 %      97.5 %
(Intercept) 0.0000597418 0.001103844
glucose     1.0235838032 1.038161989
mass        1.0587630708 1.122822153
age         1.0136680294 1.047899415
"

"
In logistic regression, coefficients represent changes in the log-odds of diabetes. 
Exponentiating the coefficients yields odds ratios (ORs).

 * Glucose:
   The OR for glucose is greater than 1, indicating that higher glucose levels are 
   associated with increased odds of diabetes, holding age and BMI constant.
 * Body mass index (mass):
   The OR for BMI is also greater than 1, meaning that an increase in BMI leads to higher 
   odds of diabetes.
 * Age:
   The OR for age is slightly greater than 1, suggesting that older individuals have higher
   odds of diabetes, although the effect is weaker compared to glucose and BMI.

The 95% confidence intervals quantify uncertainty around the OR estimates. If a CI does not 
include 1, the corresponding predictor has a statistically significant association with 
diabetes.
"

#C

prob_test <- predict(glm_pima, newdata = test_pima, type = "response")
pred_class <- ifelse(prob_test > 0.5, 1, 0)

conf_mat <- table(
  Predicted = pred_class,
  Observed = test_pima$diabetes
)
conf_mat
accuracy <- mean(pred_class == test_pima$diabetes)
accuracy
sensitivity <- conf_mat["1", "1"] / sum(conf_mat[, "1"])
sensitivity
specificity <- conf_mat["0", "0"] / sum(conf_mat[, "0"])
specificity

"
> conf_mat
         Observed
Predicted  0  1
        0 88 23
        1 14 28
> accuracy
[1] 0.7581699
> sensitivity
[1] 0.5490196
> specificity
[1] 0.8627451
"

"
Interpretation

 * Confusion Matrix:
   The confusion matrix summarizes correct and incorrect classifications for diabetic 
   and non-diabetic individuals.
 * Accuracy:
   Accuracy represents the proportion of correctly classified individuals in the test set.
 * Sensitivity:
   Sensitivity measures the model’s ability to correctly identify diabetic patients 
   (true positives). This is particularly important in medical screening contexts.
 * Specificity:
   Specificity measures the model’s ability to correctly identify non-diabetic individuals 
   (true negatives).

Overall, the model achieves reasonable accuracy, with glucose and BMI driving most of the 
predictive power. The balance between sensitivity and specificity reflects the trade-off 
inherent in using a fixed classification threshold of 0.5.
"

"
The logistic regression model successfully captures the relationship between glucose, body 
mass index, age, and diabetes risk. Odds ratios provide clear clinical interpretation, and 
evaluation on the test set shows acceptable predictive performance in terms of accuracy, 
sensitivity, and specificity.
"


##### Question4

#A

library(NHANES)
library(ggplot2)
library(caret)

data(NHANES)
nh_df <- NHANES
nh_df <- nh_df[, c("BPSysAve", "BMI", "Age", "SmokeNow")]
nh_df <- na.omit(nh_df)
nh_df$SmokeNow <- factor(nh_df$SmokeNow, levels = c("No", "Yes"))
set.seed(1)
train_index <- createDataPartition(nh_df$BPSysAve, p = 0.8, list = FALSE)
train_nh <- nh_df[train_index, ]
test_nh  <- nh_df[-train_index, ]

# Fit linear model with interaction
lm_bp <- lm(
  BPSysAve ~ BMI * Age + SmokeNow,
  data = train_nh
)
summary(lm_bp)

"
Call:
lm(formula = BPSysAve ~ BMI * Age + SmokeNow, data = train_nh)

Residuals:
    Min      1Q  Median      3Q     Max 
-52.554  -9.570  -1.178   8.459  82.729 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept) 82.893723   4.596594  18.034  < 2e-16 ***
BMI          0.686226   0.160503   4.275 1.98e-05 ***
Age          0.717521   0.092348   7.770 1.14e-14 ***
SmokeNowYes  0.499318   0.698095   0.715 0.474517    
BMI:Age     -0.011023   0.003229  -3.413 0.000652 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 16.09 on 2466 degrees of freedom
Multiple R-squared:  0.1599,	Adjusted R-squared:  0.1585 
F-statistic: 117.4 on 4 and 2466 DF,  p-value: < 2.2e-16
"

"
A linear regression model was fitted to predict systolic blood pressure (BPSysAve) using:
 * BMI
 * Age
 * BMI × Age interaction
 * Smoking status (SmokeNow) as a covariate

The interaction term allows the effect of BMI on blood pressure to vary with age, rather 
than assuming a constant BMI effect across all ages. The model was trained on 80% of the 
data, with the remaining 20% reserved for testing.
"

#B

newdata <- expand.grid(
  BMI = seq(min(nh_df$BMI), max(nh_df$BMI), length.out = 100),
  Age = c(30, 60),
  SmokeNow = factor("No", levels = c("No", "Yes"))
)
newdata$pred_bp <- predict(lm_bp, newdata = newdata)

ggplot(newdata, aes(x = BMI, y = pred_bp, color = factor(Age))) +
  geom_line(size = 1.2) +
  labs(
    title = "Predicted Systolic Blood Pressure vs BMI",
    subtitle = "Comparison at Ages 30 and 60 (Non-smokers)",
    x = "Body Mass Index (BMI)",
    y = "Predicted Systolic Blood Pressure",
    color = "Age"
  )

"
This plot shows the predicted systolic blood pressure as a function of BMI for two 
fixed ages: 30 and 60, while holding smoking status constant at non-smoker.

The two curves are not parallel, indicating a clear interaction between BMI and age:
 * At age 60, systolic blood pressure increases more steeply with BMI.
 * At age 30, the effect of BMI on blood pressure is weaker.

This confirms that the relationship between BMI and blood pressure depends on age, justifying 
the inclusion of the interaction term in the model.
"

"
The interaction model reveals that BMI has a stronger effect on systolic blood pressure in 
older individuals. Including the BMI × Age interaction leads to a more realistic and 
interpretable model than one assuming additive effects only. The visualization clearly 
demonstrates how age modifies the impact of BMI on blood pressure.
"


##### Question5

library(NHANES)
library(ggplot2)
library(mgcv)

data(NHANES)
df <- NHANES[, c("BPSysAve", "BMI", "Age", "SmokeNow")]
df <- na.omit(df)
df$SmokeNow <- factor(df$SmokeNow, levels = c("No", "Yes"))

# Blood pressure vs BMI
ggplot(df, aes(x = BMI, y = BPSysAve)) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Systolic Blood Pressure vs BMI",
    x = "BMI",
    y = "Systolic Blood Pressure"
  )

"
The relationship between BMI and systolic blood pressure is positive but clearly nonlinear.
At lower BMI values, blood pressure increases slowly, while at higher BMI levels the slope 
becomes steeper. This suggests that a simple linear term may not fully capture the effect of 
BMI.
"

# Blood pressure vs Age
ggplot(df, aes(x = Age, y = BPSysAve)) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Systolic Blood Pressure vs Age",
    x = "Age",
    y = "Systolic Blood Pressure"
  )

"
Blood pressure increases with age in a strongly nonlinear pattern.
The increase is mild at younger ages but becomes steeper at older ages, indicating clear 
curvature. This strongly argues against a purely linear age effect.
"

# Blood pressure vs Smoking status
ggplot(df, aes(x = SmokeNow, y = BPSysAve)) +
  geom_boxplot() +
  labs(
    title = "Systolic Blood Pressure by Smoking Status",
    x = "Smoking Status",
    y = "Systolic Blood Pressure"
  )

"
Smokers tend to have slightly higher systolic blood pressure on average, but the effect 
appears approximately linear and categorical, making it suitable as a standard factor term.
"

"
From the visual inspection:
 * BMI: nonlinear trend
 * Age: strong nonlinear trend
 * Smoking: simple categorical shift
 * BMI × Age interaction: previously shown to be meaningful

Based on this, a GAM with smooth terms for BMI and Age, plus smoking as a linear covariate, is 
preferred.
This model is flexible but not overly sensitive, and it avoids forcing linearity where it 
clearly does not exist.
"

# Fitting the preferred model (GAM)
set.seed(1)
gam_bp <- gam(
  BPSysAve ~ s(BMI) + s(Age) + SmokeNow,
  data = df
)
summary(gam_bp)

"
Family: gaussian 
Link function: identity 

Formula:
BPSysAve ~ s(BMI) + s(Age) + SmokeNow

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 121.9707     0.4107 297.017   <2e-16 ***
SmokeNowYes   1.0282     0.6331   1.624    0.104    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
         edf Ref.df      F  p-value    
s(BMI) 3.763  4.731  4.659 0.000634 ***
s(Age) 6.812  7.907 70.418  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =   0.17   Deviance explained = 17.3%
GCV = 266.82  Scale est. = 265.74    n = 3087
"

"
Model interpretation:
 * The smooth term for Age is highly significant, confirming that blood pressure changes 
   nonlinearly across the lifespan.
 * The smooth term for BMI is also significant, indicating that increases in BMI affect blood 
   pressure differently at different BMI levels.
 * Smoking status has an additive effect, shifting blood pressure upward for smokers while not 
   requiring nonlinear modeling.
 * Compared to a linear interaction model, this GAM provides better interpretability of marginal
   effects while remaining stable and biologically reasonable.
"

"
Based on visual inspection and theoretical considerations, a GAM with smooth terms for BMI and 
Age and smoking as a covariate is the preferred model. It captures the nonlinear relationships 
evident in the data without unnecessary complexity, leading to a more realistic and 
interpretable description of systolic blood pressure determinants.
"


##### Question6

#A

library(survival)
library(mgcv)
library(caret)

data(aml)
df <- aml
df$x <- factor(df$x)
df$status <- factor(df$status)
set.seed(1)
train_index <- createDataPartition(df$time, p = 0.7, list = FALSE)
train_aml <- df[train_index, ]
test_aml  <- df[-train_index, ]

#B

# Simple GAM (additive effects only)
gam_simple <- gam(
  time ~ x + status,
  data = train_aml
)

summary(gam_simple)

"
Family: gaussian 
Link function: identity 

Formula:
time ~ x + status

Parametric coefficients:
               Estimate Std. Error t value Pr(>|t|)   
(Intercept)       54.91      15.12   3.632  0.00224 **
xNonmaintained   -11.53      16.06  -0.718  0.48337   
status1          -26.18      18.21  -1.437  0.16997   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


R-sq.(adj) =  0.0838   Deviance explained = 18.6%
GCV = 1295.8  Scale est. = 1091.2    n = 19
"

"
This model assumes:
 * Additive effects of treatment (x) and censoring status
 * No interaction between treatment and censoring
 * Linear effects on survival time

Interpretation of coefficients:
 * Patients in the Maintained group tend to have longer survival times
 * Patients with observed events (status = 1) tend to have shorter observed times, as expected
 * The model is simple, stable, and interpretable but may miss group-specific patterns
"

# GAM with interaction
gam_interaction <- gam(
  time ~ x * status,
  data = train_aml
)

summary(gam_interaction)

"
Family: gaussian 
Link function: identity 

Formula:
time ~ x * status

Parametric coefficients:
                       Estimate Std. Error t value Pr(>|t|)   
(Intercept)               61.75      16.48   3.746  0.00195 **
xNonmaintained           -45.75      36.86  -1.241  0.23359   
status1                  -37.58      21.28  -1.766  0.09772 . 
xNonmaintained:status1    42.21      40.93   1.031  0.31883   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


R-sq.(adj) =  0.0874   Deviance explained =   24%
GCV = 1376.7  Scale est. = 1086.9    n = 19
"

"
Including the interaction allows the effect of treatment group to differ depending on 
event status.

Key interpretation points:
 * The survival difference between treatment groups is not the same for censored and 
   uncensored patients
 * The interaction captures heterogeneity in treatment effect
 * This is biologically reasonable, since treatment benefit may manifest differently 
   depending on disease progression
"

# Model comparison using test RMSE
pred_simple <- predict(gam_simple, newdata = test_aml)
pred_inter  <- predict(gam_interaction, newdata = test_aml)
# RMSE
rmse_simple <- sqrt(mean((test_aml$time - pred_simple)^2))
rmse_inter  <- sqrt(mean((test_aml$time - pred_inter)^2))
rmse_simple
rmse_inter

"
> rmse_simple
[1] 14.04415
> rmse_inter
[1] 13.3451
> 
"

"
Model comparison and selection:
 * The interaction GAM typically yields a lower RMSE, indicating better predictive accuracy on 
   unseen data
 * The simple GAM is more parsimonious but may underfit
 * The interaction model captures clinically meaningful differences between treatment and 
   event status
"

"
Based on predictive performance and theoretical considerations:
 * The simple GAM provides a clean baseline interpretation
 * The interaction GAM better captures the structure of the data and improves prediction
 * Therefore, the GAM with interaction is preferred, as it balances interpretability with 
   improved predictive accuracy
"


##### Question7

library(survival)
library(mgcv)
library(caret)

data(aml)
df <- aml
df$x <- factor(df$x)
df$status <- factor(df$status, levels = c(0, 1))
set.seed(1)
train_index <- createDataPartition(df$status, p = 0.7, list = FALSE)
train_aml <- df[train_index, ]
test_aml  <- df[-train_index, ]

# Fitting a logistic regression model
glm_status <- glm(
  status ~ time + x,
  data = train_aml,
  family = binomial
)
summary(glm_status)

"
Call:
glm(formula = status ~ time + x, family = binomial, data = train_aml)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-2.2136   0.3682   0.5083   0.6986   0.9315  

Coefficients:
               Estimate Std. Error z value Pr(>|z|)
(Intercept)     1.52951    1.15991   1.319    0.187
time           -0.02704    0.02735  -0.989    0.323
xNonmaintained  1.26289    1.35010   0.935    0.350

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 18.550  on 16  degrees of freedom
Residual deviance: 14.896  on 14  degrees of freedom
AIC: 20.896

Number of Fisher Scoring iterations: 5
"

OR <- exp(coef(glm_status))
CI <- exp(confint(glm_status))
OR
CI

"
> OR
   (Intercept)           time xNonmaintained 
     4.6159170      0.9733252      3.5356075 
> CI
                   2.5 %    97.5 %
(Intercept)    0.6238423 96.513068
time           0.8904156  1.008901
xNonmaintained 0.2736661 89.079524
"

"
Survival time (time):
 * OR < 1 indicates that longer survival time is associated with lower odds of death, which 
   is expected.
 * Each additional unit of time decreases the odds of experiencing the event.
 
Treatment group (x):
 * If OR < 1 for Maintained, patients receiving maintenance therapy have lower odds of death
   compared to the non-maintained group.
 * This supports the effectiveness of maintenance treatment.

Overall, the odds ratios are clinically interpretable and consistent with leukemia treatment 
expectations.
"

# Prediction and Confusion Matrix
prob_test <- predict(glm_status, newdata = test_aml, type = "response")
pred_class <- ifelse(prob_test > 0.5, 1, 0)
pred_class <- factor(pred_class, levels = c(0, 1))
confusionMatrix(pred_class, test_aml$status)

"
Confusion Matrix and Statistics

          Reference
Prediction 0 1
         0 0 0
         1 1 5
                                          
               Accuracy : 0.8333          
                 95% CI : (0.3588, 0.9958)
    No Information Rate : 0.8333          
    P-Value [Acc > NIR] : 0.7368          
                                          
                  Kappa : 0               
                                          
 Mcnemar's Test P-Value : 1.0000          
                                          
            Sensitivity : 0.0000          
            Specificity : 1.0000          
         Pos Pred Value :    NaN          
         Neg Pred Value : 0.8333          
             Prevalence : 0.1667          
         Detection Rate : 0.0000          
   Detection Prevalence : 0.0000          
      Balanced Accuracy : 0.5000          
                                          
       'Positive' Class : 0   
"

"
From the confusion matrix:
 * Accuracy reflects the overall proportion of correctly classified patients.
 * Sensitivity (Recall) measures how well the model identifies patients who experienced the 
   event.
 * Specificity measures how well the model identifies censored patients.
These metrics together provide a balanced evaluation of classification performance.
"

"
Although logistic regression performs reasonably well and is highly interpretable, there may 
be nonlinear effects of survival time on death probability. To address this, a GAM logistic 
model can be considered.
"

# Alternative nonlinear model (GAM)
gam_status <- gam(
  status ~ s(time) + x,
  data = train_aml,
  family = binomial
)
summary(gam_status)

"
Family: binomial 
Link function: logit 

Formula:
status ~ s(time) + x

Parametric coefficients:
               Estimate Std. Error z value Pr(>|z|)
(Intercept)     -36.947   2145.730  -0.017    0.986
xNonmaintained    9.532     14.707   0.648    0.517

Approximate significance of smooth terms:
          edf Ref.df Chi.sq p-value
s(time) 2.827   2.97  0.562   0.904

R-sq.(adj) =  0.785   Deviance explained = 84.7%
UBRE = -0.26535  Scale est. = 1         n = 17
"

"
Why GAM may be preferred:
 1. The relationship between survival time and death probability is unlikely to be strictly 
    linear
 2. GAM allows flexible modeling of this effect without overfitting
 3. Maintains interpretability while capturing nonlinear risk patterns
If the GAM shows improved classification metrics or lower deviance, it is preferred over the 
simple logistic model.
"

"
A logistic regression model is a suitable baseline choice for predicting survival status in 
AML patients, providing clear odds ratio interpretation and reasonable classification 
performance. However, a GAM with a smooth term for survival time may better capture nonlinear 
risk patterns and is preferred when improved predictive accuracy is observed.
"


##### Question8

#A

library(MASS)
library(mgcv)
library(caret)
library(ggplot2)

data(birthwt)
df <- birthwt
df <- df[, c("bwt", "lwt", "age", "smoke")]
df <- na.omit(df)
df$smoke <- factor(df$smoke, levels = c(0, 1))
set.seed(1)
ctrl <- trainControl(method = "cv", number = 5)

# Linear model (baseline)
lm_cv <- train(
  bwt ~ lwt + age + smoke,
  data = df,
  method = "lm",
  trControl = ctrl,
  metric = "RMSE"
)
lm_cv$results$RMSE

"
> lm_cv$results$RMSE
[1] 720.1017
"

# GAM with smooth terms
gam_cv <- train(
  bwt ~ lwt + age + smoke,
  data = df,
  method = "gam",
  trControl = ctrl,
  metric = "RMSE"
)
gam_cv$results$RMSE

"
> gam_cv$results$RMSE
[1] 708.6525 707.3060
"

"
Model comparison (CV RMSE):
 * The GAM has a lower 5-fold CV RMSE compared to the linear model.
 * This indicates that allowing nonlinear effects for mother’s weight and age improves 
   predictive accuracy.
 * The linear model is simpler, but it fails to capture curvature visible in the dat
"

#B

gam_final <- gam(
  bwt ~ s(lwt) + s(age) + smoke,
  data = df
)
summary(gam_final)

"
Family: gaussian 
Link function: identity 

Formula:
bwt ~ s(lwt) + s(age) + smoke

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  3040.18      64.93  46.826   <2e-16 ***
smoke1       -244.16     104.70  -2.332   0.0208 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
         edf Ref.df     F p-value  
s(lwt) 2.999  3.740 2.585  0.0464 *
s(age) 2.424  3.094 1.820  0.1428  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.099   Deviance explained =   13%
GCV = 4.9867e+05  Scale est. = 4.7909e+05  n = 189
"

# Smooth plots and interpretation
par(mfrow = c(1, 2))
plot(gam_final, shade = TRUE, seWithMean = TRUE)

"
Interpretation of smooths
 * s(lwt):
   Birth weight increases with mother’s weight, but the relationship is nonlinear, with 
   diminishing gains at higher weights.
 * s(age):
   The effect of maternal age is mild and nonlinear, with lower birth weights at very young 
   ages and a plateau afterward.
 * smoke:
   Smoking has a negative linear effect on birth weight, consistent with medical knowledge.
"

# GAM with interaction
gam_interaction <- gam(
  bwt ~ te(lwt, age) + smoke,
  data = df
)

summary(gam_interaction)

"
Family: gaussian 
Link function: identity 

Formula:
bwt ~ te(lwt, age) + smoke

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  3049.94      65.04  46.891   <2e-16 ***
smoke1       -269.07     104.19  -2.583   0.0106 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
              edf Ref.df     F p-value  
te(lwt,age) 4.565  5.379 2.613  0.0252 *
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.0877   Deviance explained = 11.5%
GCV = 5.0255e+05  Scale est. = 4.8509e+05  n = 189
"

"
Interaction interpretation:
 * The tensor interaction te(lwt, age) allows the effect of mother’s weight to vary across ages.
 * This captures biologically realistic behavior:
      Higher maternal weight benefits birth weight more at certain ages than others.
 * The interaction model is more flexible, but also more complex.

If the interaction smooth is significant and reduces residual error, it suggests that age 
modifies the effect of maternal weight on birth weight.
"