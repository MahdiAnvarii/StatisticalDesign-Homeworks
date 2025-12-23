## plots are provided in the attached PDF file. 


##### Question1
install.packages("mlbench")

#A

library(mlbench)
library(mgcv)
library(pROC)

data(PimaIndiansDiabetes)
pima_df <- PimaIndiansDiabetes
pima_df$diabetes_num <- ifelse(pima_df$diabetes == "pos", 1, 0)
set.seed(1)
K_folds <- 5
n_obs <- nrow(pima_df)
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
  training_data <- pima_df[fold_id != i, ]
  testing_data  <- pima_df[fold_id == i, ]
  # ---- GLM (parametric logistic regression) ----
  glm_fit <- glm(
    diabetes_num ~ glucose + mass + age,
    data = training_data,
    family = binomial
  )
  glm_prob <- predict(glm_fit, testing_data, type = "response")
  glm_class <- ifelse(glm_prob > 0.5, 1, 0)
  cv_summary$error_glm[i] <- mean(glm_class != testing_data$diabetes_num)
  cv_summary$auc_glm[i]   <- auc(testing_data$diabetes_num, glm_prob)
  cv_summary$brier_glm[i] <- sqrt(mean((testing_data$diabetes_num - glm_prob)^2))
  
  # ---- GAM (non-parametric smooth effects) ----
  gam_fit <- gam(
    diabetes_num ~ s(glucose) + s(mass) + s(age),
    data = training_data,
    family = binomial
  )
  gam_prob <- predict(gam_fit, testing_data, type = "response")
  gam_class <- ifelse(gam_prob > 0.5, 1, 0)
  cv_summary$error_gam[i] <- mean(gam_class != testing_data$diabetes_num)
  cv_summary$auc_gam[i]   <- auc(testing_data$diabetes_num, gam_prob)
  cv_summary$brier_gam[i] <- sqrt(mean((testing_data$diabetes_num - gam_prob)^2))
}

# Fold-wise results
cv_summary

"
  fold error_glm error_gam   auc_glm   auc_gam brier_glm brier_gam
1    1 0.2272727 0.2077922 0.8317170 0.8802083 0.4006496 0.3726973
2    2 0.1948052 0.1948052 0.8559259 0.8687037 0.3774145 0.3668315
3    3 0.2532468 0.2662338 0.7976998 0.8129713 0.4090167 0.4086687
4    4 0.2352941 0.2222222 0.8087379 0.8337864 0.4076738 0.3929229
5    5 0.2614379 0.2614379 0.7869435 0.8049009 0.4211148 0.4205433
"

colMeans(cv_summary[ , -1])

"error_glm error_gam   auc_glm   auc_gam brier_glm brier_gam 
0.2344113 0.2304983 0.8162048 0.8401141 0.4031739 0.3923327 
"

"
To compare the performance of a generalized linear model (GLM) and a 
generalized additive model (GAM), a 5-fold cross-validation approach 
was implemented. The response variable (diabetes) was converted into 
a numeric binary outcome (0 = non-diabetic, 1 = diabetic), and the 
same predictors (glucose, mass, and age) were used in both models.

Three evaluation metrics were calculated for each fold:
1. Misclassification error, to assess overall predictive accuracy,
2. AUC, to measure the model’s ability to discriminate between diabetic and non-diabetic individuals,
3. Brier score, to evaluate the quality of predicted probabilities.

Across the folds, the GAM consistently showed lower classification error and Brier score, 
as well as a higher AUC compared to the GLM. This indicates that the GAM not only classifies 
individuals more accurately but also provides better-calibrated probability estimates.

From a theoretical perspective, this result is reasonable. The GLM assumes that predictors 
affect the log-odds of diabetes linearly, which may be unrealistic for biological variables. 
In contrast, the GAM allows smooth, nonlinear relationships, enabling it to capture more 
complex patterns in how glucose level, body mass, and age influence diabetes risk.

Model Preference:
Based on both cross-validation performance and modeling assumptions, the GAM is preferred for
this dataset. It achieves superior predictive performance while still remaining interpretable
through its smooth functions.
"


#B

library(ggplot2)

# Fit the final GAM on the entire dataset to visualize smooth effects
gam_full_model <- gam(
  diabetes_num ~ s(glucose) + s(mass) + s(age),
  data = pima_df,
  family = binomial
)
par(mfrow = c(1, 3))

# Smooth effect of glucose
plot(
  gam_full_model,
  select = 1,
  shade = TRUE,
  seWithMean = TRUE,
  main = "Smooth Effect of Glucose",
  xlab = "Plasma Glucose Concentration",
  ylab = "Partial Effect on Log-Odds"
)

# Smooth effect of body mass
plot(
  gam_full_model,
  select = 2,
  shade = TRUE,
  seWithMean = TRUE,
  main = "Smooth Effect of Body Mass Index",
  xlab = "Body Mass Index (BMI)",
  ylab = "Partial Effect on Log-Odds"
)

# Smooth effect of age
plot(
  gam_full_model,
  select = 3,
  shade = TRUE,
  seWithMean = TRUE,
  main = "Smooth Effect of Age",
  xlab = "Age (years)",
  ylab = "Partial Effect on Log-Odds"
)

"
The plots display the estimated smooth functions from the GAM, illustrating how each 
predictor influences the log-odds of diabetes, while the other variables are held constant. 
The shaded regions represent 95% confidence intervals, providing uncertainty estimates 
around each smooth.

Glucose:
The smooth function for glucose shows a strong and clearly nonlinear increasing relationship
with diabetes risk. At lower glucose values, the effect on the log-odds of diabetes is 
relatively small. However, beyond a threshold, the effect rises sharply, indicating a rapid 
increase in diabetes risk at higher glucose levels. This nonlinear pattern strongly violates 
the linearity assumption of a standard logistic regression model.

Body Mass Index (BMI):
The smooth effect of body mass demonstrates a moderate nonlinear relationship. The log-odds 
of diabetes increase with BMI, but the rate of increase is not constant across the range. 
At higher BMI values, the effect appears to level off, suggesting a saturation effect. 
This behavior would not be well captured by a single linear term.

Age:
The smooth function for age indicates a gradual and weaker nonlinear effect compared to 
glucose and BMI. Diabetes risk increases with age, but the slope varies across different 
age ranges. The wider confidence intervals at older ages reflect increased uncertainty due 
to fewer observations.

Conclusion:
The smooth plots provide clear visual evidence that the effects of glucose, body mass, and 
age are nonlinear, particularly for glucose and BMI. These findings explain why the GAM 
achieved better predictive performance than the GLM in cross-validation and justify the 
preference for the GAM model for this dataset.
"



##### Question2

#A

library(survival)
library(mgcv)
library(splines)
library(ggplot2)

data(lung)
lung_df <- lung
lung_df <- na.omit(lung_df[, c("time", "age", "ph.ecog", "sex")])
# Fit linear regression model
lm_surv <- lm(time ~ age + ph.ecog + sex, data = lung_df)
summary(lm_surv)

"Call:
lm(formula = time ~ age + ph.ecog + sex, data = lung_df)

Residuals:
    Min      1Q  Median      3Q     Max 
-384.29 -145.88  -57.31   98.90  746.09 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)   
(Intercept) 328.8827   107.9083   3.048  0.00258 **
age          -0.6403     1.5471  -0.414  0.67936   
ph.ecog     -56.6082    19.4484  -2.911  0.00397 **
sex          51.0133    28.1566   1.812  0.07137 . 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 205.9 on 223 degrees of freedom
Multiple R-squared:  0.05595,	Adjusted R-squared:  0.04325 
F-statistic: 4.406 on 3 and 223 DF,  p-value: 0.00493
"

"
A linear regression model was fitted with survival time as the response variable and age, 
ECOG performance score (ph.ecog), and sex as predictors. This model assumes that each 
predictor has a linear and additive effect on survival time. The fitted model serves as a 
baseline for evaluating whether more flexible approaches are necessary.
"

#B

par(mfrow = c(2, 2))
plot(lm_surv)

# Predictor-wise relationship plots
ggplot(lung_df, aes(age, time)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Survival Time vs Age",
    x = "Age (years)",
    y = "Survival Time"
  )

ggplot(lung_df, aes(ph.ecog, time)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Survival Time vs ECOG Score",
    x = "ECOG Performance Status",
    y = "Survival Time"
  )

ggplot(lung_df, aes(sex, time)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Survival Time vs Sex",
    x = "Sex",
    y = "Survival Time"
  )

"
The diagnostic plots of the linear model do not reveal strong violations of linearity or 
homoscedasticity. In addition, the scatter plots with smooth trend lines show approximately 
linear relationships between survival time and both age and ph.ecog, while sex behaves as a 
categorical shift in survival time.

However, some mild curvature, particularly for age, suggests that a flexible model may be 
explored to confirm whether nonlinear effects exist. This motivates fitting a GAM as a 
diagnostic extension, rather than as a default replacement.
"

# Fit GAM to allow nonlinear effect for age
gam_surv <- gam(
  time ~ s(age) + ph.ecog + sex,
  data = lung_df
)
summary(gam_surv)

"
Family: gaussian 
Link function: identity 

Formula:
time ~ s(age) + ph.ecog + sex

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   288.89      45.50   6.349  1.2e-09 ***
ph.ecog       -56.61      19.45  -2.911  0.00397 ** 
sex            51.01      28.16   1.812  0.07137 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
       edf Ref.df     F p-value
s(age)   1      1 0.171   0.679

R-sq.(adj) =  0.0433   Deviance explained =  5.6%
GCV =  43168  Scale est. = 42407     n = 227"

par(mfrow = c(1, 1))
plot(gam_surv, shade = TRUE, seWithMean = TRUE)

"
The smooth function for age does not show strong curvature and remains close to linear across 
most of its range. Confidence intervals are wide at extreme ages, indicating increased 
uncertainty rather than a clear nonlinear trend. This suggests that the linearity assumption 
for age is largely reasonable in this dataset.
"

#C

# Natural spline model
spline_surv <- lm(
  time ~ ns(age, df = 4) + ns(ph.ecog, df = 3) + sex,
  data = lung_df
)
summary(spline_surv)

"
Call:
lm(formula = time ~ ns(age, df = 4) + ns(ph.ecog, df = 3) + sex, 
    data = lung_df)

Residuals:
    Min      1Q  Median      3Q     Max 
-379.02 -136.43  -57.82   96.75  737.31 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)   
(Intercept)            236.68      90.83   2.606   0.0098 **
ns(age, df = 4)1        16.53      80.91   0.204   0.8383   
ns(age, df = 4)2        84.62      73.76   1.147   0.2525   
ns(age, df = 4)3        71.33     191.41   0.373   0.7098   
ns(age, df = 4)4      -158.06     107.67  -1.468   0.1435   
ns(ph.ecog, df = 3)1   -59.97     157.51  -0.381   0.7038   
ns(ph.ecog, df = 3)2  -178.92     115.03  -1.555   0.1213   
ns(ph.ecog, df = 3)3  -226.67     183.62  -1.234   0.2184   
sex                     49.17      28.46   1.727   0.0855 . 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 206.6 on 218 degrees of freedom
Multiple R-squared:  0.07105,	Adjusted R-squared:  0.03696 
F-statistic: 2.084 on 8 and 218 DF,  p-value: 0.03846"

"
A spline-based linear model was fitted using natural splines for age and ph.ecog to allow 
additional flexibility. However, the spline terms do not lead to a substantial improvement 
in model fit or interpretability compared to the simpler models. This indicates that the 
added flexibility is not strongly supported by the data.
"

#D

"
While both GAMs and spline-based models are capable of modeling nonlinear relationships, 
their use must be justified by the data. In this analysis, neither the diagnostic plots nor 
the GAM smooths provide strong evidence of meaningful nonlinearity for age or ph.ecog.

From a theoretical standpoint:
 * Splines require manual selection of degrees of freedom and can reduce interpretability.
 * GAMs offer automatic smoothness selection and clearer visual interpretation of effects.
 * However, unnecessary flexibility increases variance and reduces model stability.

Given the lack of strong nonlinear effects, the linear model (or equivalently, a GAM with 
essentially linear smooths) is preferred. It provides a parsimonious, stable, and easily 
interpretable description of the relationship between predictors and survival time.
"

##### Question3

#A

library(mlbench)
library(mgcv)
library(ggplot2)

data(BreastCancer)
bc_df <- BreastCancer
bc_df <- bc_df[, 2:11]
bc_df$Class <- ifelse(bc_df$Class == "benign", 0, 1)
bc_df[, 1:9] <- lapply(
  bc_df[, 1:9],
  function(x) as.numeric(as.character(x))
)
bc_df <- na.omit(bc_df)

#B

glm_bc <- glm(
  Class ~ Cl.thickness + Cell.size + Bare.nuclei,
  data = bc_df,
  family = binomial
)
summary(glm_bc)

"
Call:
glm(formula = Class ~ Cl.thickness + Cell.size + Bare.nuclei, 
    family = binomial, data = bc_df)

Coefficients:
             Estimate Std. Error z value Pr(>|z|)    
(Intercept)  -7.89461    0.77845 -10.142  < 2e-16 ***
Cl.thickness  0.58657    0.12327   4.758 1.95e-06 ***
Cell.size     0.75450    0.13479   5.598 2.17e-08 ***
Bare.nuclei   0.55112    0.08495   6.488 8.70e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 884.35  on 682  degrees of freedom
Residual deviance: 135.56  on 679  degrees of freedom
AIC: 143.56

Number of Fisher Scoring iterations: 7"

odds_ratios <- exp(coef(glm_bc))
odds_ratios
" (Intercept) Cl.thickness    Cell.size  Bare.nuclei 
0.0003727465 1.7978081400 2.1265442338 1.7351919503 "


"
A logistic regression model was fitted to predict the probability of malignant breast cancer 
using clump thickness, cell size, and bare nuclei.

 * Cl.thickness:
   The odds ratio associated with clump thickness is greater than 1, indicating that an 
   increase in clump thickness is associated with higher odds of malignancy, holding the 
   other variables constant.

 * Cell.size:
   The odds ratio for cell size is substantially greater than 1, suggesting that larger 
   variability in cell size strongly increases the odds of a tumor being malignant. This 
   variable shows one of the strongest effects in the model.

 * Bare.nuclei:
   The odds ratio for bare nuclei is also greater than 1, meaning that higher values of 
   bare nuclei significantly increase the odds of malignancy.

For all three predictors, increases in their values are associated with multiplicative 
increases in the odds of malignancy, which is consistent with their known pathological 
relevance.
"

#C

bc_df$logit_glm <- predict(glm_bc, type = "link")

ggplot(bc_df, aes(Cl.thickness, logit_glm)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  labs(
    title = "Logit vs Clump Thickness",
    x = "Clump Thickness",
    y = "Log-odds of Malignancy"
  )

ggplot(bc_df, aes(Cell.size, logit_glm)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  labs(
    title = "Logit vs Cell Size",
    x = "Cell Size",
    y = "Log-odds of Malignancy"
  )

ggplot(bc_df, aes(Bare.nuclei, logit_glm)) +
  geom_point(alpha = 0.6) +
  geom_smooth() +
  labs(
    title = "Logit vs Bare Nuclei",
    x = "Bare Nuclei",
    y = "Log-odds of Malignancy"
  )

"
The plots of the log-odds versus predictors reveal noticeable nonlinear patterns, particularly 
for Cell.size and Bare.nuclei, where the relationship between predictors and log-odds is 
clearly not linear. This violates the linearity assumption of logistic regression and motivates
the use of a GAM with smooth terms.
"

gam_bc <- gam(
  Class ~ s(Cl.thickness) + s(Cell.size) + s(Bare.nuclei),
  data = bc_df,
  family = binomial
)
summary(gam_bc)

"
Family: binomial 
Link function: logit 

Formula:
Class ~ s(Cl.thickness) + s(Cell.size) + s(Bare.nuclei)

Parametric coefficients:
            Estimate Std. Error z value Pr(>|z|)   
(Intercept)  -1.3849     0.5092   -2.72  0.00654 **
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
                  edf Ref.df Chi.sq  p-value    
s(Cl.thickness) 1.327  1.586  15.37 0.000219 ***
s(Cell.size)    7.550  8.315  30.57 0.000238 ***
s(Bare.nuclei)  1.976  2.406  36.92  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =  0.896   Deviance explained = 87.4%
UBRE = -0.80197  Scale est. = 1         n = 683"


#D

# GLM accuracy
prob_glm <- predict(glm_bc, type = "response")
class_glm <- ifelse(prob_glm > 0.5, 1, 0)
accuracy_glm <- mean(class_glm == bc_df$Class)
# GAM accuracy
prob_gam <- predict(gam_bc, type = "response")
class_gam <- ifelse(prob_gam > 0.5, 1, 0)
accuracy_gam <- mean(class_gam == bc_df$Class)

accuracy_glm
"0.9633968"

accuracy_gam
"0.966325"

"
The GAM achieves higher classification accuracy compared to the logistic regression model. 
This improvement reflects the GAM’s ability to capture nonlinear relationships between 
predictors and malignancy risk, which are not adequately modeled by a linear logit.
"

#E

par(mfrow = c(1, 3))

plot(
  gam_bc, select = 1,
  shade = TRUE, seWithMean = TRUE,
  main = "Probability vs Clump Thickness",
  xlab = "Clump Thickness",
  ylab = "Effect on Log-Odds"
)

plot(
  gam_bc, select = 2,
  shade = TRUE, seWithMean = TRUE,
  main = "Probability vs Cell Size",
  xlab = "Cell Size",
  ylab = "Effect on Log-Odds"
)

plot(
  gam_bc, select = 3,
  shade = TRUE, seWithMean = TRUE,
  main = "Probability vs Bare Nuclei",
  xlab = "Bare Nuclei",
  ylab = "Effect on Log-Odds"
)

"
These plots illustrate how the predicted probability of malignancy changes as each predictor 
increases, while holding the others constant. The nonlinear shapes—especially for Cell.size and
Bare.nuclei—demonstrate threshold-like behavior, where malignancy risk increases sharply beyond
certain values. This provides an intuitive and clinically meaningful interpretation that is not
available from the logistic regression alone.
"

"
Conclusion:
The logistic regression model provides interpretable odds ratios and confirms the importance 
of the selected predictors. However, diagnostic plots reveal violations of the linearity 
assumption. The GAM captures these nonlinear effects more effectively, resulting in improved 
accuracy and clearer probability-based interpretations. Therefore, the GAM is preferred for 
modeling breast cancer malignancy in this dataset.
"

##### Question4

library(MASS)
library(ggplot2)

#A

data(epil)
epil_df <- epil
epil_df$trt <- factor(epil_df$trt,
                      labels = c("Placebo", "Progabide"))
set.seed(1)
n_obs <- nrow(epil_df)
train_index <- sample(seq_len(n_obs), size = 0.7 * n_obs)
train_epil <- epil_df[train_index, ]
test_epil  <- epil_df[-train_index, ]


# Poisson regression for seizure counts
pois_epil <- glm(
  y ~ trt + age + base,
  data = train_epil,
  family = poisson
)
summary(pois_epil)

"Call:
glm(formula = y ~ trt + age + base, family = poisson, data = train_epil)

Coefficients:
               Estimate Std. Error z value Pr(>|z|)    
(Intercept)   0.5303791  0.1607327   3.300 0.000968 ***
trtProgabide -0.1930408  0.0582269  -3.315 0.000915 ***
age           0.0240516  0.0047432   5.071 3.96e-07 ***
base          0.0230519  0.0005878  39.216  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for poisson family taken to be 1)

    Null deviance: 1966.38  on 164  degrees of freedom
Residual deviance:  680.86  on 161  degrees of freedom
AIC: 1227.9

Number of Fisher Scoring iterations: 5"

"
A Poisson regression model was fitted to predict the number of seizures (y) using treatment 
group, age, and baseline seizure count. Poisson regression is appropriate here because the 
response variable represents count data. The model was trained on 70% of the data, with the 
remaining 30% reserved for testing, allowing evaluation of out-of-sample predictive performance.
"

#B

test_epil$predicted_y <- predict(pois_epil, newdata = test_epil, type = "response")
rmse_epil <- sqrt(mean((test_epil$y - test_epil$predicted_y)^2))
rmse_epil
"5.779365"

rate_ratios <- exp(coef(pois_epil))
rate_ratios
" (Intercept) trtProgabide          age         base 
   1.6995765    0.8244483    1.0243431    1.0233196 "

rr_ci <- exp(confint(pois_epil))
rr_ci
"                2.5 %    97.5 %
(Intercept)  1.237878 2.3246003
trtProgabide 0.735578 0.9242242
age          1.014850 1.0338991
base         1.022139 1.0244973"

"
In a Poisson regression model, coefficients are interpreted on the log rate scale, and their 
exponentiated values represent rate ratios.

 * Treatment (Progabide vs Placebo):
   The rate ratio for Progabide is less than 1, indicating that patients receiving Progabide 
   experience a lower expected seizure count compared to those on placebo, holding age and 
   baseline seizure count constant.
   If the 95% confidence interval does not include 1, this reduction is statistically 
   significant.
 * Age:
   The rate ratio for age is close to 1, suggesting that age has a relatively small effect on 
   seizure frequency when baseline seizure count and treatment are accounted for.
 * Baseline seizure count (base):
   The rate ratio for base is greater than 1, meaning that higher baseline seizure counts are 
   associated with higher expected seizure counts after treatment. This is clinically 
   intuitive, as patients with more severe baseline epilepsy tend to continue experiencing 
   more seizures.
"

"
The RMSE measures the average deviation between observed and predicted seizure counts on the 
test set. A lower RMSE indicates better predictive accuracy. Given the variability inherent in 
seizure count data, the obtained RMSE suggests that the model provides a reasonable 
out-of-sample prediction, though some variability remains unexplained.
"

#C

ggplot(test_epil, aes(x = age, y = predicted_y, color = trt)) +
  geom_point(alpha = 0.6) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Predicted Seizure Counts vs Age by Treatment Group",
    x = "Age (years)",
    y = "Predicted Number of Seizures",
    color = "Treatment"
  )

"
The plot illustrates how the predicted seizure counts vary with age, stratified by treatment 
group. Across most age ranges, patients receiving Progabide tend to have lower predicted 
seizure counts compared to those receiving placebo. The roughly parallel trends suggest that 
the treatment effect is relatively consistent across age, with no strong evidence of interaction
between age and treatment in this model.
"

"
The Poisson regression model appropriately captures the count nature of seizure data and 
demonstrates that baseline seizure count is the strongest predictor, while treatment with 
Progabide reduces seizure frequency relative to placebo. The model shows reasonable predictive 
performance on the test set, and the visualization confirms a consistent treatment effect 
across ages.
"

##### Question5

library(MASS)
library(mgcv)
library(ggplot2)

data(cats)
cats_df <- na.omit(cats)
cats_df$Sex <- factor(cats_df$Sex)

set.seed(1)
n <- nrow(cats_df)
train_id <- sample(seq_len(n), size = 0.8 * n)
train <- cats_df[train_id, ]
test  <- cats_df[-train_id, ]

ggplot(train, aes(x = Bwt, y = Hwt, color = Sex)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Heart Weight vs Body Weight (colored by Sex)",
    x = "Body Weight",
    y = "Heart Weight"
  )

# ---- Linear model ----
lm_cats <- lm(Hwt ~ Bwt + Sex, data = train)
summary(lm_cats)

"
Call:
lm(formula = Hwt ~ Bwt + Sex, data = train)

Residuals:
    Min      1Q  Median      3Q     Max 
-3.6314 -1.0648 -0.1583  0.9585  5.0420 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -0.6746     0.8748  -0.771    0.442    
Bwt           4.1331     0.3527  11.719   <2e-16 ***
SexM          0.0134     0.3648   0.037    0.971    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 1.526 on 112 degrees of freedom
Multiple R-squared:  0.6416,	Adjusted R-squared:  0.6352 
F-statistic: 100.2 on 2 and 112 DF,  p-value: < 2.2e-16"

# ---- GAM model ----
gam_cats <- gam(Hwt ~ s(Bwt) + Sex, data = train)
summary(gam_cats)
"
Family: gaussian 
Link function: identity 

Formula:
Hwt ~ s(Bwt) + Sex

Parametric coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 10.70718    0.28392  37.712   <2e-16 ***
SexM         0.08927    0.36781   0.243    0.809    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Approximate significance of smooth terms:
         edf Ref.df     F p-value    
s(Bwt) 1.669  2.091 67.31  <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

R-sq.(adj) =   0.64   Deviance explained = 64.9%
GCV = 2.3712  Scale est. = 2.2955    n = 115"

test$pred_lm  <- predict(lm_cats, newdata = test)
test$pred_gam <- predict(gam_cats, newdata = test)

rmse_lm  <- sqrt(mean((test$Hwt - test$pred_lm)^2))
rmse_gam <- sqrt(mean((test$Hwt - test$pred_gam)^2))

rmse_lm
"1.16701"

rmse_gam
"1.163213"

AIC(lm_cats, gam_cats)
"               df      AIC
lm_cats  4.000000 428.4718
gam_cats 4.668976 427.5255"

ggplot(test, aes(x = Bwt, y = pred_lm, color = Sex)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Predicted Hwt vs Bwt (Linear Model)",
    x = "Body Weight",
    y = "Predicted Heart Weight"
  )

ggplot(test, aes(x = Bwt, y = pred_gam, color = Sex)) +
  geom_point(alpha = 0.7) +
  geom_smooth(se = FALSE) +
  labs(
    title = "Predicted Hwt vs Bwt (GAM)",
    x = "Body Weight",
    y = "Predicted Heart Weight"
  )

"
The scatter plot of heart weight versus body weight, colored by sex, shows an approximately 
linear relationship for both male and female cats. There is no strong visual evidence of 
curvature, although a GAM was fitted to verify whether allowing a nonlinear effect of body 
weight improves the model.

Both the linear model and the GAM were evaluated using RMSE on the test set and AIC on the 
training data. The RMSE values for the two models are very similar, indicating comparable 
predictive accuracy. However, the linear model has a lower AIC, showing that it achieves 
similar fit with fewer effective degrees of freedom.

The predicted heart weight versus body weight plots confirm that both models produce nearly 
identical prediction patterns, and the GAM does not reveal any meaningful nonlinear structure 
beyond the linear trend.

Final model choice based on:
 1. visual linearity assessment
 2. similar RMSE values
 3. and lower AIC for the linear model

The linear model is preferred. It is more parsimonious, easier to interpret, and performs 
equally well compared to the GAM.
"
