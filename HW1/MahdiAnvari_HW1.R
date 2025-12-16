# Question 1

before <- c(30.1,32.0,29.5,33.1,31.8,29.9,34.2,30.6,31.2,33.5, 
            32.1,29.7,30.9,31.5,33.0,30.3,31.9,32.5,29.8,34.0)


after <- c(28.4,30.2,28.0,30.9,30.1,28.8,31.5,29.6,29.9,31.8, 
           30.5,28.9,29.7,30.6,31.0,29.5,30.7,31.2,28.6,32.1)

# Test that after-before is normal with Shapiro–Wilk normality test
shapiro.test(after - before)

t.test(after, before, alternative = "less", mu = 0, paired = TRUE)

# Since the normality test confirmed that the differences have normal distribution, 
# we can proceed with parametric paired two-sample t-test to compare mean BMI before and after the diet.
# The p-value was below the threshold, so the null hypothesis is rejected, and the diet significantly decreased the BMI in patients.


# Question 2


hb <- c(14.83, 12.98, 13.23, 13.44, 13.29, 11.60, 12.99, 12.77, 12.27, 12.95, 
        13.42, 11.86, 13.01, 11.64, 12.39, 12.95, 13.85, 13.30, 12.18, 13.31, 
        12.53, 12.12, 12.96, 12.77, 12.32, 11.56, 12.14, 13.56, 13.27, 13.06, 
        12.30, 12.58, 12.63, 12.12, 12.22, 12.88, 12.13, 12.73, 12.86, 13.40, 
        13.69, 11.77, 12.44, 13.67, 12.11, 13.89, 12.02, 13.24, 13.98, 14.11, 
        14.22, 11.78, 12.39, 12.82, 13.64, 13.74, 12.87, 12.64, 11.58, 12.47)

# Test that after-before is normal with Shapiro–Wilk normality test
shapiro.test(hb)

t.test(hb, mu = 13.5)

# The Shapiro test confirmed the normality of the hemoglobin samples, 
# we can proceed with parametric one sample t-test to compare mean hemoglobin with a specific value(13.5 g/dl).
# The p-value of the t-test was significant, so the null hypothesis is rejected, and the mean hemoglobin in population differ from this value (13.5 g/dl)


# Question 3

cancer <- c(8.61, 5.24, 6.45, 7.22, 6.60, 8.36, 6.07, 8.12, 6.92, 7.33, 
            5.45, 6.77, 7.24, 5.18, 6.69, 7.19, 6.94, 6.59, 7.21, 7.33, 
            8.85, 7.08, 4.98, 7.36, 6.60, 8.88, 6.05, 7.99, 7.35, 6.67, 
            6.45, 8.16, 7.59, 7.16, 8.42, 8.88, 6.27, 7.12, 6.18, 8.27, 
            7.42, 8.39, 8.58, 6.49, 7.21, 8.07, 6.61, 6.60, 6.27, 7.85, 
            7.95, 7.06, 5.32, 6.55, 6.29, 7.46, 6.77, 6.14, 7.18, 7.04, 
            6.80, 8.09, 6.23, 6.94, 6.58, 6.66, 6.37, 6.95, 7.13, 6.52) 

control <- c(4.72, 5.35, 5.88, 6.08, 4.96, 4.05, 5.01, 4.98, 4.57, 4.88, 
             5.46, 5.07, 4.68, 4.86, 5.17, 4.56, 4.71, 5.95, 5.18, 4.85, 
             4.92, 4.96, 5.22, 4.42, 4.99, 5.58, 5.35, 4.90, 5.55, 6.88, 
             4.93, 4.77, 5.05, 5.09, 5.81, 4.97, 5.18, 4.95, 4.52, 6.05, 
             5.51, 4.41, 5.29, 5.69, 5.07, 4.62, 5.01, 5.86, 5.46, 5.56, 
             4.86, 5.18, 5.39, 4.90, 5.22, 5.14, 4.99, 5.63, 5.11, 5.28, 5.71, 5.31, 5.18, 5.02, 4.88) 

# Checks normality for each group (Shapiro–Wilk)
shapiro.test(cancer)
shapiro.test(control)

# Checks equality of variances (Levene’s test)
install.packages("car")
library(car)
group <- factor(c(rep("cancer", length(cancer)), rep("control", length(control))))
values <- c(cancer, control)
leveneTest(values, group)

# Both tests suggest non-normality and unequal variances, that’s a signal that a parametric t-test might not be appropriate.
# t.test(cancer, control, alternative = "less", mu = 0, var.equal = FALSE)



# Question 4

set.seed(1)
n_per_cell <- 40 

genotype <- rep(c("AA","AG"), each = 2 * n_per_cell)
treatment <- rep(rep(c("Drug","Placebo"), each = n_per_cell), 2) 

AA_Placebo = rnorm(n_per_cell, mean = 50, sd = 5)
AA_Drug = rnorm(n_per_cell, mean = 60, sd = 5)
AG_Placebo = rnorm(n_per_cell, mean = 55, sd = 5)
AG_Drug = rnorm(n_per_cell, mean = 65, sd = 5)

score <- c(AA_Drug, AA_Placebo, AG_Drug, AG_Placebo)
data <- data.frame(genotype, treatment, score)
head(data)


# Run a two-way ANOVA
anova_result <- aov(score ~ genotype * treatment, data = data)
summary(anova_result)

# 1. Check normality of residuals
shapiro.test(residuals(anova_result))

# 2. Check equal variances
leveneTest(score ~ genotype * treatment, data = data)

# Since the normality test confirmed that the residuals are approximately normally distributed,
# we can proceed with a two-way ANOVA to examine the effects of Genotype and Treatment.
# The results indicate that both Genotype and Treatment have significant effects,
# whereas their interaction did not show a significant effect.


# Question 5

set.seed(1)
n_patients = 60

baseline_activity = rnorm(n_patients, mean = 40, sd = 5)
the_activity = rnorm(n_patients, mean = 8, sd = 4)
after_activity = baseline_activity + the_activity

# Test that after-before is normal with Shapiro–Wilk normality test
shapiro.test(after_activity - baseline_activity)

t.test(after_activity, baseline_activity, alternative = "greater", mu = 0, paired = TRUE)


# Since the normality test confirmed that the differences have normal distribution, 
# we can proceed with parametric paired two-sample t-test to compare mean enzyme activity before and after a therapy.
# The p-value was below the threshold, so the null hypothesis is rejected, and the diet significantly increased the enzyme activity.



# Question 6

before <- c(228.45, 198.45, 225.01, 220.55, 209.81, 205.05, 243.45, 206.53, 
            199.84, 214.14, 235.10, 252.23, 201.69, 240.97, 231.13, 216.03, 
            210.72, 217.27, 236.58, 227.21, 211.36, 231.26, 223.95, 222.57, 
            214.61, 203.97, 210.77, 234.46, 201.64, 239.79, 219.06, 219.72, 
            236.22, 228.69, 200.61, 199.18, 210.06, 208.68, 219.09, 220.21, 
            241.95, 223.35, 223.19, 214.93, 219.83, 196.72, 240.66, 228.11, 
            227.09, 195.51, 223.66, 236.95, 239.24, 216.38, 200.95, 217.70, 
            216.14, 242.08, 232.43, 234.90) 

after <- c(210.70, 188.20, 210.65, 203.17, 196.46, 183.95, 220.60, 189.23, 
           184.39, 201.69, 217.71, 235.09, 199.22, 230.29, 210.45, 201.45, 
           199.75, 201.13, 211.78, 205.38, 196.06, 206.90, 201.11, 199.04, 
           190.94, 193.62, 194.40, 213.12, 193.31, 221.94, 198.40, 203.43, 
           206.28, 211.46, 190.43, 196.91, 192.04, 193.30, 199.09, 202.21, 
           228.84, 205.45, 207.07, 201.36, 202.85, 186.70, 215.75, 206.11, 
           211.77, 190.68, 206.76, 228.60, 207.30, 201.05, 189.57, 203.86, 
           205.16, 217.96, 211.42, 206.49) 

# Test that after-before is normal with Shapiro–Wilk normality test
shapiro.test(after - before)

t.test(after, before, alternative = "less", mu = 0, paired = TRUE)

# Since the normality test confirmed that the differences have normal distribution, 
# we can proceed with parametric paired two-sample t-test to compare mean cholesterol before and after the surgery
# The p-value was below the threshold, so the null hypothesis is rejected, and the diet significantly increased the enzyme activity.



# Question 7

install.packages("NHANES")
library(NHANES) 

data("NHANES")
str(NHANES) 

# We'll use BMI, SmokingStatus (e.g., SmokeNow), Gender 
df <- NHANES[, c("Gender","BMI","SmokeNow") ]

# Rename / factor 
df <- na.omit(df) 
df$SmokeNow <- factor(df$SmokeNow, levels=unique(df$SmokeNow)) 
df$Gender <- factor(df$Gender)

df

# Run a two-way ANOVA
anova_result <- aov(BMI ~ Gender * SmokeNow, data = df)
summary(anova_result)

# 1. Check normality of residuals
shapiro.test(residuals(anova_result))

# 2. Check equal variances
leveneTest(BMI ~ Gender * SmokeNow, data = df)

# Both tests suggest non-normality and unequal variances, that’s a signal that a parametric test might not be appropriate.



# Question 8

chol_LowFat <- c(195.97, 186.86, 189.03, 189.03, 190.19, 203.14, 198.72, 
                 185.93, 192.99, 193.13, 186.98, 183.74, 199.24, 200.01, 
                 181.74, 183.36, 196.08, 186.27, 184.49, 191.12, 195.17, 
                 189.68, 188.91, 185.33, 199.64, 196.93, 193.29, 198.34, 
                 182.27, 187.30, 183.60, 193.66, 198.45, 194.25, 189.93, 
                 197.37, 199.21, 201.72, 196.69, 190.36, 186.18, 196.35, 
                 185.68, 191.05, 189.93, 201.98, 196.13, 
                 192.42, 201.34, 186.29, 204.12, 190.66, 196.55, 197.50, 
                 196.07, 188.90, 191.83, 201.10, 195.04, 185.74) 

chol_Keto <- c(205.30, 211.92, 202.25, 216.14, 211.79, 208.92, 200.17, 
               198.94, 206.09, 205.63, 233.53, 206.89, 208.25, 193.62, 
               210.22, 218.27, 206.82, 217.37, 209.53, 219.79, 219.08, 
               206.31, 213.81, 195.64, 208.90, 206.06, 226.72, 210.27, 
               200.29, 215.18, 213.84, 200.99, 210.90, 223.87, 198.52, 
               198.43, 227.07, 217.36, 209.20, 204.24, 
               217.06, 220.65, 205.54, 213.00, 226.76, 217.85, 204.19, 
               206.54, 201.46, 208.70, 216.05, 203.96, 206.56, 216.00, 
               214.28, 206.53, 216.20, 208.10, 210.72, 214.56) 

chol_Med <- c(197.47, 199.80, 198.03, 201.03, 199.51, 194.02, 196.27, 
              191.41, 183.95, 203.61, 201.29, 198.35, 196.57, 193.36, 
              207.24, 199.31, 204.96, 199.23, 191.87, 200.03, 193.05, 
              200.66, 209.10, 183.80, 200.99, 195.73, 190.65, 201.06, 
              197.32, 195.81, 184.08, 206.13, 206.15, 200.11, 193.90, 
              190.49, 195.11, 196.68, 192.51, 203.76, 204.05, 193.84, 
              193.82, 193.01, 196.66, 197.12, 196.64, 190.78, 196.73, 
              200.28, 204.56, 204.07, 200.03, 187.63, 196.52, 201.90, 
              192.19, 195.22, 200.07, 201.79) 

chol <- c(chol_LowFat, chol_Keto, chol_Med)
diet <- factor(rep(c("Lowfat","Keto","Mediterranean"), each = 60))
data <- data.frame(diet, chol)

# Use a one-way ANOVA test
aov_mod <- aov(chol ~ diet, data = data)
summary(aov_mod)

# 1. Check normality of residuals
shapiro.test(residuals(aov_mod))

# 2. Check equal variances
leveneTest(chol ~ diet, data = data)

# Since the normality test confirmed that the residuals are normally distributed, 
# we can proceed with a one-way ANOVA test to compare mean cholesterol across diet groups.
# Next, we perform Tukey’s HSD post-hoc test to determine which specific diet pairs differ.)

TukeyHSD(aov_mod) 

# The TukeyHSD results suggest that all diet pairs differ significantly from each other.



# Question 9

set.seed(1) 
expr <- c( 
  rnorm(25, mean = 10.5, sd = 0.6),   
  rnorm(25, mean = 9.8, sd = 0.6),   
  rnorm(25, mean = 11.2, sd = 0.7),   
  rnorm(25, mean = 10.0, sd = 0.5)    
) 

cell_type <- factor(rep(c("Cardiac","Adipose","Neuronal","Fibroblast"), each = 25)) 
df1 <- data.frame(expr, cell_type) 

# Use a one-way ANOVA test
aov_mod <- aov(expr ~ cell_type, data = df1)
summary(aov_mod)

# 1. Check normality of residuals
shapiro.test(residuals(aov_mod))

# 2. Check equal variances
leveneTest(chol ~ diet, data = df1)

# Since the normality test confirmed that the residuals are normally distributed, 
# we can proceed with a one-way ANOVA test to compare mean expression across different cell types.
# Next, we perform Tukey’s HSD post-hoc test to determine which specific cell type pairs differ.)

TukeyHSD(aov_mod) 

# The TukeyHSD results suggest that all cell type pairs differ significantly from each other except Fibroblast-Adipose pair.



# Question 10

set.seed(1)

pain_morphine <- rnorm(30, mean = 3.2, sd = 1.0)
pain_ketorolac <- rnorm(30, mean = 4.0, sd = 1.1) 
pain_placebo <- rnorm(30, mean = 6.2, sd = 1.3) 

pain <- c(pain_morphine, pain_ketorolac, pain_placebo)
treatment <- factor(rep(c("Morphine", "Ketorolac", "Placebo"), each = 30))
data <- data.frame(treatment, pain)

# Use a one-way ANOVA test
aov_mod <- aov(pain ~ treatment, data = data)
summary(aov_mod)

# 1. Check normality of residuals
shapiro.test(residuals(aov_mod))

# 2. Check equal variances
leveneTest(pain ~ treatment, data = data)

# Since the normality test confirmed that the residuals are normally distributed, 
# we can proceed with a one-way ANOVA test to compare mean pain across different treatments.
# Next, we perform Tukey’s HSD post-hoc test to determine which specific treatment pairs differ.)

TukeyHSD(aov_mod)

# The TukeyHSD results suggest that all cell type pairs differ significantly from each other and Morphine is the most effective one.



# Question 11

glucose <- c( 
  99.97, 93.62, 101.48, 110.23, 92.66, 99.01, 81.46, 97.62, 82.35, 92.34, 
  122.21, 98.85, 88.40, 81.30, 103.38, 96.46, 84.51, 105.30, 84.30, 93.79, 
  103.14, 84.37, 94.85, 99.30, 114.23, 93.67, 94.79, 109.30, 104.32, 101.45, 
  96.84, 87.35, 84.76, 97.84, 92.22, 95.75, 95.56, 107.34, 96.87, 94.11, 
  92.39, 95.85, 92.27, 86.54, 86.24, 102.99, 97.60, 97.30, 83.56, 97.16, 
  85.45, 94.32, 88.66, 79.86, 95.80, 107.96, 89.59, 100.34, 89.25, 95.36, 
  91.25, 98.66, 89.93, 102.46, 82.46, 105.13, 94.85, 87.08, 90.63, 95.66, 
  96.48, 94.42, 95.03, 95.67, 99.58, 84.11, 103.62, 102.78, 86.08, 99.06, 
  92.07, 95.80, 86.62, 90.86, 86.98, 96.07, 101.11, 106.44, 102.15, 88.45, 
  96.79, 90.41, 97.64, 97.53, 96.92, 106.93, 96.59, 88.83, 101.73, 85.22, 
  109.56, 95.94, 106.67, 100.97, 107.92, 110.29, 112.92, 95.28, 88.00, 108.21, 
  96.85, 90.30, 106.06, 103.20, 109.65, 94.36, 92.77, 102.51, 95.92, 108.50, 
  112.95, 108.76, 110.45, 108.34, 110.80, 98.64, 108.61, 109.92, 98.64, 95.05, 
  116.15, 105.94, 96.94, 108.93, 100.20, 116.48, 97.06, 111.59, 125.11, 99.67, 
  106.22, 100.59, 103.78, 100.48, 100.11, 117.46, 104.66, 115.58, 101.33, 107.29, 
  99.43, 106.07, 104.50, 101.37, 112.38, 107.66, 103.62, 103.18, 111.85, 96.46, 
  102.16, 94.67, 102.65, 117.17, 104.61, 97.88, 106.51, 106.86, 114.62, 95.32, 
  111.03, 100.27, 103.74, 94.70, 106.59, 111.12, 103.27, 95.48, 100.20, 111.09, 
  101.00, 87.30, 120.05, 103.86, 97.40, 101.61, 96.83, 119.66, 97.66, 103.88, 
  93.12, 119.26, 91.30, 107.20, 105.98, 107.02, 117.05, 117.49, 100.22, 115.11 
) 

drug_type <- factor(rep(c("A", "B"), each = 100))
gender <- factor(rep(rep(c("Male", "Female"), each = 50), 2))
data <- data.frame(glucose, drug_type, gender)

# Use a two-way ANOVA test
aov_mod <- aov(glucose ~ drug_type * gender, data = data)
summary(aov_mod)

# 1. Check normality of residuals
shapiro.test(residuals(aov_mod))

# 2. Check equal variances
leveneTest(glucose ~ drug_type * gender, data = data)


# Since the normality test confirmed that the residuals are normally distributed, 
# we can proceed with a two-way ANOVA test to examine the effect of gender and drug type on the blood glucose.
# So the only factor that matters is the drug type based on the tests