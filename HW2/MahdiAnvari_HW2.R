# Question 1


data <- data.frame(
  shift = factor(rep(c("Day","Night"), each=20)),
  noise = factor(rep(rep(c("Low","High"), each=10), 2)),
  productivity = c(56, 58, 57, 59, 58, 58, 57, 56, 57,55,
                   88,85,52,84,87,45,86,58,83,84,
                   61,70, 47, 50, 88, 56, 90,79, 69, 55, 58, 65, 47, 66, 53, 70,
                   67, 83, 61 ,60))

# Test that productivity is normal with Shapiro–Wilk normality test and test the variance homogeneity with the Levene test:
shapiro.test(data$productivity)

"	Shapiro-Wilk normality test

data:  data$productivity
W = 0.88599, p-value = 0.0007668"

library(car)
leveneTest(productivity~noise*shift, data=data)
"
Levene's Test for Homogeneity of Variance (center = median)
      Df F value  Pr(>F)  
group  3  3.2723 0.03211 *
      36                  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1"

library(ARTool)
model1 <- art(productivity~noise*shift, data=data)
summary(model1)
"Aligned Rank Transform of Factorial Model

Call:
art(formula = productivity ~ noise * shift, data = data)

Column sums of aligned responses (should all be ~0):
      noise       shift noise:shift 
          0           0           0 

F values of ANOVAs on aligned responses not of interest (should all be ~0):
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
      0       0       0       0       0       0 "

anova(model1)

"Analysis of Variance of Aligned Rank Transformed Data

Table Type: Anova Table (Type III tests) 
Model: No Repeated Measures (lm)
Response: art(productivity)

              Df Df.res F value   Pr(>F)  
1 noise        1     36 4.80817 0.034871 *
2 shift        1     36 0.37053 0.546535  
3 noise:shift  1     36 4.77993 0.035376 *
---
Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 "

# Since the normality test reject that productivity has the normal distribution,
# and the levene test reject the homogeneity of the variances,
# we can not proceed with parametric two-way ANOVA to compare the distributions among different shift and noise groups.
# The p-value was below the threshold for the noise groups and the interaction between noise and shift. but it was not significant for the shift.
# This means that the noise, and its interaction with shift has significant effect on the productivity, but there is no significant effect for the different shifts. 



# Question 2


machine1 <- c(25,26,24,27,26,28,25,27,26,24)
machine2 <- c(20,22,19,21,23,20,24,22,21,23)
machine3 <- c(35,36,34,37,36,35,60,62,61,59)
machine4 <- c(30,31,29,32,28,27,26,25,24,100)

strength <- c(machine1, machine2, machine3, machine4)
machines <- factor(rep(c("1", "2", "3", "4"), each = 10))
data <- data.frame(machines, strength)

# Test that strength is normal with Shapiro–Wilk normality test and variances are homogen:
shapiro.test(data$strength)
"
	Shapiro-Wilk normality test

data:  data$strength
W = 0.66616, p-value = 2.829e-08
"

leveneTest(strength~machines, data = data)
"
Levene's Test for Homogeneity of Variance (center = median)
      Df F value Pr(>F)
group  3   1.599 0.2066
      36    
"

kruskal.test(strength ~ machines, data = data)

"
	Kruskal-Wallis rank sum test

data:  strength by machines
Kruskal-Wallis chi-squared = 32.131, df = 3, p-value = 4.911e-07

"

# The Shapiro test reject the normality of the tensile strength, 
# so we can not proceed with parametric ANOVA to compare the strength of these 4 machine types. so we use the non-parametric Kruskal-Walis
# The p-value of the kruskal wallis was significant, so the null hypothesis is rejected, and all 4 machine types do not have the same strength (at least one is different from the other)


boxplot(strength ~ machines, data=data,
        main="Strength of 4 Machines",
        xlab="Machines", ylab="Strength")

#The plot is uploaded as a separate file.


# Question 3

reaction <- matrix(
  c(350,320,300,
    400,370,340,
    360,330,310,
    370,350,320,
    390,360,330,
    380,350,325,
    365,340,310,
    375,345,315,
    355,330,300,
    360,335,305),
  ncol=3, byrow=TRUE
)
colnames(reaction) <- c("NoCaffeine","100mg","200mg")

data <- as.data.frame(reaction)

# Checks normality for between each two conditions (Shapiro–Wilk)
shapiro.test(data$NoCaffeine - data$`100mg`)
"
	Shapiro-Wilk normality test

data:  data$NoCaffeine - data$`100mg`
W = 0.73087, p-value = 0.002088
"
shapiro.test(data$NoCaffeine - data$`200mg`)
"
	Shapiro-Wilk normality test

data:  data$NoCaffeine - data$`200mg`
W = 0.83184, p-value = 0.03521
"
shapiro.test(data$`100mg` - data$`200mg`)
"	
  Shapiro-Wilk normality test

data:  data$`100mg` - data$`200mg`
W = 0.62758, p-value = 0.0001181
"


friedman.test(reaction)
"
  Friedman rank sum test

data:  reaction
Friedman chi-squared = 20, df = 2, p-value = 4.54e-05"

# Shapiro tests suggest non-normality of the differences between 3 conditions, that’s a signal that a parametric repeated-measure ANOVA might not be appropriate.
# so we use the friedman test, which is the non-parametric version of the repeated measure ANOVA
# the p-value for the friedman test is significant, suggesting that the reaction time is not equal among 3 caffeine dose for these samples.



# Question 4

cancer <- c(8.61, 5.24, 6.45, 7.22, 6.60, 8.36, 6.07, 8.12, 6.92, 7.33,  
            5.45, 6.77, 7.24, 5.18, 6.69, 7.19, 6.94, 6.59, 7.21, 7.33,  8.85, 
            7.08, 4.98, 7.36, 6.60, 8.88, 6.05, 7.99, 7.35, 6.67,  6.45, 8.16, 
            7.59, 7.16, 8.42, 8.88, 6.27, 7.12, 6.18, 8.27,  7.42, 8.39, 8.58, 
            6.49, 7.21, 8.07, 6.61, 6.60, 6.27, 7.85,  7.95, 7.06, 5.32, 6.55, 
            6.29, 7.46, 6.77, 6.14, 7.18, 7.04,  6.80, 8.09, 6.23, 6.94, 6.58, 
            6.66, 6.37, 6.95, 7.13, 6.52)  
control <- c(4.72, 5.35, 5.88, 6.08, 4.96, 4.05, 5.01, 4.98, 4.57, 4.88,  5.46, 
             5.07, 4.68, 4.86, 5.17, 4.56, 4.71, 5.95, 5.18, 4.85,  4.92, 4.96, 
             5.22, 4.42, 4.99, 5.58, 5.35, 4.90, 5.55, 6.88,  4.93, 4.77, 5.05, 
             5.09, 5.81, 4.97, 5.18, 4.95, 4.52, 6.05,  5.51, 4.41, 5.29, 5.69, 
             5.07, 4.62, 5.01, 5.86, 5.46, 5.56,  4.86, 5.18, 5.39, 4.90, 5.22, 
             5.14, 4.99, 5.63, 5.11, 5.28, 5.71, 5.31, 5.18, 5.02, 4.88)

# Check normality
shapiro.test(cancer)
"
	Shapiro-Wilk normality test

data:  cancer
W = 0.972, p-value = 0.1182
"

shapiro.test(control)
"
	Shapiro-Wilk normality test

data:  control
W = 0.95724, p-value = 0.02468
"

# Check equal variances
exp <- c(cancer, control)
group <-  factor(c(rep("cancer", length(cancer)), rep("control", length(control))))
leveneTest(exp~group)

"
Levene's Test for Homogeneity of Variance (center = median)
       Df F value    Pr(>F)    
group   1   19.64 1.937e-05 ***
      133                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
"

wilcox.test(cancer, control, alternative = "greater", paired = FALSE)
"
	Wilcoxon rank sum test with continuity correction

data:  cancer and control
W = 4398, p-value < 2.2e-16
alternative hypothesis: true location shift is greater than 0"

# Since both normality and variance homogeneity assumptions are violated,
# we proceed with non-parametric Wilcoxon Rank Sum test.
# The results indicate that both BRCA1 expression is greater in the cancer samples compared
# to the control sample (the cancer distribution is shifted to the right of control distribution)


# Question 5

library(NHANES) 

data("NHANES")
str(NHANES) 

# We'll use BMI, SmokingStatus (e.g., SmokeNow), Gender 
df <- NHANES[, c("Gender","BMI","SmokeNow") ]

# Rename / factor 
df <- na.omit(df) 
df$SmokeNow <- factor(df$SmokeNow, levels=unique(df$SmokeNow)) 
df$Gender <- factor(df$Gender)

# 1. Check normality
shapiro.test(df$BMI)
"
	Shapiro-Wilk normality test

data:  df$BMI
W = 0.94856, p-value < 2.2e-16
"

# 2. Check equal variances
leveneTest(BMI ~ Gender * SmokeNow, data = df)
"
Levene's Test for Homogeneity of Variance (center = median)
        Df F value    Pr(>F)    
group    3  31.716 < 2.2e-16 ***
      3180                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
"

model <- art(BMI~Gender*SmokeNow, data = df)
summary(model)
"Aligned Rank Transform of Factorial Model

Call:
art(formula = BMI ~ Gender * SmokeNow, data = df)

Column sums of aligned responses (should all be ~0):
         Gender        SmokeNow Gender:SmokeNow 
              0               0               0 

F values of ANOVAs on aligned responses not of interest (should all be ~0):
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.00000 0.00000 0.00000 0.09402 0.12026 0.40376 "

anova(model)

"Analysis of Variance of Aligned Rank Transformed Data

Table Type: Anova Table (Type III tests) 
Model: No Repeated Measures (lm)
Response: art(BMI)

                  Df Df.res F value     Pr(>F)    
1 Gender           1   3180  6.4107   0.011391   *
2 SmokeNow         1   3180 49.7529 2.1305e-12 ***
3 Gender:SmokeNow  1   3180 22.0538 2.7634e-06 ***
---
Signif. codes:   0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 "


# Both tests suggest non-normality and unequal variances, that’s a signal that a parametric test might not be appropriate. 
# so we proceed with art test (non-parametric version of the two-way ANOVA)
# The test result shows that there is a significant effect on BMI for both Gender and Smoking status and also their interactions.
# it means that the BMI distribution differ in different gender groups and smoker/non-smoker groups, and also in smoker/non-smoker groups
# the BMI distribution differs based on the gender.



# Question 6

BRCA1_expression <- c(
  9.32, 11.14, 10.56, 7.83, 8.97, 12.25, 9.88, 11.47, 10.15, 8.44,
  9.76, 10.93, 11.38, 7.95, 9.05, 10.64, 8.71, 9.42, 11.77, 10.02,
  8.86, 10.57, 9.61, 11.03, 8.32, 10.74, 9.89, 12.05, 9.54, 10.11,
  8.59, 11.21, 10.45, 9.07, 8.95, 11.62, 10.28, 9.73, 11.09, 9.68
)

BRCA2_expression <- c(
  9.89, 11.92, 11.38, 8.54, 9.42, 12.97, 10.33, 12.21, 10.72, 9.07,
  10.14, 11.54, 11.89, 8.66, 9.77, 11.26, 9.42, 9.91, 12.44, 10.58,
  9.52, 11.35, 10.17, 11.73, 9.01, 11.48, 10.49, 12.74, 10.22, 10.87,
  9.33, 11.84, 11.03, 9.66, 9.47, 12.19, 10.97, 10.22, 11.56, 10.41
)

gender= factor(rep(c("female", "male"), each = 20))


data <- data.frame(
  B1 = BRCA1_expression,
  B2 = BRCA2_expression,
  Gender = gender
)


# Test that expression is normal in both samples with Shapiro–Wilk normality test
shapiro.test(BRCA1_expression)
"
	Shapiro-Wilk normality test

data:  BRCA1_expression
W = 0.98332, p-value = 0.8098
"

shapiro.test(BRCA2_expression)
"
	Shapiro-Wilk normality test

data:  BRCA2_expression
W = 0.97733, p-value = 0.5914
"
#whole sample
cor.test(data$B1, data$B2, method = "pearson")
"
	Pearson's product-moment correlation

data:  data$B1 and data$B2
t = 66.832, df = 38, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.9919635 0.9977788
sample estimates:
      cor 
0.9957731 
"

#only females
cor.test(data$B1[1:20]~data$B2[1:20], method = "pearson")
"
  Pearson's product-moment correlation

data:  data$B1 and data$B2
t = 66.832, df = 38, p-value < 2.2e-16
alternative hypothesis: true correlation is not equal to 0
95 percent confidence interval:
 0.9919635 0.9977788
sample estimates:
      cor 
0.9957731 
"
# Since the normality test confirmed that the differences have normal distribution, 
# we can proceed with pearson correlation coefficient. the test results for both whole samples and female only,
# show that there is a strong positive linear correlation between the BRCA 1 & 2 expression and these relations 
# are significant due to the significant p-value of both tests



# Question 7

hours <- c(2, 3, 4, 5, 1, 6, 7, 3, 4, 8, 2, 5, 6, 7, 8)
exam_rank <- c(15, 13, 12, 10, 14, 9, 7, 11, 12, 5, 14, 9, 8, 6, 4)

cor.test(hours, exam_rank, method = "spearman")
"
	Spearman's rank correlation rho

data:  hours and exam_rank
S = 1103.4, p-value = 2.175e-09
alternative hypothesis: true rho is not equal to 0
sample estimates:
       rho 
-0.9702766 
"

# The spearman correlation coefficient is used, since the data is ordinal and discrete,
# and the result shows a strong negative linear relationship between the students's rank and the study hours.
# and this relationship is significant due to the very low p-value of the test.



# Question 8

women_weight <- c(38.9, 61.2, 73.3, 21.8, 63.4, 64.6, 48.4, 48.8, 48.5)
men_weight <- c(67.8, 60, 63.4, 76, 89.4, 73.3, 67.3, 61.3, 62.4)

wilcox.test(women_weight, men_weight, paired = FALSE)
"
	Wilcoxon rank sum test with continuity correction

data:  women_weight and men_weight
W = 15, p-value = 0.02712
alternative hypothesis: true location shift is not equal to 0
"

# Since we wnat to compare the median of two samples, we proceed with wilcoxon rank sum test,
# and the test result shows that the distribution of weights are different in men and women, and one is 
# shifted to the left or to the right of the other.




# Question 9

Branch_A <- c(4, 5, 7, 6, 3, 4, 5)
Branch_B <- c(8, 9, 7, 8, 7, 8, 7)
Branch_C <- c(3, 2, 1, 4, 3, 2, 3)

df = data.frame(
  score = c(Branch_A, Branch_B, Branch_C),
  branch = factor(rep(c("A", "B", "C"), each = 7))
)


# 1. Check normality
shapiro.test(Branch_A)
"
	Shapiro-Wilk normality test

data:  Branch_A
W = 0.96664, p-value = 0.8733
"

shapiro.test(Branch_B)
"
	Shapiro-Wilk normality test

data:  Branch_B
W = 0.83338, p-value = 0.08614
"

shapiro.test(Branch_C)
"
	Shapiro-Wilk normality test

data:  Branch_C
W = 0.93662, p-value = 0.6085"

kruskal.test(score~branch, data = df)
"
	Kruskal-Wallis rank sum test

data:  score by branch
Kruskal-Wallis chi-squared = 16.388, df = 2, p-value = 0.0002763"

# The normality test confirmed that score are normally distributed, 
# but we proceed with the non-parametric kruskal wallis test, because the sample size is too small, and the parametric
# test may be unstable in this case.
# The test result shows a significant p-value, means that the different branches do not all have the same level of satisfaction.



# Question 10

Patient <- factor(1:5)
Drug_A <- c(8,7,9,10,8)
Drug_B <- c(6,5,8,8,7)
Drug_C <- c(8,8,8,7,7)
Drug_D <- c(5,6,6,7,7)

# Check normality 
shapiro.test(Drug_A - Drug_B)
"

	Shapiro-Wilk normality test

data:  Drug_A - Drug_B
W = 0.68403, p-value = 0.00647"

shapiro.test(Drug_A - Drug_C)
"
	Shapiro-Wilk normality test

data:  Drug_A - Drug_C
W = 0.95563, p-value = 0.7773"

shapiro.test(Drug_A - Drug_D)
"
	Shapiro-Wilk normality test

data:  Drug_A - Drug_D
W = 0.68403, p-value = 0.00647"

shapiro.test(Drug_B - Drug_C)
"
	Shapiro-Wilk normality test

data:  Drug_B - Drug_C
W = 0.91367, p-value = 0.4899"

shapiro.test(Drug_B - Drug_D)
"
	Shapiro-Wilk normality test

data:  Drug_B - Drug_D
W = 0.96086, p-value = 0.814"

shapiro.test(Drug_C - Drug_D)
"
	Shapiro-Wilk normality test

data:  Drug_C - Drug_D
W = 0.85191, p-value = 0.2006"


data <- matrix(
  c(Drug_A,
    Drug_B, 
    Drug_C, 
    Drug_D),
  ncol = 4,
)
colnames(data) <- c("A", "B", "C", "D")

friedman.test(data)
"
	Friedman rank sum test

data:  data
Friedman chi-squared = 9.2093, df = 3, p-value = 0.02663"

# Since the normality test is rejected in differences for some conditions,
# we proceed with non-parametric friedman test (~ repeated measure ANOVA)
# the test result indicates that the null hypothesis is rejected and the pain distributions of samples after using different drugs
# are not identical and may be shifted.


#parametric for practice:
data_df <- as.data.frame(data)
data_df$Patient_id <- Patient

data_long <- pivot_longer(
  data_df,
  cols = c("A", "B", "C", "D"),
  names_to = "drug",
  values_to = "pain"
)
data_long$drug = factor(data_long$drug)
data_long

library(rstatix)

anova_test(
  data = data_long,
  dv = pain,
  wid = Patient_id,
  within = drug
)

"ANOVA Table (type III tests)

$ANOVA
  Effect DFn DFd     F     p p<.05   ges
1   drug   3  12 5.789 0.011     * 0.462

$`Mauchly's Test for Sphericity`
  Effect     W     p p<.05
1   drug 0.277 0.645      

$`Sphericity Corrections`
  Effect   GGe     DF[GG] p[GG] p[GG]<.05   HFe      DF[HF] p[HF] p[HF]<.05
1   drug 0.629 1.89, 7.54 0.031         * 1.172 3.52, 14.06 0.011         *
"

#This test also has the significant p-value and the null hypothesis (identical distribution among 4 conditions) is rejected