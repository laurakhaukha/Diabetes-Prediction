# -----------------------------------------------------------------------------#
# Created by Laura Lisa Khaukha
# Please Note: Load all the packages beforehand

# -----------------------------------------------------------------------------#
#  #Packages installed, corresponding libraries used
# -----------------------------------------------------------------------------#
#Installing the relevant packages
install.packages("tidyverse")
install.packages("tableone")
install.packages("haven")
install.packages("boot")
install.packages("car")
install.packages("corrplot")
install.packages('nortest')
install.packages('splines')
install.packages("pROC")
install.packages("skimr")
install.packages("rstatix")
install.packages("flextable")
install.packages("caret")
install.packages("ggplot2")
install.packages("survey")
install.packages("ipw")
install.packages("ggdag")
install.packages("lmtest")
install.packages("zoo")
install.packages("boot")

# Loading the corresponding libraries
library(tidyverse)
library(tableone)     
library(haven)
library(boot)
library(car)
library(corrplot)
library(nortest)
library(splines)
library(pROC)
library(rstatix)
library(flextable)
library(caret)
library(ipw)
library(survey)
library(ggplot2)
library(ggdag)
library(lmtest)
library(zoo)
library(boot)

# -----------------------------------------------------------------------------#
#   1   # Data Preperation 
# -----------------------------------------------------------------------------#
#Setting Working Directory to location of assignment & Loading Data for 
setwd("~/Desktop/") # Working Directory remained 
df <- read_dta('#') 

#Converting each variables to numeric values for consistency throughout analyis
df <- df %>% mutate(across(c(pregnancies, glucose, insulin, skin_thickness, blood_pressure, bmi, diabetes_genetic_score, age),as.numeric))
# -----------------------------------------------------------------------------#
#   2  # DATA EXPLORATION & Establishment of Descriptive statistics 
# -----------------------------------------------------------------------------#
#Initial Insight of Data
view(df) #Viewing the data-set (df)
str(df) #Reviewing the structure of the data-set

#Creating table One/Summary statistics table for continuous variables, to identify potential abnormalities
names(df) #Looking up variable names

#Creating corresponding Variables, in preparation for Table1 creation
cv<- c('pregnancies','glucose','insulin','skin_thickness','blood_pressure', "bmi","diabetes_genetic_score","age") 
strata <- 'diabetes' #Stratifying by 
vars <- c(cv)

Table1_ <- CreateTableOne(data = df,strata = strata, vars = cv) 
print(Table1_) 

#Converting Table1_ into a data frame, in preparation for Export out of R for display in report
Table1_df <- as.data.frame(print(Table1_, quote = FALSE, noSpaces = TRUE))
write.csv(Table1_df, file = "TB1.csv", row.names = TRUE) # Exporting Table 1 

#Establishing full set of descriptive statistics for each variable or column
ss <- df %>%
  get_summary_stats(
    pregnancies, glucose, insulin, skin_thickness, blood_pressure, bmi, diabetes_genetic_score, age,
    type = "common"
  )
#Printing Summary stats Table for analysis
print(ss)

#Creating a summary table (Descriptive Statistics) to display in the report in Section 2.2.2
ss_ <- flextable(ss)
save_as_docx(ss_, path = "Descriptive_Statistics.docx") #Exporting Table out of R

######Distribution Analysis for continuous variables (Detecting potential skewness & visually evident outliers)

#Selecting and assigning columns (variables) to the variable, nv
nv <- df %>% select('pregnancies','glucose','insulin','skin_thickness','blood_pressure', "bmi","diabetes_genetic_score","age")

#Generating histograms via ggplot, hereby using a for loop to avoid repetitive code and lengthy script
for (var in colnames(nv)) {
  nv_hist <- ggplot(df, aes_string(x = var)) + geom_histogram(fill = "lightpink", color = "black", bins = 30) +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme_minimal()
  print(nv_hist)
}

# Q-Q Plot, to further confirm the distribution and observed skewness of the variables,using a for loop to avoid repetitive code and lengthy script
for (var in colnames(nv)) {
  print( ggplot(df, aes_string(sample = var)) + stat_qq(color = "blue") + stat_qq_line(color = "red") +
           labs(title = paste("QQ-Plot of", var), x = "Theoretical Quantiles", y = "Sample Quantiles") +
           theme_minimal()
  )
}

#Performing the Anderson-Darling test on Each Variable to quantify the visually evident skewness observed, seen in the histogram and Q-Q plot above. 
ad_t <- sapply(nv, function(x) {
  if (length(na.omit(x)) > 3) {
    ad.test(x)$p.value
  } else {
    NA
  }
})

# Creating and printing a data-frame (df) for the Anderson-Darling test results, displaying each obtained P-value
ad_df <- data.frame(Variable = names(ad_t), P_Value = ad_t)
print(ad_df)

#Distribution Analysis Observations:
#Detected Skewed Variables via visual inspection(Q-Q Plot, Histogram): Age, diabetes_genetic_score, bmi, skin_thickness, insulin
#The skewness of these variables, were suspected to be due to  outliers : relevant transformations are considered in section 6
#Observation of Anderson-Darling test: All the variables do not follow normal distribution 

#Visually inspecting for potential outliers via Box-plots for each variable(nv), via for loop  
for (var in colnames(nv)) {
  print(
    ggplot(df, aes_string(y = var)) +
      geom_boxplot(fill = "lightpink", color = "black", outlier.colour = "red", outlier.shape = 16) +
      labs(title = paste("Boxplot of", var), y = var) +
      theme_minimal()
  )
}

# -----------------------------------------------------------------------------#
#   3 #Model Selection (Logistic Regression) & Feature Analysis
# -----------------------------------------------------------------------------#

# Logistic regression modeling (relationship/correlation analysis), performed as diabetes is a binary outcome 
lm_1 <- glm(diabetes ~ bmi + glucose + diabetes_genetic_score + age 
            + pregnancies + blood_pressure +  skin_thickness + insulin, data = df, 
            family = binomial)

#Displaying summary of lm_1
summary(lm_1) #AIC: 2606.5, reference point for model performance improvement/worsening

# Converting the Coefficients to Odds Ratios for easier interpretation of their Association 
exp(coef(lm_1))
#Observation:The Odds Ratios suggest that Blood-pressure & insulin have a protective(inverse) affect over diabetes
#Plausible exclusion of Blood-pressure & insulin, if inverse effect remains after variable transformation & Outlier removal

#Re-plotting the logistic regression, without blood-pressure and insulin as predictors
Lm_2 <- glm(diabetes ~ bmi + glucose + diabetes_genetic_score + age + pregnancies + skin_thickness, data = df, family = binomial)

print(Lm_2) 
summary(Lm_2) #AIC: slightly worsened to 2616 (increased AIC = worsened model peformance)

# -----------------------------------------------------------------------------#
#                 4 # OUTLIER & SENSITIVITY ANALYSIS 
# -----------------------------------------------------------------------------#
#Calculating Cooks distance lm_1, including blood pressure and  
cd <- cooks.distance(lm_1)
print(cd)

#Setting the threshold for cooks distance 
th <- 4 / nrow(df)  # 4/n was deemed overly sensitive, flagging too many points
th_2 <- 20 / nrow(df) #Seemed better healthcare context of the data

#Plotting the Cooks distance (cd), the threshold (th:(4/n )
plot(cd, pch = 10, main = "Cook's Distance for lm_1 (Logistic Regression)", 
     xlab = "Observation Index", ylab = "Cook's Distance")
abline(h = th, col = "darkred", lty = 2)  

text(x = 1:length(cd), y = cd, 
     labels = ifelse(cd > th, 1:length(cd), ""), 
     col = "blue", cex = 0.7, pos = 3)

####### Transporting/saving Cooks distance into local working directiory
png("cdlot.png", width = 1200, height = 1200, res = 150)

#Re-plotting the Cooks distance(cd) using the adapted threshold th_2 (20/n )
plot(cd, pch = 10, main = "Cook's Distance for the Logistic Regression", lwd = 2,  cex.main = 2, cex = 2,
     xlab = "Observation Index", ylab = "Cook's Distance", cex.lab = 1.5, cex.axis = 1.5)
abline(h = th_2, col = "darkred", lty = 2, lwd = 5)  

text(x = 1:length(cd), y = cd, 
     labels = ifelse(cd > th_2, 1:length(cd), ""), 
     col = "blue", cex = 1.5, pos = 3)

dev.off()

#Observation: ~5 or more patient IDs were flagged, to have abnormal(influential) values present inverse variables(columns)
#Printing the max and summary cooks distance values
summary(cd) 
max(cd)

#Assigning Custom Threshold for the sub-setting of the influential data-point observed via Cooks distance, above
ct <- 0.01  #0.01, to isolate the most influential data-points observed above, these were above 0.01

#Identifying/ Inspecting & Isolating the influential observations above the custom threshold (0.1)
ipc <- which(cd > ct)
print(ipc)
ipcc <- df[ipc, ] #Sub-setting influential points 
view(ipcc) # Viewing the data(rows), corresponding to the influential points

#Plotting a Residual vs Leverage Plot to visually inspect leverage
par(mfrow = c(1, 1))  
plot(lm_1, which = 5, 
     main = "Residuals VS Leverage Plot")

#leverage analysis for influential points 
l <- hatvalues(lm_1)

#Grouping both the resulting leverage and Cook's Distance for influential points 
l_analysis <- data.frame(Observation = ipc, Cook_Distance = cd[ipc], Leverage = l[ipc])
print(l_analysis) #Printing the noticeable data points that appear influential yet have a low leverage

################### Sensitivity Analysis

# Removing the influential points from the data-set, creating a new data-set
d_part_clean <- df[-ipc, ]

# Refit the logistic regression model with insulin and blood_pressure
Log_model_clean <- glm(diabetes ~ bmi + glucose + diabetes_genetic_score + age +
                         pregnancies + blood_pressure + skin_thickness + insulin, 
                       data = d_part_clean, family = "binomial")

#Viewing summary of Log_model_clean
summary(Log_model_clean) 
#Observation: Reduction in AIC value to 2545.1 

#Refit the logistic regression model without insulin and blood_pressure
Log_model_clean_2 <- glm(diabetes ~ bmi + glucose + diabetes_genetic_score + age +
                           pregnancies + skin_thickness, data = d_part_clean, family = "binomial")
summary(Log_model_clean_2) #Observation: AIC: 2550.9 from model without outliers & insulin,blood_pressure appears lower

#Checking Odds Ratios 
print(exp(coef(Log_model_clean))) #Observation: Insulin and blood_pressure still have an inverse effect 

# -----------------------------------------------------------------------------#
#         # 5 Checking Linearity of independent variables and Log-odds 
# -----------------------------------------------------------------------------#
#Creating variable(v) including all variables within data-set, in preparation for LOESS plot creation
v <- c('pregnancies', 'glucose', 'skin_thickness', 'bmi', 'diabetes_genetic_score', 'age', 'blood_pressure', 'insulin')

#Adding the fitted log-odds to the data-set using the logistic regression model,without outliers
d_part_clean$lo_fitted <- predict(Log_model_clean, type = "link")

#Defining the bandwidth for LOESS, using 0.8, instead of default 0.75
lb <- 0.8

#Creating LOESS plots for each predictor
for (var in v) {
  p <- ggplot(d_part_clean, aes_string(x = var, y = "lo_fitted")) +
    geom_point(alpha = 0.5, color = "blue") +  
    geom_smooth(method = "loess", color = "red", se = FALSE, span = lb) + 
    labs(
      title = paste("LOESS Smoothing. Fitted Log-Odds vs", var),
      x = var,
      y = "Fitted Log-Odds"
    ) +
    theme_minimal()
  print(p)
}

#Observations from LOESS plots: 
#Linear: Glucose, BMI (almost-linear trend)
#Non- Linear: Blood-pressure, insulin, skin thickness, diabetic genetic score, Age

# -----------------------------------------------------------------------------#
#  #6 Transforming the variables, violating the linearity assumption  
# -----------------------------------------------------------------------------#

#Selecting and assigning each column(variable) from the cleaned data-set to nv_clean, in preparation for subsequent distribution analysis
nv_clean  <- d_part_clean%>% select('pregnancies','glucose','insulin','skin_thickness',
                                    'blood_pressure', "bmi","diabetes_genetic_score","age")

#Evaluating the type of Transformations necessary for non-linear predictors, observed via LOESS plots above
#Re-plotting Histograms for each predictor via for loop, to examine their distribution without outliers
for (var in colnames(nv_clean)) {
  p <- ggplot(d_part_clean, aes_string(x = var)) +
    geom_histogram(fill = "lightpink", color = "black", bins = 30) +
    labs(title = paste("Histogram of", var), x = var, y = "Frequency") +
    theme_minimal()
  print(p)
}

#Observations: 
#Right skewed:BMI, age, genetic score, skin thickness,insulin 
#Normally Distributed: Blood pressure, Pregnancies (Require factorization), Glucose 

############### Pregnancies
#Converting Pregnancies into factor (categories) since its range is limited (0-5), additionally as it was identified as non-linear in the LOESS plot
d_part_clean$pregnancies <- factor(d_part_clean$pregnancies)

#Refitting the model 
Lm_clean3 <- glm(diabetes ~ bmi + glucose + diabetes_genetic_score + age +
                   pregnancies + skin_thickness, data = d_part_clean, family = "binomial")
summary(Lm_clean3) #AIC: 2527.3 Improved from last AIC of 2545.1 (Log_model_clean)

############### Blood_pressure

#Categorizing Blood Pressure into clinically significant groups, see report table 2 for reference.
d_part_clean$BP_Category <- cut(
  d_part_clean$blood_pressure,
  breaks = c(-Inf, 80, 90, 100,Inf), 
  labels = c("Normal", 'Prehypertension', "Hypertension Stage 1", "Hypertension Stage 2") 
)
#Viewing the number of entries within each blood pressure category
table(d_part_clean$BP_Category) 

#Re-plotting the model with BP_Category
Lm_BP <- glm(diabetes ~ ns(skin_thickness, df = 3) + BP_Category + glucose + bmi + 
               diabetes_genetic_score + age + pregnancies + insulin, data = d_part_clean, family = "binomial")
summary(Lm_BP) #AIC: 2498.2, Notable improvement

#Re-assessing Odds ratio of Blood pressure categories
OR <- exp(coef(Lm_BP))
print(OR)
#Observation: Confirmation that blood pressure has an a (persistent) protective affect over diabetes despite categorization. 
#Final Decision: As the outlier analysis and categorizing of blood pressure variable, did not reverse its inverse affect on diabetes.
#Blood pressure was excluded from the model from this point onward, despite AIC improvements.

############### Insulin
#Applying a log transformation on insulin, as the distribution of Insulin was greatly skewed to the right, as seen earlier.
d_part_clean$l_insulin <- log(d_part_clean$insulin + 1) #Adding a constant +1 to mitigate the 0 values in the insulin columns

#Looking at the distribution of insulin upon the log-transformation
hist(d_part_clean$l_insulin, main = "Histogram of Log-Transformed Insulin",
     xlab = "Log(Insulin)", col = "lightpink", border = "black")
#Observation: Zero values split distribution

#Re-plotting the logistic regression model with log_transformed insulin
Lm_insulin <- glm(diabetes ~ l_insulin + bmi + glucose + diabetes_genetic_score + age + pregnancies + skin_thickness,
                  data = d_part_clean, family = "binomial")

#Viewing the summary of Lm_insulin
summary(Lm_insulin) #AIC: 2516, worsened

#Re-assessing Odds ratio of Blood pressure categories
OR_2 <- exp(coef(Lm_insulin)) 
print(OR_2) #Observation: Persisting Inverse affect of insulin on diabetes despite log transformation.

#Final Decision: As the outlier analysis and log-transformation insulin did not resolve its inverse affect on diabetes
#Insulin was excluded from the model from this point onward, despite AIC improvements.

############### BMI
#Applying a log transformation on BMI 
d_part_clean$l_bmi <- log(d_part_clean$bmi)

#Investigating BMIs' distribution upon log transformation
hist(d_part_clean$l_bmi, main = "Histogram of Log-Transformed BMI", xlab = "Log(BMI)", col = "lightblue", border = "black")
#Observation: Looks normally distributed

#Checking whether BMIS' non-linearity has been resolved after log-transformation with log-odds, LOESS
ggplot(d_part_clean, aes(x = l_bmi, y = lo_fitted)) +
  geom_point(alpha = 0.5, color = "steelblue") + geom_smooth(method = "loess", color = "darkred", se = FALSE) + 
  labs(title = "LOESS Smoothing: Fitted Log-Odds vs Log(BMI)", x = "Log(BMI)", y = "Fitted Log-Odds") +
  theme_minimal()
#Observation: Appears Linear

#Re-plotting the model
Lm_lb <- glm(diabetes ~ l_bmi + glucose + diabetes_genetic_score + age + pregnancies + skin_thickness, data = d_part_clean, family = "binomial")
summary(Lm_lb) # AIC improved from 2527.3 (Log_model_clean_3) to 2515.9

############### Skin_thickness:

#Applying Log transformation on non-linear skin_thickness as it is right skewed
d_part_clean$l_skin_thickness <- log(d_part_clean$skin_thickness + 1)
hist(d_part_clean$l_skin_thickness) 
#Observation: Less Skewed, distribution is normal upon log-transformation

#Checking whether non-linearity of Skin thickness is resolved after log transformation
ggplot(d_part_clean, aes(x = l_skin_thickness, y = lo_fitted)) + geom_point(alpha = 0.5, color = "blue") + geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  labs(title = "LOESS Smoothing: Fitted Log-Odds vs Log(Skin Thickness)",
       x = "Log(Skin Thickness)", y = "Fitted Log-Odds") +
  theme_minimal()
#Observation: Skin-tickness remains non-linear, despite log-transformation

#Thus, Square root transformation on skin_thickness applied, as log-transformation did not resolve non-linearity.
d_part_clean$sqrt_skin_thickness <- sqrt(d_part_clean$skin_thickness)

#Investigating linearity assumption upon square root transformation of Skin_tickness via LOESS
ggplot(d_part_clean, aes(x = sqrt_skin_thickness, y = lo_fitted)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  labs(title = "LOESS Smoothing: Fitted Log-Odds vs Square Root of Skin Thickness",
       x = "sqrt(Skin Thickness)", y = "Fitted Log-Odds") +
  theme_minimal()

#Observation:Non-linearity of Skin_ticknes despite Square root transformation 
#Both transformations seemingly didn't resolve non-linearity violation for skin thickness.
#These were initial hints towards plausible interactions of Skin_tickness with another variable, and a more complex relationship between skin thickness and diabetes.
#Plausible interactions were explored via a correlation matrix below, as well as further transformation methods

#Re-plotting Logistic Regression, using splines to account for non-linearity of skin thickness
Lm_s <- glm(diabetes ~ ns(skin_thickness, df = 3) + l_bmi + glucose + 
              diabetes_genetic_score + age + pregnancies, data = d_part_clean, family = "binomial")
#Viewing summary of 
summary(Lm_s) # AIC 2492.5 improved from 2515.9 


############### Diabetes genetic score:

#Appliying log-transformation for the diabetes genetic score
d_part_clean$l_diabetes_genetic_score <- log(d_part_clean$diabetes_genetic_score)

#Viewing the histogram upon log transformation for diabetes genetic score
hist(d_part_clean$l_diabetes_genetic_score, main = "Histogram of Log-Transformed Insulin",
     xlab = "Log od Insulin", col = "lightpink", border = "black") 
#Observation: Log-transformation,successfully applied

#Re-ploting LOESS to check whether non-linearity was resolved non-linearity issue with diabetes genetic score
ggplot(d_part_clean, aes(x = l_diabetes_genetic_score, y = lo_fitted)) + geom_point(alpha = 0.5, color = "blue") + geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  labs(title = "LOESS Smoothing: Fitted Log-Odds vs log_diabetes_genetic_score", x = "log_diabetes_genetic_score", y = "Fitted Log-Odds") +
  theme_minimal()
#Observation: Non-linearity of diabetes genetic score was not resolved

#Square root transformation on skin_thickness applied instead as log-transformation did not resolve non-linearity.
d_part_clean$sqrt_diabetes_genetic_score <- sqrt(d_part_clean$diabetes_genetic_score)

#Investigating linearity assumption upon square root transformation of diabetes_genetic_score via LOESS
ggplot(d_part_clean, aes(x = sqrt_diabetes_genetic_score, y = lo_fitted)) + geom_point(alpha = 0.5, color = "blue") + geom_smooth(method = "loess", color = "darkred", se = FALSE) +
  labs(title = "LOESS Smoothing: Fitted Log-Odds vs sqrtd iabetes_genetic_score",
       x = "sqrt of diabetes_genetic_score", 
       y = "Fitted Log-Odds") +
  theme_minimal()
#Observation: Non-linearity not resolved for diabetes_genetic_score

#Thus, re-plotting Logistic Regression, using splines to account for the non-linearity of diabetes_genetic_score 
Lm_ss<- glm(diabetes ~ ns(diabetes_genetic_score, df = 3) + l_bmi + glucose + 
              ns(skin_thickness, df = 3) + age + pregnancies, data = d_part_clean, family = binomial)
# Display the summary of the updated model
summary(Lm_ss)  #AIC:  2493.2, increased very slightly with the addition of splines of diabetes_genetic_score 

############### Age 
#For age splines were used, 
Lm_sss <- glm(diabetes ~ ns(diabetes_genetic_score, df = 3) + l_bmi + glucose + ns(skin_thickness, df = 3) + 
                ns(age, df = 3) + pregnancies, data = d_part_clean, family = binomial)
# Display the summary of the updated model
summary(Lm_sss) # AIC: 2396.2, noticeably decreased (improved) upon adding splines for Age

# -----------------------------------------------------------------------------#
#  # 7  Multi-collinearity Analysis via Correlation matrix & Interaction Term 
# -----------------------------------------------------------------------------#
# Computing the correlation matrix
c_m <- cor(nv_clean , use = "pairwise.complete.obs")

#Printing the correlation matrix for inspection
print(c_m)

#Employing Variance Inflation Factors (VIF) to quantify plausible Multi-collinearity within the latest model
vif(Lm_sss) #Observation: No Multi-collinearity detected

#Replotting model adding an interaction term between skin_thickness and l_bmi
Lm_i <- glm(diabetes ~ ns(diabetes_genetic_score, df = 3) + l_bmi * ns(skin_thickness, df = 3) + glucose + 
              ns(age, df = 3) + pregnancies, data = d_part_clean,  family = binomial)

# Display the summary of the updated model
summary(Lm_i) #AIC:  2376.4, decreased 
#Observation:Significant interaction found between skin thickness and BMI

#Checking whether the interaction term has impacted Multicollinearity
vif(Lm_i) 
#Observation: Severe Multicollinearity detected between interaction term bmi:skin_thickness and ns(skin_thickness, df = 3) 
#Final Decision: Removal of Interaction term as model assumption was violated

# -----------------------------------------------------------------------------#
#        # 8 Internal Validation, Model/Data Training 
# -----------------------------------------------------------------------------#
#Setting a seed for reproducibility 
set.seed(123)

# Data Splitting Into 70% and 30% Split for Model Evaluation, assessing models ability to generalize to unseen data
#Generating training and testing dataset
ti <- createDataPartition(d_part_clean$diabetes, p = 0.7, list = FALSE)
td <- d_part_clean[ti, ]
t_data <- d_part_clean[-ti, ]

# Training the final Logistic regression 
f_lm <- glm(diabetes ~ ns(diabetes_genetic_score, df = 3) + l_bmi + glucose
            + ns(skin_thickness, df = 3) + ns(age, df = 3) + pregnancies, data = t_data, family = binomial)

#Viewing the summary of the final model
summary(f_lm) #AIC immensly improved to 737.44

# Predicting the probabilities on the 30% data tested (Tested Data)
t_data$pp <- predict(f_lm, newdata = t_data, type = "response")
# -----------------------------------------------------------------------------#
#           #9 Evaluation of Final Model Performance
# -----------------------------------------------------------------------------#
# Calculating the Receiver Operating Characteristic (ROC) curve
rc <- roc(t_data$diabetes, t_data$pp)

#Transporting/saving the subsequent plot as png into local working directiory
png("ROCPartA7.png",  width = 1200, height = 1200, res = 150)

#Plotting the ROC curve 
plot(rc, col = "blue", main = "ROC Curve for Final Model",
     lwd = 5,  cex.main = 2,
     xlab = "Specificity", ylab = "Sensitivity", cex.lab = 1.5,
     cex.axis = 1.5)

# Adding a diagonal line for random guessing
abline(a = 0, b = 1, col = "darkred",lty = 2, lwd = 5)
#Stopping the graphing 
dev.off()

#From the ROC curve above, calculating the AUC
auc_v <- auc(rc)
print(auc_v) #AUC: 0.8643

#Identifying and calculating the final models optimal threshold
opt <- coords(rc, "best", ret = "threshold")
print(opt) #Optimal Threshold: 0.2479577

# Creating a confusion matrix at the optimal threshold:  0.2479577, to view TP, TN, FP, FN
cmm <- table(Predicted = t_data$pp >= 0.2479, Actual = t_data$diabetes)
print(cmm) #Printing the Confusion matrix

#Calculating Sensitivity and specificity using the confusion matrix
sen <- cmm[2, 2] / (cmm[2, 1] + cmm[2, 2])
spen <- cmm[1, 1] / (cmm[1, 1] + cmm[1, 2])
print(sen) # Sensitivity: 0.5959079
print(spen) # Specificity: 0.9084668
