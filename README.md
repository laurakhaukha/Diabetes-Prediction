# Diabetes-Prediction 📊
## Predicting Diabetes Using Clinical and Biological Markers (Logistic Regression Model)

# Overview 👩🏽‍💻
This analysis builds a predictive model for diabetes using clinical and biological features from a synthetic dataset from UCL designed to mimic electronic health records (EHR). A logistic regression model is developed, refined, and evaluated, including outlier analysis, variable transformation, and discrimination assessment using R.

# Key Variables Used 🏨
- Pregnancies
- Blood Glucose Concentration (BGC)
- Insulin Levels
- Triceps Skinfold Thickness
- Diastolic Blood Pressure (DBP)
- Body Mass Index (BMI)
- Diabetes Genetic Score (DGS)
- Age

# Workflow 🔨
- Exploratory Data Analysis and Summary Statistics
- Outlier Detection and Removal using Cook’s Distance
- Multicollinearity Check (Correlation Matrix + VIF)
- Non-linearity Checks via LOESS Plots
- Variable Transformation & Categorisation
- Logistic Regression Model Training (70/30 split)
- Model Evaluation:
  ROC Curve
  Sensitivity / Specificity
  AUC Score
