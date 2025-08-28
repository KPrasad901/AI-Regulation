load("AISurveySTATES.Rdata")
# 
#
library(MASS)
library(forcats)
library(stargazer)
#
# Subset to consumer treatment and set order of levels
df = merged_df[merged_df$Treatment=="CONSUMER",]
df$Consumer_Q1 = factor(df$Consumer_Q1, ordered=T)
df$Consumer_Q2 = factor(df$Consumer_Q2, ordered=T)
df$Consumer_Q3 = factor(df$Consumer_Q3, ordered=T)
df$Consumer_Q4 = factor(df$Consumer_Q4, ordered=T)
df$Consumer_Q1 <- fct_rev(df$Consumer_Q1)
df$Consumer_Q2 <- fct_rev(df$Consumer_Q2)
df$Consumer_Q3 <- fct_rev(df$Consumer_Q3)
df$Consumer_Q4 <- fct_rev(df$Consumer_Q4)
# 
changing_variable1 <- "SupportTransparency"
changing_variable2 <- "SupportAutonomous"
changing_variable3 <- "SupportBias"
#
FORM2 = "~ as.numeric(Consumer_Q1)+as.numeric(Consumer_Q2)+as.numeric(Consumer_Q3)+as.numeric(Consumer_Q4)"
#
# Construct the formula string
# 
formula_obj3a <- as.formula(paste(changing_variable1, FORM2))
formula_obj3b <- as.formula(paste(changing_variable2, FORM2))
formula_obj3c <- as.formula(paste(changing_variable3, FORM2))
# Fit the proportional odds model
# 
model1 <- polr(formula_obj3a,data=df, method = "logistic", Hess=TRUE)
model2 <- polr(formula_obj3b,data=df, method = "logistic", Hess=TRUE)
model3 <- polr(formula_obj3c,data=df, method = "logistic", Hess=TRUE)
#
stargazer(model1, model2, model3, title = "Ordinal Logistic Regression Results", type = "html", out = "models_comparison.html")
#
