load("AISurveySTATES.Rdata")
df = merged_df[merged_df$Treatment=="COMPANY",]
# 

df$Company_Q1 = factor(df$Company_Q1, ordered=F)
df$Company_Q2 = factor(df$Company_Q2, ordered=F)
df$Company_Q3 = factor(df$Company_Q3, ordered=F)
df$Company_Q4 = factor(df$Company_Q4, ordered=F)
#
df$Company_Q4 <- relevel(factor(df$Company_Q4), ref = "Neither agree nor disagree")
levels(df$Company_Q4)

library(MASS)

library(stargazer)
#
# 
changing_variable1 <- "SupportTransparency"
changing_variable2 <- "SupportAutonomous"
changing_variable3 <- "SupportBias"
#
FORM1 = "~ Company_Q4"
FORM2 = "~ Company_Q1+Company_Q2+Company_Q3+Company_Q4"
# FORM03 = "~ as.numeric(Company_Q4)+I(as.numeric(Company_Q4)^2)+Company_Q1+Company_Q2+Company_Q3"
# Construct the formula string
# 
formula_obj1 <- as.formula(paste(changing_variable1, FORM1))
formula_obj2 <- as.formula(paste(changing_variable2, FORM1))
formula_obj3 <- as.formula(paste(changing_variable3, FORM1))
# 
model1 <- polr(formula_obj1,data=df, method = "logistic", Hess=TRUE)
model2 <- polr(formula_obj2,data=df, method = "logistic", Hess=TRUE)
model3 <- polr(formula_obj3,data=df, method = "logistic", Hess=TRUE)
#
stargazer(model1, model2, model3, 
          title = "Ordinal Logistic Regression Results", type = "html", out = "models1.html")

#-----------------------------------------------------------------------------
changing_variable1 <- "as.numeric(SupportTransparency)"
changing_variable2 <- "as.numeric(SupportAutonomous)"
changing_variable3 <- "as.numeric(SupportBias)"
#
# Construct the formula string
# 
formula_obj1 <- as.formula(paste(changing_variable1, FORM1))
formula_obj2 <- as.formula(paste(changing_variable2, FORM1))
formula_obj3 <- as.formula(paste(changing_variable3, FORM1))
#
model1 <- lm(formula_obj1,data=df)
model2 <- lm(formula_obj2,data=df)
model3 <- lm(formula_obj3,data=df)

stargazer(model1, model2, model3, 
          omit=c("state","Sector","Degree", "black", "white", "asian", "hispanic", "GenderMale", "FirmSize", "UseAIFirmYes","AIImpactFirm"), 
          title = "Linear Regression Results", type = "html", out = "models2.html")
#
# --------------------------------------------------------------------------------------------------------
# 

