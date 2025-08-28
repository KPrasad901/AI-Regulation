#
# This file computes Table 3 of the paper (Proportional Odds Model) as well as 
# the linear regression results in Supplementary Materials 5
#
# Load the data
load("AISurveySTATES.Rdata")
library(dplyr)
#
###############################################################################
# -------------------------------------------------------------------------------------
#                         Ordered Logit
# -------------------------------------------------------------------------------------
# 
library(MASS)
library(brant)
merged_df$UseAIFirmYes = ifelse(merged_df$UseAIFirm=="Yes",1,0)
merged_df$GenderMale = ifelse(merged_df$Gender=="Male",1,0)
#
merged_df$margindummy = ifelse(merged_df$margin>0,1,0)
merged_df$Treatment = factor(merged_df$Treatment, levels = c("CONSUMER", "COMPANY"))
# 
merged_df$ExperienceN = as.numeric(merged_df$Experience)
merged_df$ConcernSelf = factor(merged_df$ConcernSelf, ordered=T)
merged_df$TrustGovN = as.numeric(merged_df$TrustGov)
merged_df$TrustExecsN = as.numeric(merged_df$TrustExecs)
merged_df$RegProtectionN = as.numeric(merged_df$RegProtection)
merged_df$RegBurdenN = as.numeric(merged_df$RegBurden)
merged_df$AIImpactFirmN = as.numeric(merged_df$AIImpactFirm)
#
merged_df$TrustGovHigh = ifelse(as.numeric(merged_df$TrustGov)>4,1,0)
merged_df$ConcernSelfHigh = ifelse(as.numeric(merged_df$ConcernSelf)>4,1,0)
# 
merged_df$Sector = factor(merged_df$Sector)
merged_df$state = factor(merged_df$state)
merged_df$Degree = factor(merged_df$Degree)
merged_df$FirmSize = as.numeric(merged_df$FirmSize)

#
changing_variable <- "SupportTransparency"
#
FORM0 = "~ Treatment"
FORM2 = "~ Treatment+ExperienceN+Sector+state+ Degree+
             InvolvementAI+TrustGovHigh+TrustExecsN+RegProtectionN+RegBurdenN+AIImpactFirm+
              black+white+asian+hispanic+GenderMale+FirmSize+UseAIFirmYes+ConcernSelfHigh"
# Construct the formula string
# 
#
formula_obj0 <- as.formula(paste(changing_variable, FORM0))
#
formula_obj2 <- as.formula(paste(changing_variable, FORM2))
# Fit the proportional odds model
# 
model0T <- polr(formula_obj0,data=merged_df, method = "logistic", Hess=TRUE)
#
model2T <- polr(formula_obj2,data=merged_df, method = "logistic", Hess=TRUE)

#--------------------------------------------------------------------------------
changing_variable <- "SupportAutonomous"
formula_obj0 <- as.formula(paste(changing_variable, FORM0))
#
formula_obj2 <- as.formula(paste(changing_variable, FORM2))
# Fit the proportional odds model
# 
model0A <- polr(formula_obj0,data=merged_df, method = "logistic", Hess=TRUE)
#
model2A <- polr(formula_obj2,data=merged_df, method = "logistic", Hess=TRUE)
#--------------------------------------------------------------------------------
changing_variable <- "SupportBias"
formula_obj0 <- as.formula(paste(changing_variable, FORM0))
#
formula_obj2 <- as.formula(paste(changing_variable, FORM2))
# Fit the proportional odds model
# 
model0B <- polr(formula_obj0,data=merged_df, method = "logistic", Hess=TRUE)
#
model2B <- polr(formula_obj2,data=merged_df, method = "logistic", Hess=TRUE)
#
brant(model0T,by.var=F)
brant(model0A,by.var=F)
brant(model0B,by.var=F)
#
brant(model2T,by.var=F)
brant(model2A,by.var=F)
brant(model2B,by.var=F)
# Now use stargazer to create a table
library(stargazer)
stargazer(model0T, model2T, model0A, model2A, model0B, model2B, 
#          omit = c("state"),
          omit=c("state","Sector","Degree", "black", "white", "asian", "hispanic", "GenderMale", "FirmSize", "UseAIFirmYes", "AIImpactFirm"), 
          title = "Ordinal Logistic Regression Results", type = "html", out = "models_comparison.html")
################################################################################################
# Linear Regression
#----------------------------------------------------------------------------------------------
changing_variable <- "as.numeric(SupportTransparency)"
#
FORM0 = "~ Treatment"
FORM2 = "~ Treatment+Sector+ state+Degree+ExperienceN+
             InvolvementAI+TrustGovHigh+TrustExecsN+RegProtectionN+RegBurdenN+AIImpactFirm+
              black+white+asian+hispanic+GenderMale+as.numeric(FirmSize)+UseAIFirmYes+ConcernSelfHigh"
# Construct the formula string

merged_dfb = subset(merged_df, merged_df$Degree != "Prefer not to say")
#
formula_obj0 <- as.formula(paste(changing_variable, FORM0))
#
formula_obj2 <- as.formula(paste(changing_variable, FORM2))
# Fit the linear model
# 
model0T <- lm(formula_obj0,data=merged_df)
#
model2T <- lm(formula_obj2,data=merged_df)
#--------------------------------------------------------------------------------
changing_variable <- "as.numeric(SupportAutonomous)"
formula_obj0 <- as.formula(paste(changing_variable, FORM0))
#
formula_obj2 <- as.formula(paste(changing_variable, FORM2))
# Fit the proportional odds model
# 
model0A <- lm(formula_obj0,data=merged_df)
#
model2A <- lm(formula_obj2,data=merged_df)
#--------------------------------------------------------------------------------
changing_variable <- "as.numeric(SupportBias)"
formula_obj0 <- as.formula(paste(changing_variable, FORM0))
#
formula_obj2 <- as.formula(paste(changing_variable, FORM2))
# Fit the proportional odds model
# 
model0B <- lm(formula_obj0,data=merged_df)
#
model2B <- lm(formula_obj2,data=merged_df)
#
# Now use stargazer to create a table
library(stargazer)
stargazer(model0T, model2T, model0A, model2A, model0B, model2B, 
          omit=c("state","Sector","Degree", "black", "white", "asian", "hispanic", "GenderMale", "FirmSize", "UseAIFirmYes","AIImpactFirm"), 
          title = "Linear Regression Results", type = "html", out = "models_comparisonLR.html")
################################################################################################