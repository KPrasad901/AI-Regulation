# Load the data
load("AISurveySTATES.Rdata")

#
###############################################################################
# -------------------------------------------------------------------------------------
#                         Ordered Logit/Probit
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
merged_df$ExperienceHigh = ifelse(as.numeric(merged_df$Experience)>3,1,0)
merged_df$ConcernSelfHigh = ifelse(as.numeric(merged_df$ConcernSelf)>3,1,0)
merged_df$TrustGovHigh = ifelse(as.numeric(merged_df$TrustGov)>3,1,0)
merged_df$TrustExecsHigh = ifelse(as.numeric(merged_df$TrustExecs)>3,1,0)
merged_df$RegProtectionHigh = ifelse(as.numeric(merged_df$RegProtection)>3,1,0)
merged_df$RegBurdenHigh = ifelse(as.numeric(merged_df$RegBurden)>3,1,0)

changing_variable <- "SupportTransparency"
#
FORM0 = "~ Treatment*margindummy"
FORM1 = "~ Treatment*InvolvementAI+as.factor(Sector)+as.factor(Degree)+as.factor(state)+
              black+white+asian+hispanic+GenderMale"
FORM2 = "~ Treatment*margindummy+as.factor(Sector)+as.factor(Degree)+ExperienceHigh+as.factor(state)+
             black+white+asian+hispanic+GenderMale+
             RegProtectionHigh+RegBurdenHigh+TrustGovHigh+TrustExecsHigh+FirmSize"
# Construct the formula string

merged_dfb = subset(merged_df, merged_df$Degree != "Prefer not to say")
#
formula_obj0 <- as.formula(paste(changing_variable, FORM0))
formula_obj1 <- as.formula(paste(changing_variable, FORM1))
formula_obj2 <- as.formula(paste(changing_variable, FORM2))
# Fit the proportional odds model
# 
model0T <- polr(formula_obj0,data=merged_df, method = "logistic", Hess=TRUE)
model1T <- polr(formula_obj1,data=merged_df, method = "logistic", Hess=TRUE)
model2T <- polr(formula_obj2,data=merged_df, method = "logistic", Hess=TRUE)
#--------------------------------------------------------------------------------
changing_variable <- "SupportAutonomous"
formula_obj0 <- as.formula(paste(changing_variable, FORM0))
formula_obj1 <- as.formula(paste(changing_variable, FORM1))
formula_obj2 <- as.formula(paste(changing_variable, FORM2))
# Fit the proportional odds model
# 
model0A <- polr(formula_obj0,data=merged_df, method = "logistic", Hess=TRUE)
model1A <- polr(formula_obj1,data=merged_df, method = "logistic", Hess=TRUE)
model2A <- polr(formula_obj2,data=merged_df, method = "logistic", Hess=TRUE)
#--------------------------------------------------------------------------------
changing_variable <- "SupportBias"
formula_obj0 <- as.formula(paste(changing_variable, FORM0))
formula_obj1 <- as.formula(paste(changing_variable, FORM1))
formula_obj2 <- as.formula(paste(changing_variable, FORM2))
# Fit the proportional odds model
# 
model0B <- polr(formula_obj0,data=merged_df, method = "logistic", Hess=TRUE)
model1B <- polr(formula_obj1,data=merged_df, method = "logistic", Hess=TRUE)
model2B <- polr(formula_obj2,data=merged_df, method = "logistic", Hess=TRUE)
# Now use stargazer to create a table
library(stargazer)
stargazer(model0T, model2T, model0A, model2A, model0B, model2B, 
          omit=c("state","Sector","Degree", "black", "white", "asian", "hispanic", "GenderMale", "FirmSize","UseAIFirmYes",
                 "ExperienceHigh","ConcernSelf", "RegProtectionHigh", "RegBurdenHigh","TrustGovHigh",
                 "TrustExecsHigh"), title = "Ordinal Logistic Regression Results", type = "html", out = "models_comparison.html")
