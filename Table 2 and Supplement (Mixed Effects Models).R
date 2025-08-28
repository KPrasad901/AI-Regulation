load("AISurveySTATES.Rdata")
library(dplyr)
library(pandoc)
library(stargazer)
df = merged_df[,c("SupportTransparency", "SupportAutonomous", "SupportBias", "TrustGov","InvolvementAI", "Experience")]
df$RespondentID <- seq_len(nrow(df))

library(tidyr)
library(dplyr)

df_long <- df %>%
  pivot_longer(cols = starts_with("Support"),
               names_to = "RegulationType",
               values_to = "SupportLevel") %>%
  mutate(RegulationType = gsub("Support", "", RegulationType),
         SupportLevel = factor(SupportLevel,
                               levels = c("Strongly Oppose", "Somewhat Oppose", "Neutral",
                                          "Somewhat Support", "Strongly Support"),
                               ordered = TRUE))
library(ordinal)

model1 <- clmm(SupportLevel ~ RegulationType + (1 | RespondentID), data = df_long)

summary(model1)
#
#---------------------------
# Relevel RegulationType so that "Bias" is the reference
df_long$RegulationType <- relevel(factor(df_long$RegulationType), ref = "Transparency")

# Re-fit the model
model2 <- clmm(SupportLevel ~ RegulationType+(1 | RespondentID), data = df_long)

# View summary
summary(model2)
#-----------------------------
#
# Re-fit the model
model3 <- clmm(SupportLevel ~ RegulationType*InvolvementAI+(1 | RespondentID), data = df_long)

# View summary
summary(model3)
#-----------------------------------------
#
# install.packages("sjPlot")
library(sjPlot)
library(broom.mixed)
library(modelsummary)

modelsummary(
  list("Model 1" = model1, "Model 2" = model2, "Model 3" = model3),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = "Ordinal Mixed Effects Models",
  output = "model.docx"
)
#
#------------------------------------------------------------------

#--------------------------------

library(lme4)

# Convert SupportLevel to numeric (1–5)
df_long$SupportNumeric <- as.numeric(df_long$SupportLevel)

# Fit linear mixed model
df_long$RegulationType <- relevel(factor(df_long$RegulationType), ref = "Autonomous")
model4 <- lmer(SupportNumeric ~ RegulationType + (1 | RespondentID), data = df_long)
df_long$RegulationType <- relevel(factor(df_long$RegulationType), ref = "Transparency")

model5 <- lmer(SupportNumeric ~ RegulationType + (1 | RespondentID), data = df_long)
model6 <- lmer(SupportNumeric ~ RegulationType * InvolvementAI + (1 | RespondentID), data = df_long)

# View results
summary(model4)
summary(model5)
#
#
modelsummary(
  list("Model 1" = model4, "Model 2" = model5, "Model 3" = model6),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  title = "Linear Mixed Effects Models",
  output = "model2.docx"
)
#