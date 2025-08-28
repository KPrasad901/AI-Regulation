library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(tidyverse, dplyr)

# Load the data
load("AISurveySTATES.Rdata")

#-------------------------------------------------------------------------------
#               Figure 1
#-------------------------------------------------------------------------------
df = merged_df
#
df <- df %>% rename(Transparency = SupportTransparency)
df <- df %>% rename(Explainability = SupportAutonomous)
df <- df %>% rename(Bias = SupportBias)
#-------------------------------------------------------------------------------
long_df <- df %>%
  gather(key = "Question", value = "Response", Transparency, Explainability, Bias)

levels_order <- c("Strongly Oppose", "Somewhat Oppose", "Neutral", "Somewhat Support", "Strongly Support")
long_df <- long_df %>%
  mutate(Response = factor(Response, levels = levels_order, ordered = TRUE))

# Calculate proportions
proportion_df <- long_df %>%
  group_by(Question, Response) %>%
  summarise(Proportion = n() / nrow(filter(long_df, Question == unique(Question))))

ggplot(proportion_df, aes(x = Response, y = Proportion, fill = Question)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") + 
  labs(x = "Response", y = "Proportion", fill = "Regulation:") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
        legend.position = "bottom") +
  scale_fill_viridis_d(option = "cividis", begin = 0.1, end = 0.9)
#
#-------------------------------------------------------------------------------
#    Figure 2
#---------------------------------------------------------------------------------

# Reshape into long format
df_long <- df %>%
  pivot_longer(
    cols = c(Explainability, Bias, Transparency),
    names_to = "Domain",
    values_to = "Response"
  )

# Order responses
levels_order <- c("Strongly Oppose", "Somewhat Oppose", "Neutral",
                  "Somewhat Support", "Strongly Support")
df_long <- df_long %>%
  mutate(Response = factor(Response, levels = levels_order, ordered = TRUE))

# Summarize counts and proportions by Domain and Treatment
result_df <- df_long %>%
  group_by(Domain, Response, Treatment) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Domain, Treatment) %>%
  mutate(freq = n / sum(n))

# Plot
p <- ggplot(result_df, aes(x = Response, y = freq, fill = Treatment)) +
  geom_col(position = "dodge") +
  facet_wrap(~Domain, nrow = 1, scales = "fixed") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    legend.position = "bottom"
  ) +
  labs(
    fill = "Treatment",
    x = "Support for Regulation",
    y = "Proportion"
  ) +
  scale_fill_viridis_d(option = "cividis", begin = 0.1, end = 0.9)

p <- ggplot(result_df, aes(x = Response, y = freq, fill = Treatment)) +
  geom_col(position = "dodge") +
  facet_wrap(~Domain, nrow = 1, scales = "fixed") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 11),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    fill = "Treatment",
    x = "Support for Regulation",
    y = "Proportion"
  ) +
  scale_fill_viridis_d(option = "cividis", begin = 0.1, end = 0.9)
p
#
#-------------------------------------------------------------------------------
# Figure 3
#-------------------------------------------------------------------------------

df = merged_df
dfy = df[,c("SupportAutonomous", "SupportBias", "SupportTransparency", "ConcernSelf")]
# Ensure complete data
dfy <- dfy[complete.cases(dfy),]

# Reshape to long format
df_long <- dfy %>%
  pivot_longer(
    cols = c(SupportTransparency, SupportAutonomous, SupportBias),
    names_to = "Outcome",
    values_to = "Response"
  )

# Order responses (optional, for logical order in the plot)
levels_order <- c("Strongly Oppose", "Somewhat Oppose", "Neutral",
                  "Somewhat Support", "Strongly Support")
df_long <- df_long %>%
  mutate(Response = factor(Response, levels = levels_order, ordered = TRUE))

# Count and normalize to proportions
df_summary <- df_long %>%
  count(ConcernSelf, Outcome, Response) %>%
  group_by(ConcernSelf, Outcome) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Nice labels for facets
facet_labels <- c(
  "SupportTransparency" = "Transparency",
  "SupportAutonomous"   = "Explainability",
  "SupportBias"         = "Bias"
)

# Plot: side-by-side bars with boxes
p <- ggplot(df_summary, aes(x = ConcernSelf, y = prop, fill = Response)) +
  geom_col(position = "dodge") +
  facet_wrap(~Outcome, nrow = 1, scales = "fixed",
             labeller = labeller(Outcome = facet_labels)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    fill = "Response",
    x = "Concern about job displacement from AI",
    y = "Proportion"
  ) +
  scale_fill_viridis_d(option = "cividis", begin = 0.1, end = 0.9) +
  scale_x_discrete(labels = function(x) gsub("_", " ", x))

p

#-------------------------------------------------------------------------------
# Figure 4
#-------------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
df = merged_df[merged_df$Treatment=="COMPANY",]
dfy = df[,c("SupportAutonomous", "SupportBias", "SupportTransparency", "Company_Q4")]
# Ensure complete data
dfy <- dfy[complete.cases(dfy),]

# Reshape to long format
df_long <- dfy %>%
  pivot_longer(
    cols = c(SupportTransparency, SupportAutonomous, SupportBias),
    names_to = "Outcome",
    values_to = "Response"
  )

# Order responses (optional, for logical order in the plot)
levels_order <- c("Strongly Oppose", "Somewhat Oppose", "Neutral",
                  "Somewhat Support", "Strongly Support")
df_long <- df_long %>%
  mutate(Response = factor(Response, levels = levels_order, ordered = TRUE))

# Count and normalize to proportions
df_summary <- df_long %>%
  count(Company_Q4, Outcome, Response) %>%
  group_by(Company_Q4, Outcome) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# Nice labels for facets
facet_labels <- c(
  "SupportTransparency" = "Transparency",
  "SupportAutonomous"   = "Explainability",
  "SupportBias"         = "Bias"
)

# Plot: side-by-side bars with boxes
p <- ggplot(df_summary, aes(x = Company_Q4, y = prop, fill = Response)) +
  geom_col(position = "dodge") +
  facet_wrap(~Outcome, nrow = 1, scales = "fixed",
             labeller = labeller(Outcome = facet_labels)) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "grey90", color = "black"),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    fill = "Response",
    x = "Competitive disadvantage from transparent, accountable, and fair AI",
    y = "Proportion"
  ) +
  scale_fill_viridis_d(option = "cividis", begin = 0.1, end = 0.9) +
  scale_x_discrete(labels = function(x) gsub("_", " ", x))

p
