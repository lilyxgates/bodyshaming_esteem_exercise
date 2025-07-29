# Analyzing How Self-Esteem and Hearing Negative Body Talk Influences Exercise Motivation

# By Lily Gates

# -------------------------
# Importing Libraries
# -------------------------
# Computations
library(psych)  # Calculate alpha and omega, along with specific item analysis
library(car)  # Calculate non-constant variance score test

# Data Organization
library(dplyr)  # Stats and visualizations
library(tidyr)  # Handles data and organizes it
library(broom)  # Allows extraction of statistics to later be used in code

# Data Visualization
library(ggplot2)  # Create the majority of data visualizations
library(patchwork)  # Create grids of multiple plots
library(jtools) # Make figures APA format
library(ggcorrplot)  # Plot correlations
library(interactions)  # Help interpret and visualize interactions in regression models

# -------------------------
# Reading in Data
# -------------------------
survey <- read.csv("bodyshaming_esteem_exercise_data.csv")

# -------------------------
# Summarize Data
# -------------------------
# Obtain means and other descriptions of variables in data 
summary(survey)

# See data types and variable heads (also visible in global enviornment)
str(survey)

# See column names
colnames(survey)

# -------------------------
# Cleaning Data
# -------------------------

# Rename columns
colnames(survey)[1] <- "RSE_Q1"
colnames(survey)[3] <- "RSE_Q3"
colnames(survey)

# Remove all missing values listwise
survey <- na.omit(survey)
summary(survey)

# -------------------------
# Reverse Coding
# -------------------------
# Reverse score negatively worded items: 0 = highest SE, 5 = lowest SE
max_val <- 5
survey$RSE_Q2 <- max_val - survey$RSE_Q2_RS
survey$RSE_Q4 <- max_val - survey$RSE_Q4_RS
survey$RSE_Q7 <- max_val - survey$RSE_Q7_RS

# -------------------------
# Organizing Data
# -------------------------

# Create vectors and subsequent dataframes for survey question categories

# Self-Esteem - Moderator
RSE_items <- c("RSE_Q1", "RSE_Q2", "RSE_Q3", "RSE_Q4", "RSE_Q5", "RSE_Q6", "RSE_Q7")
RSE_data <- survey[, RSE_items]

# Intrinsic Motivation for Exercise - Outcome (1 of 2)
intrinsic_motiv_items <- c("I_MEW_Q1", "I_MEW_Q2", "I_PH_Q1", "I_PA_Q1", "I_PA_Q2", "I_SC_Q1", "I_SC_Q2")
intrinsic_motiv_data <- survey[, intrinsic_motiv_items]

# Extrinsic Motivation for Exercise - Outcome (2 of 2)
extrinsic_motiv_items <- c("E_SP_Q1", "E_SP_Q2", "E_AD_Q1", "E_PA_Q3")
extrinsic_motiv_data <- survey[, extrinsic_motiv_items]

# Self-Directed Body Shaming (Overhearing negative body talk) - Predictor
bodyshame_items <- c("BS_BI_Q1", "BS_BI_Q2", "BS_WL_Q1", "BS_WL_Q2", "BS_EX_Q1", "BS_EX_Q2", "BS_SC_Q1")
bodyshame_data <- survey[, bodyshame_items]

# --------------------------------------
# Recode Demographic Variables
# --------------------------------------

# Recode 'sex'
survey$sex <- factor(survey$sex, levels = 1:5, labels = c(
  "Male", "Female", "Intersex", "Other", "Prefer not to say"
))

# Recode 'gender'
survey$gender <- factor(survey$gender, levels = 1:5, labels = c(
  "Man", "Woman", "Non-binary", "Other", "Prefer not to say"
))

# Recode 'race'
survey$race <- factor(survey$race, levels = 1:9, labels = c(
  "White/Caucasian", 
  "Black/African American", 
  "Hispanic or Latino", 
  "Middle Eastern/North African", 
  "Asian/Pacific Islander", 
  "Indigenous American/Alaskan Native", 
  "Native Hawaiian/Pacific Islander", 
  "Other", 
  "Prefer not to say"
))

# --------------------------------------
# Summarize Sample Demographics
# --------------------------------------

# Summary for Sex (counts & %)
sex_summary <- survey %>%
  count(sex) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# Summary for Gender (counts & %)
gender_summary <- survey %>%
  count(gender) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# Summary for Race (counts & %)
race_summary <- survey %>%
  count(race) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

# Summary for Age (mean, SD, min, max)
age_summary <- survey %>%
  summarize(
    mean_age = mean(age, na.rm = TRUE),
    sd_age = sd(age, na.rm = TRUE),
    min_age = min(age, na.rm = TRUE),
    max_age = max(age, na.rm = TRUE)
  )

# Print results
print(sex_summary)
print(gender_summary)
print(race_summary)
print(age_summary)

# --------------------------------------
# Contingency Table: Age group, race, gender
# --------------------------------------


survey <- survey %>%
  mutate(age_group = cut(age, breaks = c(17, 20, 25, 30, 40, 60, Inf),
                         labels = c("18-20", "21-25", "26-30", "31-40", "41-60", "61+"),
                         right = TRUE))

# Filter or remove NAs if necessary
survey_clean <- survey %>%
  filter(!is.na(age_group), !is.na(race), !is.na(gender))

# Count combinations by age_group, race, and gender
gender_counts <- survey_clean %>%
  count(age_group, race, gender)

# Pivot wider to have gender columns with counts
gender_wide <- gender_counts %>%
  pivot_wider(names_from = gender, values_from = n, values_fill = 0)

print(gender_wide)


# --------------------------------------
# Age and Gender (Stacked Bar Graph)
# --------------------------------------


# Create age groups if not done yet
survey <- survey %>%
  mutate(age_group = cut(age, breaks = c(17, 20, 25, 30, 40, 60, Inf),
                         labels = c("18-20", "21-25", "26-30", "31-40", "41-60", "61+"),
                         right = TRUE))

# Filter out NAs for plotting
plot_data <- survey %>%
  filter(!is.na(age_group), !is.na(gender))

# Plot with counts and labels centered in each stack
ggplot(plot_data, aes(x = age_group, fill = gender)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), color = "white", size = 3, fontface = 'bold') +
  labs(
    title = "Distribution of Age Groups by Gender with Counts",
    x = "Age Group",
    y = "Count",
    fill = "Gender"
  ) +
  theme_apa()

# --------------------------------------
# Age and Gender (Stacked Bar Graph)
# --------------------------------------

# Make sure age_group exists
survey <- survey %>%
  mutate(age_group = cut(age, breaks = c(17, 20, 25, 30, 40, 60, Inf),
                         labels = c("18-20", "21-25", "26-30", "31-40", "41-60", "61+"),
                         right = TRUE))

# Filter to valid data
plot_data <- survey %>%
  filter(!is.na(age_group), !is.na(race))

# Plot stacked bar of age groups filled by race, with counts inside segments
ggplot(plot_data, aes(x = age_group, fill = race)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), color = "white", size = 3, fontface = 'bold') +
  labs(
    title = "Distribution of Age Groups by Race with Counts",
    x = "Age Group",
    y = "Count",
    fill = "Race"
  ) +
  theme_apa()

# --------------------------------------
# Estinate Chrombach's alpha - internal consistency/reliability
# --------------------------------------

# Note: Good for unidimensional scales and where items are roughly equivalent

# Calculating Chrombach's alpha
psych::alpha(RSE_data)
psych::alpha(intrinsic_motiv_data)
psych::alpha(extrinsic_motiv_data)
psych::alpha(bodyshame_data)


# --------------------------------------
# Estinate McDonald's Omega - internal consistency/reliability
# --------------------------------------
# Note: Better estimate than Chronbach's alpha because it accounts for the factor structure of items

# Calculating McDonald's Omega
psych::omega(RSE_data, plot = FALSE)
psych::omega(intrinsic_motiv_data, plot = FALSE)
psych::omega(extrinsic_motiv_data, plot = FALSE)
psych::omega(bodyshame_data, plot = FALSE)

# --------------------------------------
# Creating Total Scores for Each Scale
# --------------------------------------

# Creating columns that hold the total scores for each scale
survey$RSE_total <- rowSums(RSE_data, na.rm = TRUE)
survey$intrinsic_motiv_total <- rowSums(intrinsic_motiv_data, na.rm = TRUE)
survey$extrinsic_motiv_total <- rowSums(extrinsic_motiv_data, na.rm = TRUE)
survey$bodyshame_total <- rowSums(bodyshame_data, na.rm = TRUE)

# Standardize total scores (mean = 0, sd = 1)
survey$RSE_total_z <- scale(survey$RSE_total)
survey$intrinsic_motiv_total_z <- scale(survey$intrinsic_motiv_total)
survey$extrinsic_motiv_total_z <- scale(survey$extrinsic_motiv_total)
survey$bodyshame_total_z <- scale(survey$bodyshame_total)

# --------------------------------------
# Modeling: Testing Assumptions
# --------------------------------------

# Testing Heteroscedisity

model_extrinsic <- lm(extrinsic_motiv_total_z ~ bodyshame_total_z, data = survey)
plot(model_extrinsic, which = 1)  # Residuals vs. Fitted
ncvTest(model_extrinsic)

model_intrinsic <- lm(intrinsic_motiv_total_z ~ bodyshame_total_z, data = survey)
plot(model_intrinsic, which = 1)  # Residuals vs. Fitted
ncvTest(model_intrinsic)

# Testing Linearity of variables
# Scatterplots

p1 <- ggplot(survey, aes(x = bodyshame_total_z, y = extrinsic_motiv_total_z)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "cadetblue3") +
  labs(
    title = "Body shame and\nextrinsic motivation",
    x = "Body shame\n(standardized)",
    y = "Extrinsic motivation\n(standardized)"
  ) +
  theme_apa()

p2 <- ggplot(survey, aes(x = bodyshame_total_z, y = intrinsic_motiv_total_z)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "steelblue3") +
  labs(
    title = "Body shame and\nintrinsic motivation",
    x = "Body shame\n(standardized)",
    y = "Intrinsic motivation\n(standardized)"
  ) +
  theme_apa()

p3 <- ggplot(survey, aes(x = RSE_total_z, y = extrinsic_motiv_total_z)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "goldenrod3") +
  labs(
    title = "Self-esteem and\nextrinsic motivation",
    x = "Self-esteem\n(standardized)",
    y = "Extrinsic motivation\n(standardized)"
  ) +
  theme_apa()

p4 <- ggplot(survey, aes(x = RSE_total_z, y = intrinsic_motiv_total_z)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "sienna3") +
  labs(
    title = "Self-esteem and\nintrinsic motivation",
    x = "Self-esteem\n(standardized)",
    y = "Intrinsic motivation\n(standardized)"
  ) +
  theme_apa()

# Combine plots in 2x2 grid
combined_plot <- (p1 | p2) / (p3 | p4)

# Display combined plot
print(combined_plot)

# --------------------------------------
# Normality of variables
# --------------------------------------

# Testing Normality Visually: Q-Q Plot and Histogram

# Q-Q Plots
qq1 <- ggplot(survey, aes(sample = intrinsic_motiv_total_z)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Intrinsic motivation\n(standardized)") +
  theme_apa()

qq2 <- ggplot(survey, aes(sample = extrinsic_motiv_total_z)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Extrinsic motivation\n(standardized)") +
  theme_apa()

qq3 <- ggplot(survey, aes(sample = bodyshame_total_z)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Body shame\n(standardized)") +
  theme_apa()

qq4 <- ggplot(survey, aes(sample = RSE_total_z)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("Self-esteem\n(standardized)") +
  theme_apa()

# Combine in a 2x2 grid
qq_grid <- (qq1 | qq2) / (qq3 | qq4)

print(qq_grid)

# --------------------------------------

# Histograms
hist1 <- ggplot(survey, aes(x = extrinsic_motiv_total_z)) +
  geom_histogram(binwidth = 0.5, fill = "darkslategray3", color = "black") +
  ggtitle("Extrinsic motivation\n(standardized)") +
  theme_apa()

hist2 <- ggplot(survey, aes(x = intrinsic_motiv_total_z)) +
  geom_histogram(binwidth = 0.5, fill = "darkolivegreen3", color = "black") +
  ggtitle("Intrinsic motivation\n(standardized)") +
  theme_apa()

hist3 <- ggplot(survey, aes(x = bodyshame_total_z)) +
  geom_histogram(binwidth = 0.5, fill = "goldenrod3", color = "black") +
  ggtitle("Body shame\n(standardized)") +
  theme_apa()

hist4 <- ggplot(survey, aes(x = RSE_total_z)) +
  geom_histogram(binwidth = 0.5, fill = "lightpink3", color = "black") +
  ggtitle("Self-esteem\n(standardized)") +
  theme_apa()

# Combine in a 2x2 grid

hist_grid <- (hist1 | hist2) / (hist3 | hist4)

print(hist_grid)

# --------------------------------------
# Testing Normality: Shapiro-Wilk Test
# --------------------------------------

# Run Shapiro-Wilk tests and store results
shapiro_intrinsic <- shapiro.test(survey$intrinsic_motiv_total_z)
shapiro_extrinsic <- shapiro.test(survey$extrinsic_motiv_total_z)
shapiro_bodyshame <- shapiro.test(survey$bodyshame_total_z)
shapiro_RSE <- shapiro.test(survey$RSE_total_z)

# Create a data frame with results
shapiro_results_df <- data.frame(
  Variable = c("Intrinsic motivation", "Extrinsic motivation", "Body shame", "Self-esteem"),
  W = c(shapiro_intrinsic$statistic, shapiro_extrinsic$statistic, shapiro_bodyshame$statistic, shapiro_RSE$statistic),
  p_value = c(shapiro_intrinsic$p.value, shapiro_extrinsic$p.value, shapiro_bodyshame$p.value, shapiro_RSE$p.value)
)

print(shapiro_results_df)

# --------------------------------------
# Data Visualization: Correlations
# --------------------------------------

# Select variables
vars <- survey[, c("bodyshame_total_z", "intrinsic_motiv_total_z", "extrinsic_motiv_total_z", "RSE_total_z")]

# Define nicer display names
display_names <- c(
  intrinsic_motiv_total_z = "Intrinsic Motivation",
  extrinsic_motiv_total_z = "Extrinsic Motivation",
  bodyshame_total_z = "Body Shame",
  RSE_total_z = "Self-Esteem"
)

# Compute correlations first
  # Pearson's r - the strength and direction of a linear relationship between two continuous variables
  # Spearman's rho - the strength and direction of a monotonic relationship between two continuous or ordinal variables. 

pearson_corr <- cor(vars, method = "pearson")
spearman_corr <- cor(vars, method = "spearman")

# Reorder correlation matrices rows and columns to match display_names order
ordered_vars <- names(display_names)

pearson_corr <- pearson_corr[ordered_vars, ordered_vars]
spearman_corr <- spearman_corr[ordered_vars, ordered_vars]

# Rename rows and columns with display names
rownames(pearson_corr) <- display_names[rownames(pearson_corr)]
colnames(pearson_corr) <- display_names[colnames(pearson_corr)]

rownames(spearman_corr) <- display_names[rownames(spearman_corr)]
colnames(spearman_corr) <- display_names[colnames(spearman_corr)]

# Define color palette
my_palette <- c("navy", "white", "red4")

# Create ggcorrplot objects
p_pearson <- ggcorrplot(pearson_corr, hc.order = FALSE, type = "full",
                        lab = TRUE, lab_size = 2, method = "square",
                        colors = my_palette,
                        title = "Pearson Correlation Matrix")

p_spearman <- ggcorrplot(spearman_corr, hc.order = FALSE, type = "full",
                         lab = TRUE, lab_size = 2, method = "square",
                         colors = my_palette,
                         title = "Spearman Correlation Matrix")

# Adding APA theme
apa_theme_custom <- theme(
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_text(color = "black", size = 8),
  axis.text.y = element_text(color = "black", size = 8, angle = 0, hjust = 1),
  plot.title = element_text(size = 12, face = "bold", hjust = 0.5)
)

p_pearson <- p_pearson + apa_theme_custom + labs(x = NULL, y = NULL)
p_spearman <- p_spearman + apa_theme_custom + labs(x = NULL, y = NULL)

# Combine side by side with patchwork
combined_plot <- p_pearson + p_spearman + plot_layout(ncol = 2)

# Print combined plot
print(combined_plot)

# Print correlations
print(pearson_corr)
print(spearman_corr)

# --------------------------------------
# Model: Regression Between Overhearing Self-Deprecating Bodyshaming and Exercise Motivations (Linear and Loess)
# --------------------------------------

# Plot 1: Body shame vs Extrinsic motivation
p1 <- ggplot(survey, aes(x = bodyshame_total_z, y = extrinsic_motiv_total_z)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(color = "Linear Fit"), method = "lm", se = FALSE, linetype = "dotted") +
  geom_smooth(aes(color = "Loess Fit"), method = "loess", se = TRUE) +
  scale_color_manual(name = "Smoothing Method",
                     values = c("Linear Fit" = "blue", "Loess Fit" = "red")) +
  labs(title = "Extrinsic Motivation\nvs. Body Shame",
       x = "Body Shame (standardized)",
       y = "Extrinsic Motivation (standardized)") +
  theme_apa() +
  theme(legend.position = "none")

# Plot 2: Body shame vs Intrinsic motivation
p2 <- ggplot(survey, aes(x = bodyshame_total_z, y = intrinsic_motiv_total_z)) +
  geom_point(alpha = 0.6) +
  geom_smooth(aes(color = "Linear Fit"), method = "lm", se = FALSE, linetype = "dotted") +
  geom_smooth(aes(color = "Loess Fit"), method = "loess", se = TRUE) +
  scale_color_manual(name = "Smoothing Method",
                     values = c("Linear Fit" = "blue", "Loess Fit" = "red")) +
  labs(title = "Intrinsic Motivation\nvs. Body Shame",
       x = "Body Shame (standardized)",
       y = "Intrinsic Motivation (standardized)") +
  theme_apa() +
  theme(legend.position = "right")

# Combine side by side with shared legend on the right
combined_plot <- p1 + p2 + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "right")


print(combined_plot)

# --------------------------------------
# Understanding the Linear Model
# --------------------------------------

lm_extrinsic <- lm(extrinsic_motiv_total_z ~ bodyshame_total_z, data = survey)
summary(lm_extrinsic)

lm_intrinsic <- lm(intrinsic_motiv_total_z ~ bodyshame_total_z, data = survey)
summary(lm_intrinsic)

# --------------------------------------
# Understanding the Loees Model
# --------------------------------------

loess_extrinsic <- loess(extrinsic_motiv_total_z ~ bodyshame_total_z, data = survey)
summary(loess_extrinsic)

loess_intrinsic <- loess(intrinsic_motiv_total_z ~ bodyshame_total_z, data = survey)
summary(loess_extrinsic <- loess(extrinsic_motiv_total_z ~ bodyshame_total_z, data = survey)
)

# --------------------------------------
# Measuring the Effect of Self-Esteem as a Moderator
# --------------------------------------

# Moderation Model: Body Shame x Self-Esteem predicting Extrinsic Motivation
mod_extrinsic <- lm(extrinsic_motiv_total_z ~ bodyshame_total_z * RSE_total_z, data = survey)
summary(mod_extrinsic)

# Moderation Model: Body Shame x Self-Esteem predicting Intrinsic Motivation
mod_intrinsic <- lm(intrinsic_motiv_total_z ~ bodyshame_total_z * RSE_total_z, data = survey)
summary(mod_intrinsic)

# --------------------------------------
# Interaction Plots to Analyze Self-Esteem as a Moderator
# --------------------------------------

p1 <- interact_plot(mod_extrinsic,
                    pred = bodyshame_total_z,
                    modx = RSE_total_z,
                    plot.points = TRUE,
                    interval = TRUE,
                    main.title = "Body Shame → \nExtrinsic Motivation\nby Self-Esteem",
                    x.label = "Body Shame (standardized)",
                    y.label = "Extrinsic Motivation (standardized)",
                    modx.values = "plus-minus") + theme_apa()

p2 <- interact_plot(mod_intrinsic,
                    pred = bodyshame_total_z,
                    modx = RSE_total_z,
                    plot.points = TRUE,
                    interval = TRUE,
                    main.title = "Body Shame → \nIntrinsic Motivation\nby Self-Esteem",
                    x.label = "Body Shame (standardized)",
                    y.label = "Intrinsic Motivation (standardized)",
                    modx.values = "plus-minus") + theme_apa()

# Combine plots side by side
p1 + p2 + plot_layout(guides = "collect")

# --------------------------------------
# Combining Demographics with Predictor, Moderator and Outcome Variables
# --------------------------------------
# Exercise Motivation vs Hearing Bodyshaming

# Two scatterplots with regression

p1 <- ggplot(survey, aes(x = extrinsic_motiv_total_z, y = bodyshame_total_z, color = sex)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", aes(group = sex, color = sex), se = FALSE) + 
  labs(
    title = "Extrinsic Motivation\nvs Bodyshaming Score",
    x = "Extrinsic Motivation Score",
    y = "Bodyshaming Score"
  ) +
  theme_apa() +
  theme(legend.position = "right")

p2 <- ggplot(survey, aes(x = intrinsic_motiv_total_z, y = bodyshame_total_z, color = sex)) +
  geom_point(alpha = 0.7, size = 3) +
  geom_smooth(method = "lm", aes(group = sex, color = sex), se = FALSE) + 
  labs(
    title = "Intrinsic Motivation\nvs Bodyshaming Score",
    x = "Intrinsic Motivation Score",
    y = "Bodyshaming Score"
  ) +
  theme_apa() +
  theme(legend.position = "right")

# Combine and share legend
(p1 + p2) + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "right")

# --------------------------------------
# Seperate Regressions Filtered by Sex-Assigned at Birth
# --------------------------------------

mod_extrinsic_male <- lm(extrinsic_motiv_total_z ~ bodyshame_total_z * RSE_total_z, 
                        data = filter(survey, sex == "Male"))

mod_intrinsic_male <- lm(intrinsic_motiv_total_z ~ bodyshame_total_z * RSE_total_z, 
                        data = filter(survey, sex == "Male"))

mod_extrinsic_female <- lm(extrinsic_motiv_total_z ~ bodyshame_total_z * RSE_total_z, 
                          data = filter(survey, sex == "Female"))

mod_intrinsic_female <- lm(intrinsic_motiv_total_z ~ bodyshame_total_z * RSE_total_z, 
                          data = filter(survey, sex == "Female"))

tidy_results_long <- survey %>%
  filter(sex %in% c("Male", "Female")) %>%
  group_by(sex) %>%
  summarise(
    extrinsic = list(tidy(lm(extrinsic_motiv_total_z ~ bodyshame_total_z, data = cur_data()))),
    intrinsic = list(tidy(lm(intrinsic_motiv_total_z ~ bodyshame_total_z, data = cur_data())))
  ) %>%
  pivot_longer(cols = c(extrinsic, intrinsic), names_to = "model_type", values_to = "model") %>%
  unnest(model)

print(tidy_results_long)

# --------------------------------------
# Moderation: How Self-Esteem Moderates Exercise Motivation Among Genders 
# --------------------------------------

# Create a list of models with labels
models_list <- list(
  male_extrinsic = mod_extrinsic_male,
  male_intrinsic = mod_intrinsic_male,
  female_extrinsic = mod_extrinsic_female,
  female_intrinsic = mod_intrinsic_female
)

# Tidy all models and bind into one dataframe
model_results_df <- bind_rows(
  lapply(names(models_list), function(name) {
    tidy(models_list[[name]]) %>%
      mutate(model = name)
  }),
  .id = "id"
)

# Separate the model label into sex and motivation type for clarity
model_results_df <- model_results_df %>%
  separate(model, into = c("sex", "motivation"), sep = "_")

# View the dataframe
print(model_results_df)

# --------------------------------------
# Barplot Interaction Effects of Body Shame x Self-Esteem by Gender
# --------------------------------------

# Filter only interaction terms
interaction_df <- model_results_df %>%
  filter(term == "bodyshame_total_z:RSE_total_z")

# Plot estimates with error bars
ggplot(interaction_df, aes(x = motivation, y = estimate, fill = sex)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error),
                position = position_dodge(width = 0.7), width = 0.2) +
  labs(title = "Interaction Effects of Body Shame x Self-Esteem by Sex and Motivation",
       x = "Motivation Type",
       y = "Estimate (Interaction Term)",
       fill = "Sex") +
  theme_apa()

# --------------------------------------
# Interaction Plots
# --------------------------------------

# Male extrinsic
p1_male <- interact_plot(mod_extrinsic_male,
                        pred = bodyshame_total_z,
                        modx = RSE_total_z,
                        plot.points = TRUE,
                        interval = TRUE,
                        main.title = "Male: Body Shame → \nExtrinsic Motivation\nby Self-Esteem",
                        x.label = "Body Shame\n(standardized)",
                        y.label = "Extrinsic Motivation\n(standardized)",
                        modx.values = "plus-minus") + theme_apa()

# Male intrinsic
p2_male <- interact_plot(mod_intrinsic_male,
                        pred = bodyshame_total_z,
                        modx = RSE_total_z,
                        plot.points = TRUE,
                        interval = TRUE,
                        main.title = "Male: Body Shame → \nIntrinsic Motivation\nby Self-Esteem",
                        x.label = "Body Shame\n(standardized)",
                        y.label = "Intrinsic Motivation\n(standardized)",
                        modx.values = "plus-minus") + theme_apa()

# Female extrinsic
p1_female <- interact_plot(mod_extrinsic_female,
                          pred = bodyshame_total_z,
                          modx = RSE_total_z,
                          plot.points = TRUE,
                          interval = TRUE,
                          main.title = "Female: Body Shame → \nExtrinsic Motivation\nby Self-Esteem",
                          x.label = "Body Shame\n(standardized)",
                          y.label = "Extrinsic Motivation\n(standardized)",
                          modx.values = "plus-minus") + theme_apa()

# Female intrinsic
p2_female <- interact_plot(mod_intrinsic_female,
                          pred = bodyshame_total_z,
                          modx = RSE_total_z,
                          plot.points = TRUE,
                          interval = TRUE,
                          main.title = "Female: Body Shame → \nIntrinsic Motivation\nby Self-Esteem",
                          x.label = "Body Shame\n(standardized)",
                          y.label = "Intrinsic Motivation\n(standardized)",
                          modx.values = "plus-minus") + theme_apa()

# Combine all four plots in a 2x2 grid
(p1_male + p2_male) / (p1_female + p2_female)

