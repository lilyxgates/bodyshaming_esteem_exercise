# Social Influences on Exercise Motivation: The Impact of Overheard Self-Deprecating Body Comments
*Written by Lily Gates*
* May 2025*

## Overview
This study examines how exposure to overheard self-deprecating comments about body image influences individuals' motivation to exercise. The research specifically explores whether this exposure affects intrinsic motivation (e.g., exercising for enjoyment or health) versus extrinsic motivation (e.g., exercising for appearance or social approval). Additionally, it investigates whether self-esteem moderates this relationship, given its role in how individuals internalize social feedback related to body image.

## Research Questions
1. Does exposure to overheard self-deprecating body comments influence exercise motivation?
2. Are different types of motivation (intrinsic vs. extrinsic) affected differently?
3. Does self-esteem moderate the relationship between overheard comments and motivation to exercise?

## Key Variables
* Predictor: Exposure to Overheard Self-Deprecating Body Comments
* Outcome: Motivation to Exercise (Intrinsic vs. Extrinsic)
* Moderator: Self-Esteem
* Control: Sex Assigned at Birth

## Methodology
**Participants**
   * Target Population: Young adults in the Washington, D.C. metropolitan area
   * Sample Size: 75 survey respondents
   * Final Sample: 70 participants (after excluding incomplete responses)

**Recruitment**
   * Participants were recruited via convenience sampling through the University of Maryland academic community. The survey link was shared via:
      * ELMS (Canvas) course platforms
      * GroupMe chats
      * Courses: Break Through Tech UMD, BSOS 326, INST 414, INST 462, PSYC 341, and SURV 400
      * Text messages to personal contacts (friends and family)
   * Participation was voluntary and self-selected.

## Survey Measures
* Self-Esteem: Adapted Rosenberg Self-Esteem Scale (7 items)
* Exercise Motivation: Selected items from the Exercise Motivation Inventory-2 (EMI-2), categorized into intrinsic and extrinsic subtypes
* Exposure to Overheard Comments: Custom scale of 8 items measuring frequency of exposure to self-deprecating body image remarks overheard from others

## Data Analysis Workflow
* Data Preprocessing
   * Cleaning and excluding cases with substantial missing data
   * Recoding and reverse-coding items as needed
* Assumption Testing
   * Normality, linearity, and multicollinearity checks
* Correlation Analysis
   * Pearson’s r and Spearman’s ρ to explore relationships between variables
* Regression Modeling
   * Linear regression and locally estimated scatterplot smoothing (Loess)
* Moderation Analysis
   * Interaction models to test whether self-esteem moderates the impact of overheard comments on exercise motivation



## Required Dependencies

# Computations
* library(psych) -- Reliability analysis (alpha, omega), item analysis
* library(car) -- Non-constant variance tests

# Data Wrangling
* library(dplyr)
* library(tidyr)
* library(broom) -- Convert statistical objects into tidy tibbles

# Visualization
* library(ggplot2)
* library(patchwork) -- Arrange multiple plots into a grid
* library(jtools) -- APA-style plots
* library(ggcorrplot) -- Correlation matrix plots
* library(interactions) -- Visualizing interaction effects in regression

# Data Availability
* CSV data file not shared due to participant privacy
* Only processed figures and summary statistics are included

# Course & Attribution
* Project for SURV400: Survey Methodology, Spring 2025
* Conducted at University of Maryland, College Park