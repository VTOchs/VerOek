# Load required libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(broom)

# Read and preprocess data
# Only select variables starting with 'v_' and the group variable
# Use stringsAsFactors = FALSE for easier manipulation

data <- read.csv2("Data/data_project_1070426_2025_07_02.csv", stringsAsFactors = FALSE) %>% 
  select(starts_with("v_"), rnd_pg_7365787)

# Rename and recode group and key variables for clarity
names(data)[names(data) == "rnd_pg_7365787"] <- "group"
names(data)[names(data) == "v_1"] <- "age"
names(data)[names(data) == "v_9"] <- "climate_info"
names(data)[names(data) == "v_16"] <- "climate_fear"
data$group <- as.character(data$group)
data$group[data$group == "7365788"] <- "pos"
data$group[data$group == "7365789"] <- "neg"
data$group[data$group == "7365790"] <- "control"

# Check sample size and group distribution
nrow(data) # 110
table(data$group) # pos: 38; neg: 31; control: 41

# Variable key (for reference)
# v1 = age
# v2-4 = gender
# v9 = Info Klima
# v10-13 = Stadtgröße
# v15 = Bildungsjahre
# v16 = Sorgen Klima
# v38 = persönliche Bedrohung
# v40 = Häufigkeit Gedanke Klimakrise
# v41 = Sorge Nahestehende
# v42 = Verhalten Einfluss Klima
# v51 = Zukunft gefährlich
# v52 = Wetterereignisse Region
# v53 = globale Instabilität
# v54 = Risiko für Gesellschaft
# v17 = Reduktion Fleisch
# v18 = Fahrrad/Fuß
# v19 = Verzicht Kurzstreckenflug
# v20 = umweltfreundlicher Supermarktkonsum
# v21 = Energie/Müll grüner
# v22 = Fast Fashion Verzicht
# v23 = politischer Einsatz Klima

# Age: clean and filter
summary(data$age) # between 16 and 68
table(is.na(data$age)) # no missings
table(data$age) #
# Drop implausible ages and underage people
data <- data %>% filter(age >= 18 & age <= 100)
nrow(data) # 108

# Age Histogram (ggplot2)
# Visualizes the age distribution of the sample
ggplot(data, aes(x = as.numeric(age))) +
  geom_histogram(bins = 20, fill = "#0072B2", color = "white", alpha = 0.8) +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal(base_size = 14)
ggsave("graphs/age_histogram.png", width = 8, height = 6)

# Gender: check coding and plot
# v_2 = female, v_3 = male, v_4 = diverse
# Check for multiple selections and summarize
table(data$v_2, useNA="ifany")
table(data$v_3, useNA="ifany")
table(data$v_4, useNA="ifany")
table(rowSums(data[,c("v_2","v_3","v_4")])) # check for multiple selections

# Gender Barplot (ggplot2)
# Shows the gender distribution in the sample
gender_counts <- colSums(data[,c("v_2","v_3","v_4")], na.rm=TRUE)
gender_df <- data.frame(
  Gender = c("Female", "Male", "Diverse"),
  Count = as.numeric(gender_counts)
)

ggplot(gender_df, aes(x = Gender, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("Female" = "#E69F00", "Male" = "#56B4E9", "Diverse" = "#009E73")) +
  labs(title = "Gender Distribution", y = "Count", x = "Gender") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
ggsave("graphs/gender_barplot.png", width = 8, height = 6)

# City Size: check and clean
# v_10 to v_13 are city size categories
# Ensure only one city size per respondent
# Convert to numeric and filter for valid responses
table(data$v_10, useNA="ifany")
table(data$v_11, useNA="ifany")
table(data$v_12, useNA="ifany")
table(data$v_13, useNA="ifany")
table(rowSums(data[,c("v_10","v_11","v_12","v_13")]))
data <- data %>%
  mutate(across(v_10:v_13, as.numeric)) %>%
  filter(rowSums(select(., v_10:v_13), na.rm = TRUE) <= 1)
nrow(data) # 105

table(data$rnd_pg_7365787) # pos: 35; neg: 31; control: 39

# City Size Barplot (ggplot2)
# Shows the distribution of city sizes in the sample
city_size_counts <- colSums(data[,c("v_10","v_11","v_12","v_13")], na.rm=TRUE)
city_size_df <- data.frame(
  CitySize = c("over 100k", "20k-100k", "5k-20k", "under 5k"),
  Count = as.numeric(city_size_counts)
)
ggplot(city_size_df, aes(x = CitySize, y = Count, fill = CitySize)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("over 100k" = "#0072B2", "20k-100k" = "#56B4E9", "5k-20k" = "#009E73", "under 5k" = "#E69F00")) +
  labs(title = "City Size Distribution", y = "Count", x = "City Size") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
ggsave("graphs/city_size_barplot.png", width = 8, height = 6)

# Education: check and plot
summary(data$v_15)
table(is.na(data$v_15)) # no missings
# Filter for plausible values (<= 25 years)
data_edu_plot <- data %>% filter(as.numeric(v_15) <= 25)
# Education Histogram (ggplot2)
# Shows the distribution of years of education
ggplot(data_edu_plot, aes(x = as.numeric(v_15))) +
  geom_histogram(bins = 20, fill = "#009E73", color = "white", alpha = 0.8) +
  labs(title = "Years of Education", x = "Years", y = "Count") +
  theme_minimal(base_size = 14)
ggsave("graphs/education_histogram.png", width = 8, height = 6)

# Info Klima: barplot of self-reported climate information
# 1 = sehr wenig, ..., 5 = sehr viel
tabKlima <- table(data$climate_info)
info_df <- data.frame(
  Info = factor(names(tabKlima), levels = c("1","2","3","4","5")),
  Count = as.numeric(tabKlima)
)
ggplot(info_df, aes(x = Info, y = Count, fill = Info)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("1" = "#D55E00", "2" = "#E69F00", "3" = "#F0E442", "4" = "#009E73", "5" = "#0072B2")) +
  labs(title = "Ich bin sehr informiert über Klimaschutzthemen", y = "Count", x = "Info-Level") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
ggsave("graphs/info_klima_barplot.png", width = 8, height = 6)

# Sorgen Klima: barplot of climate fear
# 1 = sehr wenig, ..., 5 = sehr viel
tabSorgen <- table(data$climate_fear)
sorgen_df <- data.frame(
  Sorgen = factor(names(tabSorgen), levels = c("1","2","3","4","5")),
  Count = as.numeric(tabSorgen)
)
ggplot(sorgen_df, aes(x = Sorgen, y = Count, fill = Sorgen)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = c("1" = "#D55E00", "2" = "#E69F00", "3" = "#F0E442", "4" = "#009E73", "5" = "#0072B2")) +
  labs(title = "Ich bin sehr besorgt über den Klimawandel", y = "Count", x = "Sorgen-Level") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")
ggsave("graphs/sorgen_klima_barplot.png", width = 8, height = 6)

# Create behavioral and anxiety indices
# index_behav: mean of v_17 to v_23 (pro-environmental behaviors)
v_17_23_names <- paste0("v_", 17:23)
v_17_23_names <- v_17_23_names[v_17_23_names %in% names(data)]
data$index_behav <- rowMeans(sapply(data[v_17_23_names], as.numeric), na.rm = TRUE)
summary(data$index_behav)

# index_angst: mean of v_38 to v_54 (climate anxiety/concern)
v_38_54_names <- paste0("v_", 38:54)
v_38_54_names <- v_38_54_names[v_38_54_names %in% names(data)]
data$index_angst <- rowMeans(sapply(data[v_38_54_names], as.numeric), na.rm = TRUE)
summary(data$index_angst)

# index_behav Boxplot (ggplot2)
# Shows the distribution of the behavioral index by group
ggplot(data, aes(x = group, y = index_behav, fill = group)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  scale_fill_manual(values = c("pos" = "#D55E00", "neg" = "#0072B2", "control" = "#009E73")) +
  labs(title = "Verhaltensindex nach Gruppen", x = "Gruppe", y = "Verhaltensindex") +
  theme_minimal(base_size = 14)
ggsave("graphs/index_behav_boxplot.png", width = 8, height = 6)

# index_angst Boxplot (ggplot2)
# Shows the distribution of the anxiety index by group
ggplot(data, aes(x = group, y = index_angst, fill = group)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red") +
  scale_fill_manual(values = c("pos" = "#D55E00", "neg" = "#0072B2", "control" = "#009E73")) +
  labs(title = "Sorgenindex nach Gruppen", x = "Gruppe", y = "Sorgenindex") +
  theme_minimal(base_size = 14)
ggsave("graphs/index_angst_boxplot.png", width = 8, height = 6)

# index_behav Histogram (ggplot2)
# Shows the distribution of the behavioral index
ggplot(data, aes(x = index_behav)) +
  geom_histogram(bins = 20, fill = "#D55E00", color = "white", alpha = 0.8) +
  labs(title = "Verhaltensindex", x = "Verhaltensindex", y = "Count") +
  theme_minimal(base_size = 14)
ggsave("graphs/index_behav_histogram.png", width = 8, height = 6)

# index_angst Histogram (ggplot2)
# Shows the distribution of the anxiety index
ggplot(data, aes(x = index_angst)) +
  geom_histogram(bins = 20, fill = "#56B4E9", color = "white", alpha = 0.8) +
  labs(title = "Sorgenindex", x = "Sorgenindex", y = "Count") +
  theme_minimal(base_size = 14)
ggsave("graphs/index_angst_histogram.png", width = 8, height = 6)

# index_behav vs. index_angst scatterplot (ggplot2)
# Visualizes the relationship between behavioral and anxiety indices, colored by group
ggplot(data, aes(x = index_behav, y = index_angst, color = group)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = c("pos" = "#D55E00", "neg" = "#0072B2", "control" = "#009E73")) +
  labs(title = "Verhaltensindex vs. Sorgenindex", x = "Verhaltensindex", y = "Sorgenindex") +
  theme_minimal(base_size = 14) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") +
  theme(legend.position = "top")
ggsave("graphs/index_behav_angst_scatterplot.png", width = 8, height = 6)

# Group summary statistics for reporting
data %>%
  group_by(group) %>%
  summarise(
    mean_behav = mean(index_behav, na.rm = TRUE),
    median_behav = median(index_behav, na.rm = TRUE),
    mean_angst = mean(index_angst, na.rm = TRUE),
    median_angst = median(index_angst, na.rm = TRUE)
  )

# Main Analysis (ANOVA)
# Gender: get the column name where value is 1, then label
# City size: get the column name where value is 1, then label
data$gender <- apply(data[,c("v_2","v_3","v_4")], 1, function(x) which(x == 1)[1])
data$gender <- factor(data$gender, labels = c("gender1", "gender2", "gender3"))
data$city_size <- apply(data[,c("v_10","v_11","v_12","v_13")], 1, function(x) which(x == 1)[1])
data$city_size <- factor(data$city_size, labels = c("city1", "city2", "city3", "city4"))

# Run ANOVA models for behavioral and anxiety indices, with and without attitude controls
anova_behav <- aov(index_behav ~ group + age + gender + city_size + climate_info + climate_fear, data = data)
summary(anova_behav)

anova_angst <- aov(index_angst ~ group + age + gender + city_size + climate_info + climate_fear, data = data)
summary(anova_angst)

anova_behav_short <- aov(index_behav ~ group + age + gender + city_size, data = data)
summary(anova_behav_short)

anova_angst_short <- aov(index_angst ~ group + age + gender + city_size, data = data)
summary(anova_angst_short)

# Create a professional ANOVA summary table using modelsummary
library(modelsummary)
models <- list(
  "Behavior index (all controls)"  = anova_behav,
  "Behavior index (no attitude questions)" = anova_behav_short,
  "Anxiety index (all controls)"   = anova_angst,
  "Anxiety index (no attitude questions)"  = anova_angst_short
)

# Print a classic table with stars, ready for copy-paste or export
modelsummary(
  models,
  stars = TRUE,
  statistic = "({p.value})",
  gof_omit = "AIC|BIC|Log.Lik|Adj.R2|RMSE|Deviance|R2|Sigma|F|Num.Obs|Std.Errors",
  output = "markdown" # or "html", "latex", "docx", "pptx"
)

# Export as HTML for PowerPoint
modelsummary(
  models,
  stars = TRUE,
  statistic = "({p.value})",
  gof_omit = "AIC|BIC|Log.Lik|Adj.R2|RMSE|Deviance|R2|Sigma|F|Num.Obs|Std.Errors",
  output = "anova_table.html"
)
