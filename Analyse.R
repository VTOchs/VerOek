library(tidyverse)
library(ggplot2)
library(dplyr)

data <- read.csv2("Data/data_project_1070426_2025_07_02.csv", stringsAsFactors = FALSE) %>% 
  select(starts_with("v_"), rnd_pg_7365787)

# Rename & recode group
names(data)[names(data) == "rnd_pg_7365787"] <- "group"
data$group <- as.character(data$group)
data$group[data$group == "7365788"] <- "pos"
data$group[data$group == "7365789"] <- "neg"
data$group[data$group == "7365790"] <- "control"


nrow(data) # 110
table(data$group) # pos: 38; neg: 31; control: 41

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

# Age
summary(data$v_1) # between 16 and 68
table(is.na(data$v_1)) # no missings
table(data$v_1) #
# Drop implausible ages and underage people
data <- data %>% filter(v_1 >= 18 & v_1 <= 100)
nrow(data) # 108

# Make a histogram
png("graphs/age_histogram.png", width=800, height=600)
hist(as.numeric(data$v_1),
     breaks=20,
     main="Age Distribution",
     xlab="Age",
     xlim=c(16, 70),
     ylim=c(0, 30),
     xaxt="n",
     yaxt="n")
axis(1, at=seq(15, 70, by=5))
axis(2, at=seq(0, 30, by=5))
dev.off()



# Gender

table(data$v_2, useNA="ifany")
table(data$v_3, useNA="ifany")
table(data$v_4, useNA="ifany")
# 64 female, 43 male, 1 diverse

# Check for multiple selections
table(rowSums(data[,c("v_2","v_3","v_4")]))
png("graphs/gender_barplot.png", width=800, height=600)
barplot(colSums(data[,c("v_2","v_3","v_4")], na.rm=TRUE),
        names.arg=c("Female", "Male", "Diverse"),
        main="Gender Distribution", ylab="Count")
axis(2, at=seq(0, 65, by=5))
dev.off()



# City Size

table(data$v_10, useNA="ifany")
table(data$v_11, useNA="ifany")
table(data$v_12, useNA="ifany")
table(data$v_13, useNA="ifany")
# Check for multiple selections
table(rowSums(data[,c("v_10","v_11","v_12","v_13")]))

# Make sure v_10 to v_13 are numeric (if not, convert them)
data <- data %>%
  mutate(across(v_10:v_13, as.numeric))
# Filter out rows where the sum of v_10 to v_13 is greater than 1
data <- data %>%
  filter(rowSums(select(., v_10:v_13), na.rm = TRUE) <= 1)
nrow(data) # 105
table(data$rnd_pg_7365787) # pos: 35; neg: 31; control: 39
# Make a barplot
png("graphs/city_size_barplot.png", width=800, height=600)
barplot(colSums(data[,c("v_10","v_11","v_12","v_13")], na.rm=TRUE),
        names.arg=c("over 100k", "20k-100k", "5k-20k", "under 5k"),
        main="City Size Distribution", ylab="Count")
dev.off()



# Education
summary(data$v_15)
table(is.na(data$v_15)) # no missings
# Check for implausible values
data_edu_plot <- data %>% filter(as.numeric(v_15) <= 25)

png("graphs/education_histogram.png", width=800, height=600)
hist(as.numeric(data_edu_plot$v_15), breaks=20, main="Years of Education", xlab="Years")
dev.off()



# Info Klima
tabKlima <- table(data$v_9)
tabKlima <- c(tabKlima, "1" = 0)
tabKlima <- tabKlima[order(as.numeric(names(tabKlima)))]
png("graphs/info_klima_barplot.png", width=800, height=600)
barplot(tabKlima, main="Ich bin sehr informiert über Klimaschutzthemen", ylab="Count",
        names.arg=c("sehr wenig", "wenig", "mittel", "viel", "sehr viel"),
        col=c("red", "orange", "yellow", "green", "blue"), ylim=c(0, 45))
axis(2, at=seq(0, 45, by=5))
dev.off()
barplot(table(data$v_9))

# Sorgen Klima
tabSorgen <- table(data$v_16)
png("graphs/sorgen_klima_barplot.png", width=800, height=600)
barplot(tabSorgen, main="Ich bin sehr besorgt über den Klimawandel", ylab="Count",
        names.arg=c("sehr wenig", "wenig", "mittel", "viel", "sehr viel"),
        col=c("red", "orange", "yellow", "green", "blue"), ylim=c(0, 45))
axis(2, at=seq(0, 45, by=5))
dev.off()








# Analysis individual groups

# 5 möglichst umweltfreundlich -  1 nicht umweltfreundlich

# Get the names of columns that match v_17 to v_23 and exist in your data
v_17_23_names <- paste0("v_", 17:23)
v_17_23_names <- v_17_23_names[v_17_23_names %in% names(data)]
# Create the index variable (row mean)
data$index_behav <- rowMeans(sapply(data[v_17_23_names], as.numeric), na.rm = TRUE)
summary(data$index_behav)

v_38_54_names <- paste0("v_", 38:54)
v_38_54_names <- v_38_54_names[v_38_54_names %in% names(data)]
data$index_angst <- rowMeans(sapply(data[v_38_54_names], as.numeric), na.rm = TRUE)
summary(data$index_angst)


png("graphs/index_behav_boxplot.png", width=800, height=600)
boxplot(index_behav ~ group, data = data,
        main = "Verhalten Klimaschutz nach Gruppen",
        xlab = "Gruppe",
        ylab = "Verhaltensindex")
dev.off()

png("graphs/index_angst_boxplot.png", width=800, height=600)
boxplot(index_angst ~ group, data = data,
        main = "Sorgen Klimawandel nach Gruppen",
        xlab = "Gruppe",
        ylab = "Sorgenindex")
dev.off()


png("graphs/index_behav_histogram.png", width=800, height=600)
hist(data$index_behav, main="Verhaltensindex", xlab="Verhaltensindex")
dev.off()
png("graphs/index_angst_histogram.png", width=800, height=600)
hist(data$index_angst, main="Sorgenindex", xlab="Sorgenindex")
dev.off()


# Colored Scatterplot Index vs. Group
group_colors <- ifelse(data$group == "pos", "red",
                  ifelse(data$group == "neg", "blue",
                  ifelse(data$group == "control", "green", "black")))

png("graphs/index_behav_angst_scatterplot.png", width=800, height=600)
plot(data$index_behav, data$index_angst,
     main="Verhaltensindex vs. Sorgenindex",
     xlab="Verhaltensindex", ylab="Sorgenindex",
     col=group_colors, pch=19)
legend("topright", legend=c("pos", "neg", "control"),
       col=c("red", "blue", "green"), pch=19)

# Add linear regression line (for all data)
model <- lm(index_angst ~ index_behav, data = data)
abline(model, col="black", lwd=1, lty=2)

dev.off()

data %>%
  group_by(group) %>%
  summarise(
    mean_behav = mean(index_behav, na.rm = TRUE),
    median_behav = median(index_behav, na.rm = TRUE),
    mean_angst = mean(index_angst, na.rm = TRUE),
    median_angst = median(index_angst, na.rm = TRUE)
  )


# Main Analysis (ANOVA)

# Gender: get the column name where value is 1
data$gender <- apply(data[,c("v_2","v_3","v_4")], 1, function(x) which(x == 1)[1])
data$gender <- factor(data$gender, labels = c("gender1", "gender2", "gender3"))

# City size: get the column name where value is 1
data$city_size <- apply(data[,c("v_10","v_11","v_12","v_13")], 1, function(x) which(x == 1)[1])
data$city_size <- factor(data$city_size, labels = c("city1", "city2", "city3", "city4"))


anova_behav <- aov(index_behav ~ group + as.numeric(v_1) + gender + city_size + as.numeric(v_9) + as.numeric(v_16), data = data)
summary(anova_behav)

anova_angst <- aov(index_angst ~ group + as.numeric(v_1) + gender + city_size + as.numeric(v_9) + as.numeric(v_16), data = data)
summary(anova_angst)

# w/o Info & Sorge Kontrolle

anova_behav_short <- aov(index_behav ~ group + as.numeric(v_1) + gender + city_size, data = data)
summary(anova_behav_short)

anova_angst_short <- aov(index_angst ~ group + as.numeric(v_1) + gender + city_size, data = data)
summary(anova_angst_short)