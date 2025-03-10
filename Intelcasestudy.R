# ============================================================
# Intelcasestudy.R: Smartwatch Market Segmentation Analysis
# ============================================================
# =========================

# -----------------------------------
# 1. INSTALL & LOAD REQUIRED PACKAGES
# -----------------------------------

# Install necessary packages (run only once)
install.packages("readxl")
install.packages("tidyverse")
install.packages("cluster")
install.packages("factoextra")
install.packages("openxlsx")

# Load required libraries
library(readxl)      # For reading Excel files
library(tidyverse)   # For data manipulation and visualization
library(cluster)     # For clustering analysis
library(factoextra)  # For visualizing clustering results
library(openxlsx)    # For exporting results

# -----------------------------------
# 2. IMPORT DATASET
# -----------------------------------

# Read the dataset
df <- read_excel("~/Desktop/SmartWatch Data File.xlsx")
> head(df)  # Check if it loaded correctly
# A tibble: 6 × 12
  ConstCom TimelyInf TaskMgm DeviceSt Wellness Athlete Style AmznP Female Degree Income   Age
     <dbl>     <dbl>   <dbl>    <dbl>    <dbl>   <dbl> <dbl> <dbl>  <dbl>  <dbl>  <dbl> <dbl>
1        3         2       3        3        2       3     3     1      1      1      2    38
2        6         6       6        6        5       3     1     1      0      2      3    38
3        7         4       4        4        6       4     1     0      0      1      3    42
4        7         5       4        5        5       4     4     1      0      2      5    35
5        7         4       2        6        3       2     4     1      0      1      3    36
6        2         1       3        2        2       4     3     0      1      2      2    47
# View structure of the dataset
str(df)
summary(df)

# -----------------------------------
# 3. DATA PREPROCESSING & CLEANING
# -----------------------------------

# Check for missing values
colSums(is.na(df))  # No missing values found

# Standardize numerical features (excluding demographic variables)
features <- df[, c("ConstCom", "TimelyInf", "TaskMgm", "DeviceSt", 
                   "Wellness", "Athlete", "Style")]

# Scale the features
df_scaled <- scale(features)

# Summary of standardized features
summary(df_scaled)

# -----------------------------------
# 4. CLUSTERING ANALYSIS
# -----------------------------------

## 4.1 Determining Optimal K

# Elbow Method
fviz_nbclust(df_scaled, FUN = kmeans, method = "wss") +
  ggtitle("Elbow Method: Optimal Number of Clusters")

# Silhouette Method
fviz_nbclust(df_scaled, FUN = kmeans, method = "silhouette") +
  ggtitle("Silhouette Method: Optimal Number of Clusters")

## 4.2 K-Means Clustering (k = 3)

# Run K-Means with 3 clusters
set.seed(123)  # Ensuring reproducibility
kmeans_3 <- kmeans(df_scaled, centers = 3, nstart = 25)

# Add cluster labels to original data
df_final <- df
df_final$cluster <- as.factor(kmeans_3$cluster)

# Save clustered dataset
write.csv(df_final, "Smartwatch_Segmented_Data_K3.csv", row.names = FALSE)

# Cluster Size Comparison
cluster_counts <- as.data.frame(table(df_final$cluster))

# Visualizing Cluster Size
p_size <- ggplot(cluster_counts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  ggtitle("Cluster Size Comparison") +
  xlab("Cluster") + 
  ylab("Number of Respondents") +
  theme(legend.position = "none")

ggsave("Cluster_Size_Comparison.png", plot = p_size, width = 8, height = 6)

# -----------------------------------
# 5. CLUSTER FEATURE ANALYSIS
# -----------------------------------

# Compute mean values for each feature per cluster
cluster_summary <- df_final %>%
  group_by(cluster) %>%
  summarise(across(where(is.numeric), mean, .names = "{col}_mean"))

# Save cluster feature summary
write.csv(cluster_summary, "Smartwatch_Cluster_Summary.csv", row.names = FALSE)

# Feature Importance Heatmap
heatmap_data <- as.matrix(cluster_summary[, -1])

heatmap(heatmap_data, col = heat.colors(10), 
        main = "Feature Importance by Cluster", 
        xlab = "Features", ylab = "Clusters")

ggsave("Feature_Importance_Heatmap.png")

# -----------------------------------
# 6. DEMOGRAPHIC ANALYSIS
# -----------------------------------

## 6.1 Gender Distribution

# Convert gender to factor
df_final$Female <- factor(df_final$Female, labels = c("Male", "Female"))

# Gender Distribution per Cluster
p_gender <- ggplot(df_final, aes(x = cluster, fill = Female)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  ggtitle("Gender Distribution per Cluster") +
  xlab("Cluster") +
  ylab("Proportion") +
  scale_fill_manual(values = c("steelblue", "lightblue"))

ggsave("Gender_Distribution_by_Cluster.png", plot = p_gender, width = 8, height = 6)

## 6.2 Age Distribution (Boxplot)

p_age <- ggplot(df_final, aes(x = cluster, y = Age, fill = cluster)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Age Distribution by Cluster") +
  xlab("Cluster") +
  ylab("Age")

ggsave("Age_Distribution_by_Cluster.png", plot = p_age, width = 8, height = 6)

## 6.3 Income Distribution

# Convert income to factor
df_final$Income <- factor(df_final$Income, 
                          levels = c(1, 2, 3, 4, 5),
                          labels = c("Very Low", "Low", "Moderate", "High", "Very High"))

p_income <- ggplot(df_final, aes(x = cluster, fill = Income)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  ggtitle("Income Distribution by Cluster") +
  xlab("Cluster") +
  ylab("Proportion")

ggsave("Income_Distribution_by_Cluster.png", plot = p_income, width = 8, height = 6)

# -----------------------------------
# 7. STATISTICAL VALIDATION (ANOVA & CHI-SQUARE)
# -----------------------------------

## 7.1 ANOVA for Age Differences

anova_age <- aov(Age ~ cluster, data = df_final)
summary(anova_age)

# Tukey's HSD Test for Age
tukey_age <- TukeyHSD(anova_age)
print(tukey_age)

## 7.2 Chi-Square Test for Gender, Income, and Degree

# Gender vs Cluster
gender_cluster_table <- table(df_final$Female, df_final$cluster)
chi_gender <- chisq.test(gender_cluster_table)
print(chi_gender)

# Income vs Cluster
income_cluster_table <- table(df_final$Income, df_final$cluster)
chi_income <- chisq.test(income_cluster_table)
print(chi_income)

# Degree vs Cluster
degree_cluster_table <- table(df_final$Degree, df_final$cluster)
chi_degree <- chisq.test(degree_cluster_table)
print(chi_degree)

# -----------------------------------
# 8. CONCLUSION
# -----------------------------------

print("Smartwatch segmentation analysis completed. All outputs saved.")