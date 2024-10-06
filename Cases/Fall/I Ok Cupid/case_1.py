library(dplyr)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(leaflet.extras)
library(mapproj)
library(lubridate)
library(DataExplorer)
library(stringr)
library(tidyr)
library(knitr)
library(esquisse)

setwd("/Users/alvaroramirez/Library/CloudStorage/OneDrive-Personal/estudio/Harvard/Classes/CSCI E-96/CSCI E-96/Cases/Fall/I Ok Cupid")

profiles <- read.csv("profiles.csv", stringsAsFactors = FALSE)
latlon <- read.csv("LatLon.csv", stringsAsFactors = FALSE)

summary(profiles)
summary(latlon)

str(profiles)
str(latlon)

"""NOTE: latlon and profiles have a one-to-many relationship. The field used to
      generate this link between both tables is 'location'.
"""

missing_summary <- sapply(profiles, function(x) sum(is.na(x)))
missing_percentage <- sapply(profiles, function(x) mean(is.na(x)) * 100)

missing_data <- data.frame(
  Column = names(profiles),
  MissingValues = missing_summary,
  MissingPercentage = missing_percentage
)

print(missing_data)

create_df_with_counts <- function(column) {
  column_clean <- ifelse(is.na(column), "<NA>", column)
  unique_values <- unique(column_clean)
  df <- data.frame(
    UniqueValue = unique_values,
    Count = sapply(unique_values, function(x) sum(column_clean == x))
  )
  df$Percentage <- (df$Count / length(column)) * 100
  return(list(df = df, num_categories = length(unique_values)))
}

print_df_with_category_count <- function(df_list, column_name) {
  cat("Number of categories for '", column_name, "': ",
      df_list$num_categories, "\n", sep = "")
  print(df_list$df)
  cat("\n")
}

df_body_type <- create_df_with_counts(profiles$body_type)
print_df_with_category_count(df_body_type, 'body_type')

df_diet <- create_df_with_counts(profiles$diet)
print_df_with_category_count(df_diet, 'diet')

df_drinks <- create_df_with_counts(profiles$drinks)
print_df_with_category_count(df_drinks, 'drinks')

df_drugs <- create_df_with_counts(profiles$drugs)
print_df_with_category_count(df_drugs, 'drugs')

df_education <- create_df_with_counts(profiles$education)
print_df_with_category_count(df_education, 'education')

df_ethnicity <- create_df_with_counts(profiles$ethnicity)
print_df_with_category_count(df_ethnicity, 'ethnicity')

df_job <- create_df_with_counts(profiles$job)
print_df_with_category_count(df_job, 'job')

df_location <- create_df_with_counts(profiles$location)
print_df_with_category_count(df_location, 'location')

df_offspring <- create_df_with_counts(profiles$offspring)
print_df_with_category_count(df_offspring, 'offspring')

df_orientation <- create_df_with_counts(profiles$orientation)
print_df_with_category_count(df_orientation, 'orientation')

df_pets <- create_df_with_counts(profiles$pets)
print_df_with_category_count(df_pets, 'pets')

df_religion <- create_df_with_counts(profiles$religion)
print_df_with_category_count(df_religion, 'religion')

df_sex <- create_df_with_counts(profiles$sex)
print_df_with_category_count(df_sex, 'sex')

df_sign <- create_df_with_counts(profiles$sign)
print_df_with_category_count(df_sign, 'sign')

df_smokes <- create_df_with_counts(profiles$smokes)
print_df_with_category_count(df_smokes, 'smokes')

df_speaks <- create_df_with_counts(profiles$speaks)
print_df_with_category_count(df_speaks, 'speaks')

df_status <- create_df_with_counts(profiles$status)
print_df_with_category_count(df_status, 'status')

write.csv(df_speaks$df, file = "speaks.csv", row.names = FALSE)

process_speaks <- function(speaks) {
  if (is.na(speaks) || speaks == "") {
    return("")
  }
  languages <- unlist(strsplit(speaks, ","))
  languages <- str_trim(languages)
  languages <- unique(languages)
  return(paste(languages, collapse = ", "))
}

profiles$speaks <- sapply(profiles$speaks, process_speaks)

library(dplyr)

profiles <- profiles %>%
  mutate(language_count = sapply(strsplit(speaks, ", "), length))

language_counts <- profiles %>%
  separate_rows(speaks, sep = ", ") %>%
  group_by(speaks) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

language_level_counts <- profiles %>%
  separate_rows(speaks, sep = ", ") %>%
  mutate(language = str_extract(speaks, "^[^()]+"),
         level = str_extract(speaks, "\\(([^)]+)\\)")) %>%
  group_by(language, level) %>%
  summarise(count = n()) %>%
  arrange(language, desc(count))

print(profiles)
print(language_counts)
print(language_level_counts)

process_speaks <- function(speaks) {
  if (is.na(speaks) || speaks == "") {
    return("english")
  }
  languages <- unlist(strsplit(speaks, ","))
  languages <- str_trim(languages)
  languages <- gsub("\\s+", "", languages)
  languages <- unique(languages)
  return(paste(languages, collapse = ", "))
}

profiles$speaks <- sapply(profiles$speaks, process_speaks)

profiles <- profiles %>%
  mutate(language_count = sapply(strsplit(speaks, ", "), length))

language_level_counts <- profiles %>%
  separate_rows(speaks, sep = ", ") %>%
  mutate(language = str_extract(speaks, "^[^()]+"),
         level = str_extract(speaks, "\\(([^)]+)\\)")) %>%
  group_by(language, level) %>%
  summarise(count = n()) %>%
  arrange(language, desc(count))

totals_per_language <- language_level_counts %>%
  group_by(language) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count))

print(totals_per_language)

profiles <- read.csv("profiles.csv")

fluency_levels <- c(NA, "(poorly)", "(okay)", "(fluently)")

process_speaks <- function(speaks) {
  if (is.na(speaks) || speaks == "") {
    return("")
  }
  languages <- unlist(strsplit(speaks, ","))
  languages <- str_trim(languages)
  languages <- gsub("\\s+", "", languages)
  languages_df <- data.frame(language = str_extract(languages, "^[^()]+"),
                             level = str_extract(languages, "\\(([^)]+)\\)"))
  if (!any(grepl("^english", tolower(languages_df$language)))) {
    languages_df <- rbind(languages_df, data.frame(language = "english", level = NA))
  }
  cleaned_languages_df <- languages_df %>%
    mutate(level = factor(level, levels = fluency_levels, ordered = TRUE)) %>%
    group_by(language) %>%
    filter(if(all(is.na(level))) TRUE else level == max(level, na.rm = TRUE)) %>%
    distinct(language, .keep_all = TRUE) %>%
    ungroup()
  languages <- paste(cleaned_languages_df$language, ifelse(is.na(cleaned_languages_df$level), "", paste0(cleaned_languages_df$level)), sep = "", collapse = ", ")
  return(languages)
}

str <- 'english, english (okay), spanish (okay), spanish (poorly), french, italian (fluently), chinese (fluently), chinese (fluently), thai, thai'
print(process_speaks(str))
profiles_processed <- profiles
profiles_processed$speaks <- sapply(profiles_processed$speaks, process_speaks)
profiles_processed <- profiles_processed %>%
  mutate(language_count = sapply(strsplit(speaks, ", "), length))
language_level_counts <- profiles_processed %>%
  separate_rows(speaks, sep = ", ") %>%
  mutate(language = str_extract(speaks, "^[^()]+"),
         level = str_extract(speaks, "\\(([^)]+)\\)")) %>%
  group_by(language, level) %>%
  summarise(count = n_distinct(row_number())) %>%
  arrange(language, desc(count))
totals_per_language <- language_level_counts %>%
  group_by(language) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count))

print(language_level_counts)
print(totals_per_language)

summary(profiles$age)

invalid_age <- profiles %>%
  filter(age < 18 | age > 100)

invalid_age_count <- nrow(invalid_age)
invalid_age_percentage <- (invalid_age_count / nrow(profiles)) * 100

cat("Number of invalid age records: ", invalid_age_count, "\n")
cat("Percentage of invalid age records: ", invalid_age_percentage, "%\n")

ggplot(profiles, aes(x = factor(0), y = age)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Boxplot of Age", x = "", y = "Age")

ggplot(profiles, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Age Distribution", x = "Age", y = "Count")

summary(profiles$height)

invalid_height <- profiles %>%
  filter(height < 50 | height > 95)

invalid_height_count <- nrow(invalid_height)
invalid_height_percentage <- (invalid_height_count / nrow(profiles)) * 100

cat("Number of invalid height records: ", invalid_height_count, "\n")
cat("Percentage of invalid height records: ", invalid_height_percentage, "%\n")

ggplot(profiles, aes(x = factor(0), y = height)) +
  geom_boxplot(fill = "salmon") +
  theme_minimal() +
  labs(title = "Boxplot of Height", x = "", y = "Height (in inches)")

ggplot(profiles, aes(x = height)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
  theme_minimal() +
  labs(title = "Height Distribution", x = "Height", y = "Count")

summary(profiles$income)

invalid_income <- profiles %>%
  filter(income > 1000000)

invalid_income_count <- nrow(invalid_income)
invalid_income_percentage <- (invalid_income_count / nrow(profiles)) * 100

cat("Number of invalid income records: ", invalid_income_count, "\n")
cat("Percentage of invalid income records: ", invalid_income_percentage, "%\n")

ggplot(profiles, aes(x = factor(0), y = income)) +
  geom_boxplot(fill = "lightgreen") +
  theme_minimal() +
  labs(title = "Boxplot of Income", x = "", y = "Income")

ggplot(profiles, aes(x = income)) +
  geom_histogram(binwidth = 10000, fill = "lightgreen", color = "black") +
  theme_minimal() +
  labs(title = "Income Distribution", x = "Income", y = "Count")

complete_records <- profiles[complete.cases(profiles), ]

complete_data_count <- nrow(complete_records)
complete_data_percentage <- (complete_data_count / nrow(profiles)) * 100

cat("Number of complete records: ", complete_data_count, "\n")
cat("Percentage of complete records: ", complete_data_percentage, "%\n")

ggplot(profiles, aes(x = age, y = income)) +
  geom_point(alpha = 0.5, color = "blue") +
  theme_minimal() +
  labs(title = "Age vs Income", x = "Age", y = "Income")

ggplot(profiles, aes(x = education, y = income, fill = education)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Education Level vs Income", x = "Education Level", y = "Income")

diet_drinks_table <- table(profiles$diet, profiles$drinks)
print(diet_drinks_table)

ggplot(profiles, aes(x = diet, fill = drinks)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Diet vs Drinking Habits", x = "Diet", y = "Count")

profiles_latlon <- merge(profiles, latlon, by = "location")

summary(profiles_latlon)

leaflet(profiles_latlon) %>%
  addTiles() %>%
  addCircleMarkers(~lon, ~lat, radius = 2, color = "blue",
                   fillOpacity = 0.5, popup = ~location) %>%
  addProviderTiles(providers$Stamen.TonerLite)

ggplot(profiles, aes(x = sex, y = income, fill = sex)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Income Distribution by Gender", x = "Gender", y = "Income")

profiles$last_online <- as.Date(profiles$last_online, format = "%Y-%m-%d")

active_users <- profiles %>%
  filter(last_online > Sys.Date() - years(1))

ggplot(active_users, aes(x = location)) +
  geom_bar(fill = "orange") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Active Users by Location", x = "Location", y = "Count")

write.csv(profiles_latlon, "profiles_latlon_cleaned.csv", row.names = FALSE)

table(profiles$orientation)
hist(profiles$age)

table(profiles$age, profiles$orientation)

sum(is.na(profiles$income))
profiles$income[is.na(profiles$income)] <- mean(profiles$income, na.rm = TRUE)

profiles$statEDU <- paste(profiles$status, profiles$education, sep = "_")
table(profiles$statEDU)

moreData <- left_join(profiles, latlon, by = "location")
head(moreData)

completeMoreData <- moreData[complete.cases(moreData), ]
completeMoreData
nrow(completeMoreData)
dim(completeMoreData)