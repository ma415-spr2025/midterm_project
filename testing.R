# Load necessary libraries
library(tidyverse)
library(knitr)
library(kableExtra)
library(stringr)

# Load the dataset
file_path <- "strawb_mar6.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Drop single-value columns
df <- drop_one_value_col(df)

# Filter for California and Florida and only strawberry-related data
df_filtered <- df %>%
  filter(State %in% c("CALIFORNIA", "FLORIDA"),
         grepl("STRAWBERRIES", Commodity, ignore.case = TRUE))

# Split Data Item into separate columns
df_filtered <- df_filtered |>
  separate_wider_delim(cols = `Data.Item`,
                       delim = ",",
                       names = c("Fruit", "Category", "Item", "Metric"),
                       too_many = "error",
                       too_few = "align_start")

# Split data into Census and Survey
df_census <- df_filtered %>% filter(Program == "CENSUS")
df_survey <- df_filtered %>% filter(Program == "SURVEY")

# Identify chemical treatment data
df_chemicals <- df_filtered %>%
  filter(grepl("CHEMICAL", Category, ignore.case = TRUE))

# Select three chemicals with divergent use patterns
chemicals_of_interest <- df_chemicals %>%
  group_by(State, Item) %>%
  summarise(Total_Use = sum(as.numeric(gsub(",", "", Value)), na.rm = TRUE)) %>%
  spread(State, Total_Use) %>%
  arrange(desc(abs(CALIFORNIA - FLORIDA))) %>%
  head(3)

# Convert numbers formatted in thousands properly
df_filtered <- df_filtered %>%
  mutate(Value = as.numeric(gsub(",", "", Value)))

# Compare production and sales of organic vs. conventional strawberries
df_production <- df_filtered %>%
  filter(grepl("PRODUCTION|SALES", Category, ignore.case = TRUE))

# Generate summary statistics
summary_stats <- df_production %>%
  group_by(State, Year, Category) %>%
  summarise(Total_Value = sum(Value, na.rm = TRUE))

# Visualize trends
ggplot(summary_stats, aes(x = Year, y = Total_Value, color = State)) +
  geom_line() +
  facet_wrap(~ Category, scales = "free_y") +
  labs(title = "Trends in Strawberry Production and Sales",
       y = "Total Value",
       x = "Year") +
  theme_minimal()

# Output tables
print(chemicals_of_interest)
print(summary_stats)
