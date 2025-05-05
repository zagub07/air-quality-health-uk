

# === From script4.R ===
library(dplyr)
library(readr)

# Load the dataset
health_data <- read_csv("indicators-England.data.csv")

# View all unique indicators to explore what's available
unique_indicators <- unique(health_data$`Indicator Name`)
print(unique_indicators)

# Filter for heart-related indicators
heart_keywords <- c("heart", "cardio", "stroke", "hypertension", "coronary")

heart_data <- health_data %>%
  filter(
    grepl(paste(heart_keywords, collapse = "|"), `Indicator Name`, ignore.case = TRUE)
  )

# Preview result
head(heart_data)


# === From script5.R ===
airquality_uk <- read_csv("uk_air.csv")
respiratory_health <- read_csv("uk_respiratory_admissions_mortality.csv")

merged_data <- uk_air %>%
  left_join(respiratory_health, by = "Region")

# Correlation between NO2 and Hospital Admissions
cor(merged_data$no2_concentration, merged_data$Hospital_Admissions, use = "complete.obs")

# Correlation between PM10 and Hospital Admissions
cor(merged_data$pm10_concentration, merged_data$Hospital_Admissions, use = "complete.obs")

# Correlation between PM25 and Hospital Admissions
cor(merged_data$pm25_concentration, merged_data$Hospital_Admissions, use = "complete.obs")

# Correlation between NO2 and Mortality Rate
cor(merged_data$no2_concentration, merged_data$Mortality_Rate, use = "complete.obs")

# Correlation between PM10 and Mortality Rate
cor(merged_data$pm10_concentration, merged_data$Mortality_Rate, use = "complete.obs")

# Correlation between PM25 and Mortality Rate
cor(merged_data$pm25_concentration, merged_data$Mortality_Rate, use = "complete.obs")

# === From script6.R ===
merged_df <- uk_air %>%
  inner_join(IHD_Mortality_by_Region, by = "Region")


regional_pollution <- uk_air %>%
  group_by(Region) %>%
  summarise(
    mean_no2 = mean(no2_concentration, na.rm = TRUE),
    mean_pm10 = mean(pm10_concentration, na.rm = TRUE),
    mean_pm25 = mean(pm25_concentration, na.rm = TRUE)
  )



cor(merged_df$mean_no2, merged_df$`ASMR per 100,000`, use = "complete.obs")
cor(merged_df$mean_pm10, merged_df$`ASMR per 100,000`, use = "complete.obs")
cor(merged_df$mean_pm25, merged_df$`ASMR per 100,000`, use = "complete.obs")


merged_df <- merged_df %>%
  mutate(
    mean_no2 = as.numeric(mean_no2),
    `ASMR per 100,000` = as.numeric(`ASMR per 100,000`)
  )