

# === From script7.R ===
# Assume your merged dataset is called merged_air_health_data
# It contains:
# - NO2, PM10, PM25 concentrations
# - Hospital Admissions (or Mortality Rate)
# - Area Type (Urban/Rural)
# - Population

# OPTIONAL: check structure
glimpse(merged_air_health_data)

# 1. Create a clean dataset (optional step if needed)
model_data <- merged_data %>%
  select(Hospital_Admissions, no2_concentration, pm10_concentration, pm25_concentration, population, type_of_stations) %>%
  filter(!is.na(Hospital_Admissions))  # Make sure no missing admissions

# 2. Build a linear regression model
# Predict Hospital Admissions based on Air Quality + Population + Urban/Rural
model <- lm(Hospital_Admissions ~ no2_concentration + pm10_concentration + pm25_concentration + population + type_of_stations, data = model_data)

# 3. View model summary
summary(model)

# 4. (Optional) Check diagnostic plots
par(mfrow = c(2, 2))  # Show 4 plots in 1 screen
plot(model)

# === From script8.R ===
ggplot(merged_air_asthma, aes(x = no2_concentration, y = Value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "NO₂ vs Asthma Admissions (Under 19)",
       x = "NO₂ (μg/m³)", y = "Admissions per 100,000")

lm(Value ~ no2_concentration + pm10_concentration + pm25_concentration, data = merged_air_asthma)


ggplot(merged_air_asthma, aes(x = pm25_concentration, y = Value)) +
  geom_point(color = "black", alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "PM₂.₅ vs Asthma Admissions (Under 19)",
    x = "PM₂.₅ (μg/m³)",
    y = "Admissions per 100,000"
  )

model_pm25 <- lm(Value ~ pm25_concentration, data = merged_air_asthma)
summary(model_pm25)


model_pm10 <- lm(Value ~ pm10_concentration, data = merged_air_asthma)
summary(model_pm10)

ggplot(merged_air_asthma, aes(x = pm10_concentration, y = Value)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(
    title = "PM₁₀ vs Asthma Admissions (Under 19)",
    x = "PM₁₀ (μg/m³)",
    y = "Admissions per 100,000"
  )


merged_air_asthma <- merged_air_asthma %>%
  mutate(area_type = ifelse(population > 200000, "Urban", "Rural"))

top_cases <- merged_air_asthma %>%
  arrange(desc(Value)) %>%
  select(clean_city, year, Value, pm25_concentration, no2_concentration)


ggplot(regional_pollution, aes(x = reorder(Region, -mean_pm25), y = mean_pm25)) +
  geom_col(fill = "skyblue") +
  coord_flip() +
  labs(title = "Average PM₂.₅ by Region", y = "μg/m³", x = "")

pollution_matrix <- merged_air_asthma %>%
  select(pm25_concentration, pm10_concentration, no2_concentration) %>%
  na.omit()

kmeans_result <- kmeans(scale(pollution_matrix), centers = 3)

ggplot(airquality_uk, aes(x = year, y = pm25_concentration, group = city)) +
  geom_line(alpha = 0.2) +
  stat_summary(fun = mean, geom = "line", color = "blue", size = 1.2) +
  labs(title = "Average PM₂.₅ Over Time", y = "μg/m³")


merged_air_asthma %>%
  group_by(area_type) %>%
  summarise(
    avg_admissions = mean(Value, na.rm = TRUE),
    avg_pm25 = mean(pm25_concentration, na.rm = TRUE),
    avg_pm10 = mean(pm10_concentration, na.rm = TRUE),
    avg_no2 = mean(no2_concentration, na.rm = TRUE),
    n = n()
  )

ggplot(merged_air_asthma, aes(x = area_type, y = Value)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Asthma Admissions by Area Type", x = "", y = "Admissions per 100,000")


ggplot(merged_air_asthma, aes(x = area_type, y = pm25_concentration)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "PM₂.₅ Concentration by Area Type", x = "", y = "PM₂.₅ (μg/m³)")


###statistcal

t.test(Value ~ area_type, data = merged_air_asthma)

t.test(no2_concentration ~ area_type, data = merged_air_asthma)


