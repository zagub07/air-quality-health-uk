

# === From script1.R ===
# arrange the table to display top countries
top_cities2020no2 <- airquality_uk %>%
  filter(year == 2020, !is.na(no2_concentration)) %>%
  select(city, year, no2_concentration) %>%
  arrange(desc(no2_concentration)) %>%
  slice_head(n = 10)

## this analyses countries above the WHO limit

airquality_ukWHOlimit_NO2 <- airquality_uk %>%
  filter(year == 2020) %>% 
  select(city, year, no2_concentration) %>% 
  mutate(above_WHO_limit = ifelse(no2_concentration > 10, "Above Safe Limit > 10", "Safe"))


##trend
trend_data <- airquality_uk %>%
  select(year, pm10_concentration, pm25_concentration, no2_concentration) %>%
  group_by(year) %>%
  summarise(
    pm10_avg = mean(pm10_concentration, na.rm = TRUE),
    pm25_avg = mean(pm25_concentration, na.rm = TRUE),
    no2_avg = mean(no2_concentration, na.rm = TRUE))

trend_long <- tidyr::pivot_longer(
  trend_data,
  cols = c(pm10_avg, pm25_avg, no2_avg),
  names_to = "pollutant",
  values_to = "concentration")

ggplot(trend_long, aes(x = year, y = concentration, color = pollutant)) +
  geom_line(size = 1.2) +
  labs(
    title = "Global Average Pollutant Concentration Over Time",
    x = "Year",
    y = "Average Concentration (µg/m³)",
    color = "Pollutant"
  ) +
  theme_minimal()





# === From script2.R ===
limits <- list(
  no2 = list(who = 10, uk = 40),
  pm10 = list(who = 15, uk = 40),
  pm25 = list(who = 5,  uk = 20)
)

##library(dplyr)

# Define WHO and UK limits
limits <- list(
  no2 = list(who = 10, uk = 40),
  pm10 = list(who = 15, uk = 40),
  pm25 = list(who = 5,  uk = 20)
)

# Function to calculate compliance for one pollutant
check_compliance <- function(data, pollutant, who_limit, uk_limit) {
  data %>%
    filter(!is.na(.data[[pollutant]])) %>%
    mutate(
      WHO_status = ifelse(.data[[pollutant]] > who_limit, "Above WHO", "Compliant with WHO"),
      UK_status  = ifelse(.data[[pollutant]] > uk_limit, "Above UK", "Compliant with UK")
    ) %>%
    summarise(
      n = n(),
      WHO_non_compliant = sum(WHO_status == "Above WHO"),
      WHO_compliant = sum(WHO_status == "Compliant with WHO"),
      UK_non_compliant = sum(UK_status == "Above UK"),
      UK_compliant = sum(UK_status == "Compliant with UK"),
      WHO_non_compliant_pct = round(WHO_non_compliant / n * 100, 1),
      UK_non_compliant_pct = round(UK_non_compliant / n * 100, 1)
    ) %>%
    mutate(Pollutant = pollutant) %>%
    select(Pollutant, everything())  # Reorder to put 'Pollutant' first
}

# Run for each pollutant
no2_summary <- check_compliance(airquality_uk_latest, "no2_concentration", limits$no2$who, limits$no2$uk)
pm10_summary <- check_compliance(airquality_uk_latest, "pm10_concentration", limits$pm10$who, limits$pm10$uk)
pm25_summary <- check_compliance(airquality_uk_latest, "pm25_concentration", limits$pm25$who, limits$pm25$uk)

# Combine results
compliance_summary <- bind_rows(no2_summary, pm10_summary, pm25_summary)

# View result
print(compliance_summary)

##run the latest data
airquality_uk_latest <- airquality_uk %>%
  filter(country_name == "United Kingdom of Great Britain and Northern Ireland") %>%
  filter(!is.na(year)) %>%
  filter(year == max(year, na.rm = TRUE))

###run for each pollutant

no2_summary <- check_compliance(airquality_uk_latest, "no2_concentration", limits$no2$who, limits$no2$uk)
pm10_summary <- check_compliance(airquality_uk_latest, "pm10_concentration", limits$pm10$who, limits$pm10$uk)
pm25_summary <- check_compliance(airquality_uk_latest, "pm25_concentration", limits$pm25$who, limits$pm25$uk)




compliance_summary <- bind_rows(no2_summary, pm10_summary, pm25_summary)





plot_pollutant_map <- function(data, pollutant, title) {
  ggplot(data = data, aes(x = longitude, y = latitude)) +
    geom_point(aes_string(color = pollutant), size = 3, alpha = 0.8) +
    scale_color_viridis_c(option = "plasma", na.value = "grey90") +
    labs(
      title = paste(title, "Concentration in UK Cities"),
      x = "Longitude",
      y = "Latitude",
      color = paste0(toupper(pollutant), " (µg/m³)")
    ) +
    theme_minimal()
}


no2_map <- airquality_uk_latest %>%
  filter(!is.na(no2_concentration), !is.na(latitude), !is.na(longitude))

pm10_map <- airquality_uk_latest %>%
  filter(!is.na(pm10_concentration), !is.na(latitude), !is.na(longitude))

pm25_map <- airquality_uk_latest %>%
  filter(!is.na(pm25_concentration), !is.na(latitude), !is.na(longitude))

# Create plots
plot_pollutant_map(no2_map, "no2_concentration", "NO₂")
plot_pollutant_map(pm10_map, "pm10_concentration", "PM₁₀")
plot_pollutant_map(pm25_map, "pm25_concentration", "PM₂.₅")


library(scales)  # for comma formatting

plot_population_correlation <- function(data, pollutant, label) {
  data_clean <- data %>%
    filter(!is.na(.data[[pollutant]]))
  
  ggplot(data_clean, aes(x = population, y = .data[[pollutant]])) +
    geom_point(alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
    stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top", size = 5) +
    scale_x_continuous(labels = comma) +  # 👈 Format population axis
    labs(
      title = paste(label, "vs Population"),
      x = "Population",
      y = paste0(label, " Concentration (µg/m³)")
    ) +
    theme_minimal()
}
# Create plots
plot_population_correlation(airquality_uk_latest, "no2_concentration", "NO₂")
plot_population_correlation(airquality_uk_latest, "pm10_concentration", "PM₁₀")
plot_population_correlation(airquality_uk_latest, "pm25_concentration", "PM₂.₅")

 

# === From script3.R ===
##creating and comparing the regions from another dataset 
city_to_region <- c(
  "Aberdeen" = "NA",
  "Alfriston" = "South East",
  "Armagh" = "NA",
  "Ballymena" = "NA",
  "Barnsley" = "Yorkshire and The Humber",
  "Barnstaple" = "South West",
  "Bath" = "South West",
  "Bath and North East Somerset" = "South West",
  "Bedford" = "East of England",
  "Belfast" = "NA",
  "Biandrate" = "NA",
  "Billingham" = "North East",
  "Birmingham" = "West Midlands",
  "Blackburn With Darwen" = "North West",
  "Blackpool" = "North West",
  "Bolton" = "North West",
  "Bournemouth" = "South West",
  "Bradford" = "Yorkshire and The Humber",
  "Brighton" = "South East",
  "Bristol" = "South West",
  "Burnley" = "North West",
  "Bury" = "North West",
  "Caerphilly" = "Wales",
  "Cambridge" = "East of England",
  "Camden" = "London",
  "Cannock Chase" = "West Midlands",
  "Canterbury" = "South East",
  "Cardiff" = "Wales",
  "Carlisle" = "North West",
  "Central Bedfordshire" = "East of England",
  "Cheshire East" = "North West",
  "Chesterfield" = "East Midlands",
  "Cornwall" = "South West",
  "Coventry" = "West Midlands",
  "Derby" = "East Midlands",
  "Derry And Strabane" = "NA",
  "Doncaster" = "Yorkshire and The Humber",
  "Dumfries" = "NA",
  "Dundee" = "NA",
  "Ealing" = "London",
  "East Devon" = "South West",
  "East Staffordshire" = "West Midlands",
  "Eastbourne" = "South East",
  "Edinburgh" = "NA",
  "Exeter" = "South West",
  "Glasgow" = "NA",
  "Halton" = "North West",
  "Haringey" = "London",
  "Hartlepool" = "North East",
  "Hertsmere" = "East of England",
  "Highland" = "NA",
  "Hillingdon" = "London",
  "Horsham" = "South East",
  "Inverclyde" = "NA",
  "Kensington And Chelsea" = "London",
  "Kingston Upon Hull" = "Yorkshire and The Humber",
  "Kirklees" = "Yorkshire and The Humber",
  "Leeds" = "Yorkshire and The Humber",
  "Leicester" = "East Midlands",
  "Lewisham" = "London",
  "Lincoln" = "East Midlands",
  "Liverpool" = "North West",
  "London" = "London",
  "Luton" = "East of England",
  "Manchester" = "North West",
  "Medway" = "South East",
  "Mid And East Antrim" = "NA",
  "Monmouthshire" = "Wales",
  "Newcastle Upon Tyne" = "North East",
  "Newport" = "Wales",
  "Norwich" = "East of England",
  "Nottingham" = "East Midlands",
  "Oldham" = "North West",
  "Oxford" = "South East",
  "Plymouth" = "South West",
  "Portsmouth" = "South East",
  "Preston" = "North West",
  "Reading" = "South East",
  "Richmond Upon Thames" = "London",
  "Salford" = "North West",
  "Sandwell" = "West Midlands",
  "Scottish Borders" = "NA",
  "Sheffield" = "Yorkshire and The Humber",
  "Southampton" = "South East",
  "Southend On Sea" = "East of England",
  "Southwark" = "London",
  "St Helens" = "North West",
  "Stockton On Tees" = "North East",
  "Stoke On Trent" = "West Midlands",
  "Sunderland" = "North East",
  "Swansea" = "Wales",
  "Swindon" = "South West",
  "Telford And Wrekin" = "West Midlands",
  "Thurrock" = "East of England",
  "Tower Hamlets" = "London",
  "Walsall" = "West Midlands",
  "Warwick" = "West Midlands",
  "West Dunbartonshire" = "NA",
  "West Northamptonshire" = "East Midlands",
  "Westminster" = "London",
  "Wigan" = "North West",
  "Wirral" = "North West",
  "Worthing" = "South East",
  "Wrexham" = "Wales",
  "York" = "Yorkshire and The Humber"
)
uk_air <- airquality_uk %>%
  filter(country_name == "United Kingdom of Great Britain and Northern Ireland") %>%
  mutate(clean_city = gsub("/GBR", "", city)) 

uk_air <- uk_air %>%
  mutate(Region = city_to_region[clean_city]) %>%
  filter(!is.na(Region))  # only keep cities with mapped regions

##check for deleted entries
uk_air <- airquality_uk %>%
  filter(country_name == "United Kingdom of Great Britain and Northern Ireland") %>%
  mutate(clean_city = gsub("/GBR", "", city))

uk_air <- uk_air %>%
  mutate(Region = city_to_region[clean_city])

unmapped_cities <- uk_air %>%
  filter(Region == 'NA')
  


