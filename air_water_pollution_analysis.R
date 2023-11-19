options(max.print = .Machine$integer.max)


air_water_pollution_ds <- read.csv("E:/Courses/Data Science Projects/World's Air Quality and Water Pollution Dataset/cities_air_quality_water_pollution.csv")
air_water_pollution_ds

head(air_water_pollution_ds)
tail(air_water_pollution_ds)

str(air_water_pollution_ds)
summary(air_water_pollution_ds)
dim(air_water_pollution_ds)

is.na(air_water_pollution_ds)

names(air_water_pollution_ds)

colSums(is.na(air_water_pollution_ds))
names(air_water_pollution_ds)[names(air_water_pollution_ds) == "X..Region."] <- "Regions"
names(air_water_pollution_ds)[names(air_water_pollution_ds) == "X..Country."] <- "Countries"
names(air_water_pollution_ds)[names(air_water_pollution_ds) == "X..AirQuality."] <- "AirQuality"
names(air_water_pollution_ds)[names(air_water_pollution_ds) == "X..WaterPollution."] <- "WaterPollution"

air_quality_outliers <- boxplot(air_water_pollution_ds$AirQuality, main="Air quality distribution", ylab="Air quality")$out
cat("Potential Air Quality Outliers: ", air_quality_outliers, "\n")

air_quality_outliers_row <- which(air_water_pollution_ds$AirQuality > 100)
cat("potential air quality outliers rows: ", air_quality_outliers_row, "\n")


water_pollution_outliers <- boxplot(air_water_pollution_ds$WaterPollution, main="Water Pollution Distribution", ylab="Water Pollution")$out
cat("Potentials Water pollution outliers are: ", water_pollution_outliers, "\n")

water_pollution_outliers_rows <- which(air_water_pollution_ds$WaterPollution > 100)
cat("Water pollution outliers rows: ", water_pollution_outliers_rows, "\n")

for (i in 1:3963){
  if (nchar(air_water_pollution_ds$Regions[i]) == 3){
    air_water_pollution_ds$Regions[i] <- NA
  }
}
frequent_table <- table(air_water_pollution_ds$Regions)
mode_value_regions <- names(frequent_table)[which.max(frequent_table)]
mode_value_regions


frequent_table_air_quality <- table(air_water_pollution_ds$AirQuality)
mode_value_airQuality <- as.numeric(names(frequent_table_air_quality)[which.max(frequent_table_air_quality)])
mode_value_airQuality


frequent_table_water_pollution <- table(air_water_pollution_ds$WaterPollution)
mode_value_water <- as.numeric(names(frequent_table_water_pollution)[which.max(frequent_table_water_pollution)])
mode_value_water

air_water_pollution_ds



