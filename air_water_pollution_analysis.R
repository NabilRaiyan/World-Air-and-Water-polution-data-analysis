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



