options(max.print = .Machine$integer.max)

# Importing and Inspecting the data set

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

# Changing columns name of the data set

names(air_water_pollution_ds)[names(air_water_pollution_ds) == "X..Region."] <- "Regions"
names(air_water_pollution_ds)[names(air_water_pollution_ds) == "X..Country."] <- "Countries"
names(air_water_pollution_ds)[names(air_water_pollution_ds) == "X..AirQuality."] <- "AirQuality"
names(air_water_pollution_ds)[names(air_water_pollution_ds) == "X..WaterPollution."] <- "WaterPollution"

# Finding air quality and water pollution outliers
air_quality_outliers <- boxplot(air_water_pollution_ds$AirQuality, main="Air quality distribution", ylab="Air quality")$out
cat("Potential Air Quality Outliers: ", air_quality_outliers, "\n")

air_quality_outliers_row <- which(air_water_pollution_ds$AirQuality > 100)
cat("potential air quality outliers rows: ", air_quality_outliers_row, "\n")


water_pollution_outliers <- boxplot(air_water_pollution_ds$WaterPollution, main="Water Pollution Distribution", ylab="Water Pollution")$out
cat("Potentials Water pollution outliers are: ", water_pollution_outliers, "\n")

water_pollution_outliers_rows <- which(air_water_pollution_ds$WaterPollution > 100)
cat("Water pollution outliers rows: ", water_pollution_outliers_rows, "\n")

# Replacing empty value of region column with NA
for (i in 1:3963){
  if (nchar(air_water_pollution_ds$Regions[i]) == 3){
    air_water_pollution_ds$Regions[i] <- NA
  }
}

# Finding mode value of regions
frequent_table <- table(air_water_pollution_ds$Regions)
mode_value_regions <- names(frequent_table)[which.max(frequent_table)]
cat("Mode value for regions: ", mode_value_regions, "\n")
air_water_pollution_ds$Regions[is.na(air_water_pollution_ds$Regions)] <- mode_value_regions

# Finding and deleting invalid values of regions column
region_pattern <- "[.?/]"
invalid_value <- grepl(region_pattern, air_water_pollution_ds$Regions)
cat("Invalid value: ", "\n")
air_water_pollution_ds[invalid_value, ]

rows_region_to_delete <- c(504,505,752,766,1507,1845,1906,1950,2177,2529,2914,3078,3189,3449)
air_water_pollution_ds<- air_water_pollution_ds[-rows_region_to_delete, ]


# Finding mode value of air quality and water pollution column

frequent_table_air_quality <- table(air_water_pollution_ds$AirQuality)
mode_value_airQuality <- as.numeric(names(frequent_table_air_quality)[which.max(frequent_table_air_quality)])
cat("Mode value for air quality: ", mode_value_airQuality, "\n")

frequent_table_water_pollution <- table(air_water_pollution_ds$WaterPollution)
mode_value_water <- as.numeric(names(frequent_table_water_pollution)[which.max(frequent_table_water_pollution)])
cat("Mode value for water pollution: ", mode_value_water, "\n")


# Finding median value of air quality and water pollution column

air_quality_median <- median(air_water_pollution_ds$AirQuality, na.rm = TRUE)
cat("Median value for air quality: ", air_quality_median, "\n")

water_pollution_median <- median(air_water_pollution_ds$WaterPollution, na.rm = TRUE)
cat("Median value for water pollution: ", water_pollution_median, "\n")


# Replacing missing values of air and water pollution with median

air_water_pollution_ds$AirQuality[is.na(air_water_pollution_ds$AirQuality)] <- air_quality_median
air_water_pollution_ds$WaterPollution[is.na(air_water_pollution_ds$WaterPollution)] <- water_pollution_median

# Replacing Outliers value of air and water pollution with median

for (i in 1:length(air_water_pollution_ds$AirQuality)){
  if (air_water_pollution_ds$AirQuality[i] > 100){
    air_water_pollution_ds$AirQuality[i] = air_quality_median
  }
}

for (i in 1:length(air_water_pollution_ds$WaterPollution)){
  if(air_water_pollution_ds$WaterPollution[i] > 100){
    air_water_pollution_ds$WaterPollution[i] = water_pollution_median
  }
}

# Visualizing of data (histogram, barplot)
library(ggplot2)

# Histogram of air quality for different regions of different countries
ggplot(air_water_pollution_ds, aes(x = AirQuality)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "white", alpha = 0.7) +
  labs(x = "Air Quality", y="Frequency") +
  ggtitle("Histogram of air quality for different regions of different countries")

# Histogram of water pollution for different regions of different countries

ggplot(air_water_pollution_ds, aes(x = WaterPollution)) + 
  geom_histogram(binwidth = 5, fill = "blue", color="white", alpha = 0.7) +
  labs(x = "Water pollution", y = "Frequency") + 
  ggtitle("Histogram of water pollution for different regions of different countries")


# Finding Measure of spread of air quality and water pollution data set

air_quality_var <- var(air_water_pollution_ds$AirQuality)
air_quality_sd <- sd(air_water_pollution_ds$AirQuality)
air_quality_range <- range(air_water_pollution_ds$AirQuality)

cat("Air Quality variance: ", air_quality_var, "\n")
cat("Air quality Standard Deviation: ", air_quality_sd, "\n")
cat("Air Quality Range: ", air_quality_range, "\n")


water_pollution_var <- var(air_water_pollution_ds$WaterPollution)
water_pollution_sd <- sd(air_water_pollution_ds$WaterPollution)
water_pollution_range <- range(air_water_pollution_ds$WaterPollution)

cat("Water Pollution variance: ", water_pollution_var, "\n")
cat("Water Pollution Standard Deviation: ", water_pollution_sd, "\n")
cat("Water Pollution Range: ", water_pollution_range, "\n")

# Multivariate data analysis

numeric_data <- air_water_pollution_ds[, c("AirQuality", "WaterPollution")]
scaled_data <- scale(numeric_data)

pca_result <- prcomp(scaled_data, scale = TRUE)
summary(pca_result)
biplot(pca_result)

# Pairwise scatterplot matrix
pairs(air_water_pollution_ds[, c("AirQuality", "WaterPollution")])


# Correlation matrix
cor_matrix <- cor(air_water_pollution_ds[, c("AirQuality", "WaterPollution")])
print(cor_matrix)




# Scatter plot for air quality vs. water pollution
ggplot(air_water_pollution_ds, aes(x = AirQuality, y = WaterPollution)) +
  geom_point(color = "green", alpha = 0.5) +
  labs(x = "Air Quality", y = "Water Pollution") +
  ggtitle("Scatter Plot of Air Quality vs. Water Pollution")
