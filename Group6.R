library(ggplot2)  
library(dplyr)   
library(tidyr)    
library(plotly)   

# Read the dataset and specify that strings should not be treated as factors
data <- read.csv("GlobalLandTemperaturesByState.csv", stringsAsFactors = FALSE)

# Rename the columns for better readability
colnames(data) <- c("Date", "Avg_Temp", "Temp_Uncertainty", "State", "Country")

# Convert Date column to Date format
data$Date <- as.Date(data$Date)

# Convert Avg_Temp column to numeric
data$Avg_Temp <- as.numeric(data$Avg_Temp)

# Convert State and Country columns to factor datatype
data$State <- as.factor(data$State)
data$Country <- as.factor(data$Country)

# Remove rows with missing temperature values
data <- data %>% drop_na(Avg_Temp)


# Extract year and month from Date column
data$Year <- as.numeric(format(data$Date, "%Y"))
data$Month <- as.numeric(format(data$Date, "%m"))

# Cross-tabulation of temperature count by Country and Year
cross_tab <- table(data$Country, data$Year)

# Convert cross-tabulation to dataframe format for better viewing
cross_tab_df <- as.data.frame.matrix(cross_tab)

# Frequency table of temperature observations per Country
freq_table <- data %>%
  group_by(Country) %>%
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency))

# Display the results
print("Cross Tabulation of Temperature Count by Country and Year:")
print(cross_tab_df)

print("Frequency Table of Temperature Observations per Country:")
print(freq_table)

# Find the latest year in the dataset
latest_year <- max(as.numeric(format(data$Date, "%Y")))

# Filter data for the last 50 years
last_50_years_data <- data %>% filter(as.numeric(format(Date, "%Y")) >= (latest_year - 50))

# Extract year and month from Date column
last_50_years_data$Year <- as.numeric(format(last_50_years_data$Date, "%Y"))
last_50_years_data$Month <- format(last_50_years_data$Date, "%m")

# Calculate average temperature per country per year
country_temp_trend <- last_50_years_data %>%
  group_by(Country, Year) %>%
  summarise(Avg_Temp = mean(Avg_Temp, na.rm = TRUE), .groups = 'drop')

# Plot the climate change trend by country over the last 50 years
p <- ggplot(country_temp_trend, aes(x = Year, y = Avg_Temp, group = Country, color = Country)) +
  geom_line(size = 1) +  # Line plot for trends
  geom_smooth(method = "loess", se = FALSE, color = "black", size = 0.3) + # Smoothed trend line
  ggtitle("Climate Change Trends by Country (Last 50 Years)") +
  xlab("Year") +
  ylab("Average Temperature (°C)") +
  scale_color_viridis_d() +
  theme_minimal()
ggplotly(p, tooltip = c("x", "y", "color"))  # Convert plot to interactive

# Compute summary statistics for temperature data
summary_stats <- data %>%
  group_by(Country) %>%
  summarise(
    Mean_Temp = mean(Avg_Temp, na.rm = TRUE),
    Median_Temp = median(Avg_Temp, na.rm = TRUE),
    SD_Temp = sd(Avg_Temp, na.rm = TRUE),
    Min_Temp = min(Avg_Temp, na.rm = TRUE),
    Max_Temp = max(Avg_Temp, na.rm = TRUE),
    .groups = 'drop'
  )

# Reshape data for visualization
summary_melted <- summary_stats %>%
  pivot_longer(cols = -Country, names_to = "Statistic", values_to = "Temperature")

# Plot summary statistics for each country
p <- ggplot(summary_melted, aes(x = Country, y = Temperature, fill = Statistic)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Statistic, scales = "free_y") +
  ggtitle("Summary Statistics of Temperature by Country") +
  xlab("Country") +
  ylab("Temperature (°C)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set2")
ggplotly(p)  # Convert to interactive plot

# Histogram of global land temperatures
p <- ggplot(data, aes(x = Avg_Temp)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "skyblue", color = "black") +
  geom_density(alpha = .35, fill = "#FF6666") +
  ggtitle("Distribution of Global Land Temperatures") +
  xlab("Average Temperature (°C)") +
  ylab("Density") +
  theme_minimal()
ggplotly(p)

# Compute temperature variability by country
temp_variability <- country_temp_trend %>%
  group_by(Country) %>%
  summarise(Temp_Variability = max(Avg_Temp, na.rm = TRUE) - min(Avg_Temp, na.rm = TRUE), .groups = 'drop')

# Plot temperature variability by country
p <- ggplot(temp_variability, aes(x = reorder(Country, -Temp_Variability), y = Temp_Variability, fill = Temp_Variability)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ggtitle("Temperature Variability by Country (Last 50 Years)") +
  xlab("Country") +
  ylab("Temperature Variability (°C)") +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_minimal()
ggplotly(p)

# Analyze monthly temperature seasonality
seasonality_analysis <- last_50_years_data %>%
  group_by(Month) %>%
  summarise(Mean_Temp = mean(Avg_Temp, na.rm = TRUE), .groups = 'drop')

# Plot temperature seasonality over months
p <- ggplot(seasonality_analysis, aes(x = as.numeric(Month), y = Mean_Temp)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  ggtitle("Monthly Temperature Seasonality") +
  xlab("Month") +
  ylab("Mean Temperature (°C)") +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  theme_minimal()
ggplotly(p)

