install.packages("scatterplot3d")
install.packages("viridis")
install.packages("GGally")

# Here we load the required libraries
library(ggplot2)
library(maps)
# We loaded the necessary library for the 3D scatterplot
library(scatterplot3d)
library(viridis)



clim <- read.csv("https://userpage.fu-berlin.de/soga/data/raw-data/Climfrance.csv", sep = ";")


# Here the feature names are changed for better readability
colnames(clim) <- c("station_name", "altitude", "latitude", "longitude", "mean_temp", "max_temp", "min_temp", "relative_humidity", "mean_precip", "max_precip_24hours", "number_rainy_days", "number_sunshine_hours/yr")


# Here we viewed the dataframe that we read into the variable 'clim'
View(clim)


?str()

# We checked the descriptive nature of the data, displaying the structure of the dataframe
str(clim)


# > str(clim)
# 'data.frame':   36 obs. of  12 variables:
#  $ station_name            : chr  "Marseille" "Nice" "Cherbourg" "Bastia (Korsika)" ...
#  $ altitude                : chr  "3" "5" "8" "10" ...
#  $ latitude                : num  43.5 43.7 49.6 42.5 47.2 ...
#  $ longitude               : num  5.21 7.2 -1.63 9.48 -1.61 -1.71 -3.21 2.86 3.1 -0.7 ...
#  $ mean_temp               : num  14.2 14.8 11.4 14.9 11.7 11.3 12.3 12.5 9.7 12.3 ...
#  $ max_temp                : num  24.4 23.7 19.2 25 22.6 22.1 19.9 26.2 21.3 24.9 ...
#  $ min_temp                : num  2.3 7.1 4.3 7.5 1.6 0.6 4.5 5.4 -0.2 0.6 ...
#  $ relative_humidity       : int  68 72 80 70 82 82 89 66 82 81 ...
#  $ mean_precip             : chr  "546" "862" "931" "735" ...
#  $ max_precip_24hours      : int  86 147 62 201 62 45 81 186 49 54 ...
#  $ number_rainy_days       : int  76 86 171 91 172 168 147 85 171 162 ...
#  $ number_sunshine_hours/yr: int  2764 2775 1608 2603 1952 1805 2111 2644 1574 2052 ...



# we found that the altitude and mean precipitation were characters instead of numbers
# therefore we need to change it from characters to numbers to be able to work with our data



# Changing the altitude to integer/number.
# We use the gsub to remove the commas in the dataframe. Because it introduces NAs upon conversion

# So we remove commas and convert to numeric
clim$altitude <- as.numeric(gsub(",", "", clim$altitude))
clim$p_mean <- as.numeric(gsub(",", "", clim$p_mean))


# Here we cross check our dataframe to find that both altitude and mean precipitation
# are now converted to numbers and integers.
clim



# We used the summary() function to find the descriptive statistics of the dataframe we have.
summary(clim)

# > summary(clim)
#  station_name          altitude          latitude       longitude
#  Length:36          Min.   :   3.00   Min.   :42.55   Min.   :-4.4100
#  Class :character   1st Qu.:  46.25   1st Qu.:44.00   1st Qu.: 0.6075
#  Mode  :character   Median :  96.00   Median :45.80   Median : 3.1000
#                     Mean   : 249.94   Mean   :46.19   Mean   : 2.7628
#                     3rd Qu.: 214.00   3rd Qu.:48.16   3rd Qu.: 5.2275
#                     Max.   :2860.00   Max.   :50.73   Max.   : 9.4800
#    mean_temp        max_temp        min_temp     relative_humidity
#  Min.   :-1.20   Min.   : 8.60   Min.   :-11.8   Min.   :66.00
#  1st Qu.:10.47   1st Qu.:21.68   1st Qu.: -1.1   1st Qu.:73.00
#  Median :11.15   Median :22.95   Median :  0.6   Median :78.00
#  Mean   :11.10   Mean   :22.37   Mean   :  0.9   Mean   :77.11
#  3rd Qu.:12.35   3rd Qu.:24.43   3rd Qu.:  2.4   3rd Qu.:80.25
#  Max.   :16.40   Max.   :26.60   Max.   : 10.4   Max.   :89.00
#  mean_precip        max_precip_24hours number_rainy_days
#  Length:36          Min.   : 42.00     Min.   : 58.0
#  Class :character   1st Qu.: 55.75     1st Qu.:136.5
#  Mode  :character   Median : 77.00     Median :157.5
#                     Mean   : 90.03     Mean   :144.4
#                     3rd Qu.:112.50     3rd Qu.:168.5
#                     Max.   :201.00     Max.   :201.0
#  number_sunshine_hours/yr
#  Min.   :1574
#  1st Qu.:1840
#  Median :2050
#  Mean   :2078
#  3rd Qu.:2210
#  Max.   :2805

# We print our column names for clarity
colnames(clim)

# colnames(clim)
#  [1] "station_name"             "altitude"
#  [3] "latitude"                 "longitude"
#  [5] "mean_temp"                "max_temp"
#  [7] "min_temp"                 "relative_humidity"
#  [9] "mean_precip"              "max_precip_24hours"
# [11] "number_rainy_days"        "number_sunshine_hours/yr"


# We also find the number of rows and colums respectively
# We have 36 rows and 12 columns in the dataframe

dim(clim)

# > dim(clim)
#  36 12


# First we exclude the two high mountain extremes
climfrar <- clim[1:34, ]


# We write the linear model including the spatial attributes i.e latitude, longitude and altitude as explanatory variables
# for the mean annual temperature.
# we model our dependent variable as the mean temperature in the dataset
# we also model our explanatory variables as 'latitude', 'longitude', and 'altitude'
model <- lm(mean_temp ~ altitude + latitude + longitude, data = climfrar)
summary(model)


# >summary(model)

# Call:
# lm(formula = mean_temp ~ altitude + latitude + longitude, data = climfrar)

# Residuals:
#      Min       1Q   Median       3Q      Max
# -1.76492 -0.32755  0.04337  0.24787  2.58927

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept) 37.2650364  2.6220099  14.212 7.29e-15 ***
# altitude    -0.0064139  0.0008688  -7.383 3.17e-08 ***
# latitude    -0.5339603  0.0557546  -9.577 1.24e-10 ***
# longitude    0.0321010  0.0395728   0.811    0.424
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.7308 on 30 degrees of freedom
# Multiple R-squared:  0.8329,    Adjusted R-squared:  0.8162
# F-statistic: 49.84 on 3 and 30 DF,  p-value: 9.112e-12


# Interpretations


# We refit the model with only significant variables only, noting that from our previous model 'model',
# 'longitude' is not significant in the model with a p-value of 0.424 > 0.05
refined_model <- lm(mean_temp ~ altitude + latitude, data = climfrar)

summary(refined_model)

# > summary(refined_model)

# Call:
# lm(formula = mean_temp ~ altitude + latitude, data = climfrar)

# Residuals:
#      Min       1Q   Median       3Q      Max
# -1.79206 -0.27571 -0.00556  0.30536  2.71871

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept) 37.9147567  2.4828724   15.27 5.68e-16 ***
# altitude    -0.0062643  0.0008443   -7.42 2.34e-08 ***
# latitude    -0.5465325  0.0532610  -10.26 1.72e-11 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.7268 on 31 degrees of freedom
# Multiple R-squared:  0.8292,    Adjusted R-squared:  0.8182
# F-statistic: 75.26 on 2 and 31 DF,  p-value: 1.268e-12

# Mont-Ventoux and Pic-du-midi data (latitude and altitude) was used only,
# because our refined model now uses only the altitude and latitude as predictors
# since the longitude is not a significant predictor in terms of p-value > 0.05.



# Here we want to predict the exclude data points that we indexed out,
# climfrar <- clim[1:34, ]
# corresponding to 'Mont-Ventoux' and 'Pic-du-midi',
# We now have a model that uses the altitude and latitude for prediction.
# Therefore, we use this for prediction, by creating a dataframe for prediction.

# Mont-Ventoux: altitude = 1212, latitude = 44.16
# Pic-du-midi: altitude = 2860, latitude = 42.93


new_data <- data.frame(
    altitude = c(1212, 2860), # Altitudes for Mont-Ventoux and Pic-du-Midi
    latitude = c(44.16, 42.93) # Latitudes
)

# Predict temperature with confidence intervals
predictions <- predict(refined_model, newdata = new_data, interval = "confidence")

# Print predictions and compare with measured means
print(predictions)

# > print(predictions)
#         fit       lwr      upr
# 1  6.187574  4.333741 8.041407
# 2 -3.463727 -8.134782 1.207329
# >

# from the dataframe before exclusion, we find our average means
# for Mont-Ventoux and Pic-du-Midi to be '3.6' and '-1.2'
#
clim
refined_model <- lm(mean_temp ~ altitude + latitude, data = climfrar)


png("3d_scatterplot.png", width = 800, height = 600, dpi = 800)
# Create 3D scatterplot
s3d <- scatterplot3d(
    x = climfrar$altitude,
    y = climfrar$latitude,
    z = climfrar$mean_temp,
    xlab = "Altitude (m)",
    ylab = "Latitude (°N)",
    zlab = "Mean Temperature (°C)",
    main = "3D Scatterplot: Mean Temperature vs Altitude & Latitude",
    pch = 16, # Filled circles
    cex.symbols = 1.5, # Larger points for better visibility
    color = viridis(34), # Gradient color scale
    grid = FALSE, # Remove grid lines
    box = TRUE, # Keep box around the plot
    angle = 32 # Adjust angle for better perspective
)

# Add regression plane
s3d$plane3d(
    refined_model,
    draw_polygon = TRUE, # Ensure the plane is drawn as a polygon
    polygon_args = list(col = adjustcolor("#85C1E9", alpha.f = 0.3)) # Semi-transparent blue (adjust alpha for better visibility)
)

# Add text labels for selected points (optional)
highlight_indices <- c(1, 10, 15, 20, 30) # Indices of points to label
text(
    s3d$xyz.convert(
        climfrar$altitude[highlight_indices],
        climfrar$latitude[highlight_indices],
        climfrar$mean_temp[highlight_indices]
    ),
    labels = climfrar$station_name[highlight_indices],
    pos = 3, cex = 0.8, col = "black"
)


dev.off()

# Model summary
summary(refined_model)

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept) 37.9147567  2.4828724   15.27 5.68e-16 ***
# altitude    -0.0062643  0.0008443   -7.42 2.34e-08 ***
# latitude    -0.5465325  0.0532610  -10.26 1.72e-11 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 0.7268 on 31 degrees of freedom
# Multiple R-squared:  0.8292,    Adjusted R-squared:  0.8182
# F-statistic: 75.26 on 2 and 31 DF,  p-value: 1.268e-12




# We get the map of France using the map_data() function and save it to a variable
# 'france_map'.

# Get the map data for France
france_map <- map_data("france")

# Plot the map of France with climate stations
ggplot() +
    # Plot map of France
    geom_map(
        data = france_map, map = france_map,
        aes(x = long, y = lat, map_id = region),
        fill = "lightgray", color = "white", size = 0.1
    ) +

    # Plot climate stations with mean temperature reflected in color
    geom_point(
        data = climfrar, aes(x = longitude, y = latitude, color = mean_temp),
        size = 3, alpha = 0.7, shape = 19
    ) +

    # Customize color scale for mean temperature
    scale_color_viridis_c(option = "plasma", name = "Mean Temperature (°C)") +

    # Customize title and labels
    labs(
        title = "Climate Stations in France",
        subtitle = "Mean Temperature Intensity at 34 Climate Stations",
        x = "Longitude",
        y = "Latitude"
    ) +

    # Modernize the theme for a clean, minimalistic look
    theme_minimal(base_size = 15) +
    theme(
        plot.title = element_text(size = 18, face = "bold", color = "#333333", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#555555", hjust = 0.5),
        axis.title.x = element_text(size = 14, color = "#444444"),
        axis.title.y = element_text(size = 14, color = "#444444"),
        axis.text = element_text(size = 12, color = "#666666"),

        # Adjust the size of the legend
        legend.key.size = unit(0.5, "cm"), # Smaller legend key
        legend.title = element_text(size = 10), # Smaller legend title
        legend.text = element_text(size = 8), # Smaller legend text

        # Removed the grid lines for a cleaner look
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )


ggsave("map_by_region.png", width = 10, height = 6, dpi = 300)


# Other plots we explored for better visualization.

# Create the plot
ggplot(climfrar, aes(x = relative_humidity, y = altitude, color = mean_temp)) +
    geom_point(size = 3, alpha = 0.7) +
    facet_wrap(~station_name) + # Faceting by station name
    scale_color_viridis_c() + # Color scale for temperature
    labs(
        title = "Mean Temperature by Region",
        x = "Relative Humidity",
        y = "Altitude",
        color = "Mean Temperature (°C)"
    ) +
    # Customizing the theme separately
    theme_minimal() +
    theme(
        plot.title = element_text(size = 18, face = "bold", color = "#333333", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#555555", hjust = 0.5),
        axis.title.x = element_text(size = 14, color = "#444444"),
        axis.title.y = element_text(size = 14, color = "#444444"),
        axis.text = element_text(size = 12, color = "#666666"),
        strip.text = element_text(size = 11, color = "#333333")
    )

# Save the plot as a high-resolution PNG image
ggsave("mean_temp_by_region.png", width = 12, height = 8, dpi = 300)





ggplot(climfrar, aes(x = altitude, y = mean_temp)) +
    geom_point(color = "#ae282c", size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    labs(title = "Mean Temperature plotted against Altitude", x = "Altitude (m)", y = "Mean Temperature (°C)") +
    theme_minimal() +
    theme(
        plot.title = element_text(size = 18, face = "bold", color = "#333333", hjust = 0.5),
        plot.subtitle = element_text(size = 14, color = "#555555", hjust = 0.5),
        axis.title.x = element_text(size = 14, color = "#444444"),
        axis.title.y = element_text(size = 14, color = "#444444"),
        axis.text = element_text(size = 12, color = "#666666"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
    )

# Save the plot as a high-resolution PNG image
ggsave("mean_temp_vs_altitude.png", width = 14, height = 8, dpi = 300)




























colnames(climfrar)

# Standardize the predictor variables
climfrar$altitude_std <- scale(climfrar$altitude)
climfrar$latitude_std <- scale(climfrar$latitude)

# Fit the regression model with standardized variables
standardized_model <- lm(mean_temp ~ altitude_std + latitude_std, data = climfrar)

# View summary of the standardized model
summary(standardized_model)
