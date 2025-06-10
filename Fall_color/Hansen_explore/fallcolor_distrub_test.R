
library(googlesheets4)
library(ggplot2)
library(terra)
library(MODISTools)
library(dplyr)
library(sf)
library(jsonlite)
library(readr)
library(tidyr)
#----reading in data
floss.dat <- read_sheet("https://docs.google.com/spreadsheets/d/1glBSZbN2uHsR0PzRiEAV0W1CDGh8kGb_-gQmxT2sVpw/edit?gid=1258618079#gid=1258618079")
head(floss.dat)


#Making the year correct
floss.dat$Year <- as.numeric(floss.dat$Year)
floss.dat$Year <- ifelse(floss.dat$Year < 10, 
                         paste0("200", floss.dat$Year),  # Add '200' for single-digit years
                         paste0("20", floss.dat$Year))   # Add '20' for two-digit years
head(floss.dat)
#View(floss.dat)
#----basic stats on size
# Basic summary statistics
summary(floss.dat)

# Mean, standard deviation, variance
mean(floss.dat$area_sqm)
sd(floss.dat$area_sqm)
var(floss.dat$area_sqm)
range(floss.dat$area_sqm)
#----Getting the  10 polygons with the highest square meters
floss10.dat <- floss.dat[order(floss.dat$area_sqm, decreasing = TRUE), ][1:10, ]
head(floss10.dat)

ggplot(floss.dat, aes(x = area_sqm)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Distribution of Area (sq m)",
       x = "Area (sq m)",
       y = "Frequency") +
  theme_minimal()


# Convert Year to a factor for better boxplot grouping
floss.dat$Year <- as.factor(floss.dat$Year)
floss.dat$area_sqm <- as.numeric(floss.dat$area_sqm)

# Create the box plot
ggplot(floss.dat, aes(x = Year, y = area_sqm)) +
  geom_boxplot() +
  labs(title = "Box Plot of Area by Year", x = "Year", y = "Area (sqm)") +
  theme_minimal() + # Apply a clean theme
theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#--- read CSV data in 
flossdvi<-read.csv("~/Google Drive/My Drive/Reidy_research/Hansen exploration/modis_ndvi_mat_sen_gd50_data.csv")
head(flossdvi)
# Rename the "GD202_8" value to "GD2002_8"
flossdvi$Label[flossdvi$Label == "GD202_8"] <- "GD2002_8"

# Check the updated data
head(flossdvi)

unique(flossdvi$Label)

ggplot(flossdvi, aes(x=Year, y= NDVI, color=as.factor(Label))) +
  geom_line() +
  labs(title = "Mean NDVI by year", x = "Year", y = "NDVI") +
  theme_minimal() + # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(flossdvi, aes(x = as.factor(Year), y = NDVI)) + # Year as factor for boxplots
  geom_boxplot() +
  labs(title = "NDVI Distribution Across Years", x = "Year", y = "NDVI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(flossdvi, aes(x=Year, y= NDVI, color=as.factor(Label))) +
  geom_smooth() +
  labs(title = "Mean NDVI by year", x = "Year", y = "NDVI") +
  theme_minimal() + # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggplot(flossdvi, aes(x=Year, y= NDVI)) +
  geom_line() +
  facet_grid(Label ~ .) +
  labs(title = "Mean NDVI by year", x = "Year", y = "NDVI") +
  theme_minimal() + # Apply a clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

Fln17 <-flossdvi[flossdvi$Label == "GD2017_11", c("Label", "NDVI","Year")]
head(Fln17)

ggplot(Fln17, aes(x=Year, y=NDVI)) +
  geom_line() +
  geom_vline(xintercept = 2017, color = "red", linetype = "dashed") +  # Add vertical line
  labs(title = "NDVI series for 2017 disturbance year", x = "Year", y = "NDVI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


Fln15 <-flossdvi[flossdvi$Label == "GD2015_4", c("Label", "NDVI","Year")]
head(Fln15)

ggplot(Fln15, aes(x=Year, y=NDVI)) +
  geom_line() +
  geom_vline(xintercept = 2015, color = "red", linetype = "dashed") +  # Add vertical line
  labs(title = "NDVI series for 2015 disturbance year", x = "Year", y = "NDVI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Fln02 <-flossdvi[flossdvi$Label == "GD2002_8", c("Label", "NDVI","Year")]
head(Fln02)

ggplot(Fln02, aes(x=Year, y=NDVI)) +
  geom_line() +
  geom_vline(xintercept = 2002, color = "red", linetype = "dashed") +  # Add vertical line
  labs(title = "NDVI series for 2002 disturbance year", x = "Year", y = "NDVI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Calculate mean NDVI by year for all labels, excluding specific years
years_to_exclude <- c(2002, 2015, 2016, 2017, 2018, 2019, 2020, 2022)

mean_ndvi_by_year <- flossdvi %>%
  filter(!Year %in% years_to_exclude) %>%
  group_by(Year) %>%
  summarize(Mean_NDVI = mean(NDVI, na.rm = TRUE))
mean_ndvi_by_year <- flossdvi %>%
  group_by(Year) %>%
  summarize(Mean_NDVI = mean(NDVI, na.rm = TRUE))
mean_ndvi_by_year$Year <- as.numeric(mean_ndvi_by_year$Year)
flossdvi$Year <- as.numeric(flossdvi$Year)


 ggplot(Fln17, aes(x = Year, y = NDVI)) +
  geom_line() +
  geom_vline(xintercept = 2017, color = "red", linetype = "dashed") +
  geom_line(data = mean_ndvi_by_year, aes(x = Year, y = Mean_NDVI), color = "blue", size = 1.2) +  # Changed to geom_line
  labs(title = "NDVI series for 2017 disturbance year", x = "Year", y = "NDVI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



 ggplot(Fln17, aes(x = Year, y = NDVI)) +
  geom_line() +
  geom_vline(xintercept = 2017, color = "red", linetype = "dashed") +
  geom_smooth(data = mean_ndvi_by_year, aes(x = Year, y = Mean_NDVI), color = "blue", size = 1.2, method = "lm") + # Use linear model
  labs(title = "NDVI series for 2017 disturbance year", x = "Year", y = "NDVI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





 ggplot(Fln15, aes(x = Year, y = NDVI)) +
   geom_line() +
   geom_vline(xintercept = 2015, color = "red", linetype = "dashed") +
   geom_line(data = mean_ndvi_by_year, aes(x = Year, y = Mean_NDVI), color = "blue", size = 1.2) +  # Changed to geom_line
   labs(title = "NDVI series for 2015 disturbance year", x = "Year", y = "NDVI") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 
 
 ggplot(Fln15, aes(x = Year, y = NDVI)) +
   geom_line() +
   geom_vline(xintercept = 2015, color = "red", linetype = "dashed") +
   geom_smooth(data = mean_ndvi_by_year, aes(x = Year, y = Mean_NDVI), color = "blue", size = 1.2, method = "lm") + # Use linear model
   labs(title = "NDVI series for 2015 disturbance year", x = "Year", y = "NDVI") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
ggplot(Fln02, aes(x = Year, y = NDVI)) +
  geom_line() +
  geom_vline(xintercept = 2002, color = "red", linetype = "dashed") +
  geom_line(data = mean_ndvi_by_year, aes(x = Year, y = Mean_NDVI), color = "blue", size = 1.2) +  # Changed to geom_line
  labs(title = "NDVI series for 2002 disturbance year", x = "Year", y = "NDVI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



 ggplot(Fln02, aes(x = Year, y = NDVI)) +
  geom_line() +
  geom_vline(xintercept = 2002, color = "red", linetype = "dashed") +
  geom_smooth(data = mean_ndvi_by_year, aes(x = Year, y = Mean_NDVI), color = "blue", size = 1.2, method = "lm") + # Use linear model
  labs(title = "NDVI series for 2002 disturbance year", x = "Year", y = "NDVI") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

 

 
# Graphing senescence, maturity, and growing season
 #Convert date columns to actual Date format
 # Read CSV data in 

 
 # Convert date columns to actual Date format
 flossdvi$Maturity_Date <- as.Date(flossdvi$Maturity_Date)
 flossdvi$Senescence_Date <- as.Date(flossdvi$Senescence_Date)
 flossdvi$MidGreendown_Date <- as.Date(flossdvi$MidGreendown_Date)
 # Get unique plot labels
 unique_labels <- unique(flossdvi$Label)
 n_plots <- length(unique_labels)
 
 # Set up colors
 maturity_color <- "green4"
 midgreendown_color <- "goldenrod2"
 senescence_color <- "red3"
 plot_colors <- rainbow(n_plots)
 
 # Option 1: Plot Day of Year by Plot (separate panels)
 # Calculate panel layout
 n_cols <- ceiling(sqrt(n_plots))
 n_rows <- ceiling(n_plots / n_cols)
 
 par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))
 
 for (i in 1:n_plots) {
   plot_data <- flossdvi[flossdvi$Label == unique_labels[i], ]
   
   # Create the plot
   plot(plot_data$Year, plot_data$Maturity_DOY, 
        type = "n",
        ylim = range(c(plot_data$Maturity_DOY, plot_data$MidGreendown_DOY, plot_data$Senescence_DOY), na.rm = TRUE),
        xlim = range(plot_data$Year, na.rm = TRUE),
        xlab = "Year", 
        ylab = "Day of Year",
        main = paste("Phenology -", unique_labels[i]))
   
   # Add lines and points for Maturity
   lines(plot_data$Year, plot_data$Maturity_DOY, col = maturity_color, lwd = 2)
   points(plot_data$Year, plot_data$Maturity_DOY, col = maturity_color, pch = 16, cex = 1.2)
   
   # Add lines and points for MidGreendown
   lines(plot_data$Year, plot_data$MidGreendown_DOY, col = midgreendown_color, lwd = 2)
   points(plot_data$Year, plot_data$MidGreendown_DOY, col = midgreendown_color, pch = 16, cex = 1.2)
   
   # Add lines and points for Senescence
   lines(plot_data$Year, plot_data$Senescence_DOY, col = senescence_color, lwd = 2)
   points(plot_data$Year, plot_data$Senescence_DOY, col = senescence_color, pch = 16, cex = 1.2)
   
   # Add trend lines
   if (sum(!is.na(plot_data$Maturity_DOY)) > 2) {
     maturity_trend <- lm(Maturity_DOY ~ Year, data = plot_data, na.action = na.exclude)
     abline(maturity_trend, col = maturity_color, lty = 2, lwd = 1)
   }
   
   if (sum(!is.na(plot_data$MidGreendown_DOY)) > 2) {
     midgreendown_trend <- lm(MidGreendown_DOY ~ Year, data = plot_data, na.action = na.exclude)
     abline(midgreendown_trend, col = midgreendown_color, lty = 2, lwd = 1)
   }
   
   if (sum(!is.na(plot_data$Senescence_DOY)) > 2) {
     senescence_trend <- lm(Senescence_DOY ~ Year, data = plot_data, na.action = na.exclude)
     abline(senescence_trend, col = senescence_color, lty = 2, lwd = 1)
   }
   
   # Add legend only to first plot
   if (i == 1) {
     legend("topright", 
            legend = c("Maturity", "MidGreendown", "Senescence"), 
            col = c(maturity_color, midgreendown_color, senescence_color),
            lwd = 2, pch = 16, cex = 0.8)
   }
 }
 
 # Reset plotting parameters
 par(mfrow = c(1, 1))
 
 # Option 2: All plots on one graph
 plot(range(flossdvi$Year, na.rm = TRUE), 
      range(c(flossdvi$Maturity_DOY, flossdvi$MidGreendown_DOY, flossdvi$Senescence_DOY), na.rm = TRUE),
      type = "n",
      xlab = "Year", 
      ylab = "Day of Year",
      main = "Vegetation Phenology - All Plots")
 
 # Plot each site
 for (i in 1:n_plots) {
   plot_data <- flossdvi[flossdvi$Label == unique_labels[i], ]
   
   # Maturity - solid lines
   lines(plot_data$Year, plot_data$Maturity_DOY, 
         col = plot_colors[i], lwd = 2, lty = 1)
   points(plot_data$Year, plot_data$Maturity_DOY, 
          col = plot_colors[i], pch = 16, cex = 1)
   
   # MidGreendown - dotted lines  
   lines(plot_data$Year, plot_data$MidGreendown_DOY, 
         col = plot_colors[i], lwd = 2, lty = 3)
   points(plot_data$Year, plot_data$MidGreendown_DOY, 
          col = plot_colors[i], pch = 15, cex = 1)
   
   # Senescence - dashed lines  
   lines(plot_data$Year, plot_data$Senescence_DOY, 
         col = plot_colors[i], lwd = 2, lty = 2)
   points(plot_data$Year, plot_data$Senescence_DOY, 
          col = plot_colors[i], pch = 17, cex = 1)
 }
 
 # Add legend
 legend("topright", 
        legend = c(paste(unique_labels, "- Maturity"), 
                   paste(unique_labels, "- MidGreendown"),
                   paste(unique_labels, "- Senescence")),
        col = rep(plot_colors, 3),
        lty = rep(c(1, 3, 2), each = n_plots),
        lwd = 2,
        cex = 0.6,
        ncol = 1)
 
 # Option 3: Calculate and plot growing season length and decline phases
 flossdvi$Growing_Season_Length <- flossdvi$Senescence_DOY - flossdvi$Maturity_DOY
 flossdvi$Early_Decline_Length <- flossdvi$MidGreendown_DOY - flossdvi$Maturity_DOY
 flossdvi$Late_Decline_Length <- flossdvi$Senescence_DOY - flossdvi$MidGreendown_DOY
 
 # Handle cases where senescence might be in next year
 flossdvi$Growing_Season_Length[flossdvi$Growing_Season_Length < 0] <- 
   flossdvi$Growing_Season_Length[flossdvi$Growing_Season_Length < 0] + 365
 
 # Handle negative values for decline phases
 flossdvi$Early_Decline_Length[flossdvi$Early_Decline_Length < 0] <- 
   flossdvi$Early_Decline_Length[flossdvi$Early_Decline_Length < 0] + 365
 flossdvi$Late_Decline_Length[flossdvi$Late_Decline_Length < 0] <- 
   flossdvi$Late_Decline_Length[flossdvi$Late_Decline_Length < 0] + 365
 
 # Plot growing season length by plot
 par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))
 
 for (i in 1:n_plots) {
   plot_data <- flossdvi[flossdvi$Label == unique_labels[i], ]
   
   plot(plot_data$Year, plot_data$Growing_Season_Length,
        type = "o",
        col = plot_colors[i],
        lwd = 2,
        pch = 16,
        xlab = "Year",
        ylab = "Growing Season Length (Days)",
        main = paste("Growing Season -", unique_labels[i]))
   
   # Add trend line
   if (sum(!is.na(plot_data$Growing_Season_Length)) > 2) {
     trend <- lm(Growing_Season_Length ~ Year, data = plot_data, na.action = na.exclude)
     abline(trend, col = plot_colors[i], lty = 2, lwd = 2)
   }
 }
 
 par(mfrow = c(1, 1))
 
 # Option 4: Plot decline phases
 par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
 
 # Early decline phase (Maturity to MidGreendown)
 plot(range(flossdvi$Year, na.rm = TRUE), 
      range(flossdvi$Early_Decline_Length, na.rm = TRUE),
      type = "n",
      xlab = "Year", 
      ylab = "Early Decline Length (Days)",
      main = "Early Decline Phase (Maturity to MidGreendown)")
 
 for (i in 1:n_plots) {
   plot_data <- flossdvi[flossdvi$Label == unique_labels[i], ]
   lines(plot_data$Year, plot_data$Early_Decline_Length, 
         col = plot_colors[i], lwd = 2)
   points(plot_data$Year, plot_data$Early_Decline_Length, 
          col = plot_colors[i], pch = 16, cex = 1.2)
 }
 
 legend("topright", legend = unique_labels, col = plot_colors, lwd = 2, pch = 16, cex = 0.8)
 
 # Late decline phase (MidGreendown to Senescence)
 plot(range(flossdvi$Year, na.rm = TRUE), 
      range(flossdvi$Late_Decline_Length, na.rm = TRUE),
      type = "n",
      xlab = "Year", 
      ylab = "Late Decline Length (Days)",
      main = "Late Decline Phase (MidGreendown to Senescence)")
 
 for (i in 1:n_plots) {
   plot_data <- flossdvi[flossdvi$Label == unique_labels[i], ]
   lines(plot_data$Year, plot_data$Late_Decline_Length, 
         col = plot_colors[i], lwd = 2)
   points(plot_data$Year, plot_data$Late_Decline_Length, 
          col = plot_colors[i], pch = 16, cex = 1.2)
 }
 
 legend("topright", legend = unique_labels, col = plot_colors, lwd = 2, pch = 16, cex = 0.8)
 
 par(mfrow = c(1, 1))
 
 # Option 5: Growing season length - all plots together
 plot(range(flossdvi$Year, na.rm = TRUE), 
      range(flossdvi$Growing_Season_Length, na.rm = TRUE),
      type = "n",
      xlab = "Year", 
      ylab = "Growing Season Length (Days)",
      main = "Growing Season Length - All Plots")
 
 for (i in 1:n_plots) {
   plot_data <- flossdvi[flossdvi$Label == unique_labels[i], ]
   
   lines(plot_data$Year, plot_data$Growing_Season_Length, 
         col = plot_colors[i], lwd = 2)
   points(plot_data$Year, plot_data$Growing_Season_Length, 
          col = plot_colors[i], pch = 16, cex = 1.2)
 }
 
 legend("topright", 
        legend = unique_labels,
        col = plot_colors,
        lwd = 2, pch = 16,
        cex = 0.8)
 
 # Summary statistics using base R
 cat("\n=== SUMMARY STATISTICS ===\n\n")
 
 for (i in 1:n_plots) {
   plot_data <- flossdvi[flossdvi$Label == unique_labels[i], ]
   
   cat("Plot:", unique_labels[i], "\n")
   cat("Maturity DOY - Mean:", round(mean(plot_data$Maturity_DOY, na.rm = TRUE), 1),
       "SD:", round(sd(plot_data$Maturity_DOY, na.rm = TRUE), 1),
       "Range:", range(plot_data$Maturity_DOY, na.rm = TRUE), "\n")
   cat("MidGreendown DOY - Mean:", round(mean(plot_data$MidGreendown_DOY, na.rm = TRUE), 1),
       "SD:", round(sd(plot_data$MidGreendown_DOY, na.rm = TRUE), 1),
       "Range:", range(plot_data$MidGreendown_DOY, na.rm = TRUE), "\n")
   cat("Senescence DOY - Mean:", round(mean(plot_data$Senescence_DOY, na.rm = TRUE), 1),
       "SD:", round(sd(plot_data$Senescence_DOY, na.rm = TRUE), 1),
       "Range:", range(plot_data$Senescence_DOY, na.rm = TRUE), "\n")
   cat("Growing Season Length - Mean:", round(mean(plot_data$Growing_Season_Length, na.rm = TRUE), 1),
       "SD:", round(sd(plot_data$Growing_Season_Length, na.rm = TRUE), 1), "\n")
   cat("Early Decline Length - Mean:", round(mean(plot_data$Early_Decline_Length, na.rm = TRUE), 1),
       "SD:", round(sd(plot_data$Early_Decline_Length, na.rm = TRUE), 1), "\n")
   cat("Late Decline Length - Mean:", round(mean(plot_data$Late_Decline_Length, na.rm = TRUE), 1),
       "SD:", round(sd(plot_data$Late_Decline_Length, na.rm = TRUE), 1), "\n")
   cat("---\n")
 }
 
 # Calculate correlations with year (to detect trends)
 cat("\n=== TREND ANALYSIS ===\n\n")
 for (i in 1:n_plots) {
   plot_data <- flossdvi[flossdvi$Label == unique_labels[i], ]
   
   cat("Plot:", unique_labels[i], "\n")
   
   # Maturity trend
   if (sum(!is.na(plot_data$Maturity_DOY)) > 3) {
     mat_cor <- cor(plot_data$Year, plot_data$Maturity_DOY, use = "complete.obs")
     cat("Maturity trend correlation with year:", round(mat_cor, 3), "\n")
   }
   
   # MidGreendown trend
   if (sum(!is.na(plot_data$MidGreendown_DOY)) > 3) {
     mid_cor <- cor(plot_data$Year, plot_data$MidGreendown_DOY, use = "complete.obs")
     cat("MidGreendown trend correlation with year:", round(mid_cor, 3), "\n")
   }
   
   # Senescence trend  
   if (sum(!is.na(plot_data$Senescence_DOY)) > 3) {
     sen_cor <- cor(plot_data$Year, plot_data$Senescence_DOY, use = "complete.obs")
     cat("Senescence trend correlation with year:", round(sen_cor, 3), "\n")
   }
   
   # Growing season trend
   if (sum(!is.na(plot_data$Growing_Season_Length)) > 3) {
     gs_cor <- cor(plot_data$Year, plot_data$Growing_Season_Length, use = "complete.obs")
     cat("Growing season length correlation with year:", round(gs_cor, 3), "\n")
   }
   
   # Early decline trend
   if (sum(!is.na(plot_data$Early_Decline_Length)) > 3) {
     ed_cor <- cor(plot_data$Year, plot_data$Early_Decline_Length, use = "complete.obs")
     cat("Early decline length correlation with year:", round(ed_cor, 3), "\n")
   }
   
   # Late decline trend
   if (sum(!is.na(plot_data$Late_Decline_Length)) > 3) {
     ld_cor <- cor(plot_data$Year, plot_data$Late_Decline_Length, use = "complete.obs")
     cat("Late decline length correlation with year:", round(ld_cor, 3), "\n")
   }
   
   cat("---\n")
 }
 
# # 1. Prepare a spatial object of ALL polygons
# 
# # Convert .geo to sf objects. Handle potential errors
# polygons_sf <- lapply(floss10.dat$.geo, function(geo_json) {
#   tryCatch({
#     st_read(geo_json, quiet = TRUE)
#   }, error = function(e) {
#     message(paste("Error reading GeoJSON:", e))
#     return(NULL) # Or handle the error differently, e.g., skip the polygon
#   })
# })
# 
# # Remove NULL entries (if any GeoJSON read errors occurred)
# polygons_sf <- polygons_sf[!sapply(polygons_sf, is.null)]
# 
# # Combine all polygons into a single sf object
# all_polygons_sf <- do.call(rbind, polygons_sf)
# all_polygons_sf$Label <- floss10.dat$Label # Add the labels back
# 
# # 2. Approximate tile grid (without external data)
# 
# # Define approximate tile size (degrees) - adjust if needed.  MODIS tiles are roughly 10 degrees.
# tile_size <- 10
# 
# # Get the bounding box of all polygons
# bbox_all <- st_bbox(all_polygons_sf)
# 
# # Calculate the range of latitudes and longitudes
# lat_range <- bbox_all["ymax"] - bbox_all["ymin"]
# lon_range <- bbox_all["xmax"] - bbox_all["xmin"]
# 
# # Calculate the number of tiles in each direction
# n_lat_tiles <- ceiling(lat_range / tile_size)
# n_lon_tiles <- ceiling(lon_range / tile_size)
# 
# # Create a data frame to hold tile information
# unique_tiles <- data.frame(horizontal = integer(), vertical = integer())
# 
# for(i in 1:nrow(all_polygons_sf)){
#   poly <- all_polygons_sf[i,]
#   
#   poly_centroid <- st_centroid(poly)
#   poly_lat <- st_coordinates(poly_centroid)[, "Y"]
#   poly_lon <- st_coordinates(poly_centroid)[, "X"]
#   
#   # Calculate the horizontal and vertical tile indices for the polygon
#   h_index <- ceiling((poly_lon - bbox_all["xmin"]) / tile_size)
#   v_index <- ceiling((poly_lat - bbox_all["ymin"]) / tile_size)
#   
#   # Add the tile indices to the unique_tiles data frame
#   unique_tiles <- rbind(unique_tiles, data.frame(horizontal = h_index, vertical = v_index))
# }
# 
# unique_tiles <- unique(unique_tiles)
# 
# 
# # 3. Download data for unique tiles and years
# 
# all_ndvi_data <- list()
# 
# for (year in 2001:2023) {
#   for (i in 1:nrow(unique_tiles)) {
#     h <- unique_tiles$horizontal[i]
#     v <- unique_tiles$vertical[i]
#     
#     tryCatch({
#       ndvi_data <- mt_subset(
#         product = "MOD13Q1",
#         band = "250m_16_days_NDVI",
#         tile_h = h,
#         tile_v = v,
#         start = paste0(year, "-01-01"),
#         end = paste0(year, "-12-31"),
#         site_name = paste0("h", h, "v", v), # Unique site name for each tile
#         internal = TRUE,
#         progress = TRUE
#       )
#       
#       if (!is.null(ndvi_data) && nrow(ndvi_data) > 0) {
#         all_ndvi_data <- append(all_ndvi_data, list(ndvi_data))
#       } else {
#         message(paste("No MODIS data returned for tile h", h, " v", v, " and year ", year))
#       }
#       
#     }, error = function(e) {
#       message(paste("Error downloading MODIS data:", e))
#     })
#   }
# }
# 
# all_ndvi_data <- dplyr::bind_rows(all_ndvi_data)
# 
# # 4. Extract NDVI for each polygon (after download)
# 
# # Convert to SpatVector for efficient spatial operations
# all_ndvi_spatvector <- vect(all_ndvi_data)
# 
# # Function to extract NDVI for a single polygon
# extract_ndvi_from_downloaded <- function(poly_sf, ndvi_spatvector) {
#   tryCatch({
#     # Crop the SpatVector to the polygon
#     cropped_ndvi <- crop(ndvi_spatvector, poly_sf)
#     
#     # Convert to dataframe and calculate mean
#     cropped_ndvi_df <- as.data.frame(cropped_ndvi)
#     
#     if(nrow(cropped_ndvi_df) > 0){
#       cropped_ndvi_df$NDVI <- cropped_ndvi_df$value / as.numeric(cropped_ndvi_df$scale)
#       
#       mean_ndvi <- aggregate(NDVI ~ calendar_date, data = cropped_ndvi_df, FUN = mean, na.rm = TRUE)
#       mean_ndvi$Label <- poly_sf$Label # Add the label
#       return(mean_ndvi)
#     } else {
#       return(data.frame()) # Return empty df if no data
#     }
#   }, error = function(e){
#     message(paste("Error extracting NDVI for polygon:", e))
#     return(data.frame()) # Return empty df if error
#   })
# }
# 
# polygon_ndvi_list <- list()
# for(i in 1:nrow(all_polygons_sf)){
#   polygon_ndvi <- extract_ndvi_from_downloaded(all_polygons_sf[i,], all_ndvi_spatvector)
#   polygon_ndvi_list[[i]] <- polygon_ndvi
# }
# 
# polygon_ndvi_df <- dplyr::bind_rows(polygon_ndvi_list)
# 
# # ... (Visualization using polygon_ndvi_df) ...
# 
# 
# ggplot(average_ndvi, aes(x = Year, y = mean_NDVI, group = Label, color = Label)) +
#   geom_line() +
#   geom_point() +
#   labs(title = "Average NDVI Time Series per Label", x = "Year", y = "Mean NDVI") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
