
library(googlesheets4)
library(ggplot2)
library(terra)
library(MODISTools)
library(dplyr)
library(sf)
library(jsonlite)
library(readr)
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
flossdvi<-read.csv("~/Google Drive/My Drive/Reidy_research/Hansen exploration/hansen_modis.csv")
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

 
# 
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
