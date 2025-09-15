
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

 
 
 
 ###--- Senesence  
 # Plot MidGreendown only - all plots together
 plot(range(flossdvi$Year, na.rm = TRUE), 
      range(flossdvi$MidGreendown_DOY, na.rm = TRUE),
      type = "n",
      xlab = "Year", 
      ylab = "MidGreendown Day of Year",
      main = "MidGreendown Timing - All Plots")
 
 # Get unique plot labels and colors
 unique_labels <- unique(flossdvi$Label)
 n_plots <- length(unique_labels)
 plot_colors <- rainbow(n_plots)
 
 for (i in 1:n_plots) {
   plot_data <- flossdvi[flossdvi$Label == unique_labels[i], ]
   
   lines(plot_data$Year, plot_data$MidGreendown_DOY, 
         col = plot_colors[i], lwd = 2)
   points(plot_data$Year, plot_data$MidGreendown_DOY, 
          col = plot_colors[i], pch = 16, cex = 1.2)
   
   # Add trend line if enough data points
   if (sum(!is.na(plot_data$MidGreendown_DOY)) > 2) {
     midgreendown_trend <- lm(MidGreendown_DOY ~ Year, data = plot_data, na.action = na.exclude)
     abline(midgreendown_trend, col = plot_colors[i], lty = 2, lwd = 1)
   }
 }
 
 # Add legend
 legend("topright", 
        legend = unique_labels,
        col = plot_colors,
        lwd = 2, pch = 16,
        cex = 0.8)

 
 # Individual MidGreendown plots for each label with disturbance year lines
 unique_labels <- unique(flossdvi$Label)
 n_plots <- length(unique_labels)
 
 # Function to extract disturbance year from label
 extract_disturbance_year <- function(label) {
   # Extract year from labels like "GD2017_11", "GD2015_4", etc.
   year_match <- regmatches(label, regexpr("\\d{4}", label))
   if(length(year_match) > 0) {
     return(as.numeric(year_match))
   } else {
     return(NA)
   }
 }
 
 # Calculate panel layout
 n_cols <- ceiling(sqrt(n_plots))
 n_rows <- ceiling(n_plots / n_cols)
 
 # Set up the plotting layout
 par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))
 
 # Color for MidGreendown
 midgreendown_color <- "goldenrod2"
 
 for (i in 1:n_plots) {
   plot_data <- flossdvi[flossdvi$Label == unique_labels[i], ]
   disturbance_year <- extract_disturbance_year(unique_labels[i])
   
   # Create the plot
   plot(plot_data$Year, plot_data$MidGreendown_DOY, 
        type = "o",
        col = midgreendown_color,
        lwd = 2,
        pch = 16,
        cex = 1.2,
        xlab = "Year", 
        ylab = "MidGreendown Day of Year",
        main = paste("MidGreendown -", unique_labels[i]))
   
   # Add disturbance year line
   if (!is.na(disturbance_year)) {
     abline(v = disturbance_year, col = "red", lty = 1, lwd = 2)
   }
   
   # Add trend line if enough data points
   if (sum(!is.na(plot_data$MidGreendown_DOY)) > 2) {
     midgreendown_trend <- lm(MidGreendown_DOY ~ Year, data = plot_data, na.action = na.exclude)
     abline(midgreendown_trend, col = midgreendown_color, lty = 2, lwd = 2)
   }
 }
 
 # Reset plotting parameters
 par(mfrow = c(1, 1))

 
 # Function to extract disturbance year from label
 extract_disturbance_year <- function(label) {
   year_match <- regmatches(label, regexpr("\\d{4}", label))
   if(length(year_match) > 0) {
     return(as.numeric(year_match))
   } else {
     return(NA)
   }
 }
 
 # Add disturbance year to the dataset
 flossdvi$Disturbance_Year <- sapply(flossdvi$Label, extract_disturbance_year)
 
 # Calculate pre-disturbance mean MidGreendown for all plots
 # Get all pre-disturbance data points
 pre_disturbance_data <- flossdvi[flossdvi$Year < flossdvi$Disturbance_Year & !is.na(flossdvi$MidGreendown_DOY), ]
 
 # Calculate overall pre-disturbance mean
 overall_predist_mean <- mean(pre_disturbance_data$MidGreendown_DOY, na.rm = TRUE)
 
 cat("Overall pre-disturbance mean MidGreendown DOY:", round(overall_predist_mean, 1), "\n")
 
 # Individual MidGreendown plots with pre-disturbance mean comparison
 unique_labels <- unique(flossdvi$Label)
 n_plots <- length(unique_labels)
 
 # Calculate panel layout
 n_cols <- ceiling(sqrt(n_plots))
 n_rows <- ceiling(n_plots / n_cols)
 
 # Set up the plotting layout
 par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))
 
 # Colors
 midgreendown_color <- "goldenrod2"
 mean_color <- "blue"
 disturbance_color <- "red"
 for (i in 1:n_plots) {
   plot_data <- flossdvi[flossdvi$Label == unique_labels[i], ]
   disturbance_year <- extract_disturbance_year(unique_labels[i])
   
   # Create the plot
   plot(plot_data$Year, plot_data$MidGreendown_DOY, 
        type = "o",
        col = midgreendown_color,
        lwd = 2,
        pch = 16,
        cex = 1.2,
        xlab = "Year", 
        ylab = "MidGreendown Day of Year",
        main = paste("MidGreendown -", unique_labels[i]),
        ylim = range(c(plot_data$MidGreendown_DOY, overall_predist_mean), na.rm = TRUE))
   
   # Add horizontal line for overall pre-disturbance mean
   abline(h = overall_predist_mean, col = mean_color, lty = 1, lwd = 2)
   
   # Add disturbance year line
   if (!is.na(disturbance_year)) {
     abline(v = disturbance_year, col = disturbance_color, lty = 1, lwd = 2)
   }
   
   # Add trend line if enough data points
   if (sum(!is.na(plot_data$MidGreendown_DOY)) > 2) {
     midgreendown_trend <- lm(MidGreendown_DOY ~ Year, data = plot_data, na.action = na.exclude)
     abline(midgreendown_trend, col = midgreendown_color, lty = 2, lwd = 1)
   }
   
   # Add legend only to first plot
   if (i == 1) {
     legend("topright", 
            legend = c("MidGreendown", "Pre-dist Mean", "Disturbance Year"), 
            col = c(midgreendown_color, mean_color, disturbance_color),
            lty = c(1, 1, 1), lwd = 2, cex = 0.7)
   }
 }
 
 # Reset plotting parameters
 par(mfrow = c(1, 1)) 
 
 
 # ggplot2 version with pre-disturbance mean comparison
 ggplot(flossdvi, aes(x = Year, y = MidGreendown_DOY)) +
   geom_line(color = "goldenrod2", size = 1.2) +
   geom_point(color = "goldenrod2", size = 2) +
   geom_smooth(method = "lm", se = FALSE, color = "goldenrod2", linetype = "dashed", size = 0.8) +
   geom_hline(yintercept = overall_predist_mean, color = "blue", linetype = "solid", size = 1) +
   geom_vline(aes(xintercept = Disturbance_Year), color = "red", linetype = "solid", size = 1) +
   facet_wrap(~ Label, scales = "free_x") +
   labs(title = paste("MidGreendown vs Pre-Disturbance Mean (", round(overall_predist_mean, 1), " DOY)"),
        x = "Year", 
        y = "MidGreendown Day of Year") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         strip.text = element_text(size = 10)) +
   annotate("text", x = -Inf, y = Inf, 
            label = paste("Pre-dist mean:", round(overall_predist_mean, 1)), 
                hjust = -0.1, vjust = 1.1, color = "blue", size = 3)

 # Function to extract disturbance year from label
 extract_disturbance_year <- function(label) {
   year_match <- regmatches(label, regexpr("\\d{4}", label))
   if(length(year_match) > 0) {
     return(as.numeric(year_match))
   } else {
     return(NA)
   }
 }
 
 # Add disturbance year to the dataset
 flossdvi$Disturbance_Year <- sapply(flossdvi$Label, extract_disturbance_year)
 
 # Calculate pre-disturbance mean MidGreendown for all plots
 pre_disturbance_data <- flossdvi[flossdvi$Year < flossdvi$Disturbance_Year & !is.na(flossdvi$MidGreendown_DOY), ]
 overall_predist_mean <- mean(pre_disturbance_data$MidGreendown_DOY, na.rm = TRUE)
 
 # Calculate years from disturbance and deviation for each row
 flossdvi$Years_From_Disturbance <- flossdvi$Year - flossdvi$Disturbance_Year
 flossdvi$MidGreendown_Deviation <- flossdvi$MidGreendown_DOY - overall_predist_mean
 
 # Individual plots with years from disturbance vs deviation
 unique_labels <- unique(flossdvi$Label)
 n_plots <- length(unique_labels)
 
 # Calculate panel layout
 n_cols <- ceiling(sqrt(n_plots))
 n_rows <- ceiling(n_plots / n_cols)
 
 # Set up the plotting layout
 par(mfrow = c(n_rows, n_cols), mar = c(4, 4, 3, 1))
 
 # Colors
 midgreendown_color <- "goldenrod2"
 
 for (i in 1:n_plots) {
   plot_data <- flossdvi[flossdvi$Label == unique_labels[i] & !is.na(flossdvi$MidGreendown_Deviation), ]
   
   # Create the plot
   plot(plot_data$Years_From_Disturbance, plot_data$MidGreendown_Deviation, 
        type = "o",
        col = midgreendown_color,
        lwd = 2,
        pch = 16,
        cex = 1.2,
        xlab = "Years from Disturbance", 
        ylab = "MidGreendown Deviation (Days)",
        main = paste("MidGreendown Deviation -", unique_labels[i]))
   
   # Add horizontal line at zero (no deviation)
   abline(h = 0, col = "black", lty = 2, lwd = 1)
   
   # Add vertical line at disturbance year (year 0)
   abline(v = 0, col = "red", lty = 1, lwd = 2)
   
   # Add trend line if enough data points
   if (sum(!is.na(plot_data$MidGreendown_Deviation)) > 2) {
     deviation_trend <- lm(MidGreendown_Deviation ~ Years_From_Disturbance, data = plot_data, na.action = na.exclude)
     abline(deviation_trend, col = midgreendown_color, lty = 3, lwd = 2)
   }
   
   # Add legend only to first plot
   if (i == 1) {
     legend("topright", 
            legend = c("MidGreendown Dev", "No Deviation", "Disturbance"), 
            col = c(midgreendown_color, "black", "red"),
            lty = c(1, 2, 1), lwd = c(2, 1, 2), cex = 0.7)
   }
 }
 
 # Reset plotting parameters
 par(mfrow = c(1, 1))
 
 # ggplot2 version with years from disturbance vs deviation
 ggplot(flossdvi[!is.na(flossdvi$MidGreendown_Deviation), ], 
        aes(x = Years_From_Disturbance, y = MidGreendown_Deviation)) +
   geom_line(color = "goldenrod2", size = 1.2) +
   geom_point(color = "goldenrod2", size = 2) +
   geom_smooth(method = "lm", se = FALSE, color = "goldenrod2", linetype = "dotted", size = 1) +
   geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 0.8) +
   geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 1) +
   facet_wrap(~ Label, scales = "free") +
   labs(title = paste("MidGreendown Deviation from Pre-Disturbance Mean (", round(overall_predist_mean, 1), " DOY)"),
        x = "Years from Disturbance", 
        y = "MidGreendown Deviation (Days)",
        subtitle = "Positive = Later than normal, Negative = Earlier than normal") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1),
         strip.text = element_text(size = 10)) 
 # Function to extract disturbance year from label
 extract_disturbance_year <- function(label) {
   year_match <- regmatches(label, regexpr("\\d{4}", label))
   if(length(year_match) > 0) {
     return(as.numeric(year_match))
   } else {
     return(NA)
   }
 }
 
 # Add disturbance year to the dataset
 flossdvi$Disturbance_Year <- sapply(flossdvi$Label, extract_disturbance_year)
 
 # Calculate pre-disturbance mean MidGreendown for all plots
 pre_disturbance_data <- flossdvi[flossdvi$Year < flossdvi$Disturbance_Year & !is.na(flossdvi$MidGreendown_DOY), ]
 overall_predist_mean <- mean(pre_disturbance_data$MidGreendown_DOY, na.rm = TRUE)
 
 # Calculate deviation for each row
 flossdvi$MidGreendown_Deviation <- flossdvi$MidGreendown_DOY - overall_predist_mean
 
 # Remove rows with missing deviation data
 plot_data <- flossdvi[!is.na(flossdvi$MidGreendown_Deviation), ]
 
 # Get unique labels and set up colors
 unique_labels <- unique(plot_data$Label)
 n_plots <- length(unique_labels)
 plot_colors <- rainbow(n_plots)
 
 # Create single plot with all labels
 plot(range(plot_data$Year, na.rm = TRUE), 
      range(plot_data$MidGreendown_Deviation, na.rm = TRUE),
      type = "n",
      xlab = "Year", 
      ylab = "MidGreendown Deviation (Days)",
      main = paste("MidGreendown Deviation from Pre-Disturbance Mean -", round(overall_predist_mean, 1), "DOY"))
 
 # Add horizontal line at zero (no deviation)
 abline(h = 0, col = "black", lty = 2, lwd = 2)
 
 # Plot each label
 for (i in 1:n_plots) {
   label_data <- plot_data[plot_data$Label == unique_labels[i], ]
   disturbance_year <- unique(label_data$Disturbance_Year)[1]
   
   # Sort by year for proper line connections
   label_data <- label_data[order(label_data$Year), ]
   
   # Plot lines and points
   lines(label_data$Year, label_data$MidGreendown_Deviation, 
         col = plot_colors[i], lwd = 2)
   points(label_data$Year, label_data$MidGreendown_Deviation, 
          col = plot_colors[i], pch = 16, cex = 1.2)
   
   # Add vertical line for disturbance year for this label
   if (!is.na(disturbance_year)) {
     abline(v = disturbance_year, col = plot_colors[i], lty = 3, lwd = 1)
   }
 }
 
 # Add legend
 legend("topright", 
        legend = unique_labels,
        col = plot_colors,
        lwd = 2, pch = 16,
        cex = 0.8)
 
 # Add reference line legend
 legend("topleft",
        legend = "No Deviation",
        col = "black",
        lty = 2,
        lwd = 2,
        cex = 0.8)
 
 # ggplot2 version - all labels on single plot with actual years
 ggplot(flossdvi[!is.na(flossdvi$MidGreendown_Deviation), ], 
        aes(x = Year, y = MidGreendown_Deviation, color = Label)) +
   geom_line(size = 1.2) +
   geom_point(size = 2) +
   geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1) +
   geom_vline(aes(xintercept = Disturbance_Year, color = Label), linetype = "dotted", size = 1, alpha = 0.7) +
   labs(title = paste("MidGreendown Deviation from Pre-Disturbance Mean (", round(overall_predist_mean, 1), " DOY)"),
        x = "Year", 
        y = "MidGreendown Deviation (Days)",
        subtitle = "Positive = Later than normal, Negative = Earlier than normal. Dotted lines = disturbance years",
        color = "Plot") +
   theme_minimal() +
   theme(legend.position = "right") +
   guides(color = guide_legend(override.aes = list(size = 3)))
 
 
 # Filter data to include only years within reasonable range of disturbance
 plot_data <- flossdvi[!is.na(flossdvi$MidGreendown_Deviation) & 
                         abs(flossdvi$Years_From_Disturbance) <= 10, ]
 
 # Create the ggplot
 ggplot(plot_data, aes(x = Years_From_Disturbance, y = MidGreendown_Deviation, color = Label)) +
   geom_line(size = 1.2, alpha = 0.8) +
   geom_point(size = 2.5, alpha = 0.9) +
   geom_hline(yintercept = 0, color = "black", linetype = "dashed", size = 1) +
   geom_vline(xintercept = 0, color = "red", linetype = "solid", size = 1.5, alpha = 0.7) +
   scale_x_continuous(breaks = seq(-10, 10, 1), 
                      minor_breaks = NULL,
                      limits = c(-3, 3)) +  # Adjust limits as needed
   labs(title = paste("MidGreendown Deviation Relative to Disturbance Year"),
        subtitle = paste("Baseline: Pre-disturbance mean =", round(overall_predist_mean, 1), "DOY"),
        x = "Years Relative to Disturbance (0 = Disturbance Year)", 
        y = "MidGreendown Deviation (Days)",
        color = "Plot ID",
        caption = "Positive values = Later than normal | Negative values = Earlier than normal") +
   theme_minimal() +
   theme(legend.position = "right",
         panel.grid.major.x = element_line(color = "gray90", size = 0.5),
         panel.grid.minor.x = element_blank(),
         axis.text.x = element_text(size = 10),
         plot.title = element_text(size = 14, face = "bold"),
         plot.subtitle = element_text(size = 11)) +
   guides(color = guide_legend(override.aes = list(size = 3, alpha = 1)))
 
 