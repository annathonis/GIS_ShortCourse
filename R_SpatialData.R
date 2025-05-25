
##################################
# Install & Load required packages
##################################

required_packages <- c("terra", "sf", "dplyr", "ggplot2", "ggspatial", "rnaturalearth",
                       "rgbif", "FedData")

lapply(required_packages, require, character.only = TRUE)

#############################################################################
# Direct importing data from different data sources
#############################################################################

### Direct importing from GBIF
library (rgbif)

# Query GBIF for Ambystoma opacum records in the US
gbif_occs_example <- occ_search(scientificName = "Ambystoma opacum", 
                       country = "US", limit = 500)

gbif_occ_df <- gbif_occs_example$data

# View structure
head(gbif_occ_df[, c("decimalLongitude", "decimalLatitude", "eventDate")])
View(gbif_occ_df)

### Direct importing from WorldClim
## this one does take more time to actually import because the files are larger
library(raster)

# Download 10 arc-minute resolution BioClim data
bio <- getData("worldclim", var = "bio", res = 10)

# Plot one layer
plot(bio[[1]], main = "Annual Mean Temperature")

### Direct importing elevation data from SRTM or WorldClim
elev <- getData("SRTM", lon = -72, lat = 42)
plot(elev)

### Direct importing land cover data (NLCD via FedData)
# Note: this one also takes some time to run
library(FedData)

# Download for a bounding box or shapefile
bb <- ext(-73, -72, 41.5, 42.5) #bounding box

# Convert to SpatRaster (FedData will work with this)
r_template <- rast(bb)

# if using bounding box
nlcd_bb <- get_nlcd(template = r_template, year = 2016, label = "NE")

#if using a shapefile
nlcd_shp <- get_nlcd(template = NE_shp, year = 2016, label = "NE")

plot(nlcd)

### Direct importing vector boundaries from Natural Earth
library(rnaturalearth)
library(sf)

states <- ne_states(country = "United States of America", returnclass = "sf")
plot(st_geometry(states))


####################################
# Load occurrences and convert to sf
####################################

occ <- read.csv("C:/Users/annat/Documents/Fordham GIS Course/NE_marb_salam.csv")
head(occ)

########################################
# Convert to sf (simple features) object
########################################
occ_sf <- st_as_sf(occ, coords = c("Longitude", "Latitude"), crs = 4326)

# Plot points only
plot(st_geometry(occ_sf), main = "Occurrence Points: Ambystoma opacum")

########################################
#  Load vector shapefile (U.S. boundary)
########################################

NE_shp <- st_read("C:/Users/annat/Documents/Fordham GIS Course/NewEngland.shp")  # Replace with your actual filename
plot(st_geometry(NE_shp), col = "gray90", main = "New England Marble Salamander Occurrences")
plot(st_geometry(occ_sf), col = "blue", add = TRUE)

# Load worldClim bioclimatic variables - just load bio1
bio1 <- terra::rast("C:/Users/annat/Documents/Fordham GIS Course/bio1-div10.tif")

# Look at bio1 layer
plot(bio1, main = "Annual Mean Temperature (Bio1)")

# Crop bio1 raster to New England extent
bio1_NE <- terra::crop(bio1, vect(NE_shp))
bio1_NE <- terra::mask(bio1_NE, vect(NE_shp))

# Plot the cropped raster
plot(bio1_NE, main = "New England Annual Mean Temperature")

# Overlay state outlines (from NE_shp)
plot(st_geometry(NE_shp), add = TRUE, col = NA, border = "black", lwd = 1.2)

# Add salamander occurrence points
plot(st_geometry(occ_sf), add = TRUE,
     pch = 21,            # filled circle with border
     bg = "white",         # fill color
     col = "black",       # border color
     cex = 0.7)           # size

####################################################################################
## Read in all of the 19 bioclimatic variables. These are pre-clipped to US states.
####################################################################################

# Set the path to your folder with all .tif files
bioclim_folder <- "C:/Users/annat/Documents/Fordham GIS Course/bioclim_US"

bioclim_files <- list.files(bioclim_folder, pattern = "\\.tif$", full.names = TRUE)

# Load them into a raster stack (SpatRaster)
bio_stack <- rast(bioclim_files)

# Need to correct bio1-bio11 so that they are at the correct scale (right now they are Celsius x 10)
bio_corrected <- bio_stack
bio_corrected[[1:11]] <- bio_corrected[[1:11]] / 10

# Now you can plot one bioclim variables at a time
plot(bio_corrected[[1]]) #annual mean temperature

# Let's also clip the entire bio_corrected raster stack so that it is just New England

# First: crop to extent
bio_clipped <- terra::crop(bio_corrected, vect(NE_shp))

# Then: mask to the actual polygon (removes values outside NE boundaries)
bio_clipped <- terra::mask(bio_clipped, vect(NE_shp))

plot(bio_clipped[[1]])

#######################################################################
# Extracting environmental data at the occurrence point locations
#######################################################################

# Extract values from bio_corrected at each point in occ_sf
env_vals <- terra::extract(bio_corrected, vect(occ_sf))

# Combine with the original occurrence data
# Also, extract() adds an ID column you can drop if not needed
occ_env <- cbind(occ_sf, env_vals[, -1])  # remove ID column

# Renaming columns so they have meaningful names
bioclim_names <- c(
  "AnnualMeanTemp", "MeanDiurnalRange", "Isothermality", "TempSeasonality",
  "MaxTempWarmestMonth", "MinTempColdestMonth", "TempAnnualRange",
  "MeanTempWettestQuarter", "MeanTempDriestQuarter",
  "MeanTempWarmestQuarter", "MeanTempColdestQuarter",
  "AnnualPrecip", "PrecipWettestMonth", "PrecipDriestMonth",
  "PrecipSeasonality", "PrecipWettestQuarter", "PrecipDriestQuarter",
  "PrecipWarmestQuarter", "PrecipColdestQuarter"
)

# Replace column names for 2–20 only (not the geometry column!)
names(occ_env)[2:20] <- bioclim_names

# Removing the top row because it is all NA and probably in the ocean 
occ_env <- occ_env[-c(1),]

# View the whole dataframe
View(occ_env)

####################
# Using these data
####################

#Show where A. opacum lives in 2D climate space:
ggplot(occ_env, aes(x = AnnualMeanTemp, y = AnnualPrecip)) +
  geom_point(color = "darkgreen") +
  labs(title = "Climatic Niche of A. opacum",
       x = "Annual Mean Temperature (°C)", y = "Annual Precipitation (mm)")

## Now as a hexbin density
ggplot(occ_env, aes(x = AnnualMeanTemp, y = AnnualPrecip)) +
  geom_hex(bins = 20) +
  scale_fill_viridis_c() +
  labs(title = "Density of Occurrence in Climate Space")

