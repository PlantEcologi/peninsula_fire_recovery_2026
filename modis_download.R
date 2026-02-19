### Code to download MODIS data
# modified from https://rspatial.org/modis/2-download.html

### Setup
library(terra)
library(luna)
library(sf)

### Explore download options
# Lists all products that are currently searchable
prod <- getProducts()
head(prod)

# List MODIS products
modis <- getProducts("^MOD|^MYD|^MCD")
head(modis)
getProducts("MOD13Q1")
productInfo(product = "MOD13Q1", version = "061")

### Settings
product = "MOD13Q1"
start <- "2000-02-18"
end <- "2026-02-19"
#aoi <- c(18.37, 18.48, -34.2, -34.34)
aoi <- ext(vect(st_as_sfc(st_bbox(c(xmin = 18.37, xmax = 18.48, ymax = -34.2, ymin = -34.34), crs = st_crs(4326)))))

### Tester
getNASA(product, start, end, aoi=aoi, download = FALSE)

### Set your personal NASA EarthData username and password (don't save and share!)
# username <- ""
# password <- ""

### The download
getNASA(product, start, end, aoi=aoi, download=TRUE, overwrite=TRUE,
        path="bigdata", username=username, password=password)
