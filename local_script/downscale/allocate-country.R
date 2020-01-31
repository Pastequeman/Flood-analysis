## Purpose:
# this script allocate a country to the 0.005 by 0.005 grid cells
# due to memmory requorement process the HYDROSHED regions

## libraries
library("tidyverse")
#library("RColorBrewer")
#library("extrafont") ; loadfonts(device = "win")
library("sf")
library("sp")
library("rworldmap")
library("raster")
library("spData")

## shapefile of the country of the world
country <- st_read("Inputs/country/Countries_WGS84.shp")
#st_geometry_type(country)
#st_bbox(country)

## can be simplify to lower memory requirement
country_simp <- rmapshaper::ms_simplify(country, keep = 0.01, keep_shapes = TRUE)


## for all country downscale to 0.05 degree resolution


xdef
ydef
xstart + 0.005*xdef
ystart - 0.005*ydef

## currently missing russia
for (i in 1:nrow(country)) {
  print(i)
  ## keeponly the datain the zone
  id <- country[i, ]
  raster_template = raster(extent(id), resolution = 0.005,
                           crs = st_crs(id)$proj4string)
  
  ch_raster1 <- rasterize(id, raster_template , field = 1)
  ch_raster1[ch_raster1 == 1] <- i

  boxes <- bbox(ch_raster1)
  if (i == 1) {
  countries_boxes <-   tibble(lon_min = boxes[1, 1],
           lon_max = boxes[1, 2],
           lat_min = boxes[2, 1],
           lat_max = boxes[2, 2],
           country_name = as.character(id$CNTRY_NAME))
  } else {
    countries_boxes <- rbind(countries_boxes, tibble(lon_min = boxes[1, 1],
                                                     lon_max = boxes[1, 2],
                                                     lat_min = boxes[2, 1],
                                                     lat_max = boxes[2, 2],
                                                     country_name = as.character(id$CNTRY_NAME)))
  }
}
write_csv(countries_boxes, "Outputs/country_specs_v2.csv")


# After getting the spec file, 
# the 14 region can be arranged

regions <- c("as1", "as2", "as3", "ca1", "eu1", "eu2",  # "af1", 
             "eu3", "na1", "na2", "oc1", "sa1", "si1") # , "si2"

for (region in regions) {
  # Regions specific
  if (region == "af1") {           # region1
    xdef   <- 11000 ; ydef   <- 14000
    xstart <- 5.000 ; ystart <- 35.000
  } else if (region == "as1") {    # region 2 
    xdef   <- 9000   ; ydef   <- 11000
    xstart <- 55.000 ; ystart <- 60.000
  } else if (region == "as2") {    # region 3
    xdef   <- 12000  ; ydef   <- 8000
    xstart <- 90.000 ; ystart <- 60.000
  } else if (region == "as3") {    # region 4
    xdef   <- 13000  ; ydef   <- 10000
    xstart <- 90.000 ; ystart <- 35.000
  } else if (region == "ca1") {    # region 5
    xdef   <- 12000    ; ydef   <- 7000
    xstart <- -120.000 ; ystart <- 40.000
  } else if (region == "eu1") {    # region 6
    xdef   <- 8000    ; ydef   <- 12000
    xstart <- -20.000 ; ystart <- 60.000
  } else if (region == "eu2") {    # region 7
    xdef   <- 13000 ; ydef   <- 8000
    xstart <- 5.000 ; ystart <- 60.000
  } else if (region == "eu3") {    # region 8
    xdef   <- 14000 ; ydef   <- 7000
    xstart <- 0.000 ; ystart <- 80.000
  } else if (region == "na1") {    # region 9
    xdef   <- 16000    ; ydef   <- 7000
    xstart <- -130.000 ; ystart <- 60.000
  } else if (region == "na2") {    # region 10
    xdef <- 23000      ; ydef <- 5000
    xstart <- -170.000 ; ystart <- 75.000
  } else if (region == "oc1") {    # region 11
    xdef <- 14000     ; ydef <- 8000
    xstart <- 110.000 ; ystart <- -10.000
  } else if (region == "sa1") {    # region 12
    xdef <- 11000    ; ydef <- 15000
    start <- -85.000 ; ystart <- 15.000
  } else if (region == "si1") {    # region 13
    xdef <- 12000    ; ydef <- 7000
    xstart <- 55.000 ; ystart <- 80.000
  } else if (region == "si2") {    # region 14
    xdef   <- 19000   ; ydef   <- 5000
    xstart <- 100.000 ; ystart <- 75.000
  }
  print(region)
  #raster_region = raster(extent(xstart, xstart+0.005*xdef, ystart-0.005*ydef, ystart), resolution = 0.005,
  #                       crs = st_crs(id)$proj4string)
  matrix_region <- matrix(0, ydef, xdef)
  #z <- raster_region
 
  # counry that are withing the boundaryb
  detected_countries <-
  countries_boxes[((countries_boxes$lon_min >= xstart & countries_boxes$lon_min <= (xstart+0.005*xdef) | 
                   (countries_boxes$lon_max >= xstart & countries_boxes$lon_max <= (xstart+0.005*xdef))) & 
                  ((countries_boxes$lat_min >= (ystart-0.005*ydef) & countries_boxes$lat_min <= ystart) | 
                  (countries_boxes$lat_max >= (ystart-0.005*ydef) & countries_boxes$lat_max <= ystart))),]
  
  for (i in detected_countries$country_name) {
    print(i) 
    
    # extract cells within the region + 
    id <- country[country$CNTRY_NAME == i, ]
    raster_template = raster(extent(id), resolution = 0.005,
                             crs = st_crs(id)$proj4string)
    ch_raster1 <- rasterize(id, raster_template , field = 1)
    ch_raster1[ch_raster1 == 1] <- id$OBJECTID
   
    # recreate the country delim within the country
    if (bbox(ch_raster1)[1, 1] < xstart) {
      # what is the position of the first grid cell within the region
      l <- seq(extent(id)[1], by = 0.005, length.out = dim(ch_raster1)[2])
      start_at <- which(near(l, xstart, tol = 0.005) == TRUE)[1]
      main_mat <- 1
    } else {
      start_at <- 1
      l <- seq(xstart, by = 0.005, length.out = xdef)
      main_mat <- which(near(l, bbox(ch_raster1)[1, 1], tol = 0.0050) == TRUE)[1]
    }
    
    # 2
    if (bbox(ch_raster1)[1, 2] > (xstart+0.005*xdef)) {
      l <- seq(extent(id)[1], by = 0.005, length.out = dim(ch_raster1)[2])
      end_at <- which(near(l, (xstart+0.005*xdef), tol = 0.005) == TRUE)[1]
      main_to <- xdef
    } else {
      end_at <- dim(ch_raster1)[2]
      l <- seq(xstart, by = 0.005, length.out = xdef)
      main_to <- which(near(l, bbox(ch_raster1)[1, 2], tol = 0.005) == TRUE)[1]
    }
    
     # 3
    if (bbox(ch_raster1)[2, 1] < (ystart-0.005*ydef)) {
      l <- seq(extent(id)[4], by = -.005, length.out = dim(ch_raster1)[1])
      row_dwn <- which(near(l, (ystart-0.005*ydef), tol = 0.005) == TRUE)[1]
      main_dwn <- ydef
    } else {
      row_dwn <- dim(ch_raster1)[1]
      l <- seq(ystart, by = -0.005, length.out = ydef)
      main_dwn <- which(near(l, bbox(ch_raster1)[2, 1], tol = 0.005) == TRUE)[1]
    }
    # 4
    if (bbox(ch_raster1)[2, 2] > ystart) {
      l <- seq(extent(id)[4], by = -.005, length.out = dim(ch_raster1)[1])
      row_up <- which(near(l, (ystart), tol = 0.005) == TRUE)[1]
      main_up <- 1
    } else {
      row_up <- 1
      l <- seq(ystart, by = -0.005, length.out = ydef)
      main_up <- which(near(l, bbox(ch_raster1)[2, 2], tol = 0.005) == TRUE)[1]
    }
    
    ## the data to take 
    temp <- 
    matrix(ch_raster1$layer[row_up:row_dwn, start_at:end_at], 
           nrow = (1+row_dwn - row_up),
           ncol = (1+end_at - start_at))
    
    ## Substitution
    matrix_region[main_up:(main_up+(row_dwn - row_up)), main_mat:(main_mat+(end_at - start_at))] <- 
      ifelse(!is.na(temp),temp , matrix_region)
    
    
    
  }
  
  file_out <- file(paste0("Outputs/downscaled_countries/", region, "_0.005deg.bin"), open = "wb")
  writeBin(object = as.integer(as.vector(matrix_region)), con = file_out, size = 4, endian = "little")
  close(file_out)
}

