


# 1. Setup ----


#set user
user <- "C:"

#set log
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
sink(paste0(user, "/Documents/Research/Rebuilt/Process/Logs/03_log_", timestamp, ".txt"), split = TRUE)
start_time <- Sys.time()

#load packages - installs if needed
ReqPkgs <-
  c(
    'dplyr',
    'tidyverse',
    'tidycensus',
    'stringr',
    'beepr',
    'sf',
    'data.table',
    'rgdal',
    'raster',
    'exactextractr'
  )

ReqPkgs <- as.list(ReqPkgs)

package.check <- lapply(
  ReqPkgs,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

rm(package.check, ReqPkgs)

#file paths
data_output <- paste0(user, "/Documents/Research/Rebuilt/Data/Derived")
data_input  <- paste0(user, "/Documents/Research/Rebuilt/Data/Source")
process_output <- paste0(user, "/Documents/Research/Rebuilt/Process")

#keep a list of everything that gets dropped
drop_list <- list()


# *******************
# 2. Load data ----

#FEMA / NFHL 
harris_fp <- st_read(dsn = paste0(data_input, "/NFHL/48201C_20211219/S_FLD_HAZ_AR.shp"))
floods <- raster(x = paste0(data_input, "/Flood/HU10.tif"))

  
#01, 02 files
load(file = paste0(data_output, "/01_spatial parcel data.RData"))
load(file = paste0(data_output, "/02_residential data.RData"))



# *******************
# 3. check projection ----

#list of sf objects
sf_objects <- list()

for (x in ls()) {
  tryCatch({
    # Check if the data frame has a valid st_crs
    if (!is.na(st_crs(get(x)))) {
      sf_objects <- c(sf_objects, x)
    }
  }, error = function(e) {
    # If an error occurs (e.g., invalid CRS or other issue), print a message and continue
    cat("Skipping object:", x, "due to error:", conditionMessage(e), "\n")
  })
}

sf_objects

#check - 
for(x in sf_objects){
  print(x)
  print(class(get(x)))
  print(st_crs(get(x)))
  cat("\n")
  cat("\n")
  cat("\n")
}


#check projection - different, need to change
st_crs(harris_parcel) #NAD83
st_crs(harris_fp) #NAD83 
# st_crs(claims) #NAD83 

#transform
harris_parcel <- st_transform(
  harris_parcel,
  crs=st_crs(floods)
)

harris_fp <- st_transform(
  harris_fp,
  crs=st_crs(floods)
)


for(x in sf_objects){
  print(x)
  print(st_crs(floods) == st_crs(get(x)))
  cat("\n")
}



# *******************
# 4. Extract flood values----


values_flood <- exact_extract(floods, harris_parcel, c("min", "max", "mean", "sum"))
harris_parcel <- cbind(harris_parcel, values_flood) %>% 
  rename(min_flood_HU10 = min, max_flood_HU10 = max, mean_flood_HU10 = mean, sum_flood_HU10 = sum)

for(x in c("HU10")){
  print(paste0("running for variables associated with geotif: ", x)) 
  min_variable <- paste0("min_flood_", x)
  max_variable <- paste0("max_flood_", x)
  mean_variable <- paste0("mean_flood_", x)
  
  print("min")
  print(summary(harris_parcel[[min_variable]]))
  print("max")
  print(summary(harris_parcel[[max_variable]]))
  print("mean")
  print(summary(harris_parcel[[mean_variable]]))
  cat("\n")
}


#clean up
rm(values_flood, floods)



# *******************
# 6. add floodplain data----

#looks
with(harris_fp, table(ZONE_SUBTY, SFHA_TF, useNA = "always"))
with(harris_fp, table(FLD_ZONE, SFHA_TF, useNA = "always"))
with(harris_fp, table(FLD_ZONE, ZONE_SUBTY, useNA = "always"))

#clean
harris_fp$geocheck <- st_is_valid(harris_fp)
table(harris_fp$geocheck, useNA="always") #13 false

harris_fp <- st_make_valid(harris_fp)
harris_fp$geocheck <- st_is_valid(harris_fp) #all true now
table(harris_fp$geocheck, useNA="always")

harris_fp <- subset(harris_fp, select = -c(geocheck)) 

#join
harris_simple <- subset(harris_parcel, select = c("HCAD_NUM"))
harris_pfp <- st_join(harris_simple, harris_fp, join=st_intersects, left = TRUE) #



harris_pfp$fp_overlap <- st_area(harris_pfp)
summary(harris_pfp$fp_overlap)

nrow(harris_pfp) - nrow(harris_simple) #some overlaps 
sum(is.na(harris_pfp$HCAD_NUM)) #0
rm(harris_simple)



# ***************
# sorting out variables

#looks - SFHA: Special flood hazard area; FLD_ZONE: flood zones; ZONE_SUBTY - zone descriptions
with(harris_pfp, table(ZONE_SUBTY, FLD_ZONE, useNA = "always")) #this seems better: 
with(harris_pfp, table(FLD_ZONE, SFHA_TF, useNA = "always"))
with(harris_pfp, table(ZONE_SUBTY, SFHA_TF, useNA = "always")) 

#set legible variables
harris_pfp$flood_500 <- 0
harris_pfp$flood_100 <- 0
harris_pfp$flood_levee <- 0
harris_pfp$flood_floodway <- 0
harris_pfp$flood_coast <- 0
harris_pfp$flood_inland <- 0

#recode appropriately 
harris_pfp$flood_500[harris_pfp$ZONE_SUBTY == "0.2 PCT ANNUAL CHANCE FLOOD HAZARD"] <- 1
harris_pfp$flood_100[harris_pfp$SFHA_TF == "T"] <- 1
harris_pfp$flood_levee[harris_pfp$ZONE_SUBTY == "AREA WITH REDUCED FLOOD RISK DUE TO LEVEE"] <- 1
harris_pfp$flood_floodway[harris_pfp$ZONE_SUBTY == "FLOODWAY"] <- 1
harris_pfp$flood_floodway[harris_pfp$ZONE_SUBTY == "RIVERINE FLOODWAY SHOWN IN COASTAL ZONE"] <- 1

harris_pfp$flood_coast[harris_pfp$FLD_ZONE == "VE"] <- 1
harris_pfp$flood_inland[harris_pfp$FLD_ZONE == "A" | harris_pfp$FLD_ZONE == "AE" | harris_pfp$FLD_ZONE == "AO" ] <- 1

table(harris_pfp$flood_500, useNA="ifany")
table(harris_pfp$flood_100, useNA="ifany")
table(harris_pfp$flood_levee, useNA="ifany")
table(harris_pfp$flood_floodway, useNA="ifany")
table(harris_pfp$flood_coast, useNA="ifany")
table(harris_pfp$flood_inland, useNA="ifany")


#subsetting to only necessary
harris_floodkey <- st_drop_geometry(harris_pfp)
harris_floodkey <- subset(harris_floodkey, select = c(HCAD_NUM, flood_100, flood_500, flood_levee, flood_floodway, flood_coast, flood_inland, fp_overlap ))
table(is.na(harris_floodkey$HCAD_NUM))

#finding max value for a HCAD_NUM
harris_floodkey <- harris_floodkey %>%
  group_by(HCAD_NUM) %>%
  mutate(flood_100 = max(flood_100)) %>% 
  mutate(flood_500 = max(flood_500)) %>% 
  mutate(flood_levee = max(flood_levee)) %>% 
  mutate(flood_floodway = max(flood_floodway)) %>% 
  mutate(flood_coast = max(flood_coast)) %>% 
  mutate(flood_inland = max(flood_inland))  %>%
  ungroup()

length(unique(harris_floodkey$HCAD_NUM)) #
nrow(harris_floodkey) - length(unique(harris_floodkey$HCAD_NUM)) #
harris_floodkey <- unique(harris_floodkey)

#check if it worked
sum(!is.na(harris_floodkey$HCAD_NUM)) #
nrow(harris_floodkey) - nrow(harris_parcel) #
length(unique(harris_floodkey$HCAD_NUM)) #

table(harris_parcel$HCAD_NUM %in% harris_floodkey$HCAD_NUM)


harris_floodkey <- unique(harris_floodkey) # 
length(unique(harris_pfp$HCAD_NUM)) #


# ***************
# Join in and clean up

nrow(harris_parcel)
harris_parcel <- left_join(harris_parcel, harris_floodkey, by=c("HCAD_NUM"))
nrow(harris_parcel)

class(harris_parcel)

rm(harris_pfp, harris_fp, harris_floodkey)




# ******************************
# 7. Final join and cleanup ----


class(hcad_res)
class(harris_parcel)
hcad_res <- left_join(hcad_res, harris_parcel, by=c("HCAD_NUM"))

hcad_res <- st_as_sf(hcad_res)
class(hcad_res)

hcad <- st_drop_geometry(hcad_res)
print(colnames(hcad))

# ******************************
# 8. Save out and Close ----


# save drop list
writeLines(as.character(drop_list), paste0(user, "/Documents/Research/Rebuilt/Process/Drop List/03_drop_list.txt"))

# save files
save(harris_parcel, file = paste0(data_output, "/03_geo only.RData"))
save(hcad_res, file = paste0(data_output, "/03_geo data.RData"))
save(hcad, file = paste0(data_output, "/03_geo data dropped spatial.RData"))

#time spent
(as.POSIXct(Sys.time()) - as.POSIXct(start_time))




