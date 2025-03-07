



# 1. Setup ----


#set user
user <- "C:"

#set log
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
start_time <- Sys.time()

#load packages - installs if needed
ReqPkgs <-
  c(
    'tidyverse',
    'tidycensus',
    'beepr',
    'sf',
    'rgdal'
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

# API 
API.key <- "" 
tidycensus::census_api_key(key = API.key)
readRenviron("~/.Renviron")


# *******************
# 2. Load data ----


# load files
load(file = paste0(data_output, "/03_geo data.RData"))
load(file = paste0(data_output, "/04_acs_select.RData"))


# ******************************
# 3. add geoids ----

states = "Texas"

# get texas geographies
geo_2010 <- get_decennial(
  geography = "tract", 
  variables = "P001001", 
  year = 2010,
  state = states, 
  geometry = TRUE
) %>% 
  rename(population = value) %>% 
  dplyr::select(GEOID, population) %>%
  st_transform(26950)

colnames(geo_2010)
geo_2010 <- subset(geo_2010, select = -c(population))

#check projection - different, need to change
st_crs(geo_2010) #NAD83
st_crs(hcad_res) #NAD83

st_crs(geo_2010) == st_crs(hcad_res) 


#transform
geo_2010 <- st_transform(
  geo_2010,
  crs=st_crs(hcad_res)
)

st_crs(geo_2010) == st_crs(hcad_res) 


#find parcel centroids
hcad_centroids <- st_centroid(subset(hcad_res, select = c("geometry", "HCAD_NUM")))
table(st_geometry_type(hcad_centroids))


#
hcad_centroids <- st_join(hcad_centroids, geo_2010, join = st_within, left = TRUE)
table(is.na(hcad_centroids))

hcad_res <- left_join(hcad_res, st_drop_geometry(hcad_centroids), by = c("HCAD_NUM"))
class(hcad_res)

view(subset(hcad_res, is.na(GEOID)))
drop_list <- c(drop_list, paste0("Removed: could not find centroids for: ", sum(is.na(hcad_res$GEOID))))

hcad_res <- subset(hcad_res, !is.na(GEOID))
drop_list <- c(drop_list, paste0("Remaining observations: ", nrow(hcad_res)))

colnames(hcad_res) <- tolower(colnames(hcad_res))


# ******************************
# 4. Final join and cleanup ----

colnames(acs)
acs_link <- subset(acs, select = c(source, geoid, persons, pc_husocc_own, pc_latino, pc_white, pc_black, pc_asian, pc_college, pc_nonmove_1yr, pc_pov, mfaminc, med_rent, med_hval, med_yrblt, tenure))

acs_link <- acs_link %>%
  pivot_wider(
    id_cols = geoid,
    names_from = source,
    values_from = c(persons, pc_husocc_own, pc_latino, pc_white, pc_black, pc_asian, pc_college, pc_nonmove_1yr, pc_pov, mfaminc, med_rent, med_hval, med_yrblt, tenure)
  )

hcad_res <- left_join(hcad_res, acs_link, by=c("geoid"))

#
class(hcad_res)
hcad <- st_drop_geometry(hcad_res)


length(unique(hcad_res$hcad_num))




# ******************************
# 5. Cleaning ----


for(x in c("hcad_res", "hcad")){
  df <- get(x)
  
  df <- df[, -grep("^middlename|firstname2|lastname2", colnames(df))]
  df <- subset(df, select = -c(city.y, zip.y, street_num.y, street_name.y)) %>%
    rename(city = city.x, zip = zip.x, street_num = street_num.x, street_name = street_name.x)
  
  assign(x, df)
  rm(df)
}

hcad_simple <- subset(hcad, select = -c(flood_500, flood_levee, flood_coast, flood_inland, fp_overlap))

# ******************************
# 6. Save out and Close ----


# save drop list
writeLines(as.character(drop_list), paste0(user, "/Documents/Research/Rebuilt/Process/Drop List/05_drop_list.txt"))

# save files
save(hcad_res, file = paste0(data_output, "/05_census spatial.RData"))
save(hcad, file = paste0(data_output, "/05_census.RData"))
save(hcad_simple, file = paste0(data_output, "/05_census_simple.RData"))


#time spent
(as.POSIXct(Sys.time()) - as.POSIXct(start_time))



