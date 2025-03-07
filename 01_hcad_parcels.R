

# 1. Setup ----


#set user
user <- "C:"

#set log
timestamp <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
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
    'data.table'
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
data_input  <- paste0(user, "/Documents/Research/Rebuilt/Data/Source/HCAD/Parcels GIS")
process_output <- paste0(user, "/Documents/Research/Rebuilt/Process")


#keep an error report
drop_list <- list()

#set seed
set.seed(123)

# *******************
# 2. Load data ----

for(x in c(2016, 2017, 2018)){
  #start
  print(paste0("loading for year ", x))
  
  #load dbf files
  parcels<- st_read(dsn = paste0(data_input, "/Parcels_", x, "_Oct/Parcels.dbf"))
  
  #assign name
  assign(paste0("parcels_", x), parcels)

  #general info
  print(table(parcels$parcel_typ))
  print(colnames(parcels))
  
  #clean
  rm(parcels)
}



# ******************************
# 3. Cleaning ----


#drop list setup
drop_list <- c(drop_list,
               paste0("total number of observations in parcel file 2016: ", nrow(parcels_2016)))
drop_list <- c(drop_list,
               paste0("total number of observations in parcel file 2017: ", nrow(parcels_2017)))
drop_list <- c(drop_list,
               paste0("total number of observations in parcel file 2018: ", nrow(parcels_2018)))


#clean
for(x in c(2016, 2017, 2018)){
  
  #get file
  parcels_data <- get(paste0("parcels_", x))
  
  #validity check
  parcels_data$valid <- st_is_valid(parcels_data)
  print(table(parcels_data$valid, useNA = "always"))
  count <- nrow(parcels_data)
  
  #subset 1
  parcels_data <- subset(parcels_data, valid == TRUE) 
  drop_list <- c(drop_list, paste0(
    "total number of observations dropped for non valid geometries in parcel file ", x, ": ",
    count - nrow(parcels_data)
  ))
  
  #na values
  print(table(st_is_empty(parcels_data)))
  count <- nrow(parcels_data)
  
  #subset 2
  parcels_data <- subset(parcels_data, !st_is_empty(geometry))
  drop_list <- c(drop_list, paste0(
    "total number of observations dropped for NA geometries in parcel file ", x, ": ",
    count - nrow(parcels_data)
  ))
  
  #final check
  parcels_data$geometry_check <- st_geometry_type(parcels_data)
  print(table(parcels_data$geometry_check, useNA = "always"))
  
  #cleaning
  parcels_data <- subset(parcels_data, select = -c(valid, geometry_check))
  assign(paste0("parcels_", x), parcels_data)
  rm(parcels_data, count)
  
}


# different number of variables, looking at column names and head
for(x in c(2016, 2017, 2018)){
  cat("\n")
  cat("\n")
  print(paste0("check for year ", x))
  print(colnames(get(paste0("parcels_", x))))
  print(head(get(paste0("parcels_", x))))
}


# checking ID numbers - needs string padding to 13
for(x in c(2016, 2017, 2018)){
  cat("\n")
  cat("\n")
  print(paste0("check for year ", x))
  print(table(str_length(get(paste0("parcels_", x))$HCAD_NUM)))
}

# fix string pad issue for id number
for(x in c(2016, 2017, 2018)) {
  cat("\n")
  cat("\n")
  print(paste0("running for year ", x))
  
  parcels_data <- get(paste0("parcels_", x))
  parcels_data$HCAD_NUM <- str_pad(as.character(parcels_data$HCAD_NUM), 13, pad = "0")
  assign(paste0("parcels_", x), parcels_data)
  
}


# count of observations
# there are only 2 duplicates in the 2016 file (4 observations), none in 2017, dropping them
for(x in c(2016, 2017, 2018)){
  cat("\n")
  cat("\n")
  print(paste0("running for year ", x))
  
  parcels_data <- get(paste0("parcels_", x))
  
  
  #observation counts // unique HCAD_NUM // duplicated HCAD_NUM entries
  print(nrow(parcels_data))
  print(length(unique(parcels_data$HCAD_NUM)))
  print(nrow(parcels_data[parcels_data$HCAD_NUM %in% parcels_data$HCAD_NUM[duplicated(parcels_data$HCAD_NUM)],] ))
  
  parcels_data <- parcels_data[!parcels_data$HCAD_NUM %in% parcels_data$HCAD_NUM[duplicated(parcels_data$HCAD_NUM)],] 
  assign(paste0("parcels_", x), parcels_data)
  print(nrow(parcels_data[parcels_data$HCAD_NUM %in% parcels_data$HCAD_NUM[duplicated(parcels_data$HCAD_NUM)],] ))
  
  rm(parcels_data)
  
}

drop_list <- c(drop_list, "dropped 4 observations in 2016 for having duplicated HCAD_NUM's")

parcels_2018 <- parcels_2018 %>% rename("Shape_area" = "Shape_Area")


# subset and rename
for(x in c(2016, 2017, 2018)) {
  cat("\n")
  cat("\n")
  print(paste0("running for year ", x))
  
  parcels_data <- get(paste0("parcels_", x))
  parcels_data <- subset(parcels_data, select = c(HCAD_NUM, LocAddr, city, zip, LocNum, LocName, Shape_area)) %>% 
    rename(
      street_add = LocAddr,
      street_num = LocNum,
      street_name = LocName,
      shape_area = Shape_area
    )  
    
  assign(paste0("parcels_", x), parcels_data)
  
}


# ******************************
# 4. Joining ----


#simplify
parcels_2017_simple = st_drop_geometry(parcels_2017) %>% dplyr::select(HCAD_NUM)

#merge
harris_parcel <- merge(
  x = parcels_2016,
  y = parcels_2017_simple,
  by = "HCAD_NUM", all.x = TRUE)

# Get IDs from 2016 that aren't in 2017
df_2017_not_in_2016 <- setdiff(parcels_2017$HCAD_NUM, parcels_2016$HCAD_NUM) 
length(df_2017_not_in_2016)

#add
harris_parcel <- bind_rows(
  harris_parcel,
  parcels_2017[parcels_2017$HCAD_NUM %in% df_2017_not_in_2016,])


#drop list interlude
drop_list <- c(drop_list,
               paste0("total number of observations in combined 2016/2017 parcel file: ", length(unique(harris_parcel$HCAD_NUM))
               )
)
count <- length(unique(harris_parcel$HCAD_NUM))


# ******************
# do it again for 2018


# Get IDs from 2016 that aren't in 2017
df_2018_not_in_parcels <- setdiff(parcels_2018$HCAD_NUM, harris_parcel$HCAD_NUM) 
length(df_2018_not_in_parcels)

#add
harris_parcel <- bind_rows(
  harris_parcel,
  parcels_2018[parcels_2018$HCAD_NUM %in% df_2018_not_in_parcels,])



# ******************
# tidy

#new count
drop_list <- c(drop_list,
               paste0("total number of observations in combined full parcel file: ", length(unique(harris_parcel$HCAD_NUM))
               )
)
drop_list <- c(drop_list, paste0("number of observations added w/ 2018 file: ",
                                 length(unique(harris_parcel$HCAD_NUM)) - count))

#add flags
harris_parcel <- harris_parcel %>% mutate(
  yr_2016 = harris_parcel$HCAD_NUM %in% parcels_2016$HCAD_NUM,
  yr_2017 = harris_parcel$HCAD_NUM %in% parcels_2017$HCAD_NUM,
  yr_2018 = harris_parcel$HCAD_NUM %in% parcels_2018$HCAD_NUM
)

harris_parcel$new_flag <- ifelse(harris_parcel$yr_2016 == FALSE & harris_parcel$yr_2017 == FALSE & harris_parcel$yr_2018 == TRUE, TRUE, FALSE)

table(harris_parcel$yr_2016)
table(harris_parcel$yr_2017)
table(harris_parcel$yr_2018)
table(harris_parcel$new_flag)


#clean up
rm(parcels_2017_simple, df_2017_not_in_2016, df_2018_not_in_parcels)

#check
length(unique(harris_parcel$HCAD_NUM))
length(unique(harris_parcel$HCAD_NUM)) == nrow(harris_parcel)


# ******************************
# 5. Save out and Close ----

#save file
save(harris_parcel, file = paste0(data_output, "/01_spatial parcel data.RData"))

#time spent
(as.POSIXct(Sys.time()) - as.POSIXct(start_time))

#save logs
writeLines(as.character(drop_list), paste0(user, "/Documents/Research/Rebuilt/Process/Drop List/01_drop_list.txt"))




