

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
    'data.table',
    'viridis',
    'ggdist',
    'ggplot2',
    
    'tidytext',
    'tm',
    'purrr',
    'tigris',
    'areal',
    'haven',
    'psych'
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
data_input  <- paste0(user, "/Documents/Research/Rebuilt/Data/Source/HCAD/Property Data")
process_output <- paste0(user, "/Documents/Research/Rebuilt/Process")
image_output <- paste0(user, "/Documents/Research/Rebuilt/Output/Images")

#options
options(scipen = 999)

#set seed
set.seed(123)

# API : PROVIDE YOUR OWN API KEY
API.key <- "" 
tidycensus::census_api_key(key = API.key)
readRenviron("~/.Renviron")


# *******************
# 2. Load comprehensive codebooks ----

# ACS
for(x in c(2010, 2011, 2015,2019)) {
  filename <- paste0("v", x)
  assign(filename, (load_variables(x, "acs5", cache = TRUE)))
}

#decennial
for(x in c(1:4)) {
  filename <- paste0("vd2000_sf", x)
  assign(filename, (load_variables(2000, paste0("sf", x), cache = TRUE)))
}


# *******************
# 3. Select Variables ----

desired_vars_acs = c(
  persons = "B01003_001",  #Estimate!!Total TOTAL POPULATION
  
  hhs          = "B11001_001", # Estimate!!Total HOUSEHOLD TYPE (INCLUDING LIVING ALONE)
  hus          = "B25002_001", #Estimate!!Total OCCUPANCY STATUS
  husocc       = "B25002_002", #Estimate!!Total!!Occupied
  husvac       = "B25002_003", #Estimate!!Total!!Vacant
  husocc_own   = "B25003_002", #Estimate!!Total:!!Owner occupied TENURE
  husocc_rent = "B25003_003",  #Estimate!!Total:!!Renter occupied TENURE

  latino = "B03002_012",   # Estimate!!Total!!Hispanic or Latino  HISPANIC OR LATINO ORIGIN BY RACE
  white = "B03002_003",    # Estimate!!Total!!Not Hispanic or Latino!!White alone
  black = "B03002_004",    # Estimate!!Total!!Not Hispanic or Latino!!Black or African American alone
  am = "B03002_005",       # Estimate!!Total!!Not Hispanic or Latino!!American Indian and Alaska Native alone
  asian = "B03002_006",    # Estimate!!Total!!Not Hispanic or Latino!!Asian alone
  nhpi =    "B03002_007",    # Estimate!!Total!!Not Hispanic or Latino!!Native Hawaiian and Other Pacific Islander alone
  other = "B03002_010",    # Estimate!!Total!!Not Hispanic or Latino!!Two or more races!!Two races including Some other race
  
  educ_sum           = "B15002_001", #Estimate!!Total: SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER
  educ_hs_m          = "B15002_011", #Estimate!!Total:!!Male:!!High school graduate (includes equivalency)
  educ_hs_f          = "B15002_028", #Estimate!!Total:!!Female:!!High school graduate (includes equivalency)
  educ_as_m          = "B15002_014", #Estimate!!Total:!!Male:!!Associate's degree
  educ_as_f          = "B15002_031", #Estimate!!Total:!!Female:!!Associate's degree
  educ_ba_m          = "B15002_015", #Estimate!!Total:!!Male:!!Bachelor's degree
  educ_ba_f          = "B15002_032", #Estimate!!Total:!!Female:!!Bachelor's degree
  educ_ma_m          = "B15002_016", #Estimate!!Total:!!Male:!!Master's degree
  educ_ma_f          = "B15002_033", #Estimate!!Total:!!Female:!!Master's degree
  educ_pf_m          = "B15002_017", #Estimate!!Total:!!Male:!!Professional school degree
  educ_pf_f          = "B15002_034", #Estimate!!Total:!!Female:!!Professional school degree
  educ_dr_m          = "B15002_018", #Estimate!!Total:!!Male:!!Doctorate degree
  educ_dr_f          = "B15002_035", #Estimate!!Total:!!Female:!!Doctorate degree

  nonmove_1yr_sum  = "B07003_001", #Estimate!!Total: GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY SEX FOR CURRENT RESIDENCE IN THE UNITED STATES
  nonmove_1yr      = "B07003_004", #Estimate!!Total:!!Same house 1 year ago:
  
  per_inc =  "B19301_001", #Estimate!!Per capita income in the past 12 months (in 20xx*** inflation-adjusted dollars)
  mhhinc = "B19013_001", #Estimate!!Median household income in the past 12 months (in 20xx** inflation-adjusted dollars)
  mfaminc = "B19113_001", #Estimate!!Median family income in the past 12 months (in 2010 inflation-adjusted dollars)
  
  pov_sum = "B17001_001",  #Estimate!!Total || POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE
  povdet =  "B17001_002",  #Estimate!!Total!!Income in the past 12 months below poverty level
  
  med_rent = "B25064_001",    #Estimate!!Median gross rent
  med_rent_ct = "B25063_002", #count median val is based off of
  med_hval = "B25077_001",    #Estimate!!Median value (dollars) ///	 MEDIAN VALUE (DOLLARS)
  med_hval_ct = "B25075_001", #count median val is based off of
  med_yrblt =  "B25035_001",    #Estimate!!Median year structure built
  med_yrblt_ct = "B25034_001",  #count median val is based off of
  tenure =   "B25039_001",    #Estimate!!Median year householder moved into unit!!Total
  tenure_ct = "B25034_001"   #count median val is based off of

)



# *******************
# 4. Pull Variables ----


states <- as.list(
  c('Alabama', 'Alaska', 'Arizona', 'Arkansas',
    'California', 'Colorado', 'Connecticut',
    'Delaware', 'District of Columbia',
    'Florida', 'Georgia', 'Hawaii',
    'Idaho', 'Illinois', 'Indiana', 'Iowa',
    'Kansas', 'Kentucky', 'Louisiana',
    'Maine', 'Maryland', 'Massachusetts', 'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana',
    'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico', 'New York', 'North Carolina', 'North Dakota',
    'Ohio', 'Oklahoma', 'Oregon',
    'Pennsylvania','Puerto Rico', 'Rhode Island', 'South Carolina', 'South Dakota',
    'Tennessee', 'Texas', 'Utah',
    'Vermont', 'Virginia',
    'Washington', 'West Virginia', 'Wisconsin', 'Wyoming'
  )
)


acs_cv <- map_dfr(
  setNames(c(2010, 2011, 2016, 2019),c(2010, 2011, 2016, 2019)),
  ~get_acs(
    geography = "tract",
    state = states,
    variables = desired_vars_acs,
    year = .x,
    output = "wide",
    geometry = FALSE
  ),
  .id = "source"
)

# ***************************
# cleaning up and recoding

#recodes source in a more readable way
acs_cv$source <- paste0('acs', as.integer(acs_cv$source) - 4, '_', acs_cv$source)


#splits out geographic features into sep. columns b.c it pulls in one column
acs_cv <- separate(acs_cv, NAME, into = c("TRACT", "COUNTY", "STATE"), sep = ", ")
acs_cv <- acs_cv %>% mutate(st_fip = substr(GEOID, 1, 2)) %>% dplyr::select(source, GEOID, TRACT, COUNTY, STATE, st_fip, everything())


#removes MOE COLUMNS
acs_cv <- acs_cv %>% dplyr::select(-ends_with("M")) %>%
  rename_with(~str_remove(.x, "E$")) %>% 
  rename("STATE" = "STAT")


# *******************
# 5. Final recode ----

acs_cv$pphh <- with(acs_cv, persons / hhs)
acs_cv$race_oth <- with(acs_cv, asian + nhpi + am + other)

acs_cv$pc_latino <- with(acs_cv, latino / persons)
acs_cv$pc_white  <- with(acs_cv, white / persons)
acs_cv$pc_asian  <- with(acs_cv, asian / persons)
acs_cv$pc_black  <- with(acs_cv, black / persons)
acs_cv$pc_race_oth  <- with(acs_cv, (asian + nhpi + am + other) / persons)
acs_cv$pc_nonwh  <- with(acs_cv, (persons - white) / persons)

acs_cv$pc_husocc_own  <- with(acs_cv, husocc_own / hus)
acs_cv$pc_husocc_rent <- with(acs_cv, husocc_rent / hus)
acs_cv$pc_husvac      <- with(acs_cv, husvac / hus)

acs_cv$college   <- with(acs_cv, educ_as_m + educ_as_f + educ_ba_m + educ_ba_f + educ_ma_m + educ_ma_f + educ_pf_m + educ_pf_f + educ_dr_m + educ_dr_f )
acs_cv$pc_college   <- with(acs_cv, college / educ_sum)

acs_cv$pc_pov  <- with(acs_cv, povdet / pov_sum)

acs_cv$pc_nonmove_1yr <- with(acs_cv, nonmove_1yr / nonmove_1yr_sum) 



# acs subset just var interested
acs <- subset(acs_cv, select = c(
  source, GEOID, TRACT, COUNTY, STATE, st_fip,
  persons, husocc_own, pc_husocc_own, husocc_rent, pc_husocc_rent, hus, latino, pc_latino,
  white, pc_white, black, pc_black, asian, pc_asian, race_oth, pc_race_oth, college, pc_college, nonmove_1yr, pc_nonmove_1yr,
  povdet, pc_pov, mfaminc, med_rent, med_hval, med_yrblt, tenure
))


colnames(acs) <- tolower(colnames(acs))

# ******************************
# 6. Save out and Close ----


# save files
save(acs_cv, file = paste0(data_output, "/04_acs_all.RData"))
save(acs, file = paste0(data_output, "/04_acs_select.RData"))

#time spent
(as.POSIXct(Sys.time()) - as.POSIXct(start_time))



  