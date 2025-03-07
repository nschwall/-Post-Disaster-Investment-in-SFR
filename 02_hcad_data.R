

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
    'tidytext',
    'stringdist',
    'rethnicity',
    'ggplot2'
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

#keep a list of everything that gets dropped
drop_list <- list()

#set seed
set.seed(123)
options(scipen = 999)

# *******************
# 2. Load data ----

for(x in 2016:2018){
  #start
  print(paste0("loading for year ", x))
  

  bldgres <- read.table(file = paste0(data_input, "/", x, "/Real_building_land/building_res.txt"), sep="\t", header=TRUE, comment.char="#",
                        na.strings=".", stringsAsFactors=FALSE,
                        quote="", fill=TRUE)
  realacct <- read.table(file = paste0(data_input, "/", x, "/Real_acct_owner/real_acct.txt"), sep="\t", header=TRUE, comment.char="#",
                         na.strings=".", stringsAsFactors=FALSE,
                         quote="", fill=TRUE)
  
  #assign name
  assign(paste0("bldgres_", x), bldgres)
  assign(paste0("realacct_", x), realacct)
    
  #clean
  rm(struct, bldgres, realacct)
}

# ******************************
# 3. Data Review and Initial Clean----

# check if data structure is the same - all true
for(x in 2016:2018){
  print(paste0("check for year ", x))
  
  print(table(colnames(bldgres_2016) == colnames(get(paste0("bldgres_", x)))))
  print(table(colnames(realacct_2016) == colnames(get(paste0("realacct_", x)))))
}


# look at column names
colnames(bldgres_2016)
colnames(realacct_2016)


# checking ID numbers - needs string padding to 13
for(x in 2016:2018){
  print(paste0("check for year ", x))
  
  print(table(str_length(get(paste0("bldgres_", x))$acct)))
  print(table(str_length(get(paste0("realacct_", x))$acct)))
}



# fix string pad issue for id number
for(x in 2016:2018) {
  cat("\n")
  cat("\n")
  print(paste0("running for year ", x))
  
  bldgres_data <- get(paste0("bldgres_", x))
  realacct_data <- get(paste0("realacct_", x))
  
  bldgres_data$HCAD_NUM <- str_pad(as.character(bldgres_data$acct), 13, pad = "0")
  realacct_data$HCAD_NUM <- str_pad(as.character(realacct_data$acct), 13, pad = "0")
  
  assign(paste0("bldgres_", x), bldgres_data)
  assign(paste0("realacct_", x), realacct_data)
  
}

rm(struct_data, bldgres_data, realacct_data)
beep()



# ******************************
# 4. create address book ----


#just residential
realacct_2016 <- subset(realacct_2016, (grepl("[ABZ]",realacct_2016$state_class)))
realacct_2016 <- subset(realacct_2016, state_class != "TMBR")
realacct_2017 <- subset(realacct_2017, (grepl("[ABZ]",realacct_2017$state_class)))
realacct_2017 <- subset(realacct_2017, state_class != "TMBR")

realacct_2018 <- subset(realacct_2018, (grepl("[ABZ]",realacct_2018$state_class)))
realacct_2018 <- subset(realacct_2018, state_class != "TMBR")

drop_list <- c(drop_list, paste0("number of residential observations in real account 2016: ", nrow(realacct_2016)))
drop_list <- c(drop_list, paste0("number of residential observations in real account 2017: ", nrow(realacct_2017)))
drop_list <- c(drop_list, paste0("number of residential observations in real account 2018: ", nrow(realacct_2018)))

drop_list <- c(drop_list, paste0("number of residential observations from A group in real account 2016: ", 
                                 nrow(subset(realacct_2016, grepl("[A]", state_class)))))
drop_list <- c(drop_list, paste0("number of residential observations from A group in real account 2017: ", 
                                 nrow(subset(realacct_2017, grepl("[A]", state_class)))))
drop_list <- c(drop_list, paste0("number of residential observations from A group in real account 2018: ", 
                                 nrow(subset(realacct_2018, grepl("[A]", state_class)))))


drop_list <- c(drop_list, paste0("number of residential observations from B group in real account 2016: ", 
                                 nrow(subset(realacct_2016, grepl("[B]", state_class)))))
drop_list <- c(drop_list, paste0("number of residential observations from B group in real account 2017: ", 
                                 nrow(subset(realacct_2017, grepl("[B]", state_class)))))
drop_list <- c(drop_list, paste0("number of residential observations from B group in real account 2018: ", 
                                 nrow(subset(realacct_2018, grepl("[B]", state_class)))))


drop_list <- c(drop_list, paste0("number of residential observations from Z group in real account 2016: ", 
                                 nrow(subset(realacct_2016, grepl("[Z]", state_class)))))
drop_list <- c(drop_list, paste0("number of residential observations from Z group in real account 2017: ", 
                                 nrow(subset(realacct_2017, grepl("[Z]", state_class)))))
drop_list <- c(drop_list, paste0("number of residential observations from Z group in real account 2018: ", 
                                 nrow(subset(realacct_2018, grepl("[Z]", state_class)))))


print(drop_list)

#checking
summary(as.Date(realacct_2016$new_own_dt, format = "%m/%d/%Y"))
summary(as.Date(realacct_2017$new_own_dt, format = "%m/%d/%Y"))

#check, just two each
nrow(subset(realacct_2016, as.Date(new_own_dt, format = "%m/%d/%Y") > as.Date("01/01/2017", format = "%m/%d/%Y"))) #2
nrow(subset(realacct_2017, as.Date(new_own_dt, format = "%m/%d/%Y") > as.Date("01/01/2018", format = "%m/%d/%Y"))) #2

#look, both the same accounts too
subset(realacct_2016, as.Date(new_own_dt, format = "%m/%d/%Y") > as.Date("01/01/2017", format = "%m/%d/%Y"))
subset(realacct_2017, as.Date(new_own_dt, format = "%m/%d/%Y") > as.Date("01/01/2018", format = "%m/%d/%Y"))

#drop the errors
realacct_2016 <- subset(realacct_2016, as.Date(new_own_dt, format = "%m/%d/%Y") < as.Date("01/01/2017", format = "%m/%d/%Y"))
realacct_2017 <- subset(realacct_2017, as.Date(new_own_dt, format = "%m/%d/%Y") < as.Date("01/01/2018", format = "%m/%d/%Y"))
drop_list <-  c(drop_list, "dropped 2 observations in 2016 and the same 2 in 2017 from residential list for having bad new owner dates")


# ******
# return to first check: good now
summary(as.Date(realacct_2016$new_own_dt, format = "%m/%d/%Y"))
summary(as.Date(realacct_2017$new_own_dt, format = "%m/%d/%Y"))


#subset all addresses
adds_2018 <- subset(realacct_2018, select = c(
  "HCAD_NUM", "state_class", "str_unit", "site_addr_1", "site_addr_2", "site_addr_3", "str_num", "str", "str_sfx"
)) %>% rename(unit = str_unit, street = site_addr_1, city = site_addr_2, zip = site_addr_3, street_num = str_num, street_name = str, street_type = str_sfx)

adds_2017 <- subset(realacct_2017, !(HCAD_NUM %in% adds_2018$HCAD_NUM))
adds_2017 <- subset(adds_2017, select = c(
  "HCAD_NUM", "state_class", "str_unit", "site_addr_1", "site_addr_2", "site_addr_3", "str_num", "str", "str_sfx"
)) %>% rename(unit = str_unit, street = site_addr_1, city = site_addr_2, zip = site_addr_3, street_num = str_num, street_name = str, street_type = str_sfx)

addbook <- rbind(adds_2018, adds_2017)

adds_2016 <- subset(realacct_2016, !(HCAD_NUM %in% addbook$HCAD_NUM))
adds_2016 <- subset(adds_2016, select = c(
  "HCAD_NUM", "state_class", "str_unit", "site_addr_1", "site_addr_2", "site_addr_3", "str_num", "str", "str_sfx"
)) %>% rename(unit = str_unit, street = site_addr_1, city = site_addr_2, zip = site_addr_3, street_num = str_num, street_name = str, street_type = str_sfx)


addbook <- rbind(addbook, adds_2016)

#should all be 0
nrow(subset(realacct_2018, !(HCAD_NUM %in% addbook$HCAD_NUM)))
nrow(subset(realacct_2017, !(HCAD_NUM %in% addbook$HCAD_NUM)))
nrow(subset(realacct_2016, !(HCAD_NUM %in% addbook$HCAD_NUM)))

# check for duplicates - should be TRUE
length(unique(addbook$HCAD_NUM)) == nrow(addbook)

# total
nrow(addbook)


# ******************************
# 5. Final cleaning & reshaping ----


# ***********************
# change to realacct

# checking some things
colnames(realacct_2016)
head(realacct_2016)


#setup check for indicators owned by company
realacct_2016 %>% 
  unnest_tokens(word, mailto) %>%    
  anti_join(stop_words) %>%    
  count(word, sort = TRUE) %>%    
  top_n(100)   

realacct_2018 %>% 
  unnest_tokens(word, mailto) %>%    
  anti_join(stop_words) %>%   
  count(word, sort = TRUE) %>%   
  top_n(100) 


#creating the flags
for(x in 2016:2018){
  cat("\n")
  cat("\n")
  print(paste0("running for year ", x))
  
  #setup
  realacct_data <- get(paste0("realacct_", x))
  print(paste0("first HCAD count is: ", length(unique(realacct_data$HCAD_NUM))))
  print(paste0("first observation count is: ", nrow(realacct_data)))  
  
  #check
  print(
    length(unique(realacct_data$HCAD_NUM)) == nrow(realacct_data)
  )

  print("running the grepl")
  realacct_data$company <- grepl("\\b(properties|copr|llc|ltd|homes|inc|lp|count|investments|borrower|development|holdings|residential|land|american|company|houses|rent|home|homes|group|builders|partners|investment|holding|holdings|housing|management|partnership|calatlantic|realty|enterprises|interests|series|classic|sfrhou|bank|limited|associates|et al|corp|corporation|corp|real estate|realestate)\\b", 
                                 realacct_data$mailto, ignore.case = TRUE)
  
  realacct_data$other_owner <- grepl("\\b(trust|district|current owner|poa|trustee|guardian|custodian|esq|atty)\\b", 
                                     realacct_data$mailto, ignore.case = TRUE)
  
  # Additional condition for 'houston' in 'mailto' and 'company' is FALSE
  realacct_data$other_owner <- ifelse((grepl("houston", realacct_data$mailto, ignore.case = TRUE) | 
                                         grepl("estate", realacct_data$mailto, ignore.case = TRUE)) & 
                                        !realacct_data$company, 
                                      TRUE, realacct_data$other_owner)
  
  
  # simplifying some variables
  realacct_data$mail_zip_simple <- sub("-.*", "", realacct_data$mail_zip)
  realacct_data$mail_add <- paste0(realacct_data$mail_addr_1, "|", realacct_data$mail_city, "|", realacct_data$mail_state, "|", realacct_data$mail_zip_simple)
  realacct_data$street_add <- paste0(realacct_data$site_addr_1, "|", realacct_data$site_addr_2, "|TX|", realacct_data$site_addr_3 )
  realacct_data$stringdist_owner <- stringdist(realacct_data$street_add, realacct_data$mail_add, method="lv")

  print(realacct_data %>% sample_n(20) %>% dplyr::select(mail_add, street_add, stringdist_owner))
  
  
  # Getting names setup
  realacct_data$mailto <- gsub("&", " & ", realacct_data$mailto)
  realacct_data$mailto <- gsub("  ", " ", realacct_data$mailto)
  
  # get names - basic 
  realacct_data <- realacct_data %>%
    mutate(lastname = word(mailto, 1),
           firstname = word(mailto, 2, -1))
  
  # Handle multi-owner & middle names
  realacct_data <- realacct_data %>%
    mutate(
      firstname1 = ifelse(str_detect(firstname, "&"), sub("\\s*&.*", "", firstname), firstname),
      middlename1 = ifelse(str_count(firstname1, "\\s") > 0, word(firstname1, -1), NA),
      firstname1 = ifelse(str_count(firstname1, "\\s") > 0, sub("\\s*\\w+\\s*$", "", firstname1), firstname1),
      firstname2 = ifelse(str_detect(firstname, "&"), sub(".*&\\s*", "", firstname), NA),
      middlename2 = ifelse(str_count(firstname2, "\\s") > 0, word(firstname2, -1), NA),
      firstname2 = ifelse(str_count(firstname2, "\\s") > 0, sub("\\s*\\w+\\s*$", "", firstname2), firstname2)
    ) %>%
    dplyr::select(-firstname)
  
  # Remove names for companies or other owners
  realacct_data$lastname[realacct_data$company == TRUE | realacct_data$other_owner == TRUE] <- NA
  realacct_data$firstname1[realacct_data$company == TRUE | realacct_data$other_owner == TRUE] <- NA
  realacct_data$firstname2[realacct_data$company == TRUE | realacct_data$other_owner == TRUE] <- NA
  realacct_data$middlename1[realacct_data$company == TRUE | realacct_data$other_owner == TRUE] <- NA
  realacct_data$middlename2[realacct_data$company == TRUE | realacct_data$other_owner == TRUE] <- NA
  
  print(realacct_data %>% sample_n(20) %>% dplyr::select(mail_add, street_add, stringdist_owner, mailto, company, other_owner, firstname1, middlename1, firstname2, middlename2, lastname))
  
  
  #just info that is importat
  realacct_data <- subset(realacct_data, select = c(
    "HCAD_NUM", "mailto", "mail_add", 'street_add', 'company', 'other_owner',
    'new_own_dt', "stringdist_owner", "firstname1", "middlename1", "firstname2", "middlename2", "lastname",
    "yr_impr", "bld_ar", "land_ar", "bld_val", "land_val", "x_features_val", "tot_appr_val", "assessed_val", "tot_mkt_val", "state_class")) %>% 
    rename(owner = mailto)
  
  #drop dupes
  count <- nrow(realacct_data)
  realacct_data <- unique(realacct_data) 
  print(paste0("final HCAD count is: ", length(unique(realacct_data$HCAD_NUM))))
  print(paste0("final observation count is: ", nrow(realacct_data)))
  drop_list <- c(drop_list, paste0("observations dropped as duplicates for realacct ",x,": ", count - nrow(realacct_data)))
  
  #closet out
  assign(paste0("realacct_", x), realacct_data)
  rm(realacct_data, count)
}


# looking at stringdist_owner 
for(x in 2016:2018){
  cat("\n")
  cat("\n")
  print(paste0("running for year ", x))
  
  # setup
  realacct_data <- get(paste0("realacct_", x))
  
  # looks
  print("summarizing stringdist_owner")
  print(summary(realacct_data$stringdist_owner))
  
  # by state class
  print("check for A properties")
  print(summary(subset(realacct_data, grepl("[A]", state_class))$stringdist_owner))
  
  print("check for B properties")
  print(summary(subset(realacct_data, grepl("[B]", state_class))$stringdist_owner)) 
  
  print("check for Z properties")
  print(summary(subset(realacct_data, grepl("[Z]", state_class))$stringdist_owner)) 
  
  # ***
  # looks
  print("summarizing stringdist_owner not including company")
  print(summary(subset(realacct_data, company != TRUE)$stringdist_owner))
  
  # by state class
  print("check for A properties")
  print(summary(subset(realacct_data, grepl("[A]", state_class) & company != TRUE)$stringdist_owner))
  
  print("check for B properties")
  print(summary(subset(realacct_data, grepl("[B]", state_class) & company != TRUE)$stringdist_owner)) 
  
  print("check for Z properties")
  print(summary(subset(realacct_data, grepl("[Z]", state_class) & company != TRUE)$stringdist_owner)) 
  

  # make a category
  realacct_data$landlord <- ifelse(realacct_data$stringdist_owner > 3 & realacct_data$company == FALSE, TRUE, FALSE)
  
  
  #closet out
  assign(paste0("realacct_", x), realacct_data)
  rm(realacct_data, count)
  
}



#add ethnicity columns
for(x in 2016:2018){
  cat("\n")
  cat("\n")
  print(paste0("running for year ", x))

  #get correct file
  realacct_data <- get(paste0("realacct_", x))
  
  #create a matchable dataset w/out na's 
  key <- subset(realacct_data, !is.na(firstname1) & !is.na(lastname) & company != TRUE)
  key <- subset(key, select = c(HCAD_NUM, firstname1, lastname))

  #flag for changes
  key_original <- key
  key$firstname1 <- str_replace_all(key$firstname1, "[^[:graph:]]", "")
  key$lastname <- str_replace_all(key$lastname, "[^[:graph:]]", "")
  key$nameerror_flag <- ifelse(key$firstname1 != key_original$firstname1 | key$lastname != key_original$lastname, TRUE, FALSE)
  
  #predict_ethnicity  
  ethnicity <- predict_ethnicity(firstnames = key$firstname1, lastnames =  key$lastname, method = "fullname")
  
  print(nrow(key) == nrow(ethnicity))
  ethnicity <- cbind(subset(key, select = c("HCAD_NUM", "nameerror_flag")), 
                     subset(ethnicity, select = c("prob_asian", "prob_black", "prob_hispanic", "prob_white", "race")))
  realacct_data <- left_join(realacct_data, ethnicity, by=c("HCAD_NUM"))
  
  print(realacct_data %>% sample_n(20) %>% dplyr::select(HCAD_NUM, firstname1, firstname2, lastname, nameerror_flag, prob_asian, prob_black, prob_hispanic, prob_white, race))

  #close out
  assign(paste0("realacct_", x), realacct_data)
  rm(realacct_data, ethnicity, key, key_original)
  
}


#add new columns
for(x in 2016:2018){
  cat("\n")
  cat("\n")
  print(paste0("running for year ", x))

}



# ***********************
# changes to bldgres


#looks
colnames(bldgres_2016)
head(bldgres_2016)


#making sure all residential --- 
for(x in 2016:2018){
  temp <- get(paste0("bldgres_", x))
  temp <- subset(temp, bld_num > 1)
  print(table(temp$structure_dscr))
  print(table(temp$structure))
  rm(temp)
}

#just creating a flag for mobile homes
for(x in 2016:2018){
  #get
  bldgres_data <- get(paste0("bldgres_", x))
  
  #flag
  bldgres_data$mobile <- ifelse(bldgres_data$structure_dscr == "Mobile Home", 1, 0)
  print(paste0("mobile home count for year ",x))
  print(table(bldgres_data$mobile))

  #closet out
  assign(paste0("bldgres_", x), bldgres_data)
  rm(bldgres_data)
}


#checking things out for the bld number problem
temp <- bldgres_2018[bldgres_2018$HCAD_NUM %in% bldgres_2018$HCAD_NUM[(bldgres_2018$bld_num > 1)],] 

for(x in 1:10){
  print(summary(temp$gross_ar[temp$bld_num == x]))
}

temp <- subset(temp, select = c(HCAD_NUM, bld_num, gross_ar))
length(unique(temp$HCAD_NUM)) 
length(unique(bldgres_2016$HCAD_NUM)) 
length(unique(bldgres_2018$HCAD_NUM)) 

#of these, almost all the building no. 1 buildings are the largest
temp <- subset(temp, bld_num < 11)
temp <- temp |> 
  pivot_wider(names_from = bld_num, values_from = gross_ar)
colnames(temp) <- paste0("a", colnames(temp))
table(temp$a1 >= temp$a2)
table(temp$a1 >= temp$a3) 
table(temp$a1 >= temp$a4) 
table(temp$a1 >= temp$a5)
table(temp$a1 >= temp$a6)
table(temp$a1 >= temp$a7)
table(temp$a1 >= temp$a8)
table(temp$a1 >= temp$a9)
table(temp$a1 >= temp$a10)

#and a lot of those sizes are pretty close
temp2 <- subset(temp, a1 < a2)
summary(temp2$a1 - temp2$a2)

rm(temp, temp2)


#deciding to just use bld 1
for(x in 2016:2018){
  cat("\n")
  cat("\n")
  print(paste0("running for year ", x))
  
  #setup
  bldgres_data <- get(paste0("bldgres_", x))
  print(paste0("pre-subset HCAD count is: ", length(unique(bldgres_data$HCAD_NUM))))
  print(paste0("pre-subset observation count is: ", nrow(bldgres_data)))
  count <- nrow(bldgres_data)
  drop_list <- c(drop_list, paste0("observations for all 'building' numbers in bldgres ", x, ": ", nrow(bldgres_data)))

  # privileging building 1
  bldgres_data <- bldgres_data %>%
    group_by(HCAD_NUM) %>%
    mutate(num_bldgs = max(bld_num, na.rm=TRUE)) %>% ungroup()

  bldgres_data <- subset(bldgres_data, bld_num == 1)
  bldgres_data <- subset(bldgres_data, select = c(HCAD_NUM, date_erected, yr_remodel, dscr, num_bldgs, dpr_val, accrued_depr_pct, mobile)) %>% 
                          rename(yr_built = date_erected, qual = dscr, depr_pct = accrued_depr_pct)
  
  print(paste0("bld 1 HCAD count is: ", length(unique(bldgres_data$HCAD_NUM))))
  print(paste0("bld 1 observation count is: ", nrow(bldgres_data)))
  
  drop_list <- c(drop_list, paste0("observations for only building #1 in bldgres  ", x, ": ", nrow(bldgres_data)))
  drop_list <- c(drop_list, paste0("dropped values for additional buildings in bldgres  ", x, ": ", count - nrow(bldgres_data)))
  
  
  #drop dupes
  bldgres_data <- unique(bldgres_data) 
  print(paste0("final HCAD count is: ", length(unique(bldgres_data$HCAD_NUM))))
  print(paste0("final observation count is: ", length(unique(bldgres_data))))
  
  #closet out
  assign(paste0("bldgres_", x), bldgres_data)
  rm(bldgres_data)
}

drop_list



# ******************************
# 6. Joining ----


#check
for(x in 2016:2018){
  cat("\n")
  print(x)
  temp <- get(paste0("bldgres_", x))
  print(length(unique(temp$HCAD_NUM)) == nrow(temp))
  
  temp <- get(paste0("realacct_", x))
  print(length(unique(temp$HCAD_NUM)) == nrow(temp))

  
}



#rename
for(x in 2016:2018){
  temp <- get(paste0("bldgres_", x))
  temp <- temp %>% rename_at(vars(-HCAD_NUM), ~ paste0(., "_", x))
  assign(paste0("bldgres_", x), temp)
  
  temp <- get(paste0("realacct_", x))
  temp <- temp %>% rename_at(vars(-HCAD_NUM), ~ paste0(., "_", x))
  assign(paste0("realacct_", x), temp)

  
}

# List of dataframe names
df_names <- c(paste0("bldgres_", 2016:2018),
              paste0("realacct_", 2016:2018))

# Load all dataframes into a list
df_list <- lapply(df_names, function(name) get(name))

#setup
hcad_res <- addbook

# Join dataframes using full_join and rename columns
for (i in 1:length(df_list)) {
  hcad_res <- left_join(hcad_res, df_list[[i]], by = "HCAD_NUM")
  
}

rm(df_names, df_list)

# re-oreders column names
front_list <- c(colnames(addbook))
back_list <- c(colnames(hcad_res)) %>% setdiff(front_list)
back_list <- back_list[order(as.numeric(gsub("[^0-9]", "", back_list)))]
hcad_res <- hcad_res[, c(front_list, back_list)]
rm(front_list, back_list)


# final dupe check on length = observations
length(unique(hcad_res$HCAD_NUM))
nrow(hcad_res)




# ****************************** 
# new owners - flags and counts

temp <- hcad_res

#new owner by post harvey
hcad_res$new_own_postharvey <- as.Date(hcad_res$new_own_dt_2018, format = "%m/%d/%Y") > as.Date("2017-08-17")
summary(hcad_res$new_own_postharvey)

# Tenure calculation
hcad_res$tenure <- (as.numeric(as.Date(hcad_res$new_own_dt_2016, format = "%m/%d/%Y") - as.Date("2017-08-17")) / 365.25) * -1
summary(hcad_res$tenure)
class(hcad_res$tenure)

# Set sale_outcome 
hcad_res$sale_outcome <- ifelse(hcad_res$company_2018 == TRUE, "COMPANY", ifelse(hcad_res$stringdist_owner_2018 > 3, "LANDLORD", "OWNER-OCCUPIED"))
hcad_res$sale_outcome[hcad_res$other_owner_2018 == TRUE] <- "OTHER OWNER"
hcad_res$sale_outcome <- ifelse(hcad_res$new_own_postharvey != TRUE, "NO-SALE", hcad_res$sale_outcome)

hcad_res$sale_outcome <- factor(hcad_res$sale_outcome, levels =  c("OTHER OWNER", "NO-SALE", "OWNER-OCCUPIED", "LANDLORD", "COMPANY"))
table(hcad_res$sale_outcome, useNA = "always")

# Original Owner
hcad_res$og_ownerstat <- ifelse(hcad_res$company_2016 == TRUE, "COMPANY", ifelse(hcad_res$stringdist_owner_2016 > 3, "LANDLORD", "OWNER-OCCUPIED"))
hcad_res$og_ownerstat[hcad_res$other_owner_2016 == TRUE] <- "OTHER OWNER"
hcad_res$og_ownerstat <- factor(hcad_res$og_ownerstat, levels =  c("OTHER OWNER", "OWNER-OCCUPIED", "LANDLORD", "COMPANY"))

# checks
table(hcad_res$og_ownerstat, useNA = "always")
with(hcad_res, table(og_ownerstat, is.na(owner_2016), useNA = "always"))



# fix for purchases in 2017 prior to Harvey
# id cases
hcad_res$new_own_dt_2016 <- as.Date(hcad_res$new_own_dt_2016, format = "%m/%d/%Y")
hcad_res$new_own_dt_2017 <- as.Date(hcad_res$new_own_dt_2017, format = "%m/%d/%Y")
hcad_res$new_own_dt_2018 <- as.Date(hcad_res$new_own_dt_2018, format = "%m/%d/%Y")

hcad_res$preharvey_sale <- ifelse(hcad_res$new_own_dt_2018 > hcad_res$new_own_dt_2016 &
                                    hcad_res$new_own_dt_2018 <= as.Date("2017-08-17"), TRUE, FALSE)

summary(hcad_res$new_own_dt_2018[hcad_res$preharvey_sale == TRUE])
summary(hcad_res$new_own_dt_2018[hcad_res$preharvey_sale == FALSE])

ggplot(hcad_res, aes(x = as.numeric(format(as.Date(new_own_dt_2018, format = "%m/%d/%Y"), "%Y")), 
                     fill = as.factor(preharvey_sale))) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1) +
  scale_fill_manual(values = c("TRUE" = "blue", "FALSE" = "red")) +
  labs(title = "Histogram of Year of Ownership by Pre-Harvey Sale Status",
       x = "Year of Ownership", 
       y = "Frequency") +
  theme_minimal() +
  theme(legend.title = element_blank())

# Mid Owner
hcad_res$mid_ownerstat <- ifelse(hcad_res$company_2017 == TRUE, "COMPANY", ifelse(hcad_res$stringdist_owner_2017 > 3, "LANDLORD", "OWNER-OCCUPIED"))
hcad_res$mid_ownerstat[hcad_res$other_owner_2017 == TRUE] <- "OTHER OWNER"
hcad_res$mid_ownerstat <- factor(hcad_res$mid_ownerstat, levels =  c("OTHER OWNER", "OWNER-OCCUPIED", "LANDLORD", "COMPANY"))
table(hcad_res$mid_ownerstat, useNA = "always")

# new owner stat variable
hcad_res$preharv_ownerstat <- ifelse(hcad_res$preharvey_sale == TRUE, 
                                     as.character(hcad_res$mid_ownerstat), 
                                     as.character(hcad_res$og_ownerstat))
hcad_res$preharv_ownerstat <- factor(hcad_res$preharv_ownerstat, levels =  c("OTHER OWNER", "OWNER-OCCUPIED", "LANDLORD", "COMPANY"))
print(with(hcad_res, table(og_ownerstat, preharv_ownerstat)))

table(hcad_res$preharv_owner)

# other new variables
hcad_res$preharv_owner <- ifelse(hcad_res$preharvey_sale == TRUE, hcad_res$owner_2017, hcad_res$owner_2016)
hcad_res$preharv_prob_white <- ifelse(hcad_res$preharvey_sale == TRUE, hcad_res$prob_white_2017, hcad_res$prob_white_2016)
hcad_res$preharv_prob_black <- ifelse(hcad_res$preharvey_sale == TRUE, hcad_res$prob_black_2017, hcad_res$prob_black_2016)
hcad_res$preharv_prob_hispanic <- ifelse(hcad_res$preharvey_sale == TRUE, hcad_res$prob_hispanic_2017, hcad_res$prob_hispanic_2016)
hcad_res$preharv_prob_asian <- ifelse(hcad_res$preharvey_sale == TRUE, hcad_res$prob_asian_2017, hcad_res$prob_asian_2016)
hcad_res$preharv_race <- ifelse(hcad_res$preharvey_sale == TRUE, hcad_res$race_2017, hcad_res$race_2016)
hcad_res$preharv_tenure <- ifelse(hcad_res$preharvey_sale == TRUE, 
                                  ((as.numeric(hcad_res$new_own_dt_2018 - as.Date("2017-08-17")) / 365.25) * -1), hcad_res$tenure)


summary(hcad_res$preharv_tenure)
summary(hcad_res$tenure)


hcad_res$tenure <- (as.numeric(as.Date(hcad_res$new_own_dt_2016, format = "%m/%d/%Y") - as.Date("2017-08-17")) / 365.25) * -1



# Verify the result
with(hcad_res, table(sale_outcome, new_own_postharvey, useNA = "always"))
with(hcad_res, table(sale_outcome, company_2018, useNA = "always"))
with(hcad_res, table(sale_outcome, landlord_2018, useNA = "always"))

with(hcad_res, table(og_ownerstat, sale_outcome, useNA = "always"))
with(hcad_res, prop.table(table(og_ownerstat, sale_outcome), margin = 1)) * 100

with(hcad_res, table(preharv_ownerstat, sale_outcome, useNA = "always"))
with(hcad_res, prop.table(table(preharv_ownerstat, sale_outcome), margin = 1)) * 100




# get names - basic 
hcad_res <- hcad_res %>%
  mutate(preharv_lastname = word(preharv_owner, 1),
         preharv_firstname = word(preharv_owner, 2, -1))

# Handle multi-owner & middle names
hcad_res <- hcad_res %>%
  mutate(
    preharv_firstname1 = ifelse(str_detect(preharv_firstname, "&"), sub("\\s*&.*", "", preharv_firstname), preharv_firstname),
    preharv_middlename1 = ifelse(str_count(preharv_firstname1, "\\s") > 0, word(preharv_firstname1, -1), NA),
    preharv_firstname1 = ifelse(str_count(preharv_firstname1, "\\s") > 0, sub("\\s*\\w+\\s*$", "", preharv_firstname1), preharv_firstname1),
    preharv_firstname2 = ifelse(str_detect(preharv_firstname, "&"), sub(".*&\\s*", "", preharv_firstname), NA),
    preharv_middlename2 = ifelse(str_count(preharv_firstname2, "\\s") > 0, word(preharv_firstname2, -1), NA),
    preharv_firstname2 = ifelse(str_count(preharv_firstname2, "\\s") > 0, sub("\\s*\\w+\\s*$", "", preharv_firstname2), preharv_firstname2)
  ) %>%
  dplyr::select(-preharv_firstname)

# Remove names for companies or other owners
hcad_res$preharv_lastname[hcad_res$og_ownerstat == "COMPANY" | hcad_res$og_ownerstat == "OTHER OWNER"] <- NA
hcad_res$preharv_firstname1[hcad_res$og_ownerstat == "COMPANY" | hcad_res$og_ownerstat == "OTHER OWNER"] <- NA
hcad_res$preharv_firstname1[hcad_res$preharv_firstname1 == ""] <- NA
hcad_res$preharv_firstname2[hcad_res$og_ownerstat == "COMPANY" | hcad_res$og_ownerstat == "OTHER OWNER"] <- NA
hcad_res$preharv_firstname2[hcad_res$preharv_firstname2 == ""] <- NA
hcad_res$preharv_middlename1[hcad_res$og_ownerstat == "COMPANY" | hcad_res$og_ownerstat == "OTHER OWNER"] <- NA
hcad_res$preharv_middlename2[hcad_res$og_ownerstat == "COMPANY" | hcad_res$og_ownerstat == "OTHER OWNER"] <- NA

hcad_res$last_matchflag <-  ifelse(str_detect(hcad_res$owner_2018, fixed(hcad_res$preharv_lastname, ignore_case = TRUE)), 1, 0)
hcad_res$first_matchflag <- ifelse(str_detect(hcad_res$owner_2018, fixed(hcad_res$preharv_firstname1, ignore_case = TRUE)) |
                                     str_detect(hcad_res$owner_2018, fixed(hcad_res$preharv_firstname2, ignore_case = TRUE))  , 1, 0)


print(colnames(hcad_res))
print(with(hcad_res, table(last_matchflag, first_matchflag)))




# 8. Saving out ----

save(hcad_res, file = paste0(data_output, "/02_residential data.RData"))
writeLines(as.character(drop_list), paste0(user, "/Documents/Research/Rebuilt/Process/Drop List/02_drop_list.txt"))

#time spent
(as.POSIXct(Sys.time()) - as.POSIXct(start_time))


