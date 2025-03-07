
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
    'sandwich',
    'lme4',
    'car',
    'glmmTMB',
    'MASS',
    'broom.mixed',
    'jtools',
    'ggplot2',
    'huxtable',
    'viridis',
    'caret',
    'lmtest',
    'nnet',
    'broom'
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
paper_output <- paste0(user, "/Documents/Research/Rebuilt/Output")

#keep an error report
drop_list <- list()

#set seed
set.seed(123)

#turn off scientific notation
options(scipen = 999)


# ******************
# 2. Load data ----

load(file = paste0(data_output, "/05_census_simple.RData"))

hcad <- hcad_simple


# ******************
# 3. Review ----

#just looking over this
colnames(hcad)
head(hcad)


# create the analytic sample
hcad_runsample <- hcad
drop_list <- c(drop_list, paste0("Initial analytic sample observation count: ", nrow(hcad_runsample)))
drop_count <- nrow(hcad_runsample)



# ******************
# 4. Probably needs robustness checks  ----


# false positives on names
with(hcad_runsample, table((first_matchflag == 1 & last_matchflag == 1), sale_outcome))
hcad_runsample$sale_outcome[hcad_runsample$last_matchflag == 1 & hcad_runsample$first_matchflag == 1] <- "NO-SALE"


hcad_runsample <- hcad_runsample %>%
  mutate(
    flood_binary = factor(ifelse(min_flood_hu10 > 0, "flood", "safe"), levels = c("safe", "flood")),
    white_bi = ifelse(preharv_prob_white > .5 & !is.na(race_2016), 1, 0),
    neigh_white = ifelse(pc_white_acs2012_2016 > .5 & pc_white_acs2012_2016 > pc_black_acs2012_2016 & pc_white_acs2012_2016 > pc_latino_acs2012_2016 & pc_white_acs2012_2016 > pc_asian_acs2012_2016, 1, 0)
)


# ******************
# 5. Major Model Decisions  ----


# state class problem
drop_count <- nrow(hcad_runsample)
with(hcad_runsample, table(state_class, sale_outcome))
hcad_runsample <- subset(hcad_runsample, state_class == "A1")

drop_list <- c(drop_list, paste0("Drop to single family residential only: ", drop_count- nrow(hcad_runsample)))
drop_list <- c(drop_list, paste0("Total after drop: ", nrow(hcad_runsample)))


# origin condition outcome
drop_count <- nrow(hcad_runsample)
with(hcad_runsample, table(og_ownerstat, sale_outcome))
hcad_runsample <- subset(hcad_runsample, og_ownerstat == "OWNER-OCCUPIED")

drop_list <- c(drop_list, paste0("Origin condition set to owner occupied; dropped observations: ", drop_count- nrow(hcad_runsample)))
drop_list <- c(drop_list, paste0("Total after drop: ", nrow(hcad_runsample)))


# outcome change
table(hcad_runsample$sale_outcome)
hcad_runsample <- subset(hcad_runsample, sale_outcome != "OTHER OWNER")
hcad_runsample$sale_outcome <- factor(hcad_runsample$sale_outcome, levels =  c("NO-SALE", "OWNER-OCCUPIED", "LANDLORD", "COMPANY"))


drop_list <- c(drop_list, paste0("Drop other owner: ", drop_count- nrow(hcad_runsample)))
drop_list <- c(drop_list, paste0("Total after drop: ", nrow(hcad_runsample)))



# Missingness
drop_count <- nrow(hcad_runsample)
hcad_runsample <- subset(hcad_runsample, !is.na(company_2018) & !is.na(preharv_prob_white) & !is.na(tot_appr_val_2016) & !is.na(flood_100) &
                 !is.na(pc_white_acs2012_2016) & !is.na(yr_built_2016) & !is.na(min_flood_hu10) & !is.na(tenure) & !is.na(bld_ar_2016) &
                 !is.na(pc_college_acs2012_2016) & !is.na(pc_nonmove_1yr_acs2012_2016) & !is.na(pc_pov_acs2012_2016) & 
                 !is.na(mfaminc_acs2012_2016) & !is.na(med_hval_acs2012_2016) & !is.na(med_yrblt_acs2012_2016) & !is.na(pc_husocc_own_acs2012_2016)
)

drop_list <- c(drop_list, paste0("Missing data dropped observations: ", drop_count- nrow(hcad_runsample)))
drop_list <- c(drop_list, paste0("Total after drop: ", nrow(hcad_runsample)))
print(drop_list)


# ******************
# 6. Minor Model Decisions & recoding ----



# some recoding
hcad_runsample <- hcad_runsample %>%
  mutate(
    company_2018 = as.factor(company_2018),
    og_ownerstat = preharv_ownerstat,
    preharv_race = relevel(factor(preharv_race), ref = "white"),
    preharv_race_refw = relevel(factor(preharv_race), ref = "white")
   )


# pre scale mutate || 2016/2017 combo version
hcad_runsample <- hcad_runsample %>%
  mutate(
    appr_201610k = tot_appr_val_2016 / 10000,
    house_age = 2017 - yr_built_2016
  )


# if any sale outcome
hcad_runsample$sale_outcome_any <- ifelse(hcad_runsample$sale_outcome == "NO-SALE", 0, 1)



# race homogeneity 
hcad_runsample$race_match <- ifelse(is.na(hcad_runsample$preharv_race), NA,
                                    ifelse((hcad_runsample$pc_white_acs2012_2016 > .5 & hcad_runsample$preharv_prob_white > .5) |
                                             (hcad_runsample$pc_black_acs2012_2016 > .5 & hcad_runsample$preharv_prob_black > .5) |
                                             (hcad_runsample$pc_asian_acs2012_2016 > .5 & hcad_runsample$preharv_prob_asian > .5) |
                                             (hcad_runsample$pc_latino_acs2012_2016 > .5 & hcad_runsample$preharv_prob_hispanic > .5),
                                           1,0)
)
table(hcad_runsample$race_match)

# race heterogeneity 
hcad_runsample$race_miss <- ifelse(is.na(hcad_runsample$preharv_race), NA,
                                    ifelse((hcad_runsample$pc_white_acs2012_2016 <= .5 & hcad_runsample$preharv_prob_white > .5) |
                                             (hcad_runsample$pc_black_acs2012_2016 <= .5 & hcad_runsample$preharv_prob_black > .5) |
                                             (hcad_runsample$pc_asian_acs2012_2016 <= .5 & hcad_runsample$preharv_prob_asian > .5) |
                                             (hcad_runsample$pc_latino_acs2012_2016 <= .5 & hcad_runsample$preharv_prob_hispanic > .5),
                                           1,0)
)
table(hcad_runsample$race_miss)


#interaction
hcad_runsample$race_flood_int <- hcad_runsample$race_match * ifelse(hcad_runsample$flood_binary == "flood", 1, 0)
table(hcad_runsample$race_flood_int)



#define neighborhood race
hcad_runsample <- hcad_runsample %>%
  mutate(neighborhood_race = case_when(
    pc_white_acs2012_2016 > .5 & pc_white_acs2012_2016 > pc_black_acs2012_2016 & pc_white_acs2012_2016 > pc_latino_acs2012_2016 & pc_white_acs2012_2016 > pc_asian_acs2012_2016 ~ "White",
    pc_black_acs2012_2016 > .5 & pc_black_acs2012_2016 > pc_white_acs2012_2016 & pc_black_acs2012_2016 > pc_latino_acs2012_2016 & pc_black_acs2012_2016 > pc_asian_acs2012_2016 ~ "Black",
    pc_latino_acs2012_2016 > .5 & pc_latino_acs2012_2016 > pc_white_acs2012_2016 & pc_latino_acs2012_2016 > pc_black_acs2012_2016 & pc_latino_acs2012_2016 > pc_asian_acs2012_2016 ~ "Hispanic",
    pc_asian_acs2012_2016 > .5 & pc_asian_acs2012_2016 > pc_white_acs2012_2016 & pc_asian_acs2012_2016 > pc_black_acs2012_2016 & pc_asian_acs2012_2016 > pc_latino_acs2012_2016 ~ "Asian",
    TRUE ~ "Other"  # Assign "Other" if no race group has more than 0.5 dominance
  )) %>%
  mutate(neighborhood_race = factor(neighborhood_race, levels = c("White", "Black", "Hispanic", "Asian", "Other")))


with(hcad_runsample, table(neighborhood_race, neigh_white))


# ******************
# 7. Scaling  ----


nrow(hcad_runsample)

# scaling || 2016/2017 combo version
hcad_runsample <- hcad_runsample %>%
  mutate(
    house_age = scale(house_age),
    tenure = scale(preharv_tenure),
    appr_201610k = scale(appr_201610k),
    bld_ar_2016 = scale(bld_ar_2016),
    yr_built_2016 = scale(yr_built_2016),
    min_flood_hu10 = scale(min_flood_hu10),
    mfaminc_acs2012_2016 = scale(mfaminc_acs2012_2016),
    med_hval_acs2012_2016 = scale(med_hval_acs2012_2016),
    med_yrblt_acs2012_2016 = scale(med_yrblt_acs2012_2016),
    pc_latino_acs2012_2016 = scale(pc_latino_acs2012_2016),
    pc_white_acs2012_2016 = scale(pc_white_acs2012_2016),
    pc_black_acs2012_2016 = scale(pc_black_acs2012_2016),
    pc_college_acs2012_2016 = scale(pc_college_acs2012_2016),
    pc_husocc_own_acs2012_2016 = scale(pc_husocc_own_acs2012_2016),
    pc_nonmove_1yr_acs2012_2016 = scale(pc_nonmove_1yr_acs2012_2016),
    pc_pov_acs2012_2016 = scale(pc_pov_acs2012_2016),
    prob_hispanic_2016 = scale(prob_hispanic_2016),
    prob_white_2016 = scale(prob_white_2016),
    prob_black_2016 = scale(prob_black_2016)
  )

nrow(hcad_runsample)

# ******************
# 8. Analysis | Logistic ----


# summary statistic
sum_stat1 <- as.data.frame(with(hcad_runsample, table(sale_outcome_any, neigh_white, flood_binary)))



# Fit  logistic regression models for sale_outcome
model_glm1       <- glm(sale_outcome_any ~ flood_binary + flood_100 +
                          tenure + appr_201610k + bld_ar_2016 + house_age +  
                          med_hval_acs2012_2016 + med_yrblt_acs2012_2016 + mfaminc_acs2012_2016 + 
                          pc_college_acs2012_2016 + pc_nonmove_1yr_acs2012_2016 + pc_husocc_own_acs2012_2016 + pc_pov_acs2012_2016,
                        data = hcad_runsample, family = binomial(link = "logit"))


# Fit  logistic regression models for sale_outcome
model_glm2       <- glm(sale_outcome_any ~ flood_binary + flood_100 + white_bi + 
                          tenure + appr_201610k + bld_ar_2016 + house_age +  
                          neigh_white + med_hval_acs2012_2016 + med_yrblt_acs2012_2016 + mfaminc_acs2012_2016 + 
                          pc_college_acs2012_2016 + pc_nonmove_1yr_acs2012_2016 + pc_husocc_own_acs2012_2016 + pc_pov_acs2012_2016,
                         data = hcad_runsample, family = binomial(link = "logit"))


# Fit  logistic regression models for sale_outcome
model_glm3       <- glm(sale_outcome_any ~ neigh_white*flood_binary + flood_100 + white_bi +
                          tenure + appr_201610k + bld_ar_2016 + house_age +  
                          med_hval_acs2012_2016 + med_yrblt_acs2012_2016 + mfaminc_acs2012_2016 + 
                          pc_college_acs2012_2016 + pc_nonmove_1yr_acs2012_2016 + pc_husocc_own_acs2012_2016 + pc_pov_acs2012_2016, 
                         data = hcad_runsample, family = binomial(link = "logit"))

# Fit  logistic regression models for sale_outcome
model_glm3b       <- glm(sale_outcome_any ~ neigh_white*white_bi + neigh_white*flood_binary  + white_bi*flood_binary + flood_100 +
                           tenure + appr_201610k + bld_ar_2016 + house_age +  
                           med_hval_acs2012_2016 + med_yrblt_acs2012_2016 + mfaminc_acs2012_2016 + 
                           pc_college_acs2012_2016 + pc_nonmove_1yr_acs2012_2016 + pc_husocc_own_acs2012_2016 + pc_pov_acs2012_2016, 
                        data = hcad_runsample, family = binomial(link = "logit"))



# Fit  logistic regression models for sale_outcome
model_glm3c       <- glm(sale_outcome_any ~ neigh_white*white_bi*flood_binary + flood_100 +
                           tenure + appr_201610k + bld_ar_2016 + house_age +  
                           med_hval_acs2012_2016 + med_yrblt_acs2012_2016 + mfaminc_acs2012_2016 + 
                           pc_college_acs2012_2016 + pc_nonmove_1yr_acs2012_2016 + pc_husocc_own_acs2012_2016 + pc_pov_acs2012_2016, 
                        data = hcad_runsample, family = binomial(link = "logit"))



# #summarizing

summary(model_glm1)
summary(model_glm2)
summary(model_glm3)
summary(model_glm3b)
summary(model_glm3c)

exp(coef(model_glm1))
exp(coef(model_glm2))
exp(coef(model_glm3))
exp(coef(model_glm3b))
exp(coef(model_glm3c))


# ******************
# 9. graphics | logistic ----


# Extract summaries and tidy logistic models
summary_glm1 <- tidy(model_glm1) %>% mutate(model = "model 1")
summary_glm2 <- tidy(model_glm2) %>% mutate(model = "model 2")
summary_glm3 <- tidy(model_glm3) %>% mutate(model = "model 3")
summary_glm3b <- tidy(model_glm3b) %>% mutate(model = "model 3b")
summary_glm3c <- tidy(model_glm3c) %>% mutate(model = "model 3c")


# Combine summaries for logistic regression models
combined_summary_glm <- bind_rows(
  summary_glm1, summary_glm2, summary_glm3, 
  summary_glm3b, summary_glm3c
)

# Create a summary table with exponentiated estimates and confidence intervals for logistic regression
summary_glm <- combined_summary_glm %>%
  dplyr::mutate(
    exp_estimate = exp(estimate),
    conf.low = exp(estimate - 1.96 * std.error),
    conf.high = exp(estimate + 1.96 * std.error)
  ) %>%
  dplyr::select(term, model, exp_estimate, p.value, conf.low, conf.high)

print(summary_glm, n = "INF")

summary_glm <- summary_glm %>%
  mutate(term = case_when(
    term == "flood_binaryflood" ~ "Flood Exposure",
    term == "flood_100" ~ "Floodplain 1%",
    term == "appr_201610k" ~ "Appraisal Value\u2020",
    term == "bld_ar_2016" ~ "Building Area\u2020", 
    term == "house_age" ~ "House Age\u2020",  
    term == "med_hval_acs2012_2016" ~ "Median Housing Value\u2020", 
    term == "med_yrblt_acs2012_2016" ~ "Median Year Built\u2020",  
    term == "tenure" ~ "Pre-Harvey Tenure\u2020",
    term == "neigh_white" ~ "Neighborhood: White",
    term == "white_bi" ~ "Individual: White",
    term == "neigh_white:white_bi" ~ "Neighborhood: White*Individual: White",
    term == "white_bi:flood_binaryflood" ~ "Individual: White*Flood Exposure",
    term == "neigh_white:flood_binaryflood" ~ "Neighborhood: White*Flood Exposure",
    term == "mfaminc_acs2012_2016" ~ "Median Family Income\u2020",
    term == "pc_college_acs2012_2016" ~ "Percent College Educated\u2020",
    term == "pc_nonmove_1yr_acs2012_2016" ~ "Percent Residence 1+ Year\u2020",
    term == "pc_husocc_own_acs2012_2016" ~ "Percent Owner Occupied Housing\u2020",
    term == "pc_pov_acs2012_2016" ~ "Percent Poverty\u2020",
    term == "neigh_white:white_bi:flood_binaryflood" ~ "Neighborhood: White*Individual: White*Flood Exposure",
    term == "(Intercept)" ~ "Intercept",
    TRUE ~ term
  ))

summary_glm_main <- subset(summary_glm, model == "model 3")

# Reorder terms as done for the multinomial models
ordered_terms <- rev(c(
  "Flood Exposure", "Floodplain 1%", 
  
  "Individual: White", "Pre-Harvey Tenure\u2020",  "Appraisal Value\u2020", "Building Area\u2020", 
  "House Age\u2020", "Year Built\u2020", 
  
  "Neighborhood: White", "Median Housing Value\u2020", 
  "Median Year Built\u2020", "Median Family Income\u2020", "Percent College Educated\u2020", 
  "Percent Residence 1+ Year\u2020", "Percent Owner Occupied Housing\u2020", "Percent Poverty\u2020",
  
  "Neighborhood: White*Individual: White",
  "Neighborhood: White*Flood Exposure", "Individual: White*Flood Exposure", "Neighborhood: White*Individual: White*Flood Exposure", "Intercept"
))

summary_glm$term <- factor(summary_glm$term, levels = ordered_terms)


# Filter out the Intercept before plotting for each individual model
gg_glm1 <- ggplot(subset(summary_glm, model == "model 1" & term != "Intercept"), aes(x = term, y = exp_estimate)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  coord_flip() +
  labs(title = "Logistic Model 1: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)") +
  theme_minimal()

gg_glm1

gg_glm2 <- ggplot(subset(summary_glm, model == "model 2" & term != "Intercept"), aes(x = term, y = exp_estimate)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  coord_flip() +
  labs(title = "Logistic Model 2: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)") +
  theme_minimal()

gg_glm2

gg_glm3 <- ggplot(subset(summary_glm, model == "model 3" & term != "Intercept"), aes(x = term, y = exp_estimate)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  coord_flip() +
  labs(title = "Logistic Model 3: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

gg_glm3


gg_glm3b <- ggplot(subset(summary_glm, model == "model 3b" & term != "Intercept"), aes(x = term, y = exp_estimate)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  coord_flip() +
  labs(title = "Logistic Model 3b: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

gg_glm3b


gg_glm3c <- ggplot(subset(summary_glm, model == "model 3c" & term != "Intercept"), aes(x = term, y = exp_estimate)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  coord_flip() +
  labs(title = "Logistic Model 3b: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)") +
  theme_minimal()

gg_glm3c


# combo plot
gg_glm_combo <- ggplot(subset(summary_glm, (model == "model 1" | model == "model 2" | model == "model 3") & term != "Intercept"), 
                       aes(x = term, y = exp_estimate, color = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  coord_flip() +
  labs(title = "Logistic Models: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)",
       color = "Model") +
  theme_minimal()

gg_glm_combo


# alternate combo plot
gg_glm_alt <- ggplot(subset(summary_glm, (model == "model 3" | model == "model 3b" | model == "model 3c")& term != "Intercept"), 
                     aes(x = term, y = exp_estimate, color = model)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  coord_flip() +
  labs(title = "Logistic Models: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)",
       color = "Model") +
  theme_minimal()

gg_glm_alt





# ******************
# 10. Analysis | Multinomial ----


hcad_sales <- subset(hcad_runsample, sale_outcome != "NO-SALE")
hcad_sales$sale_outcome <- factor(hcad_sales$sale_outcome, 
                                      levels = c("OWNER-OCCUPIED", "LANDLORD", "COMPANY"),
                                      labels = c("OWNER-OCCUPIED", "Small Firms", "Large Firms"))

# summary statistic
sum_stat2 <- as.data.frame(with(hcad_sales, table(sale_outcome, neigh_white, flood_binary)))


# Fit multinomial logistic regression models for sale_outcome
model_nom1 <- multinom(sale_outcome ~ flood_binary + flood_100 +
                         tenure + appr_201610k + bld_ar_2016 + house_age +  
                         med_hval_acs2012_2016 + med_yrblt_acs2012_2016 + mfaminc_acs2012_2016 + 
                         pc_college_acs2012_2016 + pc_nonmove_1yr_acs2012_2016 + pc_husocc_own_acs2012_2016 + pc_pov_acs2012_2016,
                       data = hcad_sales, maxit = 500)

model_nom2 <- multinom(sale_outcome ~ flood_binary + flood_100 + white_bi + 
                         tenure + appr_201610k + bld_ar_2016 + house_age +  
                         neigh_white + med_hval_acs2012_2016 + med_yrblt_acs2012_2016 + mfaminc_acs2012_2016 + 
                         pc_college_acs2012_2016 + pc_nonmove_1yr_acs2012_2016 + pc_husocc_own_acs2012_2016 + pc_pov_acs2012_2016,
                       data = hcad_sales, maxit = 500)

model_nom3 <- multinom(sale_outcome ~ neigh_white*flood_binary + flood_100 + white_bi +
                         tenure + appr_201610k + bld_ar_2016 + house_age +  
                         med_hval_acs2012_2016 + med_yrblt_acs2012_2016 + mfaminc_acs2012_2016 + 
                         pc_college_acs2012_2016 + pc_nonmove_1yr_acs2012_2016 + pc_husocc_own_acs2012_2016 + pc_pov_acs2012_2016,
                       data = hcad_sales, maxit = 500)

model_nom3b <- multinom(sale_outcome ~ neigh_white*white_bi + neigh_white*flood_binary  + white_bi*flood_binary + flood_100 +
                          tenure + appr_201610k + bld_ar_2016 + house_age +  
                          med_hval_acs2012_2016 + med_yrblt_acs2012_2016 + mfaminc_acs2012_2016 + 
                          pc_college_acs2012_2016 + pc_nonmove_1yr_acs2012_2016 + pc_husocc_own_acs2012_2016 + pc_pov_acs2012_2016,
                        data = hcad_sales, maxit = 500)


model_nom3c <- multinom(sale_outcome ~ neigh_white*white_bi*flood_binary + flood_100 +
                          tenure + appr_201610k + bld_ar_2016 + house_age +  
                          med_hval_acs2012_2016 + med_yrblt_acs2012_2016 + mfaminc_acs2012_2016 + 
                          pc_college_acs2012_2016 + pc_nonmove_1yr_acs2012_2016 + pc_husocc_own_acs2012_2016 + pc_pov_acs2012_2016,
                       data = hcad_sales, maxit = 500)





# Extract summaries and round numeric columns to 4 decimal places
summary_1_sale <- tidy(model_nom1) %>% mutate(model = "model 1")
summary_2_sale <- tidy(model_nom2) %>% mutate(model = "model 2")
summary_3_sale <- tidy(model_nom3) %>% mutate(model = "model 3")
summary_3b_sale <- tidy(model_nom3b) %>% mutate(model = "model 3b")
summary_3c_sale <- tidy(model_nom3c) %>% mutate(model = "model 3c")


# Combine summaries for sale_outcome models
summary_sale <- bind_rows(
  summary_1_sale, summary_2_sale, summary_3_sale, summary_3b_sale, summary_3c_sale
)

print(summary_sale, n="INF")


# Create a summary table with exponentiated estimates and confidence intervals
summary_sale <- summary_sale %>%
  dplyr::mutate(level = y.level,
                exp_estimate = exp(estimate),
                conf.low = exp(estimate - 1.96 * std.error),
                conf.high = exp(estimate + 1.96 * std.error)) %>%
  dplyr::select(term, model, level, exp_estimate, p.value, conf.low, conf.high)

print(summary_sale, n="INF")


# ******************
# 11. Plots ----


# Rename the terms for consistency with multinomial models
summary_sale <- summary_sale %>%
  mutate(term = case_when(
    term == "flood_binaryflood" ~ "Flood Exposure",
    term == "flood_100" ~ "Floodplain 1%",
    term == "appr_201610k" ~ "Appraisal Value\u2020",
    term == "bld_ar_2016" ~ "Building Area\u2020", 
    term == "house_age" ~ "House Age\u2020",  
    term == "med_hval_acs2012_2016" ~ "Median Housing Value\u2020", 
    term == "med_yrblt_acs2012_2016" ~ "Median Year Built\u2020",  
    term == "tenure" ~ "Pre-Harvey Tenure\u2020",
    term == "neigh_white" ~ "Neighborhood: White",
    term == "white_bi" ~ "Individual: White",
    term == "neigh_white:white_bi" ~ "Neighborhood: White*Individual: White",
    term == "white_bi:flood_binaryflood" ~ "Individual: White*Flood Exposure",
    term == "neigh_white:flood_binaryflood" ~ "Neighborhood: White*Flood Exposure",
    term == "mfaminc_acs2012_2016" ~ "Median Family Income\u2020",
    term == "pc_college_acs2012_2016" ~ "Percent College Educated\u2020",
    term == "pc_nonmove_1yr_acs2012_2016" ~ "Percent Residence 1+ Year\u2020",
    term == "pc_husocc_own_acs2012_2016" ~ "Percent Owner Occupied Housing\u2020",
    term == "pc_pov_acs2012_2016" ~ "Percent Poverty\u2020",
    term == "neigh_white:white_bi:flood_binaryflood" ~ "Neighborhood: White*Individual: White*Flood Exposure",
    term == "(Intercept)" ~ "Intercept",
    TRUE ~ term
  ))

summary_sale_main <- subset(summary_sale, model == "model 3")


# Set term as a factor with the specified order
summary_sale$term <- factor(summary_sale$term, levels = ordered_terms)


# Filter out the Intercept before plotting for each individual model
gg_nom1 <- ggplot(subset(summary_sale, model == "model 1" & term != "Intercept"),  aes(x = term, y = exp_estimate, shape = level, linetype = level)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  coord_flip() +
  labs(title = "Multinomial Model 1: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)") +
  theme_minimal()

gg_nom1

gg_nom2 <- ggplot(subset(summary_sale, model == "model 2" & term != "Intercept"),  aes(x = term, y = exp_estimate, shape = level, linetype = level)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  coord_flip() +
  labs(title = "Multinomial Model 2: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)") +
  theme_minimal()

gg_nom2

gg_nom3 <- ggplot(subset(summary_sale, model == "model 3" & term != "Intercept"), 
                  aes(x = term, y = exp_estimate, shape = level, linetype = level)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  coord_flip() +
  labs(title = "Multinomial Model 3: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

gg_nom3



gg_nom3b <- ggplot(subset(summary_sale, model == "model 3b" & term != "Intercept"), aes(x = term, y = exp_estimate, shape = level, linetype = level)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  coord_flip() +
  labs(title = "Multinomial Model 3b: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)") +
  theme_minimal()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

gg_nom3b


gg_nom3c <- ggplot(subset(summary_sale, model == "model 3c" & term != "Intercept"), aes(x = term, y = exp_estimate, shape = level, linetype = level)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 1) +
  coord_flip() +
  labs(title = "Multinomial Model 3c: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)") +
  theme_minimal()

gg_nom3c



# Combo of core models
gg_nom_combo <- ggplot(subset(summary_sale, (model == "model 1" | model == "model 2" | model == "model 3")  & term != "Intercept"), 
                       aes(x = term, y = exp_estimate, color = model, shape = level, linetype = level)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  coord_flip() +
  labs(title = "Multinomial Models: Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)",
       color = "Model") +
  theme_minimal()

gg_nom_combo


# Combo of alternate models
gg_nom_alt <- ggplot(subset(summary_sale, (model == "model 3" | model == "model 3b" | model == "model 3c")  & term != "Intercept"), 
                       aes(x = term, y = exp_estimate, color = model, shape = level, linetype = level)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 1, linetype = "dashed", size = 1) +
  coord_flip() +
  labs(title = "Multinomial Models (alternate): Exponentiated Coefficients for Sale Outcome",
       x = "Terms",
       y = "Exponentiated Coefficient (Odds Ratio)",
       color = "Model") +
  theme_minimal()

gg_nom_alt




# ******************
# 12. Save out ----

# drop list
writeLines(as.character(drop_list), paste0(user, "/Documents/Research/Rebuilt/Process/Drop List/07_drop_list.txt"))


# tables
write.csv(summary_glm, file = paste0(paper_output, "/Tables/variants/07_logistic regression_variations.csv"), row.names = FALSE)
write.csv(summary_glm_main, file = paste0(paper_output, "/Tables/07_logistic regression.csv"), row.names = FALSE)
write.csv(sum_stat1, file = paste0(paper_output, "/Tables/07_summary stat 1.csv"), row.names = FALSE)


write.csv(summary_sale, file = paste0(paper_output, "/Tables/variants/07_multinom regression_variations.csv"), row.names = FALSE)
write.csv(summary_sale_main, file = paste0(paper_output, "/Tables/07_multinomial regression.csv"), row.names = FALSE)
write.csv(sum_stat2, file = paste0(paper_output, "/Tables/07_summary stat 2.csv"), row.names = FALSE)


# Save the plots (logistic)
ggsave(gg_glm1, file = paste0(paper_output, "/Images/variants/07_expcoef_logistic_plot_glm1.png"), width = 14, height = 6)
ggsave(gg_glm2, file = paste0(paper_output, "/Images/variants/07_expcoef_logistic_plot_glm2.png"), width = 14, height = 6)
ggsave(gg_glm3, file = paste0(paper_output, "/Images/07_expcoef_logistic_plot_glm3.png"), width = 14, height = 9)
ggsave(gg_glm3b, file = paste0(paper_output, "/Images/variants/07_expcoef_logistic_plot_glm3b.png"), width = 14, height = 9)
ggsave(gg_glm3c, file = paste0(paper_output, "/Images/variants/07_expcoef_logistic_plot_glm3c.png"), width = 14, height = 6)
ggsave(gg_glm_combo, file = paste0(paper_output, "/Images/07_expcoef_logistic_plot_glm_combo.png"), width = 14, height = 6)
ggsave(gg_glm_alt, file = paste0(paper_output, "/Images/variants/07_expcoef_logistic_plot_glm_alt.png"), width = 14, height = 6)



#graphs / plots (multinomnom)
ggsave(gg_nom1, file = paste0(paper_output, "/Images/variants/07_expcoef_sale_plot_op1.png"), width = 14, height = 6)
ggsave(gg_nom2, file = paste0(paper_output, "/Images/variants/07_expcoef_sale_plot_op2.png"), width = 14, height = 6)
ggsave(gg_nom3, file = paste0(paper_output, "/Images/07_expcoef_sale_plot_op3.png"), width = 14, height = 9)
ggsave(gg_nom3b, file = paste0(paper_output, "/Images/variants/07_expcoef_sale_plot_op3b.png"), width = 14, height = 9)
ggsave(gg_nom3c, file = paste0(paper_output, "/Images/variants/07_expcoef_sale_plot_op3c.png"), width = 14, height = 6)
ggsave(gg_nom_combo, file = paste0(paper_output, "/Images/07_expcoef_sale_plot_combo.png"), width = 14, height = 6)
ggsave(gg_nom_alt, file = paste0(paper_output, "/Images/variants/07_expcoef_sale_plot_alt.png"), width = 14, height = 6)



#time spent
(as.POSIXct(Sys.time()) - as.POSIXct(start_time))






