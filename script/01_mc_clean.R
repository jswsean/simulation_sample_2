# first, we load all the required libraries
library(tidyverse)
library(haven)

# importing the datasets
sak_feb20 <- read_dta('data/sak_feb2020_mc.dta')  # importing Sakernas Feb 2020 data, for baseline comparison
sak_aug20 <- read_dta('data/sak_aug2020_mc.dta')  # importing Sakernas Aug 2020 data, for HiFy Jul-20 comparison
hify <- read_dta('data/compiled_resp2nd.dta')     # importing R5 (Jul-20) HiFy data

# Data Preparation ----  

# only using select columns in the Sak datasets to speed up process
sak_feb20 <- sak_feb20 %>% 
  select(weight:agegroup, psu, hh_id, work, strata) %>% 
  rename(prov_code = kode_prov)

sak_aug20 <- sak_aug20 %>% 
  select(weight:agegroup, psu, hh_id, work, strata) %>% 
  rename(prov_code = kode_prov)

# removing 1 obs with nan province in the hify data 
hify <- hify %>% 
  filter(!is.na(province))


# Sakernas Weight Corrections -----

# Sakernas Feb 2020 ----

# obtain by-province n summary from hify data for weight corrections
hify_prov <- hify %>% 
  group_by(province) %>% 
  summarise(hify_n = n()) %>% 
  rename(prov_code = province) %>% 
  select(prov_code)

# calculating the correction factors for feb-20
sak_feb20_n_strata <- sak_feb20 %>% 
  group_by(strata) %>% 
  summarise(sak_n_full = n()) # this is to obtain full by-strata n

sak_feb20_cf <- sak_feb20 %>% 
  inner_join(hify_prov, by = "prov_code") %>% # filtering for hify provinces only
  group_by(strata) %>% 
  summarise(sak_n_hify = n()) %>%  # to get the hify-only, by-strata n
  inner_join(sak_feb20_n_strata, by = 'strata') %>% 
  mutate(
    corr_factor = sak_n_full / sak_n_hify   # which gives us the correction factor for each strata
  ) %>% 
  ungroup()

# we then merge the cf data back to original sak feb 20 data
sak_feb20 <- sak_feb20 %>% 
  inner_join(sak_feb20_cf, by = "strata") %>% 
  mutate(
    weight_2 = corr_factor * weight # which we will use for the MC simulation
  ) %>% 
  inner_join(hify_prov, by = "prov_code") # selecting only obs in the hify provinces

# indicating eligible individuals for the simulation 
sak_feb20 <- sak_feb20 %>% 
  mutate(eligible = ifelse(between(age, 18, 64), 1, 0)) # 18-64 is chosen because of hify sampling chars


# Sakernas Aug 2020 -----

# doing the same process for aug 2020 sakernas
sak_aug20_n_strata <- sak_aug20 %>% 
  group_by(strata) %>% 
  summarise(sak_n_full = n()) # this is to obtain full by-strata n

sak_aug20_cf <- sak_aug20 %>% 
  inner_join(hify_prov, by = "prov_code") %>% # filtering for hify provinces only
  group_by(strata) %>% 
  summarise(sak_n_hify = n()) %>%  # to get the hify-only, by-strata n
  inner_join(sak_aug20_n_strata, by = 'strata') %>% 
  mutate(
    corr_factor = sak_n_full / sak_n_hify   # which gives us the correction factor for each strata
  ) %>% 
  ungroup()

sak_aug20 <- sak_aug20 %>% 
  inner_join(sak_aug20_cf, by = "strata") %>% 
  mutate(
    weight_2 = corr_factor * weight # which we will use for the MC simulation
  ) %>% 
  inner_join(hify_prov, by = "prov_code") # selecting only obs in the hify provinces

sak_aug20 <- sak_aug20 %>% 
  mutate(eligible = ifelse(between(age, 18, 64), 1, 0)) # 18-64 is chosen because of hify sampling chars



# exporting the analytic dataset to csv
write_csv(sak_feb20, 'data/sak_feb20_analytic.csv')
write_csv(sak_aug20, 'data/sak_aug20_analytic.csv')

