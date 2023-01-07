# first, we load all the required libraries
library(tidyverse)
library(haven)

# importing the datasets
sak_feb20 <- read_csv('data/sak_feb20_analytic.csv')  # importing Sakernas Feb 2020 data, for baseline comparison
sak_aug20 <- read_csv('data/sak_aug20_analytic.csv')  # importing Sakernas Aug 2020 data, for HiFy Jul-20 comparison

# Data Preparation ----  

# setting the survey design for sakfeb20
sak_feb20_design <- survey::svydesign(
  id = ~psu,
  data = sak_feb20,
  weight = ~weight_2,
  strata = ~strata
)

# setting the survey design for sakaug20
sak_aug20_design <- survey::svydesign(
  id = ~psu,
  data = sak_aug20,
  weight = ~weight_2,
  strata = ~strata
)

# Simulation ----

# setting the seed
set.seed(20230105)

# DEFINING THE PRE-REQUISITES:

# defining the randomization selection and work participation tabulation function
randomize_work <- function(df) {
  
  # generating random number for all obs.
  rand <- runif(nrow(df))
  df$rand <- rand
  
  # within each HH, select only rows with top rand. value
  df_rand <- df %>% 
    filter(eligible == 1) %>% # filtering only eligible (18-64 yo) resp.
    group_by(hh_id) %>% # group by HH id
    top_n(1, rand) %>%  # selecting only 1 eligible observation within each HH, with top rand value
    ungroup()           # undo grouping
  
  # obtaining work participation summary
  result <- weighted.mean(df_rand$work, df_rand$weight_2, na.rm = T)  # performing weighted summary
  
  # return the result vector
  return(result)
}


# defining the m.carlo simulation func 
simulate_work_pcp <- function(df, nreps) {
  
  # initiating empty vector for storing the results
  result_vec <- vector(length = nreps)
  
  # iterating across nreps
  for (i in 1:nreps) {
    result_vec[i] <- randomize_work(df)
    writeLines(paste0("Successfully ran iteration ", 
                      as.character(i), " out of ", 
                      as.character(nreps)))
  }
  
  # returning the result
  return(result_vec)
}


# running the simulations with 1000 repetitions
sak_feb20_simresult <- simulate_work_pcp(sak_feb20, nreps = 1000)
sak_aug20_simresult <- simulate_work_pcp(sak_aug20, nreps = 1000)

# saving the simresult datasets 
simresult_df <- tibble(feb20 = sak_feb20_simresult, 
                       aug20 = sak_aug20_simresult)


# exporting the compiled dataset to csv format
write_csv(simresult_df, "data/simresult_df.csv")
