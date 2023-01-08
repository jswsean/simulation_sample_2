# Preamble ----

# loading the required libraries
library(tidyverse)
library(srvyr)

# importing hify data and the simulation result df
simresult_df <- read_csv('data/simresult_df.csv')
hify <- haven::read_dta('data/compiled_resp2nd.dta')

# importing the sakernas feb-20 and aug-20, to obtain pre-simulation work prop. 
sak_feb20 <- read_csv('data/sak_feb20_analytic.csv')
sak_aug20 <- read_csv('data/sak_aug20_analytic.csv')




# Data preparation ----

# filtering for nan province in the hify df
hify <- hify %>% 
  filter(!is.na(province))

# setting up the proper survey design
hify_design <- hify %>% 
  as_survey_design(
    ids = c(psu, ssu, tsu),
    strata = strata,
    fpc = c(N1h, N2hi, Zhij),
    weights = fwgt,
    nest = T
  )

options(survey.lonely.psu = "adjust")

# extracting work proportion in february 2020 
hify_feb20_summary <- hify_design %>%
  summarize(
    work_feb20 = survey_mean(work_feb20, 
                             vartype = c("se", "ci"),
                             level = .95)
    )

hify_feb20_work <- hify_feb20_summary[[1]]
hify_feb20_ll <- hify_feb20_summary[[3]]

# in jul-20 
hify_jul20_summary <- hify_design %>%
  summarize(
    work_jul20 = survey_mean(work_jul20, 
                             vartype = c("se", "ci"),
                             level = .95)
  )

hify_jul20_work <- hify_jul20_summary[[1]]
hify_jul20_ll <- hify_jul20_summary[[3]]


# generate density estimates for Sak's feb-20 and aug-20 dist, for plotting density graph
feb20_density <- density(simresult_df$feb20)
feb20_density_df <- tibble(
  x = feb20_density$x, 
  y = feb20_density$y
) %>% 
  mutate(
    dist = case_when(
     x < quantile(simresult_df$feb20, .01) | x > quantile(simresult_df$feb20, .99)  ~  1,
     x < quantile(simresult_df$feb20, .05) | x > quantile(simresult_df$feb20, .95)  ~  2,
     x < quantile(simresult_df$feb20, .1) | x > quantile(simresult_df$feb20, .9)    ~  3,
     TRUE                                                                           ~  4
    )
  )

# for aug-20
aug20_density <- density(simresult_df$aug20)
aug20_density_df <- tibble(
  x = aug20_density$x, 
  y = aug20_density$y
) %>% 
  mutate(
    dist = case_when(
      x < quantile(simresult_df$aug20, .01) | x > quantile(simresult_df$aug20, .99)  ~  1,
      x < quantile(simresult_df$aug20, .05) | x > quantile(simresult_df$aug20, .95)  ~  2,
      x < quantile(simresult_df$aug20, .1) | x > quantile(simresult_df$aug20, .9)    ~  3,
      TRUE                                                                           ~  4
    )
  )



# now, to get pre-simulation work proportion: 

# setting up Sak Feb-20's survey design
sak_feb20 <- sak_feb20 %>% 
  as_survey_design(
    id = psu, 
    strata = strata, 
    weights = weight_2
  )

pre_feb20 <- sak_feb20 %>% 
  filter(eligible == 1) %>%  # only among one 18-64 yo population within the HH
  summarise(
    pre_work = survey_mean(work, na.rm = T)
  ) %>% 
  .[[1]]  # extracting the mean only



# Sak Aug-20's survey design
sak_aug20 <- sak_aug20 %>% 
  as_survey_design(
    id = psu, 
    strata = strata, 
    weights = weight_2
  )

pre_aug20 <- sak_aug20 %>% 
  filter(eligible == 1) %>%  # only among one 18-64 yo population within the HH
  summarise(
    pre_work = survey_mean(work, na.rm = T)
  ) %>% 
  .[[1]]


# Visualization ----

# visualizing kernel density of Sak's simulated work prop in Feb-20
feb20_viz <- ggplot(feb20_density_df, 
       aes(x=x)) +
  geom_ribbon(aes(ymin = 0, ymax = y, fill = as.factor(dist))) +
  geom_line(aes(y = y)) +
  geom_vline(xintercept = quantile(simresult_df$feb20, .5),
             color = "red", linetype = "dashed") +
  geom_vline(xintercept = pre_feb20, 
             color= "black", linetype = "dashed") +
  geom_vline(xintercept = hify_feb20_ll,
             color= "black", linetype = "dashed") +
  geom_vline(xintercept = hify_feb20_work,
             color= "black", linetype = "dashed") +
  annotate("text", x = quantile(simresult_df$feb20, .5), 
           y = 200, label = "Median", color = "red") +
  annotate("text", x = pre_feb20, 
           y = 200, label = "Pre-simulation") +
  annotate("text", x = hify_feb20_ll, 
           y = 200, label = "Lower bounds\nfrom 2nd resp. \nestimate") +
  annotate("text", x = hify_feb20_work, 
           y = 200, label = "2nd resp. \nestimate") +
  geom_errorbarh(aes(xmax = quantile(simresult_df$feb20, .5),
                     xmin = pre_feb20,
                     y = 250,
                     height = 2), linetype = "twodash") +
  geom_errorbarh(aes(xmax = hify_feb20_ll,
                     xmin = quantile(simresult_df$feb20, .5),
                     y = 250,
                     height = 2), linetype = "twodash") +
  annotate("text", x = .718, 
           y = 255, label = paste0(format((quantile(simresult_df$feb20, .5) - 
                                           pre_feb20)*100, digits = 2), " p.p.")) +
  annotate("text", x = .727, 
           y = 255, label = paste0(format((hify_feb20_ll - 
                                             quantile(simresult_df$feb20, .5))*100, 
                                          digits = 2), " p.p.")) +
  labs(
    x = "Proportion working in Feb. 2020",
    y = "Density estimate", 
    fill = "Category",
    title = "Comparing February 2020 work participation between LFS and HiFy"
  ) +
  coord_cartesian(xlim = c(.71, .76)) +
  scale_fill_brewer(labels = c("1% Extreme", "5% Extreme", "10% Extreme", "Others"),
                    direction = -1, palette = "Blues") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = .5)
  )

feb20_viz
ggsave('output/feb20_viz.png') # exporting img to png.



# visualizing aug-20
aug20_viz <- ggplot(aug20_density_df, 
       aes(x=x)) +
  geom_ribbon(aes(ymin = 0, ymax = y, fill = as.factor(dist))) +
  geom_line(aes(y = y)) +
  geom_vline(xintercept = quantile(simresult_df$aug20, .5),
             color = "red", linetype = "dashed") +
  geom_vline(xintercept = pre_aug20, 
             color= "black", linetype = "dashed") +
  geom_vline(xintercept = hify_jul20_ll,
             color= "black", linetype = "dashed") +
  geom_vline(xintercept = hify_jul20_work,
             color= "black", linetype = "dashed") +
  annotate("text", x = quantile(simresult_df$aug20, .5), 
           y = 350, label = "Median", color = "red") +
  annotate("text", x = pre_aug20, 
           y = 350, label = "Pre-simulation") +
  annotate("text", x = hify_jul20_ll, 
           y = 350, label = "Lower bounds\nfrom 2nd resp. \nestimate") +
  annotate("text", x = hify_jul20_work, 
           y = 350, label = "2nd resp. \nestimate") +
  geom_errorbarh(aes(xmax = quantile(simresult_df$aug20, .5),
                     xmin = pre_aug20,
                     y = 400,
                     height = 2), linetype = "twodash") +
  geom_errorbarh(aes(xmax = hify_jul20_ll,
                     xmin = quantile(simresult_df$aug20, .5),
                     y = 400,
                     height = 2), linetype = "twodash") +
  annotate("text", x = .672, 
           y = 410, label = paste0(format((quantile(simresult_df$aug20, .5) - 
                                             pre_aug20)*100, digits = 2), " p.p.")) +
  annotate("text", x = .681, 
           y = 410, label = paste0(format((hify_jul20_ll - 
                                             quantile(simresult_df$aug20, .5))*100, 
                                          digits = 2), " p.p.")) +
  labs(
    x = "Proportion working in mid 2020",
    y = "Density estimate", 
    fill = "Category",
    title = "Comparing mid 2020 work participation between LFS and HiFy"
  ) +
  coord_cartesian(xlim = c(.66, .71)) +
  scale_fill_brewer(labels = c("1% Extreme", "5% Extreme", "10% Extreme", "Others"),
                    direction = -1, palette = "Blues") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = .5)
  )

aug20_viz
ggsave('output/aug20_viz.png') # exporting img to png.

