# get mockstudy data from the arsenal package
# process for 01-rmd-anatomy session

library(arsenal)
library(janitor)
library(randomNames)
library(tidyverse)

# load the data
data(mockstudy)

# add in sites and countries
mockstudy <- mockstudy %>% 
  janitor::clean_names() %>% 
  mutate(site = c(rep("Portland", 100), rep("Ann Arbor", 100), rep("St. Louis", 100),
                  rep("Rochester", 100), rep("New Haven", 100), rep("Boston", 100),
                  rep("Chapel Hill", 100), rep("Gainesville", 100), rep("Denver", 100),
                  rep("Los Angeles", 100), rep("Seattle", 100), rep("New York", 100),
                  rep("Madrid", 50), rep("Barcelona", 50), rep("Rio de Janeiro", 50),
                  rep("Sao Paulo", 50),
                  rep("Mexico City", 84), rep("Nur-Sultan", 15))
  ) %>% 
  mutate(country = c(rep("USA", 1200), rep("Spain", 100), rep("Brasil", 100),
                     rep("Mexico", 84), rep("Kazakhstan", 15))
  )

# count target participants by site (just the total pre-filtering)
n_arms <- n_distinct(mockstudy$arm)
mock_targets <- mockstudy %>% 
  # add all the target counts
  add_count(site, country, name = "n_target_site") %>% 
  mutate(n_target = ceiling(n_target_site / n_arms)) %>% 
  select(-n_target_site)

# BOSTON ONLY
# randomly sample 50
set.seed(1984)
mock_boston_sample <- mock_targets %>% 
  filter(site == "Boston") %>% 
  sample_n(50) 

# and calculate n's
mock_boston <- mock_boston_sample %>% 
  count(arm, site, country, n_target) 

# export Boston mock data
write_csv(mock_boston, path = here::here("data/mockboston.csv"))

# BOSTON, SEATTLE, DENVER
# randomly sample 50, 62, 78
# and calculate n's and proportions needed
set.seed(1984)
mock_bsd_sample <- mock_targets %>% 
  filter(site %in% c("Boston", "Seattle", "Denver")) %>% 
  group_by(site) %>% 
  nest() %>% 
  ungroup() %>% 
  mutate(n_samp = c(50, 62, 78)) %>% 
  mutate(samp = map2(data, n_samp, sample_n)) %>% 
  select(site, samp) %>% 
  unnest(cols = c(samp))

# and calculate n's and proportions needed
mock_bsd <- mock_bsd_sample %>% 
  count(arm, site, country, n_target) 

write_csv(mock_bsd, path = here::here("data/mockbsd.csv"))
