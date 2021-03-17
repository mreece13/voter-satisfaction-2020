rm(list=ls())

library(tidyverse)

df <- read_csv("data/cleaned/surveyCombined_weighted.csv", guess_max = 18000)

state_totals <- df %>% 
  drop_na(weights, votingMethod) %>% 
  group_by(state, votingMethod) %>%
  tally(wt = weights) %>% 
  mutate(n = round(n)) %>% 
  mutate(`Sample %` = round(100*n/sum(n), 2)) %>% 
  select(-n)

state_turnout <- df %>% 
  filter(state != "Unknown") %>% 
  filter(state != "International") %>% 
  filter(state != "District of Columbia") %>% 
  group_by(state) %>%
  tally() %>% 
  mutate(n = round(n)) %>% 
  mutate(`Sample %` = 100*n/sum(n)) %>% 
  select(-n)

state_presTotals <- df %>% 
  filter(state != "Unknown") %>% 
  filter(state != "International") %>% 
  filter(state != "District of Columbia") %>%
  mutate(Q6.5_presCandidate = recode(Q6.5_presCandidate, "A third party candidate" = "Other", "Other (please specify)" = "Other", "Donald Trump / Mike Pence (Republican)" = "Republican", "Joe Biden / Kamala Harris (Democratic)" = "Democrat")) %>% 
  drop_na(weights, Q6.5_presCandidate) %>% 
  group_by(state, Q6.5_presCandidate) %>% 
  tally(wt = weights) %>% 
  mutate(n = round(n)) %>% 
  mutate(`Sample %` = 100*n/sum(n)) %>% 
  select(-n)

state_totals %>% 
  write_csv("data/cleaned/sample_stateVotingMethodTotals.csv")

state_turnout %>% 
  write_csv("data/cleaned/sample_stateTurnout.csv")

state_presTotals %>% 
  write_csv("data/cleaned/sample_presTotals.csv")


######

mc_pres <- read_csv("data/cleaned/mcdonald_presTotals.csv")

mc_pres %>% 
  pivot_longer(-state, names_to = "Q6.5_presCandidate", values_to = "McDonald %") %>% 
  mutate(`McDonald %` = `McDonald %`*100) %>% 
  mutate(Q6.5_presCandidate = recode(Q6.5_presCandidate, "dem_percent" = "Democrat", "rep_percent" = "Republican", "other_percent" = "Other")) %>% 
  right_join(state_presTotals, by = c("Q6.5_presCandidate", "state")) %>% 
  write_csv("data/cleaned/sample_presTotals.csv")
