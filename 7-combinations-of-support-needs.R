

# READY study
# David N Borg
# June 2021

# Combinations of support needs

# Setwd
setwd("~/Downloads")

# Load data
dat = read.csv("READY-data-28-04-21.csv") %>%
  clean_names() %>%
  select(project_id, accomodation, sil, assistive_technology, home_modifications, behavioural_support, which_pathway_1_first_plan_2_first_plan_and_plan_review_3_plan_review) %>%
  mutate(
    accomodation = as.factor(accomodation),
    s_accomodation = recode_factor(accomodation, '1' = 'ACC'),
    sil = as.factor(sil),
    s_sil = recode_factor(sil, '1' = 'SIL'),
    assistive_technology = as.factor(assistive_technology),
    s_assistive_technology = recode_factor(assistive_technology, '1' = 'AT'),
    home_modifications = as.factor(home_modifications),
    s_home_modifications = recode_factor(home_modifications, '1' = 'HM'),
    behavioural_support = as.factor(behavioural_support),
    s_behavioural_support = recode_factor(behavioural_support, '1' = 'BEHAV')
  ) %>%
  dplyr::filter(which_pathway_1_first_plan_2_first_plan_and_plan_review_3_plan_review %in% c('1','2')) %>%
  dplyr::select(-which_pathway_1_first_plan_2_first_plan_and_plan_review_3_plan_review) %>%
  pivot_longer(cols = starts_with("s_"),
               names_to = "required_supps",
               values_to = "value") %>%
  select(-accomodation, -sil, -assistive_technology, -home_modifications, -behavioural_support) %>%
  group_by(project_id) %>% summarise(value=paste(value, collapse=" ")) %>%
  group_by(value) %>%
  summarise(count = n()) %>%
  arrange(-count)

# Save data
write.csv(dat, file = "support_table.csv", row.names = T)


## % delay
read.csv("READY-data-28-04-21.csv") %>%
  clean_names() %>%
  select(project_id, discharge_delay_binary, accomodation, sil, assistive_technology, home_modifications, behavioural_support, which_pathway_1_first_plan_2_first_plan_and_plan_review_3_plan_review) %>%
  mutate(
    accomodation = as.factor(accomodation),
    s_accomodation = recode_factor(accomodation, '1' = 'ACC'),
    sil = as.factor(sil),
    s_sil = recode_factor(sil, '1' = 'SIL'),
    assistive_technology = as.factor(assistive_technology),
    s_assistive_technology = recode_factor(assistive_technology, '1' = 'AT'),
    home_modifications = as.factor(home_modifications),
    s_home_modifications = recode_factor(home_modifications, '1' = 'HM'),
    behavioural_support = as.factor(behavioural_support),
    s_behavioural_support = recode_factor(behavioural_support, '1' = 'BEHAV')
  ) %>%
  dplyr::filter(which_pathway_1_first_plan_2_first_plan_and_plan_review_3_plan_review %in% c('1','2')) %>%
  dplyr::select(-which_pathway_1_first_plan_2_first_plan_and_plan_review_3_plan_review) %>%
  pivot_longer(cols = starts_with("s_"),
               names_to = "required_supps",
               values_to = "value") %>%
  select(-accomodation, -sil, -assistive_technology, -home_modifications, -behavioural_support) %>%
  group_by(project_id,discharge_delay_binary) %>% summarise(value=paste(value, collapse=" ")) %>%
  group_by(value,discharge_delay_binary) %>%
  summarise(count = n()) %>%
  filter(discharge_delay_binary == "1") %>%
  write.csv(file = "support_table_delays.csv", row.names = T)







