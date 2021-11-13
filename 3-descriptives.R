

# READY study
# David N Borg
# June 2021

# Care complexity model
setwd("~/Downloads")

# Load data
d = read.csv("READY-data-28-04-21.csv") %>%
  clean_names()

# Recode disability type
d$primary_disability_recat <- car::recode(d$primary_disability, "'ABI' = 'ABI';
                                      'Autism' = 'Intellectual';
                                      'Acquired Brain Injury' = 'ABI';
                                      'Cerebral Palsy' = 'Neurological';
                                      'Intellectual Disability' = 'Intellectual';
                                      'Multiple Sclerosis' = 'Neurological';
                                      'Other Neurological' = 'Neurological';
                                      'Other Physical' = 'Amputation';
                                      'Psychosocial Disability' = 'Psychosocial';
                                      'Spinal Cord Injury' = 'SCI';
                                      'Stroke' = 'ABI';
                                      'Visual Impairment' = 'Neurological'")
table(d$primary_disability_recat)

dat = d %>%
  dplyr::filter(which_pathway_1_first_plan_2_first_plan_and_plan_review_3_plan_review %in% c('1','2')) %>%
  mutate(
    second_disabil = as.factor(more_than_one_disability_type_identified),
    indigenous = as.factor(indigenous_status_model),
    change_in_housing = as.factor(change_in_housing_situation_at_discharge),
    decision_maker = recode_factor(socio_legal_index, '0' = '0', '1' = '1', '2' = '1', '3' = '1'),
    supports = recode_factor(complexity_index, '0' = '0', '1' = '1', '2' = '2', '3' = '3', '4' = '3', '5' = '3'),
    plan_approval = ndis_pathway_journey,
    plan_implementation = plan_approval_to_discharge,
    facility = relevel({as.factor(msh_hospital)}, ref = 'PAH')
    )

# Overall
dat %>%
  dplyr::select(facility, age, length_of_stay, plan_approval, plan_implementation) %>%
  summary()

dat %>%
  group_by(gender) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(marital_status) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(indigenous) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(income_source_on_admission) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(msh_hospital) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(primary_disability_recat) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(second_disabil) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(housing_situation_on_admission) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(change_in_housing_situation_at_discharge) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))


# Descriptives by facility
dat %>%
  dplyr::select(facility, age, length_of_stay, plan_approval, plan_implementation) %>%
  group_by(facility) %>%
  summarise(age_med = median(age, na.rm = T),
            age_25 = quantile(age, prob = 0.25),
            age_75 = quantile(age, prob = 0.75),
            los_med = median(length_of_stay, na.rm = T),
            los_25 = quantile(length_of_stay, prob = 0.25, na.rm = T),
            los_75 = quantile(length_of_stay, prob = 0.75, na.rm = T),
            ndis_plan_med = median(plan_approval, na.rm = T),
            ndis_plan_25 = quantile(plan_approval, prob = 0.25, na.rm = T),
            ndis_plan_75 = quantile(plan_approval, prob = 0.75, na.rm = T),
            ndis_implent_med = median(plan_implementation, na.rm = T),
            ndis_implent_25 = quantile(plan_implementation, prob = 0.25, na.rm = T),
            ndis_implent_75 = quantile(plan_implementation, prob = 0.75, na.rm = T))

dat %>%
  group_by(facility,gender) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(facility,marital_status) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(facility,indigenous) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(facility,income_source_on_admission) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(facility,primary_disability_recat) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(facility,second_disabil) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(facility,housing_situation_on_admission) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(facility,change_in_housing_situation_at_discharge) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(facility,accomodation) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(facility,assistive_technology) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(facility,behavioural_support) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(facility,home_modifications) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(facility,sil) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

# Descriptives by primary disability type
dat %>%
  dplyr::select(primary_disability_recat, age, length_of_stay, plan_approval, plan_implementation) %>%
  group_by(primary_disability_recat) %>%
  summarise(age_med = median(age, na.rm = T),
            age_25 = quantile(age, prob = 0.25),
            age_75 = quantile(age, prob = 0.75),
            los_med = median(length_of_stay, na.rm = T),
            los_25 = quantile(length_of_stay, prob = 0.25, na.rm = T),
            los_75 = quantile(length_of_stay, prob = 0.75, na.rm = T),
            ndis_plan_med = median(plan_approval, na.rm = T),
            ndis_plan_25 = quantile(plan_approval, prob = 0.25, na.rm = T),
            ndis_plan_75 = quantile(plan_approval, prob = 0.75, na.rm = T),
            ndis_implent_med = median(plan_implementation, na.rm = T),
            ndis_implent_25 = quantile(plan_implementation, prob = 0.25, na.rm = T),
            ndis_implent_75 = quantile(plan_implementation, prob = 0.75, na.rm = T))

dat %>%
  group_by(primary_disability_recat,gender) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(primary_disability_recat,marital_status) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(primary_disability_recat,indigenous) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(primary_disability_recat,income_source_on_admission) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(primary_disability_recat,facility) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(primary_disability_recat,second_disabil) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(primary_disability_recat,housing_situation_on_admission) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  View()

dat %>%
  group_by(primary_disability_recat,change_in_housing_situation_at_discharge) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(primary_disability_recat,accomodation) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  filter(accomodation=='1')

dat %>%
  group_by(primary_disability_recat,assistive_technology) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  filter(assistive_technology=='1')

dat %>%
  group_by(primary_disability_recat,behavioural_support) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  filter(behavioural_support=='1')

dat %>%
  group_by(primary_disability_recat,home_modifications) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  filter(home_modifications=='1')

dat %>%
  group_by(primary_disability_recat,sil) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count)) %>%
  filter(sil=='1')


# Overall sample: summary of support needs
dat %>%
  group_by(accomodation) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(assistive_technology) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(behavioural_support) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(home_modifications) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))

dat %>%
  group_by(sil) %>%
  summarise(count = n()) %>%
  mutate(percent = count/sum(count))
