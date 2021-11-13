

# READY study
# David N Borg
# June 2021

# Care complexity model
setwd("~/Downloads")

# Load data
d = read.csv("READY-data-28-04-21.csv") %>%
  clean_names()

# Change wd
setwd("~/Downloads/Project - READY study")

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

# Second recode disability type, reduce for model
d$primary_disability_recat_3 <- car::recode(d$primary_disability_recat, "'ABI' = 'ABI';
                                      'Amputation' = 'other';
                                      'Intellectual' = 'other';
                                      'Neurological' = 'other';
                                      'Psychosocial' = 'other';
                                      'SCI' = 'SCI'")
table(d$primary_disability_recat_3)

dat = d %>%
  dplyr::filter(which_pathway_1_first_plan_2_first_plan_and_plan_review_3_plan_review %in% c('1','2')) %>%
  dplyr::select(discharge_delay_binary,
                socio_legal_index,
                age,
                primary_disability_recat_3,
                more_than_one_disability_type_identified,
                indigenous_status_model,
                change_in_housing_situation_at_discharge,
                complexity_index,
                ndis_pathway_journey,
                plan_approval_to_discharge,
                msh_hospital) %>%
  mutate(
    second_disabil = as.factor(more_than_one_disability_type_identified),
    indigenous = as.factor(indigenous_status_model),
    change_in_housing = as.factor(change_in_housing_situation_at_discharge),
    decision_maker = recode_factor(socio_legal_index, '0' = '0', '1' = '1', '2' = '1', '3' = '1'),
    supports = recode_factor(complexity_index, '0' = '0', '1' = '1', '2' = '2', '3' = '3', '4' = '3', '5' = '3'),
    primiary_disabil = factor(primary_disability_recat_3, levels = c("ABI", "SCI", "other")),
    facility = relevel({as.factor(msh_hospital)}, ref = 'PAH'),
    primary_disabil = primary_disability_recat_3,
    plan_approval = ndis_pathway_journey,
    plan_implementation = plan_approval_to_discharge
  )

# Remove unwanted variables for model
data = dat %>%
  select(-more_than_one_disability_type_identified,
         -indigenous_status_model,
         -change_in_housing_situation_at_discharge,
         -complexity_index,
         -socio_legal_index,
         -primiary_disabil,
         -msh_hospital,
         -primary_disability_recat_3)


# EDA
# age_s
data %>%
  mutate(discharge_delay_binary = recode_factor(discharge_delay_binary, '0' = 'No', '1' = 'Yes')) %>%
    ggplot(aes(x = as.factor(discharge_delay_binary), y = age)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.2, size = 2.5, alpha = 0.25) +
  labs(x = "Delayed Discharge", y = "Age (years)") +
  facet_grid(~"Age") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(size = 12)) -> plot_age
plot_age

# second_disabil
data %>%
  group_by(discharge_delay_binary,second_disabil) %>%
  summarise(Count = n()) %>%
  mutate(second_disabil = as.factor(second_disabil),
         second_disabil = recode_factor(second_disabil, '0' = 'No', '1' = 'Yes'),
         discharge_delay_binary = as.factor(discharge_delay_binary),
         discharge_delay_binary = recode_factor(discharge_delay_binary, '0' = 'No', '1' = 'Yes')) %>%
  ggplot() +
  geom_point(aes(x = second_disabil, y = discharge_delay_binary, 
                 size = Count, colour = Count)) +
  scale_size_continuous(range = c(5, 25)) +
  labs(x = "Secondary Disability", y = "Delayed Discharge") +
  guides(size = 'none') +
  facet_grid(~"Secondary Disability") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(size = 12)) -> plot_second_disabil
plot_second_disabil

# indigenous
data %>%
  drop_na(indigenous) %>%
  group_by(discharge_delay_binary,indigenous) %>%
  summarise(Count = n()) %>%
  mutate(indigenous = as.factor(indigenous),
         indigenous = recode_factor(indigenous, '0' = 'Not Indigenous', '1' = 'Indigenous'),
         discharge_delay_binary = as.factor(discharge_delay_binary),
         discharge_delay_binary = recode_factor(discharge_delay_binary, '0' = 'No', '1' = 'Yes')) %>%
  ggplot() +
  geom_point(aes(x = indigenous, y = discharge_delay_binary, 
                 size = Count, colour = Count)) +
  scale_size_continuous(range = c(5, 25)) +
  labs(x = "Indigenous Status", y = "Delayed Discharge") +
  guides(size = 'none') +
  facet_grid(~"Indigenous Status") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(size = 12)) -> plot_indigenous
plot_indigenous

# change_in_housing
data %>%
  group_by(discharge_delay_binary,change_in_housing) %>%
  summarise(Count = n()) %>%
  mutate(change_in_housing = as.factor(change_in_housing),
         change_in_housing = recode_factor(change_in_housing, '0' = 'No', '1' = 'Yes'),
         discharge_delay_binary = as.factor(discharge_delay_binary),
         discharge_delay_binary = recode_factor(discharge_delay_binary, '0' = 'No', '1' = 'Yes')) %>%
  ggplot() +
  geom_point(aes(x = change_in_housing, y = discharge_delay_binary, 
                 size = Count, colour = Count)) +
  scale_size_continuous(range = c(5, 25)) +
  labs(x = "Change in Housing at Discharge", y = "Delayed Discharge") +
  guides(size = 'none') +
  facet_grid(~"Change in Housing at Discharge") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(size = 12)) -> plot_housing
plot_housing

# Decision maker
data %>%
  group_by(discharge_delay_binary,decision_maker) %>%
  summarise(Count = n()) %>%
  mutate(decision_maker = as.factor(decision_maker),
         decision_maker = recode_factor(decision_maker, '0' = 'No', '1' = 'Yes'),
         discharge_delay_binary = as.factor(discharge_delay_binary),
         discharge_delay_binary = recode_factor(discharge_delay_binary, '0' = 'No', '1' = 'Yes')) %>%
  ggplot() +
  geom_point(aes(x = decision_maker, y = discharge_delay_binary, 
                 size = Count, colour = Count)) +
  scale_size_continuous(range = c(5, 25)) +
  labs(x = "Decision Maker", y = "Delayed Discharge") +
  guides(size = 'none') +
  facet_grid(~"Appointed Decision Maker") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(size = 12)) -> plot_decision
plot_decision

# supports
data %>%
  group_by(discharge_delay_binary,supports) %>%
  summarise(Count = n()) %>%
  mutate(supports = as.factor(supports),
         supports = recode_factor(supports, '0' = 'None', '1' = '1', '2' = '2', '3' = '>2'),
         discharge_delay_binary = as.factor(discharge_delay_binary),
         discharge_delay_binary = recode_factor(discharge_delay_binary, '0' = 'No', '1' = 'Yes')) %>%
  ggplot() +
  geom_point(aes(x = supports, y = discharge_delay_binary, 
                 size = Count, colour = Count)) +
  scale_size_continuous(range = c(5, 25)) +
  labs(x = "Required Supports", y = "Delayed Discharge") +
  guides(size = 'none') +
  facet_grid(~"Required Supports") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(size = 12)) -> plot_supports
plot_supports

# ndis plan approval pathway
data %>%
  mutate(discharge_delay_binary = recode_factor(discharge_delay_binary, '0' = 'No', '1' = 'Yes')) %>%
  ggplot(aes(x = as.factor(discharge_delay_binary), y = plan_approval)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.2, size = 2.5, alpha = 0.25) +
  labs(x = "Delayed Discharge", y = "Plan Approval (days)") +
  facet_grid(~"NDIS Plan Approval Timeframe") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(size = 12)) -> plot_approval_pathway
plot_approval_pathway

# ndis plan implementation pathway
data %>%
  mutate(discharge_delay_binary = recode_factor(discharge_delay_binary, '0' = 'No', '1' = 'Yes')) %>%
  ggplot(aes(x = as.factor(discharge_delay_binary), y = plan_implementation)) +
  geom_boxplot(outlier.size = -1) +
  geom_jitter(width = 0.2, size = 2.5, alpha = 0.25) +
  labs(x = "Delayed Discharge", y = "Plan Implementation (days)") +
  facet_grid(~"NDIS Plan Implementation Timeframe") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(size = 12)) -> plot_implementation_pathway
plot_implementation_pathway

# facility
data %>%
  group_by(discharge_delay_binary,facility) %>%
  summarise(Count = n()) %>%
  mutate(facility = as.factor(facility),
         facility = recode_factor(facility, 'PAH' = 'Tertiary', 'Other' = 'Other'),
         discharge_delay_binary = as.factor(discharge_delay_binary),
         discharge_delay_binary = recode_factor(discharge_delay_binary, '0' = 'No', '1' = 'Yes')) %>%
  ggplot() +
  geom_point(aes(x = facility, y = discharge_delay_binary, 
                 size = Count, colour = Count)) +
  scale_size_continuous(range = c(5, 25)) +
  labs(x = "Facility Type", y = "Delayed Discharge") +
  guides(size = 'none') +
  facet_grid(~"Facility Type") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(size = 12)) -> plot_facility
plot_facility

# primary_disabil
data %>%
  group_by(discharge_delay_binary,primary_disabil) %>%
  summarise(Count = n()) %>%
  mutate(primary_disabil = recode_factor(primary_disabil, 'ABI' = 'ABI', 'SCI' = 'SCI', 'Other' = 'Other'),
         discharge_delay_binary = as.factor(discharge_delay_binary),
         discharge_delay_binary = recode_factor(discharge_delay_binary, '0' = 'No', '1' = 'Yes')) %>%
  ggplot() +
  geom_point(aes(x = primary_disabil, y = discharge_delay_binary, 
                 size = Count, colour = Count)) +
  scale_size_continuous(range = c(5, 25)) +
  labs(x = "Primary Disability", y = "Delayed Discharge") +
  guides(size = 'none') +
  facet_grid(~"Primary Disability Type") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(size = 12)) -> plot_disabil
plot_disabil


# EDA panel of effects
plot_grid(plot_age,
          plot_indigenous,
          plot_disabil,
          plot_housing,
          plot_second_disabil,
          plot_facility,
          plot_decision,
          plot_supports,
          plot_approval_pathway,
          plot_implementation_pathway,
          ncol = 3, nrow = 4, 
          align = 'v', axis = "lr")
ggsave(file = "eda.png", units="in", width = 15, height = 11.5, dpi = 300)





