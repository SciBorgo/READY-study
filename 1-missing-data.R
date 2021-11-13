

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
    age = as.numeric(scale(age, center = T, scale = T)),
    discharge_delay = discharge_delay_binary,
    second_disability = as.factor(more_than_one_disability_type_identified),
    indigenous_status = as.factor(indigenous_status_model),
    change_in_housing = as.factor(change_in_housing_situation_at_discharge),
    decision_maker = recode_factor(socio_legal_index, '0' = '0', '1' = '1', '2' = '1', '3' = '1'),
    supports = recode_factor(complexity_index, '0' = '0', '1' = '1', '2' = '2', '3' = '3', '4' = '3', '5' = '3'),
    disability = factor(primary_disability_recat_3, levels = c("ABI", "SCI", "other")),
    plan_approval = ndis_pathway_journey,
    plan_implementation = plan_approval_to_discharge,
    facility = relevel({as.factor(msh_hospital)}, ref = 'PAH'),
    primary_disabil = primary_disability_recat_3
  )

# Remove unwanted variables for model
data = dat %>%
  select(-more_than_one_disability_type_identified,
         -indigenous_status_model,
         -change_in_housing_situation_at_discharge,
         -complexity_index,
         -socio_legal_index,
         -ndis_pathway_journey,
         -plan_approval_to_discharge,
         -primary_disabil,
         -msh_hospital,
         #-age,
         -primary_disability_recat_3,
         -discharge_delay_binary)

# Plot: overall
data %>% 
  select(discharge_delay,
         age,
         indigenous_status,
         disability,
         second_disability,
         facility,
         change_in_housing,
         decision_maker,
         plan_approval,
         plan_implementation,
         supports
         ) %>%
  vis_miss() +
  theme(axis.text.x = element_text(angle = 45)) -> plot_miss1
plot_miss1

# Plot: NDIS pathway length against delayed discharge
data %>% 
  mutate(discharge_delay = as.factor(discharge_delay),
         discharge_delay = recode_factor(discharge_delay, '0' = 'No', '1' = 'Yes')) %>%
  gg_miss_fct(fct = discharge_delay) +
  labs(x = "Discharge Delay", y  = "Variable") -> plot_miss2
plot_miss2



# Panel of effects
plot_grid(plot_miss1,
          plot_miss2,
          ncol = 1,
          nrow = 2,
          scale = 0.95, 
          align = 'v', 
          axis = "lr",
          labels = c('A','B'),
          label_size = 14)
ggsave(file = "missing-data.png", units="in", width = 6, height = 8, dpi = 300)

