

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

# Second recode disability type, reduce for model
d$primary_disability_recat_3 <- car::recode(d$primary_disability_recat, "'ABI' = 'ABI';
                                      'Amputation' = 'other';
                                      'Intellectual' = 'other';
                                      'Neurological' = 'other';
                                      'Psychosocial' = 'other';
                                      'SCI' = 'SCI'")

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
                msh_hospital,
                plan_approval_to_discharge) %>%
  mutate(
    age_s = as.numeric(scale(age, center = T, scale = T)),
    second_disabil = as.factor(more_than_one_disability_type_identified),
    indigenous = as.factor(indigenous_status_model),
    change_in_housing = as.factor(change_in_housing_situation_at_discharge),
    decision_maker = recode_factor(socio_legal_index, '0' = '0', '1' = '1', '2' = '1', '3' = '1'),
    supports = recode_factor(complexity_index, '0' = '0', '1' = '1', '2' = '2', '3' = '3', '4' = '3', '5' = '3'),
    primiary_disabil = factor(primary_disability_recat_3, levels = c("ABI", "SCI", "other")),
    ndis_approval_s = as.numeric(scale(ndis_pathway_journey, center = T, scale = T)),
    ndis_implementation_s = as.numeric(scale(plan_approval_to_discharge, center = T, scale = T)),
    facility = relevel({as.factor(msh_hospital)}, ref = 'PAH'),
    primary_disabil = primary_disability_recat_3
  )

# Load model

# Marginal effect of ndis plan implementation
prob_95 <- conditional_effects(fit, effects = "ndis_implementation_s", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95")
prob_66 <- conditional_effects(fit, effects = "ndis_implementation_s", plot = F, prob = .66)[[1]] %>%
  as_tibble() %>% mutate(ci = "66")

prob_supp <- bind_rows(prob_95, prob_66)

# Mean and SD of age
mu = mean(dat$plan_approval_to_discharge, na.rm = T)
sigma = sd(d$plan_approval_to_discharge, na.rm = T)

prob_supp %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = (effect1__*sigma)+mu, y = estimate__)) +
  geom_line(aes(x = (effect1__*sigma)+mu, y = estimate__)) +
  geom_ribbon(data = prob_95, aes(x = (effect1__*sigma)+mu, ymin = lower__, ymax = upper__), alpha = 0.35) +
  geom_ribbon(data = prob_66, aes(x = (effect1__*sigma)+mu, ymin = lower__, ymax = upper__), alpha = 0.35) +
  labs(y = "Posterior Probability of a Delayed Discharge", x = "Plan Implementation Timeframe (days)") +
  scale_x_continuous(limit = c(0,500), n.breaks = 12) +
  scale_y_continuous(limit = c(0.2,1)) +
  theme_bw() + 
  theme(axis.text=element_text(size = 9),
        axis.title=element_text(size = 10),
        legend.text=element_text(size = 9),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) +
  facet_grid(~"Marginal Effect of Plan Implementation Timeframe") +
  theme(strip.text.x = element_text(size = 10)) -> plot_marg_implement
plot_marg_implement


# Marginal effect of age
prob_95 <- conditional_effects(fit, effects = "age_s", plot = F, prob = .95)[[1]] %>%
  as_tibble() %>% mutate(ci = "95")
prob_66 <- conditional_effects(fit, effects = "age_s", plot = F, prob = .66)[[1]] %>%
  as_tibble() %>% mutate(ci = "66")

prob_supp <- bind_rows(prob_95, prob_66)

# Mean and SD of age
mu2 = mean(dat$age, na.rm = T)
sigma2 = sd(dat$age, na.rm = T)

prob_supp %>%
  select(effect1__,estimate__,se__,lower__,upper__) %>%
  ggplot(aes(x = (effect1__*sigma2)+mu2, y = estimate__)) +
  geom_line(aes(x = (effect1__*sigma2)+mu2, y = estimate__)) +
  geom_ribbon(data = prob_95, aes(x = (effect1__*sigma2)+mu2, ymin = lower__, ymax = upper__), alpha = 0.35) +
  geom_ribbon(data = prob_66, aes(x = (effect1__*sigma2)+mu2, ymin = lower__, ymax = upper__), alpha = 0.35) +
  labs(y = "Posterior Probability of a Delayed Discharge", x = "Age (years)") +
  scale_x_continuous(n.breaks = 10) +
  scale_y_continuous(limit = c(0.2,1)) +
  theme_bw() + 
  theme(axis.text=element_text(size = 9),
        axis.title=element_text(size = 10),
        legend.text=element_text(size = 9),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank()) +
  facet_grid(~"Marginal Effect of Age") +
  theme(strip.text.x = element_text(size = 10)) -> plot_marg_age
plot_marg_age


# Panel plots
plot_grid(plot_marg_implement,
          plot_marg_age,
          ncol = 2,
          nrow = 1,
          scale = 0.95, 
          align = 'v',
          axis = "lr")
#ggsave(file = "Figure2.png", units="in", width = 10, height = 4, dpi = 300)
ggsave(file = "Figure2.tiff", units="in", width = 8, height = 3, dpi = 300, compression = 'lzw')
