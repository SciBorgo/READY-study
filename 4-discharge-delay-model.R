

# READY study
# David N Borg
# June 2021

# Set seed
set.seed(1)

# Care complexity model
setwd("~/Downloads")

# Load data
d = read.csv("READY-data-28-04-21.csv") %>%
  clean_names()

# Explore discharge delay
ggplot() + geom_histogram(data = d, aes(x = discharge_delay_days), bins = 20) +
  theme_bw()

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

# Remove unwanted variables for model
data = dat %>%
  select(-more_than_one_disability_type_identified,
         -indigenous_status_model,
         -change_in_housing_situation_at_discharge,
         -complexity_index,
         -socio_legal_index,
         -ndis_pathway_journey,
         -primiary_disabil,
         -msh_hospital,
         -age,
         -primary_disability_recat_3,
         -plan_approval_to_discharge)

# Variables
head(data,5)

# Missing data
vis_miss(data)
vis_dat(data)

sapply(data, function(x) sum(is.na(x))) %>%
  .[which(.>0)]

imp_datasets <- mice(data, m = 5, method = "pmm", seed = 123)
imp_datasets

# look at where data is imputed to
stripplot(imp_datasets, indigenous, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, ndis_approval_s, pch = 19, xlab = "Imputation number")
stripplot(imp_datasets, ndis_implementation_s, pch = 19, xlab = "Imputation number")

# Model formula
delay_formula = discharge_delay_binary ~ 
  age_s + 
  indigenous +
  change_in_housing + 
  primary_disabil + 
  second_disabil + 
  facility +
  ndis_approval_s +
  ndis_implementation_s + 
  decision_maker + 
  supports


# Penalised regression model
fit <- brm_multiple(formula = delay_formula,
           data = imp_datasets,
           prior = set_prior(horseshoe(df = 1, par_ratio = 0.2)),
           family = bernoulli(link = "logit"),
           chains = 8,
           cores = 8,
           iter = 10000,
           seed = 123,
           thin = 5,
           control = list(adapt_delta = 0.99, max_treedepth = 15))


# Check Rhats
round(fit$rhats, 3)

# Save model
#save(fit, file = "fit_delay.RData")

# Predictive check and chains
pp_check(fit, re_formula = NULL, ndraws = 100)

# Check chain convergence
plot(fit)

# Model summary
summary(fit)

# Plot
conditional_effects(fit)

# Priors
prior_summary(fit)

# R2
r2 = print(bayes_R2(fit), digits = 2)
r2

# Look at effects
plot(fit)

# NDIS implementation timeframe
pathway_effect <- 
  gather_draws(fit, b_ndis_implementation_s) %>%
  mutate(effect = .value)
pathway_effect %>% mean_qi(exp(effect), .width = c(.95))
pathway_effect %>% mean_qi(effect>0)
pathway_effect %>% mean_qi(effect<0)
pathway_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
# Pr >0 = 0.88

# Age
age_effect <- 
  gather_draws(fit, b_age_s) %>%
  mutate(effect = .value)
age_effect %>% mean_qi(exp(effect), .width = c(.95))
age_effect %>% mean_qi(effect>0)
age_effect %>% mean_qi(effect<0)
age_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 14)
# Pr >0 = 0.88










