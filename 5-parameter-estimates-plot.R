

# READY study
# David N Borg
# June 2021

# Load model

# Explore effect of age
age_effect <- 
  gather_draws(fit, b_age_s) %>%
  mutate(effect = .value)
age_effect %>% ggplot() + geom_histogram(aes(x = effect))
age_effect %>% mean_qi(exp(effect), .width = c(0.5, .95))
age_effect %>% mean_qi(exp(effect)>1)
age_effect %>% mean_qi(exp(effect)<1)
age_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Age") -> plotage
plotage

# Explore effect of indigenous status
ind_effect <- 
  gather_draws(fit, b_indigenous1) %>%
  mutate(effect = .value)
ind_effect %>% ggplot() + geom_histogram(aes(x = effect))
ind_effect %>% mean_qi(exp(effect), .width = c(0.66, .95))
ind_effect %>% mean_qi(exp(effect)>1)
ind_effect %>% mean_qi(exp(effect)<1)
ind_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Indigenous (True)") -> plotind
plotind

# Change in housing
housing_effect <- 
  gather_draws(fit, b_change_in_housing1) %>%
  mutate(effect = .value)
housing_effect %>% ggplot() + geom_histogram(aes(x = effect))
housing_effect %>% mean_qi(exp(effect), .width = c(0.5, .95))
housing_effect %>% mean_qi(exp(effect)>1)
housing_effect %>% mean_qi(exp(effect)<1)
housing_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Change in Housing (True)") -> plothouse
plothouse

# Explore effect of b_primary_disabilSCI
sci_effect <- 
  gather_draws(fit, b_primary_disabilSCI) %>%
  mutate(effect = .value)
sci_effect %>% ggplot() + geom_histogram(aes(x = effect))
sci_effect %>% mean_qi(exp(effect), .width = c(0.66, .95))
sci_effect %>% mean_qi(exp(effect)>1)
sci_effect %>% mean_qi(exp(effect)<1)
sci_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Disability (SCI)") -> plotsci
plotsci

# Explore effect of b_primary_disabilother
other_disabil_effect <- 
  gather_draws(fit, b_primary_disabilother) %>%
  mutate(effect = .value)
other_disabil_effect %>% ggplot() + geom_histogram(aes(x = effect))
other_disabil_effect %>% mean_qi(exp(effect), .width = c(0.66, .95))
other_disabil_effect %>% mean_qi(exp(effect)>1)
other_disabil_effect %>% mean_qi(exp(effect)<1)
other_disabil_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Disability (Other)") -> plotother
plotother

# Explore effect of b_second_disabil1
co_disabil_effect <- 
  gather_draws(fit, b_second_disabil1) %>%
  mutate(effect = .value)
co_disabil_effect %>% ggplot() + geom_histogram(aes(x = effect))
co_disabil_effect %>% mean_qi(exp(effect), .width = c(0.66, .95))
co_disabil_effect %>% mean_qi(exp(effect)>1)
co_disabil_effect %>% mean_qi(exp(effect)<1)
co_disabil_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Second Disability (True)") -> plotcodisabil
plotcodisabil

# Explore effect of facility type
facility_effect <- 
  gather_draws(fit, b_facilityOther) %>%
  mutate(effect = .value)
facility_effect %>% ggplot() + geom_histogram(aes(x = effect))
facility_effect %>% mean_qi(exp(effect), .width = c(0.66, .95))
facility_effect %>% mean_qi(exp(effect)>1)
facility_effect %>% mean_qi(exp(effect)<1)
facility_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Facility Type (Other)") -> plotfac
plotfac

# Explore effect of having a decision maker
dec_maker_effect <- 
  gather_draws(fit, b_decision_maker1) %>%
  mutate(effect = .value)
dec_maker_effect %>% ggplot() + stat_halfeye(aes(x = effect))
dec_maker_effect %>% mean_qi(exp(effect), .width = c(0.5, .95))
dec_maker_effect %>% mean_qi(exp(effect)>1)
dec_maker_effect %>% mean_qi(exp(effect)<1)
dec_maker_effect %>% ggplot() + stat_halfeye(aes(x = effect)) + theme_bw()
dec_maker_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Decision Maker (True)") -> plotdec
plotdec


# Explore effect of ndis plan approval timeframe
path_effect <- 
  gather_draws(fit, b_ndis_approval_s) %>%
  mutate(effect = .value)
path_effect %>% ggplot() + geom_histogram(aes(x = effect))
path_effect %>% mean_qi(exp(effect), .width = c(0.66, .95))
path_effect %>% mean_qi(exp(effect)>1)
path_effect %>% mean_qi(exp(effect)<1)
path_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Plan Approval Timeframe") -> plotndisapprov
plotndisapprov


# Explore effect of ndis plan implementation timeframe
path_effect <- 
  gather_draws(fit, b_ndis_implementation_s) %>%
  mutate(effect = .value)
path_effect %>% ggplot() + geom_histogram(aes(x = effect))
path_effect %>% mean_qi(exp(effect), .width = c(0.66, .95))
path_effect %>% mean_qi(exp(effect)>1)
path_effect %>% mean_qi(exp(effect)<1)
path_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Plan Implementation Timeframe") -> plotndisimplement
plotndisimplement


# Explore effect of supp index one
supp_1_effect <- 
  gather_draws(fit, b_supports1) %>%
  mutate(effect = .value)
supp_1_effect %>% ggplot() + geom_histogram(aes(x = effect))
supp_1_effect %>% mean_qi(exp(effect), .width = c(0.66, .95))
supp_1_effect %>% mean_qi(exp(effect)>1)
supp_1_effect %>% mean_qi(exp(effect)<1)
supp_1_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Required Supports (1)") -> plotsupp1
plotsupp1

# Explore effect of supp index two
supp_2_effect <- 
  gather_draws(fit, b_supports2) %>%
  mutate(effect = .value)
supp_2_effect %>% ggplot() + geom_histogram(aes(x = effect))
supp_2_effect %>% mean_qi(exp(effect), .width = c(0.66, .95))
supp_2_effect %>% mean_qi(exp(effect)>1)
supp_2_effect %>% mean_qi(exp(effect)<1)
supp_2_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Required Supports (2)") -> plotsupp2
plotsupp2

# Explore effect of supp index three or more
supp_3_effect <- 
  gather_draws(fit, b_supports3) %>%
  mutate(effect = .value)
supp_3_effect %>% ggplot() + geom_histogram(aes(x = effect))
supp_3_effect %>% mean_qi(exp(effect), .width = c(0.66, .95))
supp_3_effect %>% mean_qi(exp(effect)>1)
supp_3_effect %>% mean_qi(exp(effect)<1)
supp_3_effect %>% ggplot() + stat_halfeye(aes(x = effect), .width = c(0.66, 0.95)) + theme_bw(base_size = 10) +
  theme(axis.text=element_text(size = 8),
        axis.title=element_text(size = 8),
        legend.text=element_text(size = 8),
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.minor.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(x = expression(beta~"(logit)"), y = "Density") +
  geom_vline(xintercept=0, linetype=1, color = "grey20") +
  facet_grid(~"Required Supports (>2)") -> plotsupp3
plotsupp3


# Panel of effects (logit)
plot_grid(plotage,
          plotind,
          plothouse,
          plotsci,
          plotother,
          plotcodisabil,
          plotfac,
          plotdec,
          plotndisapprov,
          plotndisimplement,
          plotsupp1,
          plotsupp2,
          plotsupp3,
          ncol = 4,
          nrow = 4,
          scale = 0.95, 
          align = 'v',
          axis = "lr")
#ggsave(file = "Figure1.png", units="in", width = 10, height = 8.5, dpi = 300)
ggsave(file = "Figure1.tiff", units="in", width = 10, height = 8.5, dpi = 300, compression = 'lzw')





