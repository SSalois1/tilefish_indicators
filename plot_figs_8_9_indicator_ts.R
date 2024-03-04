library(patchwork)
g1 = ggplot(indicators %>% filter(indicator == 'SST'), 
            aes(year, sample.mean))+
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin=lower.bound,
                  ymax=upper.bound), alpha=.3) +
  ecodata::geom_gls() +
  annotate('text', x = 2000.5,
           y = 21, label = '(a)', size = 7) +
  labs(x = 'Year',
       y ='Mean SST (°C)')+
  ecodata::theme_ts() +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm"))

g2 = ggplot(indicators %>% filter(indicator == 'Bottom_temperature'), 
            aes(year, sample.mean))+
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin=lower.bound,
                  ymax=upper.bound), alpha=.3) +
  ecodata::geom_gls() +
  annotate('text', x = 1971,
           y = 13.5, label = '(b)', size = 7) +
  labs(x = 'Year',
       y ='Mean bottom temperature (°C)')+
  ecodata::theme_ts() +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm"))


g3 = ggplot(indicators %>% filter(indicator == 'Salinity 78m'), 
            aes(year, sample.mean))+
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin=lower.bound,
                  ymax=upper.bound), alpha=.3) +
  ecodata::geom_gls() +
  annotate('text', x = 1993.5,
           y = 36, label = '(c)', size = 7) +
  labs(x = 'Year',
       y ='Mean salinity 78m')+
  ecodata::theme_ts() +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm"))


g4 = ggplot(indicators %>% filter(indicator == 'Shelf_water_volume'), 
            aes(year, sample.mean))+
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin=lower.bound,
                  ymax=upper.bound), alpha=.3) +
  ecodata::geom_gls() +
  annotate('text', x = 1978,
           y = 7080, label = '(d)', size = 7) +
  labs(x = 'Year',
       y ='Mean shelf water volume') +
  ecodata::theme_ts() +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm"))

g5 = ggplot(indicators %>% filter(indicator == 'Shelf_water_temperature'), 
            aes(year, sample.mean))+
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin=lower.bound,
                  ymax=upper.bound), alpha=.3) +
  ecodata::geom_gls() +
  annotate('text', x = 1978,
           y = 31, label = '(e)', size = 7) +
  labs(x = 'Year',
       y ='Mean shelf water temperature (°C)') +
  ecodata::theme_ts() +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm"))

g6 = ggplot(indicators %>% filter(indicator == 'Shelf_water_salinity'), 
            aes(year, sample.mean))+
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin=lower.bound,
                  ymax=upper.bound), alpha=.3) +
  ecodata::geom_gls() +
  annotate('text', x = 1978,
           y = 35.5, label = '(f)', size = 7) +
  labs(x = 'Year',
       y ='Mean shelf water salinity') +
  ecodata::theme_ts() +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm"))

g7 = ggplot(indicators %>% filter(indicator == 'Gulf_stream_index'), 
            aes(year, sample.mean))+
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin=lower.bound,
                  ymax=upper.bound), alpha=.3) +
  ecodata::geom_gls() +
  annotate('text', x = 1955.5,
           y = 2, label = '(a)', size = 7) +
  labs(x = 'Year',
       y ='Mean gulf stream index') +
  ecodata::theme_ts() +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm"))

g8 = ggplot(indicators %>% filter(indicator == 'Chl-A'), 
           aes(year, sample.mean)) +
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin=lower.bound,
                  ymax=upper.bound), alpha=.3) +
  ecodata::geom_gls() +
  annotate('text', x = 1998.5,
           y = 1, label = '(b)', size = 7) +
  labs(x = 'Year',
       y ='Mean CHL-A')+
  ecodata::theme_ts() +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 15),
        axis.ticks.length = unit(-1.4, "mm"))
g9 = ggplot(indicators %>% filter(indicator == 'Microplankton'), 
             aes(year, sample.mean))+
  geom_line(lwd = 1) +
  geom_ribbon(aes(ymin=lower.bound,
                  ymax=upper.bound), alpha=.3) +
  ecodata::geom_gls() +
  annotate('text', x = 1998.5,
            y = 0.55, label = '(c)', size = 7) +
  labs(x = 'Year',
       y ='Mean microplankton abundance') +
  ecodata::theme_ts() +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm"))

g10 = cp %>% filter(Var == 'cold_pool_index') %>%
  group_by(Time) %>% 
  summarize(cp_pers = mean(Value), 
            sd_pers = sd(Value))%>% 
  ggplot(., aes(Time, cp_pers*-1))+
  geom_line() +
  annotate('text', x = 1960.5,
           y = 2.4, label = '(d)', size = 7) +
  ecodata::geom_gls() +
  labs(x = 'Year',
       y ='Cold pool index (x(-1))')+
  ecodata::theme_ts()      +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm"))

g11 = cp %>% filter(Var == 'extent_index') %>%
  group_by(Time) %>% 
  summarize(cp_ext = mean(Value), 
            sd_ext = sd(Value)) %>% 
  ggplot(., aes(Time, cp_ext))+
  geom_line() +
  annotate('text', x = 1960.5,
           y = 200, label = '(e)', size = 7) +
  ecodata::geom_gls() +
  labs(x = 'Year',
       y ='Spatial extent index')+
  ecodata::theme_ts()  +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm"))

g12 = cp %>% filter(Var == 'persistence_index') %>%
  group_by(Time) %>% 
  summarize(cp_pers = mean(Value), 
            sd_pers = sd(Value))%>% 
  ggplot(., aes(Time, cp_pers))+
  geom_line() +
  annotate('text', x = 1960.5,
           y = 2, label = '(f)', size = 7) +
  ecodata::geom_gls() +
  labs(x = 'Year',
       y ='Persistence index')+
  ecodata::theme_ts()      +
  theme(axis.text = element_text(size = 15), 
        axis.title = element_text(size = 16),
        axis.ticks.length = unit(-1.4, "mm"))

(g1 + g2 + g3) / (g4 + g5 + g6)

(g7 + g8 + g9) / (g10 + g11 + g12)
