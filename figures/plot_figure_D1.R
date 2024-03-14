larv <- read.csv(here::here('data/larval/Malacanthidae_all_lengthdata_Oct2023.csv'))
larv <- larv %>%
  rename_all(., .funs = tolower) %>% 
  mutate(date = lubridate::dmy_hms(event_date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         week = lubridate::week(date), 
         day = lubridate::day(date)) %>% 
  rename(cruise_id = cruise_name)

# Define category breaks
range(larv$length)
size_breaks <- c(1:28)
# Making a function to bin the catches
label_interval <- function(breaks) {
  paste0("(", breaks[1:length(breaks) - 1], "-", breaks[2:length(breaks)], ")")
}
labels = label_interval(size_breaks)
tab = table(cut(larv$length, 
                breaks = size_breaks,
                labels = label_interval(size_breaks)))

tab.df <- as.data.frame(tab)
colnames(tab.df) <- c('Lengths', 'Frequency')
## -- Length freq plot -- ##
ggplot(data=tab.df, aes(x = Lengths, y = Frequency)) +
  geom_bar(stat="identity", col = 'black', fill = 'black') +   
  geom_rect(aes(xmin = 0, xmax = 9.0, ymin = -Inf, ymax = Inf), 
            fill = 'lightgrey', alpha = 0.02) +
  geom_rect(aes(xmin = 9.0, xmax = max(28), ymin = -Inf, ymax = Inf), 
            fill = 'grey34', alpha = 0.02) +
  geom_bar(stat="identity", col = 'black', fill = 'black') +   
  theme_classic() +  
  theme(legend.position = c(0.85, 0.8)) +
  theme(legend.background = element_rect(linetype="solid", 
                                         colour ="grey20")) +
  theme(legend.key.size = unit(0.5, 'cm')) + 
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15),
        axis.ticks.length = unit(-1.4, "mm")) 
ggsave(path = here::here('figures'),'length_freq_larval.png', 
       width = 10, height = 8, units = "in")




ttl_larv <- larv %>% filter(taxa_ichthyo == '170070000') %>% 
  dplyr::select(total_count, year, event_number, taxa_ichthyo) %>% 
  group_by(year, event_number) %>%
  summarise(n = sum(total_count))
ttl_larv2 <- ttl_larv %>% 
  group_by(year) %>%
  summarise(ttl = sum(n))
years = seq(from = min(larv$year), to= max(larv$year), by = 1)


ttl_larv2$survey <- NA
ttl_larv2[c(1:9),3] <- 'marmap'
ttl_larv2[c(10:30),3] <- 'ecomon'

var.test(ttl ~ survey,
         data = ttl_larv2)$p.value
t.test(ttl ~ survey,
               data = ttl_larv2,
               var.equal = FALSE,
               alternative = "less")

ggplot(ttl_larv2, aes(x= year, y = ttl)) +
  geom_line() + 
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") + 
hist(sqrt(ttl_larv2$ttl))  
ttl_larv3 <- ttl_larv2 %>% filter(year != 2014 & year !=2021)
summary(lm(ttl~year, data = ttl_larv2))
summary(lm(ttl~year, data = ttl_larv3))

ttl_larv2 %>% 
ggpubr::ggscatter(x = 'year', y = 'ttl', 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.coeff.args = list(method="pearson"),
          add.params=list(color="dodgerblue", fill="lightgray"),
          xlab = "Mean salinity (92m)", ylab = "Recruitment Estimate",
          title="Lag one year 92m")
ttl_larv3 %>% 
  ggpubr::ggscatter(x = 'year', y = 'ttl', 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.coeff.args = list(method="pearson"),
                    add.params=list(color="dodgerblue", fill="lightgray"),
                    xlab = "Mean salinity (92m)", ylab = "Recruitment Estimate",
                    title="Lag one year 92m")


g1 = ggplot(ttl_larv2,
       aes(x = year, y = ttl)) + 
  geom_bar(stat = 'identity', fill = 'grey45', color = 'black') + 
  annotate('text', x = 1977,
           y = 152, label = '(a)', size = 5) +
  xlab('Year') +
  ylab('Total count tilefish larvae') +
  theme(axis.text.x = element_text(color = 'black',
                                   size = 13, angle = 45, 
                                   vjust = 1, hjust=1)) +
  scale_x_continuous('years', labels = NULL, breaks = years) +
  theme(text = element_text(size = 15), 
        axis.text = element_text(size = 15), 
        axis.title.y = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.ticks.length = unit(-1.4, "mm"),
        axis.text.x = element_blank()) +
  theme_facet()
g2 = ggplot(larv %>% filter(taxa_ichthyo == '170070000'),
            aes(x = year)) + 
  geom_bar(position = position_dodge(), 
           fill= 'grey45', color="black") + 
  xlab('') +
  ylab('Number of tilefish larval events') +
  annotate('text', x = 1977,
           y = 16, label = '(b)', size = 5) +
  theme(axis.text.x = element_text(color = 'black',
                                   size = 13, angle = 45, 
                                   vjust = 1, hjust=1)) +
  scale_x_continuous('Year', labels = as.character(years), breaks = years) +
  theme(text = element_text(size = 15), 
        axis.text = element_text(size = 15), 
        axis.title.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.ticks.length = unit(-1.4, "mm")) +
  theme_facet()
g1/g2
ggsave(path = here::here('figures'),'larval_summaries.png',
       width = 12, height = 8, units = "in")
