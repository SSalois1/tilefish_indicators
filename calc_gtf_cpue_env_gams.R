library(mgcv)
library(gratia)
library(dplyr)

# CPUE >0
df.recl <- read.csv(here::here('gtf_sfob_cpue_env_with_lags_full_updated.csv'))
# all cpue with zeros
df.recl <- read.csv(here::here('gtf_sfob_cpue_env_with_lags_full_updated_with_zeros.csv'))

df.recl$sqrtcpue <- sqrt(df.recl$total_cpue)
hist(df.recl$total_cpue)
hist(df.recl$sqrtcpue)
hist(log(df.recl$total_cpue))
# does TS have trend? 
trend::cs.test(df.recl$total_cpue) # no!
# covariance matrix
GGally::ggpairs(df[,c(2:3, 20:22)])

hist(df$total_cpue)
hist(log(df$total_cpue))
hist(sqrt(df$total_cpue))
range(df$year)
# Cold pool extent: 
# Positive values == Larger
# Negative values == Smaller
# Persistence Index:
# Positive values == Longer
# Negative values == Shorter
# high shv: front pushed towards sbf
# low shv: front pushed inshore (more slope water on shelf)

### FINAL MODEL???
df.w.zeros <- df.recl %>% 
  mutate(cpue = total_cpue + 1,
         gs_pos = factor(ifelse(gsi_lag3 > 0, 'Northerly', 'Southerly')),
         cpe_size = factor(ifelse(cp_ext_lag3 > 0, 'Larger', 'Smaller')), 
         cpp_length = factor(ifelse(cp_pers_lag3 > 0, 'Longer', 'Shorter'))) 

# Put 70% of the data into the training set 
set.seed(111)
df_split <- rsample::initial_split(df, prop = 0.7)
# Create data frames for the two sets:
df_train <- rsample::training(df_split)
df_test  <- rsample::testing(df_split)
# set knots
K= 5
# Maybe we want to try different ways to add lat/lon? see below
# te(start_lon,start_lat,bs='ds') + 
mod <- mgcv::gam(cpue ~  
                         s(start_lon, start_lat, k = 18) +
                         ti(year,month, k = c(K,K), bs = c('tp','cc')) +
                         s(weighted_mean_sst, k = K) + 
                         s(mean_sst_lag3, k = K) + 
                          s(mean_bt, k = K) +
                          s(mean_bs, k = K) +
                         # s(bt_lag3, k = K) +
                        #  s(bs_lag3, k = K) +
                         s(shw_v_lag3, k = K) + 
                        # s(mean_shw_t, k = K) + 
                        # s(shw_t_lag3, k = K) +  
                        # s(shw_s_lag3, k = K) +
                         s(gsi, k = K) +
                          s(cp_pers_lag3, k = K) +
                   # s(weighted_mean_micro, k = K) +
                         s(micro_lag3, k = K) +
                         s(mean_sed, k = K) +
                         gs_pos + cpe_size, # +
                         #cpp_length,
                       data = df.w.zeros,
                       bs = 'cr', 
                       family = nb,
                       method = 'REML', 
                       select = TRUE) 


#Model summary
summary(mod)
plot(mod, pages = 1, all.terms = TRUE)
mgcViz::getViz(mod) %>% plot(all.Terms = T) %>% print(pages = 1)
#Plotting partial effects from each covariate - other ways to viz
gratia::draw(mod)
plot(mod, select = 5, shade = TRUE, rug = TRUE)
mgcViz::getViz(mod) %>% plot(all.Terms = T) %>% print(pages = 1)
### Next steps: 
# 1. diagnostics
# 2. touch-ups
# 3. model comparison/selection (tweedie vs negbin)
# 4. model validation (training vs test data)
# 



