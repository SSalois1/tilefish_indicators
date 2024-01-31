library(mgcv)
library(gratia)
library(dplyr)
#library(spatialRF) # for vif

# all cpue with zeros
df <- read.csv(here::here('gtf_sfob_cpue_env_with_lags_full_updated_with_zeros.csv'))

hist(df$total_cpue)
hist(sqrt(df$total_cpue))
hist(log(df$total_cpue))
# does TS have trend? 
df.ts <- df %>% arrange(year,month) %>%
  group_by(year) %>% 
  summarise(cpue = mean(total_cpue))
plot(df.ts$year,df.ts$cpue, t='l')
df.ts.even = df.ts[-(nrow(df.ts)+1)/2, ] # need even # of observations
trend::cs.test(df.ts$cpue) # no!
summary(lm(df.ts.even$cpue ~ df.ts.even$year))


# Cold pool extent: 
# Positive values == Larger
# Negative values == Smaller
# Persistence Index:
# Positive values == Longer
# Negative values == Shorter
# high shv: front pushed towards sbf
# low shv: front pushed inshore (more slope water on shelf)

### Create factors
df <- df %>% 
  mutate(cpue = total_cpue + 1,
         gs_pos = factor(ifelse(gsi > 0, 'Northerly', 'Southerly')), 
         gs_pos_lag3 = factor(ifelse(gsi_lag3 > 0, 'Northerly', 'Southerly')),
         cpe_size = factor(ifelse(cp_ext_lag3 > 0, 'Larger', 'Smaller')), 
         cpp_length = factor(ifelse(cp_pers_lag3 > 0, 'Longer', 'Shorter'))) 

# Put 70% of the data into the training set 
set.seed(141)
df_split <- rsample::initial_split(df, prop = 0.7)
# Create data frames for the two sets:
df_train <- rsample::training(df_split)
df_test  <- rsample::testing(df_split)
# set knots
K= 5
### Full Model 
mod <- mgcv::gam(cpue ~  #te(start_lon,start_lat,bs='ds') +
                          s(start_lon, start_lat, k = 18) +
                         ti(year,month, k = c(K,K), bs = c('tp','cc')) +
                         s(weighted_mean_sst, k = K) + 
                         s(mean_sst_lag3, k = K) + 
                          s(mean_bt, k = K) +
                          s(mean_bs, k = K) +
                         # s(bt_lag3, k = K) +
                         # s(bs_lag3, k = K) +
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
                       data = df_train,
                       bs = 'cr', 
                       family = nb,
                       method = 'REML', 
                       select = TRUE) 


#Model summary
summary(mod)
plot(mod, pages = 1, all.terms = TRUE)
mgcViz::getViz(mod) %>% plot(all.Terms = T) %>% print(pages = 1)

# ar there significant patterns in residuals?  
# If so, change K value (# of basis functions)
mgcv::gam.check(mod)
mgcViz::mod %>% getViz() %>% check()

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
# ------ Variance Inflation Factor --------- #
# Testing for multicollinearity - vif measures how much a variable is
# contributing to the standard error in the regression 
# VIF equal to 1 = variables are not correlated
# VIF between 1 and 5 = variables are moderately correlated 
# VIF greater than 5 = variables are highly correlated
# --- data frame of model variables --- #
df.vars <- df %>% 
  select(year, month, start_lon, start_lat,
         weighted_mean_sst, mean_sst_lag3, 
         mean_bt, bt_lag3, mean_bs, bs_lag3, 
         mean_shw_t, shw_v_lag3, shw_t_lag3, shw_s_lag3,
         cp_pers_lag3, cp_ext_lag3, gsi, gsi_lag3, 
         weighted_mean_micro, micro_lag3, mean_sed) %>%
  tidyr::drop_na()
# ------ VIF --------- #
vif_values = spatialRF::vif(df.vars) 
format(vif_values, scientific = F, digits = 3)

# ------ CORRELATIONS --------- #
# Create correlation matrix
corrplot::corrplot(cor(df.vars, method = 'spearman'),
         method = "color", # color, circle, square, number, etc
         type = "upper")  # show only upper side
corrplot::corrplot(cor(df.vars, method = 'spearman'),
         method = 'number', 
         number.cex = 0.7, 
         type = 'upper', 
         order = 'AOE')
# ---  model adjusted for multicollinearity - #
#set.seed(115)
# set knots
K= 5
# --- final model ---- #
mod.f <- mgcv::gam(cpue ~  te(start_lon,start_lat,bs='ds') +
                     ti(year,month, k = c(K,K), bs = c('tp','cc')) +
                     s(mean_bt, k = 10) +
                     s(mean_bs, k = 10) +
                     s(shw_v_lag3, k = K) + 
                     s(mean_shw_t, k = 7) + 
                     s(micro_lag3, k = 5) +
                     s(mean_sed, k = K) +
                     gs_pos + gs_pos_lag3,
                   data = df_train,
                   bs = 'cr', 
                   family=tw(),
                   method = 'REML', 
                   select = TRUE) 
summary(mod.f)
plot(mod.f, pages = 1, all.terms = TRUE)
mgcViz::getViz(mod.f) %>% plot(all.Terms = T) %>% print(pages = 1)
# ------ Diagnostic plot --------- #
gam.check(mod.f)
mod.f %>% mgcViz::getViz() %>% check()

# ------ Concurvity --------- #
concurvity(mod.f) # if high values do step 2 below
concurvity(mod.f, full = FALSE)
dsm::vis_concurvity(mod.f)

# ------ Model selection --------- #

train.lims <- data.frame("lon" = df_train$start_lon,
                         "lat" = df_train$start_lat,
                         "cpue" = df_train$cpue)

pred_m1 <- predict(mod, df_test, type = "response", se.fit = TRUE)
pred.lims.m1 <- data.frame("lon" = df_test$start_lon,
                           "lat" = df_test$start_lat,
                           "fit" = pred_m1$fit)
# original model
pred_m2 <- predict(mod.f, df_test, type = "response", se.fit = TRUE)
pred.lims.m2 <- data.frame("lon" = df_test$start_lon,
                           "lat" = df_test$start_lat,
                           "fit" = pred_m2$fit)

# Taking two vectors
actual = na.omit(train.lims$cpue)     
predicted.m1 = na.omit(pred.lims.m1$fit)      
predicted.m2 = na.omit(pred.lims.m2$fit)      

# Calculating RMSE       
results = data.frame(mod = Metrics::rmse(actual, predicted.m1), 
                     mod.f = Metrics::rmse(actual, predicted.m2))

results[1] < results[2]

# ------ Model validation --------- #

# Predict # 
pred <- predict(mod.f, newdata = df_test, type = "response",
                se.fit = TRUE)

pred.lims <- data.frame("lon" = df_test$start_lon,
                        "lat" = df_test$start_lat,
                        "fit" = pred$fit)


### getting ready to plot
predicted <- pred.lims %>% tidyr::drop_na()
actual <- df_train %>% mutate(lon = start_lon,
                              lat = start_lat) %>% 
  dplyr::select('lon', 'lat', 'cpue') %>% 
  tidyr::drop_na()
agreement = data.frame(x = actual$lon, y = actual$lat, 
                       value = actual$cpue/predicted$fit)

# Rasterizing data
v1 <- terra::vect(agreement, geom=c('x', 'y'))
r1 <- terra::rast(v1)

nams <- names(v1)

agreerast <- lapply(nams, function(x) {
  terra::rasterize(v1, r1,
                   field = x
  )
})

# Merge (bind) all objects
agreerast <- do.call("c", agreerast)

#terra::plot(agreerast)
agreerast_df <- as.data.frame(agreerast, xy=TRUE)

# For the map
pal <- wesanderson::wes_palette("Zissou1", 10, type = "continuous")
# scales::show_col(pal2.1)
bathy <- marmap::getNOAA.bathy(-82,-63, 25, 43)
bathy = marmap::fortify.bathy(bathy)
blues = colorRampPalette(RColorBrewer::brewer.pal(9,'Blues'))(25)
depths <- c(0,50,100,200,300,500,Inf)
depths2 <- c('0','50','100','200','>300')
blues2 <- blues[c(5,7,9,11,13,24)]
US.areas <- sf::st_read(here::here('shapefiles/USA.shp'), quiet = TRUE)

# plotting the map 
ggplot() +
  geom_contour_filled(data = bathy,
                      aes(x=x,y=y,z=-1*z),
                      breaks=c(0,50,100,250,500,Inf),
                      linewidth=c(0.3)) + 
  scale_fill_manual(values = blues2, # 5:20
                    name = paste("Depth (m)"),
                    labels = depths2) +
  geom_contour(data = bathy,
               aes(x=x,y=y,z=-1*z),
               breaks=c(0,50,100,250,500,Inf),
               size=c(0.3), 
               col = 'lightgrey') +
  ggnewscale::new_scale_fill() +
  geom_sf(data = US.areas %>% sf::st_as_sf(),color = 'gray20', fill = '#cbdbcd') +
  geom_point(data = df, 
             aes(x = start_lon, y = start_lat, 
                 size =cpue), color = 'black', alpha = 0.5)  +
  geom_tile(data = agreerast_df,
            aes(x = x, y = y, 
                fill = last), color = 'black',
            alpha = 0.8)+
  scale_fill_gradientn('Agreement', colours = pal, 
                       na.value = NA) +
  coord_sf(xlim = c(-76,-66.5), ylim = c(34.5,42), datum = sf::st_crs(4326))  +
  labs(fill = 'Agreement') +
  xlab('Longitude') + 
  ylab('Latitude') +
  theme_bw()+  
theme(legend.position = c(0.85, 0.3)) +
  theme(legend.background = element_rect(linetype="solid", 
                                         colour ="grey20")) +
  theme(legend.key.size = unit(0.5, 'cm')) + 
  theme(axis.text = element_text(size = 13), 
        axis.title = element_text(size = 15),
        axis.ticks.length = unit(-1.4, "mm")) 

ggsave(path = here::here('figures'),'model_validation2.png', 
       width = 10, height = 8, units = "in", dpi = 300)



















# ------ Some extra runs -------- #
# The gams below were used while deciding about cold pool and shelf
# water volume, both had high multicollinearity  


# shelf water volume no cold pool 
mod.a <- mgcv::gam(cpue ~  #te(start_lon,start_lat,bs='ds') +
                     s(start_lon, start_lat, k = 18) +
                     ti(year,month, k = c(K,K), bs = c('tp','cc')) +
                     s(mean_bt, k = K) +
                     s(mean_bs, k = K) +
                     s(bt_lag3, k = K) +
                     s(bs_lag3, k = K) +
                     s(shw_v_lag3, k = K) + 
                     s(mean_shw_t, k = K) + 
                     s(gsi, k = K) +
                     s(micro_lag3, k = K) +
                     s(mean_sed, k = K),
                   data = df,
                   bs = 'cr', 
                   family = nb,
                   method = 'REML', 
                   select = TRUE) 
# just cp extent lag 3
mod.b <- mgcv::gam(cpue ~  #te(start_lon,start_lat,bs='ds') +
                     s(start_lon, start_lat, k = 18) +
                     ti(year,month, k = c(K,K), bs = c('tp','cc')) +
                     s(mean_bt, k = K) +
                     s(mean_bs, k = K) +
                     s(bt_lag3, k = K) +
                     s(bs_lag3, k = K) +
                     s(cp_ext_lag3, k = K) + 
                     s(mean_shw_t, k = K) + 
                     s(gsi, k = K) +
                     s(micro_lag3, k = K) +
                     s(mean_sed, k = K),
                   data = df,
                   bs = 'cr', 
                   family = nb,
                   method = 'REML', 
                   select = TRUE) 
# just cp persistent lag 3
mod.c <- mgcv::gam(cpue ~  #te(start_lon,start_lat,bs='ds') +
                     s(start_lon, start_lat, k = 18) +
                     ti(year,month, k = c(K,K), bs = c('tp','cc')) +
                     s(mean_bt, k = K) +
                     s(mean_bs, k = K) +
                     s(bt_lag3, k = K) +
                     s(bs_lag3, k = K) +
                     s(cp_pers_lag3, k = K) + 
                     s(mean_shw_t, k = K) + 
                     s(gsi, k = K) +
                     s(micro_lag3, k = K) +
                     s(mean_sed, k = K),
                   data = df,
                   bs = 'cr', 
                   family = nb,
                   method = 'REML', 
                   select = TRUE) 

summary(mod.a)
summary(mod.b)
summary(mod.c)
plot(mod.a, pages = 1, all.terms = TRUE)
plot(mod.b, pages = 1, all.terms = TRUE)
plot(mod.c, pages = 1, all.terms = TRUE)
