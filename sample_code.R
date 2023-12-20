# Metadata ----

### Project name: ESP code tutorial
### Code purpose: Example of making code more efficient
### adapted from SST section of Stephanie's .Rmd

### Author: AT
### Date started: 2023-12-20

### Code reviewer:
### Date reviewed:

# Libraries & functions ----
`%>%` <- magrittr::`%>%`
library(dplyr)
library(ggplot2)
library(ggpubr)

season_stats <- function(data){
  lm_winter<-lm(recruit_est ~ weighted_mean_sst, data=data)
  summary(lm_winter) %>%
    print()
  cor.test(data$weighted_mean_sst, data$recruit_est, method=c("pearson")) %>%
    print()
}

season_plts <- function(data, season_name) {
  plt1 <- data %>%
    ggscatter(y = "recruit_est", x = "weighted_mean_sst", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.coeff.args = list(method="pearson"),
              add.params=list(color="dodgerblue", fill="lightgray"),
              ylab = "Recruitment Estimate", 
              xlab = paste(season_name, "Weighted Mean SST"),
              title = season_name)
  
  plt2 <- data %>%  
    ggplot(aes(y = recruit_est, 
               x = weighted_mean_sst,
               fill = as.factor(month)))+
    geom_point()
  
  print(plt1)
  print(plt2)
}

# Data ----

# sst
sst<-read.csv(here::here('data/sst/sst_ts_gtf_strata.csv')) %>%
  mutate(mean_sst_lag1 = lag(weighted_mean_sst,12))
recruit <- read.csv(here::here('data/assessment_data/tilefish_rec_estimate_2021.csv'))

# Join with recruit estimate
df <- dplyr::left_join(sst, 
                       recruit,
                       by = "year")

# Analyses ----

## winter ----

season_stats(df %>%
               dplyr::filter(month %in% 1:3))
season_plts(df %>%
              dplyr::filter(month %in% 1:3),
            season_name = "Winter")

## looping over seasons ----
key <- list(c("Winter", 1, 3),
            c("Spring", 4, 6),
            c("Summer", 7, 9),
            c("Fall", 10, 12))
for(i in 1:length(key)){
  dat <- df %>%
    dplyr::filter(month %in% c(key[[i]][2]:key[[i]][3]))
  
  print(key[[i]][1])
  
  season_stats(dat)
  season_plts(dat,
              season_name = key[[i]][1])
}

## mapping over seasons ----
key <- list(c("Winter", 1, 3),
            c("Spring", 4, 6),
            c("Summer", 7, 9),
            c("Fall", 10, 12))

purrr::map(key,
           ~ { dat <- df %>%
             dplyr::filter(month %in% .x[2]:.x[3])
           
           print(.x[1])
           season_stats(dat)
           season_plts(dat,
                       season_name = .x[1])
             })
