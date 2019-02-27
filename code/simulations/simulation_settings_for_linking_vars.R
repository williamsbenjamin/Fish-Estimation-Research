#########
# Simulation Settings
#########


##########
# No Extra Errors in Linking Variables
##########

######################################################################
pop <- pop %>% 
  mutate(cls_party = round(party + rnorm(nrow(pop),0,1)),
         cls_harvest = round(total_harvest + rnorm(nrow(pop),0,18)),
         cls_release = round(total_release + rnorm(nrow(pop),0,37)),
         cls_red_snapper_harvest = round(red_snapper_harvest + 
                                           rnorm(nrow(pop),0,1.7)),
         species_caught_cls = round(species_caught_mrip + 
                                      rnorm(nrow(pop),0,1.5)),
         species_released_cls = round(species_released_mrip + 
                                        rnorm(nrow(pop),0,1.4)),
         date_cls = date + rnorm(nrow(pop),0,0.5),
         d_cls = day(date_cls),
         CLS_LAT = SITE_LAT + rnorm(nrow(pop),0,5),
         CLS_LONG = SITE_LONG + rnorm(nrow(pop),0,.5)) 

pop <- pop %>% 
  mutate(cls_party = if_else(cls_party <= 0,
                             party,cls_party),
         cls_harvest = sapply(cls_harvest, neg_to_zero),
         cls_release = sapply(cls_release, neg_to_zero),
         cls_red_snapper_harvest = sapply(cls_red_snapper_harvest,
                                          neg_to_zero),
         species_caught_cls = sapply(species_caught_cls,
                                     neg_to_zero),
         species_released_cls = sapply(species_released_cls,
                                       neg_to_zero))

pop <- pop %>% 
  mutate(cls_party = if_else(CLS == 1,
                             cls_party,0),
         cls_harvest = if_else(CLS == 1,
                               cls_harvest,0),
         cls_release = if_else(CLS == 1,
                               cls_release,0),
         cls_red_snapper_harvest = if_else(CLS == 1,
                                           cls_red_snapper_harvest,0),
         species_caught_cls = if_else(CLS == 1,
                                      species_caught_cls,0),
         species_released_cls = if_else(CLS == 1,
                                        species_released_cls,0),
         CLS_LAT = if_else(CLS == 1,
                           CLS_LAT,0),
         CLS_LONG = if_else(CLS == 1,
                            CLS_LONG,0)
  )

#Check correlations
pop %>% 
  filter(CLS == 1) %>% 
  mutate(d_mrip = day(date)) %>% 
  summarize(corr_party = cor(party,cls_party),
            corr_harv = cor(total_harvest,cls_harvest),
            corr_rel = cor(total_release,cls_release),
            corr_rs = cor(cls_red_snapper_harvest,
                          red_snapper_harvest),
            corr_s_c = cor(species_caught_mrip,
                           species_caught_cls),
            corr_s_r = cor(species_released_mrip,
                           species_released_cls),
            corr_date = cor(d_cls,d_mrip),
            corr_lat = cor(CLS_LAT,
                           SITE_LAT,use = "complete.obs"),
            corr_long = cor(CLS_LONG,
                            SITE_LONG,use = "complete.obs")) 


###################################################################

####################
# Additional Errors in Linking Variables
####################
pop <- pop %>% 
mutate(cls_party = round(party + rnorm(nrow(pop),0,3)),
       cls_harvest = round(total_harvest + 
                             rnorm(nrow(pop),0,30)),
       cls_release = round(total_release + 
                             rnorm(nrow(pop),0,55)),
       cls_red_snapper_harvest = round(
                        red_snapper_harvest + 
                                       rnorm(nrow(pop),
                                             0,4)),
       species_caught_cls = round(species_caught_mrip + 
                                    rnorm(nrow(pop),
                                          0,3)),
       species_released_cls = round(
                            species_released_mrip + 
                                      rnorm(nrow(pop),
                                            0,3)),
       date_cls = date + rnorm(nrow(pop),0,1),
       d_cls = day(date_cls),
       CLS_LAT = SITE_LAT + rnorm(nrow(pop),0,9),
       CLS_LONG = SITE_LONG + rnorm(nrow(pop),0,1)) 

pop <- pop %>% 
  mutate(cls_party = if_else(cls_party <= 0,
                             party,cls_party),
         cls_harvest = sapply(cls_harvest, neg_to_zero),
         cls_release = sapply(cls_release, neg_to_zero),
         cls_red_snapper_harvest = sapply(cls_red_snapper_harvest,
                                          neg_to_zero),
         species_caught_cls = sapply(species_caught_cls,
                                     neg_to_zero),
         species_released_cls = sapply(species_released_cls,
                                       neg_to_zero))

pop <- pop %>% 
  mutate(cls_party = if_else(CLS == 1,
                             cls_party,0),
         cls_harvest = if_else(CLS == 1,
                               cls_harvest,0),
         cls_release = if_else(CLS == 1,
                               cls_release,0),
         cls_red_snapper_harvest = if_else(CLS == 1,
                                           cls_red_snapper_harvest,0),
         species_caught_cls = if_else(CLS == 1,
                                      species_caught_cls,0),
         species_released_cls = if_else(CLS == 1,
                                        species_released_cls,0),
         CLS_LAT = if_else(CLS == 1,
                           CLS_LAT,0),
         CLS_LONG = if_else(CLS == 1,
                            CLS_LONG,0)
  )

#Check correlations
pop %>% 
  filter(CLS == 1) %>% 
  mutate(d_mrip = day(date)) %>% 
  summarize(corr_party = cor(party,cls_party),
            corr_harv = cor(total_harvest,cls_harvest),
            corr_rel = cor(total_release,cls_release),
            corr_rs = cor(cls_red_snapper_harvest,
                          red_snapper_harvest),
            corr_s_c = cor(species_caught_mrip,
                           species_caught_cls),
            corr_s_r = cor(species_released_mrip,
                           species_released_cls),
            corr_date = cor(d_cls,d_mrip),
            corr_lat = cor(CLS_LAT,
                           SITE_LAT,use = "complete.obs"),
            corr_long = cor(CLS_LONG,
                            SITE_LONG,use = "complete.obs")) 

