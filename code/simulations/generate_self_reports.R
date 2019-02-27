#Need to generate all of the self-reports

# In 2017 actual mrip data, we have about 100 true matches
# Which is approx 7% of the sample (n2)
# So make n1 be 7% of N

n1 <- round(nrow(pop) * 0.07)  

# 40% of the CLS sampled trips actually made a report

cls_ids_cls <- cls17 %>% 
  select(CLS_ID) 

self_gen <- s_pop %>% 
  unnest() %>% 
  filter(CLS == 1) %>% 
  filter(CLS_ID %in% cls17$CLS_ID) %>% 
  select(cls_party : true_match_id)

additional_self_reps <- n1 - nrow(self_gen)
self_reports <- sample_n(tbl = cls_ids_cls,
           size = additional_self_reps,
           replace = T
         )

self_reports <- self_gen %>% 
  sample_n(additional_self_reps,
           replace = T) %>% 
  mutate(true_match_id = 0) %>% 
  mutate(CLS_ID = self_reports$CLS_ID) %>% 
  mutate(cls_part = round(cls_party + rnorm(additional_self_reps,
                                            0,0.5)),
         cls_harvest = round(cls_harvest + rnorm(additional_self_reps,
                                                 0,10)),
         cls_release = round(cls_release + rnorm(additional_self_reps,
                                                 0,20)),
         cls_red_snapper_harvest = round(cls_red_snapper_harvest +
                                           rnorm(additional_self_reps,
                                                 0,2)),
         species_caught_cls = round(species_caught_cls +
                                      rnorm(additional_self_reps,
                                            0,4)),
         species_released_cls = round(species_released_cls +
                                        rnorm(additional_self_reps,0,6)),
         date_cls = date_cls + rnorm(additional_self_reps,0,20),
         d_cls = day(date_cls),
         CLS_LAT = CLS_LAT + rnorm(additional_self_reps,0,3),
         CLS_LONG = CLS_LONG + rnorm(additional_self_reps,0,5)) %>% 
  mutate(cls_party = if_else(cls_part <=0,
                             cls_party,cls_part),
         cls_harvest = sapply(cls_harvest, neg_to_zero),
         cls_release = sapply(cls_release, neg_to_zero),
         cls_red_snapper_harvest = sapply(cls_red_snapper_harvest,
                                          neg_to_zero),
         species_caught_cls = sapply(species_caught_cls,
                                     neg_to_zero),
         species_released_cls = sapply(species_released_cls,
                                       neg_to_zero)) %>% 
  select(-cls_part) %>% 
  bind_rows(self_gen) %>% 
  mutate(trip_id = 1:n())



