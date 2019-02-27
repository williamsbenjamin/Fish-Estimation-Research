library(geosphere)
self_reports <- self_reports %>% 
  select(-id) %>% 
  rename(true_match_id_cls = true_match_id)
sample_pop <- s_pop %>%
  unnest() %>%
  select(-c(cls_party:CLS_LONG)) %>% 
  rename(true_match_id_mrip = true_match_id)
tidy_all_matches_docks <- self_reports %>% 
  drop_na(CLS_ID) %>% 
  inner_join(sample_pop, by = "CLS_ID")

tidy_all_matches_docks <- tidy_all_matches_docks %>% 
  mutate(diff_total_catch = total_harvest - cls_harvest,
         diff_total_release = total_release - cls_release,
         diff_red_snapper = red_snapper_harvest - cls_red_snapper_harvest,
         km_difference = distHaversine(
                          as.matrix(tibble(CLS_LONG, CLS_LAT)),
                          as.matrix(tibble(SITE_LONG, SITE_LAT))) / 1000,
         diff_spec_caught = species_caught_mrip - species_caught_cls,
         diff_spec_rel = species_released_mrip - species_released_cls,
         diff_num_anglers = party - cls_party,
         diff_date = date - date_cls)
