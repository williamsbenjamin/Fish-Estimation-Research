
cut <- c(5:25)
cut_tib <- tibble()
for(i in 1:length(cut)){
ex_match <- tidy_all_matches_one_per %>%
  select(rl_score, true_match_id_cls, true_match_id_mrip) %>%
  mutate(link = if_else(rl_score > cut[i],
                         1,0)) %>% 
  mutate(cut = cut[i]) %>% 
  mutate(
    match_status = case_when( 
      link == 1 & true_match_id_mrip == 0 ~ "false-pos",
      link == 0 & true_match_id_mrip != 0 &
        true_match_id_cls == true_match_id_mrip ~ "false-neg",
      link == 0 & true_match_id_mrip != 0 &
        true_match_id_cls != true_match_id_mrip ~ "false-neg",
      link == 1 & true_match_id_mrip != 0 &
        true_match_id_cls != true_match_id_mrip ~ "mismatch",
      link == 0 & true_match_id_mrip == 0 ~ "true-neg",
      link == 1 & true_match_id_mrip != 0 &
        true_match_id_cls == true_match_id_mrip ~ "true-pos"
    )
  ) 
cut_tib <- bind_rows(cut_tib,ex_match)
}

cut_tib %>% 
  group_by(cut) %>% 
  summarize(fp_rate = sum(match_status == "false-pos" &
                            link == 1) / 
              sum(link == 1),
            fn_rate = sum(match_status == "false-neg" & 
                            link == 0) / 
              sum(link == 0),
            tp_rate = sum(match_status == "true-pos" &
                            link == 1) / 
              sum(link == 1)
            ) 

cut_tib %>% 
  ggplot(aes(rl_score)) +
  geom_histogram(aes(fill = match_status)) +
  facet_wrap(~match_status)
cut_tib %>% 
  filter(cut %in% c(10:15)) %>% 
  ggplot(aes(rl_score)) +
  geom_histogram(aes(fill = match_status),
                 color = "black") + 
  facet_wrap(~cut)

#
matches <- tidy_all_matches_one_per %>% 
  filter(rl_score > 13) %>% 
  mutate(link = 1)

match_ids <- pull(matches,id)

non_matches <- sample_pop %>% 
  filter(!id %in% match_ids)

matched_samp <- non_matches %>% 
  bind_rows(matches)

tidy_all_matches_one_per %>% 
  filter(rl_score > 12) %>% 
  nrow()
  
# number of true positives
tidy_all_matches_one_per %>% 
  filter(rl_score > 13) %>% 
  filter(true_match_id_mrip != 0 &
           true_match_id_cls == true_match_id_mrip) %>% 
  nrow()

# number of false-negatives
tidy_all_matches_one_per %>% 
  filter(rl_score <= 13) %>% 
  filter(true_match_id_mrip != 0 &
           true_match_id_cls == true_match_id_mrip) %>% 
  nrow()

# mismatches
tidy_all_matches_one_per %>% 
  filter(true_match_id_mrip != 0 &
           true_match_id_cls != true_match_id_mrip) %>% 
  nrow()

####

true_match_s <- nrow(filter(sample_pop,true_match_id_mrip != 0))
cut <- c(5:25)
cut_tib <- tibble()
links <- c()
for(i in 1:length(cut)){
  links[i] <- nrow(filter(tidy_all_matches_one_per,rl_score > cut[i]))
}
cutoff <- cut[min(abs(links - true_match_s))]
