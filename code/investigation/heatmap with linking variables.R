library(scales)
tidy_all_matches_one_per <- read_csv("data/tidy_all_matches_rl_one_per.csv")
dat <- tidy_all_matches_one_per
dat %>% 
  sample_n(10) %>% 
  ggplot(aes())


dat %>% 
  select(tripID,contains("score")) %>% 
  sample_n(150) %>% 
  gather(contains("score"),key = "variable",value = "value") %>%
  filter(variable != "score_diff_red_snapper_caught") %>% 
  filter(variable != "rl_score") %>% 
  group_by(variable) %>% 
  mutate(value = rescale(value)) %>% 
  ungroup() %>% 
  mutate(tripID = factor(tripID)) %>% 
  ggplot(aes(variable,tripID)) +
  geom_tile(aes(fill = value),colour = "white") + 
  scale_fill_gradient(low = "red", high = "green",
                      na.value = "black") +
  ylab(NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


