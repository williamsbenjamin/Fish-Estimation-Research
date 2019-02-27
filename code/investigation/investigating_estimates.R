mrip_all_17 <- read_csv("data/mrip_tidy2_17.csv")

mrip_all_17 %>% 
  summarize(rs_h = sum(`RED SNAPPER_claim`,na.rm = T)) 

mrip_all_17 %>% 
  filter(reported == 1) %>% 
  nrow()

mrip_all_17 %>% 
  filter(reported == 1) %>% 
  summarize(rs_mrip = sum(`RED SNAPPER_claim`,na.rm = T),
            rs_cls = sum(`RED SNAPPER_kept`,na.rm = T))
462/1713
nrow(mrip_all_17)
109/1484

mrip_all_17 %>% 
  filter(reported == 1) %>% 
  ggplot(aes(`RED SNAPPER_kept`)) +
  geom_histogram()

mrip_all_17 %>% 
  filter(reported == 1) %>% 
  select(`RED SNAPPER_claim`,
         `RED SNAPPER_kept`) %>% 
  View()

mrip_all_17 %>% 
  ggplot(aes(`RED SNAPPER_claim`)) +
  geom_histogram() +
  facet_wrap(~reported)

mrip_all_17 %>% 
  group_by(reported) %>% 
  summarize(rs_sum = sum(`RED SNAPPER_claim`,na.rm = T),
            rs_mean = mean(`RED SNAPPER_claim`,na.rm = T),
            n = n())

mrip_all_17 %>% 
  filter(`RED SNAPPER_claim` > 0) %>% 
  group_by(reported) %>% 
  summarize(rs_sum = sum(`RED SNAPPER_claim`,na.rm = T),
            rs_mean = mean(`RED SNAPPER_claim`,na.rm = T),
            n = n())


cls_17 %>% 
  summarise(rs = sum(`RED SNAPPER_kept`,na.rm = T))

400 / 1713
109 / 1484

mrip_all_17 %>% 
  mutate(tot = wp_int*`RED SNAPPER_claim`) %>% 
  summarize(t = sum(tot,na.rm = T))


mrip_all_17 %>% 
  filter(reported == 1) %>% 
  mutate(red_sn = if_else(`RED SNAPPER_kept` > 0,
                          1,0)) %>% 
  group_by(red_sn) %>% 
  summarise(tot = sum(`RED SNAPPER_kept`,na.rm = T))

mrip_all_17 %>% 
  filter(reported == 1) %>% 
  select(`RED SNAPPER_claim`,`RED SNAPPER_kept`) %>% 
  filter(`RED SNAPPER_kept` > 0) %>% 
  nrow()

33/109 #% that reported RS
mrip_all_17 %>% 
  filter(`RED SNAPPER_claim` > 0) %>% 
  nrow()
178 / 1484 #%that caught RS

# 30% of reports include red snapper, 12% of intercepts include Red Snapper

mrip_all_17 %>% 
  group_by(reported) %>% 
  summarise(m_weight = mean(wp_int),
            s_weight = var(wp_int))
mrip_all_17 %>% 
  ggplot(aes(wp_int)) +
  geom_histogram()+
  facet_wrap(~reported,nrow = 2)

mrip_all_17 %>% 
  filter(reported == 1) %>% 
  select(`VERMILION SNAPPER_claim`,`VERMILION SNAPPER_kept`) %>% 
  filter(`VERMILION SNAPPER_kept` > 0) %>% 
  nrow()

26/109 #% that reported vS
mrip_all_17 %>% 
  filter(`VERMILION SNAPPER_claim` > 0) %>% 
  nrow()
189 / 1484 #%that caught vS
# 24% report Vermilio snapper vs 12% of intercepts include Vermilion


mrip_all_17 %>% 
  filter(reported == 1) %>% 
  select(`RED GROUPER_claim`,`RED GROUPER_kept`) %>% 
  filter(`RED GROUPER_kept` > 0) %>% 
  nrow()

12/109 #% that reported rg
mrip_all_17 %>% 
  filter(`RED GROUPER_claim` > 0) %>% 
  nrow()
80 / 1484 #%that caught rg
# 11% report RG vs 5% of intercepts include RG

mrip_all_17 %>% 
  summarise(m_mrip = mean(total_claim_mrip))
mrip_all_17 %>% 
  filter(reported == 1) %>% 
  summarise(m_cls = mean(total_kept_cls))

tidy_one <- read_csv("data/tidy_all_matches_rl_one_per.csv")
tidy_one  %>% 
  filter(rl_score > 13.5) %>% 
  summarise(rs_cl = sum(`RED SNAPPER_claim`))
tidy_one  %>% 
  filter(rl_score > 13.5) %>% 
  select(`RED SNAPPER_claim`,
         `RED SNAPPER_kept`) %>% 
  View()
