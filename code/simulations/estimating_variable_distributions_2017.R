library(tidyverse)
library(truncdist)
mripall <- read_csv("data/mrip_tidy2_17.csv")
mripall <- mripall %>% 
  select(-mrip_non_cls_claim,-mrip_non_cls_release)
mrip17 <- mripall %>%
  bind_cols(select(mripall, ends_with("claim")) %>%
              transmute(total_claim_mrip = rowSums(., na.rm = T))) %>%
  bind_cols(select(mripall, ends_with("release")) %>%
              transmute(total_release_mrip = rowSums(., na.rm = T)))

is.zero.na <- function(x){
  x == 0 | is.na(x)
}

mrip17 <- mrip17 %>% 
  bind_cols(
    select(mrip17,ends_with("claim")) %>%
      transmute(reported_species_claim_mrip = rowSums(!is.zero.na(.)))
  ) %>%
  bind_cols(
    select(mrip17,ends_with("release")) %>%
      transmute(reported_species_released_mrip = rowSums(!is.zero.na(.)))
  ) 
#Anglers per boat
mrip17 %>% 
 filter(PARTY < 15) %>% 
 ggplot(aes(PARTY)) + 
 geom_histogram(bins = 15)

mrip17 %>% 
  ggplot(aes(PARTY)) + 
  geom_histogram()

mrip17 %>% 
  filter(PARTY < 14) %>% 
  nrow()
mrip17 %>% 
  filter(PARTY < 14) %>% 
  count(PARTY) %>% 
  mutate(prop = n/1477) %>% 
  arrange(-prop)

#RED SNAPPER CLAIM
mrip17 %>% 
  filter(`RED SNAPPER_claim` == 0 |
           `RED SNAPPER_claim` > 30) %>% 
  nrow()
1310/1484
mrip17 %>% 
  filter(`RED SNAPPER_claim` > 0 &
           `RED SNAPPER_claim` < 30) %>% 
  summarise(m = mean(`RED SNAPPER_claim`))
mrip17 %>% 
  filter(`RED SNAPPER_claim` > 0) %>% 
  ggplot(aes(`RED SNAPPER_claim`)) + 
  geom_histogram(bins = 20)

mrip17 %>% 
  filter(`RED SNAPPER_claim` > 0 &
         `RED SNAPPER_claim` < 30 ) %>% 
  ggplot(aes(`RED SNAPPER_claim`)) + 
  geom_histogram(bins = 13)

mrip17 %>% 
  filter(`RED SNAPPER_claim` > 0 ) %>% 
  mutate(mean_rs = `RED SNAPPER_claim`/PARTY) %>% 
  filter(mean_rs < 3) %>%
  ggplot(aes(mean_rs)) +
  geom_histogram()

mrip17 %>% 
  mutate(mean_rs = `RED SNAPPER_claim`/PARTY) %>%
  filter(mean_rs < 3 &
           mean_rs >2) %>% 
  nrow()
9/1484
mrip17 %>% 
  filter(`RED SNAPPER_claim` > 0 ) %>% 
  mutate(mean_rs = `RED SNAPPER_claim`/PARTY) %>% 
  filter(mean_rs < 2) %>%
  ggplot(aes(mean_rs)) +
  geom_histogram(bins = 10)
mrip17 %>% 
  filter(`RED SNAPPER_claim` > 0 ) %>% 
  mutate(mean_rs = `RED SNAPPER_claim`/PARTY) %>% 
  filter(mean_rs < 2) %>%
  select(mean_rs) %>% View()
mrip17 %>% 
  filter(`RED SNAPPER_claim` > 0) %>% 
  summarise(mean_rs = mean(`RED SNAPPER_claim`/PARTY),
            sd_rs = sd(`RED SNAPPER_claim`/PARTY))
mrip17 %>% 
  filter(`RED SNAPPER_claim` > 0 ) %>% 
  nrow()
mrip17 %>% 
  filter(`RED SNAPPER_claim` > 0 ) %>% 
  mutate(mean_rs = `RED SNAPPER_claim`/PARTY) %>% 
  filter(mean_rs == 2) %>%
  nrow()
92/178
# Of those who caught snapper, 52.7 percent caught 
# an average of 2 RS 

mrip17 %>% 
  count(date) %>% 
  ggplot(aes(date,n)) +
  geom_col()

mrip17 %>% 
  count(WAVE) %>% 
  ggplot(aes(WAVE,n)) +
  geom_col()
mrip17 %>% 
  count(WAVE) %>% 
  mutate(prop = n/1484) %>% 
  arrange(-prop)

dates <- mrip17 %>% 
  select(date)

#total catch
mrip17 %>% 
  filter(total_claim_mrip >0 ) %>% 
  ggplot(aes(total_claim_mrip)) + 
  geom_histogram()

mrip17 %>% 
  filter(total_claim_mrip > 0) %>% 
  mutate(m = total_claim_mrip / PARTY) %>% 
  filter(m < 20) %>% 
  ggplot(aes(m))+
  geom_histogram()

mrip17 %>% 
  filter(total_claim_mrip > 0) %>% 
  mutate(m = total_claim_mrip / PARTY) %>% 
  summarise(mm = mean(m))
mrip17 %>% 
  filter(total_claim_mrip > 0 &
           total_claim_mrip < 100) %>% 
  mutate(m = total_claim_mrip / PARTY) %>% 
  ggplot(aes(m)) +
  geom_histogram()

mrip17 %>% 
  filter(total_claim_mrip > 0 &
           total_claim_mrip < 100) %>% 
  mutate(m = total_claim_mrip / PARTY) %>% 
  summarise(mm = mean(m))

mrip17 %>% 
  filter(total_claim_mrip > 0 ) %>% 
  summarise(m = mean(total_claim_mrip))
mrip17 %>% 
  filter(total_claim_mrip == 0 ) %>% 
  nrow()
203/nrow(mrip17)

#Total Release
mrip17 %>% 
  ggplot(aes(total_release_mrip)) +
  geom_histogram()
mrip17 %>% 
  filter(total_release_mrip > 0) %>% 
  ggplot(aes(total_release_mrip)) +
  geom_histogram()
mrip17 %>% 
  filter(total_release_mrip > 0 &
           total_release_mrip < 125) %>% 
  ggplot(aes(total_release_mrip)) +
  geom_histogram()

mrip17 %>% 
  filter(total_release_mrip > 0 & 
           total_release_mrip < 125) %>% 
  mutate(m = total_release_mrip / PARTY) %>% 
  summarise(mm = mean(m))

mrip17 %>% 
  filter(total_release_mrip == 0 ) %>% 
  nrow()
179 / nrow(mrip17)

#Number of Species caught 
mrip17 %>% 
  ggplot(aes(reported_species_claim_mrip)) +
  geom_histogram(bins = 10)

mrip17 %>% 
  count(reported_species_claim_mrip) %>% 
  mutate(prop = n / nrow(mrip17))

#Number of Species Released
mrip17 %>% 
  ggplot(aes(reported_species_released_mrip)) +
  geom_histogram(bins = 10)

mrip17 %>% 
  count(reported_species_released_mrip) %>% 
  mutate(prop = n / nrow(mrip17))

#CLS
mrip17 %>% 
  count(CLS)
