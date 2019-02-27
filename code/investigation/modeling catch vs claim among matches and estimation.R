cut_16 <- read_csv("data/mrip_all_16_cutoff_public.csv")
cut_17 <- read_csv("data/mrip_all_17_cutoff_public.csv")

names(cut_16)

test1 <- cut_17 %>% 
  mutate(`BANDED RUDDERFISH_kept` = as.numeric(`BANDED RUDDERFISH_kept`),
         `ALMACO JACK_kept` = as.numeric(`ALMACO JACK_kept`),
         `ALMACO JACK_released` = as.numeric(`ALMACO JACK_released`),
         `GRAY SNAPPER_kept` = as.numeric(`GRAY SNAPPER_kept`),
         `GRAY SNAPPER_released` = as.numeric(`GRAY SNAPPER_released`),
         `LITTLE TUNNY_kept` = as.numeric(`LITTLE TUNNY_kept`),
         `LITTLE TUNNY_released` = as.numeric(`LITTLE TUNNY_released`),
         `SANDBAR SHARK_claim` = as.numeric(`SANDBAR SHARK_claim`),
         `SANDBAR SHARK_release` = as.numeric(`SANDBAR SHARK_release`)) %>% 
  bind_rows(cut_16) %>% 
  select(total_kept_cls,total_claim_mrip)

test1 %>% 
  filter(total_kept_cls > 0) %>% 
  mutate(diff = total_kept_cls - total_claim_mrip) %>% 
  summarise(cor = cor(total_kept_cls,total_claim_mrip))

lm.y <- test1 %>%
           filter(total_kept_cls > 0) %>%
           pull(total_claim_mrip)
lm.x <- test1 %>%
  filter(total_kept_cls > 0) %>%
  pull(total_kept_cls)

test.lm <- lm( lm.y ~ lm.x )

summary(test.lm)

predict(test.lm,newdata =  data.frame(lm.x))
predict(test.lm)

test1 %>% 
  filter(total_kept_cls > 0) %>% 
  ggplot(aes(total_kept_cls,total_claim_mrip)) +
  geom_point() +
  geom_smooth(method = "lm") 
  
predict(test.lm, data.frame(lm.x = cls_17$total_kept_cls), 
        se.fit = T)

test.pred <- predict(test.lm, data.frame(lm.x = cls_17$total_kept_cls), 
        se.fit = T)$fit
sum(cls_17$total_kept_cls)
sum(test.pred)

##try with a lower cutoff

mrip_all_16 <- read_csv("data/mrip_all_16_public.csv")
mrip_all_17 <- read_csv("data/mrip_all_17_public.csv")

mrip_all_16_cut_1 <- mrip_all_16 %>% 
  filter(rl_score > 10)
mrip_all_17_cut_1 <- mrip_all_17 %>% 
  filter(rl_score > 10)

test2 <-  mrip_all_17_cut_1 %>% 
  mutate(`BANDED RUDDERFISH_kept` = as.numeric(`BANDED RUDDERFISH_kept`),
         `ALMACO JACK_kept` = as.numeric(`ALMACO JACK_kept`),
         `ALMACO JACK_released` = as.numeric(`ALMACO JACK_released`),
         `GRAY SNAPPER_kept` = as.numeric(`GRAY SNAPPER_kept`),
         `GRAY SNAPPER_released` = as.numeric(`GRAY SNAPPER_released`),
         `LITTLE TUNNY_kept` = as.numeric(`LITTLE TUNNY_kept`),
         `LITTLE TUNNY_released` = as.numeric(`LITTLE TUNNY_released`),
         `SANDBAR SHARK_claim` = as.numeric(`SANDBAR SHARK_claim`),
         `SANDBAR SHARK_release` = as.numeric(`SANDBAR SHARK_release`)) %>% 
  bind_rows(mrip_all_16_cut_1) %>% 
  select(total_kept_cls,total_claim_mrip)

nrow(test2)

lm.y.2 <- test2 %>%
  filter(total_kept_cls > 0) %>%
  pull(total_claim_mrip)
lm.x.2 <- test2 %>%
  filter(total_kept_cls > 0) %>%
  pull(total_kept_cls)

test.2.lm <- lm( lm.y.2 ~ lm.x.2 )

summary(test.lm)

test2 %>% 
  filter(total_kept_cls > 0) %>% 
  ggplot(aes(total_kept_cls,total_claim_mrip)) +
  geom_point() +
  geom_smooth(method = "lm") 

#predict(test.2.lm, data.frame(lm.x.2 = cls_17$total_kept_cls), 
#       se.fit = T)

test.pred.2 <- predict(test.2.lm, data.frame(lm.x.2 = cls_17$total_kept_cls), 
                     se.fit = T)$fit
sum(cls_17$total_kept_cls)
sum(test.pred.2)

