#for Charlie
library(tidyverse)
library(lubridate)
cls17 <- read_csv("cls_tidy2_17.csv")
mrip17 <- read_csv("mrip_tidy2_17.csv")
names(cls17)
names(mrip17)


cols <-c(22:105)
cls17[,cols] = apply(cls17[,cols],2,
                                function(x)as.numeric(x))  


cls17 <- cls17 %>%
  bind_cols(
    select(cls17,ends_with("kept")) %>%
      transmute(total_kept_cls = rowSums(.,na.rm = T))
  ) %>%
  bind_cols(
    select(cls17,ends_with("released")) %>%
      transmute(total_released_cls = rowSums(.,na.rm = T))
  ) 

cls17 <- cls17 %>%
  mutate(year = year(return_date_ymd))  

cls17 %>%
  filter(year == 2017) %>%
  mutate(cpue_tot = total_kept_cls/nbAnglers) %>%
  pull(cpue_tot) %>%
  mean(na.rm=T)

cls17 %>%
  filter(year == 2017) %>%
  mutate(cpue_tot = total_kept_cls/nbAnglers) %>%
  pull(cpue_tot) %>%
  var(na.rm=T)

cls17 %>%
  filter(year == 2017) %>%
  mutate(cput_tr = total_released_cls/nbAnglers) %>%
  pull(cput_tr) %>%
  mean(na.rm=T)

cls17 %>%
  filter(year == 2017) %>%
  mutate(cput_tr = total_released_cls/nbAnglers) %>%
  pull(cput_tr) %>%
  var(na.rm=T)

cls17 %>%
  filter(year == 2017) %>%
  mutate(cpe_rs_k = `RED SNAPPER_kept`/nbAnglers) %>%
  pull(cpe_rs_k) %>%
  mean(na.rm=T)

cls17 %>%
  filter(year == 2017) %>%
  mutate(cpe_rs_k = `RED SNAPPER_kept`/nbAnglers) %>%
  pull(cpe_rs_k) %>%
  var(na.rm=T)

cls17 %>%
  filter(year == 2017) %>%
  mutate(cpe_rg_k = /nbAnglers) %>%
  pull(cpe_rg_k) %>%
  mean(na.rm=T)

cls17 %>%
  filter(year == 2017) %>%
  mutate(cpe_rg_k = `RED GROUPER_kept`/nbAnglers) %>%
  pull(cpe_rg_k) %>%
  var(na.rm=T)

cols2 <-c(17:96)
mrip17[,cols2] = apply(mrip17[,cols2],2,function(x)as.numeric(x))  

mrip17 <- mrip17 %>%
  bind_cols(
    select(mrip17,ends_with("claim")) %>%
      transmute(total_claim_mrip = rowSums(.,na.rm = T))
  ) %>%
  bind_cols(
    select(mrip17,ends_with("release")) %>%
      transmute(total_release_mrip = rowSums(.,na.rm = T))
  ) 

###

mrip17 <- mrip17 %>%
  mutate(w_tot_c = wp_int*total_claim_mrip,
         w_tot_r = wp_int*total_release_mrip)
mrip17 <- mrip17 %>%
  mutate(cpue_claim = total_claim_mrip/PARTY,
         cpue_rel = total_release_mrip/PARTY,
         cpue_red_c = `RED SNAPPER_claim`/PARTY,
         cpue_red_r = `RED SNAPPER_release`/PARTY)

mrip17 %>%
  select(wp_int,w_tot_c,w_tot_r) %>%
  summarize(mean_c = sum(w_tot_c)/sum(wp_int),
            mean_r = sum(w_tot_r)/sum(wp_int))

####
mrip_surv <- svydesign(id = ~0,
                       weights= ~wp_int,
                       data=mrip17)

svymean(~cpue_claim,mrip_surv,na.rm=T)
svyvar(~cpue_claim,mrip_surv,na.rm=T)


svymean(~cpue_rel,mrip_surv,na.rm=T)
svyvar(~cpue_rel,mrip_surv,na.rm=T)


svymean(~cpue_red_c,mrip_surv,na.rm=T)
svyvar(~cpue_red_c,mrip_surv,na.rm=T)


#some tests
wilcox.test(cls17$total_kept_cls,
            mrip17$total_claim_mrip)

wilcox.test(cls17$total_released_cls,
            mrip17$total_release_mrip)


#weekday vs weekend

cls17 <- cls17 %>%
  mutate(days = wday(return_date_ymd,label=T)) %>%
  mutate(weekend = if_else(days %in% c("Sat","Sun"),
                           "weekend","weekday")) 
cls17 %>%
  group_by(weekend) %>%
  summarise(average_total_kept = mean(total_kept_cls/nbAnglers),
            variance_total_kept = var(total_kept_cls/nbAnglers),
            average_total_released = mean(total_released_cls/nbAnglers),
            variance_total_released = var(total_released_cls/nbAnglers))

mrip17 <- mrip17 %>%
  mutate(days = wday(date,label=T)) %>%
  mutate(weekend = if_else(days %in% c("Sat","Sun"),
                           "weekend","weekday")) 
mrip17 %>%
  group_by(weekend) %>%
  summarise(average_total_claim = mean(total_claim_mrip),
            variance_total_claim = var(total_claim_mrip),
            average_total_release = mean(total_release_mrip),
            variance_total_release = var(total_release_mrip))


#some tests
wilcox.test(cls17 %>%
              filter(weekend == "weekday") %>%
              pull(total_kept_cls),
            mrip17 %>%
              filter(weekend == "weekday") %>%
              pull(total_claim_mrip))

wilcox.test(cls17 %>%
              filter(weekend == "weekend") %>%
              pull(total_kept_cls),
            mrip17 %>%
              filter(weekend == "weekend") %>%
              pull(total_claim_mrip))
