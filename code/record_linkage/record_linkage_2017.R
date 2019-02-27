library(tidyverse)
library(lubridate)

# use auto_rl_algorithm.R for Record Linkage


#take the cls only and mrip only datasets
#for the actual record linkage 
#Do a little wrangling of them too
mrip_tidy2_17 <- read_csv("mrip_tidy2_17.csv")
cls_tidy2_17 <- read_csv("cls_tidy2_17.csv")

#Get dataset of the mrip trips with a cls id

mrip_with_clsid <- mrip_tidy2_17 %>%
  filter(CLS_ID != "NA")

nrow(mrip_with_clsid)

cool <- c(17:98)
mrip_with_clsid[,cool] = apply(mrip_with_clsid[,cool],2,function(x)as.numeric(x))  
is.zero.na <- function(x){
  x == 0 | is.na(x)
}
mrip_with_clsid <- mrip_with_clsid %>% 
 bind_cols(
  select(mrip_with_clsid,ends_with("claim")) %>%
    transmute(total_claim_mrip = rowSums(.,na.rm = T),
              reported_species_claim_mrip = rowSums(!is.zero.na(.)))
) %>%
  bind_cols(
    select(mrip_with_clsid,ends_with("release")) %>%
      transmute(total_release_mrip = rowSums(.,na.rm = T),
                reported_species_release_mrip = rowSums(!is.zero.na(.)))
  ) 

#CLS 2-17 ONLY
cls_17_only <- cls_tidy2_17 %>%
  filter(year(return_date_ymd) > 2016)
nrow(cls_17_only)

colz <- c(22:106)
cls_17_only[,colz] = apply(cls_17_only[,colz],2,function(x)as.numeric(x))  
is.zero.na <- function(x){
  x == 0 | is.na(x)
}

cls_17_only <- cls_17_only %>%
  bind_cols(
select(cls_17_only,ends_with("kept")) %>%
    transmute(total_kept_cls = rowSums(.,na.rm = T),
              reported_species_kept_cls = rowSums(!is.zero.na(.)))
) %>%
bind_cols(
    select(cls_17_only,ends_with("released")) %>%
      transmute(total_released_cls = rowSums(.,na.rm = T),
                reported_species_released_cls = rowSums(!is.zero.na(.)))
  ) 

#########
#Ok now everything is clean
#########

#Start with just reported species caught
for(i in 1:nrow(cls_17_only)) {
  score <- c()
  for(j in 1:nrow(mrip_with_clsid)) {
    if(cls_17_only$reported_species_kept_cls[i] ==
        mrip_with_clsid$reported_species_claim_mrip[j]){
      score[i] <- -log(sum(mrip_with_clsid$reported_species_claim_mrip ==
                             cls_17_only$reported_species_kept_cls[i])/
                         nrow(mrip_with_clsid))
    }else{print("Not")}
  }
}

for(i in 1:nrow(cls_17_only)) {
  score <- c()
   if(cls_17_only$reported_species_kept_cls[i] ==
       mrip_with_clsid$reported_species_claim_mrip[1]){
      score[i] <- -log(sum(mrip_with_clsid$reported_species_claim_mrip ==
                             cls_17_only$reported_species_kept_cls[i])/
                         nrow(mrip_with_clsid))
   }else if(abs(cls_17_only$reported_species_kept_cls[i] -
       mrip_with_clsid$reported_species_claim_mrip[1]) %in% c(1,2)){
     score[i] <- -log(sum(abs(cls_17_only$reported_species_kept_cls[i] -
                                mrip_with_clsid$reported_species_claim_mrip) ==
                            (abs(cls_17_only$reported_species_kept_cls[i] -
                                   mrip_with_clsid$reported_species_claim_mrip[1])))/
                        nrow(mrip_with_clsid)) + log(0.4937238)
    
   }
  }


cls_17_only$reported_species_kept_cls[1]
sum(mrip_with_clsid$reported_species_claim_mrip==4)/nrow(mrip_with_clsid)


abs(cls_17_only$reported_species_kept_cls[55] -
      mrip_with_clsid$reported_species_claim_mrip[1])

sum(abs(cls_17_only$reported_species_kept_cls[i] -
  mrip_with_clsid$reported_species_claim_mrip) ==
  (abs(cls_17_only$reported_species_kept_cls[i] -
        mrip_with_clsid$reported_species_claim_mrip[1])))/nrow(mrip_with_clsid)




