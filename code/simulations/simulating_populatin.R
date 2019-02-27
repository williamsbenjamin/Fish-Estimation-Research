library(tidyverse)
library(haven)
library(lubridate)
library(truncdist)
cls17 <- read_csv("data/cls_17_all_sites.csv")

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

w_data <- read_sas("data/weights_2016_2017_gulf_block.sas7bdat")


#Get 2017 data
w_data <- w_data %>% 
  filter(YEAR == 2017)

#Lat/Long Info
dock_info <- read_csv("data/dock_locations.csv")

dock_location <- dock_info %>% 
  select(SITE_EXTERNAL_ID,
         SITE_LAT,
         SITE_LONG,
         STATE,
         STATUS,
         SITE_ZIP,
         `SHORE AREA`) %>%
  filter(STATUS != "Retired")
#Reformat Lat and Long
dock_location$SITE_LAT <- gsub(":", ".",
                               gsub(".", "",
                                    (
                                      str_replace(dock_location$SITE_LAT, "\t", "")
                                    ),
                                    fixed = T),
                               fixed = T)
dock_location$SITE_LONG <- gsub(":", ".",
                                gsub(".", "",
                                     (
                                       str_replace(dock_location$SITE_LONG, "\t", "")
                                     ),
                                     fixed = T),
                                fixed = T)
#All the longitudes need to be changed to negative
#Negative longitude is everything west of england
dock_location$SITE_LONG <- paste0("-",dock_location$SITE_LONG,sep="")

dock_location$SITE_LAT <- as.numeric(dock_location$SITE_LAT)
dock_location$SITE_LONG <- as.numeric(dock_location$SITE_LONG)

#get the unique psu sampling weights
psu_weights <- w_data %>% 
  distinct(psu,site,block_start,w_psu,strat_interval,kod,block_start) %>% 
  mutate(psu_id = 1:n())

#replicate the psus proportional to their weights (2 times)
y <- c()
for(i in 1:nrow(psu_weights)){
  x <- rep(psu_weights$psu_id[i],round(1*psu_weights$w_psu[i]))
  y <- c(y,x)
}

#add the vector of replicates onto the weighting info
psus <- tibble(y)
pop <- psus %>% 
  rename(psu_id = y) %>% 
  left_join(psu_weights) %>% 
  mutate(trip_rep_id = 1:n())

#Add on State
pop <- pop %>% 
  mutate(ST = str_sub(psu, end =2)) %>% 
  mutate(ST = case_when(
    ST == "01" ~ "AL",
    ST == "12" ~ "FL",
    ST == "28" ~ "MS"
  )
  )

docks_for_joins <- dock_location %>%
  filter(STATE %in% c("FL","AL","MS")) %>%
  filter(SITE_EXTERNAL_ID %in% unique(pop$site)) %>% 
  select(-STATUS) %>% 
  mutate(site = SITE_EXTERNAL_ID,
         ST = STATE)
#get them ready to put back on
first_dock <- docks_for_joins %>% 
  filter(STATE == "FL") %>% 
  filter(SITE_EXTERNAL_ID %in% c(516,306,216,286,
                                 314,946,295,509,
                                 931,287,174)) %>% 
  filter(!is.na(SITE_ZIP)) %>% 
  count(SITE_EXTERNAL_ID,STATE) %>% 
  filter(n == 1) %>% 
  select(SITE_EXTERNAL_ID,STATE)

second_dock <- docks_for_joins %>% 
  filter(STATE == "FL") %>% 
  filter(SITE_EXTERNAL_ID %in% c(516,306,216,286,
                                 314,946,295,509,
                                 931,287,174)) %>% 
  anti_join(first_dock) %>% 
  filter(!is.na(`SHORE AREA`)) %>% 
  select(SITE_EXTERNAL_ID, STATE)

docks_for_joins <- docks_for_joins %>% 
  anti_join(bind_rows(first_dock,second_dock)) %>% 
  bind_rows(first_dock) %>% 
  bind_rows(second_dock)

pop <- pop %>%
  left_join(docks_for_joins)

#Number of trips per psu

trips <- c()
trips_per_psu <- mrip17 %>% 
                     count(psu_id) %>% 
                     pull(n)
for(t in 1:nrow(pop)){
  trips[t] = sample(trips_per_psu,1)
}
pop <- pop %>% 
  mutate(trips = trips)

pop <- pop %>% 
  uncount(trips)
#Add Party size to simulated population
#estimated from "estimating_variable_distributions_2017.R"
u <- runif(nrow(pop))
party <- rep(2,nrow(pop)) #2 is most common size
for(j in 1:nrow(pop)){
  if(u[j] <= 0.00135){party[j] = 13
  } else if(u[j] <= 0.00609){party[j] = 11
  } else if(u[j] <= 0.01218){party[j] = 12
  } else if(u[j] <= 0.02298){party[j] = 10
  } else if(u[j] <= 0.03518){party[j] = 9
  } else if(u[j] <= 0.05078){party[j] = 7
  } else if(u[j] <= 0.06858){party[j] = 8
  } else if(u[j] <= 0.11668){party[j] = 1
  } else if(u[j] <= 0.23068){party[j] = 5
  } else if(u[j] <= 0.37668){party[j] = 6
  } else if(u[j] <= 0.54568){party[j] = 3
  } else if(u[j] <= 0.74968){party[j] = 4}
}

pop <- pop %>% 
  mutate(party = party) #add onto pop

#Add Red Snapper Claim to Simulated Population
#multiply part by mean snapper catch
#Indicator of if they caught snapper
z <- runif(nrow(pop))
red_snapper_ind <- rep(0,nrow(pop))
for(k in 1:nrow(pop)){
  if(z[k] > 0.88){
    red_snapper_ind[k] = 1
  }
}
red_snapper_harvest_mean <- rep(0,nrow(pop))
z2 <- runif(nrow(pop))
for(l in 1:nrow(pop)){
  if(red_snapper_ind[l] == 1){
     if(z2[l] <= 0.5168539){
       red_snapper_harvest_mean[l] = 2
     } else{
       red_snapper_harvest_mean[l] = runif(1,0.1,1.95)
     }
  }
}

pop <- pop %>% 
  mutate(red_snapper_harvest_mean = red_snapper_harvest_mean) %>% 
  mutate(red_snapper_harvest = round(red_snapper_harvest_mean*party)) %>% 
  select(-red_snapper_harvest_mean)

#Date
pop <- pop %>% 
  mutate(year = str_sub(psu,-8,(nchar(psu) - 4)),
         month = str_sub(psu,-4,(nchar(psu) - 2)),
         day = str_sub(psu,-2,nchar(psu))) %>% 
  mutate(date = as.character(paste(year,month,day,sep=""))) %>% 
  mutate(date = ymd(date))

#Total Harvest
#multiply part by mean snapper catch
#Indicator of if they caught snapper
h <- runif(nrow(pop))
total_catch_ind <- rep(0,nrow(pop))
for(k in 1:nrow(pop)){
  if(h[k] > 0.136){
    total_catch_ind[k] = 1
  }
}

total_harvest_mean <- rep(0,nrow(pop))
harv_m <- rtrunc(nrow(pop), spec="gamma",a = 0.00001,
                             b=9900, shape = 1.8, rate = .4)
for(l in 1:nrow(pop)){
  if(total_catch_ind[l] == 1){
      total_harvest_mean[l] <- harv_m[l]
  }
}

pop <- pop %>% 
  mutate(total_harvest_mean = total_harvest_mean) %>% 
  mutate(total_harvest = round(total_harvest_mean*party)) %>% 
  select(-total_harvest_mean)

#Total Release
l <- runif(nrow(pop))
total_rel_ind <- rep(0,nrow(pop))
for(k in 1:nrow(pop)){
  if(l[k] > 0.1206){
    total_rel_ind[k] = 1
  }
}

total_rel_mean <- rep(0,nrow(pop))
rel_m <- rtrunc(nrow(pop), spec="gamma",a = 0.00001,
                 b=9900, shape = 2, rate = .35)
for(ll in 1:nrow(pop)){
  if(total_rel_ind[ll] == 1){
    total_rel_mean[ll] <- rel_m[ll]
  }
}

pop <- pop %>% 
  mutate(total_rel_mean = total_rel_mean) %>% 
  mutate(total_release = round(total_rel_mean*party)) %>% 
  select(-total_rel_mean)

#Number of Species Caught

g <- runif(nrow(pop))
sp_caught <- rep(0,nrow(pop))
for(gg in 1:nrow(pop)){
  if(g[gg] <= 0.137){sp_caught[gg] = 0
  } else if(g[gg] <= 0.421){sp_caught[gg] = 1
  } else if(g[gg] <= 0.701){sp_caught[gg] = 2
  } else if(g[gg] <= 0.857){sp_caught[gg] = 3
  } else if(g[gg] <= 0.9318){sp_caught[gg] = 4
  } else if(g[gg] <= 0.9722){sp_caught[gg] = 5
  } else if(g[gg] <= 0.9904){sp_caught[gg] = 6
  } else if(g[gg] <= 0.99646){sp_caught[gg] = 7
  } else if(g[gg] <= 0.99848){sp_caught[gg] = 8
  } else if(g[gg] <= 0.99983){sp_caught[gg] = 9
  } else(sp_caught[gg] = 10)
}

pop <- pop %>% 
  mutate(species_caught_mrip = sp_caught)

#Number of Species Released
q <- runif(nrow(pop))
sp_rel <- rep(0,nrow(pop))
for(qq in 1:nrow(pop)){
  if(q[qq] <= 0.247){sp_rel[qq] = 0
  } else if(q[qq] <= 0.585){sp_rel[qq] = 1
  } else if(q[qq] <= 0.838){sp_rel[qq] = 2
  } else if(q[qq] <= 0.951){sp_rel[qq] = 3
  } else if(q[qq] <= 0.9881){sp_rel[qq] = 4
  } else if(q[qq] <= 0.9996){sp_rel[qq] = 5
  } else(sp_rel[qq] = 6)
}

pop <- pop %>% 
  mutate(species_released_mrip = sp_rel)

#CLS 
pp1 = 0.3 #prop of boats with a CLS ID,
          #not necessarily a CLS ID from self-reports
cl <- runif(nrow(pop))
cl_i <- rep(0,nrow(pop))
for(cc in 1:nrow(pop)){
  if(cl[cc] < pp1){cl_i[cc] = 1}
}
pop <- pop %>% 
  mutate(CLS = cl_i)

## For the CLS trips, Dilute the values to be not exact
## Need to give a CLS ID to the vessels
## dilute the cls variables and check the resulting

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

neg_to_zero <- function(x){
  if(x < 0){ x = 0}
  x
}

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
#Give the CLS Trips a CLS_ID
cls_id_in_mrip <- mrip17 %>% 
  filter(CLS == "Y") %>% 
  pull(CLS_ID)
cls_id_sample <- sample(x = cls_id_in_mrip,
                        size = nrow(pop),
                        replace = T)
pop <- pop %>% 
  mutate(CLS_ID = if_else(CLS == 1,
                          as.numeric(cls_id_sample),
                          as.numeric(NA))
         ) %>% 
  mutate(strat_int = paste0(date,kod,strat_interval)) %>% 
  mutate(id = 1:n())

true_match_leng <- pop %>% 
  mutate(id = 1:n()) %>% 
  filter(CLS_ID %in% cls17$CLS_ID) %>% 
  select(id) 

true_match_leng <- true_match_leng %>% 
  sample_n(size = 0.40*nrow(true_match_leng))
# what I am saying above is that of the intercepts who
# see a CLS boat with a CLS_ID that is on the self-report
# list, only 40% of those actually made a report

pop <- pop %>% 
  mutate(true_match_id = if_else(id %in% true_match_leng$id,
                                 id,as.integer(0))) 

nest_pop <- pop %>% 
  group_by(`psu_id`,
           `block_start`,
           `strat_interval`,
           `strat_int`,
           `kod`,                  
           `site`,
           `w_psu`,
           `psu`,
           `trip_rep_id`,          
           `ST`,
           `SITE_EXTERNAL_ID`,
           `SITE_LAT`,
           `SITE_LONG`,            
           `STATE`,   
           `SITE_ZIP`,
           `SHORE AREA`) %>% 
  nest()

psu_probs <- nest_pop %>% 
  count(psu_id) %>% 
  mutate(prob = n/sum(n)) 

nest_pop <- nest_pop %>% 
  left_join(psu_probs) %>% 
  select(-n)

# nest pop is a tibble of replicates of psu's 
# and then for each replicate, there is a nested tibble
# of trips associated with the replicated


