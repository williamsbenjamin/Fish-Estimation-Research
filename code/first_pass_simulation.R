
#Simulate Population for 2017

library(tidyverse)
library(truncdist)
library(stringr)
#The Data, make sure psu_id is a character
mrip <- read_csv("data/mrip_tidy2_17.csv",
                 col_types = cols(psu_id = col_character()))

#How many PSUs are there
mrip %>% 
  count(psu_id) # 588 psus with at least one trip

mrip %>% 
  count(psu_id)%>%
  pull(n) %>% 
  mean() # about 2.3 trips per psu in 2017 MRIP

#Max number of times a psu was sampled?
mrip %>% 
  count(psu_id) %>% 
  arrange(-n)

#are all PSU's associated with a single sampling weight?
mrip %>%
  group_by(psu_id) %>% 
  summarize(weights = n_distinct(wp_int)) #no

#How may psus have more than 1 weight
mrip %>%
  group_by(psu_id) %>% 
  summarize(weights = n_distinct(wp_int)) %>% 
  filter(weights > 1) %>% 
  nrow()
#233 out of 588

#What does this distribution look like
mrip %>%
  group_by(psu_id) %>% 
  summarize(weights = n_distinct(wp_int)) %>% 
  ggplot(aes(weights)) + 
  geom_histogram()+
  ggtitle("Distribution of # of sampling weights associated with a PSU")

#what is the psu with the largest # of sampling weights
mrip %>%
  group_by(psu_id) %>% 
  summarize(distinct_weights = n_distinct(wp_int)) %>% 
  arrange(-distinct_weights)

#For now, only keep the largest weight
one_weight_per_psu <- mrip %>%
  group_by(psu_id) %>% 
  summarise(weight = max(wp_int))

one_weight_per_psu

#Replicate each psu where the number of replications
#is equal to the integer portion of its sampling weight
#now have a simulated population of psus with its sampling
#weight attached
simulated_pop <- tibble(psu = rep(one_weight_per_psu$psu_id,
                                  one_weight_per_psu$weight),
                        weight = rep(one_weight_per_psu$weight,
                                     one_weight_per_psu$weight
                        ))

simulated_pop

#how big is the simulated pop
nrow(simulated_pop)
#this correctly corresponds closely 
#to the sum of the weights of each psu 
one_weight_per_psu %>% 
  pull(weight) %>% 
  sum()

#how many trips are there per psu
simulated_pop %>% 
  count(psu) %>% 
  pull(n) %>% 
  mean() #261 trips per psu

#Add a variable for Stratum ID
#Strat ID = year, month, st, region, mode_fx, kod, strat_interval
#strat_interval = 6/12 hour interval in which sampling was conducted
#PSU ID = year, wave, st, region, mode_fx, asg_code
#region = 
#mode_fx = 1 digit mode of fishing number
#asg_code = Assignment number (1 digit), 
#             interviewer code (4 digit), date (YYYYMMDD)

#See if any psus contain multiple stratum
mrip %>% 
  mutate(time = str_sub(strat_id,start = 14)) %>%
  mutate(time = if_else(time == "8PM-8AM","Night","Day")) %>% 
  group_by(psu_id) %>% 
  summarize(ids = n_distinct(time)) %>% 
  arrange(-ids) #no they do not
#How often are day/night sampled
mrip %>% 
  mutate(time = str_sub(strat_id,start = 14)) %>%
  mutate(time = if_else(time == "8PM-8AM","Night","Day")) %>% 
  count(time) # Only Day
#So strat_interval is always 8AM-8PM

#So Can Make Stratum ID from PSU ID 
simulated_pop %>% 
  mutate(year = str_sub(psu,start = 1,end = 4)) %>% 
  mutate(month = str_sub(psu,start = -4,end = 19)) %>% 
  mutate(st = )


#generate Red Snapper Catch for each trip
#need to look at other species later
#also, for now just use zero-truncated poisson
#but will need to do some sort of mixture model
#with a spike at 0

#go back and do this with truncdist
#rtrunc(nrow(simulated_pop),"pois",a=1e-16,b=Inf,lambda=10)
red_snapper_for_sim <- rpois(nrow(simulated_pop),lambda = 10)
sim_pop <- simulated_pop %>% 
  mutate(red_snapper_mrip = red_snapper_for_sim)

#plot red_snapper
sim_pop %>% 
  ggplot(aes(red_snapper_mrip)) +
  geom_histogram(bins=20) +
  ggtitle("Simulated Population Red Snapper Catch")

#add on cls values
#need to assume measurement error
red_snap_cls <- floor(red_snapper_for_sim + 
                        rnorm(nrow(sim_pop),
                              mean = 0,
                              sd = 4)) #sd = 4 gives correlation 
                                      # of 0.619
sim_pop <- sim_pop %>% 
  mutate(red_snapper_cls = red_snap_cls)













