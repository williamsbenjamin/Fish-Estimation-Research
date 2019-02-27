#Simulate the population of trips
source("code/simulations/simulating_populatin.R")

# take 1 sample of 200 psu's with replacement
# sampling weight is proportional to number of psu 
# replicates
samp1_psu <- nest_pop %>% 
  sample_n(size = 200,replace = F,weight = (prob)) %>% 
  select(psu_id)

s_pop <- tibble()

for(i in 1:nrow(samp1_psu)){
a <- filter(nest_pop,psu_id == samp1_psu$psu_id[i]) %>% 
    sample_n(1)
s_pop <- bind_rows(s_pop,
                   a)
}


