mrip_all_17_dm <- read_csv("data/mrip_dm_17_all_sites.csv")
mrip_all_17_rl <- read_csv("data/mrip_all_17_cutoff13_all_sites.csv")

nrow(mrip_all_17_dm)
nrow(mrip_all_17_rl)

mrip_all_17_dm %>% 
  filter(reported == 1) %>% 
  nrow()

mrip_all_17_rl %>% 
  filter(reported == 1) %>% 
  nrow()


rl <- mrip_all_17_rl %>% 
  filter(reported == 1) %>% 
  select(CLS_ID,return_date_ymd,returndate) %>% 
  distinct() 

dm <- mrip_all_17_dm %>% 
   filter(reported == 1) %>% 
   select(CLS_ID,return_date_ymd,returndate) %>% 
   distinct()


mrip_all_17_rl %>% 
  filter(reported == 1) %>% 
  select(CLS_ID,return_date_ymd,returndate) %>% 
  distinct() %>% 
  inner_join(mrip_all_17_dm %>% 
  filter(reported == 1) %>% 
  select(CLS_ID,return_date_ymd,returndate) %>% 
  distinct()
  )



