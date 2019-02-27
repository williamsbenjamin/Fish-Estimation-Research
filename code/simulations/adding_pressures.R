fl_p <- read_csv("data/fl_pressure.csv")
al_p <- read_csv("data/al_pressure.csv")
ms_p <- read_csv("data/ms_pressure.csv")

fl_p %>% 
  filter(SITE_GROUP == "CHARTER") %>% 
  group_by(SITE_EXTERNAL_ID,MONTH,KOD) %>% 
  summarize(nn = n_distinct(`CHARTER BOAT`))

press <- fl_p %>% 
  bind_rows(al_p,
            ms_p) %>% 
  filter(SITE_GROUP == "CHARTER") %>% 
  select(SITE_EXTERNAL_ID,STATE,MONTH,KOD,`CHARTER BOAT`,INTERVAL)

pop.press <- pop %>% 
  select(psu,psu_id,month,kod,SITE_EXTERNAL_ID,ST,block_start) %>% 
  rename(MONTH = month,
         KOD = kod,
         STATE = ST,
         INTERVAL = block_start) %>% 
  mutate(MONTH = 
           case_when(
             MONTH == "01" ~ "JAN",
             MONTH == "02" ~ "FEB",
             MONTH == "03" ~ "MAR",
             MONTH == "04" ~ "APR",
             MONTH == "05" ~ "MAY",
             MONTH == "06" ~ "JUN",
             MONTH == "07" ~ "JUL",
             MONTH == "08" ~ "AUG",
             MONTH == "09" ~ "SEP",
             MONTH == "10" ~ "OCT",
             MONTH == "11" ~ "NOV",
             MONTH == "12" ~ "DEC"
           ),
         KOD = 
           case_when(
             KOD == "WD" ~ "WEEKDAY",
             KOD == "WE" ~ "WEEKEND"
           ),
         INTERVAL = 
           case_when(
             INTERVAL == 1400 ~ "1400-2000",
             INTERVAL == 1100 ~ "1100-1700",
             INTERVAL == 1700 ~ "1700-2300",
             INTERVAL == 800 ~ "0800-1400",
             INTERVAL == 2000 ~ "2000-0200"
           )) %>% 
  left_join(press)
  
