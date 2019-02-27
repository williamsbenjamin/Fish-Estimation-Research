cls_data <- read_sas("data/clsdata_20180403.sas7bdat")
cls_data <- cls_data %>% #try this
  distinct(speciesCode,
           returndate,
           departdate,
           kept,
           releasedAlive,
           releasedDead,
           TID,USCG,
           captainName,
           permit,
           isCharterTrip,
           nbPassengers,
           nbAnglers,
           nbCrew,
           region,
           firstTarget,
           secondTarget,
           depthMin,
           depthMax,
           depthPrimary,
           hours,
           state,
           county,
           Name,
           gallons,
           pricegallons,
           .keep_all = TRUE)

tid_cls <- read_sas("data/cls2tid_20170905.sas7bdat")
tid_cls$TID <- as.integer(tid_cls$TID)
cls_data <- cls_data %>%
  left_join(tid_cls)

cls_data <- cls_data %>% 
  filter(year(reportdate) == 2017) %>% 
  filter(year(returndate) == 2017) %>% 
  filter(!CLS_ID %in% 
           c(4,5,48,81,87,117,118,
             146,184,193,222,226,
             248,39,70,73,92,99,
             239,51,80,89,93,126,
             129,161,180,203,41,
             104,117,116,125,129,
             131,132,149,150,151,
             152,165,166,167,168,
             169,170,171,172,173,
             174,175,176,177,178,
             179,180,181,216))  %>% 
  filter(CLS_ID > 0)

cls_data <- cls_data %>%
  filter(!TID %in% c(512517,
                     512518,
                     512146,
                     512494,
                     512220,
                     512220,
                     512146,
                     512494,
                     512517,
                     512518,
                     512516,
                     512068,
                     512249,
                     512260,
                     512295,
                     512298,
                     512306,
                     512340,
                     512498,
                     513709,
                     520278,
                     502576))
cls_data<- cls_data %>% 
  filter(state %in% c("FL","AL","MS"))

nrow(cls_data)

nrow(cls_data %>% 
       distinct(tripID))
