
mrip_catch <- read_sas("data/catch_2017_cls.sas7bdat")
mrip_trip <- read_sas("data/trip_2017_cls.sas7bdat")
mrip_trip <- mrip_trip %>% 
  mutate(PRT_CODE = as.character(PRT_CODE))

##I will be interested in CLAIM + HARVEST

#I will title claim to be the sum of 
#unadjusted claim and unadjusted harvest because
#I have already written all the code...

# mrip_catch <- mrip_catch %>% 
#   mutate(CLAIM_UNADJ = CLAIM_UNADJ + HARVEST_UNADJ)
mrip_catch <- mrip_catch %>% 
  mutate(CLAIM = CLAIM + HARVEST)


###get one ID_CODE per row
#One interview per row with columns being trip and species specific
#start with mrip_catch
m_catch <-
  mrip_catch %>% 
  select(-tot_len,-wgt_ab1,-CLAIM_UNADJ,
         -RELEASE_UNADJ,-HARVEST,
         -tot_len_b1,-wgt_b1,-tot_len_a,-wgt_a,
         -HARVEST_UNADJ,-tot_cat,-tot_len,-landing,
         -MODE_FX,-AREA_X,-month,-fl_reg,-alt_flag,
         -arx_method,-kod,-SUB_REG,-region) %>%
  unite("fish_spec",c(common,SP_CODE),sep="/") %>%
  unite("info",c(strat_id,psu_id,YEAR,ST,
                 ID_CODE,WAVE,
                 var_id,
                 CLS,CLS_ID,wp_int,wp_catch),sep="/") %>%
  gather(key,val, -c(fish_spec,info)) %>%
  unite(key2,fish_spec,key,sep="_")%>%
  spread(key2,val) %>%
  separate(info, into = c("strat_id","psu_id","YEAR","ST",
                          "ID_CODE","WAVE",
                          "var_id",
                          "CLS","CLS_ID","wp_int","wp_catch"),
           sep="/")

#Fix 2 CLS_IDs per some PRT_CODES, some were NA
cls_2_prt <- mrip_trip %>% 
  distinct(PRT_CODE,CLS_ID) %>%
  drop_na(CLS_ID)
mrip_trip <- mrip_trip %>% 
  select(-CLS_ID) %>% 
  left_join(cls_2_prt)



#maybe remove
m_trip <- mrip_trip %>% 
  select(ID_CODE,
         prim1_common,
         PARTY,
         INTSITE,
         TIME,
         PRT_CODE,
         CLS_ID) %>%
  mutate(ID_CODE = as.character(ID_CODE),
         prim1_common = as.character(prim1_common),
         PARTY = as.numeric(PARTY),
         INTSITE = as.numeric(INTSITE),
         TIME = as.numeric(TIME),
         PRT_CODE = as.character(PRT_CODE),
         CLS_ID)


#join the mrip datasets
mrip_ct <- m_catch %>%
  left_join(m_trip, by = "ID_CODE") %>%
  distinct() %>% 
  mutate(CLS_ID =  CLS_ID.x) %>% 
  select(-CLS_ID.x,-CLS_ID.y)

mrip_ct <- mrip_ct %>%
  mutate(
    date = ymd(
      str_sub(
        ID_CODE,
        6,
        13
      )
    )
  )
#format time
mrip_ct <- mrip_ct %>%
  mutate(TIME = if_else(TIME < 1000,
                        paste("0",TIME,sep = ""),as.character(TIME))) %>% 
  mutate(TIME = if_else(TIME == "015",
                        "0015",as.character(TIME)))


mrip_ct <- mrip_ct %>%
  mutate(TIME = paste0(str_sub(TIME,1,2),
                       ":",
                       str_sub(TIME,3,4))) 

#  mutate(TIME = if_else(TIME == "NA:NA",
#                        "12:00",TIME))
#date_time_mrip has date and time of interview
mrip_ct <- mrip_ct %>%
  mutate(date_time_mrip =ymd_hm(paste0(mrip_ct$date,
                                       " ",
                                       mrip_ct$TIME)
  )
  ) 

#need to get each row to represent one trip
#each trip is comprised of interviews

#To start, get catch totals for each trip
#Sum up over each interview
# mrip_ct <- mrip_ct %>% 
#   mutate(trip_id = str_sub(ID_CODE,0,13))

mrip_ct <- mrip_ct %>%
  group_by(PRT_CODE) %>%
  mutate(`UNKNOWN_claim`=sum(`/_CLAIM`,na.rm=T),
         `UNKNOWN_release`=sum(`/_RELEASE`,na.rm=T),
         `AFRICAN POMPANO_claim`=sum(`AFRICAN POMPANO/8835280201_CLAIM`,na.rm = T),
         `AFRICAN POMPANO_release`=sum(`AFRICAN POMPANO/8835280201_RELEASE`,na.rm = T),
         `ALMACO JACK_claim`=sum(`ALMACO JACK/8835280803_CLAIM`,na.rm=T),
         `ALMACO JACK_release`=sum(`ALMACO JACK/8835280803_RELEASE`,na.rm=T),
         `AMBERJACK GENUS_claim`=sum(`AMBERJACK GENUS/8835280800_CLAIM`,na.rm=T),
         `AMBERJACK GENUS_release`=sum(`AMBERJACK GENUS/8835280800_RELEASE`,na.rm=T),
         `ATLANTIC BONITO_claim`=sum(`ATLANTIC BONITO/8850030202_CLAIM`,na.rm=T),
         `ATLANTIC BONITO_release`=sum(`ATLANTIC BONITO/8850030202_RELEASE`,na.rm=T),
         `ATLANTIC BUMPER_claim`=sum(`ATLANTIC BUMPER/8835280401_CLAIM`,na.rm=T),
         `ATLANTIC BUMPER_release`=sum(`ATLANTIC BUMPER/8835280401_RELEASE`,na.rm=T),
         `ATLANTIC CROAKER_claim`=sum(`ATLANTIC CROAKER/8835440702_CLAIM`,na.rm=T),
         `ATLANTIC CROAKER_release`=sum(`ATLANTIC CROAKER/8835440702_RELEASE`,na.rm=T),
         `ATLANTIC SHARPNOSE SHARK_claim`=sum(`ATLANTIC SHARPNOSE SHARK/8708020301_CLAIM`,na.rm=T),
         `ATLANTIC SHARPNOSE SHARK_release`=sum(`ATLANTIC SHARPNOSE SHARK/8708020301_RELEASE`,na.rm=T),
         `ATLANTIC SPADEFISH_claim`=sum(`ATLANTIC SPADEFISH/8835520101_CLAIM`,na.rm=T),
         `ATLANTIC SPADEFISH_release`=sum(`ATLANTIC SPADEFISH/8835520101_RELEASE`,na.rm=T),
         `ATLANTIC STINGRAY_claim`=sum(`ATLANTIC STINGRAY/8713050105_CLAIM`,na.rm=T),
         `ATLANTIC STINGRAY_release`=sum(`ATLANTIC STINGRAY/8713050105_RELEASE`,na.rm=T),
         `ATLANTIC TARPON_claim`=sum(`ATLANTIC TARPON/8738020201_CLAIM`,na.rm=T),
         `ATLANTIC TARPON_release`=sum(`ATLANTIC TARPON/8738020201_RELEASE`,na.rm=T),
         `ATLANTIC THREAD HERRING_claim`=sum(`ATLANTIC THREAD HERRING/8747010701_CLAIM`,na.rm=T),
         `ATLANTIC THREAD HERRING_release`=sum(`ATLANTIC THREAD HERRING/8747010701_RELEASE`,na.rm=T),
         `BANDED RUDDERFISH_claim`=sum(`BANDED RUDDERFISH/8835280804_CLAIM`,na.rm=T),
         `BANDED RUDDERFISH_release`=sum(`BANDED RUDDERFISH/8835280804_RELEASE`,na.rm=T),
         `BANK SEA BASS_claim`=sum(`BANK SEA BASS/8835020304_CLAIM`,na.rm=T),
         `BANK SEA BASS_release`=sum(`BANK SEA BASS/8835020304_RELEASE`,na.rm=T),
         `BARRELFISH_claim`=sum(`BARRELFISH/8851010201_CLAIM`,na.rm=T),
         `BARRELFISH_release`=sum(`BARRELFISH/8851010201_RELEASE`,na.rm=T),
         `BIGEYE FAMILY_claim`=sum(`BIGEYE FAMILY/8835170000_CLAIM`,na.rm=T),
         `BIGEYE FAMILY_release`=sum(`BIGEYE FAMILY/8835170000_RELEASE`,na.rm=T),
         `BIGEYE_claim`=sum(`BIGEYE/8835170101_CLAIM`,na.rm=T),
         `BIGEYE_release`=sum(`BIGEYE/8835170101_RELEASE`,na.rm=T),
         `BLACK DRUM_claim`=sum(`BLACK DRUM/8835440801_CLAIM`,na.rm=T),
         `BLACK DRUM_release`=sum(`BLACK DRUM/8835440801_RELEASE`,na.rm=T),
         `BLACK GROUPER_claim`=sum(`BLACK GROUPER/8835020502_CLAIM`,na.rm=T),
         `BLACK GROUPER_release`=sum(`BLACK GROUPER/8835020502_RELEASE`,na.rm=T),
         `BLACK SEA BASS_claim`=sum(`BLACK SEA BASS/8835020301_CLAIM`,na.rm=T),
         `BLACK SEA BASS_release`=sum(`BLACK SEA BASS/8835020301_RELEASE`,na.rm=T),
         `BLACKFIN TUNA_claim`=sum(`BLACKFIN TUNA/8850030404_CLAIM`,na.rm=T),
         `BLACKFIN TUNA_release`=sum(`BLACKFIN TUNA/8850030404_RELEASE`,na.rm=T),
         `BLACKNOSE SHARK_claim`=sum(`BLACKNOSE SHARK/8708020504_CLAIM`,na.rm=T),
         `BLACKNOSE SHARK_release`=sum(`BLACKNOSE SHARK/8708020504_RELEASE`,na.rm=T),
         `BLACKTIP SHARK_claim`=sum(`BLACKTIP SHARK/8708020507_CLAIM`,na.rm=T),
         `BLACKTIP SHARK_release`=sum(`BLACKTIP SHARK/8708020507_RELEASE`,na.rm=T),
         `BLUE RUNNER_claim`=sum(`BLUE RUNNER/8835280306_CLAIM`,na.rm=T),
         `BLUE RUNNER_release`=sum(`BLUE RUNNER/8835280306_RELEASE`,na.rm=T),
         `BLUEFISH_claim`=sum(`BLUEFISH/8835250101_CLAIM`,na.rm=T),
         `BLUEFISH_release`=sum(`BLUEFISH/8835250101_RELEASE`,na.rm=T),
         `BLUELINE TILEFISH_claim`=sum(`BLUELINE TILEFISH/8835220104_CLAIM`,na.rm=T),
         `BLUELINE TILEFISH_release`=sum(`BLUELINE TILEFISH/8835220104_RELEASE`,na.rm=T),
         `BONEFISH_claim`=sum(`BONEFISH/8739010101_CLAIM`,na.rm=T),
         `BONEFISH_release`=sum(`BONEFISH/8739010101_RELEASE`,na.rm=T),
         `BONNETHEAD_claim`=sum(`BONNETHEAD/8708030101_CLAIM`,na.rm=T),
         `BONNETHEAD_release`=sum(`BONNETHEAD/8708030101_RELEASE`,na.rm=T),
         `BOXFISH GENUS_claim`=sum(`BOXFISH GENUS/8860030100_CLAIM`,na.rm=T),
         `BOXFISH GENUS_release`=sum(`BOXFISH GENUS/8860030100_RELEASE`,na.rm=T),
         `BULL SHARK_claim`=sum(`BULL SHARK/8708020502_CLAIM`,na.rm=T),
         `BULL SHARK_release`=sum(`BULL SHARK/8708020502_RELEASE`,na.rm=T),
         `BURRFISH GENUS_claim`=sum(`BURRFISH GENUS/8861030100_CLAIM`,na.rm=T),
         `BURRFISH GENUS_release`=sum(`BURRFISH GENUS/8861030100_RELEASE`,na.rm=T),
         `BUTTERFISH FAMILY_claim`=sum(`BUTTERFISH FAMILY/8851030000_CLAIM`,na.rm=T),
         `BUTTERFISH FAMILY_release`=sum(`BUTTERFISH FAMILY/8851030000_RELEASE`,na.rm=T),
         `CERO_claim`=sum(`CERO/8850030503_CLAIM`,na.rm=T),
         `CERO_release`=sum(`CERO/8850030503_RELEASE`,na.rm=T),
         `CHUB MACKEREL_claim`=sum(`CHUB MACKEREL/8850030301_CLAIM`,na.rm=T),
         `CHUB MACKEREL_release`=sum(`CHUB MACKEREL/8850030301_RELEASE`,na.rm=T),
         `COBIA_claim`=sum(`COBIA/8835260101_CLAIM`,na.rm=T),
         `COBIA_release`=sum(`COBIA/8835260101_RELEASE`,na.rm=T),
         `COMMON SNOOK_claim`=sum(`COMMON SNOOK/8835010105_CLAIM`,na.rm=T),
         `COMMON SNOOK_release`=sum(`COMMON SNOOK/8835010105_RELEASE`,na.rm=T),
         `COTTONWICK_claim`=sum(`COTTONWICK/8835400111_CLAIM`,na.rm=T),
         `COTTONWICK_release`=sum(`COTTONWICK/8835400111_RELEASE`,na.rm=T),
         `CREOLE-FISH_claim`=sum(`CREOLE-FISH/8835021701_CLAIM`,na.rm=T),
         `CREOLE-FISH_release`=sum(`CREOLE-FISH/8835021701_RELEASE`,na.rm=T),
         `CREVALLE JACK_claim`=sum(`CREVALLE JACK/8835280303_CLAIM`,na.rm=T),
         `CREVALLE JACK_release`=sum(`CREVALLE JACK/8835280303_RELEASE`,na.rm=T),
         `CUBBYU_claim`=sum(`CUBBYU/8835441206_CLAIM`,na.rm=T),
         `CUBBYU_release`=sum(`CUBBYU/8835441206_RELEASE`,na.rm=T),
         `CUBERA SNAPPER_claim`=sum(`CUBERA SNAPPER/8835360101_CLAIM`,na.rm=T),
         `CUBERA SNAPPER_release`=sum(`CUBERA SNAPPER/8835360101_RELEASE`,na.rm=T),
         `DOGFISH SHARK_claim`=sum(`DOGFISH SHARK/8710010000_CLAIM`,na.rm=T),
         `DOGFISH SHARK_release`=sum(`DOGFISH SHARK/8710010000_RELEASE`,na.rm=T),
         `DOLPHIN_claim`=sum(`DOLPHIN/8835290101_CLAIM`,na.rm=T),
         `DOLPHIN_release`=sum(`DOLPHIN/8835290101_RELEASE`,na.rm=T),
         `FINETOOTH SHARK_claim`=sum(`FINETOOTH SHARK/8708021001_CLAIM`,na.rm=T),
         `FINETOOTH SHARK_release`=sum(`FINETOOTH SHARK/8708021001_RELEASE`,na.rm=T),
         `FLORIDA POMPANO_claim`=sum(`FLORIDA POMPANO/8835280901_CLAIM`,na.rm=T),
         `FLORIDA POMPANO_release`=sum(`FLORIDA POMPANO/8835280901_RELEASE`,na.rm=T),
         `GAFFTOPSAIL CATFISH_claim`=sum(`GAFFTOPSAIL CATFISH/8777180101_CLAIM`,na.rm=T),
         `GAFFTOPSAIL CATFISH_release`=sum(`GAFFTOPSAIL CATFISH/8777180101_RELEASE`,na.rm=T),
         `GAG_claim`=sum(`GAG/8835020501_CLAIM`,na.rm=T),
         `GAG_release`=sum(`GAG/8835020501_RELEASE`,na.rm=T),
         `GOLIATH GROUPER_claim`=sum(`GOLIATH GROUPER/8835020401_CLAIM`,na.rm=T),
         `GOLIATH GROUPER_release`=sum(`GOLIATH GROUPER/8835020401_RELEASE`,na.rm=T),
         `GRASS PORGY_claim`=sum(`GRASS PORGY/8835430501_CLAIM`,na.rm=T),
         `GRASS PORGY_release`=sum(`GRASS PORGY/8835430501_RELEASE`,na.rm=T),
         `GRAY SNAPPER_claim`=sum(`GRAY SNAPPER/8835360102_CLAIM`,na.rm=T),
         `GRAY SNAPPER_release`=sum(`GRAY SNAPPER/8835360102_RELEASE`,na.rm=T),
         `GRAY TRIGGERFISH_claim`=sum(`GRAY TRIGGERFISH/8860020201_CLAIM`,na.rm=T),
         `GRAY TRIGGERFISH_release`=sum(`GRAY TRIGGERFISH/8860020201_RELEASE`,na.rm=T),
         `GRAYSBY_claim`=sum(`GRAYSBY/8835021801_CLAIM`,na.rm=T),
         `GRAYSBY_release`=sum(`GRAYSBY/8835021801_RELEASE`,na.rm=T),
         `GREAT BARRACUDA_claim`=sum(`GREAT BARRACUDA/8837010104_CLAIM`,na.rm=T),
         `GREAT BARRACUDA_release`=sum(`GREAT BARRACUDA/8837010104_RELEASE`,na.rm=T),
         `GREATER AMBERJACK_claim`=sum(`GREATER AMBERJACK/8835280801_CLAIM`,na.rm=T),
         `GREATER AMBERJACK_release`=sum(`GREATER AMBERJACK/8835280801_RELEASE`,na.rm=T),
         `GROUPER GENUS EPINEPHELUS_claim`=sum(`GROUPER GENUS (EPINEPHELUS)/8835020400_CLAIM`,na.rm=T),
         `GROUPER GENUS EPINEPHELUS_release`=sum(`GROUPER GENUS (EPINEPHELUS)/8835020400_RELEASE`,na.rm=T),
         `GRUNT FAMILY_claim`=sum(`GRUNT FAMILY/8835400000_CLAIM`,na.rm=T),
         `GRUNT FAMILY_release`=sum(`GRUNT FAMILY/8835400000_RELEASE`,na.rm=T),
         `GULF FLOUNDER_claim`=sum(`GULF FLOUNDER/8857030302_CLAIM`,na.rm=T),
         `GULF FLOUNDER_release`=sum(`GULF FLOUNDER/8857030302_RELEASE`,na.rm=T),
         `GULF KINGFISH_claim`=sum(`GULF KINGFISH/8835440602_CLAIM`,na.rm=T),
         `GULF KINGFISH_release`=sum(`GULF KINGFISH/8835440602_RELEASE`,na.rm=T),
         `GULF TOADFISH_claim`=sum(`GULF TOADFISH/8783010202_CLAIM`,na.rm=T),
         `GULF TOADFISH_release`=sum(`GULF TOADFISH/8783010202_RELEASE`,na.rm=T),
         `HAMMERHEAD SHARK GENUS_claim`=sum(`HAMMERHEAD SHARK GENUS/8708030100_CLAIM`,na.rm=T),
         `HAMMERHEAD SHARK GENUS_release`=sum(`HAMMERHEAD SHARK GENUS/8708030100_RELEASE`,na.rm=T),
         `HARDHEAD CATFISH_claim`=sum(`HARDHEAD CATFISH/8777180202_CLAIM`,na.rm=T),
         `HARDHEAD CATFISH_release`=sum(`HARDHEAD CATFISH/8777180202_RELEASE`,na.rm=T),
         `HERRING FAMILY_claim`=sum(`HERRING FAMILY/8747010000_CLAIM`,na.rm=T),
         `HERRING FAMILY_release`=sum(`HERRING FAMILY/8747010000_RELEASE`,na.rm=T),
         `HOGFISH_claim`=sum(`HOGFISH/8839010901_CLAIM`,na.rm=T),
         `HOGFISH_release`=sum(`HOGFISH/8839010901_RELEASE`,na.rm=T),
         `HORSE-EYE JACK_claim`=sum(`HORSE-EYE JACK/8835280304_CLAIM`,na.rm=T),
         `HORSE-EYE JACK_release`=sum(`HORSE-EYE JACK/8835280304_RELEASE`,na.rm=T),
         `HOUNDFISH_claim`=sum(`HOUNDFISH/8803020302_CLAIM`,na.rm=T),
         `HOUNDFISH_release`=sum(`HOUNDFISH/8803020302_RELEASE`,na.rm=T),
         `INSHORE LIZARDFISH_claim`=sum(`INSHORE LIZARDFISH/8762020101_CLAIM`,na.rm=T),
         `INSHORE LIZARDFISH_release`=sum(`INSHORE LIZARDFISH/8762020101_RELEASE`,na.rm=T),
         `JACK FAMILY_claim`=sum(`JACK FAMILY/8835280000_CLAIM`,na.rm=T),
         `JACK FAMILY_release`=sum(`JACK FAMILY/8835280000_RELEASE`,na.rm=T),
         `JOLTHEAD PORGY_claim`=sum(`JOLTHEAD PORGY/8835430502_CLAIM`,na.rm=T),
         `JOLTHEAD PORGY_release`=sum(`JOLTHEAD PORGY/8835430502_RELEASE`,na.rm=T),
         `KING MACKEREL_claim`=sum(`KING MACKEREL/8850030501_CLAIM`,na.rm=T),
         `KING MACKEREL_release`=sum(`KING MACKEREL/8850030501_RELEASE`,na.rm=T),
         `KINGFISH GENUS_claim`=sum(`KINGFISH GENUS/8835440600_CLAIM`,na.rm=T),
         `KINGFISH GENUS_release`=sum(`KINGFISH GENUS/8835440600_RELEASE`,na.rm=T),
         `KNOBBED PORGY_claim`=sum(`KNOBBED PORGY/8835430506_CLAIM`,na.rm=T),
         `KNOBBED PORGY_release`=sum(`KNOBBED PORGY/8835430506_RELEASE`,na.rm=T),
         `LADYFISH_claim`=sum(`LADYFISH/8738010101_CLAIM`,na.rm=T),
         `LADYFISH_release`=sum(`LADYFISH/8738010101_RELEASE`,na.rm=T),
         `LANE SNAPPER_claim`=sum(`LANE SNAPPER/8835360112_CLAIM`,na.rm=T),
         `LANE SNAPPER_release`=sum(`LANE SNAPPER/8835360112_RELEASE`,na.rm=T),
         `LEATHERJACKET FAMILY_claim`=sum(`LEATHERJACKET FAMILY/8860020000_CLAIM`,na.rm=T),
         `LEATHERJACKET FAMILY_release`=sum(`LEATHERJACKET FAMILY/8860020000_RELEASE`,na.rm=T),
         `LEFTEYE FLOUNDER FAMILY_claim`=sum(`LEFTEYE FLOUNDER FAMILY/8857030000_CLAIM`,na.rm=T),
         `LEFTEYE FLOUNDER FAMILY_release`=sum(`LEFTEYE FLOUNDER FAMILY/8857030000_RELEASE`,na.rm=T),
         `LEFTEYE FLOUNDER GENUS_claim`=sum(`LEFTEYE FLOUNDER GENUS/8857030300_CLAIM`,na.rm=T),
         `LEFTEYE FLOUNDER GENUS_release`=sum(`LEFTEYE FLOUNDER GENUS/8857030300_RELEASE`,na.rm=T),
         `LEMON SHARK_claim`=sum(`LEMON SHARK/8708020801_CLAIM`,na.rm=T),
         `LEMON SHARK_release`=sum(`LEMON SHARK/8708020801_RELEASE`,na.rm=T),
         `LIONFISH_claim`=sum(`LIONFISH/8826011401_CLAIM`,na.rm=T),
         `LIONFISH_release`=sum(`LIONFISH/8826011401_RELEASE`,na.rm=T),
         `LITTLE TUNNY_claim`=sum(`LITTLE TUNNY/8850030102_CLAIM`,na.rm=T),
         `LITTLE TUNNY_release`=sum(`LITTLE TUNNY/8850030102_RELEASE`,na.rm=T),
         `LITTLEHEAD PORGY_claim`=sum(`LITTLEHEAD PORGY/8835430508_CLAIM`,na.rm=T),
         `LITTLEHEAD PORGY_release`=sum(`LITTLEHEAD PORGY/8835430508_RELEASE`,na.rm=T),
         `LIZARDFISH FAMILY_claim`=sum(`LIZARDFISH FAMILY/8762020000_CLAIM`,na.rm=T),
         `LIZARDFISH FAMILY_release`=sum(`LIZARDFISH FAMILY/8762020000_RELEASE`,na.rm=T),
         `LIZARDFISH GENUS_claim`=sum(`LIZARDFISH GENUS/8762020100_CLAIM`,na.rm=T),
         `LIZARDFISH GENUS_release`=sum(`LIZARDFISH GENUS/8762020100_RELEASE`,na.rm=T),
         `LONGNOSE GAR_claim`=sum(`LONGNOSE GAR/8732010101_CLAIM`,na.rm=T),
         `LONGNOSE GAR_release`=sum(`LONGNOSE GAR/8732010101_RELEASE`,na.rm=T),
         `LOOKDOWN_claim`=sum(`LOOKDOWN/8835280701_CLAIM`,na.rm=T),
         `LOOKDOWN_release`=sum(`LOOKDOWN/8835280701_RELEASE`,na.rm=T),
         `MACKEREL FAMILY_claim`=sum(`MACKEREL FAMILY/8850030000_CLAIM`,na.rm=T),
         `MACKEREL FAMILY_release`=sum(`MACKEREL FAMILY/8850030000_RELEASE`,na.rm=T),
         `MENHADEN GENUS_claim`=sum(`MENHADEN GENUS/8747010400_CLAIM`,na.rm=T),
         `MENHADEN GENUS_release`=sum(`MENHADEN GENUS/8747010400_RELEASE`,na.rm=T),
         `MORAY FAMILY_claim`=sum(`MORAY FAMILY/8741050000_CLAIM`,na.rm=T),
         `MORAY FAMILY_release`=sum(`MORAY FAMILY/8741050000_RELEASE`,na.rm=T),
         `MUTTON SNAPPER_claim`=sum(`MUTTON SNAPPER/8835360103_CLAIM`,na.rm=T),
         `MUTTON SNAPPER_release`=sum(`MUTTON SNAPPER/8835360103_RELEASE`,na.rm=T),
         `NEEDLEFISH FAMILY_claim`=sum(`NEEDLEFISH FAMILY/8803020000_CLAIM`,na.rm=T),
         `NEEDLEFISH FAMILY_release`=sum(`NEEDLEFISH FAMILY/8803020000_RELEASE`,na.rm=T),
         `NURSE SHARK_claim`=sum(`NURSE SHARK/8707020101_CLAIM`,na.rm=T),
         `NURSE SHARK_release`=sum(`NURSE SHARK/8707020101_RELEASE`,na.rm=T),
         `OYSTER TOADFISH_claim`=sum(`OYSTER TOADFISH/8783010201_CLAIM`,na.rm=T),
         `OYSTER TOADFISH_release`=sum(`OYSTER TOADFISH/8783010201_RELEASE`,na.rm=T),
         `PARROTFISH FAMILY_claim`=sum(`PARROTFISH FAMILY/8839030000_CLAIM`,na.rm=T),
         `PARROTFISH FAMILY_release`=sum(`PARROTFISH FAMILY/8839030000_RELEASE`,na.rm=T),
         `PERMIT_claim`=sum(`PERMIT/8835280902_CLAIM`,na.rm=T),
         `PERMIT_release`=sum(`PERMIT/8835280902_RELEASE`,na.rm=T),
         `PIGFISH_claim`=sum(`PIGFISH/8835400201_CLAIM`,na.rm=T),
         `PIGFISH_release`=sum(`PIGFISH/8835400201_RELEASE`,na.rm=T),
         `PINFISH_claim`=sum(`PINFISH/8835430201_CLAIM`,na.rm=T),
         `PINFISH_release`=sum(`PINFISH/8835430201_RELEASE`,na.rm=T),
         `POMPANO DOLPHIN_claim`=sum(`POMPANO DOLPHIN/8835290102_CLAIM`,na.rm=T),
         `POMPANO DOLPHIN_release`=sum(`POMPANO DOLPHIN/8835290102_RELEASE`,na.rm=T),
         `PORGY FAMILY_claim`=sum(`PORGY FAMILY/8835430000_CLAIM`,na.rm=T),
         `PORGY FAMILY_release`=sum(`PORGY FAMILY/8835430000_RELEASE`,na.rm=T),
         `PORKFISH_claim`=sum(`PORKFISH/8835400306_CLAIM`,na.rm=T),
         `PORKFISH_release`=sum(`PORKFISH/8835400306_RELEASE`,na.rm=T),
         `PUFFER FAMILY_claim`=sum(`PUFFER FAMILY/8861010000_CLAIM`,na.rm=T),
         `PUFFER FAMILY_release`=sum(`PUFFER FAMILY/8861010000_RELEASE`,na.rm=T),
         `PUFFER GENUS_claim`=sum(`PUFFER GENUS/8861010200_CLAIM`,na.rm=T),
         `PUFFER GENUS_release`=sum(`PUFFER GENUS/8861010200_RELEASE`,na.rm=T),
         `QUEEN TRIGGERFISH_claim`=sum(`QUEEN TRIGGERFISH/8860020202_CLAIM`,na.rm=T),
         `QUEEN TRIGGERFISH_release`=sum(`QUEEN TRIGGERFISH/8860020202_RELEASE`,na.rm=T),
         `RAINBOW RUNNER_claim`=sum(`RAINBOW RUNNER/8835281301_CLAIM`,na.rm=T),
         `RAINBOW RUNNER_release`=sum(`RAINBOW RUNNER/8835281301_RELEASE`,na.rm=T),
         `RED DRUM_claim`=sum(`RED DRUM/8835440901_CLAIM`,na.rm=T),
         `RED DRUM_release`=sum(`RED DRUM/8835440901_RELEASE`,na.rm=T),
         `RED GROUPER_claim`=sum(`RED GROUPER/8835020408_CLAIM`,na.rm=T),
         `RED GROUPER_release`=sum(`RED GROUPER/8835020408_RELEASE`,na.rm=T),
         `RED HIND_claim`=sum(`RED HIND/8835020406_CLAIM`,na.rm=T),
         `RED HIND_release`=sum(`RED HIND/8835020406_RELEASE`,na.rm=T),
         `RED PORGY_claim`=sum(`RED PORGY/8835430602_CLAIM`,na.rm=T),
         `RED PORGY_release`=sum(`RED PORGY/8835430602_RELEASE`,na.rm=T),
         `RED SNAPPER_claim`=sum(`RED SNAPPER/8835360107_CLAIM`,na.rm=T),
         `RED SNAPPER_release`=sum(`RED SNAPPER/8835360107_RELEASE`,na.rm=T),
         `REMORA FAMILY_claim`=sum(`REMORA FAMILY/8835270000_CLAIM`,na.rm=T),
         `REMORA FAMILY_release`=sum(`REMORA FAMILY/8835270000_RELEASE`,na.rm=T),
         `REMORA GENUS_claim`=sum(`REMORA GENUS/8835270100_CLAIM`,na.rm=T),
         `REMORA GENUS_release`=sum(`REMORA GENUS/8835270100_RELEASE`,na.rm=T),
         `REMORA_claim`=sum(`REMORA/8835270103_CLAIM`,na.rm=T),
         `REMORA_release`=sum(`REMORA/8835270103_RELEASE`,na.rm=T),
         `REQUIEM SHARK FAMILY_claim`=sum(`REQUIEM SHARK FAMILY/8708020000_CLAIM`,na.rm=T),
         `REQUIEM SHARK FAMILY_release`=sum(`REQUIEM SHARK FAMILY/8708020000_RELEASE`,na.rm=T),
         `REQUIEM SHARK GENUS_claim`=sum(`REQUIEM SHARK GENUS/8708020500_CLAIM`,na.rm=T),
         `REQUIEM SHARK GENUS_release`=sum(`REQUIEM SHARK GENUS/8708020500_RELEASE`,na.rm=T),
         `REQUIEM SHARK_claim`=sum(`REQUIEM SHARK/8708020000_CLAIM`,na.rm=T),
         `REQUIEM SHARK_release`=sum(`REQUIEM SHARK/8708020000_RELEASE`,na.rm=T),
         `ROCK HIND_claim`=sum(`ROCK HIND/8835020402_CLAIM`,na.rm=T),
         `ROCK HIND_release`=sum(`ROCK HIND/8835020402_RELEASE`,na.rm=T),
         `ROUND SCAD_claim`=sum(`ROUND SCAD/8835281202_CLAIM`,na.rm=T),
         `ROUND SCAD_release`=sum(`ROUND SCAD/8835281202_RELEASE`,na.rm=T),
         `SAILFISH_claim`=sum(`SAILFISH/8850060101_CLAIM`,na.rm=T),
         `SAILFISH_release`=sum(`SAILFISH/8850060101_RELEASE`,na.rm=T),
         `SAILORS CHOICE_claim`=sum(`SAILORS CHOICE/8835400117_CLAIM`,na.rm=T),
         `SAILORS CHOICE_release`=sum(`SAILORS CHOICE/8835400117_RELEASE`,na.rm=T),
         `SAND PERCH_claim`=sum(`SAND PERCH/8835021002_CLAIM`,na.rm=T),
         `SAND PERCH_release`=sum(`SAND PERCH/8835021002_RELEASE`,na.rm=T),
         `SAND SEATROUT_claim`=sum(`SAND SEATROUT/8835440106_CLAIM`,na.rm=T),
         `SAND SEATROUT_release`=sum(`SAND SEATROUT/8835440106_RELEASE`,na.rm=T),
         `SAUCEREYE PORGY_claim`=sum(`SAUCEREYE PORGY/8835430503_CLAIM`,na.rm=T),
         `SAUCEREYE PORGY_release`=sum(`SAUCEREYE PORGY/8835430503_RELEASE`,na.rm=T),
         `SCAMP_claim`=sum(`SCAMP/8835020505_CLAIM`,na.rm=T),
         `SCAMP_release`=sum(`SCAMP/8835020505_RELEASE`,na.rm=T),
         `SCORPIONFISH FAMILY_claim`=sum(`SCORPIONFISH FAMILY/8826010000_CLAIM`,na.rm=T),
         `SCORPIONFISH FAMILY_release`=sum(`SCORPIONFISH FAMILY/8826010000_RELEASE`,na.rm=T),
         `SCRAWLED COWFISH_claim`=sum(`SCRAWLED COWFISH/8860030201_CLAIM`,na.rm=T),
         `SCRAWLED COWFISH_release`=sum(`SCRAWLED COWFISH/8860030201_RELEASE`,na.rm=T),
         `SEA BASS FAMILY_claim`=sum(`SEA BASS FAMILY/8835020000_CLAIM`,na.rm=T),
         `SEA BASS FAMILY_release`=sum(`SEA BASS FAMILY/8835020000_RELEASE`,na.rm=T),
         `SEA BASS GENUS_claim`=sum(`SEA BASS GENUS/8835020300_CLAIM`,na.rm=T),
         `SEA BASS GENUS_release`=sum(`SEA BASS GENUS/8835020300_RELEASE`,na.rm=T),
         `SEA CATFISH FAMILY_claim`=sum(`SEA CATFISH FAMILY/8777180000_CLAIM`,na.rm=T),
         `SEA CATFISH FAMILY_release`=sum(`SEA CATFISH FAMILY/8777180000_RELEASE`,na.rm=T),
         `SEA CHUB GENUS_claim`=sum(`SEA CHUB GENUS/8835510100_CLAIM`,na.rm=T),
         `SEA CHUB GENUS_release`=sum(`SEA CHUB GENUS/8835510100_RELEASE`,na.rm=T),
         `SEAROBIN FAMILY_claim`=sum(`SEAROBIN FAMILY/8826020000_CLAIM`,na.rm=T),
         `SEAROBIN FAMILY_release`=sum(`SEAROBIN FAMILY/8826020000_RELEASE`,na.rm=T),
         `SEATROUT GENUS_claim`=sum(`SEATROUT GENUS/8835440100_CLAIM`,na.rm=T),
         `SEATROUT GENUS_release`=sum(`SEATROUT GENUS/8835440100_RELEASE`,na.rm=T),
         `SHEEPSHEAD_claim`=sum(`SHEEPSHEAD/8835430301_CLAIM`,na.rm=T),
         `SHEEPSHEAD_release`=sum(`SHEEPSHEAD/8835430301_RELEASE`,na.rm=T),
         `SHORT BIGEYE_claim`=sum(`SHORT BIGEYE/8835170201_CLAIM`,na.rm=T),
         `SHORT BIGEYE_release`=sum(`SHORT BIGEYE/8835170201_RELEASE`,na.rm=T),
         `SILK SNAPPER_claim`=sum(`SILK SNAPPER/8835360113_CLAIM`,na.rm=T),
         `SILK SNAPPER_release`=sum(`SILK SNAPPER/8835360113_RELEASE`,na.rm=T),
         `SILVER PERCH_claim`=sum(`SILVER PERCH/8835440301_CLAIM`,na.rm=T),
         `SILVER PERCH_release`=sum(`SILVER PERCH/8835440301_RELEASE`,na.rm=T),
         `SKIPJACK TUNA_claim`=sum(`SKIPJACK TUNA/8850030101_CLAIM`,na.rm=T),
         `SKIPJACK TUNA_release`=sum(`SKIPJACK TUNA/8850030101_RELEASE`,na.rm=T),
         `SNAKE EEL FAMILY_claim`=sum(`SNAKE EEL FAMILY/8741130000_CLAIM`,na.rm=T),
         `SNAKE EEL FAMILY_release`=sum(`SNAKE EEL FAMILY/8741130000_RELEASE`,na.rm=T),
         `SNAPPER FAMILY_claim`=sum(`SNAPPER FAMILY/8835360000_CLAIM`,na.rm=T),
         `SNAPPER FAMILY_release`=sum(`SNAPPER FAMILY/8835360000_RELEASE`,na.rm=T),
         `SNOOK GENUS_claim`=sum(`SNOOK GENUS/8835010100_CLAIM`,na.rm=T),
         `SNOOK GENUS_release`=sum(`SNOOK GENUS/8835010100_RELEASE`,na.rm=T),
         `SNOWY GROUPER_claim`=sum(`SNOWY GROUPER/8835020411_CLAIM`,na.rm=T),
         `SNOWY GROUPER_release`=sum(`SNOWY GROUPER/8835020411_RELEASE`,na.rm=T),
         `SOAPFISH GENUS_claim`=sum(`SOAPFISH GENUS/8835030200_CLAIM`,na.rm=T),
         `SOAPFISH GENUS_release`=sum(`SOAPFISH GENUS/8835030200_RELEASE`,na.rm=T),
         `SOUTHERN FLOUNDER_claim`=sum(`SOUTHERN FLOUNDER/8857030304_CLAIM`,na.rm=T),
         `SOUTHERN FLOUNDER_release`=sum(`SOUTHERN FLOUNDER/8857030304_RELEASE`,na.rm=T),
         `SOUTHERN KINGFISH_claim`=sum(`SOUTHERN KINGFISH/8835440601_CLAIM`,na.rm=T),
         `SOUTHERN KINGFISH_release`=sum(`SOUTHERN KINGFISH/8835440601_RELEASE`,na.rm=T),
         `SOUTHERN PUFFER_claim`=sum(`SOUTHERN PUFFER/8861010208_CLAIM`,na.rm=T),
         `SOUTHERN PUFFER_release`=sum(`SOUTHERN PUFFER/8861010208_RELEASE`,na.rm=T),
         `SPANISH HOGFISH_claim`=sum(`SPANISH HOGFISH/8839010302_CLAIM`,na.rm=T),
         `SPANISH HOGFISH_release`=sum(`SPANISH HOGFISH/8839010302_RELEASE`,na.rm=T),
         `SPANISH MACKEREL_claim`=sum(`SPANISH MACKEREL/8850030502_CLAIM`,na.rm=T),
         `SPANISH MACKEREL_release`=sum(`SPANISH MACKEREL/8850030502_RELEASE`,na.rm=T),
         `SPECKLED HIND_claim`=sum(`SPECKLED HIND/8835020404_CLAIM`,na.rm=T),
         `SPECKLED HIND_release`=sum(`SPECKLED HIND/8835020404_RELEASE`,na.rm=T),
         `SPINNER SHARK_claim`=sum(`SPINNER SHARK/8708020509_CLAIM`,na.rm=T),
         `SPINNER SHARK_release`=sum(`SPINNER SHARK/8708020509_RELEASE`,na.rm=T),
         `SPOTTAIL PINFISH_claim`=sum(`SPOTTAIL PINFISH/8835430401_CLAIM`,na.rm=T),
         `SPOTTAIL PINFISH_release`=sum(`SPOTTAIL PINFISH/8835430401_RELEASE`,na.rm=T),
         `SPOTTED SEATROUT_claim`=sum(`SPOTTED SEATROUT/8835440102_CLAIM`,na.rm=T),
         `SPOTTED SEATROUT_release`=sum(`SPOTTED SEATROUT/8835440102_RELEASE`,na.rm=T),
         `SQUIRRELFISH FAMILY_claim`=sum(`SQUIRRELFISH FAMILY/8810080000_CLAIM`,na.rm=T),
         `SQUIRRELFISH FAMILY_release`=sum(`SQUIRRELFISH FAMILY/8810080000_RELEASE`,na.rm=T),
         `SQUIRRELFISH GENUS_claim`=sum(`SQUIRRELFISH GENUS/8810080100_CLAIM`,na.rm=T),
         `SQUIRRELFISH GENUS_release`=sum(`SQUIRRELFISH GENUS/8810080100_RELEASE`,na.rm=T),
         `SQUIRRELFISH_claim`=sum(`SQUIRRELFISH/8810080101_CLAIM`,na.rm=T),
         `SQUIRRELFISH_release`=sum(`SQUIRRELFISH/8810080101_RELEASE`,na.rm=T),
         `STINGRAY FAMILY_claim`=sum(`STINGRAY FAMILY/8713050000_CLAIM`,na.rm=T),
         `STINGRAY FAMILY_release`=sum(`STINGRAY FAMILY/8713050000_RELEASE`,na.rm=T),
         `STINGRAY GENUS_claim`=sum(`STINGRAY GENUS/8713050100_CLAIM`,na.rm=T),
         `STINGRAY GENUS_release`=sum(`STINGRAY GENUS/8713050100_RELEASE`,na.rm=T),
         `STRIPED BURRFISH_claim`=sum(`STRIPED BURRFISH/8861030101_CLAIM`,na.rm=T),
         `STRIPED BURRFISH_release`=sum(`STRIPED BURRFISH/8861030101_RELEASE`,na.rm=T),
         `STRIPED MULLET_claim`=sum(`STRIPED MULLET/8836010101_CLAIM`,na.rm=T),
         `STRIPED MULLET_release`=sum(`STRIPED MULLET/8836010101_RELEASE`,na.rm=T),
         `TILEFISH FAMILY_claim`=sum(`TILEFISH FAMILY/8835220000_CLAIM`,na.rm=T),
         `TILEFISH FAMILY_release`=sum(`TILEFISH FAMILY/8835220000_RELEASE`,na.rm=T),
         `TOADFISH GENUS_claim`=sum(`TOADFISH GENUS/8783010200_CLAIM`,na.rm=T),
         `TOADFISH GENUS_release`=sum(`TOADFISH GENUS/8783010200_RELEASE`,na.rm=T),
         `TOMTATE_claim`=sum(`TOMTATE/8835400101_CLAIM`,na.rm=T),
         `TOMTATE_release`=sum(`TOMTATE/8835400101_RELEASE`,na.rm=T),
         `TRIPLETAIL_claim`=sum(`TRIPLETAIL/8835380101_CLAIM`,na.rm=T),
         `TRIPLETAIL_release`=sum(`TRIPLETAIL/8835380101_RELEASE`,na.rm=T),
         `UNIDENTIFIED EEL_claim`=sum(`UNIDENTIFIED EEL/8741000000_CLAIM`,na.rm=T),
         `UNIDENTIFIED EEL_release`=sum(`UNIDENTIFIED EEL/8741000000_RELEASE`,na.rm=T),
         `UNIDENTIFIED FISH_claim`=sum(`UNIDENTIFIED FISH/1000000000_CLAIM`,na.rm=T),
         `UNIDENTIFIED FISH_release`=sum(`UNIDENTIFIED FISH/1000000000_RELEASE`,na.rm=T),
         `UNIDENTIFIED SHARK OR RAY_claim`=sum(`UNIDENTIFIED SHARK OR RAY/8701000000_CLAIM`,na.rm=T),
         `UNIDENTIFIED SHARK OR RAY_release`=sum(`UNIDENTIFIED SHARK OR RAY/8701000000_RELEASE`,na.rm=T),
         `VERMILION SNAPPER_claim`=sum(`VERMILION SNAPPER/8835360501_CLAIM`,na.rm=T),
         `VERMILION SNAPPER_release`=sum(`VERMILION SNAPPER/8835360501_RELEASE`,na.rm=T),
         `WAHOO_claim`=sum(`WAHOO/8850030601_CLAIM`,na.rm=T),
         `WAHOO_release`=sum(`WAHOO/8850030601_RELEASE`,na.rm=T),
         `WHITE GRUNT_claim`=sum(`WHITE GRUNT/8835400102_CLAIM`,na.rm=T),
         `WHITE GRUNT_release`=sum(`WHITE GRUNT/8835400102_RELEASE`,na.rm=T),
         `WHITEBONE PORGY_claim`=sum(`WHITEBONE PORGY/8835430505_CLAIM`,na.rm=T),
         `WHITEBONE PORGY_release`=sum(`WHITEBONE PORGY/8835430505_RELEASE`,na.rm=T),
         `WRASSE FAMILY_claim`=sum(`WRASSE FAMILY/8839010000_CLAIM`,na.rm=T),
         `WRASSE FAMILY_release`=sum(`WRASSE FAMILY/8839010000_RELEASE`,na.rm=T),
         `YELLOW JACK_claim`=sum(`YELLOW JACK/8835280301_CLAIM`,na.rm=T),
         `YELLOW JACK_release`=sum(`YELLOW JACK/8835280301_RELEASE`,na.rm=T),
         `YELLOWTAIL SNAPPER_claim`=sum(`YELLOWTAIL SNAPPER/8835360401_CLAIM`,na.rm=T),
         `YELLOWTAIL SNAPPER_release`=sum(`YELLOWTAIL SNAPPER/8835360401_RELEASE`,na.rm=T)
  ) %>%
  ungroup() %>%
  select(-ends_with("unadj"),
         -ID_CODE) %>%
  distinct() %>%
  mutate(psu_id = as.character(psu_id))

#only keep the first interview per trip done by MRIP
mrip_ct <- mrip_ct %>%
  select(-ends_with("CLAIM",ignore.case = F),
         -ends_with("RELEASE",ignore.case = F)) %>% 
  group_by(PRT_CODE) %>%
  filter(date_time_mrip == min(date_time_mrip)) %>%
  ungroup()
mrip_ct <- mrip_ct %>% 
  mutate(wp_int = as.numeric(wp_int))

mrip_dmatch_17 <- read_sas("data/tidy_naive_20180912.sas7bdat")
mrip_dmatch_17 <- mrip_dmatch_17 %>% 
  select(CLS_ID,date_time_mrip) %>% 
  mutate(reported = 1,
         CLS_ID = as.character(CLS_ID))

mrip_ct_dm <- mrip_ct %>% 
  left_join(mrip_dmatch_17)
mrip_ct_dm <- mrip_ct_dm %>% 
  mutate(reported = if_else(is.na(reported),
                            0,reported))

tyrs <- svydesign(~psu_id,
                  weights = ~wp_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_ct_dm)

options(survey.lonely.psu = "adjust") 

ty_hat_rs <- svytotal(~`RED SNAPPER_claim`,
                      design = tyrs)
#ty*_hat
ty_star_hat_rs <- svytotal(~`RED SNAPPER_kept`,
                           design = tyrs)

#t_y*
ty_star_rs <- sum(cls_17$`RED SNAPPER_kept`,na.rm=T)

n1_hat <- svytotal(~reported,
                   design = tyrs)

