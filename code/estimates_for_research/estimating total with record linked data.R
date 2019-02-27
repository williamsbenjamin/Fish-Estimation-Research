library(tidyverse)
library(ggthemes)
library(lubridate)
library(survey)
library(haven)

mrip_all_16 <- read_csv("data/mrip_all_16_public.csv")
mrip_all_17 <- read_csv("data/mrip_all_17_public.csv")
cls_16 <- read_csv("data/cls_16_public.csv")
cls_17 <- read_csv("data/cls_17_public.csv")

####USING ALL RECORD LINKAGE ROWS
#2016 

des16_all <- svydesign(id=~psu_id,
                       weights=~wp_int,
                       strata=~strat_id,
                       nest=T,
                       data=mrip_all_16)
options(survey.lonely.psu = "adjust") 
#centers the stratum with only 1 psu
#at population 
#mean for variance estimation

tyc_all <- svyratio(~total_claim_mrip,~total_kept_cls,
                    design=des16_all,na.rm=T)
tyc_16_all_se <- as.numeric(predict(tyc_all,total=sum(cls_16$total_kept_cls,na.rm=T))$se)
#tyc_16_all_se
tyc_16_all_total <- as.numeric(predict(tyc_all,total=sum(cls_16$total_kept_cls,na.rm=T))$total)

ty2.r <- svyratio(~delta,~reported,design=des16_all,na.rm=T)

ty2_16_all_se <- as.numeric(predict(ty2.r,total = nrow(cls_16))$se)
#ty2_all_se
ty2_16_all_total <- predict(ty2.r,total = nrow(cls_16))[[1]] + sum(cls_16$total_kept_cls,na.rm=T)
ty2_16_all_total <- as.numeric(ty2_16_all_total)
#ty2_16_all_total

typ <- svyratio(~total_claim_mrip,~reported,design=des16_all,na.rm=T)
typ_16_all_se <- as.numeric(predict(typ,total = nrow(cls_16),na.rm=T)$se)
#typ_16_all_se
typ_16_all_total <- as.numeric(predict(typ,total = nrow(cls_16),na.rm=T)$total)
#typ_16_all_total

tydiff.d <- svytotal(~delta,design=des16_all,na.rm=T)
tydiff_16_all_se <- as.numeric(SE(tydiff.d))
#tydiff_16_all_se
tydiff_16_all_total <- tydiff.d[[1]] + sum(cls_16$total_kept_cls,na.rm=T)
#tydiff_16_all_total

tynew.nr <- svytotal(~non_reporter_mrip_claim,design=des16_all)
tynew_16_all_se <- as.numeric(SE(tynew.nr))
#tynew_16_all_se

tynew_16_all_total <- tynew.nr[[1]] + sum(cls_16$total_kept_cls,na.rm=T)
#tynew_16_all_total

#2017
desi17_all <- svydesign(id=~psu_id,
                    weights=~wp_int,
                    strata = ~strat_id,
                    nest=T,
                    data=mrip_all_17)
options(survey.lonely.psu = "adjust") 

#centers the stratum with only 1 psu
#at population 
#mean for variance estimation
tyc17_all <- svyratio(~total_claim_mrip,
                      ~total_kept_cls,
                      design=desi17_all,
                      na.rm=T)

tyc_17_all_se <- as.numeric(predict(tyc17_all,total=sum(cls_17$total_kept_cls,na.rm=T))$se)
#tyc_17_all_se
tyc_17_all_total <- as.numeric(predict(tyc17_all,total=sum(cls_17$total_kept_cls,na.rm=T))$total)
#tyc_17_all_total

ty2.r17_all <- svyratio(~delta,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
ty2_17_all_se <- as.numeric(predict(ty2.r17_all,total = nrow(cls_17))$se)
#ty2_17_all_se

ty2_17_all_total <- predict(ty2.r17_all,total = nrow(cls_17))[[1]] + sum(cls_17$total_kept_cls,na.rm=T)
ty2_17_all_total <- as.numeric(ty2_17_all_total)
#ty2_17_all_total

typ.17 <- svyratio(~total_claim_mrip,~reported,design=desi17_all,na.rm=T)
typ_17_all_se <- as.numeric(predict(typ.17,total = nrow(cls_17),na.rm=T)$se)
#typ_17_all_se
typ_17_all_total <- as.numeric(predict(typ.17,total = nrow(cls_17),na.rm=T)$total)
#typ_17_all_total

tydiff.d.17 <- svytotal(~delta,design=desi17_all,na.rm=T)
tydiff_17_all_se <- as.numeric(SE(tydiff.d.17))
#tydiff_17_all_se
tydiff_17_all_total <- tydiff.d.17[[1]] + sum(cls_17$total_kept_cls,na.rm=T)
#tydiff_17_all_total

tynew.nr.17 <- svytotal(~non_reporter_mrip_claim,design=desi17_all,na.rm=T)
tynew_17_all_se <- as.numeric(SE(tynew.nr.17))
#tynew_17_all_se
tynew_17_all_total <- tynew.nr.17[[1]] + sum(cls_17$total_kept_cls,na.rm=T)
#tynew_17_all_total

# USING RECORD LINKAGE ROWS WITH CUTOFF AT 15

mrip_all_16_2 <- read_csv("data/mrip_all_16_cutoff_public.csv")
mrip_all_17_2 <- read_csv("data/mrip_all_17_cutoff_public.csv")

#2016 
des16_all_2 <- svydesign(id=~psu_id,
                       weights=~wp_int,
                       strata=~strat_id,
                       nest=T,
                       data=mrip_all_16_2)
options(survey.lonely.psu = "adjust") 
#centers the stratum with only 1 psu
#at population 
#mean for variance estimation

tyc_all_2 <- svyratio(~total_claim_mrip,
                      ~total_kept_cls,
                      design=des16_all_2,
                      na.rm=T)
tyc_16_cut_se <- as.numeric(predict(tyc_all_2,total=sum(cls_16$total_kept_cls,na.rm=T))$se)
#tyc_16_cut_se
tyc_16_cut_total <- as.numeric(predict(tyc_all_2,total=sum(cls_16$total_kept_cls,na.rm=T))$total)
#tyc_16_cut_total

ty2.r_2 <- svyratio(~delta,~reported,design=des16_all_2,na.rm=T)
ty2_16_cut_se <- as.numeric(predict(ty2.r_2,total = nrow(cls_16))$se)
#ty2_16_cut_se
ty2_16_cut_total <- predict(ty2.r_2,total = nrow(cls_16))[[1]] + sum(cls_16$total_kept_cls,na.rm=T)
ty2_16_cut_total <- as.numeric(ty2_16_cut_total)
#ty2_16_cut_total

typ.2 <- svyratio(~total_claim_mrip,~reported,design=des16_all_2,na.rm=T)
typ_16_cut_se <- as.numeric(predict(typ.2,total = nrow(cls_16),na.rm=T)$se)
#typ_16_cut_se
typ_16_cut_total <- as.numeric(predict(typ.2,total = nrow(cls_16),na.rm=T)$total)
#typ_16_cut_total

tydiff.d.2 <- svytotal(~delta,design=des16_all_2,na.rm=T)
tydiff_16_cut_se <- as.numeric(SE(tydiff.d.2))
#tydiff_16_cut_se
tydiff_16_cut_total <- tydiff.d.2[[1]] + sum(cls_16$total_kept_cls,na.rm=T)
#tydiff_16_cut_total

tynew.nr.2 <- svytotal(~non_reporter_mrip_claim,design=des16_all_2,na.rm=T)
tynew_16_cut_se <- as.numeric(SE(tynew.nr.2))
#tynew_16_cut_se
tynew_16_cut_total <- tynew.nr.2[[1]] + sum(cls_16$total_kept_cls,na.rm=T)
#tynew_16_cut_total

#2017
desi17_all_2 <- svydesign(id=~psu_id,
                        weights=~wp_int,
                        strata = ~strat_id,
                        nest=T,
                        data=mrip_all_17_2)
options(survey.lonely.psu = "adjust") 
#centers the stratum with only 1 psu
#at population 
#mean for variance estimation
tyc17_all_2 <- svyratio(~total_claim_mrip,
                      ~total_kept_cls,
                      design=desi17_all_2)

tyc_17_cut_se <- as.numeric(predict(tyc17_all_2,total=sum(cls_17$total_kept_cls,na.rm=T))$se)
#tyc_17_cut_se
tyc_17_cut_total <- as.numeric(predict(tyc17_all_2,total=sum(cls_17$total_kept_cls,na.rm=T))$total)
#tyc_17_cut_total
ty2.r17_all_2 <- svyratio(~delta,
                        ~reported,
                        design=desi17_all_2,
                        na.rm=T)
ty2_17_cut_se <- as.numeric(predict(ty2.r17_all_2,total = nrow(cls_17))$se)
#ty2_17_cut_se
ty2_17_cut_total <- predict(ty2.r17_all_2,total = nrow(cls_17))[[1]] + sum(cls_17$total_kept_cls,na.rm=T)
ty2_17_cut_total <- as.numeric(ty2_17_cut_total)
#ty2_17_cut_total
typ.2.17 <- svyratio(~total_claim_mrip,~reported,design=desi17_all_2,na.rm=T)
typ_17_cut_se <- as.numeric(predict(typ.2.17,total = nrow(cls_17),na.rm=T)$se)
#typ_17_cut_se
typ_17_cut_total <- as.numeric(predict(typ.2.17,total = nrow(cls_17),na.rm=T)$total)
#typ_17_cut_total

tydiff.d.2.17 <- svytotal(~delta,design=desi17_all_2,na.rm=T)
tydiff_17_cut_se <- as.numeric(SE(tydiff.d.2.17))
#tydiff_17_cut_se
tydiff_17_cut_total <- tydiff.d.2.17[[1]] + sum(cls_17$total_kept_cls,na.rm=T)
#tydiff_17_cut_total

tynew.nr.2.17 <- svytotal(~non_reporter_mrip_claim,design=desi17_all_2,na.rm=T)
tynew_17_cut_se <- as.numeric(SE(tynew.nr.2.17))
#tynew_17_cut_se
tynew_17_cut_total <- tynew.nr.2.17[[1]] + sum(cls_17$total_kept_cls,na.rm=T)
#tynew_17_cut_total
