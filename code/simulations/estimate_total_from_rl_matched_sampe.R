matches <- tidy_all_matches_one_per %>% 
  filter(rl_score > cutoff) %>% 
  mutate(match = 1)

match_ids <- pull(matches,id)

non_matches <- sample_pop %>% 
  filter(!id %in% match_ids)

matched_samp <- non_matches %>% 
  bind_rows(matches)


matched_samp <- matched_samp %>% 
  mutate(
    cls_red_snapper_harvest = if_else(is.na(cls_red_snapper_harvest),
                                      0,cls_red_snapper_harvest),
    delta_c = red_snapper_harvest -
           cls_red_snapper_harvest,
    match = if_else(is.na(match),
                    0,match),
    non_reporter_mrip_claim = if_else(match == 0,
                                      red_snapper_harvest,0))

s_design <- svydesign(id =~psu_id,
                      strat = ~strat_int,
                      prob = ~prob,
                      nest = T,
                      data = matched_samp)
options(survey.lonely.psu = "adjust") 

# t_y2(
#   data = matched_samp,
#   delta = delta_c,
#   survey_design = s_design,
#   captured = match,
#   capture_units = n1,
#   total_from_capture = sum(self_reports$cls_red_snapper_harvest)
# )
# 
# svytotal(~red_snapper_harvest,
#          s_design)
svytotal(~match,
          s_design)
svytotal(~cls_red_snapper_harvest,
          s_design)
# n1
sum(self_reports$cls_red_snapper_harvest)
sum(pop$red_snapper_harvest)


tyc <- svyratio(~red_snapper_harvest,
                        ~cls_red_snapper_harvest,
                        design=s_design)

tyc_se <- as.numeric(
  predict(tyc,
          total = sum(self_reports$cls_red_snapper_harvest))$se)

tyc_total <- as.numeric(
  predict(tyc,
          total = sum(self_reports$cls_red_snapper_harvest))$total)

ty2 <- svyratio(~delta_c,
                ~match,
                design = s_design,
                na.rm = T)
ty2_se <- as.numeric(
  predict(ty2,total = n1)$se)

ty2_total <- predict(ty2,total = n1)[[1]] + 
  sum(self_reports$cls_red_snapper_harvest,na.rm=T)
ty2_total <- as.numeric(ty2_total)


typ <- svyratio(~red_snapper_harvest,
                ~match,
                design = s_design,
                na.rm = T)
typ_se <- as.numeric(
  predict(typ,total = n1,na.rm=T)$se)

typ_total <- as.numeric(
  predict(typ,total = n1,na.rm=T)$total)


tydiff <- svytotal(~delta_c,design = s_design,na.rm=T)
tydiff_se <- as.numeric(SE(tydiff))

tydiff_total <- tydiff[[1]] + sum(self_reports$cls_red_snapper_harvest,
                                  na.rm=T)

tynew.nr <- svytotal(~non_reporter_mrip_claim,
                     design = s_design,
                     na.rm=T)
tynew_se <- as.numeric(SE(tynew.nr))

tynew_total <- tynew.nr[[1]] + 
  sum(self_reports$cls_red_snapper_harvest,na.rm=T)

