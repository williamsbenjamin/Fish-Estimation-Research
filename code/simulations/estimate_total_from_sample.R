sample_pop_t <- s_pop %>%
  unnest() %>%
  rename(true_match_id_mrip = true_match_id) %>% 
  mutate(delta_c = red_snapper_harvest -
           cls_red_snapper_harvest,
         CLS = if_else(true_match_id_mrip !=0,
                       1,0)
           )
s_design <- svydesign(id =~psu_id,
                      strat = ~strat_int,
                      prob = ~prob,
                      nest = T,
                      data = sample_pop_t)
options(survey.lonely.psu = "adjust") 


t_y2(
  data = sample_pop_t,
  delta = delta_c,
  survey_design = s_design,
  captured = CLS,
  capture_units = n1,
  total_from_capture = sum(self_reports$cls_red_snapper_harvest)
)

svytotal(~red_snapper_harvest,
        s_design)
svytotal(~CLS,
         s_design)
svytotal(~cls_red_snapper_harvest,
         s_design)
sum(self_reports$cls_red_snapper_harvest)
sum(pop$red_snapper_harvest)
n1
