library(tidyverse)
e_dat <- read_rds("code/simulations/for_m2/sim_e/sim_results_e_2-19.rds")
pop_tot_e <- 375420

##Coverage Rates
e_dat %>% 
  mutate(low_yp = (t_yp_est - 1.96*t_yp_se),
         high_yp = (t_yp_est + 1.96*t_yp_se),
         cover_yp = if_else(pop_tot_e >= low_yp &
                              pop_tot_e <= high_yp,
                            1,0),
         low_yc = (t_yc_est - 1.96*t_yc_se),
         high_yc = (t_yc_est + 1.96*t_yc_se),
         cover_yc = if_else(pop_tot_e >= low_yc &
                              pop_tot_e <= high_yc,
                            1,0),
         low_y2 = (t_y2_est - 1.96*t_y2_se),
         high_y2 = (t_y2_est + 1.96*t_y2_se),
         cover_y2 = if_else(pop_tot_e >= low_y2 &
                              pop_tot_e <= high_y2,
                            1,0),
         low_yd = (t_ydiff_est - 1.96*t_ydiff_se),
         high_yd = (t_ydiff_est + 1.96*t_ydiff_se),
         cover_yd = if_else(pop_tot_e >= low_yd &
                              pop_tot_e <= high_yd,
                            1,0),
         low_yn = (t_ynew_est - 1.96*t_ynew_se),
         high_yn = (t_ynew_est + 1.96*t_ynew_se),
         cover_yn = if_else(pop_tot_e >= low_yn &
                              pop_tot_e <= high_yn,
                            1,0)
  ) %>% 
  summarise(typ_cov_rate = sum(cover_yp)/n(),
            tyc_cov_rate = sum(cover_yc)/n(),
            ty2_cov_rate = sum(cover_y2)/n(),
            tydiff_cov_rate = sum(cover_yd)/n(),
            tynew_cov_rate = sum(cover_yn)/n())

#SE

e_dat %>% 
  summarise(yp_se = mean(t_yp_se),
            yc_se = mean(t_yc_se),
            y2_se = mean(t_y2_se),
            ydiff_se = mean(t_ydiff_se),
            ynew_se = mean(t_ynew_se))

#Rel Bias
e_dat %>% 
  summarise(yp_rb = (mean(t_yp_est) - pop_tot_e) / pop_tot_e,
            yc_rb = (mean(t_yc_est) - pop_tot_e) / pop_tot_e,
            y2_rb = (mean(t_y2_est)- pop_tot_e) / pop_tot_e,
            ydiff_rb = (mean(t_ydiff_est)- pop_tot_e) / pop_tot_e,
            ynew_rb = (mean(t_ynew_est)- pop_tot_e) / pop_tot_e)

#Bias
e_dat %>% 
  summarise(yp_b = (mean(t_yp_est) - pop_tot_e),
            yc_b = (mean(t_yc_est) - pop_tot_e),
            y2_b = (mean(t_y2_est)- pop_tot_e),
            ydiff_b = (mean(t_ydiff_est)- pop_tot_e),
            ynew_b = (mean(t_ynew_est)- pop_tot_e))

#Error Rates
e_dat %>% 
  summarise(fp = mean(fp_rate),
            fn = mean(fn_rate),
            mm = mean(mm_rate),
            tp = mean(tp_rate))

e_dat %>% 
  summarise(tm = mean(true_match),
            im = mean(identified_match),
            sd_tm = sd(true_match),
            sd_im = sd(identified_match))
