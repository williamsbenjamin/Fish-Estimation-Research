# hold all but one variable constant to get distribution of 
# linking variables

#Doing this for simulations

#######
## Total Catch
#######

n_match <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
  nrow()
#agree
p.agree_total_catch <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
  filter(!is.na(diff_total_catch)) %>% 
  filter(between(diff_total_catch,-0.99999,0.99999)) %>%
  nrow() / 
  n_match

#close
p.close_total_catch <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T)  %>% #remove duplicates
  filter(between(diff_total_catch,-5.99999,-1) |
           between(diff_total_catch,1,5.99999)) %>%
  nrow() /
  n_match

#far
p.far_total_catch <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T)  %>% 
  filter(!is.na(diff_total_catch)) %>% 
  filter(between(diff_total_catch,-15.99999,-6) |
           between(diff_total_catch,6,15.99999)) %>%
  nrow() /
  n_match

#disagree
p.disagree_total_catch <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T)  %>% 
  filter(!is.na(diff_total_catch)) %>% 
  filter(between(diff_total_catch,-3000,-16) |
           between(diff_total_catch,16,9999)) %>%
  nrow() /
  n_match

####
# Total Release
####

n_total.release <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
   nrow()

#agree
p.agree_total_release <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
  filter(between(diff_total_release,-0.9999,0.9999) ) %>%
  nrow() /
  n_total.release

#close
p.close_total_release <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  filter(between(diff_total_release,-4.9999,-1) | 
                between(diff_total_release,1,4.9999)) %>%
  nrow() /
  n_total.release

#far
p.far_total_release <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  filter(between(diff_total_release,-10.9999,-5) |
           between(diff_total_release,5,10.9999)) %>%
  nrow() /
  n_total.release

#disagree
p.disagree_total_release <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  filter(between(diff_total_release,-3000,-11) |
           between(diff_total_release,11,300)) %>%
  nrow() /
  n_total.release

#####
# KM Distance
#####

n_total_distance <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  filter(km_difference != "NA") %>%
  nrow()

#agree
p.agree_total_distance <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  filter(km_difference != "NA") %>%
  filter(between(km_difference,0,15)) %>%
  nrow() /
  n_total_distance

#close
p.close_total_distance <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  filter(km_difference != "NA") %>%
  filter(between(km_difference,15.0001,40)) %>%
  nrow() /
  n_total_distance

#disagree
p.disagree_total_distance <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  filter(km_difference != "NA") %>%
  filter(between(km_difference,40.0001,4000)) %>%
  nrow() /
  n_total_distance

#Red Snapper Catch Difference

n_total_rs <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  nrow()
#agree
p.agree_total_rs <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
  filter(between(diff_red_snapper,-0.01,0.01)) %>%
  nrow() /
  n_total_rs

#close
p.disagree_total_rs <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  filter(between(diff_red_snapper,-10,-0.1001) |
           between(diff_red_snapper,0.1001,10)) %>%
  nrow() /
  n_total_rs


######
# Number Species Caught Difference
######

n_total_num_species <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  nrow()

p.agree_total_num_species <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
  filter(diff_spec_caught %in% c(0)) %>%
  nrow() /
  n_total_num_species

#close
p.close_total_num_species <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
  filter(diff_spec_caught %in% c(-1,1)) %>%
  nrow() /
  n_total_num_species
#far
p.far_total_num_species <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
  filter(diff_spec_caught %in% c(-2,2)) %>%
  nrow() /
  n_total_num_species

#disagree
p.disagree_total_num_species <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% #remove duplicates
  filter(diff_spec_caught %in% c(-10:-3,3:10)) %>%
  nrow() /
  n_total_num_species

#####
# Number Species Released Difference
#####

n_total_num_species_rel <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
  nrow()

p.agree_total_num_species_rel <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
  filter(diff_spec_rel %in% c(0)) %>%
  nrow() /
  n_total_num_species_rel

#close
p.close_total_num_species_rel <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%  #remove duplicates
  filter(diff_spec_rel %in% c(-1,1)) %>%
  nrow()/
  n_total_num_species_rel
#far
p.far_total_num_species_rel <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%  #remove duplicates
  filter(diff_spec_rel %in% c(-2,2)) %>%
  nrow() /
  n_total_num_species_rel
#disagree
p.disagree_total_num_spec_rel <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%  #remove duplicates
  filter(diff_spec_rel %in% c(-10:-3,3:10)) %>%
  nrow()/
  n_total_num_species_rel

#####
# Number of anglers
#####
n_num_anglers <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T)  %>% 
  nrow()

p.agree_num_anglers <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
  filter(diff_num_anglers %in% c(0)) %>% 
  nrow() / 
  n_num_anglers

p.close_num_anglers <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
  filter(diff_num_anglers %in% c(-1,1)) %>% 
  nrow()/ 
  n_num_anglers


p.disagree_num_anglers <- tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  filter(diff_num_anglers %in% c(-15:-2,2:15)) %>% 
  nrow()/ 
  n_num_anglers

#####
# Date
#####

n_total_date <-  tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
  nrow() 

p.agree_date <-  tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  filter(between(diff_date,-0.15,0.15)) %>% 
  nrow() /
  n_total_date

#close
p.close_date <-  tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T)%>%
  filter(between(diff_date,-1, -0.15001) |
           between(diff_date,0.15001,1)) %>%
  nrow() /
  n_total_date
#far
p.disagree_date <-  tidy_all_matches_docks %>%
  filter(true_match_id_cls == true_match_id_mrip) %>%
  distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
  filter(between(diff_date,-10,-1.0001) |
          between(diff_date, 1.0001, 10)) %>%
  nrow() /
  n_total_date


