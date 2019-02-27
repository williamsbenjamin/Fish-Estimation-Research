
library(tidyverse)
##Info about all possible matches
tidy_all_matches_docks_rl <- read_csv("data/tidy_all_matches_docks_rl.csv",
                                      col_types = cols(psu_id = col_character()))

tidy_all_matches_docks_rl %>%
  select(ends_with("pattern")) %>% 
  distinct() %>% 
  nrow()

non_matches_comparisons <- tidy_all_matches_docks_rl %>%
  unite(patt,ends_with("pattern")) %>% 
  distinct(patt) %>% 
  pull(patt)

#info about linkage patterns for potential links (one_per)
one_per <- read_csv("data/tidy_all_matches_rl_one_per.csv")
one_per <- one_per %>% 
  unite(patt,ends_with("pattern"),remove = FALSE) 
one_per %>% 
  select(ends_with("pattern")) %>% 
  distinct() %>% 
  nrow()

matches_comparisons <- one_per %>% 
  filter(angler_pattern %in% c("agree"),
         date_pattern %in% c("agree","close"),
         km_diff_pattern %in% c("agree","close")) %>% 
  distinct(patt) %>% 
  pull(patt)

matching_dataset <- one_per %>% 
  filter(angler_pattern %in% c("agree"),
         date_pattern %in% c("agree","close"),
         km_diff_pattern %in% c("agree","close"))
########
#Probability of observing agreement pattern in matches
#Should I do entire pattern or each variable on its own

#non_matches_comparisons 
tidy_all_matches_docks_rl <- tidy_all_matches_docks_rl %>%
  unite(patt,ends_with("pattern"))

cutpoint = 11
##False-positives
u_probs <- c()
for(i in 1:length(non_matches_comparisons)) {
  u_probs[i] <- sum(tidy_all_matches_docks_rl$patt == non_matches_comparisons[i]) /
    nrow(tidy_all_matches_docks_rl)
}

a1_probs <- c()
for(j in 1:length(non_matches_comparisons)) {
  a1_probs[j] <- sum((one_per$patt == non_matches_comparisons[j]) &
                       one_per$rl_score > cutpoint) /
    sum(one_per$patt == non_matches_comparisons[j])
}
for(l in 1:length(a1_probs)){
  if(is.nan(a1_probs[l])){a1_probs[l] = 0}
}
#False-Negative

m_probs <- c()
for(ii in 1:length(matches_comparisons)) {
  m_probs[ii] <- sum(matching_dataset$patt == matches_comparisons[ii]) /
    nrow(matching_dataset)
}

a3_probs <- c()
for(jj in 1:length(matches_comparisons)) {
  a3_probs[jj] <- sum((one_per$patt == matches_comparisons[jj]) &
                       one_per$rl_score < cutpoint) /
    sum(one_per$patt == matches_comparisons[jj])
}

##Make it a function
fp_fn <- function(cutpoint){
  ##False-positives
  u_probs <- c()
  for(i in 1:length(non_matches_comparisons)) {
    u_probs[i] <- sum(tidy_all_matches_docks_rl$patt == non_matches_comparisons[i]) /
      nrow(tidy_all_matches_docks_rl)
  }
  
  a1_probs <- c()
  for(j in 1:length(non_matches_comparisons)) {
    a1_probs[j] <- sum((one_per$patt == non_matches_comparisons[j]) &
                         one_per$rl_score > cutpoint) /
      sum(one_per$patt == non_matches_comparisons[j])
  }
  for(l in 1:length(a1_probs)){
    if(is.nan(a1_probs[l])){a1_probs[l] = 0}
  }
  fp = sum(u_probs*a1_probs)
  #False-Negative
  
  m_probs <- c()
  for(ii in 1:length(matches_comparisons)) {
    m_probs[ii] <- sum(matching_dataset$patt == matches_comparisons[ii]) /
      nrow(matching_dataset)
  }
  
  a3_probs <- c()
  for(jj in 1:length(matches_comparisons)) {
    a3_probs[jj] <- sum((one_per$patt == matches_comparisons[jj]) &
                          one_per$rl_score < cutpoint) /
      nrow(one_per)
  }
  fn = sum(m_probs*a3_probs)
  return(list(u_probs = u_probs,
              a1_probs = a1_probs,
              m_probs = m_probs,
              a3_probs = a3_probs,
              fp_rate = fp,
              fn_rate = fn))
}

cuts <- seq(10,24,by = 0.5)
fps <- c()
fns <- c()
for(k in 1:length(cuts)){
  fps[k] = fp_fn(cuts[k])$fp_rate
  fns[k] = fp_fn(cuts[k])$fn_rate
}

rates <- tibble(fp_rate = fps,
                fn_rate = fns) %>% 
  mutate(index = cuts)

rates %>% 
  gather(rate,value,fp_rate,fn_rate,-index) %>% 
  ggplot(aes(index,value)) +
  geom_line(aes(color = rate),
            size = 1.2) +
  theme_bw() +
  ylab("Error Rate") +
  xlab("Cutpoint") +
  scale_color_discrete(name = "Rate",
                       labels = c("FN Rate","FP Rate")) +
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))
ggsave("C:/Users/32443181/Box Sync/Research/Dissertation/Thesis/cut_point_error_rates_investigation.jpg",
       height=5, width=8.5, units='in', dpi=600)


rates %>% 
  mutate(sums = fn_rate + fp_rate) %>% 
  gather(rate,value,fp_rate,fn_rate,sums,-index) %>% 
  ggplot(aes(index,value)) +
  geom_line(aes(color = rate),
            size = 1.2) +
  theme_bw() +
  ylab("Error Rate") +
  xlab("Cutpoint") +
  scale_color_discrete(name = "Rate",
                       labels = c("FN Rate","FP Rate","Sum")) +
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20),
        axis.text = element_text(size = 20),
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20))



g_rates <- rates %>% 
  gather(rate,value,fp_rate,fn_rate,-index)

one_per %>% 
  select(rl_score) %>% 
  mutate(index = round(rl_score/0.5)*0.5) %>% 
  count(index) %>% 
  left_join(g_rates) %>% 
  ggplot(aes(rl_score)) + 
  geom_histogram()

# # hold all but one variable constant to get distribution of 
# # linking variables
# 
# tidy_link_status <- tidy_all_matches_docks %>% 
#   mutate(spec_caught_diff = reported_species_kept_cls - 
#            reported_species_claim_mrip) %>% 
#   mutate(spec_rel_diff = reported_species_released_cls - 
#            reported_species_release_mrip) %>%
#   select(nbAnglers, PARTY, 
#          strat_id,INTSITE,
#          state, STATE, total_kept_cls,
#          total_claim_mrip,
#          `RED SNAPPER_claim`,`RED SNAPPER_kept`,
#          date, return_date_ymd,
#          departdate,returndate,
#          date_time_mrip, diff_total_catch,
#          diff_total_release, km_difference,
#          spec_caught_diff,spec_rel_diff,
#          diff_cls_mrip_anglers, date_diff)
# 
# #######
# ## Total Catch
# #######
# 
# tidy_link_status <- tidy_link_status %>%
#     mutate(tot_catch_pattern = case_when(
#     between(diff_total_catch,-0.9999,0.9999) ~ "agree",
#     between(diff_total_catch,-5.9999,-1) |
#       between(diff_total_catch,1,5.9999) ~ "close",
#     between(diff_total_catch,-15.9999,-6) |
#       between(diff_total_catch,6,15.9999) ~ "far",
#     between(diff_total_catch,-25.9999,-16) |
#       between(diff_total_catch,16,25.9999) ~ "farther",
#     between(diff_total_catch,-3000,-26) |
#       between(diff_total_catch,26,9006) ~ "disagree"))
# 
# ####
# # Total Release
# ####
# 
# tidy_link_status <- tidy_link_status %>%
#   mutate(tot_rel_pattern = case_when(
#     between(diff_total_release,-0.9999,0.9999) ~ "agree",
#     between(diff_total_release,-4.9999,-1) | 
#       between(diff_total_release,1,4.9999) ~ "close",
#     between(diff_total_release,-10.9999,-5) |
#       between(diff_total_release,5,10.9999) ~ "far",
#     between(diff_total_release,-90000,-11) |
#       between(diff_total_release,11,90000) ~ "disagree"))
# 
# 
# #####
# # KM Distance
# #####
# 
# tidy_link_status <- tidy_link_status %>%
#   mutate(km_diff_pattern = case_when(
#     between(km_difference,0,15) ~ "agree",
#     between(km_difference,15.0001,40) ~ "close",
#     between(km_difference,40.0001,75) ~ "far",
#     between(km_difference,75.0001,4000) ~ "disagree"))
# 
# 
# ######
# # Number Species Caught Difference
# ######
# 
# tidy_link_status <- tidy_link_status %>%
#   mutate(spec_caught_pattern = case_when(
#     (spec_caught_diff %in% c(0)) ~ "agree",
#     (spec_caught_diff %in% c(-1,1)) ~ "close",
#     (spec_caught_diff %in% c(-2,2)) ~ "far",
#     (spec_caught_diff %in% c(-20:-3,3:20)) ~ "disagree"))
# 
# #####
# # Number Species Released Difference
# #####
# 
# tidy_link_status <- tidy_link_status %>%
#   mutate(spec_rel_pattern = case_when(
#     (spec_rel_diff %in% c(0)) ~ "agree",
#     (spec_rel_diff %in% c(-1,1)) ~ "close",
#     (spec_rel_diff %in% c(-2,2)) ~ "far",
#     (spec_rel_diff %in% c(-10:-3,3:10)) ~ "disagree"))
# 
# #####
# # Number of anglers
# #####
# tidy_link_status <- tidy_link_status %>%
#   mutate(angler_pattern = case_when(
#     (diff_cls_mrip_anglers %in% c(0)) ~ "agree",
#     (diff_cls_mrip_anglers %in% c(-1,1)) ~ "close",
#     (diff_cls_mrip_anglers %in% c(-55:-2,2:55)) ~ "disagree"))
# 
# #####
# # Date
# #####
# 
# tidy_link_status <- tidy_link_status %>%
#   mutate(date_pattern = case_when(
#     between(date_diff,-0.15,0.15) ~ "agree",
#     between(date_diff,-1.5, -0.15001) |
#       between(date_diff,0.15001,1.5) ~ "close",
#     between(date_diff,-5,-1.5001) |
#       between(date_diff, 1.5001, 5) ~ "far",
#     between(date_diff,-15, -5.001) |
#       between(date_diff, 5.001, 15) ~ "farther",
#     between(date_diff,-60000, -15.001) |
#       between(date_diff, 15.001, 60000) ~ "disagree"))
# 
# 
# 
# tidy_link_status %>% 
#   select(ends_with("pattern")) %>% 
#   nrow()
# 
# tidy_link_status %>% 
#   select(ends_with("pattern")) %>% 
#   distinct() %>% 
#   nrow()
# 
# tidy_link_status %>% 
#   select(ends_with("pattern")) %>% 
#   filter_all(all_vars(. %in% c("agree","close")))
# 
# tidy_link_status %>% 
#   filter(state == STATE,
#          nbAnglers == PARTY,
#          date == return_date_ymd) %>% 
#   nrow()
# 
# tidy_link_status %>% 
#   filter(state == STATE,
#          nbAnglers == PARTY,
#          date == return_date_ymd) %>% 
#   select(ends_with("pattern")) %>%
#   distinct() %>% 
#   nrow()
# 
# tidy_link_status %>% 
#   filter(angler_pattern %in% c("agree"),
#          date_pattern %in% c("agree","close"),
#          km_diff_pattern %in% c("agree","close")) %>% 
#   select(ends_with("pattern")) %>%
#   distinct() %>% 
#   nrow()
# 
