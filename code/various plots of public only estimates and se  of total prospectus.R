#plots of estimates and standard errors for public docks only
#With 3 matching procedures
library(tidyverse)

est_df <- tibble(Year = c(2016,2016,2016,
                          2017,2017,2017),
                 Method = c("Naive","All Links","Score > 4",
                            "Naive","All Links","Score > 4"),
                 t_yp = c(513917.8,759083.8,1038751,
                          510379,629369.6,851366.9),
                 se_t_yp = c(72649.59,86495.37,136579.9,
                             117068.1,91261.79,139327),
                 t_yc = c(209766.6,1158165,1444072,
                          124794.6,559709.1,674537.7),
                 se_t_yc = c(42583.97,189627.3,264127.3,
                             13989.25,103460.6,146828.6),
                 t_y2 = c(200730.7,817137.7,1086039,
                          127488.1,615210,821542.3),
                 se_t_y2 = c(33005.14,86819.84,136304.6,
                             17190.51,92452.01,143442.7),
                 t_ydiff = c(303272,4280062,4423828,
                             161274.4,4016836,4191040),
                 se_tydiff = c(147476,419082,425842,
                               57737,467494,482399),
                 t_ynew = c(1631437,3982547,4274074,
                            1343433,3883508,4149248),
                 se_t_ynew = c(203559,302208,346501,
                               361803,464786,483072))
estimates_only <- est_df %>% 
  select(-se_t_yp,-se_t_yc,-se_t_y2,-se_tydiff,-se_t_ynew) %>% 
  gather(t_yp,t_yc,t_y2,t_ydiff,t_ynew,key="Estimator",
         value="Estimate")

se_only <- est_df %>% 
  select(-t_yp,-t_yc,-t_y2,-t_ydiff,-t_ynew) %>% 
  gather(se_t_yp,se_t_yc,se_t_y2,se_tydiff,se_t_ynew,
         key="Estimator",value="SE_Estimate") %>% 
  mutate(Estimator = c(rep("t_yp",6),rep("t_yc",6),rep("t_y2",6),
                         rep("t_ydiff",6),rep("t_ynew",6)))

estimates_plus_se <- inner_join(estimates_only,
                                se_only,
                                by=c("Year","Estimator","Method"))



estimates_plus_se %>% 
  ggplot(aes(factor(Estimator),Estimate,fill = Method)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~Year,nrow=2) +
  geom_errorbar(aes(ymin=Estimate-SE_Estimate,
                    ymax=Estimate+SE_Estimate),
                width=0.2,
                position = position_dodge(.9))


##Unbiased Estimate of total from 2016 and 2017 
#(using only intercept data for public sites)

unbiased_2016 <- 4820731
unbiased_2017 <- 4905753

estimates_plus_se_bias <- estimates_plus_se %>% 
  mutate(Bias = if_else(Year==2016,
                        unbiased_2016 - Estimate,
                        unbiased_2017 - Estimate),
         Relative_Bias = if_else(Year==2016,
                                 (unbiased_2016 - Estimate)/unbiased_2016,
                                 (unbiased_2017 - Estimate)/unbiased_2017),
         MSE = Bias^2 + SE_Estimate^2)

estimates_plus_se_bias %>% 
  ggplot(aes(Estimator,Bias,fill = Method)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~Year,nrow=2)

estimates_plus_se_bias %>% 
  ggplot(aes(factor(Estimator),Relative_Bias,fill = Method)) +
  geom_col(position = position_dodge()) +
  facet_wrap(~Year,nrow=2) +
  scale_x_discrete(limits = c("t_yp","t_yc","t_y2",
                              "t_ydiff","t_ynew"),
                   labels=expression(t[yp],t[yc],t[y2],
                                     t[ydiff],t[ynew])) +
  ylab("Relative Bias") +
  xlab(NULL) +
  theme(legend.text = element_text(size = 16, face = "bold"),
        legend.title = element_blank(),
        legend.position = "top") 
ggsave("plots/RB_prospectus.jpg")
ggsave("C:/Users/32443181/Box Sync/Research/Dissertation/Prospectus/RB_prospectus.jpg",
        height=5, width=8.5, units='in', dpi=600)


estimates_plus_se_bias %>%
  ggplot(aes(Estimator, MSE, fill = Method)) +
  geom_col(position = position_dodge()) +
  facet_wrap( ~ Year, nrow = 2) +
  scale_x_discrete(
    limits = c("t_yp", "t_yc", "t_y2",
               "t_ydiff", "t_ynew"),
    labels = expression(t[yp], t[yc], t[y2],
                        t[ydiff], t[ynew])) +
  ylab("Pseudo - MSE") +
  xlab(NULL) +
  theme(
    legend.text = element_text(size = 16, face = "bold"),
    legend.title = element_blank(),
    legend.position = "top")

ggsave("plots/Pseudo_MSE_prospectus.jpg")
ggsave("C:/Users/32443181/Box Sync/Research/Dissertation/Prospectus/Pseudo_MSE_prospectus.jpg",
       height=5, width=8.5, units='in', dpi=600)
