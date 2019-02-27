#Starting with Red Snapper
library(tidyverse)
library(ggthemes)
source("code/source_code/mediating_datasets_for_making_data_sets_with_cutpoints.R")
#use all sites for now
cls_17 <- read_csv("data/cls_17_all_sites.csv")
cuts <- seq(50, 1850, by = 50)/100
cut_tibble <- tibble(
  cutoff = cuts,
  PSE = rep(-10,length(cuts)),
  ty2_est = rep(-10,length(cuts))
)

for(i in 1:length(cuts)){
  cutpoint <- cuts[i]
  source("code/estimates_for_research/Cutpoint Investigation/cutpoint_estimates_2017_vermilion_snapper.R")
  cut_tibble$PSE[i] <- ty2_pse
  cut_tibble$ty2_est[i] <- ty2_17_vs_total_a 
}

#source("code/estimates_for_research/estimating red snapper total with just intercept sample.R")
ty2_17 <- 697914
se_ty2_17 <- ty2_17*0.1498

cut_tibble_full <- cut_tibble %>%
  mutate(
    se = PSE * ty2_est,
    bias = ty2_17 - ty2_est,
    rel_bias = (ty2_est - ty2_17) / ty2_17,
    p_mse = se ^ 2 + bias ^ 2
  )

#pseudo-mse
cut_tibble_full %>% 
  arrange(p_mse) %>% 
  slice(1) %>% 
  pull(cutoff) #minimized at cutpoint of 13

cut_tibble_full %>% 
  ggplot() +
  geom_col(aes(cutoff, p_mse)) +
  geom_vline(xintercept = 13, color = "red")

#Bias
cut_tibble_full %>% 
  ggplot() +
  geom_col(aes(cutoff, bias))

#PSE
cut_tibble_full %>% 
  ggplot() +
  geom_col(aes(cutoff, PSE))

#REL Bias
cut_tibble_full %>% 
  ggplot() +
  geom_col(aes(cutoff, rel_bias))

#mess around
matches <- c()
for(i in 1:length(cuts)){
  matches[i] <- nrow(
    filter(
      tidy_all_matches_one_per,
      rl_score > cuts[i]
    )
  )
}

cut_tibble_full_m <- cut_tibble_full %>% 
  mutate(match_num = matches)

cut_gather <- cut_tibble_full_m %>% 
  gather(`PSE`,`ty2_est`,`se`,`bias`,`rel_bias`,
         `p_mse`,
         key = "Stat", value = "value")

stat_names<- c(
  "bias" = "BIAS",
  "se" = "STD ERROR",
  "p_mse" = "PSEUDO-MSE",
  "rel_bias" = "RELATIVE BIAS",
  "match_num" = "MATCHES"
)


cut_gather %>% 
  # filter(Stat != "ty2_est") %>% 
  ggplot(aes(cutoff,value)) +
  geom_col() +
  geom_vline(xintercept = 13,
             color = "red", 
             size = 1.2) +
  facet_wrap(~Stat,scales = "free") +
  theme_bw()

cut_gather %>%
  filter(Stat == "bias") %>% 
  ggplot(aes(cutoff,match_num)) + 
  geom_col() +
  theme_bw() +
  xlab("Cut-point") +
  ylab("Number of Matches") + 
  theme(axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 15),
        plot.title = element_text(hjust = 0.5,
                                  size = 30),
        strip.text = element_text(size = 20)) +
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350,400))

cut_gather %>% 
  filter(Stat == "se") %>% 
  ggplot(aes(reorder(as.character(match_num),cutoff),value)) +
  geom_col() +
  #facet_wrap(~Stat,scales = "free_y",nrow = 2) +
  scale_x_discrete(breaks = as.character(matches)) +
  xlab("Number of Matches") + 
  ylab(NULL) +
  theme_bw() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  size = 30),
        axis.title.x = element_text(size = 25)) +
  ggtitle("Standard Error vs Number of Matches
          Red Snapper - 2017")

cut_gather %>%
  filter(Stat == "bias" | 
           Stat == "se" |
           Stat == "p_mse") %>% 
  ggplot(aes(cutoff,value)) + 
  geom_col() +
  geom_vline(xintercept = 13,
             color = "red", 
             size = 2) +
  facet_wrap(~Stat, nrow = 3,
             strip.position = "right",
             scales = "free_y",
             labeller = as_labeller(stat_names)) + 
  theme_bw() +
  xlab("Cut-point") +
  ylab(NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 25),
        plot.title = element_text(hjust = 0.5,
                                  size = 30),
        strip.text = element_text(size = 20)) 

cut_gather %>%
  filter(Stat == "bias") %>% 
  ggplot(aes(cutoff,value)) + 
  geom_col() +
  geom_vline(xintercept = 13,
             color = "red", 
             size = 2) +
  theme_bw() +
  xlab("Cut-point") +
  #ylab(NULL) +
  theme(#axis.text.y = element_blank(),
    #axis.ticks.y = element_blank(),
    axis.title.x = element_text(size = 20),
    axis.text.x = element_text(size = 15))

cut_gather %>%
  filter(Stat == "p_mse") %>% 
  ggplot(aes(cutoff,value)) + 
  geom_col() +
  theme_bw() +
  xlab(NULL) +
  ylab(NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 20))
ggsave("C:/Users/32443181/Box Sync/Research/Dissertation/Thesis/p_mse_no_cut.jpg")
cut_gather %>%
  filter(Stat == "p_mse") %>% 
  ggplot(aes(cutoff,value)) + 
  geom_col() +
  geom_vline(xintercept = 13.5,
             color = "red", 
             size = 2) +
  theme_bw() +
  xlab("Cut-point") +
  ylab(NULL) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 15))

cut_gather %>% 
  filter(Stat == "ty2_est") %>% 
  ggplot(aes(cutoff,value)) +
  geom_hline(yintercept = 144000,
             color = "blue",
             size = 1) +
  geom_hline(yintercept = 315000,
             color = "blue",
             size = 1) +
  geom_col() +
  geom_vline(xintercept = 14.5,
             color = "red", 
             size = 2) +
  theme_bw() +
  ylab("Estimate") + 
  xlab("Cut-point") + 
  ggtitle(NULL) +
  theme(axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        axis.text.x = element_text(size = 15))


##intelligent bias
c <- -((cut_tibble_full$ty2_est - ty2_17)^2) - 
  (se_ty2_17^2) - ((cut_tibble_full$se)^2)


((-(2*(ty2_17^2) - 2)) + sqrt( ((2*(ty2_17^2)) - 2)^2 - 4*c)) /2
tibble(cuts = cuts,
       new_bias = ((-(2*(ty2_17^2) - 2)) + 
                     sqrt( ((2*(ty2_17^2)) - 2)^2 - 4*c)) /2
)
tibble(cuts = cuts,
       new_bias = ((-(2*(ty2_17^2) - 2)) + 
                     sqrt( ((2*(ty2_17^2)) - 2)^2 - 4*c)) /2
) %>% 
  arrange(new_bias)
tibble(cuts = cuts,
       new_bias = ((-(2*(ty2_17^2) - 2)) + 
                     sqrt( ((2*(ty2_17^2)) - 2)^2 - 4*c)) /2
) %>% 
  ggplot(aes(cuts,new_bias)) + 
  geom_col()

((-(2*(ty2_17^2) - 2)) - sqrt( ((2*(ty2_17^2)) - 2)^2 - 4*c)) /2
plot(bias_f(c(100:10000)))
cut_tibble_full
