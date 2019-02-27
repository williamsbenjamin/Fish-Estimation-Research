mrip_all_17_1 <- read_csv("data/mrip_all_17_cutoff13_all_sites.csv")


mrip_subset <- mrip_all_17_1 %>% 
  filter(reported == 1)

cor(mrip_subset$`RED SNAPPER_claim`,
    mrip_subset$`RED SNAPPER_kept`,
    use = "complete.obs")

cor(mrip_subset$`WHITE GRUNT_claim`,
             mrip_subset$`WHITE GRUNT_kept`,
             use = "complete.obs")

sum(!is.na(mrip_subset$`WHITE GRUNT_kept`))

sum(!is.na(mrip_subset$`RED PORGY_kept`))

sum(mrip_subset$`WHITE GRUNT_kept`,na.rm = T)
sum(mrip_subset$`RED PORGY_kept`,na.rm = T)

mrip_subset %>% 
  summarise(
    cor.rs = cor(`RED SNAPPER_claim`,
                 `RED SNAPPER_kept`,
                 use = "complete.obs"),
    cor.vs = cor(`VERMILION SNAPPER_claim`,
                 `VERMILION SNAPPER_kept`,
                 use = "complete.obs"),
    cor.sm = cor(`SPANISH MACKEREL_claim`,
                 `SPANISH MACKEREL_kept`,
                 use = "complete.obs"),
    cor.wg = cor(`WHITE GRUNT_claim`,
                 `WHITE GRUNT_kept`,
                 use = "complete.obs"),
    cor.rp = cor(`RED PORGY_claim`,
                 `RED PORGY_kept`,
                 use = "complete.obs")
  )
