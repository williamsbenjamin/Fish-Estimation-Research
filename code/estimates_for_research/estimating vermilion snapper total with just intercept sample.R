#estimating Red Snapper total with just the intercept sample

library(tidyverse)
library(survey)

mrip_16 <- read_csv("data/mrip_tidy2_16.csv",
                    col_types = cols(psu_id = col_character()))
mrip_17 <- read_csv("data/mrip_tidy2_17.csv",
                    col_types = cols(psu_id = col_character()))
mrip_16 <- mrip_16 %>%
  filter(ST != 28)
mrip_17 <- mrip_17 %>%
  filter(ST != 28)

is.zero.na <- function(x){
  x == 0 | is.na(x)
}
mrip_16 <- mrip_16 %>%
  bind_cols(
    select(mrip_16, ends_with("claim")) %>%
      transmute(
        total_claim_mrip = rowSums(., na.rm = T),
        reported_species_claim_mrip = rowSums(!is.zero.na(.))
      )
  ) %>%
  bind_cols(
    select(mrip_16, ends_with("release")) %>%
      transmute(
        total_release_mrip = rowSums(., na.rm = T),
        reported_species_release_mrip = rowSums(!is.zero.na(.))
      )
  )

mrip_17 <- mrip_17 %>%
  bind_cols(
    select(mrip_17, ends_with("claim")) %>%
      transmute(
        total_claim_mrip = rowSums(., na.rm = T),
        reported_species_claim_mrip = rowSums(!is.zero.na(.))
      )
  ) %>%
  bind_cols(
    select(mrip_17, ends_with("release")) %>%
      transmute(
        total_release_mrip = rowSums(., na.rm = T),
        reported_species_release_mrip = rowSums(!is.zero.na(.))
      )
  )


mrip16_surv <- svydesign(~psu_id,
                         weights=~wp_int,
                         strata = ~strat_id,
                         nest=T,
                         data=mrip_16)

options(survey.lonely.psu = "adjust") 

ty2_16 <- svytotal(~`VERMILION SNAPPER_claim`,
                    design=mrip16_surv)
#ty2_16

mrip17_surv <- svydesign( ~ psu_id,
                         weights = ~wp_int,
                         strata = ~strat_id,
                         nest=T,
                         data=mrip_17)

options(survey.lonely.psu = "adjust") 

ty2_17 <- svytotal(~`VERMILION SNAPPER_claim`,
                   design=mrip17_surv)
#ty2_17







