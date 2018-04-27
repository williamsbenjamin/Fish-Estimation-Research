library(tidyverse)

cls <- read_csv("2018_02_01_cls_tidy.csv")
mrip <- read_csv("2018_02_01_mrip_tidy.csv")
mrip16 <- read_csv("2018_02_07_mrip_tidy_16.csv")


cls_in_mrip<-unique(c(
unique(mrip$CLS_ID),
unique(mrip16$CLS_ID)
)
)

cls %>%
  filter(
    CLS_ID %in% cls_in_mrip
  ) %>%
  nrow()


bind_rows(mrip,mrip16) %>%
  filter(
    CLS_ID %in% unique(cls$CLS_ID)
  ) %>%
  drop_na(CLS_ID) %>%
  nrow()

