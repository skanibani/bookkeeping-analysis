library(tidyverse)

import_coupling <- read_csv("data/output_coupling.csv")
import_complexity <- read_csv("data/output_wmc.csv")
import_lcom <- read_csv("data/output_lcom.csv")

set_combined <- import_coupling %>% 
  left_join(import_complexity) %>% 
  filter(!str_detect(module, "migration"),
         !str_detect(module, "seeder")) %>% 
  mutate(wmc = ifelse(is.na(wmc), 0, wmc)) %>% 
  left_join(import_lcom)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}