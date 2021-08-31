library(tidyverse)
library(ggpubr)

import_lcom <- read_csv("data/lcom.csv") %>% 
  select(filePath, message) %>% 
  rename(file = filePath)

set_lcom <- import_lcom %>% 
  mutate(file = str_extract(file, "/lib.*.js"),
         message = str_extract(message, "(?<=but was )[:digit:]")) %>%
  rename(module = file,
         lcom = message) %>% 
  arrange(desc(lcom), module)

ggdotchart(set_lcom,
           "module",
           "lcom",
           rotate = TRUE)

write_csv(set_lcom, "output_lcom.csv")
