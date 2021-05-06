library(tidyverse)
library(ggpubr)

# Problem ----
# What is the cyclomatic complexity of all node modules?

# Plan ----
# 1. Output complexity reports form ESLint.
# 2. Normalize ESLint JSON report summary.
# 3. Import normalized report summary.
# 4. Filter complexity value with regex.

# Data ----
import_complexity <- read_csv("data/complexity.csv") %>%
  select(`filePath`, `message`) %>%
  rename(file = filePath)

set_complexity <- import_complexity %>%
  mutate(file = str_extract(file, "/lib.*.js"),
         message = str_extract(message, "[:digit:]")) %>%
  rename(complexity = message) %>%
  filter(!str_detect(file, "SmartEditor")) %>%
  mutate(file = as.factor(file),
         complexity = as.numeric(complexity))

set_wmc <- set_complexity %>%
  rename(module = file) %>%
  group_by(module) %>%
  summarise(wmc = sum(complexity))

# Analysis ----
set_wmc %>%
  ggplot(aes(wmc)) +
  geom_bar(fill = "white", color = "black") +
  ggtitle("Distribution of module complexity", "WMC metric Chidamber & Keremer 1996") +
  scale_x_continuous(limits = c(0, 30), breaks = set_wmc$wmc) +
  scale_y_continuous(limits = c(0, 55), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)) +
  xlab("Weighted Methods per Class") +
  ylab("Count") +
  theme_classic() +
  theme(axis.text.x = element_text(family = "Nimbus Sans", size = 17, face = "plain", color = "black"),
        axis.text.y = element_text(family = "Nimbus Sans", size = 17, face = "plain", color = "black"),
        axis.title.x = element_text(family = "Nimbus Sans", size = 17, face = "plain", color = "black"),
        axis.title.y = element_text(family = "Nimbus Sans", size = 17, face = "plain", color = "black"),
        plot.title = element_text(family = "Nimbus Sans", size = 22, face = "plain", color = "black"),
        plot.subtitle = element_text(family = "Nimbus Sans", size = 20, face = "italic", color = "black"))

ggsave("/home/skander/Desktop/wmc.png",
       plot = last_plot(),
       scale = 1, width = 50, height = 50, units = "in", dpi = 100)



import_coupling <- read_csv("../coupling/data/set_coupling.csv")

foo <- set_wmc %>%
  inner_join(import_coupling)

bar <- cor(foo[2:5])

