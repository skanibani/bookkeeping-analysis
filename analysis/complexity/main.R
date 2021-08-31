library(tidyverse)
library(ggpubr)

theme_bibani <- theme(axis.text.x = element_text(family = "Tex Gyre Heros", size = 10, face = "plain", color = "black"),
                        axis.text.y = element_text(family = "Tex Gyre Heros", size = 10, face = "plain", color = "black"),
                        axis.title.x = element_text(family = "Tex Gyre Heros", size = 10, face = "plain", color = "black"),
                        axis.title.y = element_text(family = "Tex Gyre Heros", size = 10, face = "plain", color = "black"),
                        plot.title = element_text(family = "Tex Gyre Heros", size = 10, face = "plain", color = "black"),
                        plot.subtitle = element_text(family = "Tex Gyre Heros", size = 10, face = "italic", color = "black"))

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
  ggtitle("Distribution of module complexity in bookkeeping system", "WMC metric") +
  scale_x_continuous(limits = c(0, 26), breaks = set_wmc$wmc) +
  scale_y_continuous(limits = c(0, 55), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)) +
  xlab("Weighted Methods per Class") +
  ylab("Count") +
  theme_bw() +
  theme_bibani


plot_1 <- set_wmc %>% 
  ggplot(aes(wmc)) +
  geom_histogram(binwidth = 1, fill = "black") +
  scale_x_continuous(limits = c(0, 161), breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, 140, 150, 160)) +
  scale_y_continuous(limits = c(0, 55), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)) +
  theme_bw() +
  theme_bibani +
  xlab("WMC metric value") +
  ylab("Count")

plot_1

plot_2 <- set_wmc %>% 
  ggplot(aes(wmc)) +
  geom_boxplot(color = "black") +
  scale_x_continuous(limits = c(0, 161), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100, 105, 110, 115, 120, 125, 130, 135, 140, 145, 150, 155, 160)) +
  theme_bw() +
  theme_bibani +
  theme(axis.title.x = element_blank(), axis.ticks.x = element_blank(),axis.text.x = element_blank()) +
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(),axis.text.y = element_blank(),
        plot.margin = margin(0.1, 0.2, 0.1, 1, "cm")) +
  ggtitle("Distribution of WMC metric values of all modules")

plot_2

ggarrange(plot_2, plot_1, ncol = 1, nrow = 2, labels = c("a)", "b)"), hjust = 2)

ggsave("/home/skander/Desktop/summary-plot-wmc.png",
       plot = last_plot(),
       scale = 1, width = 15.92, height = 11.94, units = "cm", dpi = 600)

plot_3 <-set_wmc %>% 
  ggplot(aes(wmc)) +
  geom_bar(fill = "white", color = "black") +
  scale_x_continuous(limits = c(0, 26), breaks = set_wmc$wmc) +
  scale_y_continuous(limits = c(0, 55), breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55)) +
  ggtitle("Distribution of module complexity") +
  xlab("MWC metric value") +
  ylab("Count") +
  theme_bw() +
  theme_bibani

plot_3


ggsave("/home/skander/Desktop/bar-complexity.png",
       plot = last_plot(),
       scale = 1, width = 15.92, height = 11.94, units = "cm", dpi = 600)



set_wmc_outliers <- set_wmc %>% 
  filter(wmc >= 20) %>% 
  arrange(desc(wmc))


plot_4 <- set_wmc_outliers %>% 
  ggdotchart(x = "module",
             y = "wmc",
             sorting = "descending",
             add = "segments",
             rotate = TRUE,
             dot.size = 6,
             label = set_wmc_outliers$wmc,
             font.label = list(color = "white", size = 9, vjust = 0.5),
             ggtheme = theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                         panel.background = element_blank()) + theme_bibani)

ggpar(plot_4,
      main = "Outlier modules with high WMC values",
      xlab = "Module",
      ylab = "WMC metric value")

ggsave("/home/skander/Desktop/dotchart-wmc.png",
       plot = last_plot(),
       scale = 1, width = 15.92, height = 11.94, units = "cm", dpi = 600)
