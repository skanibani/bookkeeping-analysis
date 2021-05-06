library(tidyverse)
library(R.utils)

func_construct_absolute_path <- function(module_path, relative_path) {
  if (str_detect(relative_path, "/[:alnum:]*.js$")) {
    getAbsolutePath(relative_path, str_remove(module_path, "/[:alnum:]*.js$"))
  } else if (str_detect(relative_path, "^(\\./)")) {
    getAbsolutePath(paste0(relative_path, ".js"),
                    str_remove(module_path, "/[:alnum:]*.js$"))
  } else if (
    str_detect(str_extract(relative_path, "([a-zA-Z])*$"), "[A-Z]")
  ) {
    getAbsolutePath(paste0(relative_path, ".js"), str_remove(module_path, "/[:alnum:]*.js$"))
  }else if (str_detect(relative_path, "^(\\.\\.)")) {
    getAbsolutePath(paste0(relative_path, "/index.js"), str_remove(module_path, "/[:alnum:]*.js$"))
  }
}

func_add_index_js <- function (wrong_path) {
  str_replace(wrong_path, ".js$", "/index.js")
}

calc_instability <- function (efferent_coupling, afferent_coupling) {
  if (!is.na(efferent_coupling) && !is.na(afferent_coupling)) {
    efferent_coupling / (efferent_coupling + afferent_coupling)
  }
}

theme_bibani <- theme(axis.text.x = element_text(family = "Nimbus Sans", size = 17, face = "plain", color = "black"),
                      axis.text.y = element_text(family = "Nimbus Sans", size = 17, face = "plain", color = "black"),
                      axis.title.x = element_text(family = "Nimbus Sans", size = 17, face = "plain", color = "black"),
                      axis.title.y = element_text(family = "Nimbus Sans", size = 17, face = "plain", color = "black"),
                      plot.title = element_text(family = "Nimbus Sans", size = 22, face = "plain", color = "black"),
                      plot.subtitle = element_text(family = "Nimbus Sans", size = 20, face = "italic", color = "black"))

import_coupling_outgoing <- read_csv("data/coupling.csv") %>%
  select(`filePath`, `message`) %>%
  rename(module = filePath) %>%
  filter(str_detect(module, "/lib.*.js")) %>%
  filter(!str_detect(module, "SmartEditor")) %>%
  mutate(module = str_extract(module, "/lib.*.js"))

import_wrong_paths <- read_csv("data/wrong_paths.csv")
set_wrong_paths <- as.vector(import_wrong_paths$wrong_path)

set_coupling_outgoing <- import_coupling_outgoing %>%
  mutate(new_path = map2(module, message, func_construct_absolute_path)) %>%
  group_by(module) %>%
  mutate(efferent_coupling = n()) %>%
  ungroup() %>%
  distinct(module, .keep_all = TRUE) %>%
  mutate(module = as.factor(module),
         efferent_coupling = as.integer(efferent_coupling)) %>%
  select(module, efferent_coupling)

set_coupling_incoming <- import_coupling_outgoing %>%
  mutate(new_path = map2(module, message, func_construct_absolute_path)) %>%
  rename(incoming_connections = new_path) %>%
  select(incoming_connections) %>%
  drop_na() %>%
  mutate(incoming_connections = as.factor(as.character(incoming_connections))) %>%
  group_by(incoming_connections) %>%
  summarise(afferent_coupling = n()) %>%
  ungroup() %>%
  mutate(incoming_connections = as.character(incoming_connections)) %>%
  mutate(incoming_connections = ifelse(incoming_connections %in% set_wrong_paths,
                                       map(incoming_connections, func_add_index_js),
                                       incoming_connections)) %>%
  mutate(incoming_connections = as.factor(as.character(incoming_connections))) %>%
  group_by(incoming_connections) %>%
  mutate(afferent_coupling = sum(afferent_coupling)) %>%
  distinct(afferent_coupling, .keep_all = TRUE)

wrong_paths_in_outgoing <- set_coupling_outgoing %>%
  inner_join(import_wrong_paths, by = c("module" = "wrong_path"))

# Wrong pathts are only present in the incoming, afferent coupling.
wrong_paths_in_incoming <- set_coupling_incoming %>%
  inner_join(import_wrong_paths, by = c("incoming_connections" = "wrong_path"))

# Combining outgoing and incoming coupling.
set_coupling <- set_coupling_outgoing %>%
  left_join(set_coupling_incoming, by = c("module" = "incoming_connections")) %>%
  mutate(instability = calc_instability(efferent_coupling, afferent_coupling)) %>%
  arrange(desc(module)) %>%
  drop_na()

set_unstable_modules <- set_coupling %>%
  arrange(desc(instability))

# Export
write_csv(set_coupling, "data/set_coupling.csv")

# Important note, because of multiple ways of noting filepaths, left over paths have to be added to their total amount
# of afferent coupling.

# Analysis
set_unstable_modules %>%
  ggplot(aes(instability)) +
  geom_bar(fill = "white", color = "black") +
  ggtitle("Distribution of module instability", "Instability metric R.C. Martin & Constatine") +
  xlab("Module instability") +
  ylab("Count") +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  theme_bw() +
  theme_bibani