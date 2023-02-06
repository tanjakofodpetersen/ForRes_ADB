######################################
###     PLOTS TIL HANNE HEGRE     ####
######################################

library(tidyverse)
library(ggplot2)
library(data.table)
library(ggsankey)

# Last opp fil
aronia <- read.csv2('Aronia_HH.csv')

# Plot
aronia %>%
  rename('Foer revisjon' = 'Foer_revisjon' ,
         'Etter revisjon' = 'Etter_revisjon' ) %>%
  make_long(`Foer revisjon`, `Etter revisjon`) %>%
                  ggplot(., aes(x = x, 
                                next_x = next_x, 
                                node = node, 
                                next_node = next_node,
                                fill = node,
                                label = node)) +
                    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
                    geom_sankey_label(size = 2, color = 1, fill = "white") +
                    scale_fill_manual(values = c("Aronia arbutifolia"="#d7191c",  #lightcoral
                                                 "Aronia melanocarpa"="#abd9e9",  #steelblue3
                                                 "Aronia xprunifolia"="#fdae61",   #khaki1
                                                 "Aronia sp."="gray60",
                                                 "Aronia eller Sorbaronia"="gray60",
                                                 "Sorbaronia mitschurinii"="#2c7bb6"),  #olivedrab4
                                      name = "") +
                    labs(x = "") +
                    theme_sankey(base_size = 10) +
                    theme(legend.position="none",
                          panel.background = element_rect(fill='transparent', color = NA),
                          plot.background = element_rect(fill='transparent', color=NA))
# Mangler verdier                