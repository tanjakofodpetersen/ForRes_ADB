######################################
###     PLOTS TIL HANNE HEGRE     ####
######################################

library(tidyverse)
library(ggplot2)
library(data.table)
library(ggsankey)

# Last opp fil
aronia <- read.csv2('Aronia_HH.csv')

## Uten verdier

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
                    geom_sankey_label(size = 3, color = 1, fill = "white") +
                    scale_fill_manual(values = c("Aronia arbutifolia"="#d7191c",  #lightcoral
                                                 "Aronia melanocarpa"="#abd9e9",  #steelblue3
                                                 "Aronia xprunifolia"="#fdae61",   #khaki1
                                                 "Aronia sp."="gray60",
                                                 "Aronia eller Sorbaronia"="gray60",
                                                 "Sorbaronia mitschurinii"="#2c7bb6"),  #olivedrab4
                                      name = "") +
                    labs(x = "") +
                    theme_sankey(base_size = 13) +
                    theme(legend.position="none",
                          panel.background = element_rect(fill='transparent', color = NA),
                          plot.background = element_rect(fill='transparent', color=NA))

##--------------------------------------------------------------

# Med verdier ; disse gjøres best manuelt!
{
  # Step 1
  Sankey1 <- aronia %>%
    rename('Foer revisjon' = 'Foer_revisjon' ,
           'Etter revisjon' = 'Etter_revisjon' ) %>%
    make_long(`Foer revisjon`, `Etter revisjon`)
  
  # Step 2
  Sankey2 <- Sankey1%>%
    dplyr::group_by(node)%>%
    tally()
  
  # Step 3
  Sankey3 <- merge(Sankey1, Sankey2, by.x = 'node', by.y = 'node', all.x = TRUE)
  # Juster verdier til det rette jfr tabell fra Hanne
  Sankey3$n[Sankey3$node == 'Aronia melanocarpa' & Sankey3$x == 'Foer revisjon'] <- '171'
  Sankey3$n[Sankey3$node == 'Aronia melanocarpa' & Sankey3$x == 'Etter revisjon'] <- '8'
  
  # Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
  ggplot(Sankey3, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = node,
                      label = paste0(node,",\nn=", n) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(rep(.78,3), rep(2.1,179), rep(2.1,8), rep(.78,171), rep(.78,10), rep(.78,73), rep(2.1,70))),
                      size = 3, color = 1, fill = "white") +
    scale_fill_manual(values = c("Aronia arbutifolia"="#d7191c",  #lightcoral
                                 "Aronia melanocarpa"="#abd9e9",  #steelblue3
                                 "Aronia xprunifolia"="#fdae61",   #khaki1
                                 "Aronia sp."="gray60",
                                 "Aronia eller Sorbaronia"="gray60",
                                 "Sorbaronia mitschurinii"="#2c7bb6"),  #olivedrab4
                      name = "") +
    labs(x = "") +
    theme_sankey(base_size = 13) +
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA))
}