######################################
###     PLOTS TIL HANNE HEGRE     ####
######################################

library(tidyverse)
library(ggplot2)
library(data.table)
library(ggsankey)

# Last opp fil
aronia <- read.csv2('Aronia_HH.csv')

# Litt rydding; erstatt 'x' i et artsnavn med det faktiske hybrid-tegn
aronia[aronia$Foer_revisjon == 'Aronia xprunifolia', "Foer_revisjon"] <- 'Aronia \U00D7prunifolia'

## Uten verdier

# Plot
aronia %>%
  rename('F\U00F8r revisjon' = 'Foer_revisjon' ,
         'Etter revisjon' = 'Etter_revisjon' ) %>%
  make_long('F\U00F8r revisjon', 'Etter revisjon') %>%
                  ggplot(., aes(x = x, 
                                next_x = next_x, 
                                node = node, 
                                next_node = next_node,
                                fill = node,
                                label = node)) +
                    geom_sankey(flow.alpha = 0.75, node.color = 0.9, width = .3) +
                    geom_sankey_label(size = 5, color = 1, fill = "white") +
                    scale_fill_manual(values = c("Aronia arbutifolia"="#d7191c",  #lightcoral
                                                 "Aronia melanocarpa"="#abd9e9",  #steelblue3
                                                 "Aronia \U00D7prunifolia"="#fdae61",   #khaki1
                                                 "Aronia sp."="gray60",
                                                 "Aronia eller Sorbaronia"="gray60",
                                                 "Sorbaronia mitschurinii"="#2c7bb6"),  #olivedrab4
                                      name = "") +
                    labs(x = "") +
                    theme_sankey(base_size = 15) +
                    theme(legend.position="none",
                          panel.background = element_rect(fill='transparent', color = NA),
                          plot.background = element_rect(fill='transparent', color=NA),
                          axis.text.x = element_text(size = 15),
                          plot.margin = unit(c(0,-5,0,-5), 'cm')) +
  coord_cartesian(clip = 'off')


##--------------------------------------------------------------


# Med verdier ; disse gjøres best manuelt!

# Klar gjør filer
{
  # Step 1
  Sankey1 <- aronia %>%
    rename('F\U00F8r revisjon' = 'Foer_revisjon' ,
           'Etter revisjon' = 'Etter_revisjon' ) %>%
    make_long('F\U00F8r revisjon', 'Etter revisjon')
  
  # Step 2
  Sankey2 <- Sankey1%>%
    dplyr::group_by(node)%>%
    tally()
  
  # Step 3
  Sankey3 <- merge(Sankey1, Sankey2, by.x = 'node', by.y = 'node', all.x = TRUE)
  # Juster verdier til det rette jfr tabell fra Hanne
  Sankey3$n[Sankey3$node == 'Aronia melanocarpa' & Sankey3$x == 'Foer revisjon'] <- '171'
  Sankey3$n[Sankey3$node == 'Aronia melanocarpa' & Sankey3$x == 'Etter revisjon'] <- '8'
  # Sorter i rekkefølge aht. labelling
  Sankey3 <- Sankey3 %>%
    arrange(factor(node, levels = c('Aronia arbutifolia','Aronia \U00D7prunifolia','Aronia melanocarpa','Sorbaronia mitschurinii','Aronia sp.','Aronia eller Sorbaronia')),
            factor(next_node, levels = c('Aronia arbutifolia','Aronia \U00D7prunifolia','Aronia melanocarpa','Sorbaronia mitschurinii','Aronia sp.','Aronia eller Sorbaronia')))
}

# Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
### Fargeblind-vennlig
ggplot(Sankey3, aes(x = x, 
                    next_x = next_x, 
                    node = node, 
                    next_node = next_node,
                    fill = node,
                    label = paste0(node,", ", n) )) +
  geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
  geom_sankey_label(aes(x = c(rep(.78,3), rep(.78,73), rep(.78,171), rep(2.25,8), rep(2.25,70), rep(.78,10), rep(2.25,179))),
                    size = 5, color = 1, fill = "white", width = .3) +
  scale_fill_manual(values = c("Aronia arbutifolia"="#d7191c",  #lightcoral
                               "Aronia melanocarpa"="#abd9e9",  #steelblue3
                               "Aronia \U00D7prunifolia"="#fdae61",   #khaki1
                               "Aronia sp."="gray60",
                               "Aronia eller Sorbaronia"="gray60",
                               "Sorbaronia mitschurinii"="#2c7bb6"),  #olivedrab4
                    name = "") +
  labs(x = "") +
  theme_sankey(base_size = 15) +
  theme(legend.position="none",
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.text.x = element_text(size = 15)) +
  coord_cartesian(clip = 'off')

ggsave('Aronia_v1.png', bg='transparent')

### Hannes farger
ggplot(Sankey3, aes(x = x, 
                    next_x = next_x, 
                    node = node, 
                    next_node = next_node,
                    fill = node,
                    label = paste0(node,",\nn=", n) )) +
  geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
  geom_sankey_label(aes(x = c(rep(.78,3), rep(.78,73), rep(.78,171), rep(2.25,8), rep(2.25,70), rep(.78,10), rep(2.25,179))),
                    size = 5, color = 1, width = .3, fill = "white") +
  scale_fill_manual(values = c("Aronia arbutifolia"="lightcoral",  
                               "Aronia melanocarpa"="steelblue3",
                               "Aronia \U00D7prunifolia"="khaki1",  
                               "Aronia sp."="gray60",
                               "Aronia eller Sorbaronia"="gray60",
                               "Sorbaronia mitschurinii"="olivedrab4"),
                    name = "") +
  labs(x = "") +
  theme_sankey(base_size = 15) +
  theme(legend.position="none",
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA))

ggsave('Aronia_v2.png', bg='transparent')


##---------------------------------------------------------------------