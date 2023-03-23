######################################
###     PLOTS TIL HANNE HEGRE     ####
######################################

library(tidyverse)
library(ggplot2)
library(data.table)
library(ggsankey)

# Last opp fil
aronia <- read.csv2('Aronia_HH_v2.csv')

## Litt rydding; erstatt 'x' i et artsnavn med det faktiske hybrid-tegn
aronia[aronia$Foer_revisjon == 'Aronia xprunifolia', "Foer_revisjon"] <- 'Aronia \U00D7prunifolia'
aronia[aronia$Etter_revisjon == 'Aronia xprunifolia', "Etter_revisjon"] <- 'Aronia \U00D7prunifolia'
aronia[aronia$Etter_revisjon == 'Aronia/xSorbaronia', "Etter_revisjon"] <- 'Aronia/\U00D7Sorbaronia'
aronia[aronia$Etter_revisjon == 'xSorbaronia mitschurinii', "Etter_revisjon"] <- '\U00D7Sorbaronia mitschurinii'

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
  geom_sankey_label(size = 3, color = 1, fill = "white") +
  scale_fill_manual(values = c("Aronia arbutifolia"="#d73027",  
                               "Aronia melanocarpa"="#fee090",  
                               "Aronia sp."="#fc8d59",
                               "Aronia \U00D7prunifolia"="#4575b4",   
                               "Aronia/\U00D7Sorbaronia"="#91bfdb",
                               "\U00D7Sorbaronia mitschurinii" = "#e0f3f8",
                               "Bestembart, ikke tilgjengelig" = "gray40",
                               "Ikke bestembart" = "gray70"),  
                    name = "") +
  labs(x = "") +
  theme_sankey(base_size = 14) +
  theme(legend.position="none",
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.text.x = element_text(size = 14),
        plot.margin = unit(c(0,-3,0,-4), 'cm')) +
  coord_cartesian(clip = 'off')

ggsave('Aronia_utenVerdier.png', bg='transparent', 
       width = 14.5, height = 9, units = 'cm', device = 'png', dpi = 300)


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
    dplyr::group_by(node, next_node)%>%
    tally()
  # Her må fikses litt manuelt - alle rekker med 'NA' på next_node skal stå som de er, men alle nodes med text i next_node skal summeres
  Sankey2 <- rbind(Sankey2 %>%
                     filter(is.na(next_node)) %>%
                     mutate(n2 = n,
                            x = 'Etter revisjon',
                            next_x = NA)  %>%
                     relocate(x, node, next_x, next_node, n, n2) %>%
                     select(-next_node),
                   
                   Sankey2 %>%
                     filter(!is.na(next_node)) %>%
                     group_by(node) %>%
                     summarise(n2 = sum(n)) %>%
                     mutate(next_node = 'x', n = NA,
                            x = 'F\U00F8r revisjon',
                            next_x = 'Etter revisjon') %>%
                     relocate(x, node, next_x, next_node, n, n2)%>%
                     select(-next_node) )
  
  # Step 3
  Sankey3 <- full_join(Sankey1, Sankey2, by=c('node'='node', 'x'='x', 'next_x'='next_x')) %>%
    mutate(across(c(x, next_x),
                  ~factor(.x, levels = c("F\U00F8r revisjon","Etter revisjon")))) %>%
    arrange(node, next_node)
}

# Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
### Fargeblind-vennlig
ggplot(Sankey3, aes(x = x, 
                    next_x = next_x, 
                    node = node, 
                    next_node = next_node,
                    fill = node,
                    label = paste0(node,", ", n2) )) +
  geom_sankey(flow.alpha = 0.8, node.color = 0.9) +
  geom_sankey_label(aes(x = c(
    # Aronia arbutifolia
    rep(.78, Sankey3 %>% filter(node=='Aronia arbutifolia') %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
    # Aronia melanocarpa
    rep(.78, Sankey3 %>% filter(node=='Aronia melanocarpa' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
    rep(2.1, Sankey3 %>% filter(node=='Aronia melanocarpa' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
    # Aronia sp.
    rep(.78, Sankey3 %>% filter(node=='Aronia sp.' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
    # Aronia \U00D7prunifolia
    rep(.78, Sankey3 %>% filter(node=='Aronia \U00D7prunifolia' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
    rep(2.1, Sankey3 %>% filter(node=='Aronia \U00D7prunifolia' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
    # Aronia/\U00D7Sorbaronia
    rep(2.1, Sankey3 %>% filter(node=='Aronia/\U00D7Sorbaronia' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
    # \U00D7Sorbaronia mitschurinii
    rep(2.1, Sankey3 %>% filter(node=='\U00D7Sorbaronia mitschurinii' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
    # HI
    rep(2.1, Sankey3 %>% filter(node=='Bestembart, ikke tilgjengelig' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
    # SE
    rep(2.1, Sankey3 %>% filter(node=='Ikke bestembart' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]) ) ),   
    size = 3, color = 1, fill = "white", hjust=.25)  +
  scale_fill_manual(values = c("Aronia arbutifolia"="#d73027",  
                               "Aronia melanocarpa"="#fee090",  
                               "Aronia sp."="#fc8d59",
                               "Aronia \U00D7prunifolia"="#4575b4",   
                               "Aronia/\U00D7Sorbaronia"="#91bfdb",
                               "\U00D7Sorbaronia mitschurinii" = "#e0f3f8",
                               "Bestembart, ikke tilgjengelig" = "gray40",
                               "Ikke bestembart" = "gray70"),  
                    name = "") +
  labs(x = "") +
  theme_sankey(base_size = 14) +
  theme(legend.position="none",
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.text.x = element_text(size = 14),
        plot.margin = unit(c(0,-.5,0,-2), 'cm')) +
  coord_cartesian(clip = 'off')

ggsave('Aronia_medVerdier.png', bg='transparent', 
       width = 14.5, height = 9, units = 'cm', device = 'png', dpi = 300)


##---------------------------------------------------------------------