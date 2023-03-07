##-----------------------------------------------------####
##---   STATISTIKK OG FIGURER TIL PARASITT-TEKST    ---####
##-----------------------------------------------------####

library(tidyverse)
library(ggplot2)
library(data.table)
library(ggsankey)
library(ggpubr)

getwd()
setwd("C:/Users/TKP/OneDrive - artsdatabanken.no/Dokumenter/ForRes_ADB") # Sett egen sti

# Les parasitt-filen fra mappe; denne er basert på en filtrering av en eksportfil fra FAB
# Her er inkludert bakterier, kromister, rundormer/flatormer og enkelte sopper. Arter med kategori 'NR' er filtrert nort i forkant
parasitt <- read.csv2("parasitter.csv")

##--- 1. RYDDING    ---####
##---       1.1 Samle etableringsklasse C3-E til én kategori ---####
table(parasitt$Etableringsklasse)

parasitt <- parasitt %>%
  mutate(Etableringsklasse_comb =  case_when(Etableringsklasse == "A" ~ "A",
                                             Etableringsklasse == "B1" ~ "B1",
                                             Etableringsklasse == "B2" ~ "B2",
                                             Etableringsklasse == "C0" ~ "C0",
                                             Etableringsklasse == "C1" ~ "C1",
                                             Etableringsklasse == "C2" ~ "C2",
                                             Etableringsklasse == "C3" | Etableringsklasse == "D1" | Etableringsklasse == "D2" | Etableringsklasse == "E" ~ "C3E"), .keep = "all") %>%
  mutate(across(c(Etableringsklasse_comb),
                ~ordered(.x, levels = c("A","B1","B2","C0","C1","C2","C3E"))))

# Definer en bedre kategori for arter fra Horisontskanningen
parasitt$Kategori2018[parasitt$Kategori2018==""] <- "Ikke risikovurdert tidligere"
# Definer rette faktor-nivåer i rett rekkefølge
parasitt <- parasitt %>%
  # Risikokategorier
  mutate(across(c(Kategori2023, Kategori2018),
                ~ordered(.x, levels = c("Ikke risikovurdert tidligere","NR","NK","LO","PH","HI","SE"))))



##---   2. PLOTS    ---####
##---       2.1 Risikokategori  ---####
parasitt %>%  # Innsett ev. filtre
  {
    ggplot(.,
           aes(x = Kategori2023, fill = Kategori2023)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.5, size = 6) +
      scale_x_discrete( #drop=FALSE,
        labels = c(#"NR" = "Ikke risikovurdert\nNR",
          "NK" = "Ingen kjent \nrisiko\nNK",
          "LO" = "Lav \nrisiko\nLO",
          "PH" = "Potensielt h\U00F8y \nrisiko\nPH",
          "HI" = "H\U00F8y \nrisiko\nHI",
          "SE" = "Sv\U00E6rt h\U00F8y \nrisiko\nSE")) +
      scale_fill_manual(values = c(#"NR"="white",
        "NK"="#a6ad59",
        "LO"="#60a5a3",
        "PH"="#1b586c",
        "HI"="#233368",
        "SE"="#602d5e"))  +
      theme_minimal(base_size = 16) +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 16),
            plot.margin = unit(c(.5,0,-.5,0), 'cm'),
            strip.background = element_rect(fill = 'gray90'))  +
      facet_wrap(~ Ekspertkomite) +   # Kan ev. kommenteres ut herfra
      coord_cartesian(ylim = c(0,45))
  }
#ggsave('alleArter/risikokategori.png', bg='transparent', 
#       width = 18.15, height = 11.62, units = 'cm', device = 'png', dpi = 300)

##---       2.2 Etableringsklasse ---####
parasitt %>%
  {
    ggplot(.,
           aes(x = Etableringsklasse_comb, fill = Etableringsklasse_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.5, size = 6) +
      geom_segment(aes(x = 'A', xend = 'C0', y = 90, yend = 90),    # y = 65 | 90
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'B1', y = 95, label='D\U00F8rstokkarter'), size=5) +  # y = 70 | 95
      geom_segment(aes(x = 'C2', xend = 'C3E', y = 90, yend = 90),  # y = 65 | 90
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'C2', y = 100, label='Selvstendig \nreproduserende'), size=5, hjust=-.00001) +  # y = 70 | 100
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#5FB7B1",
                                   "B2"="#71B581",
                                   "C0"="#A0BA5B",
                                   "C1"="#d2c160",
                                   "C2"="#e5b445",
                                   "C3E"="#936649")) +
      scale_x_discrete(labels = c("A" = "Forekommer \nikke i Norge",
                                  "B1" = "Forekommer \ninnend\U00F8rs eller i \nlukkede installasjoner",
                                  "B2" = "Forekommer \nutend\U00F8rs p\U00E5 eget \nproduksjonsareal",
                                  "C0" = "Dokumentert i \nnorsk natur",
                                  "C1" = "Overlever vinteren \nutend\U00F8rs uten \nmenneskelig tilsyn",
                                  "C2" = "Selvstendig \nreproduserende",
                                  "C3E" = "Etablert i \nnorsk natur")) +
      theme_minimal(base_size = 16) +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .8, size =16),
            plot.margin = unit(c(.5,.5,-1,0), 'cm'),
            strip.background = element_rect(fill = 'gray90'))  +
      facet_wrap(~ Ekspertkomite) +   # Kan ev. kommenteres ut herfra
      coord_cartesian(ylim = c(0,45), clip = 'off') 
  }
#ggsave('alleArter/etableringsklasse.png', bg='transparent', 
#       width = 28.01, height = 11.62, units = 'cm', device = 'png', dpi = 300)

### Som kakediagram
parasitt %>%
  group_by(Etableringsklasse_comb) %>%
  tally() %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(desc(Etableringsklasse_comb)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%  {
    ggplot(., aes(x = "", y = prop, fill = Etableringsklasse_comb)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0, clip = 'off')+
      geom_text(aes(y = lab.ypos, x=1.3, label = n), color = "white", size = 6)+
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#5FB7B1","B2"="#71B581",
                                   "C0"="#A0BA5B", "C1"="#d2c160", "C2"="#e5b445",
                                   "C3E"="#936649"),
                        labels = c("A" = "Forekommer ikke i Norge",
                                   "B1" = "Forekommer innend\U00F8rs eller \ni lukkede installasjoner",
                                   "B2" = "Forekommer utend\U00F8rs p\U00E5 \neget produksjonsareal",
                                   "C0" = "Dokumentert i norsk natur",
                                   "C1" = "Overlever vinteren utend\U00F8rs \nuten menneskelig tilsyn",
                                   "C2" = "Selvstendig reproduserende",
                                   "C3E" = "Etablert i norsk natur"),
                        name = '') +
      theme_void(base_size = 16) +
      theme(legend.position = "right",
            legend.text = element_text(size = 16),
            legend.spacing.y = unit(.75, 'cm'),
            plot.margin = unit(c(0,0,0,0), 'cm'))  +
      guides(fill = guide_legend(byrow = TRUE))
  }
#ggsave('alleArter/etableringsklasse_kake.png', bg='transparent', 
#       width = 28.01, height = 11.62, units = 'cm', device = 'png', dpi = 300)

# Kakediagram delt i Ekspertgrupper
### Som kakediagram
parasitt %>%
  group_by(Ekspertkomite, Etableringsklasse_comb) %>%
  tally() %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(desc(Etableringsklasse_comb)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%  {
    ggplot(., aes(x = "", y = prop, fill = Etableringsklasse_comb)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0, clip = 'off')+
      geom_text(aes(y = lab.ypos, x=1.3, label = n), color = "white", size = 6)+
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#5FB7B1","B2"="#71B581",
                                   "C0"="#A0BA5B", "C1"="#d2c160", "C2"="#e5b445",
                                   "C3E"="#936649"),
                        labels = c("A" = "Forekommer ikke i Norge",
                                   "B1" = "Forekommer innend\U00F8rs eller \ni lukkede installasjoner",
                                   "B2" = "Forekommer utend\U00F8rs p\U00E5 \neget produksjonsareal",
                                   "C0" = "Dokumentert i norsk natur",
                                   "C1" = "Overlever vinteren utend\U00F8rs \nuten menneskelig tilsyn",
                                   "C2" = "Selvstendig reproduserende",
                                   "C3E" = "Etablert i norsk natur"),
                        name = '') +
      theme_void(base_size = 16) +
      theme(legend.position = "right",
            legend.text = element_text(size = 16),
            legend.spacing.y = unit(.75, 'cm'),
            plot.margin = unit(c(0,0,0,0), 'cm'),
            strip.background = element_rect(fill = 'gray90'))  +
      facet_wrap(~ Ekspertkomite) + 
      guides(fill = guide_legend(byrow = TRUE))
  }

##---       2.3 Endring i kategori  ---####

# Versjon med forkortet kategori
{
  parasitt %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert \ntidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert \ntidligere",
                                          "NR",
                                          "NK",
                                          "LO",
                                          "PH",
                                          "HI",
                                          "SE")))) %>% {
                                            ggplot(., aes(x = x, 
                                                          next_x = next_x, 
                                                          node = node, 
                                                          next_node = next_node,
                                                          label = node,
                                                          fill = node )) +
                                              geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
                                              geom_sankey_label(size = 6, color = 1, fill = "white") +
                                              scale_fill_manual(values = c("Ikke risikovurdert \ntidligere"="gray70",
                                                                           "NR"="gray90",
                                                                           "NK"="#a6ad59",
                                                                           "LO"="#60a5a3",
                                                                           "PH"="#1b586c",
                                                                           "HI"="#233368",
                                                                           "SE"="#602d5e"),
                                                                name = "") +
                                              labs(x = "") +
                                              theme_sankey(base_size = 16) +
                                              theme(legend.position="none",
                                                    panel.background = element_rect(fill='transparent', color = NA),
                                                    plot.background = element_rect(fill='transparent', color=NA),
                                                    axis.text.x = element_text(size = 16),
                                                    plot.margin = unit(c(0,-5,0,-5), 'cm')) +
                                              coord_cartesian(clip = 'off')
                                          }
}
# Med verdier
{
  # Step 1
  Sankey1 <- parasitt %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert \ntidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert \ntidligere",
                                          "NR",
                                          "NK",
                                          "LO",
                                          "PH",
                                          "HI",
                                          "SE"))))
  
  # Step 2
  Sankey2 <- Sankey1 %>%
    dplyr::group_by(node, next_node) %>%
    tally()
  # Her må fikses litt manuelt - alle rekker med 'NA' på next_node skal stå som de er, men alle nodes med kategori i next_node skal summeres
  Sankey2 <- rbind(Sankey2 %>%
                     filter(is.na(next_node)) %>%
                     mutate(n2 = n,
                            x = 'Kategori 2023',
                            next_x = NA)  %>%
                     relocate(x, node, next_x, next_node, n, n2) %>%
                     select(-next_node),
                   
                   Sankey2 %>%
                     filter(!is.na(next_node)) %>%
                     group_by(node) %>%
                     summarise(n2 = sum(n)) %>%
                     mutate(next_node = 'x', n = NA,
                            x = 'Kategori 2018',
                            next_x = 'Kategori 2023') %>%
                     relocate(x, node, next_x, next_node, n, n2)%>%
                     select(-next_node) )
  
  ### Step 3
  Sankey3 <- full_join(Sankey1, Sankey2, by=c('node'='node', 'x'='x', 'next_x'='next_x')) %>%
    mutate(across(c(x, next_x),
                  ~factor(.x, levels = c("Kategori 2018","Kategori 2023")))) %>%
    arrange(node, next_node)
  
  # Plot 
  ggplot(Sankey3, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = node,
                      label = paste0(node,", ", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(
      # Ikke risikovurdert tidligere
      rep(.78, Sankey3 %>% filter(node=='Ikke risikovurdert \ntidligere') %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      # NK
      rep(.78, Sankey3 %>% filter(node=='NK' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='NK' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # LO
      rep(.78, Sankey3 %>% filter(node=='LO' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='LO' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # PH
      rep(.78, Sankey3 %>% filter(node=='PH' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='PH' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # HI
      rep(.78, Sankey3 %>% filter(node=='HI' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='HI' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # SE
      rep(.78, Sankey3 %>% filter(node=='SE' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='SE' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]) ) ),   
      size = 6, color = 1, fill = "white", hjust=.25)  +
    scale_fill_manual(values = c("NR"="gray90",
                                 "NK"="#a6ad59",
                                 "LO"="#60a5a3",
                                 "PH"="#1b586c",
                                 "HI"="#233368",
                                 "SE"="#602d5e"),
                      name = "") +
    labs(x = "") +
    theme_sankey(base_size = 16) +
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA),
          axis.text.x = element_text(size = 16),
          plot.margin = unit(c(0,-2.5,0,-2.5), 'cm')) +
    coord_cartesian(clip = 'off')
}
# Splittet i ekspergrupper
# Bakterier
{
  # Step 1
  Sankey1 <- parasitt %>%
    filter(Ekspertkomite == 'Bakterier') %>% 
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert \ntidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert \ntidligere",
                                          "NR",
                                          "NK",
                                          "LO",
                                          "PH",
                                          "HI",
                                          "SE"))))
  
  # Step 2
  Sankey2 <- Sankey1 %>%
    dplyr::group_by(node, next_node) %>%
    tally()
  # Her må fikses litt manuelt - alle rekker med 'NA' på next_node skal stå som de er, men alle nodes med kategori i next_node skal summeres
  Sankey2 <- rbind(Sankey2 %>%
                     filter(is.na(next_node)) %>%
                     mutate(n2 = n,
                            x = 'Kategori 2023',
                            next_x = NA)  %>%
                     relocate(x, node, next_x, next_node, n, n2) %>%
                     select(-next_node),
                   
                   Sankey2 %>%
                     filter(!is.na(next_node)) %>%
                     group_by(node) %>%
                     summarise(n2 = sum(n)) %>%
                     mutate(next_node = 'x', n = NA,
                            x = 'Kategori 2018',
                            next_x = 'Kategori 2023') %>%
                     relocate(x, node, next_x, next_node, n, n2)%>%
                     select(-next_node) )
  
  ### Step 3
  Sankey3 <- full_join(Sankey1, Sankey2, by=c('node'='node', 'x'='x', 'next_x'='next_x')) %>%
    mutate(across(c(x, next_x),
                  ~factor(.x, levels = c("Kategori 2018","Kategori 2023")))) %>%
    arrange(node, next_node)
  
  # Plot 
  plot_bakterier <- ggplot(Sankey3, aes(x = x, 
                                        next_x = next_x, 
                                        node = node, 
                                        next_node = next_node,
                                        fill = node,
                                        label = paste0(node,", ", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(
      # Ikke risikovurdert tidligere
      rep(.78, Sankey3 %>% filter(node=='Ikke risikovurdert \ntidligere') %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      # NK
      ##rep(.78, Sankey3 %>% filter(node=='NK' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      ##rep(2.1, Sankey3 %>% filter(node=='NK' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # LO
      ##rep(.78, Sankey3 %>% filter(node=='LO' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      ##rep(2.1, Sankey3 %>% filter(node=='LO' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # PH
      ##rep(.78, Sankey3 %>% filter(node=='PH' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='PH' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # HI
      ##rep(.78, Sankey3 %>% filter(node=='HI' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='HI' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # SE
      ##rep(.78, Sankey3 %>% filter(node=='SE' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='SE' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]) ) ),   
      size = 6, color = 1, fill = "white", hjust=.25)  +
    scale_fill_manual(values = c("NR"="gray90",
                                 "NK"="#a6ad59",
                                 "LO"="#60a5a3",
                                 "PH"="#1b586c",
                                 "HI"="#233368",
                                 "SE"="#602d5e"),
                      name = "") +
    labs(x = "") +
    theme_sankey(base_size = 16) +
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA),
          axis.text.x = element_text(size = 16),
          plot.margin = unit(c(0,-2.5,0,-2.5), 'cm')) +
    coord_cartesian(clip = 'off')
}
# Kromister
{
  # Step 1
  Sankey1 <- parasitt %>%
    filter(Ekspertkomite == 'Kromister') %>% 
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert \ntidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert \ntidligere",
                                          "NR",
                                          "NK",
                                          "LO",
                                          "PH",
                                          "HI",
                                          "SE"))))
  
  # Step 2
  Sankey2 <- Sankey1 %>%
    dplyr::group_by(node, next_node) %>%
    tally()
  # Her må fikses litt manuelt - alle rekker med 'NA' på next_node skal stå som de er, men alle nodes med kategori i next_node skal summeres
  Sankey2 <- rbind(Sankey2 %>%
                     filter(is.na(next_node)) %>%
                     mutate(n2 = n,
                            x = 'Kategori 2023',
                            next_x = NA)  %>%
                     relocate(x, node, next_x, next_node, n, n2) %>%
                     select(-next_node),
                   
                   Sankey2 %>%
                     filter(!is.na(next_node)) %>%
                     group_by(node) %>%
                     summarise(n2 = sum(n)) %>%
                     mutate(next_node = 'x', n = NA,
                            x = 'Kategori 2018',
                            next_x = 'Kategori 2023') %>%
                     relocate(x, node, next_x, next_node, n, n2)%>%
                     select(-next_node) )
  
  ### Step 3
  Sankey3 <- full_join(Sankey1, Sankey2, by=c('node'='node', 'x'='x', 'next_x'='next_x')) %>%
    mutate(across(c(x, next_x),
                  ~factor(.x, levels = c("Kategori 2018","Kategori 2023")))) %>%
    arrange(node, next_node)
  
  # Plot 
  plot_kromister <- ggplot(Sankey3, aes(x = x, 
                                        next_x = next_x, 
                                        node = node, 
                                        next_node = next_node,
                                        fill = node,
                                        label = paste0(node,", ", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(
      # Ikke risikovurdert tidligere
      rep(.78, Sankey3 %>% filter(node=='Ikke risikovurdert \ntidligere') %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      # NK
      rep(.78, Sankey3 %>% filter(node=='NK' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      ##rep(2.1, Sankey3 %>% filter(node=='NK' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # LO
      rep(.78, Sankey3 %>% filter(node=='LO' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='LO' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # PH
      ##rep(.78, Sankey3 %>% filter(node=='PH' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='PH' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # HI
      rep(.78, Sankey3 %>% filter(node=='HI' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='HI' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # SE
      rep(.78, Sankey3 %>% filter(node=='SE' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='SE' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]) ) ),   
      size = 6, color = 1, fill = "white", hjust=.25)  +
    scale_fill_manual(values = c("NR"="gray90",
                                 "NK"="#a6ad59",
                                 "LO"="#60a5a3",
                                 "PH"="#1b586c",
                                 "HI"="#233368",
                                 "SE"="#602d5e"),
                      name = "") +
    labs(x = "") +
    theme_sankey(base_size = 16) +
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA),
          axis.text.x = element_text(size = 16),
          plot.margin = unit(c(0,-2.5,0,-2.5), 'cm')) +
    coord_cartesian(clip = 'off')
}
# Rundormer og flatormer
{
  # Step 1
  Sankey1 <- parasitt %>%
    filter(Ekspertkomite == 'Rundormer og flatormer') %>% 
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert \ntidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert \ntidligere",
                                          "NR",
                                          "NK",
                                          "LO",
                                          "PH",
                                          "HI",
                                          "SE"))))
  
  # Step 2
  Sankey2 <- Sankey1 %>%
    dplyr::group_by(node, next_node) %>%
    tally()
  # Her må fikses litt manuelt - alle rekker med 'NA' på next_node skal stå som de er, men alle nodes med kategori i next_node skal summeres
  Sankey2 <- rbind(Sankey2 %>%
                     filter(is.na(next_node)) %>%
                     mutate(n2 = n,
                            x = 'Kategori 2023',
                            next_x = NA)  %>%
                     relocate(x, node, next_x, next_node, n, n2) %>%
                     select(-next_node),
                   
                   Sankey2 %>%
                     filter(!is.na(next_node)) %>%
                     group_by(node) %>%
                     summarise(n2 = sum(n)) %>%
                     mutate(next_node = 'x', n = NA,
                            x = 'Kategori 2018',
                            next_x = 'Kategori 2023') %>%
                     relocate(x, node, next_x, next_node, n, n2)%>%
                     select(-next_node) )
  
  ### Step 3
  Sankey3 <- full_join(Sankey1, Sankey2, by=c('node'='node', 'x'='x', 'next_x'='next_x')) %>%
    mutate(across(c(x, next_x),
                  ~factor(.x, levels = c("Kategori 2018","Kategori 2023")))) %>%
    arrange(node, next_node)
  
  # Plot 
  plot_rundormer <- ggplot(Sankey3, aes(x = x, 
                                        next_x = next_x, 
                                        node = node, 
                                        next_node = next_node,
                                        fill = node,
                                        label = paste0(node,", ", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(
      # Ikke risikovurdert tidligere
      rep(.78, Sankey3 %>% filter(node=='Ikke risikovurdert \ntidligere') %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      # NK
      #rep(.78, Sankey3 %>% filter(node=='NK' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='NK' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # LO
      rep(.78, Sankey3 %>% filter(node=='LO' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='LO' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # PH
      rep(.78, Sankey3 %>% filter(node=='PH' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='PH' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # HI
      rep(.78, Sankey3 %>% filter(node=='HI' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='HI' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # SE
      rep(.78, Sankey3 %>% filter(node=='SE' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='SE' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]) ) ),   
      size = 6, color = 1, fill = "white", hjust=.25)  +
    scale_fill_manual(values = c("NR"="gray90",
                                 "NK"="#a6ad59",
                                 "LO"="#60a5a3",
                                 "PH"="#1b586c",
                                 "HI"="#233368",
                                 "SE"="#602d5e"),
                      name = "") +
    labs(x = "") +
    theme_sankey(base_size = 16) +
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA),
          axis.text.x = element_text(size = 16),
          plot.margin = unit(c(0,-2.5,0,-2.5), 'cm')) +
    coord_cartesian(clip = 'off')
}
# Sopper
{
  # Step 1
  Sankey1 <- parasitt %>%
    filter(Ekspertkomite == 'Sopper') %>% 
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert \ntidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert \ntidligere",
                                          "NR",
                                          "NK",
                                          "LO",
                                          "PH",
                                          "HI",
                                          "SE"))))
  
  # Step 2
  Sankey2 <- Sankey1 %>%
    dplyr::group_by(node, next_node) %>%
    tally()
  # Her må fikses litt manuelt - alle rekker med 'NA' på next_node skal stå som de er, men alle nodes med kategori i next_node skal summeres
  Sankey2 <- rbind(Sankey2 %>%
                     filter(is.na(next_node)) %>%
                     mutate(n2 = n,
                            x = 'Kategori 2023',
                            next_x = NA)  %>%
                     relocate(x, node, next_x, next_node, n, n2) %>%
                     select(-next_node),
                   
                   Sankey2 %>%
                     filter(!is.na(next_node)) %>%
                     group_by(node) %>%
                     summarise(n2 = sum(n)) %>%
                     mutate(next_node = 'x', n = NA,
                            x = 'Kategori 2018',
                            next_x = 'Kategori 2023') %>%
                     relocate(x, node, next_x, next_node, n, n2)%>%
                     select(-next_node) )
  
  ### Step 3
  Sankey3 <- full_join(Sankey1, Sankey2, by=c('node'='node', 'x'='x', 'next_x'='next_x')) %>%
    mutate(across(c(x, next_x),
                  ~factor(.x, levels = c("Kategori 2018","Kategori 2023")))) %>%
    arrange(node, next_node)
  
  # Plot 
  plot_sopp <- ggplot(Sankey3, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = node,
                      label = paste0(node,", ", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(
      # Ikke risikovurdert tidligere
      rep(.78, Sankey3 %>% filter(node=='Ikke risikovurdert \ntidligere') %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      # NK
      rep(.78, Sankey3 %>% filter(node=='NK' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='NK' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # LO
      rep(.78, Sankey3 %>% filter(node=='LO' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='LO' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # PH
      rep(.78, Sankey3 %>% filter(node=='PH' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='PH' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # HI
      rep(.78, Sankey3 %>% filter(node=='HI' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='HI' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # SE
      rep(.78, Sankey3 %>% filter(node=='SE' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3 %>% filter(node=='SE' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]) ) ),   
      size = 6, color = 1, fill = "white", hjust=.25)  +
    scale_fill_manual(values = c("NR"="gray90",
                                 "NK"="#a6ad59",
                                 "LO"="#60a5a3",
                                 "PH"="#1b586c",
                                 "HI"="#233368",
                                 "SE"="#602d5e"),
                      name = "") +
    labs(x = "") +
    theme_sankey(base_size = 16) +
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA),
          axis.text.x = element_text(size = 16),
          plot.margin = unit(c(0,-2.5,0,-2.5), 'cm')) +
    coord_cartesian(clip = 'off')
}
# Kombiner plots