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
# Samle etableringsklasse C3-E til én kategori ---####
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
  # Risikokategori 2023
  mutate(across(c(Kategori2023, Kategori2018),
                ~ordered(.x, levels = c("Ikke risikovurdert tidligere","NR","NK","LO","PH","HI","SE")))) %>% 
  # Omdøp kromister
  mutate(Ekspertkomite = case_when(Ekspertkomite == 'Kromister' ~ 'Eggsporesopper',
                                   Ekspertkomite != 'Kromister' ~ Ekspertkomite))

# Omdøp Fremmedartsstatus
parasitt <- parasitt %>% 
  mutate(Fremmedartsstatus = case_when(Fremmedartsstatus == 'Selvstendig reproduserende' ~ Fremmedartsstatus,
                                       Fremmedartsstatus != 'Selvstendig reproduserende' ~ 'Doerstokkart'))


##---   2. PLOTS    ---####
##---       2.1 Risikokategori  ---####
p_risikokategori <- parasitt %>%
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
            strip.background = element_rect(fill = 'gray90')) 
  }

p_risikokategori_grupper <- parasitt %>%
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

p_risikokategori_fremmedartsstatus <- parasitt %>%
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
      facet_wrap(~ Fremmedartsstatus) +   # Kan ev. kommenteres ut herfra
      coord_cartesian(ylim = c(0,35))
  }

p_risikokategori_grupper_status <- parasitt %>%
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
      facet_grid(Ekspertkomite ~ Fremmedartsstatus) +   # Kan ev. kommenteres ut herfra
      coord_cartesian(ylim = c(0,30))
  }

p_risikokategori_grupper_etablering <- parasitt %>%
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
      facet_grid(Etableringsklasse_comb ~ Ekspertkomite) +   # Kan ev. kommenteres ut herfra
      coord_cartesian(ylim = c(0,25))
  }

##---       2.2 Etableringsklasse ---####
p_etableringsklasse <- parasitt %>%
  {
    ggplot(.,
           aes(x = Etableringsklasse_comb, fill = Etableringsklasse_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.5, size = 6) +
      geom_segment(aes(x = 'A', xend = 'C0', y = 65, yend = 65),   
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'B1', y = 70, label='D\U00F8rstokkarter'), size=5) + 
      geom_segment(aes(x = 'C2', xend = 'C3E', y = 65, yend = 65), 
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'C2', y = 70, label='Selvstendig \nreproduserende'), size=5, hjust=-.00001) +  
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
            strip.background = element_rect(fill = 'gray90')) +
      coord_cartesian(clip = 'off') 
  }
p_etableringsklasse_grupper <- parasitt %>%
  {
    ggplot(.,
           aes(x = Etableringsklasse_comb, fill = Etableringsklasse_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.5, size = 6) +
      geom_segment(aes(x = 'A', xend = 'C0', y = 90, yend = 90),    
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'B1', y = 95, label='D\U00F8rstokkarter'), size=5) +  
      geom_segment(aes(x = 'C2', xend = 'C3E', y = 90, yend = 90), 
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'C2', y = 100, label='Selvstendig \nreproduserende'), size=5, hjust=-.00001) +  
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
            strip.background = element_rect(fill = 'gray90')) + # Kan ev. kommenteres ut herfra
      facet_wrap(~ Ekspertkomite) +
      coord_cartesian(ylim = c(0,45), clip = 'off') 
  }

### Som kakediagram
p_kake <- parasitt %>%
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

# Kakediagram delt i Ekspertgrupper
### Som kakediagram
p_kake_grupper <- parasitt %>%
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

# Nødvendig med identiske risikokategorier begge år
parasitt <- parasitt %>% 
  mutate(across(c(Kategori2023, Kategori2018),
              ~ordered(.x, levels = c("Ikke risikovurdert tidligere","NR","NK","LO","PH","HI","SE"))))
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
  p_endring <- ggplot(Sankey3, aes(x = x, 
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
p_endring

# Splittet i ekspergrupper
# Bakterier
{
  # Step 1
  Sankey1_b <- parasitt %>%
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
  Sankey2_b <- Sankey1_b %>%
    dplyr::group_by(node, next_node) %>%
    tally()
  # Her må fikses litt manuelt - alle rekker med 'NA' på next_node skal stå som de er, men alle nodes med kategori i next_node skal summeres
  Sankey2_b <- rbind(Sankey2_b %>%
                     filter(is.na(next_node)) %>%
                     mutate(n2 = n,
                            x = 'Kategori 2023',
                            next_x = NA)  %>%
                     relocate(x, node, next_x, next_node, n, n2) %>%
                     select(-next_node),
                   
                   Sankey2_b %>%
                     filter(!is.na(next_node)) %>%
                     group_by(node) %>%
                     summarise(n2 = sum(n)) %>%
                     mutate(next_node = 'x', n = NA,
                            x = 'Kategori 2018',
                            next_x = 'Kategori 2023') %>%
                     relocate(x, node, next_x, next_node, n, n2)%>%
                     select(-next_node) )
  
  ### Step 3
  Sankey3_b <- full_join(Sankey1_b, Sankey2_b, by=c('node'='node', 'x'='x', 'next_x'='next_x')) %>%
    mutate(across(c(x, next_x),
                  ~factor(.x, levels = c("Kategori 2018","Kategori 2023")))) %>%
    arrange(node, next_node)
  
  # Plot 
  p_bakterier <- ggplot(Sankey3_b, aes(x = x, 
                                        next_x = next_x, 
                                        node = node, 
                                        next_node = next_node,
                                        fill = node,
                                        label = paste0(node,", ", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(
      # Ikke risikovurdert tidligere
      rep(.78, Sankey3_b %>% filter(node=='Ikke risikovurdert \ntidligere') %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      # NK
      ##rep(.78, Sankey3_b %>% filter(node=='NK' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      ##rep(2.1, Sankey3_b %>% filter(node=='NK' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # LO
      ##rep(.78, Sankey3_b %>% filter(node=='LO' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      ##rep(2.1, Sankey3_b %>% filter(node=='LO' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # PH
      ##rep(.78, Sankey3_b %>% filter(node=='PH' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_b %>% filter(node=='PH' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # HI
      ##rep(.78, Sankey3_b %>% filter(node=='HI' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_b %>% filter(node=='HI' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # SE
      ##rep(.78, Sankey3_b %>% filter(node=='SE' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_b %>% filter(node=='SE' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]) ) ),   
      size = 6, color = 1, fill = "white", hjust=.25)  +
    scale_fill_manual(values = c("NR"="gray90",
                                 "NK"="#a6ad59",
                                 "LO"="#60a5a3",
                                 "PH"="#1b586c",
                                 "HI"="#233368",
                                 "SE"="#602d5e"),
                      name = "") +
    labs(title = 'Bakterier', x = "") +
    theme_sankey(base_size = 16) +
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA),
          axis.text.x = element_text(size = 16),
          plot.margin = unit(c(0,-2.5,0,-2.5), 'cm'),
          plot.title = element_text(hjust = .5)) +
    coord_cartesian(clip = 'off')
}
p_bakterier

# Kromister/eggsporesopper
{
  # Step 1
  Sankey1_k <- parasitt %>%
    filter(Ekspertkomite == 'Eggsporesopper') %>% 
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
  Sankey2_k <- Sankey1_k %>%
    dplyr::group_by(node, next_node) %>%
    tally()
  # Her må fikses litt manuelt - alle rekker med 'NA' på next_node skal stå som de er, men alle nodes med kategori i next_node skal summeres
  Sankey2_k <- rbind(Sankey2_k %>%
                     filter(is.na(next_node)) %>%
                     mutate(n2 = n,
                            x = 'Kategori 2023',
                            next_x = NA)  %>%
                     relocate(x, node, next_x, next_node, n, n2) %>%
                     select(-next_node),
                   
                   Sankey2_k %>%
                     filter(!is.na(next_node)) %>%
                     group_by(node) %>%
                     summarise(n2 = sum(n)) %>%
                     mutate(next_node = 'x', n = NA,
                            x = 'Kategori 2018',
                            next_x = 'Kategori 2023') %>%
                     relocate(x, node, next_x, next_node, n, n2)%>%
                     select(-next_node) )
  
  ### Step 3
  Sankey3_k <- full_join(Sankey1_k, Sankey2_k, by=c('node'='node', 'x'='x', 'next_x'='next_x')) %>%
    mutate(across(c(x, next_x),
                  ~factor(.x, levels = c("Kategori 2018","Kategori 2023")))) %>%
    arrange(node, next_node)
  
  # Plot 
  p_kromister <- ggplot(Sankey3_k, aes(x = x, 
                                        next_x = next_x, 
                                        node = node, 
                                        next_node = next_node,
                                        fill = node,
                                        label = paste0(node,", ", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(
      # Ikke risikovurdert tidligere
      rep(.78, Sankey3_k %>% filter(node=='Ikke risikovurdert \ntidligere') %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      # NK
      rep(.78, Sankey3_k %>% filter(node=='NK' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      ##rep(2.1, Sankey3_k %>% filter(node=='NK' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # LO
      rep(.78, Sankey3_k %>% filter(node=='LO' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_k %>% filter(node=='LO' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # PH
      ##rep(.78, Sankey3_k %>% filter(node=='PH' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_k %>% filter(node=='PH' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # HI
      rep(.78, Sankey3_k %>% filter(node=='HI' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_k %>% filter(node=='HI' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # SE
      rep(.78, Sankey3_k %>% filter(node=='SE' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_k %>% filter(node=='SE' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]) ) ),   
      size = 6, color = 1, fill = "white", hjust=.25)  +
    scale_fill_manual(values = c("NR"="gray90",
                                 "NK"="#a6ad59",
                                 "LO"="#60a5a3",
                                 "PH"="#1b586c",
                                 "HI"="#233368",
                                 "SE"="#602d5e"),
                      name = "") +
    labs(title = 'Eggsporesopper', x = "") +
    theme_sankey(base_size = 16) +
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA),
          axis.text.x = element_text(size = 16),
          plot.margin = unit(c(0,-2.5,0,-2.5), 'cm'),
          plot.title = element_text(hjust = .5)) +
    coord_cartesian(clip = 'off')
}
p_kromister

# Rundormer og flatormer
{
  # Step 1
  Sankey1_r <- parasitt %>%
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
  Sankey2_r <- Sankey1_r %>%
    dplyr::group_by(node, next_node) %>%
    tally()
  # Her må fikses litt manuelt - alle rekker med 'NA' på next_node skal stå som de er, men alle nodes med kategori i next_node skal summeres
  Sankey2_r <- rbind(Sankey2_r %>%
                     filter(is.na(next_node)) %>%
                     mutate(n2 = n,
                            x = 'Kategori 2023',
                            next_x = NA)  %>%
                     relocate(x, node, next_x, next_node, n, n2) %>%
                     select(-next_node),
                   
                   Sankey2_r %>%
                     filter(!is.na(next_node)) %>%
                     group_by(node) %>%
                     summarise(n2 = sum(n)) %>%
                     mutate(next_node = 'x', n = NA,
                            x = 'Kategori 2018',
                            next_x = 'Kategori 2023') %>%
                     relocate(x, node, next_x, next_node, n, n2)%>%
                     select(-next_node) )
  
  ### Step 3
  Sankey3_r <- full_join(Sankey1_r, Sankey2_r, by=c('node'='node', 'x'='x', 'next_x'='next_x')) %>%
    mutate(across(c(x, next_x),
                  ~factor(.x, levels = c("Kategori 2018","Kategori 2023")))) %>%
    arrange(node, next_node)
  
  # Plot 
  p_rundormer <- ggplot(Sankey3_r, aes(x = x, 
                                        next_x = next_x, 
                                        node = node, 
                                        next_node = next_node,
                                        fill = node,
                                        label = paste0(node,", ", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(
      # Ikke risikovurdert tidligere
      rep(.78, Sankey3_r %>% filter(node=='Ikke risikovurdert \ntidligere') %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      # NK
      #rep(.78, Sankey3_r %>% filter(node=='NK' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_r %>% filter(node=='NK' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # LO
      rep(.78, Sankey3_r %>% filter(node=='LO' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_r %>% filter(node=='LO' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # PH
      rep(.78, Sankey3_r %>% filter(node=='PH' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_r %>% filter(node=='PH' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # HI
      rep(.78, Sankey3_r %>% filter(node=='HI' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_r %>% filter(node=='HI' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # SE
      rep(.78, Sankey3_r %>% filter(node=='SE' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_r %>% filter(node=='SE' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]) ) ),   
      size = 6, color = 1, fill = "white", hjust=.25)  +
    scale_fill_manual(values = c("NR"="gray90",
                                 "NK"="#a6ad59",
                                 "LO"="#60a5a3",
                                 "PH"="#1b586c",
                                 "HI"="#233368",
                                 "SE"="#602d5e"),
                      name = "") +
    labs(title = 'Rundormer og flatormer' ,x = "") +
    theme_sankey(base_size = 16) +
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA),
          axis.text.x = element_text(size = 16),
          plot.margin = unit(c(0,-2.5,0,-2.5), 'cm'),
          plot.title = element_text(hjust = .5)) +
    coord_cartesian(clip = 'off')
}
p_rundormer

# Sopper
{
  # Step 1
  Sankey1_s <- parasitt %>%
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
  Sankey2_s <- Sankey1_s %>%
    dplyr::group_by(node, next_node) %>%
    tally()
  # Her må fikses litt manuelt - alle rekker med 'NA' på next_node skal stå som de er, men alle nodes med kategori i next_node skal summeres
  Sankey2_s <- rbind(Sankey2_s %>%
                     filter(is.na(next_node)) %>%
                     mutate(n2 = n,
                            x = 'Kategori 2023',
                            next_x = NA)  %>%
                     relocate(x, node, next_x, next_node, n, n2) %>%
                     select(-next_node),
                   
                   Sankey2_s %>%
                     filter(!is.na(next_node)) %>%
                     group_by(node) %>%
                     summarise(n2 = sum(n)) %>%
                     mutate(next_node = 'x', n = NA,
                            x = 'Kategori 2018',
                            next_x = 'Kategori 2023') %>%
                     relocate(x, node, next_x, next_node, n, n2)%>%
                     select(-next_node) )
  
  ### Step 3
  Sankey3_s <- full_join(Sankey1_s, Sankey2_s, by=c('node'='node', 'x'='x', 'next_x'='next_x')) %>%
    mutate(across(c(x, next_x),
                  ~factor(.x, levels = c("Kategori 2018","Kategori 2023")))) %>%
    arrange(node, next_node)
  
  # Plot 
  p_sopp <- ggplot(Sankey3_s, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = node,
                      label = paste0(node,", ", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(
      # Ikke risikovurdert tidligere
      rep(.78, Sankey3_s %>% filter(node=='Ikke risikovurdert \ntidligere') %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      # NK
      rep(.78, Sankey3_s %>% filter(node=='NK' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_s %>% filter(node=='NK' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # LO
      rep(.78, Sankey3_s %>% filter(node=='LO' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_s %>% filter(node=='LO' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # PH
      rep(.78, Sankey3_s %>% filter(node=='PH' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_s %>% filter(node=='PH' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # HI
      rep(.78, Sankey3_s %>% filter(node=='HI' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_s %>% filter(node=='HI' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),   
      # SE
      rep(.78, Sankey3_s %>% filter(node=='SE' & !is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]),
      rep(2.1, Sankey3_s %>% filter(node=='SE' & is.na(next_node)) %>% filter(row_number()==1) %>% select(n2) %>% .[[1]]) ) ),   
      size = 6, color = 1, fill = "white", hjust=.25)  +
    scale_fill_manual(values = c("NR"="gray90",
                                 "NK"="#a6ad59",
                                 "LO"="#60a5a3",
                                 "PH"="#1b586c",
                                 "HI"="#233368",
                                 "SE"="#602d5e"),
                      name = "") +
    labs(title = 'Sopper', x = "") +
    theme_sankey(base_size = 16) +
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA),
          axis.text.x = element_text(size = 16),
          plot.margin = unit(c(0,-2.5,0,-2.5), 'cm'),
          plot.title = element_text(hjust = .5)) +
    coord_cartesian(clip = 'off')
}
p_sopp

# Kombiner plots (FUNKER DÅRLIG, IKKE BRUK)
ggarrange(p_bakterier, p_kromister, p_rundormer, p_sopp,
          labels = c('Bakterier','Eggsporesopper','Rundormer og flatormer','Sopper'),
          ncol=2, nrow=2)
