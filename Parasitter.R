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

# Les FAB eksport slik at vi kan sammenligne parasitter vs alle arter
alle <- read.csv2("eksportabsoluteall_20230309.csv")

##--- 1. RYDDING    ---####
##---     1.1 Parasitter  ---####
# Etter diskusjon med Olga, ta bort bakterier; den gruppen er gjennomarbeidet nok til at vi tør ha den med
parasitt <-  parasitt %>% 
  filter(Ekspertkomite != 'Bakterier')

# Samle etableringsklasse C3-E til én kategori
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
  # Risikokategori 2023 ; omdøp 'NR' til 'Ikke risikovurdert tidligere
  mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                   Kategori2018 != "NR" ~ Kategori2018) ) %>% 
  mutate(across(c(Kategori2023, Kategori2018),
                ~ordered(.x, levels = c("Ikke risikovurdert tidligere","NK","LO","PH","HI","SE")))) %>%   # OBS 'NR' er ikke lenger inkludert
  # Omdøp kromister
  mutate(Ekspertkomite = case_when(Ekspertkomite == 'Kromister' ~ 'Eggsporesopper',
                                   Ekspertkomite != 'Kromister' ~ Ekspertkomite))  %>% 
  # Omdøp Fremmedartsstatus
  mutate(Fremmedartsstatus = case_when(Fremmedartsstatus == 'Selvstendig reproduserende' ~ Fremmedartsstatus,
                                       Fremmedartsstatus != 'Selvstendig reproduserende' ~ 'Doerstokkart'))

# Lag ny kollonne til klimaeffekter for bedre visualisering
parasitt <- parasitt %>% 
  mutate(Klimaeffekter_comb = factor(
    case_when(KlimaeffekterInvasjonspotensial == 'yes' & KlimaeffekterOkologiskEffekt == 'yes' ~ 'IO',
              KlimaeffekterInvasjonspotensial == 'yes' & KlimaeffekterOkologiskEffekt != 'yes' ~ 'I',
              KlimaeffekterInvasjonspotensial != 'yes' & KlimaeffekterOkologiskEffekt == 'yes' ~ 'O',
              KlimaeffekterInvasjonspotensial != 'yes' & KlimaeffekterOkologiskEffekt != 'yes' ~ 'no',),
    levels = c('IO','I','O','no'), ordered = TRUE) )

##---     1.2 Alle  ---####
# Behold bare ferdigstilte arter
ferdig <- alle %>%
  filter(Vurderinsstatus == "finished") %>%
  filter(!Ekspertkomite == "Testedyr")

# Bruk denne for å sjekke om det er noe som er feil i datasettet - om tabellen er tom er alt greit
ferdig %>%
  select(Ekspertkomite, Vurderinsstatus,VitenskapeligNavn, NorskNavn, SistEndretAv,
         Kategori2018, Kategori2023, AarsakTilEndringIKategori) %>%
  filter(Kategori2023 == "" )

# Om ikke den er tom er noe feil - oftest har det vært en issue med at NR-arter hvor det ikke er tikket av for alt
# i vurderingen ikke blir tildelt 'NR' i Kategori2023 - kan fikses manuelt her, ellers gir det problemer senere
ferdig$Kategori2023[ferdig$Kategori2023==""] <- "NR"
##ferdig$Fremmedartsstatus[ferdig$Fremmedartsstatus==""] <- "Ikke fremmed"  # Dette var tidligere et issue

# Definer en bedre kategori for arter fra Horisontskanningen
ferdig$Kategori2018[ferdig$Kategori2018==""] <- "Ikke risikovurdert tidligere"
# Definer rette faktor-nivåer i rett rekkefølge
ferdig <- ferdig %>%
  # Risikokategorier
  mutate(across(c(Kategori2023, Kategori2018),
                ~ordered(.x, levels = c("Ikke risikovurdert tidligere","NR","NK","LO","PH","HI","SE")))) %>%
  # Fremmedartsstatus
  mutate(across(c(Fremmedartsstatus),
                ~ordered(.x, levels = c("Selvstendig reproduserende", "Regionalt fremmed", "Doerstokkart",
                                        "Effekt uten selvstendig reproduksjon", "Vurderes p\U00E5 et annet taksonomisk nivaa",
                                        "Etablert per 1800", "Feilbestemt i 2018", "Ikke definert", "Ikke fremmed")))) %>%
  # Etableringsklasse
  mutate(across(c(Etableringsklasse),
                ~ordered(.x, levels = c("A","B1","B2","C0","C1","C2","C3","D1","D2","E",""))))

# Samle etableringsklasse C3-E til én kategori
ferdig <- ferdig %>%
  mutate(Etableringsklasse_comb =  case_when(Etableringsklasse == "" ~ "Mangler",
                                             Etableringsklasse == "A" ~ "A",
                                             Etableringsklasse == "B1" ~ "B1",
                                             Etableringsklasse == "B2" ~ "B2",
                                             Etableringsklasse == "C0" ~ "C0",
                                             Etableringsklasse == "C1" ~ "C1",
                                             Etableringsklasse == "C2" ~ "C2",
                                             Etableringsklasse == "C3" | Etableringsklasse == "D1" | Etableringsklasse == "D2" | Etableringsklasse == "E" ~ "C3E"), .keep = "all") %>%
  mutate(across(c(Etableringsklasse_comb),
                ~ordered(.x, levels = c("A","B1","B2","C0","C1","C2","C3E","Mangler"))))

# Sjekk hvilke arter som har tom etableringsklasse - det kan her både være arter som er NR (rett), arter som ikke er
# fremmede (rett), eller arter hvor eksperten har glemt at tikke av endelig klasse
# under bakgrunnsdata (feil)
ferdig %>%
  filter(Etableringsklasse_comb == 'Mangler') %>%
  group_by(Fremmedartsstatus, Ekspertkomite) %>%
  tally()
# Se hvilke arter det er; filtrer ut NR-arter og de som ikke er fremmede - de er rette som de
# står og skal ikke ha en etableringsklasse
ferdig %>% filter(Etableringsklasse_comb == 'Mangler') %>%
  filter(Kategori2023 != 'NR',
         Fremmedartsstatus != 'Ikke fremmed') %>%    # Denne burde være dekket av 'NR', men tar den med for en sikkerhets skyld
  select(Ekspertkomite, VitenskapeligNavn, NorskNavn, Fremmedartsstatus,
         Etableringsklasse, Etableringsklasse_comb, EtablertPer1800,
         Kategori2018, Kategori2023) %>% 
  print()

# Alle her er enten 'Regionalt fremmede' eller 'selvstendig reproduserende/etablerte arter' hvor eksperten har glemt å
# hakke av for endelig kategori fanen for bakgrunnsdata. Vi kan overstyre og legge inn samlekategorien for etablerte arter:
ferdig[!(ferdig$Kategori2023=='NR' | ferdig$Fremmedartsstatus=='Ikke fremmed') &
         ferdig$Etableringsklasse_comb=='Mangler', 'Etableringsklasse_comb'] <- factor('C3E')

# Lag ny kollonne til klimaeffekter for bedre visualisering
ferdig <- ferdig %>% 
  mutate(Klimaeffekter_comb = factor(
    case_when(KlimaeffekterInvasjonspotensial == 'yes' & KlimaeffekterOkologiskEffekt == 'yes' ~ 'IO',
              KlimaeffekterInvasjonspotensial == 'yes' & KlimaeffekterOkologiskEffekt != 'yes' ~ 'I',
              KlimaeffekterInvasjonspotensial != 'yes' & KlimaeffekterOkologiskEffekt == 'yes' ~ 'O',
              KlimaeffekterInvasjonspotensial != 'yes' & KlimaeffekterOkologiskEffekt != 'yes' ~ 'no',),
    levels = c('IO','I','O','no'), ordered = TRUE) )
    

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

ggarrange(p_risikokategori,
          ggarrange(p_risikokategori_grupper, p_risikokategori_fremmedartsstatus,
                    ncol = 1, nrow = 2),
          ncol = 2, nrow = 1)

##---           2.1.1 Risikokategori, prosentdel  ---####
# For å sammenligne bedre med fordelingen blant andre grupper, lag et histogram med prosentdel i stedet
## Parasitter
p_risiko_prosent <- parasitt %>% 
  group_by(Kategori2023) %>% 
  summarise(total = n()) %>% 
  mutate(andel = total/sum(total)*100) %>%
  {
    ggplot(.,
           aes(x = Kategori2023, y = andel, fill = Kategori2023)) +
      geom_bar(color = 'black', stat = 'identity') +
      labs(x = "", y = "") +
      geom_text(x=.$Kategori2023, y=.$andel, aes(label=paste0(round(.$andel, digits = 1), ' %')), vjust=-.5, size = 6) +
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

p_risiko_prosent_alle <- ferdig %>% 
  filter(Kategori2023 != 'NR') %>% 
  group_by(Kategori2023) %>% 
  summarise(total = n()) %>% 
  mutate(andel = total/sum(total)*100) %>%
  {
    ggplot(.,
           aes(x = Kategori2023, y = andel, fill = Kategori2023)) +
      geom_bar(color = 'black', stat = 'identity') +
      labs(x = "", y = "") +
      geom_text(x=.$Kategori2023, y=.$andel, aes(label=paste0(round(.$andel, digits = 1), ' %')), vjust=-.5, size = 6) +
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

# Lag en ny dataframe for å lage et grouped barplot
andel <- rbind(parasitt %>% 
                 group_by(Kategori2023) %>% 
                 summarise(total = n()) %>% 
                 mutate(andel = total/sum(total)*100,
                        gruppe = 'parasitt') ,
               
               ferdig %>% 
                 filter(Kategori2023 != 'NR') %>% 
                 group_by(Kategori2023) %>% 
                 summarise(total = n()) %>% 
                 mutate(andel = total/sum(total)*100,
                        gruppe = 'alle')) %>% 
  arrange(Kategori2023, gruppe)  # For å sikre rett rekkefølge av verdier i plottet

p_risiko_samlet <- andel %>% {
  ggplot(.,
         aes(x = Kategori2023, y = andel, group = gruppe, fill = Kategori2023, alpha = gruppe)) +
    geom_bar(color = 'black', stat = 'identity', position = 'dodge') +
    labs(x = "", y = "") +
    geom_text(x=.$Kategori2023, y=.$andel,
              aes(label=paste0(round(.$andel, digits = 1), ' %')),
              vjust=-.5, hjust = rep(c(1,-.25),5), alpha = 1, color = 'black', size = 6) +
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
    scale_alpha_manual(values=c(.5, 1),
                       labels =c('Alle arter', 'Usynlige parasitter')) +
    theme_minimal(base_size = 16) +
    theme(legend.position=c(.85,.9),
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 16),
          plot.margin = unit(c(.5,0,-.5,0), 'cm'),
          strip.background = element_rect(fill = 'gray90'),
          legend.spacing.y = unit(.5, 'cm')) +
    guides(fill = 'none',
           alpha = guide_legend(byrow = TRUE))
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
          theme(legend.position = "none", # her fjerner jeg label; innset ev. "right"
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
      theme(legend.position = "right",   # her fjerner jeg label; innset ev. "right"
            legend.text = element_text(size = 16),
            legend.spacing.y = unit(.75, 'cm'),
            plot.margin = unit(c(0,0,0,0), 'cm'),
            strip.background = element_rect(fill = 'gray90'))  +
      facet_wrap(~ Ekspertkomite, ncol=1, nrow=3) + 
      guides(fill = guide_legend(byrow = TRUE))
  }

ggarrange(p_kake, p_kake_grupper, ncol = 2)


##---       2.3 Endring i kategori  ---####

# Nødvendig med identiske risikokategorier begge år; bruk ev. følgende for å være sikker
parasitt <- parasitt %>% 
  mutate(across(c(Kategori2023, Kategori2018),
              ~ordered(.x, levels = c("Ikke risikovurdert tidligere","NK","LO","PH","HI","SE"))))
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
    mutate(Kategori2018 =  case_when(Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert \ntidligere",   # kombiner NR og arter fra HS ble gjort i rddingen
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
    scale_fill_manual(values = c("Ikke risikovurdert \ntidligere"="gray50",
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
# Bakterier ; UTGÅTT FRA ANALYSENE
{
  # Step 1
  Sankey1_b <- parasitt %>%
    filter(Ekspertkomite == 'Bakterier') %>% 
    mutate(Kategori2018 =  case_when(Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert \ntidligere",  
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
    scale_fill_manual(values = c("Ikke risikovurdert \ntidligere"="gray50",
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
    mutate(Kategori2018 =  case_when(Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert \ntidligere",   # kombiner NR og arter fra HS
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
    scale_fill_manual(values = c("Ikke risikovurdert \ntidligere"="gray50",
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
    mutate(Kategori2018 =  case_when(Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert \ntidligere",   # kombiner NR og arter fra HS
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
    scale_fill_manual(values = c("Ikke risikovurdert \ntidligere"="gray50",
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
    mutate(Kategori2018 =  case_when(Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert \ntidligere",   # kombiner NR og arter fra HS
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
    scale_fill_manual(values = c("Ikke risikovurdert \ntidligere"="gray50",
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
ggarrange( p_kromister, p_rundormer, p_sopp,
          #labels = c('Eggsporesopper','Rundormer og flatormer','Sopper'),
          ncol=2, nrow=2)

##---       2.4 Artsgrupper ---####
# (ikke noe egentlig interessant statistikk her, bare en oversikt over hvilke arts-/ekspertgrupper som er inkludert)
# Selv om det ikke er statistisk bra, er et kakediagram pent her

p_grupper <- parasitt %>%
  group_by(Ekspertkomite) %>%
  tally() %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(desc(Ekspertkomite)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%  {
    ggplot(., aes(x = "", y = prop, fill = Ekspertkomite)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0, clip = 'off')+
      geom_text(aes(y = lab.ypos, x=1.3, label = n), color = "white", size = 6)+
      scale_fill_manual(values = c("Sopper"="#35a3b2",
                                   "Rundormer og flatormer"="#71B581",
                                   "Eggsporesopper"="#d2c160",
                                   "Bakterier"="#936649"),
                        labels = c("Sopper" = "Sopper",
                                   "Rundormer og flatormer" = "Rundormer og \nflatormer",
                                   "Eggsporesopper" = "Eggsporesopper",
                                   "Bakterier" = "Bakterier"),
                        name = '') +
      theme_void(base_size = 16) +
      theme(legend.position = "none",
            legend.text = element_text(size = 16),
            legend.spacing.y = unit(.75, 'cm'),
            plot.margin = unit(c(0,0,0,0), 'cm'))  +
      guides(fill = guide_legend(byrow = TRUE))
  }

p_grupper_status <- parasitt %>%
  group_by(Fremmedartsstatus, Ekspertkomite) %>%
  tally() %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(desc(Ekspertkomite)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%  {
    ggplot(., aes(x = "", y = prop, fill = Ekspertkomite)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0, clip = 'off')+
      geom_text(aes(y = lab.ypos, x=1.3, label = n), color = "white", size = 6)+
      scale_fill_manual(values = c("Sopper"="#35a3b2",
                                   "Rundormer og flatormer"="#71B581",
                                   "Eggsporesopper"="#d2c160",
                                   "Bakterier"="#936649"),
                        labels = c("Sopper" = "Sopper",
                                   "Rundormer og flatormer" = "Rundormer og \nflatormer",
                                   "Eggsporesopper" = "Eggsporesopper",
                                   "Bakterier" = "Bakterier"),
                        name = '') +
      theme_void(base_size = 16) +
      theme(legend.position = "right",
            legend.text = element_text(size = 16),
            legend.spacing.y = unit(.75, 'cm'),
            plot.margin = unit(c(0,0,0,0), 'cm'),
            strip.background = element_rect(fill = 'gray90'))  +
      facet_wrap(~ Fremmedartsstatus, nrow = 2) + 
      guides(fill = guide_legend(byrow = TRUE))
  }

ggarrange(p_grupper, p_grupper_status, ncol = 2)


##---       2.5 Matriseplot ---####
# Nødvendig med identiske risikokategorier begge år og et kortere navn til de som ikke er risikovurdert
cont <- parasitt %>% 
  mutate(across(c(Kategori2023, Kategori2018),
                ~as.character(.x))) %>% 
  mutate(across(c(Kategori2023, Kategori2018),
                ~case_when(.x =="Ikke risikovurdert tidligere" ~ "NR",
                           .x != "Ikke risikovurdert tidligere" ~ .x) ) ) %>% 
  mutate(across(c(Kategori2023, Kategori2018),
                ~ordered(.x, levels = c("NR","NK","LO","PH","HI","SE")))) %>% 
  # Lag contingency table med verdier
  #droplevels() %>%  # Funker ikke om denne er med pga tomme kategorier
  group_by(Kategori2018, Kategori2023) %>%
  tally() %>%
  as.data.frame() %>%
  complete(Kategori2018, Kategori2023, fill = list(n = 0)) %>%
  mutate(farge = case_when(Kategori2023 == Kategori2018 ~ '0',     # Legg til bakgrunnsfarger iht. grad av endring
                           (Kategori2018 == 'NK' & Kategori2023 == 'LO') | (Kategori2018 == 'LO' & Kategori2023 == 'PH') | (Kategori2018 == 'PH' & Kategori2023 == 'HI') | (Kategori2018 == 'HI' & Kategori2023 == 'SE') ~ '1+',
                           (Kategori2018 == 'NK' & Kategori2023 == 'PH') | (Kategori2018 == 'LO' & Kategori2023 == 'HI') | (Kategori2018 == 'PH' & Kategori2023 == 'SE') ~ '2+',
                           (Kategori2018 == 'NK' & Kategori2023 == 'HI') | (Kategori2018 == 'LO' & Kategori2023 == 'SE') ~ '3+',
                           (Kategori2018 == 'NK' & Kategori2023 == 'SE') ~ '4+',
                           (Kategori2023 == 'NK' & Kategori2018 == 'LO') | (Kategori2023 == 'LO' & Kategori2018 == 'PH') | (Kategori2023 == 'PH' & Kategori2018 == 'HI') | (Kategori2023 == 'HI' & Kategori2018 == 'SE') ~ '1-',
                           (Kategori2023 == 'NK' & Kategori2018 == 'PH') | (Kategori2023 == 'LO' & Kategori2018 == 'HI') | (Kategori2023 == 'PH' & Kategori2018 == 'SE') ~ '2-',
                           (Kategori2023 == 'NK' & Kategori2018 == 'HI') | (Kategori2023 == 'LO' & Kategori2018 == 'SE') ~ '3-',
                           (Kategori2023 == 'NK' & Kategori2018 == 'SE') ~ '4-',
                           Kategori2018 == 'NR' ~ 'NR')) 

xtabs(cont$n ~ cont$Kategori2018 + cont$Kategori2023)

cont %>% 
  {
ggplot( ., aes(x = Kategori2018, y = Kategori2023)) +
  geom_tile(aes(fill = farge), alpha=.9, color = 'black') +
  geom_text(aes(label = n), size = 6) +
  scale_fill_manual(values = c('0'='gray80',
                               '1+'='#FFFF92',
                               "2+"="#FFE778",
                               "3+"="#FFCE5F",
                               '4+'="#e5b445",
                               '1-'="#82F0FF",
                               '2-'="#68D6E5",
                               '3-'="#4FBDCC",
                               '4-'="#35a3b2",
                               "NR"="gray60",
                               "NK"="#a6ad59",
                               'LO'="#60a5a3",
                               'PH'="#1b586c",
                               'HI'="#233368",
                               'SE'="#602d5e"), na.value = 'white') +
  labs(x = 'Kategori 2018', y = 'Kategori 2023') +
  geom_label(aes(0.3, Kategori2023, label = Kategori2023, fill=Kategori2023), color='white', hjust = -.001, size = 6) +
  geom_label(aes(Kategori2018, 0.5, label = Kategori2018, fill=Kategori2018), color='white', vjust = .75, size = 6) +
  theme_minimal(base_size = 16) +
  theme(legend.position = 'none',
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10), vjust = 3, size =16),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10), vjust = -3, size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())  +
  coord_cartesian(clip = "off")
  }





##---       2.6 Klimaeffekter ---####
parasitt %>%
  {
    ggplot(.,
           aes(x = Klimaeffekter_comb, fill = Klimaeffekter_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.5, size = 6) +
      scale_x_discrete( #drop=FALSE,
        labels = c("IO" = "Invasjonspotensiale og\n\U00D8kologisk effekt",
                   "I" = "Invasjonspotensiale",
                   "O" = "\U00D8kologisk effekt",
                   "no" = "Ingen/ukjent effekt\nav klimaendringer")) +
      scale_fill_manual(values = c("IO"="#e5b445",
                                   "I"="#A0BA5B",
                                   "O"="#5FB7B1",
                                   "no"="gray60"))  +
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

ferdig %>%
  filter(!(Kategori2023 == 'NR' )) %>%    # Inkluderer fortsatt NK
  {
    ggplot(.,
           aes(x = Klimaeffekter_comb, fill = Klimaeffekter_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.5, size = 6) +
      scale_x_discrete( #drop=FALSE,
        labels = c("IO" = "Invasjonspotensiale og\n\U00D8kologisk effekt",
                   "I" = "Invasjonspotensiale",
                   "O" = "\U00D8kologisk effekt",
                   "no" = "Ingen/ukjent effekt\nav klimaendringer")) +
      scale_fill_manual(values = c("IO"="#e5b445",
                                   "I"="#A0BA5B",
                                   "O"="#5FB7B1",
                                   "no"="gray60"))  +
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

# Lag en ny dataframe for å lage et grouped barplot
andel_klima <- rbind(parasitt %>% 
                 group_by(Klimaeffekter_comb) %>% 
                 summarise(total = n()) %>% 
                 mutate(andel = total/sum(total)*100,
                        gruppe = 'parasitt') ,
               
               ferdig %>% 
                 filter(Kategori2023 != 'NR') %>%   # Ikke relevant
                 group_by(Klimaeffekter_comb) %>% 
                 summarise(total = n()) %>% 
                 mutate(andel = total/sum(total)*100,
                        gruppe = 'alle')) %>% 
  arrange(Klimaeffekter_comb, gruppe)

p_klima_samlet <- andel_klima %>% {
  ggplot(.,
         aes(x = Klimaeffekter_comb, y = andel, group = gruppe, fill = Klimaeffekter_comb, alpha = gruppe)) +
    geom_bar(color = 'black', stat = 'identity', position = 'dodge') +
    labs(x = "", y = "") +
    geom_text(x=.$Klimaeffekter_comb, y=.$andel,
              aes(label=paste0(round(.$andel, digits = 1), ' %')),
              vjust=-.5, hjust = rep(c(1,-.25),4), alpha = 1, color = 'black', size = 6) +
    scale_x_discrete( #drop=FALSE,
      labels = c("IO" = "Invasjonspotensiale og\n\U00D8kologisk effekt",
                 "I" = "Invasjonspotensiale",
                 "O" = "\U00D8kologisk effekt",
                 "no" = "Ingen/ukjent effekt\nav klimaendringer")) +
    scale_fill_manual(values = c("IO"="#e5b445",
                                 "I"="#A0BA5B",
                                 "O"="#5FB7B1",
                                 "no"="gray60"))  +
    scale_alpha_manual(values=c(.5, 1),
                       labels =c('Alle arter', 'Usynlige parasitter')) +
    theme_minimal(base_size = 16) +
    theme(legend.position=c(.5,.75),
          legend.text = element_text(size = 16),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.box.background = element_rect(colour = "black"),
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA),
          panel.grid = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_text(size = 16),
          plot.margin = unit(c(.5,0,-.5,0), 'cm'),
          strip.background = element_rect(fill = 'gray90'),
          legend.spacing.y = unit(.5, 'cm')) +
    guides(fill = 'none',
           alpha = guide_legend(byrow = TRUE))
}
