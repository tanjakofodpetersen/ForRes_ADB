###--------------------------------------------------------####
###--         FORELOEPIGE RESULTATER FRA FAL            ---####
###--------------------------------------------------------####

library(tidyverse)
library(ggplot2)
library(data.table)
library(ggsankey)

getwd()
setwd("C:/Users/TKP/OneDrive - artsdatabanken.no/Dokumenter/ForRes_ADB/20230216")

# Les eksportfilen fra mappe; oppdater til nyeste filen
### OBS! Rett æ, ø og å før filen leses
alle_arter <- read.csv2("eksportabsoluteall.csv")

###--   1. RYDDING  ---####

# Behold bare ferdigstilte arter
ferdig <- alle_arter %>%
  filter(Vurderinsstatus == "finished") %>%
  filter(!Ekspertkomite == "Testedyr")

# Bruk denne for å sjekke om det er noe som er feil i datasettet - om tabellen er tom er alt greit
ferdig %>%
  select(Ekspertkomite, Vurderinsstatus,VitenskapeligNavn, NorskNavn, SistEndretAv,
         Kategori2018, Kategori2023, AarsakTilEndringIKategori) %>%
  filter(Kategori2023 == "" )

# Definer rette faktor-nivåer i rett rekkefølge
##ferdig$Kategori2023[ferdig$Kategori2023==""] <- "NR"
##ferdig$Fremmedartsstatus[ferdig$Fremmedartsstatus==""] <- "Ikke fremmed"
ferdig$Kategori2018[ferdig$Kategori2018==""] <- "Ikke risikovurdert tidligere"
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

##---       1.1 Split utslagsgivende kriterier  --####
# Split utslagsgivende kriterier i hhv. Invasjonspotensiale og Økologisk effekt, samt samle til AxB 
ferdig <- ferdig %>%
  separate(Kriterier2023, into = c('invasjonspotensiale', 'effekt'),
           sep = ",", extra = "merge", fill = "right",
           remove = FALSE) %>%
  separate(invasjonspotensiale, into = c("invScore", "invKrit"),
           sep = 1, extra = "merge", fill = "right",
           remove = FALSE) %>%
  mutate(invKrit_comb =  case_when(invKrit =="A" | invKrit == "B" | invKrit == "AB" ~ "AxB",
                                   invKrit == "C" ~ "C",
                                   invasjonspotensiale == 1 ~ "Lite invasjonspotensiale"), .keep = "all") %>%
  separate(effekt, into = c("effektScore", "effektKrit"),
           sep = 1, extra = "merge", fill = "right",
           remove = FALSE) %>%
  # Split økologisk effekt igjen for å gjøre det lettere konvertere til long-format
  separate(effektKrit, into = c("effektKrit1", "effektKrit2", "effektKrit3"),
           sep = c(1,2), extra = "merge", fill = "right",
           remove = FALSE)

ferdig$effektKrit[ferdig$effektKrit==""] <- "Ingen effekt"

##---       1.2 Long-format for økologisk effekt og Årsak til endring uten kombinerte kategorier  ---####
# Konverter til long-format mtp effekt-kriteriene
ferdig_long.effekt <- ferdig %>%
  # For å sikre at "ingen effekt" bare telles én gang, innsett denne som effektKrit1 om det er relevant
  mutate(effektKrit1 = case_when(effektKrit == 'Ingen effekt' ~ 'Ingen effekt',
                                 effektKrit1 %in% c('D','E','F','G','H','I') ~ effektKrit1)) %>%
  pivot_longer(c("effektKrit1", "effektKrit2", "effektKrit3"),
               names_to = "effektKritNo",
               values_to = "effektKritLong")  %>%
  # Fjern tomme rekker/ingen effekt
  filter(effektKritLong %in% c("D","E","F","G","H","I","Ingen effekt"))


# Split kolonnen for Årsak til endring i kategori så det ikke finnes kombinerte kategorier
ferdig_long.endring <- ferdig %>%
  separate_rows(AarsakTilEndringIKategori, sep = "; ") %>%
  # Omdøp og kombiner 'Ny kunnskap' og 'Reell endring'
  mutate(Aarsak_norsk =  case_when(AarsakTilEndringIKategori =="realChange" | AarsakTilEndringIKategori =="newInformation"  ~ "Reell endring",
                                   #AarsakTilEndringIKategori =="newInformation"  ~ "Ny kunnskap",
                                   AarsakTilEndringIKategori =="newInterpretation"  ~ "Ny tolkning av data",
                                   AarsakTilEndringIKategori =="changedCriteria"  ~ "Endrede avgrensninger/retningslinjer",
                                   AarsakTilEndringIKategori =="changedCriteriaInterpretation"  ~ "Endret tolkning av retningslinjer",
                                   AarsakTilEndringIKategori =="changedStatus"  ~ "Endret status"), .keep = "all")


##---       1.3 Samle etableringsklasse C3-E til én kategori ---####
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

# Enkelte eksperter har ikke fylt ut korrekt for Etableringsklasse (især for sopp og karplanter); det betyr at alle arter som ikke er NR
# med manglende etableringsklasse faktisk er etablerte (usikkert om det er C3, D1, D2 eller E). Legg derfor til den endringen manuelt
ferdig[!ferdig$Kategori2023=='NR' & ferdig$Etableringsklasse_comb=='Mangler', 'Etableringsklasse_comb'] <- factor('C3E')


##------------------------------------------------------------------------------------------------####
##---   2. PLOTS  ---####
##---     2.1 Alle arter  ---####
##---         2.1.1 Risikokategori  ---####
ferdig %>%
  filter(Kategori2023 != "NR") %>%  # Fjern 'NR' fra plots
  {
    ggplot(.,
           aes(x = Kategori2023, fill = Kategori2023)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
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
        "SE"="#602d5e")) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 16))
  }
ggsave('alleArter/risikokategori.png', bg='transparent')


##---         2.1.2 Etableringsklasse ---####
ferdig %>%
  filter(!Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  {
    ggplot(.,
           aes(x = Etableringsklasse_comb, fill = Etableringsklasse_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
      geom_segment(aes(x = 'A', xend = 'C1', y = 925, yend = 925),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'B2', y = 950, label='D\U00F8rstokkarter'), size=4) +
      geom_segment(aes(x = 'C2', xend = 'C3E', y = 925, yend = 925),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'C2', y = 950, label='Selvstendig reproduserende'), size=4, hjust=-.01) +
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#5FB7B1",
                                   "B2"="#71B581",
                                   "C0"="#A0BA5B",
                                   "C1"="#d2c160",
                                   "C2"="#e5b445",
                                   "C3E"="#936649")) +
      scale_x_discrete(labels = c("A" = "Forekommer ikke i Norge",
                                  "B1" = "Forekommer innend\U00F8rs eller \ni lukkede installasjoner",
                                  "B2" = "Forekommer utend\U00F8rs p\U00E5 \neget produksjonsareal",
                                  "C0" = "Dokumentert i norsk natur",
                                  "C1" = "Overlever vinteren utend\U00F8rs \nuten menneskelig tilsyn",
                                  "C2" = "Selvstendig reproduserende",
                                  "C3E" = "Etablert i norsk natur")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .8, size =12)) 
  }
ggsave('alleArter/etableringsklasse.png', bg='transparent')

### Som kakediagram
# Dette er ikke anbefalt å gjøre, og har derfor ingen direkte pakker til det - derfor må det gjøres noe krumspring for å få til
ferdig %>%
  filter(!Kategori2023 == "NR") %>% 
  group_by(Etableringsklasse_comb) %>%
  tally() %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(desc(Etableringsklasse_comb)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%  {
    ggplot(., aes(x = "", y = prop, fill = Etableringsklasse_comb)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = n), color = "white", size = 5)+
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
      theme_void() +
      theme(legend.position = "right",
            legend.text = element_text(size = 10),
            legend.spacing.y = unit(.75, 'cm'))  +
      guides(fill = guide_legend(byrow = TRUE))
  }
ggsave('alleArter/etableringsklasse_kake.png', bg='transparent')


##---         2.1.3 Aarsak til endring  ---####
ferdig_long.endring %>%
  filter(Aarsak_norsk != "",
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>% {
    ggplot(.,
           aes(x = Aarsak_norsk, fill = Aarsak_norsk)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c("Endrede avgrensninger/retningslinjer"="#35a3b2",
                                   "Endret status"="#5FB7B1",
                                   "Endret tolkning av retningslinjer"="#71B581",
                                   #"Ny kunnskap"="#A0BA5B",
                                   "Ny tolkning av data"="#d2c160",
                                   "Reell endring"="#e5b445")) +
      scale_x_discrete(labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger\ni retningslinjene",
                                  "Endret status"="Endret status",
                                  "Endret tolkning av\nretningslinjer"="Endret tolkning av retningslinjer",
                                  #"Ny kunnskap"="Ny kunnskap",
                                  "Ny tolkning av data"="Ny tolkning av data",
                                  "Reell endring"="Reell endring")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .9, size = 12))
  }
ggsave('alleArter/aarsakEndring.png', bg='transparent')

##---             2.1.3.1 Aarsak til endring; oppsummering  ---####
# Plot over antall arter med x årsaker til endring i risikokategori
ferdig_long.endring %>%
  filter(Aarsak_norsk != "",
         Kategori2023 != 'NR',  # Ta bort NR-arter
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn) %>% 
  tally(name = 'antallAarsaker') %>% 
  {
    ggplot(., aes(x = factor(antallAarsaker), fill = factor(antallAarsaker))) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +   # Bruk ev. x = "Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c('#A0BA5B', '#d2c160', '#e5b445')) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('alleArter/aarsakEndring_antallTrinn.png', bg='transparent')

# Samme plot som over, men inkluder bare arter hvor "Endret tolkning av retningslinjer" inngår som én av årsakene
ferdig_long.endring %>%
  filter(Aarsak_norsk != "",
         Kategori2023 != 'NR',  # Ta bort NR-arter
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn) %>%
  # Filtrer ut arter som ikke har Endret tolkning av retningslinjer", og transformer tilbake til long-format
  mutate(antall = 1) %>% 
  pivot_wider(names_from = Aarsak_norsk, values_from = antall) %>% 
  filter(`Endret tolkning av retningslinjer` == 1) %>% 
  pivot_longer(cols =c(`Reell endring`, `Endret tolkning av retningslinjer`,
               `Ny tolkning av data`, `Endrede avgrensninger/retningslinjer`, `Endret status`), names_to = 'Aarsak_norsk') %>% 
  filter(!is.na(value)) %>% 
  tally(name = 'antallAarsaker') %>% 
  {
    ggplot(., aes(x = factor(antallAarsaker), fill = factor(antallAarsaker))) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +   # Bruk ev. x = "Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c('#A0BA5B', '#d2c160', '#e5b445')) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('alleArter/aarsakEndring_antallTrinn_endretTolkning.png', bg='transparent')


##---         2.1.4 Endring i kategori  ---####

# Versjon med kategorier skrevet helt ut:
{
  ferdig %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "Ingen kjent risiko (NK)",
                                     Kategori2018 == "LO" ~ "Lav risiko (LO)",
                                     Kategori2018 == "PH" ~ "Potensielt h\U00F8y risiko (PH)",
                                     Kategori2018 == "HI" ~ "H\U00F8y risiko (HI)",
                                     Kategori2018 == "SE" ~ "Sv\U00E6rt h\U00F8y risiko (SE)"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert (NR)",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "Ingen kjent risiko (NK)",
                                     Kategori2023 == "LO" ~ "Lav risiko (LO)",
                                     Kategori2023 == "PH" ~ "Potensielt h\U00F8y risiko (PH)",
                                     Kategori2023 == "HI" ~ "H\U00F8y risiko (HI)",
                                     Kategori2023 == "SE" ~ "Sv\U00E6rt h\U00F8y risiko (SE)")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
                                          "Ikke risikovurdert (NR)",
                                          "Ingen kjent risiko (NK)",
                                          "Lav risiko (LO)",
                                          "Potensielt h\U00F8y risiko (PH)",
                                          "H\U00F8y risiko (HI)",
                                          "Sv\U00E6rt h\U00F8y risiko (SE)")))) %>% {
                                            ggplot(., aes(x = x, 
                                                          next_x = next_x, 
                                                          node = node, 
                                                          next_node = next_node,
                                                          label = node,
                                                          fill = node )) +
                                              geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
                                              geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                                              scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70",
                                                                           "Ikke risikovurdert (NR)"="gray90",
                                                                           "Ingen kjent risiko (NK)"="#a6ad59",
                                                                           "Lav risiko (LO)"="#60a5a3",
                                                                           "Potensielt h\U00F8y risiko (PH)"="#1b586c",
                                                                           "H\U00F8y risiko (HI)"="#233368",
                                                                           "Sv\U00E6rt h\U00F8y risiko (SE)"="#602d5e"),
                                                                name = "") +
                                              labs(x = "") +
                                              theme_sankey(base_size = 16) +
                                              theme(legend.position="none",
                                                    panel.background = element_rect(fill='transparent', color = NA),
                                                    plot.background = element_rect(fill='transparent', color=NA))
                                          }
}

# Versjon med forkortet kategori
{
  ferdig %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
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
                                              geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                                              scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70",
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
                                                    plot.background = element_rect(fill='transparent', color=NA))
                                          }
}
ggsave('alleArter/endring.png', bg='transparent')

# Med verdier
{
  # Step 1
  Sankey1 <- ferdig %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
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
  
  # Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
  ggplot(Sankey3, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = node,
                      label = paste0(node,",\nn=", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(rep(.78,987),    # Ikke risikovurdert tidligere
                                rep(2.1,201),  # NR
                                rep(.78,443), rep(2.1,334),   # NK
                                rep(.78,736), rep(2.1,1171),  # LO
                                rep(.78,103), rep(2.1,373),  # PH
                                rep(.78,108),rep(2.1,202),  # HI
                                rep(.78,132), rep(2.1,228))),  # SE)),
                      size = 3.5, color = 1, fill = "white") +
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
          plot.background = element_rect(fill='transparent', color=NA))
  
  rm(Sankey1, Sankey2, Sankey3)
}
ggsave('alleArter/endring_verdier.png', bg='transparent')

## Bare reviderte arter; fjern "Ikke risikovurdert tidligere" fra 2018-siden
{
  ferdig %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    filter(Kategori2018 != 'Ikke risikovurdert tidligere') %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("NR",
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
                                              geom_sankey_label(size = 3.5, color = 1, fill = "white") +
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
                                                    plot.background = element_rect(fill='transparent', color=NA))
                                          }
}
ggsave('alleArter/endring_reviderteArter.png', bg='transparent')

# Med verdier
{
  # Step 1
  Sankey1 <- ferdig %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    filter(Kategori2018 != 'Ikke risikovurdert tidligere') %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("NR",
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
  
  # Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
  ggplot(Sankey3, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = node,
                      label = paste0(node,",\nn=", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(#rep(.78,965),    # Ikke risikovurdert tidligere
      rep(2.1,27),  # NR
      rep(.78,443), rep(2.1,162),   # NK
      rep(.78,736), rep(2.1,786),  # LO
      rep(.78,103), rep(2.1,236),  # PH
      rep(.78,108), rep(2.1,139),  # HI
      rep(.78,132), rep(2.1,172))),  # SE)),
      size = 3.5, color = 1, fill = "white") +
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
          plot.background = element_rect(fill='transparent', color=NA))
  
  rm(Sankey1, Sankey2, Sankey3)
}
ggsave('alleArter/endring_reviderteArter_verdier.png', bg='transparent')


##---         2.1.5 Matrise-plot ---####

# Lag contingency table med verdier

cont <- ferdig %>%
  filter(!Kategori2018 == 'Ikke risikovurdert tidligere') %>%  
  droplevels() %>%
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
                           (Kategori2023 == 'NK' & Kategori2018 == 'SE') ~ '4-')) 

xtabs(cont$n ~ cont$Kategori2018 + cont$Kategori2023)

ggplot(cont, aes(x = Kategori2018, y = Kategori2023)) +
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
                               'N'='white',
                               "NR"="gray80",
                               "NK"="#a6ad59",
                               'LO'="#60a5a3",
                               'PH'="#1b586c",
                               'HI'="#233368",
                               'SE'="#602d5e"), na.value = 'white') +
  labs(x = 'Kategori 2018', y = 'Kategori 2023') +
  geom_label(aes(0.3, Kategori2023, label = Kategori2023, fill=Kategori2023), color='white', hjust = -.25, size = 5) +
  geom_label(aes(Kategori2018, 0.5, label = Kategori2018, fill=Kategori2018), color='white', vjust = .35, size = 5) +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10), vjust = 3, size =12),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10), vjust = -3, size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave('alleArter/endring_matrise.png', bg='transparent')


##---     2.2 Karplanter  ---####
##---         2.2.1 Risikokategori  ---####
ferdig %>%
  filter(Ekspertkomite == "Karplanter",
         Kategori2023 != "NR" ) %>%   # Fjern 'NR' fra plots
  {
    ggplot(.,
           aes(x = Kategori2023, fill = Kategori2023)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
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
        "SE"="#602d5e")) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 16))
  }
ggsave('karplanter/risikokategori.png', bg='transparent')

##---         2.2.2 Etableringsklasse ---####
ferdig %>%
  filter(Ekspertkomite == "Karplanter",
         !Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  {
    ggplot(.,
           aes(x = Etableringsklasse_comb, fill = Etableringsklasse_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
      geom_segment(aes(x = 'A', xend = 'C1', y = 650, yend = 650),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'B2', y = 675, label='D\U00F8rstokkarter'), size=4) +
      geom_segment(aes(x = 'C2', xend = 'C3E', y = 650, yend = 650),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'C2', y = 675, label='Selvstendig reproduserende'), size=4, hjust=-.01) +
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#5FB7B1","B2"="#71B581",
                                   "C0"="#A0BA5B", "C1"="#d2c160", "C2"="#e5b445",
                                   "C3E"="#936649")) +
      scale_x_discrete(labels = c("A" = "Forekommer ikke i Norge",
                                  "B1" = "Forekommer innend\U00F8rs eller \ni lukkede installasjoner",
                                  "B2" = "Forekommer utend\U00F8rs p\U00E5 \neget produksjonsareal",
                                  "C0" = "Dokumentert i norsk natur",
                                  "C1" = "Overlever vinteren utend\U00F8rs \nuten menneskelig tilsyn",
                                  "C2" = "Selvstendig reproduserende",
                                  "C3E" = "Etablert i norsk natur")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .8, size = 12)) 
  }
ggsave('karplanter/etableringsklasse.png', bg='transparent')

### Som kakediagram
# Dette er ikke anbefalt å gjøre, og har derfor ingen direkte pakker til det - derfor må det gjøres noe krumspring for å få til
ferdig %>%
  filter(Ekspertkomite == "Karplanter",
         !Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  group_by(Etableringsklasse_comb) %>%
  tally() %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(desc(Etableringsklasse_comb)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%  {
    ggplot(., aes(x = "", y = prop, fill = Etableringsklasse_comb)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = n), color = "white", size = 5)+
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
      theme_void() +
      theme(legend.position = "right",
            legend.text = element_text(size = 10),
            legend.spacing.y = unit(.75, 'cm'))  +
      guides(fill = guide_legend(byrow = TRUE))
  }
ggsave('karplanter/etableringsklasse_kake.png', bg='transparent')


##---         2.2.3 Aarsak til endring  ---####
ferdig_long.endring %>%
  filter(Ekspertkomite == "Karplanter",
         Aarsak_norsk != "",
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>% {
    ggplot(.,
           aes(x = Aarsak_norsk, fill = Aarsak_norsk)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c("Endrede avgrensninger/retningslinjer"="#35a3b2",
                                   "Endret status"="#5FB7B1",
                                   "Endret tolkning av retningslinjer"="#71B581",
                                   #"Ny kunnskap"="#A0BA5B",
                                   "Ny tolkning av data"="#d2c160",
                                   "Reell endring"="#e5b445")) +
      scale_x_discrete(labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger\ni retningslinjene",
                                  "Endret status"="Endret status",
                                  "Endret tolkning av\nretningslinjer"="Endret tolkning av retningslinjer",
                                  #"Ny kunnskap"="Ny kunnskap",
                                  "Ny tolkning av data"="Ny tolkning av data",
                                  "Reell endring"="Reell endring")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .9, size = 12)) 
  }
ggsave('karplanter/aarsakEndring.png', bg='transparent')


##---             2.2.3.1 Aarsak til endring; oppsummering  ---####
# Plot over antall arter med x årsaker til endring i risikokategori
ferdig_long.endring %>%
  filter(Ekspertkomite == 'Karplanter',
         Aarsak_norsk != "",
         Kategori2023 != 'NR',  # Ta bort NR-arter
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn) %>% 
  tally(name = 'antallAarsaker') %>% 
  {
    ggplot(., aes(x = factor(antallAarsaker), fill = factor(antallAarsaker))) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +   # Bruk ev. x = "Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c('#A0BA5B', '#d2c160', '#e5b445')) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('karplanter/aarsakEndring_antallTrinn.png', bg='transparent')


# Samme plot som over, men inkluder bare arter hvor "Endret tolkning av retningslinjer" inngår som én av årsakene
ferdig_long.endring %>%
  filter(Ekspertkomite == 'Karplanter',
         Aarsak_norsk != "",
         Kategori2023 != 'NR',  # Ta bort NR-arter
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn) %>%
  # Filtrer ut arter som ikke har Endret tolkning av retningslinjer", og transformer tilbake til long-format
  mutate(antall = 1) %>% 
  pivot_wider(names_from = Aarsak_norsk, values_from = antall) %>% 
  filter(`Endret tolkning av retningslinjer` == 1) %>% 
  pivot_longer(cols =c(`Reell endring`, `Endret tolkning av retningslinjer`,
                       `Ny tolkning av data`, `Endrede avgrensninger/retningslinjer`, `Endret status`), names_to = 'Aarsak_norsk') %>% 
  filter(!is.na(value)) %>% 
  tally(name = 'antallAarsaker') %>% 
  {
    ggplot(., aes(x = factor(antallAarsaker), fill = factor(antallAarsaker))) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +   # Bruk ev. x = "Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c('#A0BA5B', '#d2c160', '#e5b445')) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('karplanter/aarsakEndring_antallTrinn_endretTolkning.png', bg='transparent')

##---         2.2.4 Endring i kategori  ---####
{
  ferdig %>%
    filter( Ekspertkomite =="Karplanter") %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
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
                                              geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                                              scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70",
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
                                                    plot.background = element_rect(fill='transparent', color=NA))
                                          }
}
ggsave('karplanter/endring.png', bg='transparent')

# Med verdier
{
  # Step 1
  Sankey1 <- ferdig %>%
    filter( Ekspertkomite =="Karplanter") %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
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
  
  # Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
  ggplot(Sankey3, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = node,
                      label = paste0(node,",\nn=", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(rep(.78,310),    # Ikke risikovurdert tidligere
                                rep(2.1,91),  # NR
                                rep(.78,361), rep(2.1,202),   # NK
                                rep(.78,422), rep(2.1,600),  # LO
                                rep(.78,46), rep(2.1,179),  # PH
                                rep(.78,56),rep(2.1,91),  # HI
                                rep(.78,73), rep(2.1,105))),  # SE)),
                      size = 3.5, color = 1, fill = "white") +
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
          plot.background = element_rect(fill='transparent', color=NA))
  
  rm(Sankey1, Sankey2, Sankey3)
}
ggsave('karplanter/endring_verdier.png', bg='transparent')

## Bare reviderte arter; fjern "Ikke risikovurdert tidligere" fra 2018-siden
{
  ferdig %>%
    filter( Ekspertkomite =="Karplanter") %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    filter(Kategori2018 != 'Ikke risikovurdert tidligere') %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("NR",
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
                                              geom_sankey_label(size = 3.5, color = 1, fill = "white") +
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
                                                    plot.background = element_rect(fill='transparent', color=NA))
                                          }
}
ggsave('karplanter/endring_reviderteArter.png', bg='transparent')

# Med verdier
{
  # Step 1
  Sankey1 <- ferdig %>%
    filter( Ekspertkomite =="Karplanter") %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    filter(Kategori2018 != 'Ikke risikovurdert tidligere') %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("NR",
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
  
  # Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
  ggplot(Sankey3, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = node,
                      label = paste0(node,",\nn=", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(#rep(.78,289),    # Ikke risikovurdert tidligere
      rep(2.1,14),  # NR
      rep(.78,361), rep(2.1,124),   # NK
      rep(.78,422), rep(2.1,470),  # LO
      rep(.78,46), rep(2.1,167),  # PH
      rep(.78,56),rep(2.1,84),  # HI
      rep(.78,73), rep(2.1,99))),  # SE)),
      size = 3.5, color = 1, fill = "white") +
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
          plot.background = element_rect(fill='transparent', color=NA))
  
  rm(Sankey1, Sankey2, Sankey3)
}
ggsave('karplanter/endring_reviderteArter_verdier.png', bg='transparent')

##---         2.2.5 Matrise-plot ---####

cont_karplanter <- ferdig %>%
  filter(!Kategori2018 == 'Ikke risikovurdert tidligere',
         Ekspertkomite=="Karplanter") %>%  
  droplevels() %>%
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
                           (Kategori2023 == 'NK' & Kategori2018 == 'SE') ~ '4-')) 

xtabs(cont_karplanter$n ~ cont_karplanter$Kategori2018 + cont_karplanter$Kategori2023)

ggplot(cont_karplanter, aes(x = Kategori2018, y = Kategori2023)) +
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
                               'N'='white',
                               "NR"="gray80",
                               "NK"="#a6ad59",
                               'LO'="#60a5a3",
                               'PH'="#1b586c",
                               'HI'="#233368",
                               'SE'="#602d5e"), na.value = 'white') +
  labs(x = 'Risikokategori 2018', y = 'Risikokategori 2023') +
  geom_label(aes(0.3, Kategori2023, label = Kategori2023, fill=Kategori2023), color='white', hjust = -.25, size = 5) +
  geom_label(aes(Kategori2018, 0.5, label = Kategori2018, fill=Kategori2018), color='white', vjust = .35, size = 5) +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10), vjust = 3, size = 12),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10), vjust = -3, size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave('karplanter/endring_matrise.png', bg='transparent')


##---     2.3 Fremmede treslag  ---####

# Vi ønsker de ovenstående plots bare for (reviderte) fremmede treslag (.xlsx-filen 'Treslag', fanen 'Treslag 2.0').
# Last inn filen og sjekk at navnene er kodet likt
treslag <- read.csv2("C:/Users/TKP/OneDrive - artsdatabanken.no/Dokumenter/ForRes_ADB/Treslag.csv")
treslag2 <- treslag %>%
  filter(Tre.eller.busk.eller.baade.og %in% c('T',
                                              'T B',
                                              't',
                                              't b'))

# Filtrer dataframes jfr. navne på treslag 
ferdig_treslag <- ferdig %>%
  filter(VitenskapeligNavn %in% treslag2$VitenskapeligNavn) 

ferdig_long.endring_treslag <- ferdig_long.endring %>%
  filter(VitenskapeligNavn %in% treslag2$VitenskapeligNavn)


##---         2.3.1 Risikokategori  ---####
ferdig_treslag %>%
  filter(Kategori2023 != 'NR') %>%
  {
    ggplot(.,
           aes(x = Kategori2023, fill = Kategori2023)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
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
        "SE"="#602d5e")) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 16))
  }
ggsave('treslag/risikokategori.png', bg='transparent')


##---         2.3.2 Etableringsklasse ---####
ferdig_treslag %>%
  filter(!Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  {
    ggplot(.,
           aes(x = Etableringsklasse_comb, fill = Etableringsklasse_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
      geom_segment(aes(x = 'A', xend = 'C1', y = 72, yend = 72),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'B2', y = 75, label='D\U00F8rstokkarter'), size=4, hjust=-.75) +
      geom_segment(aes(x = 'C2', xend = 'C3E', y = 72, yend = 72),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'C2', y = 75, label='Selvstendig reproduserende'), size=4, hjust=-.1) +
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#5FB7B1","B2"="#71B581",
                                   "C0"="#A0BA5B", "C1"="#d2c160", "C2"="#e5b445",
                                   "C3E"="#936649")) +
      scale_x_discrete(labels = c("A" = "Forekommer ikke i Norge",
                                  "B1" = "Forekommer innend\U00F8rs eller \ni lukkede installasjoner",
                                  "B2" = "Forekommer utend\U00F8rs p\U00E5 \neget produksjonsareal",
                                  "C0" = "Dokumentert i norsk natur",
                                  "C1" = "Overlever vinteren utend\U00F8rs \nuten menneskelig tilsyn",
                                  "C2" = "Selvstendig reproduserende",
                                  "C3E" = "Etablert i norsk natur")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .8, size = 12)) 
  }
ggsave('treslag/etableringsklasse.png', bg='transparent')

### Som kakediagram
# Dette er ikke anbefalt å gjøre, og har derfor ingen direkte pakker til det - derfor må det gjøres noe krumspring for å få til
ferdig_treslag %>%
  filter(!Kategori2023 == "NR") %>% 
  group_by(Etableringsklasse_comb) %>%
  tally() %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(desc(Etableringsklasse_comb)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%  {
    ggplot(., aes(x = "", y = prop, fill = Etableringsklasse_comb)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = n), color = "white", size = 5)+
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
      theme_void() +
      theme(legend.position = "right",
            legend.text = element_text(size = 10),
            legend.spacing.y = unit(.75, 'cm'))  +
      guides(fill = guide_legend(byrow = TRUE))
  }
ggsave('treslag/etableringsklasse_kake.png', bg='transparent')


##---         2.3.3 Aarsak til endring  ---####
ferdig_long.endring_treslag %>%
  filter(Aarsak_norsk != "",
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>% {
    ggplot(.,
           aes(x = Aarsak_norsk, fill = Aarsak_norsk)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c("Endrede avgrensninger/retningslinjer"="#35a3b2",
                                   "Endret status"="#5FB7B1",
                                   "Endret tolkning av retningslinjer"="#71B581",
                                   #"Ny kunnskap"="#A0BA5B",
                                   "Ny tolkning av data"="#d2c160",
                                   "Reell endring"="#e5b445")) +
      scale_x_discrete(labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger\ni retningslinjene",
                                  "Endret status"="Endret status",
                                  "Endret tolkning av\nretningslinjer"="Endret tolkning av retningslinjer",
                                  #"Ny kunnskap"="Ny kunnskap",
                                  "Ny tolkning av data"="Ny tolkning av data",
                                  "Reell endring"="Reell endring")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .9, size = 12)) 
  }
ggsave('treslag/aarsakEndring.png', bg='transparent')

##---             2.3.3.1 Aarsak til endring; oppsummering  ---####
# Plot over antall arter med x årsaker til endring i risikokategori
ferdig_long.endring_treslag %>%
  filter(Aarsak_norsk != "",
         Kategori2023 != 'NR',  # Ta bort NR-arter
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn) %>% 
  tally(name = 'antallAarsaker') %>% 
  {
    ggplot(., aes(x = factor(antallAarsaker), fill = factor(antallAarsaker))) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +   # Bruk ev. x = "Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c('#A0BA5B', '#d2c160', '#e5b445')) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('treslag/aarsakEndring_antallTrinn.png', bg='transparent')

# Samme plot som over, men inkluder bare arter hvor "Endret tolkning av retningslinjer" inngår som én av årsakene
ferdig_long.endring_treslag %>%
  filter(Aarsak_norsk != "",
         Kategori2023 != 'NR',  # Ta bort NR-arter
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn) %>%
  # Filtrer ut arter som ikke har Endret tolkning av retningslinjer", og transformer tilbake til long-format
  mutate(antall = 1) %>% 
  pivot_wider(names_from = Aarsak_norsk, values_from = antall) %>% 
  filter(`Endret tolkning av retningslinjer` == 1) %>% 
  pivot_longer(cols =c(`Reell endring`, `Endret tolkning av retningslinjer`,
                       `Ny tolkning av data`, `Endrede avgrensninger/retningslinjer`), names_to = 'Aarsak_norsk') %>% 
  filter(!is.na(value)) %>% 
  tally(name = 'antallAarsaker') %>% 
  {
    ggplot(., aes(x = factor(antallAarsaker), fill = factor(antallAarsaker))) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +   # Bruk ev. x = "Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c('#A0BA5B', '#d2c160', '#e5b445')) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('treslag/aarsakEndring_antallTrinn_endretTolkning.png', bg='transparent')


##---         2.3.4 Endring i kategori  ---####
{
  ferdig_treslag %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
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
                                              geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                                              scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70",
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
                                                    plot.background = element_rect(fill='transparent', color=NA))
                                          }
}
ggsave('treslag/endring.png', bg='transparent')

## Bare reviderte arter; fjern "Ikke risikovurdert tidligere" fra 2018-siden
{
  ferdig_treslag %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    filter(Kategori2018 != 'Ikke risikovurdert tidligere') %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("NR",
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
                                              geom_sankey_label(size = 3.5, color = 1, fill = "white") +
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
                                                    plot.background = element_rect(fill='transparent', color=NA))
                                          }
}
ggsave('treslag/endring_reviderteArter.png', bg='transparent')

### Samme plots som over, men med verdier
{
  # Step 1
  Sankey1_tre <- ferdig_treslag %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
                                          "NR",
                                          "NK",
                                          "LO",
                                          "PH",
                                          "HI",
                                          "SE"))))
  
  # Step 2
  Sankey2_tre <- Sankey1_tre %>%
    dplyr::group_by(node, next_node) %>%
    tally()
  # Her må fikses litt manuelt - alle rekker med 'NA' på next_node skal stå som de er, men alle nodes med kategori i next_node skal summeres
  Sankey2_tre <- rbind(Sankey2_tre %>%
                         filter(is.na(next_node)) %>%
                         mutate(n2 = n,
                                x = 'Kategori 2023',
                                next_x = NA)  %>%
                         relocate(x, node, next_x, next_node, n, n2) %>%
                         select(-next_node),
                       
                       Sankey2_tre %>%
                         filter(!is.na(next_node)) %>%
                         group_by(node) %>%
                         summarise(n2 = sum(n)) %>%
                         mutate(next_node = 'x', n = NA,
                                x = 'Kategori 2018',
                                next_x = 'Kategori 2023') %>%
                         relocate(x, node, next_x, next_node, n, n2)%>%
                         select(-next_node) )
  
  ### Step 3
  Sankey3_tre <- full_join(Sankey1_tre, Sankey2_tre, by=c('node'='node', 'x'='x', 'next_x'='next_x')) %>%
    mutate(across(c(x, next_x),
                  ~factor(.x, levels = c("Kategori 2018","Kategori 2023")))) %>%
    arrange(node, next_node)
  
  # Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
  ggplot(Sankey3_tre, aes(x = x, 
                          next_x = next_x, 
                          node = node, 
                          next_node = next_node,
                          fill = node,
                          label = paste0(node,",\nn=", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(rep(.78,27),    # Ikke risikovurdert tidligere
                                rep(2.1,13),  # NR
                                rep(.78,42), rep(2.1,10),   # NK
                                rep(.78,56), rep(2.1,71),  # LO
                                rep(.78,4), rep(2.1,21),  # PH
                                rep(.78,11),rep(2.1,18),  # HI
                                rep(.78,12), rep(2.1,19))),  # SE)),
                      size = 3.5, color = 1, fill = "white") +
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
          plot.background = element_rect(fill='transparent', color=NA))
  
  rm(Sankey1_tre, Sankey2_tre, Sankey3_tre)
}
ggsave('treslag/endring_verdier.png', bg='transparent')

# Bare reviderte arter
{
  # Step 1
  Sankey1_tre <- ferdig_treslag %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    filter(Kategori2018 != 'Ikke risikovurdert tidligere') %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("NR",
                                          "NK",
                                          "LO",
                                          "PH",
                                          "HI",
                                          "SE"))))
  
  # Step 2
  Sankey2_tre <- Sankey1_tre %>%
    dplyr::group_by(node, next_node) %>%
    tally()
  # Her må fikses litt manuelt - alle rekker med 'NA' på next_node skal stå som de er, men alle nodes med kategori i next_node skal summeres
  Sankey2_tre <- rbind(Sankey2_tre %>%
                         filter(is.na(next_node)) %>%
                         mutate(n2 = n,
                                x = 'Kategori 2023',
                                next_x = NA)  %>%
                         relocate(x, node, next_x, next_node, n, n2) %>%
                         select(-next_node),
                       
                       Sankey2_tre %>%
                         filter(!is.na(next_node)) %>%
                         group_by(node) %>%
                         summarise(n2 = sum(n)) %>%
                         mutate(next_node = 'x', n = NA,
                                x = 'Kategori 2018',
                                next_x = 'Kategori 2023') %>%
                         relocate(x, node, next_x, next_node, n, n2)%>%
                         select(-next_node) )
  
  ### Step 3
  Sankey3_tre <- full_join(Sankey1_tre, Sankey2_tre, by=c('node'='node', 'x'='x', 'next_x'='next_x')) %>%
    mutate(across(c(x, next_x),
                  ~factor(.x, levels = c("Kategori 2018","Kategori 2023")))) %>%
    arrange(node, next_node)
  
  # Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
  ggplot(Sankey3_tre, aes(x = x, 
                          next_x = next_x, 
                          node = node, 
                          next_node = next_node,
                          fill = node,
                          label = paste0(node,",\nn=", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(rep(2.1,1),  # NR
                                rep(.78,42), rep(2.1,9),   # NK
                                rep(.78,56), rep(2.1,59),  # LO
                                rep(.78,4), rep(2.1,20),  # PH
                                rep(.78,11),rep(2.1,18),  # HI
                                rep(.78,12), rep(2.1,18))),  # SE)),
                      size = 3.5, color = 1, fill = "white") +
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
          plot.background = element_rect(fill='transparent', color=NA))
  
  rm(Sankey1_tre, Sankey2_tre, Sankey3_tre)
}
ggsave('treslag/endring_verdier_reviderteArter.png', bg='transparent')


##---         2.3.5 Matrise-plot ---####

# Lag contingency table med verdier
cont_treslag <- ferdig_treslag %>%
  filter(!Kategori2018 == 'Ikke risikovurdert tidligere') %>%  
  droplevels() %>%
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
                           (Kategori2023 == 'NK' & Kategori2018 == 'SE') ~ '4-')) 

xtabs(cont_treslag$n ~ cont_treslag$Kategori2018 + cont_treslag$Kategori2023)

ggplot(cont_treslag, aes(x = Kategori2018, y = Kategori2023)) +
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
                               'N'='white',
                               "NR"="gray80",
                               "NK"="#a6ad59",
                               'LO'="#60a5a3",
                               'PH'="#1b586c",
                               'HI'="#233368",
                               'SE'="#602d5e"), na.value = 'white') +
  labs(x = 'Kategori 2018', y = 'Kategori 2023') +
  geom_label(aes(0.3, Kategori2023, label = Kategori2023, fill=Kategori2023), color='white', hjust = -.25, size = 5) +
  geom_label(aes(Kategori2018, 0.5, label = Kategori2018, fill=Kategori2018), color='white', vjust = .35, size = 5) +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10), vjust = 3, size = 12),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10), vjust = -3, size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave('treslag/endring_matrise.png', bg='transparent')


##---     2.4 Doerstokkarter  ---####
#---         2.4.1 Risikokategori  ---####
ferdig %>%
  filter(Fremmedartsstatus == "Doerstokkart",
         Kategori2023 != 'NR') %>%
  {
    ggplot(.,
           aes(x = Kategori2023, fill = Kategori2023)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
      scale_x_discrete( #drop=FALSE,
        labels = c(#"NR" = "Ikke risikovurdert\nNR",
          "NK" = "Ingen \nkjent risiko\nNK",
          "LO" = "Lav \nrisiko\nLO",
          "PH" = "Potensielt h\U00F8y \nrisiko\nPH",
          "HI" = "H\U00F8y \nrisiko\nHI",
          "SE" = "Sv\U00E6rt h\U00F8y \nrisiko\nSE")) +
      scale_fill_manual(values = c("NR"="white", "NK"="#a6ad59", "LO"="#60a5a3",
                                   "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 16))
  }
ggsave('doerstokkarter/risikokategori.png', bg='transparent')


##---         2.4.2 Etableringsklasse ---####
ferdig %>%
  filter(Fremmedartsstatus == "Doerstokkart",
         !Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  {
    ggplot(.,
           aes(x = Etableringsklasse_comb, fill = Etableringsklasse_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.5, size = 5) +
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#5FB7B1","B2"="#71B581",
                                   "C0"="#A0BA5B", "C1"="#d2c160", "C2"="#e5b445",
                                   "C3E"="#936649")) + 
      scale_x_discrete(labels = c("A" = "Forekommer ikke i Norge",
                                  "B1" = "Forekommer innend\U00F8rs eller \ni lukkede installasjoner",
                                  "B2" = "Forekommer utend\U00F8rs p\U00E5 \neget produksjonsareal",
                                  "C0" = "Dokumentert i norsk natur",
                                  "C1" = "Overlever vinteren utend\U00F8rs \nuten menneskelig tilsyn" )) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .8, size = 12)) 
  }
ggsave('doerstokkarter/etableringsklasse.png', bg='transparent')

### Som kakediagram
# Dette er ikke anbefalt å gjøre, og har derfor ingen direkte pakker til det - derfor må det gjøres noe krumspring for å få til
ferdig %>%
  filter(Fremmedartsstatus == "Doerstokkart",
         !Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  group_by(Etableringsklasse_comb) %>%
  tally() %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(desc(Etableringsklasse_comb)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%  {
    ggplot(., aes(x = "", y = prop, fill = Etableringsklasse_comb)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = n), color = "white", size = 5)+
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
      theme_void() +
      theme(legend.position = "right",
            legend.text = element_text(size = 10),
            legend.spacing.y = unit(.75, 'cm'))  +
      guides(fill = guide_legend(byrow = TRUE))
  }
ggsave('doerstokkarter/etableringsklasse_kake.png', bg='transparent')


##---         2.4.3 Aarsak til endring  ---####
ferdig_long.endring %>%
  filter(Fremmedartsstatus == "Doerstokkart",
         Aarsak_norsk != "",
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>% {
    ggplot(.,
           aes(x = Aarsak_norsk, fill = Aarsak_norsk)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c("Endrede avgrensninger/retningslinjer"="#35a3b2",
                                   "Endret status"="#5FB7B1",
                                   "Endret tolkning av retningslinjer"="#71B581",
                                   "Ny kunnskap"="#A0BA5B",
                                   "Ny tolkning av data"="#d2c160",
                                   "Reell endring"="#e5b445")) +
      scale_x_discrete(labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger\ni retningslinjene",
                                  "Endret status"="Endret status",
                                  "Endret tolkning av\nretningslinjer"="Endret tolkning av retningslinjer",
                                  "Ny kunnskap"="Ny kunnskap",
                                  "Ny tolkning av data"="Ny tolkning av data",
                                  "Reell endring"="Reell endring")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .9, size = 12))
  }
ggsave('doerstokkarter/aarsakEndring.png', bg='transparent')

##---             2.4.3.1 Aarsak til endring; oppsummering  ---####
# Plot over antall arter med x årsaker til endring i risikokategori
ferdig_long.endring %>%
  filter(Fremmedartsstatus == 'Doerstokkart',
         Aarsak_norsk != "",
         Kategori2023 != 'NR',  # Ta bort NR-arter
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn) %>% 
  tally(name = 'antallAarsaker') %>% 
  {
    ggplot(., aes(x = factor(antallAarsaker), fill = factor(antallAarsaker))) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +   # Bruk ev. x = "Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c('#A0BA5B', '#d2c160', '#e5b445')) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('doerstokkarter/aarsakEndring_antallTrinn.png', bg='transparent')

# Samme plot som over, men inkluder bare arter hvor "Endret tolkning av retningslinjer" inngår som én av årsakene
ferdig_long.endring %>%
  filter(Fremmedartsstatus == 'Doerstokkart',
         Aarsak_norsk != "",
         Kategori2023 != 'NR',  # Ta bort NR-arter
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn) %>%
  # Filtrer ut arter som ikke har Endret tolkning av retningslinjer", og transformer tilbake til long-format
  mutate(antall = 1) %>% 
  pivot_wider(names_from = Aarsak_norsk, values_from = antall) %>% 
  filter(`Endret tolkning av retningslinjer` == 1) %>% 
  pivot_longer(cols =c(`Reell endring`, `Endret tolkning av retningslinjer`,
                       `Ny tolkning av data`, `Endrede avgrensninger/retningslinjer`, `Endret status`), names_to = 'Aarsak_norsk') %>% 
  filter(!is.na(value)) %>% 
  tally(name = 'antallAarsaker') %>% 
  {
    ggplot(., aes(x = factor(antallAarsaker), fill = factor(antallAarsaker))) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +   # Bruk ev. x = "Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c('#A0BA5B', '#d2c160', '#e5b445')) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('doerstokkarter/aarsakEndring_antallTrinn_endretTolkning.png', bg='transparent')

##---         2.4.4 Endring i kategori  ---####
{
  ferdig %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    filter( Fremmedartsstatus == "Doerstokkart") %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
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
                                                          fill = node,
                                                          label = node)) +
                                              geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
                                              geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                                              scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70",
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
                                                    plot.background = element_rect(fill='transparent', color=NA))
                                          }
}
ggsave('doerstokkarter/endring.png', bg='transparent')

# Med verdier
{
  # Step 1
  Sankey1 <- ferdig %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    filter( Fremmedartsstatus == "Doerstokkart") %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
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
  
  # Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
  ggplot(Sankey3, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = node,
                      label = paste0(node,",\nn=", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(rep(.78,698),    # Ikke risikovurdert tidligere
                                #rep(2.1,84),  # NR
                                rep(.78,143), rep(2.1,259),   # NK
                                rep(.78,155), rep(2.1,536),  # LO
                                rep(.78,20), rep(2.1,113),  # PH
                                rep(.78,29),rep(2.1,86),  # HI
                                rep(.78,23), rep(2.1,74))),  # SE)),
                      size = 3.5, color = 1, fill = "white") +
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
          plot.background = element_rect(fill='transparent', color=NA))
  
  rm(Sankey1, Sankey2, Sankey3)
}
ggsave('doerstokkarter/endring_verdier.png', bg='transparent')


##---         2.4.5 Matrise-plot ---####

cont_DS <- ferdig %>%
  filter(!Kategori2018 == 'Ikke risikovurdert tidligere',
         Fremmedartsstatus == "Doerstokkart") %>%  
  droplevels() %>%
  mutate(across(c(Kategori2018, Kategori2023), ~ordered(.x, levels = c("NR","NK","LO","PH","HI","SE")))) %>%
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
                           (Kategori2023 == 'NK' & Kategori2018 == 'SE') ~ '4-')) 

xtabs(cont_DS$n ~ cont_DS$Kategori2018 + cont_DS$Kategori2023)

ggplot(cont_DS, aes(x = Kategori2018, y = Kategori2023)) +
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
                               'N'='white',
                               "NR"="gray80",
                               "NK"="#a6ad59",
                               'LO'="#60a5a3",
                               'PH'="#1b586c",
                               'HI'="#233368",
                               'SE'="#602d5e"), na.value = 'white') +
  labs(x = 'Risikokategori 2018', y = 'Risikokategori 2023') +
  geom_label(aes(0.3, Kategori2023, label = Kategori2023, fill=Kategori2023), color='white', hjust = -.25, size = 5) +
  geom_label(aes(Kategori2018, 0.5, label = Kategori2018, fill=Kategori2018), color='white', vjust = .35, size = 5) +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10), vjust = 3, size =12),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10), vjust = -3, size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave('doerstokkarter/endring_matrise.png', bg='transparent')


##---     2.5 Doerstokkarter fra Horisontskanningen ---####
#---         2.5.1 Risikokategori  ---####
ferdig %>%
  filter(Fremmedartsstatus == "Doerstokkart",
         Kategori2018 == 'NR' | Kategori2018 == 'Ikke risikovurdert tidligere',
         Kategori2023 != 'NR') %>%
  {
    ggplot(.,
           aes(x = Kategori2023, fill = Kategori2023)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
      scale_x_discrete( #drop=FALSE,
        labels = c("NR" = "Ikke \nrisikovurdert\nNR",
                   "NK" = "Ingen kjent \nrisiko\nNK",
                   "LO" = "Lav risiko\nLO",
                   "PH" = "Potensielt h\U00F8y \nrisiko\nPH",
                   "HI" = "H\U00F8y \nrisiko\nHI",
                   "SE" = "Sv\U00E6rt h\U00F8y \nrisiko\nSE")) +
      scale_fill_manual(values = c("NR"="white", "NK"="#a6ad59", "LO"="#60a5a3",
                                   "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 16))
  }
ggsave('doerstokkarter/ArterFraHorisontskanning/risikokategori.png', bg='transparent')


##---         2.5.2 Etableringsklasse ---####
ferdig %>%
  filter(Fremmedartsstatus == "Doerstokkart",
         Kategori2018 == 'NR' | Kategori2018 == 'Ikke risikovurdert tidligere',
         !Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  {
    ggplot(.,
           aes(x = Etableringsklasse_comb, fill = Etableringsklasse_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.5, size = 5) +
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#5FB7B1","B2"="#71B581",
                                   "C0"="#A0BA5B", "C1"="#d2c160", "C2"="#e5b445",
                                   "C3E"="#936649")) + 
      scale_x_discrete(labels = c("A" = "Forekommer ikke i Norge",
                                  "B1" = "Forekommer innend\U00F8rs eller \ni lukkede installasjoner",
                                  "B2" = "Forekommer utend\U00F8rs p\U00E5 \neget produksjonsareal",
                                  "C0" = "Dokumentert i norsk natur",
                                  "C1" = "Overlever vinteren utend\U00F8rs \nuten menneskelig tilsyn" )) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .8, size = 12)) 
  }
ggsave('doerstokkarter/ArterFraHorisontskanning/etableringsklasse.png', bg='transparent')

### Som kakediagram
# Dette er ikke anbefalt å gjøre, og har derfor ingen direkte pakker til det - derfor må det gjøres noe krumspring for å få til
ferdig %>%
  filter(Fremmedartsstatus == "Doerstokkart",
         Kategori2018 == 'NR' | Kategori2018 == 'Ikke risikovurdert tidligere',
         !Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  group_by(Etableringsklasse_comb) %>%
  tally() %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(desc(Etableringsklasse_comb)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%  {
    ggplot(., aes(x = "", y = prop, fill = Etableringsklasse_comb)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = n), color = "white", size = 5)+
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
      theme_void() +
      theme(legend.position = "right",
            legend.text = element_text(size = 10),
            legend.spacing.y = unit(.75, 'cm'))  +
      guides(fill = guide_legend(byrow = TRUE))
  }
ggsave('doerstokkarter/ArterFraHorisontskanning/etableringsklasse_kake.png', bg='transparent')


##---         2.5.3 Aarsak til endring  ---####
ferdig_long.endring %>%
  filter(Fremmedartsstatus == "Doerstokkart",
         Kategori2018 == 'NR' | Kategori2018 == 'Ikke risikovurdert tidligere',
         Aarsak_norsk != "",
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>% {
    ggplot(.,
           aes(x = Aarsak_norsk, fill = Aarsak_norsk)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c("Endrede avgrensninger/retningslinjer"="#35a3b2",
                                   "Endret status"="#5FB7B1",
                                   "Endret tolkning av retningslinjer"="#71B581",
                                   "Ny kunnskap"="#A0BA5B",
                                   "Ny tolkning av data"="#d2c160",
                                   "Reell endring"="#e5b445")) +
      scale_x_discrete(labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger\ni retningslinjene",
                                  "Endret status"="Endret status",
                                  "Endret tolkning av\nretningslinjer"="Endret tolkning av retningslinjer",
                                  "Ny kunnskap"="Ny kunnskap",
                                  "Ny tolkning av data"="Ny tolkning av data",
                                  "Reell endring"="Reell endring")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .9, size = 12))
  }
ggsave('doerstokkarter/ArterFraHorisontskanning/aarsakEndring.png', bg='transparent')

##---             2.5.3.1 Aarsak til endring; oppsummering  ---####
# Plot over antall arter med x årsaker til endring i risikokategori
ferdig_long.endring %>%
  filter(Fremmedartsstatus == "Doerstokkart",
         Kategori2018 == 'NR' | Kategori2018 == 'Ikke risikovurdert tidligere',
         Aarsak_norsk != "",
         Kategori2023 != 'NR',  # Ta bort NR-arter
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn) %>% 
  tally(name = 'antallAarsaker') %>% 
  {
    ggplot(., aes(x = factor(antallAarsaker), fill = factor(antallAarsaker))) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +   # Bruk ev. x = "Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c('#A0BA5B', '#d2c160', '#e5b445')) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('doerstokkarter/ArterFraHorisontskanning/aarsakEndring_antallTrinn.png', bg='transparent')

# Samme plot som over, men inkluder bare arter hvor "Endret tolkning av retningslinjer" inngår som én av årsakene
ferdig_long.endring %>%
  filter(Fremmedartsstatus == "Doerstokkart",
         Kategori2018 == 'NR' | Kategori2018 == 'Ikke risikovurdert tidligere',
         Aarsak_norsk != "",
         Kategori2023 != 'NR',  # Ta bort NR-arter
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn) %>%
  # Filtrer ut arter som ikke har Endret tolkning av retningslinjer", og transformer tilbake til long-format
  mutate(antall = 1) %>% 
  pivot_wider(names_from = Aarsak_norsk, values_from = antall) %>% 
  filter(`Endret tolkning av retningslinjer` == 1) %>% 
  pivot_longer(cols =c(`Reell endring`, `Endret tolkning av retningslinjer`,
                       `Ny tolkning av data`, `Endrede avgrensninger/retningslinjer`, `Endret status`), names_to = 'Aarsak_norsk') %>% 
  filter(!is.na(value)) %>% 
  tally(name = 'antallAarsaker') %>% 
  {
    ggplot(., aes(x = factor(antallAarsaker), fill = factor(antallAarsaker))) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +   # Bruk ev. x = "Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c('#A0BA5B', '#d2c160', '#e5b445')) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('doerstokkarter/ArterFraHorisontskanning/aarsakEndring_antallTrinn_endretTolkning.png', bg='transparent')


##---         2.5.4 Endring i kategori  ---####
{
  ferdig %>%
    filter( Fremmedartsstatus == "Doerstokkart",
            Kategori2018 == 'NR' | Kategori2018 == 'Ikke risikovurdert tidligere') %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
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
                                                          fill = node,
                                                          label = node)) +
                                              geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
                                              geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                                              scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70",
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
                                                    plot.background = element_rect(fill='transparent', color=NA))
                                          }
}
ggsave('doerstokkarter/ArterFraHorisontskanning/endring.png', bg='transparent')

## Samme plot som over, men med verdier
{
  # Step 1
  Sankey1 <- ferdig %>%
    filter( Fremmedartsstatus == "Doerstokkart",
            Kategori2018 == 'NR' | Kategori2018 == 'Ikke risikovurdert tidligere') %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
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
  
  # Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
  ggplot(Sankey3, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = node,
                      label = paste0(node,",\nn=", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(rep(.78,698),    # Ikke risikovurdert tidligere
                                rep(2.1,164),   # NK
                                rep(2.1,338),  # LO
                                rep(2.1,92),  # PH
                                rep(2.1,57),  # HI
                                rep(2.1,47))),  # SE
                      size = 3.5, color = 1, fill = "white") +
    scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70",
                                 "NR"="gray90",
                                 "NK"="#a6ad59",
                                 "LO"="#60a5a3",
                                 "PH"="#1b586c",
                                 "HI"="#233368",
                                 "SE"="#602d5e")) +
    labs(x = "") +
    theme_sankey(base_size = 16) +
    theme(legend.position="none",
          panel.background = element_rect(fill='transparent', color = NA),
          plot.background = element_rect(fill='transparent', color=NA))
  rm(Sankey1, Sankey2, Sankey3)
}
ggsave('doerstokkarter/ArterFraHorisontskanning/endring_verdier.png', bg='transparent')


##---     2.6 Marine arter  ---####
##---         2.6.1 Risikokategori  ---####
# For å lage noe som kan være interessant for enkelte deler av forvaltningsbrukergruppa, filtrer på marine
# (eller marine+limniske) arter

ferdig %>%
  filter(Marint == "True" & Terrestrisk == "False",
         Kategori2023 != "NR" ) %>%   # Fjern 'NR' fra plots
  {
    ggplot(.,
           aes(x = Kategori2023, fill = Kategori2023)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
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
        "SE"="#602d5e")) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 16))
  }
ggsave('marineArter/risikokategori.png', bg='transparent')

##---         2.6.2 Etableringsklasse ---####
ferdig %>%
  filter(Marint == "True" & Terrestrisk == "False",
         !Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  {
    ggplot(.,
           aes(x = Etableringsklasse_comb, fill = Etableringsklasse_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
      geom_segment(aes(x = 'A', xend = 'C1', y = 180, yend = 180),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'B2', y = 185, label='Doerstokkarter'), size=4) +
      geom_segment(aes(x = 'C2', xend = 'C3E', y = 180, yend = 180),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'C2', y = 185, label='Selvstendig reproduserende'), size=4, hjust=-.01) +
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#5FB7B1","B2"="#71B581",
                                   "C0"="#A0BA5B", "C1"="#d2c160", "C2"="#e5b445",
                                   "C3E"="#936649")) +
      scale_x_discrete(labels = c("A" = "Forekommer ikke i Norge",
                                  "B1" = "Forekommer innend\U00F8rs eller \ni lukkede installasjoner",
                                  "B2" = "Forekommer utend\U00F8rs p\U00E5 \neget produksjonsareal",
                                  "C0" = "Dokumentert i norsk natur",
                                  "C1" = "Overlever vinteren utend\U00F8rs \nuten menneskelig tilsyn",
                                  "C2" = "Selvstendig reproduserende",
                                  "C3E" = "Etablert i norsk natur")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .8, size = 12)) 
  }
ggsave('marineArter/etableringsklasse.png', bg='transparent')

### Som kakediagram
# Dette er ikke anbefalt å gjøre, og har derfor ingen direkte pakker til det - derfor må det gjøres noe krumspring for å få til
ferdig %>%
  filter(Marint == "True" & Terrestrisk == "False",
         !Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  group_by(Etableringsklasse_comb) %>%
  tally() %>%
  mutate(prop = n/sum(n)*100) %>%
  arrange(desc(Etableringsklasse_comb)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>%  {
    ggplot(., aes(x = "", y = prop, fill = Etableringsklasse_comb)) +
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      geom_text(aes(y = lab.ypos, label = n), color = "white", size = 5)+
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
      theme_void() +
      theme(legend.position = "right",
            legend.text = element_text(size = 10),
            legend.spacing.y = unit(.75, 'cm'))  +
      guides(fill = guide_legend(byrow = TRUE))
  }
ggsave('marineArter/etableringsklasse_kake.png', bg='transparent')


##---         2.6.3 Aarsak til endring  ---####
ferdig_long.endring %>%
  filter(Marint == "True" & Terrestrisk == "False",
         Aarsak_norsk != "",
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>% {
    ggplot(.,
           aes(x = Aarsak_norsk, fill = Aarsak_norsk)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c("Endrede avgrensninger/retningslinjer"="#35a3b2",
                                   "Endret status"="#5FB7B1",
                                   "Endret tolkning av retningslinjer"="#71B581",
                                   #"Ny kunnskap"="#A0BA5B",
                                   "Ny tolkning av data"="#d2c160",
                                   "Reell endring"="#e5b445")) +
      scale_x_discrete(labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger\ni retningslinjene",
                                  "Endret status"="Endret status",
                                  "Endret tolkning av\nretningslinjer"="Endret tolkning av retningslinjer",
                                  #"Ny kunnskap"="Ny kunnskap",
                                  "Ny tolkning av data"="Ny tolkning av data",
                                  "Reell endring"="Reell endring")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .9, size = 12)) 
  }
ggsave('marineArter/aarsakEndring.png', bg='transparent')

##---             2.6.3.1 Aarsak til endring; oppsummering  ---####
# Plot over antall arter med x årsaker til endring i risikokategori
ferdig_long.endring %>%
  filter(Marint == "True" & Terrestrisk == "False",
         Aarsak_norsk != "",
         Kategori2023 != 'NR',  # Ta bort NR-arter
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn) %>% 
  tally(name = 'antallAarsaker') %>% 
  {
    ggplot(., aes(x = factor(antallAarsaker), fill = factor(antallAarsaker))) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +   # Bruk ev. x = "Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c('#A0BA5B', '#d2c160', '#e5b445')) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('marineArter/aarsakEndring_antallTrinn.png', bg='transparent')

# Samme plot som over, men inkluder bare arter hvor "Endret tolkning av retningslinjer" inngår som én av årsakene
ferdig_long.endring %>%
  filter(Marint == "True" & Terrestrisk == "False",
         Aarsak_norsk != "",
         Kategori2023 != 'NR',  # Ta bort NR-arter
         Kategori2018 != Kategori2023) %>% 
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn) %>%
  # Filtrer ut arter som ikke har Endret tolkning av retningslinjer", og transformer tilbake til long-format
  mutate(antall = 1) %>% 
  pivot_wider(names_from = Aarsak_norsk, values_from = antall) %>% 
  filter(`Endret tolkning av retningslinjer` == 1) %>% 
  pivot_longer(cols =c(`Reell endring`, `Endret tolkning av retningslinjer`,
                       `Ny tolkning av data`, `Endrede avgrensninger/retningslinjer`, `Endret status`), names_to = 'Aarsak_norsk') %>% 
  filter(!is.na(value)) %>% 
  tally(name = 'antallAarsaker') %>% 
  {
    ggplot(., aes(x = factor(antallAarsaker), fill = factor(antallAarsaker))) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +   # Bruk ev. x = "Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c('#A0BA5B', '#d2c160', '#e5b445')) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('marineArter/aarsakEndring_antallTrinn_endretTolkning.png', bg='transparent')

##---         2.6.4 Endring i kategori  ---####
{
  ferdig %>%
    filter(Marint == "True" & Terrestrisk == "False") %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
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
                                              geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                                              scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70",
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
                                                    plot.background = element_rect(fill='transparent', color=NA))
                                          }
}
ggsave('marineArter/endring.png', bg='transparent')

# Med verdier
{
  # Step 1
  Sankey1 <- ferdig %>%
    filter(Marint == "True" & Terrestrisk == "False") %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("Ikke risikovurdert tidligere",
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
  
  # Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
  ggplot(Sankey3, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = node,
                      label = paste0(node,",\nn=", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(rep(.78,117),    # Ikke risikovurdert tidligere
                                rep(2.1,11),  # NR
                                rep(.78,7), rep(2.1,6),   # NK
                                rep(.78,53), rep(2.1,114),  # LO
                                rep(.78,20), rep(2.1,32),  # PH
                                rep(.78,18),rep(2.1,29),  # HI
                                rep(.78,23), rep(2.1,46))),  # SE)),
                      size = 3.5, color = 1, fill = "white") +
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
          plot.background = element_rect(fill='transparent', color=NA))
  
  rm(Sankey1, Sankey2, Sankey3)
}
ggsave('marineArter/endring_verdier.png', bg='transparent')


## Bare reviderte arter; fjern "Ikke risikovurdert tidligere" fra 2018-siden
{
  ferdig %>%
    filter(Marint == "True" & Terrestrisk == "False") %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    filter(Kategori2018 != 'Ikke risikovurdert tidligere') %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("NR",
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
                                              geom_sankey_label(size = 3.5, color = 1, fill = "white") +
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
                                                    plot.background = element_rect(fill='transparent', color=NA))
                                          }
}
ggsave('marineArter/endring_reviderteArter.png', bg='transparent')

# Med verdier
{
  # Step 1
  Sankey1 <- ferdig %>%
    filter(Marint == "True" & Terrestrisk == "False") %>%
    mutate(Kategori2018 =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",   # kombiner NR og arter fra HS
                                     Kategori2018 == "NK" ~ "NK",
                                     Kategori2018 == "LO" ~ "LO",
                                     Kategori2018 == "PH" ~ "PH",
                                     Kategori2018 == "HI" ~ "HI",
                                     Kategori2018 == "SE" ~ "SE"),
           Kategori2023 =  case_when(Kategori2023 =="NR" | Kategori2023 == "Ikke risikovurdert tidligere" ~ "NR",   # kombiner NR og arter fra HS
                                     Kategori2023 == "NK" ~ "NK",
                                     Kategori2023 == "LO" ~ "LO",
                                     Kategori2023 == "PH" ~ "PH",
                                     Kategori2023 == "HI" ~ "HI",
                                     Kategori2023 == "SE" ~ "SE")) %>%
    filter(Kategori2018 != 'Ikke risikovurdert tidligere') %>%
    rename('Kategori 2018' = 'Kategori2018' ,
           'Kategori 2023' = 'Kategori2023' ) %>%
    make_long(`Kategori 2018`, `Kategori 2023`) %>%
    mutate(across(c(node, next_node),
                  ~ordered(.x, levels = c("NR",
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
  
  # Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
  ggplot(Sankey3, aes(x = x, 
                      next_x = next_x, 
                      node = node, 
                      next_node = next_node,
                      fill = node,
                      label = paste0(node,",\nn=", n2) )) +
    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
    geom_sankey_label(aes(x = c(#rep(.78,116),    # Ikke risikovurdert tidligere
      rep(2.1,4),  # NR
      rep(.78,7), #rep(2.1,6),   # NK
      rep(.78,53), rep(2.1,54),  # LO
      rep(.78,20), rep(2.1,14),  # PH
      rep(.78,18),rep(2.1,21),  # HI
      rep(.78,23), rep(2.1,28))),  # SE)),
      size = 3.5, color = 1, fill = "white") +
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
          plot.background = element_rect(fill='transparent', color=NA))
  
  rm(Sankey1, Sankey2, Sankey3)
}
ggsave('marineArter/endring_reviderteArter_verdier.png', bg='transparent')


##---         2.6.5 Matrise-plot ---####
cont_marin <- ferdig %>%
  filter(Marint == "True" & Terrestrisk == "False",
         !Kategori2018 == 'Ikke risikovurdert tidligere') %>%  
  droplevels() %>%
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
                           (Kategori2023 == 'NK' & Kategori2018 == 'SE') ~ '4-')) 

xtabs(cont_marin$n ~ cont_marin$Kategori2018 + cont_marin$Kategori2023)

ggplot(cont_marin, aes(x = Kategori2018, y = Kategori2023)) +
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
                               'N'='white',
                               "NR"="gray80",
                               "NK"="#a6ad59",
                               'LO'="#60a5a3",
                               'PH'="#1b586c",
                               'HI'="#233368",
                               'SE'="#602d5e"), na.value = 'white') +
  labs(x = 'Risikokategori 2018', y = 'Risikokategori 2023') +
  geom_label(aes(0.3, Kategori2023, label = Kategori2023, fill=Kategori2023), color='white', hjust = -.25, size = 5) +
  geom_label(aes(Kategori2018, 0.5, label = Kategori2018, fill=Kategori2018), color='white', vjust = .35, size = 5) +
  theme_minimal() +
  theme(legend.position = 'none',
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title.y = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10), vjust = 3, size = 12),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10), vjust = -3, size = 12),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ggsave('marineArter/endring_matrise.png', bg='transparent')


##---------------------------------------------------------------------------------------------------####
##---   3. STORE ENDRINGER  ---####

# Det kan være interessant å se på de artene som har endret kategori med >=3 trinn
trinn3 <- ferdig %>%
  filter((Kategori2018 == 'NK' & Kategori2023 == 'HI') |
           (Kategori2018 == 'LO' & Kategori2023 == 'SE') |
           (Kategori2018 == 'NK' & Kategori2023 == 'SE') |
           (Kategori2023 == 'NK' & Kategori2018 == 'HI') |
           (Kategori2023 == 'LO' & Kategori2018 == 'SE') |
           (Kategori2023 == 'NK' & Kategori2018 == 'SE') )

trinn3 %>%
  select(Ekspertkomite, VitenskapeligNavn, NorskNavn,
         Kategori2018, Kriterier2018, Kategori2023, Kriterier2023,
         AarsakTilEndringIKategori, AarsakTilEndringIKategoriBeskrivelse, SistEndretAv) %>%
  write.csv2('storeEndringer.csv') #%>%
#View()

##---     3.1 Plot ekspertkomite  ---####
ggplot(trinn3,
       aes(x = Ekspertkomite, fill=Ekspertkomite)) +
  geom_bar(color = 'black') +
  labs(x = "", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-.5, size = 5) +
  scale_fill_manual(values=c('#35a3b2', '#5FB7B1', '#71B581', '#A0BA5B', '#d2c160', '#e5b445', '#936649')) +  # Obs på antall farger om antall artsgrupper endres
  theme_minimal() +
  theme(legend.position="none",
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = .8, size = 12))
ggsave('Endring3trinn/ekspertkomite.png', bg='transparent')

##---     3.2 Plot Årsak til endring  ---####
# OBS summen av barene her er ikke antallet av arter; de kan ha hatt mer en én endringskategori
ferdig_long.endring %>%
  filter(VitenskapeligNavn %in% trinn3$VitenskapeligNavn ,
         Aarsak_norsk != "",
         Kategori2018 != Kategori2023) %>%
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%  # "Reel endring' har innimellom to rader om det var både "Ny kunnskap" og "Reell endring" i rådata
  {
    ggplot(.,
           aes(x = Aarsak_norsk, fill = Aarsak_norsk)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-.2, size = 5) +
      scale_fill_manual(values = c("Endrede avgrensninger/retningslinjer"="#35a3b2",
                                   "Endret status"="#5FB7B1",
                                   "Endret tolkning av retningslinjer"="#71B581",
                                   #"Ny kunnskap"="#A0BA5B",
                                   "Ny tolkning av data"="#d2c160",
                                   "Reell endring"="#e5b445")) +
      scale_x_discrete(labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger\ni retningslinjene",
                                  "Endret status"="Endret status",
                                  "Endret tolkning av\nretningslinjer"="Endret tolkning av retningslinjer",
                                  #"Ny kunnskap"="Ny kunnskap",
                                  "Ny tolkning av data"="Ny tolkning av data",
                                  "Reell endring"="Reell endring")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .9, size = 12)) 
  }
ggsave('Endring3trinn/aarsak.png', bg='transparent')


# Årsaker for hver art
ferdig_long.endring %>%
  filter(VitenskapeligNavn %in% trinn3$VitenskapeligNavn ,
         Aarsak_norsk != "",
         Kategori2018 != Kategori2023) %>%
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn, Aarsak_norsk) %>%
  tally() %>%
  ggplot(.,
         aes(x = VitenskapeligNavn, y = n, fill = Aarsak_norsk)) +
  geom_bar(color = 'black', stat = 'identity', position = position_dodge(.5)) +
  scale_fill_manual(values = c("Endrede avgrensninger/retningslinjer"="#35a3b2",
                               "Endret status"="#5FB7B1",
                               "Endret tolkning av retningslinjer"="#71B581",
                               #"Ny kunnskap"="#A0BA5B",
                               "Ny tolkning av data"="#d2c160",
                               "Reell endring"="#e5b445"),
                    labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger\ni retningslinjene",
                               "Endret status"="Endret status",
                               "Endret tolkning av\nretningslinjer"="Endret tolkning av retningslinjer",
                               #"Ny kunnskap"="Ny kunnskap",
                               "Ny tolkning av data"="Ny tolkning av data",
                               "Reell endring"="Reell endring"),
                    name = '') +
  labs(y='', x='') +
  theme_minimal() +
  theme(legend.position="bottom",
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = .9, size = 12)) 

ggsave('Endring3trinn/aarsakArt.png', bg='transparent')


##---     3.3 Plot Årsak til endring, matrise-plot  ---####
ferdig_long.endring %>%
  filter(!Kategori2018 == 'Ikke risikovurdert tidligere') %>%  
  filter(VitenskapeligNavn %in% trinn3$VitenskapeligNavn ,
         Aarsak_norsk != "",
         Kategori2018 != Kategori2023) %>%
  distinct(VitenskapeligNavn, Aarsak_norsk) %>%
  group_by(VitenskapeligNavn, Aarsak_norsk) %>%
  tally() %>%
  as.data.frame() %>%
  complete(VitenskapeligNavn, Aarsak_norsk, fill = list(n = 0)) %>%
  mutate(farge = case_when(n == 0 ~ 'NA',     # Legg til bakgrunnsfarger iht. aarsak
                           (Aarsak_norsk == 'Reell endring' & n == 1) ~ 'Reell endring',
                           (Aarsak_norsk == 'Endret tolkning av retningslinjer' & n == 1) ~ 'Endret tolkning av retningslinjer',
                           (Aarsak_norsk == 'Ny tolkning av data' & n == 1) ~ 'Ny tolkning av data',
                           (Aarsak_norsk == 'Endrede avgrensninger/retningslinjer' & n == 1) ~ 'Endrede avgrensninger/retningslinjer',
                           (Aarsak_norsk == 'Endret status' & n == 1) ~ 'Endret status')) %>%
  left_join(ferdig[,c("VitenskapeligNavn","Ekspertkomite")]) %>%
  mutate(Ekspertkomite = factor(Ekspertkomite)) %>%
  arrange(Ekspertkomite) %>%
  mutate(VitenskapeligNavn = factor(VitenskapeligNavn, levels = unique(VitenskapeligNavn))) %>%  # Må inkluderes for å ordne de i rekkefølge basert på komite frem for alfabetisk
  {
    ggplot(., aes(x = Aarsak_norsk, y = VitenskapeligNavn)) +
      geom_tile(aes(fill = farge), color = 'gray40') +
      ### Annotation a vekspertgruppe må legges inn manuelt eller kommenteres ut!
      annotate('segment', x = c(rep(3.6, 7)), xend = c(rep(3.6, 7)),  # Vertikale streker
               y = c(1, 2, 3, 4, 6, 14, 17),
               yend = c(1, 2, 3, 5, 13, 16, 17)) +
      annotate('segment', x = c(rep(3.55, 14)), xend = c(rep(3.6, 14)) , # Horisontale streker
               y = c(1,1, 2,2, 3,3, 4,5, 6,13, 14,16, 17,17),
               yend = c(1,1, 2,2, 3,3, 4,5, 6,13, 14,16, 17,17)  ) +
      annotate("text", x = c(rep(3.85, 7)), y = c(17, 15, 10, 4.5, 3, 2, 1),  # Ekspertkomitenavn
               label = c('Amfibier og \nreptiler', 'Fisker', 'Karplanter', 'Kromister', 'Limniske \ninvertebrater', 'Pattedyr', 'Terrestriske \ninvertebrater'), size=3.5) +
      coord_cartesian(clip = "off") +  # Tillat tekst i hele margin
      ###
      scale_fill_manual(values = c('Reell endring' = '#e5b445',
                                   'Endret tolkning av retningslinjer' = '#71B581',
                                   'Ny tolkning av data' = '#d2c160',
                                   'Endrede avgrensninger/retningslinjer' = '#35a3b2',
                                   'Endret status' = '#5FB7B1'), na.value = 'white') +  labs(x = '', y = '') +
      scale_x_discrete(position = "top",
                       labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger\ni retningslinjene",
                                  "Endret status"="Endret \nstatus",
                                  "Endret tolkning av retningslinjer"="Endret tolkning \nav retningslinjer",
                                  #"Ny kunnskap"="Ny kunnskap",
                                  "Ny tolkning av data"="Ny tolkning \nav data",
                                  "Reell endring"="Reell \nendring"), ) +
      scale_y_discrete(limits = rev) +
      theme(legend.position = 'none',
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            axis.text.y = element_text(face = "italic"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin = unit(c(.5,5,.5,1), "lines") # Øk margin 
      )
  }
ggsave('Endring3trinn/aarsakArt_matrise.png', bg='transparent')


##---     3.4 Plot antall årsaker til endring ---####
ferdig_long.endring %>%
  filter(VitenskapeligNavn %in% trinn3$VitenskapeligNavn ,
         Aarsak_norsk != "",
         Kategori2018 != Kategori2023) %>%
  distinct(VitenskapeligNavn, Aarsak_norsk) %>% 
  left_join(ferdig[,c("VitenskapeligNavn","Ekspertkomite")]) %>%
  mutate(Ekspertkomite = factor(Ekspertkomite)) %>%
  arrange(Ekspertkomite) %>%
  mutate(VitenskapeligNavn = factor(VitenskapeligNavn, levels = unique(VitenskapeligNavn))) %>%  # Må inkluderes for å ordne de i rekkefølge basert på komite frem for alfabetisk
  {
    ggplot(., aes(y = VitenskapeligNavn,fill = Ekspertkomite)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +  # Bruk ev. y="Antall \U00E5rsaker til endring i risikokategori"
      geom_text(stat='count', aes(label=after_stat(count)),  hjust=-1, size = 5)  +
      ### Annotation av ekspertgruppe må legges inn manuelt eller kommenteres ut!
      annotate('segment', x = c(rep(3.15, 7)), xend = c(rep(3.15, 7)),  # Vertikale streker
               y = c(1, 2, 3, 4, 6, 14, 17),
               yend = c(1, 2, 3, 5, 13, 16, 17)) +
      annotate('segment', x = c(rep(3, 14)), xend = c(rep(3.15, 14)) , # Horisontale streker
               y = c(1,1, 2,2, 3,3, 4,5, 6,13, 14,16, 17,17),
               yend = c(1,1, 2,2, 3,3, 4,5, 6,13, 14,16, 17,17)  ) +
      annotate("text", x = c(rep(3.4, 7)), y = c(17, 15, 10, 4.5, 3, 2, 1),  # Ekspertkomitenavn
               label = c('Amfibier og \nreptiler', 'Fisker', 'Karplanter', 'Kromister', 'Limniske \ninvertebrater', 'Pattedyr', 'Terrestriske \ninvertebrater'), size=4) +
      #scale_fill_brewer(palette = 'Blues') +
      scale_fill_manual(values=c('#35a3b2', '#5FB7B1', '#71B581', '#A0BA5B', '#d2c160', '#e5b445', '#936649')) +  # Obs på antall farger om antall artsgrupper endres
      scale_y_discrete(limits = rev) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            plot.margin = unit(c(.5,3,.5,1), "lines"),
            panel.grid = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(face = "italic", size = 12))
    # Sett inn bedre indikasjon av artsgruppe etterhvert
  }
ggsave('Endring3trinn/antallAarsaker_art.png', bg='transparent')


