###--------------------------------------------------------####
###--         FORELOEPIGE RESULTATER FRA FAL            ---####
###--   Modifisert script til figurer til presentasjon  ---####
###--   i moete med med hage- og skognaeringen          ---####
###--------------------------------------------------------####

library(tidyverse)
library(ggplot2)
library(data.table)
#library(ggalluvial)
library(ggsankey)

# Les eksportfilen fra mappe; oppdater til nyeste filen
### OBS! Rett æ, ø og å før filen leses
alle_arter <- read.csv2("20221215/eksportabsoluteall.csv")

###--   1. RYDDING  ---####

# Behold bare ferdigstilte arter
ferdig <- alle_arter %>%
  filter(Vurderinsstatus == "finished") %>%
  filter(!Ekspertkomite == "Testedyr")

ferdig %>%
  select(Ekspertkomite, Vurderinsstatus,VitenskapeligNavn, NorskNavn, SistEndretAv,
         Kategori2018, Kategori2023, AarsakTilEndringIKategori) %>%
  filter(Kategori2023 == "" )
# Noe rart ved at det finnes tomme kategorier for 2023; dette burde ikke være mulig.
# Disse står som NR i FAB. Sjekk data manuelt - fiks her
# Noe er generelt sett feil i de artene - de gir problemer andre steder også

# Definer rette faktor-nivåer i rett rekkefølge
ferdig$Kategori2023[ferdig$Kategori2023==""] <- "NR"
ferdig$Fremmedartsstatus[ferdig$Fremmedartsstatus==""] <- "Ikke fremmed"
ferdig$Kategori2018[ferdig$Kategori2018==""] <- "Ikke risikovurdert tidligere"


ferdig <- ferdig %>%
  # Risikokategorier
  mutate(across(c(Kategori2023, Kategori2018),
                ~ordered(.x, levels = c("Ikke risikovurdert tidligere","NR","NK","LO","PH","HI","SE")))) %>%
  # Fremmedartsstatus
  mutate(across(c(Fremmedartsstatus),
                ~ordered(.x, levels = c("Selvstendig reproduserende", "Regionalt fremmed", "Doerstokkart",
                                        "Effekt uten selvstendig reproduksjon", "Vurderes paa et annet taksonomisk nivaa",
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
  mutate(Aarsak_norsk =  case_when(AarsakTilEndringIKategori =="realChange"  ~ "Reell endring",
                                   AarsakTilEndringIKategori =="newInformation"  ~ "Ny kunnskap",
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
  #filter(Vurderingsomraade == "N",
  #       Ekspertkomite == "Karplanter",
  #       Fremmedartsstatus == "Doerstokkart") %>%
  {
           ggplot(.,
                  aes(x = Kategori2023, fill = Kategori2023)) +
             geom_bar(color = 'black') +
             labs(x = "", y = "") +
             geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
      scale_x_discrete( #drop=FALSE,
        labels = c("NR" = "Ikke risikovurdert\nNR",
                   "NK" = "Ingen kjent risiko\nNK",
                   "LO" = "Lav risiko\nLO",
                   "PH" = "Potensielt hoey risiko\nPH",
                   "HI" = "Hoey risiko\nHI",
                   "SE" = "Svaert hoey risko\nSE")) +
             scale_fill_manual(values = c("NR"="white", "NK"="#a6ad59", "LO"="#60a5a3",
                                          "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")) + 
             theme_minimal() +
             theme(legend.position="none",
                   panel.background = element_rect(fill='transparent', color = NA),
                   plot.background = element_rect(fill='transparent', color=NA),
                   panel.grid = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.text.x = element_text(size = 12))
  }
ggsave('20221215/plots_moete20230106/alleArter/risikokategori.png', bg='transparent')

##---         2.1.2 Etableringsklasse ---####
ferdig %>%
  filter(!Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  {
    ggplot(.,
           aes(x = Etableringsklasse_comb, fill = Etableringsklasse_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
      geom_segment(aes(x = 'A', xend = 'C1', y = 625, yend = 625),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'B2', y = 650, label='Doerstokkarter'), size=4) +
      geom_segment(aes(x = 'C2', xend = 'C3E', y = 625, yend = 625),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'C2', y = 650, label='Selvstendig reproduserende'), size=4, hjust=-.01) +
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#71B581", "B2"="#71B581",
                                   "C0"="#d2c160", "C1"="#d2c160", "C2"="#d2c160",
                                   "C3E"="#e5b445")) +
      scale_x_discrete(labels = c("A" = "Forekommer ikke i Norge",
                                  "B1" = "Forekommer innendoers eller \ni lukkede installasjoner",
                                  "B2" = "Forekommer utendoers paa \neget produksjonsareal",
                                  "C0" = "Dokumentert i norsk natur",
                                  "C1" = "Overlever vinteren utendoers \nuten menneskelig tilsyn",
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
ggsave('20221215/plots_moete20230106/alleArter/etableringsklasse.png', bg='transparent')


##---         2.1.3 Aarsak til endring  ---####
ferdig_long.endring %>%
  filter(#Vurderingsomraade == "N",
         #Ekspertkomite == "Karplanter",
         #Fremmedartsstatus == "Doerstokkart" | Fremmedartsstatus == "Selvstendig reproduserende",
         Aarsak_norsk != "",) %>% {
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
             scale_x_discrete(labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger/\nretningslinjer",
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
                   #strip.background = element_rect(fill = "gray90"),
                   axis.line.x = element_blank(),
                   axis.ticks.x = element_blank(),
                   axis.text.x = element_text(angle = 45, hjust = .9, size = 12))  # +
             #facet_grid(# ~ 
               #Fremmedartsstatus ~  Vurderingsomraade )
         }
ggsave('20221215/plots_moete20230106/alleArter/aarsakEndring.png', bg='transparent')


##---         2.1.4 Endring i kategori  ---####
ferdig %>%
  rename('Risikokategori 2018' = 'Kategori2018' ,
         'Risikokategori 2023' = 'Kategori2023' ) %>%
  #filter( Vurderingsomraade =="S",
  #  Fremmedartsstatus == "Selvstendig reproduserende" )%>%
  make_long(`Risikokategori 2018`, `Risikokategori 2023`) %>%
  mutate(across(c(node, next_node),
                ~ordered(.x, levels = c("Ikke risikovurdert tidligere","NR","NK","LO","PH","HI","SE")))) %>% {
                  ggplot(., aes(x = x, 
                                next_x = next_x, 
                                node = node, 
                                next_node = next_node,
                                fill = node,
                                label = node)) +
                    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
                    geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                    scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70", "NR"="gray90", "NK"="#a6ad59", "LO"="#60a5a3",
                                                 "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e"),
                                      name = "",
                                      labels = c('Ikke risikovurdert i 2018',
                                                 'Ikke risikovurdert (NR)',
                                                 'Ingen kjent risiko (NK)',
                                                 'Lav risiko (LO)',
                                                 'Potensielt hoey risiko (PH)',
                                                 'Hoey risiko (HI)',
                                                 'Svaert hoey risiko (SE)')) +
                    labs(x = "") +
                    theme_sankey(base_size = 16) +
                    theme(legend.position="none",
                          panel.background = element_rect(fill='transparent', color = NA),
                          plot.background = element_rect(fill='transparent', color=NA))
                }
ggsave('20221215/plots_moete20230106/alleArter/endring.png', bg='transparent')


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

ggplot(cont, aes(x = Kategori2023, y = Kategori2018)) +
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
  labs(x = 'Risikokategori 2023', y = 'Risikokategori 2018') +
  geom_label(aes(0.3, Kategori2018, label = Kategori2018, fill=Kategori2018), color='white', hjust = -.25, size = 5) +
  geom_label(aes(Kategori2023, 0.5, label = Kategori2023, fill=Kategori2023), color='white', vjust = .35, size = 5) +
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

ggsave('20221215/plots_moete20230106/alleArter/endring_matrise.png', bg='transparent')


##---     2.2 Karplanter  ---####
##---         2.2.1 Risikokategori  ---####
ferdig %>%
  filter(Ekspertkomite == "Karplanter" #,
  #       Vurderingsomraade == "N",
  #       Ekspertkomite == "Karplanter",
  #       Fremmedartsstatus == "Doerstokkart"
  ) %>%
  {
    ggplot(.,
           aes(x = Kategori2023, fill = Kategori2023)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
      scale_x_discrete( #drop=FALSE,
        labels = c("NR" = "Ikke risikovurdert\nNR",
                   "NK" = "Ingen kjent risiko\nNK",
                   "LO" = "Lav risiko\nLO",
                   "PH" = "Potensielt hoey risiko\nPH",
                   "HI" = "Hoey risiko\nHI",
                   "SE" = "Svaert hoey risko\nSE")) +
      scale_fill_manual(values = c("NR"="white", "NK"="#a6ad59", "LO"="#60a5a3",
                                   "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('20221215/plots_moete20230106/karplanter/risikokategori.png', bg='transparent')

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
      geom_segment(aes(x = 'A', xend = 'C1', y = 325, yend = 325),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'B2', y = 350, label='Doerstokkarter'), size=4) +
      geom_segment(aes(x = 'C2', xend = 'C3E', y = 325, yend = 325),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'C2', y = 350, label='Selvstendig reproduserende'), size=4, hjust=-.01) +
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#71B581", "B2"="#71B581",
                                   "C0"="#d2c160", "C1"="#d2c160", "C2"="#d2c160",
                                   "C3E"="#e5b445")) +
      scale_x_discrete(labels = c("A" = "Forekommer ikke i Norge",
                                  "B1" = "Forekommer innendoers eller \ni lukkede installasjoner",
                                  "B2" = "Forekommer utendoers paa \neget produksjonsareal",
                                  "C0" = "Dokumentert i norsk natur",
                                  "C1" = "Overlever vinteren utendoers \nuten menneskelig tilsyn",
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
ggsave('20221215/plots_moete20230106/karplanter/etableringsklasse.png', bg='transparent')


##---         2.2.3 Aarsak til endring  ---####
ferdig_long.endring %>%
  filter(#Vurderingsomraade == "N",
    Ekspertkomite == "Karplanter",
    #Fremmedartsstatus == "Doerstokkart" | Fremmedartsstatus == "Selvstendig reproduserende",
    Aarsak_norsk != "",) %>% {
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
        scale_x_discrete(labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger/\nretningslinjer",
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
ggsave('20221215/plots_moete20230106/karplanter/aarsakEndring.png', bg='transparent')


##---         2.2.4 Endring i kategori  ---####
ferdig %>%
  rename('Risikokategori 2018' = 'Kategori2018' ,
         'Risikokategori 2023' = 'Kategori2023' ) %>%
  filter( Ekspertkomite =="Karplanter") %>%
  make_long(`Risikokategori 2018`, `Risikokategori 2023`) %>%
  mutate(across(c(node, next_node),
                ~ordered(.x, levels = c("Ikke risikovurdert tidligere","NR","NK","LO","PH","HI","SE")))) %>% {
                  ggplot(., aes(x = x, 
                                next_x = next_x, 
                                node = node, 
                                next_node = next_node,
                                fill = node,
                                label = node)) +
                    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
                    geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                    scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70", "NR"="gray90", "NK"="#a6ad59", "LO"="#60a5a3",
                                                 "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e"),
                                      name = "",
                                      labels = c('Ikke risikovurdert i 2018',
                                                 'Ikke risikovurdert (NR)',
                                                 'Ingen kjent risiko (NK)',
                                                 'Lav risiko (LO)',
                                                 'Potensielt hoey risiko (PH)',
                                                 'Hoey risiko (HI)',
                                                 'Svaert hoey risiko (SE)')) +
                    labs(x = "") +
                    theme_sankey(base_size = 16) +
                    theme(panel.background = element_rect(fill='transparent', color = NA),
                          plot.background = element_rect(fill='transparent', color=NA),
                          legend.position="none")
                  }
ggsave('20221215/plots_moete20230106/karplanter/endring.png', bg='transparent')


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

ggplot(cont_karplanter, aes(x = Kategori2023, y = Kategori2018)) +
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
  labs(x = 'Risikokategori 2023', y = 'Risikokategori 2018') +
  geom_label(aes(0.3, Kategori2018, label = Kategori2018, fill=Kategori2018), color='white', hjust = -.25, size = 5) +
  geom_label(aes(Kategori2023, 0.5, label = Kategori2023, fill=Kategori2023), color='white', vjust = .35, size = 5) +
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

ggsave('20221215/plots_moete20230106/karplanter/endring_matrise.png', bg='transparent')


##---     2.3 Fremmede treslag  ---####

# Vi ønsker de ovenstående plots bare for (reviderte) fremmede treslag (.xlsx-filen 'Treslag', fanen 'Treslag 2.0').
# Last inn filen og sjekk at navnene er kodet likt
treslag <- read.csv2("Treslag.csv")

# Filtrer dataframes jfr. navne på treslag
ferdig_treslag <- ferdig %>%
  filter(VitenskapeligNavn %in% treslag$VitenskapeligNavn)

ferdig_long.endring_treslag <- ferdig_long.endring %>%
  filter(VitenskapeligNavn %in% treslag$VitenskapeligNavn)


##---         2.3.1 Risikokategori  ---####
ferdig_treslag %>%
  #filter(Vurderingsomraade == "N",
  #       Ekspertkomite == "Karplanter",
  #       Fremmedartsstatus == "Doerstokkart") %>%
  {
    ggplot(.,
           aes(x = Kategori2023, fill = Kategori2023)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
      scale_x_discrete( #drop=FALSE,
        labels = c("NR" = "Ikke risikovurdert\nNR",
                   "NK" = "Ingen kjent risiko\nNK",
                   "LO" = "Lav risiko\nLO",
                   "PH" = "Potensielt hoey risiko\nPH",
                   "HI" = "Hoey risiko\nHI",
                   "SE" = "Svaert hoey risko\nSE")) +
      scale_fill_manual(values = c("NR"="white", "NK"="#a6ad59", "LO"="#60a5a3",
                                   "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('20221215/plots_moete20230106/treslag/risikokategori.png', bg='transparent')


##---         2.3.2 Etableringsklasse ---####
ferdig_treslag %>%
  filter(!Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  {
    ggplot(.,
           aes(x = Etableringsklasse_comb, fill = Etableringsklasse_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
      geom_segment(aes(x = 'A', xend = 'C1', y = 35, yend = 35),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'B2', y = 37, label='Doerstokkarter'), size=4, hjust=-.75) +
      geom_segment(aes(x = 'C2', xend = 'C3E', y = 35, yend = 35),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'C2', y = 37, label='Selvstendig reproduserende'), size=4, hjust=-.1) +
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#71B581", "B2"="#71B581",
                                   "C0"="#d2c160", "C1"="#d2c160", "C2"="#d2c160",
                                   "C3E"="#e5b445")) +
      scale_x_discrete(labels = c("A" = "Forekommer ikke i Norge",
                                  "B1" = "Forekommer innendoers eller \ni lukkede installasjoner",
                                  "B2" = "Forekommer utendoers paa \neget produksjonsareal",
                                  "C0" = "Dokumentert i norsk natur",
                                  "C1" = "Overlever vinteren utendoers \nuten menneskelig tilsyn",
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
ggsave('20221215/plots_moete20230106/treslag/etableringsklasse.png', bg='transparent')


##---         2.3.3 Aarsak til endring  ---####
ferdig_long.endring_treslag %>%
  filter(#Vurderingsomraade == "N",
    #Ekspertkomite == "Karplanter",
    #Fremmedartsstatus == "Doerstokkart" | Fremmedartsstatus == "Selvstendig reproduserende",
    Aarsak_norsk != "",) %>% {
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
        scale_x_discrete(labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger/\nretningslinjer",
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
ggsave('20221215/plots_moete20230106/treslag/aarsakEndring.png', bg='transparent')


##---         2.3.4 Endring i kategori  ---####
ferdig_treslag %>%
  rename('Risikokategori 2018' = 'Kategori2018' ,
         'Risikokategori 2023' = 'Kategori2023' ) %>%
  #filter( Vurderingsomraade =="S",
  #  Fremmedartsstatus == "Selvstendig reproduserende" )%>%
  make_long(`Risikokategori 2018`, `Risikokategori 2023`) %>%
  mutate(across(c(node, next_node),
                ~ordered(.x, levels = c("Ikke risikovurdert tidligere","NR","NK","LO","PH","HI","SE")))) %>% {
                  ggplot(., aes(x = x, 
                                next_x = next_x, 
                                node = node, 
                                next_node = next_node,
                                fill = node,
                                label = node)) +
                    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
                    geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                    scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70", "NR"="gray90", "NK"="#a6ad59", "LO"="#60a5a3",
                                                 "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e"),
                                      name = "",
                                      labels = c('Ikke risikovurdert i 2018',
                                                 'Ikke risikovurdert (NR)',
                                                 'Ingen kjent risiko (NK)',
                                                 'Lav risiko (LO)',
                                                 'Potensielt hoey risiko (PH)',
                                                 'Hoey risiko (HI)',
                                                 'Svaert hoey risiko (SE)')) +
                    labs(x = "") +
                    theme_sankey(base_size = 16) +
                    theme(legend.position="none",
                          panel.background = element_rect(fill='transparent', color = NA),
                          plot.background = element_rect(fill='transparent', color=NA))
                }
ggsave('20221215/plots_moete20230106/treslag/endring.png', bg='transparent')


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

ggplot(cont_treslag, aes(x = Kategori2023, y = Kategori2018)) +
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
  labs(x = 'Risikokategori 2023', y = 'Risikokategori 2018') +
  geom_label(aes(0.3, Kategori2018, label = Kategori2018, fill=Kategori2018), color='white', hjust = -.25, size = 5) +
  geom_label(aes(Kategori2023, 0.5, label = Kategori2023, fill=Kategori2023), color='white', vjust = .35, size = 5) +
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

ggsave('20221215/plots_moete20230106/treslag/endring_matrise.png', bg='transparent')


##---     2.4 Doerstokkarter  ---####
#---         2.4.1 Risikokategori  ---####
ferdig %>%
  filter(Fremmedartsstatus == "Doerstokkart" #,
         #       Vurderingsomraade == "N",
  ) %>%
  {
    ggplot(.,
           aes(x = Kategori2023, fill = Kategori2023)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 5) +
      scale_x_discrete( #drop=FALSE,
        labels = c("NR" = "Ikke risikovurdert\nNR",
                   "NK" = "Ingen kjent risiko\nNK",
                   "LO" = "Lav risiko\nLO",
                   "PH" = "Potensielt hoey risiko\nPH",
                   "HI" = "Hoey risiko\nHI",
                   "SE" = "Svaert hoey risko\nSE")) +
      scale_fill_manual(values = c("NR"="white", "NK"="#a6ad59", "LO"="#60a5a3",
                                   "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")) + 
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(size = 12))
  }
ggsave('20221215/plots_moete20230106/doerstokkarter/risikokategori.png', bg='transparent')


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
      #geom_segment(aes(x = 'A', xend = 'C1', y = 325, yend = 325),
      #             arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      #geom_text(aes(x = 'B2', y = 350, label='Doerstokkarter'), size=3) +
      #geom_segment(aes(x = 'C2', xend = 'C3E', y = 325, yend = 325),
      #             arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      #geom_text(aes(x = 'C2', y = 350, label='Selvstendig reproduserende'), size=3, hjust=-.01) +
      scale_fill_manual(values = c("A"="#35a3b2",
                                   "B1"="#71B581", "B2"="#71B581",
                                   "C0"="#d2c160", "C1"="#d2c160" )) + #, "C2"="#d2c160",
                                   #"C3E"="#e5b445")) +
      scale_x_discrete(labels = c("A" = "Forekommer ikke i Norge",
                                  "B1" = "Forekommer innendoers eller \ni lukkede installasjoner",
                                  "B2" = "Forekommer utendoers paa \neget produksjonsareal",
                                  "C0" = "Dokumentert i norsk natur",
                                  "C1" = "Overlever vinteren utendoers \nuten menneskelig tilsyn" )) +#,
                                  #"C2" = "Selvstendig reproduserende",
                                  #"C3E" = "Etablert i norsk natur")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.background = element_rect(fill='transparent', color = NA),
            plot.background = element_rect(fill='transparent', color=NA),
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .8, size = 12)) 
  }
ggsave('20221215/plots_moete20230106/doerstokkarter/etableringsklasse.png', bg='transparent')


##---         2.4.3 Aarsak til endring  ---####
ferdig_long.endring %>%
  filter(Fremmedartsstatus == "Doerstokkart",
    Aarsak_norsk != "",) %>% {
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
        scale_x_discrete(labels = c("Endrede avgrensninger/retningslinjer"="Endrede avgrensninger/\nretningslinjer",
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
ggsave('20221215/plots_moete20230106/doerstokkarter/aarsakEndring.png', bg='transparent')


##---         2.4.4 Endring i kategori  ---####
ferdig %>%
  rename('Risikokategori 2018' = 'Kategori2018' ,
         'Risikokategori 2023' = 'Kategori2023' ) %>%
  filter( Fremmedartsstatus == "Doerstokkart") %>%
  make_long(`Risikokategori 2018`, `Risikokategori 2023`) %>%
  mutate(across(c(node, next_node),
                ~ordered(.x, levels = c("Ikke risikovurdert tidligere","NR","NK","LO","PH","HI","SE")))) %>% {
                  ggplot(., aes(x = x, 
                                next_x = next_x, 
                                node = node, 
                                next_node = next_node,
                                fill = node,
                                label = node)) +
                    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
                    geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                    scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70", "NR"="gray90", "NK"="#a6ad59", "LO"="#60a5a3",
                                                 "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e"),
                                      name = "",
                                      labels = c('Ikke risikovurdert i 2018',
                                                 'Ikke risikovurdert (NR)',
                                                 'Ingen kjent risiko (NK)',
                                                 'Lav risiko (LO)',
                                                 'Potensielt hoey risiko (PH)',
                                                 'Hoey risiko (HI)',
                                                 'Svaert hoey risiko (SE)')) +
                    labs(x = "") +
                    theme_sankey(base_size = 16) +
                    theme(legend.position="none",
                          panel.background = element_rect(fill='transparent', color = NA),
                          plot.background = element_rect(fill='transparent', color=NA))
                }
ggsave('20221215/plots_moete20230106/doerstokkarter/endring.png', bg='transparent')


##---           2.4.4.1 Doerstokkarter fra HS ---####
ferdig %>%
  filter( Fremmedartsstatus == "Doerstokkart",
          Kategori2018 == 'NR' | Kategori2018 == 'Ikke risikovurdert tidligere') %>%
  select(Kategori2018, Kategori2023, ) %>%
  mutate(Kategori2018_edit =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",),
         .keep = "all") %>%
  rename(' ' = 'Kategori2018_edit' ,
         'Risikokategori 2023' = 'Kategori2023' ) %>%
  make_long(` `, `Risikokategori 2023`) %>%
  mutate(across(c(node, next_node),
                ~ordered(.x, levels = c("Ikke risikovurdert tidligere", "NK","LO","PH","HI","SE")))) %>% {
                  ggplot(., aes(x = x, 
                                next_x = next_x, 
                                node = node, 
                                next_node = next_node,
                                fill = node,
                                label = node)) +
                    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
                    geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                    scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70", "NK"="#a6ad59", "LO"="#60a5a3",
                                                 "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e"),
                                      name = "",
                                      labels = c('Ikke risikovurdert tidligere',
                                                 'Ingen kjent risiko (NK)',
                                                 'Lav risiko (LO)',
                                                 'Potensielt hoey risiko (PH)',
                                                 'Hoey risiko (HI)',
                                                 'Svaert hoey risiko (SE)')) +
                    labs(x = "") +
                    theme_sankey(base_size = 16) +
                    theme(legend.position="none",
                          panel.background = element_rect(fill='transparent', color = NA),
                          plot.background = element_rect(fill='transparent', color=NA))
                }
ggsave('20221215/plots_moete20230106/doerstokkarter/endring_HS.png', bg='transparent')


# Samme plot som over, men med verdier
{
# Step 1
Sankey1 <- ferdig %>%
  filter( Fremmedartsstatus == "Doerstokkart",
          Kategori2018 == 'NR' | Kategori2018 == 'Ikke risikovurdert tidligere') %>%
  select(Kategori2018, Kategori2023, ) %>%
  mutate(Kategori2018_edit =  case_when(Kategori2018 =="NR" | Kategori2018 == "Ikke risikovurdert tidligere" ~ "Ikke risikovurdert tidligere",),
         .keep = "all") %>%
  rename(' ' = 'Kategori2018_edit' ,
         'Risikokategori 2023' = 'Kategori2023' ) %>%
  make_long(` `, `Risikokategori 2023`) %>%
  mutate(across(c(node, next_node),
                ~ordered(.x, levels = c("Ikke risikovurdert tidligere", "NK","LO","PH","HI","SE"))))

# Step 2
Sankey2 <- Sankey1%>%
  dplyr::group_by(node)%>%
  tally()

# Step 3
Sankey3 <- merge(Sankey1, Sankey2, by.x = 'node', by.y = 'node', all.x = TRUE)

# Plot - OBS PÅ PLACERING OG ANTALL GJENTAGELSER AV LABELS - MÅ FIKSES MANUELT
ggplot(Sankey3, aes(x = x, 
                    next_x = next_x, 
                    node = node, 
                    next_node = next_node,
                    fill = node,
                    label = paste0(node,",\nn=", n) )) +
  geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
  geom_sankey_label(aes(x = c(rep(2.1,53), rep(.78,592), rep(2.1,283), rep(2.1,130), rep(2.1,83), rep(2.1,43))),
                    size = 3.5, color = 1, fill = "white") +
  scale_fill_manual(values = c("Ikke risikovurdert tidligere"="gray70", "NK"="#a6ad59", "LO"="#60a5a3",
                               "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e"),
                    name = "",
                    labels = c('Ikke risikovurdert tidligere',
                               'Ingen kjent risiko (NK)',
                               'Lav risiko (LO)',
                               'Potensielt hoey risiko (PH)',
                               'Hoey risiko (HI)',
                               'Svaert hoey risiko (SE)')) +
  labs(x = "") +
  theme_sankey(base_size = 16) +
  theme(legend.position="none",
        panel.background = element_rect(fill='transparent', color = NA),
        plot.background = element_rect(fill='transparent', color=NA))
}
ggsave('20221215/plots_moete20230106/doerstokkarter/endring_HS2.png', bg='transparent')


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

ggplot(cont_DS, aes(x = Kategori2023, y = Kategori2018)) +
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
  labs(x = 'Risikokategori 2023', y = 'Risikokategori 2018') +
  geom_label(aes(0.3, Kategori2018, label = Kategori2018, fill=Kategori2018), color='white', hjust = -.25, size = 5) +
  geom_label(aes(Kategori2023, 0.5, label = Kategori2023, fill=Kategori2023), color='white', vjust = .35, size = 5) +
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

ggsave('20221215/plots_moete20230106/doerstokkarter/endring_matrise.png', bg='transparent')

