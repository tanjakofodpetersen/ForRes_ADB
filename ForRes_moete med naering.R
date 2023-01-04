###--------------------------------------------------------####
###--         FORELOEPIGE RESULTATER FRA FAL            ---####
###--   Modifisert script til figurer til presentasjon  ---####
###--   i moete med med hage- og skognaeringen          ---####
###--------------------------------------------------------####

library(tidyverse)
library(ggplot2)
library(data.table)
library(ggalluvial)
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
# Disse står som NR i FAB. Sjekk data manuelt - fiks her. Dette gir ogse en feil i fremmedartsstatus som skal fikses her

# Definer rette faktor-nivåer i rett rekkefølge
ferdig$Kategori2018[ferdig$Kategori2018==""] <- "Ikke vurdert"
ferdig$Kategori2023[ferdig$Kategori2023==""] <- "NR"
ferdig$Fremmedartsstatus[ferdig$Fremmedartsstatus==""] <- "Ikke fremmed"

ferdig <- ferdig %>%
  # Risikokategorier
  mutate(across(c(Kategori2023, Kategori2018),
                ~ordered(.x, levels = c("Ikke vurdert","NR","NK","LO","PH","HI","SE")))) %>%
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
  

##------------------------------------------------------------------------------------------------####
##---   2. PLOTS  ---####
##---     2.1 Alle arter  ---####
##---       2.1.1 Etableringsklasse ---####
ferdig %>%
  filter(!Kategori2023 == "NR") %>%    # Tas ikke med i første runden
  {
    ggplot(.,
           aes(x = Etableringsklasse_comb, fill = Etableringsklasse_comb)) +
      geom_bar(color = 'black') +
      labs(x = "", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
      geom_segment(aes(x = 'A', xend = 'C1', y = 625, yend = 625),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'B2', y = 650, label='Doerstokkarter'), size=3) +
      geom_segment(aes(x = 'C2', xend = 'Mangler', y = 625, yend = 625),
                   arrow = arrow(angle=90, ends='both', length = unit(.25, 'cm')), linewidth = .5) +
      geom_text(aes(x = 'C3E', y = 650, label='Selvstendig reproduserende'), size=3) +
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
                                  "C3E" = "Etablert i norsk natur",
                                  "Mangler" = "Etableringsklasse mangler")) +
      theme_minimal() +
      theme(legend.position="none",
            panel.grid = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = .8)) 
  }

##---       2.1.2 Aarsak til endring  ---####

