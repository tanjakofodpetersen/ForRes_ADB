###----------------------------------------###
###--  FORELOEPIGE RESULTATER FRA FAL  ---####
###----------------------------------------###

library(tidyverse)
library(ggplot2)
library(data.table)
library(ggalluvial)
library(ggsankey)

# Les eksportfilen fra mappe
### Rett æ, ø og å før filen leses
alle_arter <- read.csv2("20221215/eksportabsoluteall.csv")

###--   1. RYDDING  ---####

# Behold bare ferdigstilte arter
ferdig <- alle_arter %>%
  filter(Vurderinsstatus == "finished")

ferdig %>%
  select(Ekspertkomite, Vurderinsstatus,VitenskapeligNavn, NorskNavn, SistEndretAv,
         Kategori2018, Kategori2023, AarsakTilEndringIKategori) %>%
  filter(Kategori2023 == "" )
# Noe rart ved at det finnes tomme kategorier for 2023; dette burde ikke være mulig.
# Disse står som NR i FAB. Sjekk data manuelt - fiks her

# Definer rette faktor-nivåer i rett rekkefølge
ferdig$Kategori2018[ferdig$Kategori2018==""] <- "Ikke angitt"
ferdig$Kategori2023[ferdig$Kategori2023==""] <- "NR"

ferdig <- ferdig %>%
  # Risikokategorier
  mutate(across(c(Kategori2023, Kategori2018),
                                   ~ordered(.x, levels = c("NR","NK","LO","PH","HI","SE")))) %>%
  # Fremmedartsstatus
  mutate(across(c(Fremmedartsstatus),
                ~ordered(.x, levels = c("Selvstendig reproduserende", "Regionalt fremmed", "Doerstokkart",
                                        "Effekt uten selvstendig reproduksjon", "Vurderes paa et annet taksonomisk nivaa",
                                        "Etablert per 1800", "Feilbestemt i 2018", "Ikke definert", "Ikke fremmed", "")))) %>%
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

##------------------------------------------------------------------------------------------------####
##---   2. PLOTS  ---####
##---       2.1 Risikokategori  ---####
ggplot(data = ferdig, aes(x = Kategori2023, fill = Kategori2023)) +
  geom_bar(color = 'black') +
  labs(x = "Risikokategori 2023", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
  scale_fill_manual(values = c("NR"="white", "NK"="#a6ad59", "LO"="#60a5a3",
                               "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Med mulighet for å filtrere ; innsett filtre i de første linjene
ferdig %>%
  filter(Vurderingsomraade == "N",
         #Ekspertkomite == "Karplanter",
         Fremmedartsstatus == "Doerstokkart") %>% {
    ggplot(.,
           aes(x = Kategori2023, fill = Kategori2023)) +
      geom_bar(color = 'black') +
      labs(x = "Risikokategori 2023", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
      scale_x_discrete(drop=FALSE) +
      scale_fill_manual(values = c("NR"="white", "NK"="#a6ad59", "LO"="#60a5a3",
                                   "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")) + 
             theme_minimal() +
             theme(legend.position="none",
                   panel.grid = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
         }

# Filter og fasetering
ferdig %>%
  filter(#Vurderingsomraade == "N",
         #Ekspertkomite == "Karplanter",
         Fremmedartsstatus == "Doerstokkart" | Fremmedartsstatus == "Selvstendig reproduserende") %>% 
  {
           ggplot(.,
                  aes(x = Kategori2023, fill = Kategori2023)) +
             geom_bar(color = 'black') +
             labs(x = "Risikokategori 2023", y = "") +
             geom_text(stat='count', aes(label=after_stat(count)), vjust=-.5, size = 3) +
             scale_x_discrete(drop=FALSE) +
             scale_fill_manual(values = c("NR"="white", "NK"="#a6ad59", "LO"="#60a5a3",
                                          "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")) + 
             theme_minimal() +
             theme(legend.position="none",
                   panel.grid = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank()) +
             facet_grid(# ~ 
                         Fremmedartsstatus ~  Vurderingsomraade )
                         #scales = "free_y")
         }

##---       2.2 Etableringsklasse  ---####
ggplot(data = ferdig %>% filter(!Kategori2023 %in% c("Ikke angitt", "NR"))  # Tag bort arter som per definisjon ikke har en etableringsklasse
       , aes(x = Etableringsklasse, fill = Etableringsklasse)) +
  geom_bar(color = 'black') +
  labs(x = "Etableringsklasse", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
  #scale_fill_grey(start = .2, end = 1) +
  scale_fill_manual(values = c("A"="#35a3b2",
                               "B1"="#71B581", "B2"="#71B581",
                               "C0"="#d2c160", "C1"="#d2c160", "C2"="#d2c160",
                               "C3"="#e5b445","D1"="#e5b445", "D2"="#e5b445", "E"="#e5b445")) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Med mulighet for å filtrere ; innsett filtre i de første linjene
ferdig %>%
  filter(Vurderingsomraade == "N",
         #Ekspertkomite == "Karplanter",
         #Fremmedartsstatus == "Doerstokkart",
         !Kategori2023 %in% c("Ikke angitt", "NR")) %>% {
           ggplot(.,
                  aes(x = Etableringsklasse, fill = Etableringsklasse)) +
             geom_bar(color = 'black') +
             labs(x = "Etableringsklasse", y = "") +
             geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
             #scale_x_discrete(drop=FALSE) +
             #scale_fill_grey(start = .2, end = 1) +
             scale_fill_manual(values = c("A"="#35a3b2",
                                          "B1"="#71B581", "B2"="#71B581",
                                          "C0"="#d2c160", "C1"="#d2c160", "C2"="#d2c160",
                                          "C3"="#e5b445","D1"="#e5b445", "D2"="#e5b445", "E"="#e5b445")) +
             theme_minimal() +
             theme(legend.position="none",
                   panel.grid = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
         }

##---       2.3 Endring i kategori ; flowchart  ---####
ferdig %>%
  #filter(Fremmedartsstatus == "Doerstokkart" ,
         # Vurderingsomraade =="S") %>%
  make_long(Kategori2018, Kategori2023) %>%
  mutate(across(c(node, next_node),
                ~ordered(.x, levels = c("Ikke angitt","NR","NK","LO","PH","HI","SE")))) %>% {
                  ggplot(., aes(x = x, 
                                next_x = next_x, 
                                node = node, 
                                next_node = next_node,
                                fill = node,
                                label = node)) +
                    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
                    geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                    scale_fill_manual(values = c("Ikke angitt"="gray70", "NR"="gray90", "NK"="#a6ad59", "LO"="#60a5a3",
                                                 "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")) +
                    labs(x = "") +
                    theme_sankey(base_size = 16) +
                    theme(legend.position="none")
                }

## Innsett verdier ; innsett filtre under "Step 1"
# Step 1
Sankey1 <- ferdig %>%
  #filter(Fremmedartsstatus == "Doerstokkart" ,
  # Vurderingsomraade =="S") %>%
  make_long(Kategori2018, Kategori2023) %>%
  mutate(across(c(node, next_node),
                ~ordered(.x, levels = c("Ikke angitt","NR","NK","LO","PH","HI","SE"))))

# Step 2
Sankey2 <- Sankey1%>%
  dplyr::group_by(node)%>%
  tally()

# Step 3
Sankey3 <- merge(Sankey1, Sankey2, by.x = 'node', by.y = 'node', all.x = TRUE)

ggplot(data = Sankey3, aes(x = x, 
                                next_x = next_x, 
                                node = node, 
                                next_node = next_node,
                                fill = node,
                                label = paste0(node,", n=", n) )) +
                    geom_sankey(flow.alpha = 0.75, node.color = 0.9) +
                    geom_sankey_label(size = 3.5, color = 1, fill = "white") +
                    scale_fill_manual(values = c("Ikke angitt"="gray70", "NR"="gray90", "NK"="#a6ad59", "LO"="#60a5a3",
                                                 "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")) +
                    labs(x = "") +
                    theme_sankey(base_size = 16) +
                    theme(legend.position="none")
            


##---       2.4 Utslagsgivende kriterier  ---####
##---           2.4.1 Invasjonspotensiale      ---####
ggplot(data = ferdig %>%
         filter(!Kategori2023 %in% c("Ikke angitt", "NR")) %>% # Tag bort arter som per definisjon ikke har utslagsgivende kriterier
         filter(!is.na(invKrit_comb)),   # For nettopp disse gir det ikke mening å vise NA/lite invasjonspotensiale
       aes(x = invKrit_comb, fill = invKrit_comb)) +
  geom_bar(color = 'black') +
  labs(x = "Kriterier, invasjonspotensiale", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
  #scale_fill_grey(start = .2, end = 1) +
  scale_fill_manual(values = c("AxB"="#35a3b2", "C"="#35a3b2",
                               "Lite invasjonspotensiale"="gray80")) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Med mulighet for å filtrere ; innsett filtre i de første linjene
ferdig %>%
  filter(Vurderingsomraade == "S",
         #Ekspertkomite == "Karplanter",
         Fremmedartsstatus == "Doerstokkart",
         !is.na(invKrit_comb),
         !Kategori2023 %in% c("Ikke angitt", "NR")) %>% {
           ggplot(.,
                  aes(x = invKrit_comb, fill = invKrit_comb)) +
             geom_bar(color = 'black') +
             labs(x = "Kriterier, invasjonspotensiale", y = "") +
             geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
             scale_x_discrete(drop=FALSE) +
             #scale_fill_grey(start = .2, end = 1) +
             scale_fill_manual(values = c("AxB"="#35a3b2", "C"="#35a3b2",
                                          "Lite invasjonspotensiale"="gray80")) +
             theme_minimal() +
             theme(legend.position="none",
                   panel.grid = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
         }

##---           2.4.2 Økologisk effekt      ---####
ggplot(data = ferdig %>%
         filter(!Kategori2023 %in% c("Ikke angitt", "NR")) %>% # Tag bort arter som per definisjon ikke har utslagsgivende kriterier
         filter(!is.na(effektKrit)),   # For nettopp disse gir det ikke mening å vise NA
       aes(x = effektKrit, fill = effektKrit)) +
  geom_bar(color = 'black') +
  labs(x = "Kriterier, oekologisk effekt", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
  #scale_fill_grey(start = .2, end = 1) +
  scale_fill_manual(values = c("Ingen effekt"="gray80","F"="#e5b445","G"="#e5b445","DE"="#e5b445","EF"="#e5b445","I"="#e5b445","D"="#e5b445",
                               "E"="#e5b445","DGI"="#e5b445","DF"="#e5b445","H"="#e5b445","EI"="#e5b445","DH"="#e5b445","EH"="#e5b445",
                               "DI"="#e5b445","DEF"="#e5b445","DHI"="#e5b445","EG"="#e5b445","FI"="#e5b445")) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Med mulighet for å filtrere ; innsett filtre i de første linjene
ferdig %>%
  filter(Vurderingsomraade == "N",
         #Ekspertkomite == "Karplanter",
         Fremmedartsstatus == "Selvstendig reproduserende",
         !is.na(effektKrit),
         !Kategori2023 %in% c("Ikke angitt", "NR")) %>% {
           ggplot(.,
                  aes(x = effektKrit, fill = effektKrit)) +
             geom_bar(color = 'black') +
             labs(x = "Kriterier, oekologisk effekt", y = "") +
             geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
             scale_x_discrete(drop=FALSE) +
             #scale_fill_grey(start = .2, end = 1) +
             scale_fill_manual(values = c("Ingen effekt"="gray80","F"="#e5b445","G"="#e5b445","DE"="#e5b445","EF"="#e5b445","I"="#e5b445","D"="#e5b445",
                                          "E"="#e5b445","DGI"="#e5b445","DF"="#e5b445","H"="#e5b445","EI"="#e5b445","DH"="#e5b445","EH"="#e5b445",
                                          "DI"="#e5b445","DEF"="#e5b445","DHI"="#e5b445","EG"="#e5b445","FI"="#e5b445")) +
             theme_minimal() +
             theme(legend.position="none",
                   panel.grid = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
         }

### Plots med delte økologiske effekter (obs på sum, en art kan ha fler kategorier)
ggplot(data = ferdig_long.effekt %>%
         filter(effektKritLong != ""),  # Fjerne rekker uten endring i kategori
       aes(x = effektKritLong, fill = effektKritLong)) +
  geom_bar(color = 'black') +
  labs(x = "Kriterier, oekologisk effekt", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
  #scale_fill_grey(start = .2, end = .9) +
  scale_fill_manual(values = c("Ingen effekt"="gray80","F"="#e5b445","G"="#e5b445","I"="#e5b445","D"="#e5b445",
                               "E"="#e5b445","H"="#e5b445")) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Med mulighet for å filtrere ; innsett filtre i de første linjene
ferdig_long.effekt %>%
  filter(effektKritNo != "",
         #Vurderingsomraade == "N",
         #Ekspertkomite == "Karplanter",
         Fremmedartsstatus == "Doerstokkart",
         !Kategori2023 %in% c("Ikke angitt", "NR")) %>% {
           ggplot(.,
                  aes(x = effektKritLong, fill = effektKritLong)) +
             geom_bar(color = 'black') +
             labs(x = "Kriterier, oekologsk effekt", y = "") +
             geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
             #scale_fill_grey(start = .2, end = .9) +
             scale_fill_manual(values = c("Ingen effekt"="gray80","F"="#e5b445","G"="#e5b445","I"="#e5b445","D"="#e5b445",
                                          "E"="#e5b445","H"="#e5b445")) +
             theme_minimal() +
             theme(legend.position="none",
                   panel.grid = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
         }


##---       2.5 Årsak til endring  ---####
ggplot(data = ferdig_long.endring %>%
         filter(Aarsak_norsk != ""),  # Fjerne rekker uten endring i kategori
       aes(x = Aarsak_norsk, fill = Aarsak_norsk)) +
  geom_bar(color = 'black') +
  labs(x = "Aarsak til endring", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
  #scale_fill_grey(start = .2, end = .9) +
  scale_fill_manual(values = c("Endrede avgrensninger/retningslinjer"="#35a3b2",
                               "Endret status"="#5FB7B1",
                               "Endret tolkning av retningslinjer"="#71B581",
                               "Ny kunnskap"="#A0BA5B",
                               "Ny tolkning av data"="#d2c160",
                               "Reell endring"="#e5b445")) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = .8))

# Med mulighet for å filtrere ; innsett filtre i de første linjene
ferdig_long.endring %>%
  filter(Aarsak_norsk != "",
         #Vurderingsomraade == "N",
         #Ekspertkomite == "Karplanter",
         Fremmedartsstatus == "Doerstokkart",
         !Kategori2023 %in% c("Ikke angitt", "NR")) %>% {
           ggplot(.,
                  aes(x = Aarsak_norsk, fill = Aarsak_norsk)) +
             geom_bar(color = 'black') +
             labs(x = "Aarsak til endring", y = "") +
             geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
             scale_x_discrete(drop=FALSE) +
             #scale_fill_grey(start = .2, end = .9) +
             scale_fill_manual(values = c("Endrede avgrensninger/retningslinjer"="#35a3b2",
                                          "Endret status"="#5FB7B1",
                                          "Endret tolkning av retningslinjer"="#71B581",
                                          "Ny kunnskap"="#A0BA5B",
                                          "Ny tolkning av data"="#d2c160",
                                          "Reell endring"="#e5b445")) +
             theme_minimal() +
             theme(legend.position="none",
                   panel.grid = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.text.x = element_text(angle = 45, hjust = .8))
         }

##---       2.6 Fremmedartsstatus  ---####
ggplot(data = ferdig ,
       aes(x = Fremmedartsstatus, fill = Fremmedartsstatus)) +
  geom_bar(color = 'black') +
  labs(x = "Fremmedartsstatus", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
  scale_x_discrete(drop=FALSE) +
  #scale_fill_grey(start = .2, end = 1) +
  scale_fill_manual(values = c("Selvstendig reproduserende"="#e5b445",
                               "Regionalt fremmed"="#e5b445",
                               "Doerstokkart"="#35a3b2",
                               "Effekt uten selvstendig reproduksjon"="#35a3b2",
                               "Vurderes paa et annet taksonomisk nivaa"="#e5b445",
                               "Etablert per 1800"="gray80",
                               "Feilbestemt i 2018"="gray80",
                               "Ikke definert"="white",
                               "Ikke fremmed"="gray80",
                               " "="white")) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90))

# Med filtrering
ferdig %>%
  filter(#Fremmedartsstatus == "Doerstokkart",
    #Ekspertkomite == "Karplanter",
    Vurderingsomraade == "S") %>% {
      ggplot(.,
       aes(x = Fremmedartsstatus, fill = Fremmedartsstatus)) +
  geom_bar(color = 'black') +
  labs(x = "Fremmedartsstatus", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
  scale_x_discrete(drop=FALSE) +
  #scale_fill_grey(start = .2, end = 1) +
  scale_fill_manual(values = c("Selvstendig reproduserende"="#e5b445",
                               "Regionalt fremmed"="#e5b445",
                               "Doerstokkart"="#35a3b2",
                               "Effekt uten selvstendig reproduksjon"="#35a3b2",
                               "Vurderes paa et annet taksonomisk nivaa"="#e5b445",
                               "Etablert per 1800"="gray80",
                               "Feilbestemt i 2018"="gray80",
                               "Ikke definert"="white",
                               "Ikke fremmed"="gray80",
                               " "="white")) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90))
    }
