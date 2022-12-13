###----------------------------------------###
###--  FORELOEPIGE RESULTATER FRA FAL  ---####
###----------------------------------------###

library(tidyverse)
library(ggplot2)
library(data.table)
library(ggalluvial)
library(ggsankey)

# Les eksportfilen fra mappe; rett æ, ø og å før filen leses
alle_arter <- read.csv2("20221213/eksportabsoluteall.csv")

###--   1. RYDDING  ---####

# Behold bare ferdigstilte arter
ferdig <- alle_arter %>%
  filter(Vurderinsstatus == "finished")

# Definer rette faktor-nivåer i rett rekkefølge
ferdig$Kategori2018[ferdig$Kategori2018==""] <- "Ikke angitt"
ferdig$Kategori2023[ferdig$Kategori2023==""] <- "Ikke angitt"

ferdig <- ferdig %>%
  # Risikokategorier
  mutate(across(c(Kategori2023, Kategori2018),
                                   ~ordered(.x, levels = c("Ikke angitt","NR","NK","LO","PH","HI","SE")))) %>%
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
  pivot_longer(c("effektKrit1", "effektKrit2", "effektKrit3"),
               names_to = "effektKritNo",
               values_to = "effektKritLong")  %>%
  # Fjern tomme rekker/ingen effekt
  filter(effektKritLong %in% c("D","E","F","G","H","I"))


# Split kolonnen for Årsak til endring i kategori så det ikke finnes kombinerte kategorier
ferdig_long.endring <- ferdig %>%
  separate_rows(AarsakTilEndringIKategori, sep = "; ") %>%
  mutate(Aarsak_norsk =  case_when(AarsakTilEndringIKategori =="realChange"  ~ "Reell endring",
                                   AarsakTilEndringIKategori =="newInformation"  ~ "Ny kunnskap",
                                   AarsakTilEndringIKategori =="newInterpretation"  ~ "Ny tolkning av data",
                                   AarsakTilEndringIKategori =="changedCriteria"  ~ "Endrede avgrensninger/retningslinjer",
                                   AarsakTilEndringIKategori =="changedCriteriaInterpretation"  ~ "Endret tolkning av retningslinjer",
                                   AarsakTilEndringIKategori =="changedStatus"  ~ "Endret status"), .keep = "all")

##---       1.3 Long-format, alle kriterier  ---####
# For å lage et plot med alle kriterier i et, lag ny konvertering til long-format; først samle kriterier, splitte i etterkant

## TBC


  
##------------------------------------------------------------------------------------------------####
##---   2. PLOTS  ---####
##---       2.1 Risikokategori  ---####
ggplot(data = ferdig, aes(x = Kategori2023, fill = Kategori2023)) +
  geom_bar(color = 'black') +
  labs(x = "Risikokategori 2023", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
  scale_fill_manual(values = c("Ikke angitt"="gray80", "NR"="white", "NK"="#a6ad59", "LO"="#60a5a3",
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
         Fremmedartsstatus == "Selvstendig reproduserende") %>% {
    ggplot(.,
           aes(x = Kategori2023, fill = Kategori2023)) +
      geom_bar(color = 'black') +
      labs(x = "Risikokategori 2023", y = "") +
      geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
      scale_x_discrete(drop=FALSE) +
      scale_fill_manual(values = c("Ikke angitt"="gray80", "NR"="white", "NK"="#a6ad59", "LO"="#60a5a3",
                                   "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")) + 
             theme_minimal() +
             theme(legend.position="none",
                   panel.grid = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
           }

##---       2.2 Etableringsklasse  ---####
ggplot(data = ferdig %>% filter(!Kategori2023 %in% c("Ikke angitt", "NR"))  # Tag bort arter som per definisjon ikke har en etableringsklasse
       , aes(x = Etableringsklasse, fill = Etableringsklasse)) +
  geom_bar(color = 'black') +
  labs(x = "Etableringsklasse", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
  scale_fill_grey(start = .2, end = 1) +
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
         !Kategori2023 %in% c("Ikke angitt", "NR")) %>% {
           ggplot(.,
                  aes(x = Etableringsklasse, fill = Etableringsklasse)) +
             geom_bar(color = 'black') +
             labs(x = "Etableringsklasse", y = "") +
             geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
             #scale_x_discrete(drop=FALSE) +
             scale_fill_grey(start = .2, end = 1) +
             theme_minimal() +
             theme(legend.position="none",
                   panel.grid = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
         }

##---       2.3 Endring i kategori ; flowchart  ---####
ferdig %>%
  #filter(Fremmedartsstatus == "Selvstendig reproduserende",
  #       Vurderingsomraade =="S") %>%
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

##--- 4. Utslagsgivende kriteriet
##---       2.4 Utslagsgivende kriterier  ---####
##---           2.4.1 Invasjonspotensiale      ---####
ggplot(data = ferdig %>%
         filter(!Kategori2023 %in% c("Ikke angitt", "NR")) %>% # Tag bort arter som per definisjon ikke har utslagsgivende kriterier
         filter(!is.na(invKrit_comb)),   # For nettopp disse gir det ikke mening å vise NA/lite invasjonspotensiale
       aes(x = invKrit_comb, fill = invKrit_comb)) +
  geom_bar(color = 'black') +
  labs(x = "Kriterier, invasjonspotensiale", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
  scale_fill_grey(start = .2, end = 1) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Med mulighet for å filtrere ; innsett filtre i de første linjene
ferdig %>%
  filter(Vurderingsomraade == "N",
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
             scale_fill_grey(start = .2, end = 1) +
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
  scale_fill_grey(start = .2, end = 1) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Med mulighet for å filtrere ; innsett filtre i de første linjene
ferdig %>%
  filter(Vurderingsomraade == "N",
         #Ekspertkomite == "Karplanter",
         Fremmedartsstatus == "Doerstokkart",
         !is.na(effektKrit),
         !Kategori2023 %in% c("Ikke angitt", "NR")) %>% {
           ggplot(.,
                  aes(x = effektKrit, fill = effektKrit)) +
             geom_bar(color = 'black') +
             labs(x = "Kriterier, oekologisk effekt", y = "") +
             geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
             scale_x_discrete(drop=FALSE) +
             scale_fill_grey(start = .2, end = 1) +
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
  scale_fill_grey(start = .2, end = .9) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# Med mulighet for å filtrere ; innsett filtre i de første linjene
ferdig_long.effekt %>%
  filter(effektKritNo != "",
         Vurderingsomraade == "N",
         #Ekspertkomite == "Karplanter",
         Fremmedartsstatus == "Doerstokkart",
         !Kategori2023 %in% c("Ikke angitt", "NR")) %>% {
           ggplot(.,
                  aes(x = effektKritLong, fill = effektKritLong)) +
             geom_bar(color = 'black') +
             labs(x = "Kriterier, oekologsk effekt", y = "") +
             geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
             scale_fill_grey(start = .2, end = .9) +
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
  scale_fill_grey(start = .2, end = .9) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = .8))

# Med mulighet for å filtrere ; innsett filtre i de første linjene
ferdig_long.endring %>%
  filter(Aarsak_norsk != "",
         Vurderingsomraade == "N",
         #Ekspertkomite == "Karplanter",
         Fremmedartsstatus == "Doerstokkart",
         !Kategori2023 %in% c("Ikke angitt", "NR")) %>% {
           ggplot(.,
                  aes(x = Aarsak_norsk, fill = Aarsak_norsk)) +
             geom_bar(color = 'black') +
             labs(x = "Aarsak til endring", y = "") +
             geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
             scale_x_discrete(drop=FALSE) +
             scale_fill_grey(start = .2, end = .9) +
             theme_minimal() +
             theme(legend.position="none",
                   panel.grid = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.text.x = element_text(angle = 45, hjust = .8))
         }

##---       2.6 Fremmedartsstatus  ---####
ggplot(data = ferdig %>% filter(!Kategori2023 %in% c("Ikke angitt")),
       aes(x = Fremmedartsstatus, fill = Fremmedartsstatus)) +
  geom_bar(color = 'black') +
  labs(x = "Fremmedartsstatus", y = "") +
  geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
  scale_x_discrete(drop=FALSE) +
  scale_fill_grey(start = .2, end = 1) +
  theme_minimal() +
  theme(legend.position="none",
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90))
