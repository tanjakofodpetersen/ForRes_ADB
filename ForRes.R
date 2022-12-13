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

###-- 1. RYDDING  ---####

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
  


##--- 2. PLOTS  ---####
##--- 2.1 Risikokategori  ---####
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
         Ekspertkomite == "Karplanter",
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

##--- 2.2 Etableringsklasse  ---####
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
  filter(Vurderingsomraade == "N",
         Ekspertkomite == "Karplanter",
         #Fremmedartsstatus == "Selvstendig reproduserende",
         !Kategori2023 %in% c("Ikke angitt", "NR")) %>% {
           ggplot(.,
                  aes(x = Etableringsklasse, fill = Etableringsklasse)) +
             geom_bar(color = 'black') +
             labs(x = "Etableringsklasse", y = "Antall") +
             geom_text(stat='count', aes(label=after_stat(count)), vjust=-1, size = 3) +
             scale_x_discrete(drop=FALSE) +
             scale_fill_grey(start = .2, end = 1) +
             theme_minimal() +
             theme(legend.position="none",
                   panel.grid = element_blank(),
                   axis.text.y = element_blank(),
                   axis.ticks.y = element_blank())
         }


##--- 3. Endring i kategori ; flowchart  ---####

ferdig %>%
  #filter(Ekspertkomite == "Karplanter") %>%
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