###----------------------------------------###
###--  FORELOEPIGE RESULTATER FRA FAL  ---####
###----------------------------------------###

library(tidyverse)
library(ggplot2)
library(data.table)

# Les eksportfilen fra mappe
alle_arter <- read.csv2("20221213/eksportabsoluteall.csv")

###-- 1. RYDDING  ---####

# Behold bare ferdigstilte arter
ferdig <- alle_arter %>%
  filter(Vurderinsstatus == "finished")

# Definer rette faktor-nivåer i rett rekkefølge
ferdig <- ferdig %>% mutate(across(c(Kategori2023, Kategori2018),
                                   ~ordered(.x, levels = c("","NR","NK","LO","PH","HI","SE"))))


##--- 2. PLOTS  ---####
##--- 2.1 Risikokategori  --####
ggplot(data = ferdig, aes(x = Kategori2023, fill = Kategori2023)) +
  geom_bar(color = 'black') +
  labs(x = "Risikokategori", y = "Antall") +
  scale_fill_manual(values = c("gray20", "white", "#a6ad59", "#60a5a3", "#1b586c", "#233368", "#602d5e")) +
  theme_minimal()

  c(""="gray20", "NR"="white", "NK"="#a6ad59", "LO"="#60a5a3", "PH"="#1b586c", "HI"="#233368", "SE"="#602d5e")