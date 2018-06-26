#Accesso ai dati
# Output: stff = Selected (best Segm_length) TLS derived within and between row crossectional diameters
library(tidyverse)
library(magrittr)
library(broom)

#TLSstem_vertical_crosssection
source("ReadData.R")
tff <- as.tibble(TLSderivedDiam) %>%
  # Trees in different treatments are different subjects, better using distinct Ids
  mutate(TreeId = paste0(case_when(tesi == "V400" ~ "A",
                                   tesi == "V450" ~ "B",
                                   tesi == "V500" ~ "C"),
                         right(as.character(treid),2))) %>%
dplyr::rename(Treat = tesi,                # distance between rows: Treatment
         Segm_length = length_toppo,  # length of the segments
         Sect_height = slice)         # height along the stem of diameter measurements (cross Sections)

tff <- tff %>% filter(Sect_height <= 5.5)

tff <- tff %>%
  inner_join(stems_basic_measurements,
             by = c("Treat" = "Tesi", "treid" = "TreeId"))

# This \!/ stem, above \!/ this treshold is out of range
trsh.C05         <-    4.7
tff <- tff %>%
  filter(!(TreeId == 'C05' & Sect_height >= trsh.C05))
# This \!/ stem, above \!/ this treshold is out of range
trsh.C10         <-    3.8
tff <- tff %>%
  filter(!(TreeId == 'C10' & Sect_height >= trsh.C10))

tff <- mutate(tff, delta_d = diam_btw_rows - diam_wti_rows)
oll <- 0.1 # optimal Log Length for profiles discrimination
# Segm_length ottimale: compromesso tra min lag correlation e conservazione del dettaglio della forma
stff <- tff  %>%
  filter(Segm_length == oll)

stff <- stff %>%
  mutate(delta_d_cm = delta_d * 100)  
# !!!! PER ESPRIMERE IN cm!! LE MISURE SONO IN m, VERO??

