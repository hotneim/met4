
# ---------------------------------- #
# R-SCRIPT                           #
# MET4 - EMPIRISKE METODER           #
# NHH NORGES HANDELSH�YSKOLE         #
#                                    #
# MODUL 1: GRUNNLEGGENDE STATISTIKK  #
# ---------------------------------- #

## DEL I: DESKRIPTIV STATISTIKK --------- 

# Laster inn data for sp�rreunders�kelsen
library(readr)
library(dplyr)

survey <- read_csv("data_survey.csv")

# Sjekker datatyper
str(survey)

# M�l for sentraltendens
mean(survey$age)
median(survey$age)

median(survey$religious)
median(survey$religious, na.rm = TRUE)

table(survey$party)                       # Kan lese av typetallet manuelt.
table(survey$party) %>% which.max()       # Eller bruke funksjonen which.max()

# M�l for spredning
fishermen <- 
  read_csv("data_fishermen_mercury.csv")

# Gjennomsnitt og st. avvik
mean(fishermen$total_mercury)
sd(fishermen$total_mercury)

# Kvantiler
quantile(fishermen$total_mercury, 0.20)

# Kvartilene
quantile(fishermen$total_mercury) 

# Bokplot
library(ggplot2)

# En variabel
fishermen %>% 
  ggplot +
  geom_boxplot(aes(y = total_mercury)) 

# Deler opp i grupper basert p� en annen variabel
fishermen %>% 
  ggplot +
  geom_boxplot(aes(y = total_mercury, x = as.factor(fisherman))) 

# Her er en side som viser hvordan vi kan jobbe med boksplottet v�rt for � gj�re det penere:
# http://www.sthda.com/english/wiki/ggplot2-box-plot-quick-start-guide-r-software-and-data-visualization
#
# For eksempel:

fishermen %>% 
  ggplot +
  geom_boxplot(aes(x = as.factor(fisherman), y = total_mercury, fill = as.factor(fisherman))) +
  xlab("Fisker") +
  ylab("Andel kvikks�lv") +
  labs(fill = "Fisker?") +
  theme_classic()

# Presentasjon av kategoridata
table(survey$religious)     # Frekvenstabell 

# Krysstabell med to kategorivariabler:
survey %>% 
  select(party, religious) %>% 
  table

# Den grafiske varianten av frekvenstabellen er s�ylediagrammet
survey %>% 
  ggplot +
  geom_bar(aes(x = as.factor(religious)))

# Her er en bearbeidet variant:
survey %>% 
  ggplot +
  geom_bar(aes(x = as.factor(religious))) +
  xlab("Religi�sitet (Skala 0 -- 10)") +
  ylab("Antall") +
  theme_classic()

# Grafisk presentasjon av m�ledata
survey %>% 
  ggplot + 
  geom_histogram(aes(x = age))

# En bearbeidet variant
survey %>% 
  ggplot + 
  geom_histogram(aes(x = age), bins = 12) +
  xlab("Alder") +
  ylab("Antall") +
  theme_classic()

# Grafisk presentasjon av sammenhengen mellom to m�levariabler
fishermen %>% 
  ggplot + 
  geom_point(aes(x = height, y = weight))

# En bearbeidet variant
fishermen %>% 
  ggplot + 
  geom_point(aes(x = height, y = weight)) +
  xlab("H�yde") +
  ylab("Vekt") +
  ggtitle("Sammenheng mellom h�yde og vekt") +
  theme_classic()

# Korrelasjonskoeffisienten
cor(fishermen$height, fishermen$weight)



