
# Câncer -----------------------------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 24/09/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: https://ourworldindata.org/cancer --------------------------------------------------------------------------------------------

# Sobre os dados ---------------------------------------------------------------------------------------------------------------------------

### Em 2017, estimou-se que 9,6 milhões de pessoas morreram de variadas
### formas de câncer. Uma a cada 6 mortes no mundo é devido ao câncer,
### fazendo dessa doença a segunda principal causa de morte, a primeira
### causa de morte é devido à doenças cardiovasculares.

### O progresso contra muitas outras causas de morte e factores demográficos 
### de aumento da população, da expectativa de vida e - particularmente 
### em países de rendimento mais elevado - do envelhecimento da população 
### significam que o número total de mortes por cancer continua a aumentar.
### Este é um tema muito pessoal para muitos: quase todos conhecem ou perderam 
### alguém que lhes é querido por esta doença.

### Câncer é definido pelo Instituto Nacional de Câncer como uma coleção de doenças
### em que ocorre a divisão e porpagação de células anormais nos tecidos. Câncer
### de vários tipos pode se espalhar em muitas partes do corpo através do sangue
### ou sistema linfático. 

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(cols4all)
library(hrbrthemes)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

ca <- read.csv("share-of-population-with-cancer.csv")
view(ca)
names(ca)

# Manipular dados --------------------------------------------------------------------------------------------------------------------------

ca <- ca %>%
  select(-Code) %>%
  rename(por_cancer = Prevalence...Neoplasms...Sex..Both...Age..Age.standardized..Percent.) %>%
  view()

ca1 <- ca %>%
  filter(Entity %in% c("China", "North Korea", "Cuba",
                       "United States", "Japan", "Germany")) %>%
  group_by(Entity) %>%
  summarise(media = mean(por_cancer),
            n = n(), sd = sd(por_cancer),
            se = sd/sqrt(n)) %>%
  view()

ca2 <- ca %>%
  filter(Entity %in% c("China", "North Korea", "Cuba",
                       "United States", "Japan", "Germany"),
         (between(Year, 1990, 2017))) %>%
  view()
  







