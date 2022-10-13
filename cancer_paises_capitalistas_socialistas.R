
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
library(ggthemes)

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

ca3 <- ca %>%
  filter(Entity %in% c("China", "United States")) %>%
  view()

# Gráficos ---------------------------------------------------------------------------------------------------------------------------------

c4a("safe", 6)

ggplot(ca1, aes(x = fct_reorder(Entity, media), y = media, 
                fill = Entity)) +
  geom_col(width = 0.9) +
  geom_errorbar(aes(ymin = media - se, ymax = media + se),
                size = 0.8, width = 0.2) +
  scale_fill_manual(values = c("#88CCEE", "#CC6677", "#DDCC77",
                               "#117733", "#332288", "#AA4499")) +
  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  scale_x_discrete(labels = c("China", "Coreia do Norte", "Cuba",
                              "Alemanha", "Japão", "Estados Unidos")) +
  labs(x = "Países", y = "Porcentagem da população com câncer") +
  theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(legend.position = "none",
    axis.text = element_text(color = "black"))

ggplot(ca2, aes(x = Year, y = por_cancer,
                group = Entity, color = Entity)) +
  geom_point(shape = 15, size = 2.5) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("#88CCEE", "#CC6677", "#DDCC77",
                               "#117733", "#332288", "#AA4499"),
                     labels = c("China", "Cuba", "Alemanha",
                                "Japão", "Coreia do Norte", "Estados Unidos")) +
  labs(x = "Tempo (anos)", 
       y = "Porcentagem da população com câncer",
       col = "Países") +
    theme_ipsum(axis_title_size = 16,
              axis_text_size = 14) +
  theme(axis.text = element_text(color = "black"))

c4a("dark2", 2)

ggplot(ca3, aes(x = Year, y = por_cancer, 
                  group = Entity, col = Entity)) +
  geom_line(size = 2.2) +
  scale_color_manual(values = c("#1B9E77", "#D95F02"),
                     labels = c("China", "Estados Unidos")) +
  labs(x = "Tempo (anos)", y = "Porcentagem da população com câncer", 
       color = "Países") +
  theme_hc() +
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(color = "black", size = 15),
        legend.text = element_text(size = 12))



