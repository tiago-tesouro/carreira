library(tidyverse)
library(readxl)
library(extrafont)
library(colorspace)
library(pammtools)

dados <- read_excel("carreira.xlsx", sheet = "Página6")

dados_2 <- dados %>% 
  rename("data" = 1) %>% 
  filter(lubridate::year(data) >= 2009) %>%
  select(data, 
         affc = `AFFC (base)`,
         rfb = `AFRFB (base)`,
         trfb = `ATRFB (base)`
         ) %>%
  #mutate(dif = rfb - affc) %>%
  mutate(across(-data, ~ ./rfb)) #%>%
  #mutate(rfb = rfb - affc) %>%
  #gather(-data, key = carreira, value = salario) %>%
  #mutate(carreira = factor(carreira, levels = c("rfb", "affc")))

dados_3 <- dados %>% 
  rename("data" = 1) %>% 
  filter(lubridate::year(data) >= 2009) %>%
  select(data, 
         affc = `AFFC (topo)`,
         rfb = `AFRFB (topo)`,
         #trfb = `ATRFB (base)`
  ) %>%
  #mutate(dif = rfb - affc) %>%
  mutate(across(-data, ~ ./rfb)) #%>%
#mutate(rfb = rfb - affc) %>%
#gather(-data, key = carreira, value = salario) %>%
#mutate(carreira = factor(carreira, levels = c("rfb", "affc")))


# gráfico base
ggplot(dados_2, aes(x = data)) + 
  geom_ribbon(aes(ymin = affc, ymax = rfb), fill = "#F6E2DF", color = "transparent") +
  geom_hline(yintercept = .95, linetype = "dashed") +
  #geom_step
  geom_line(aes(y = affc), color = "#BF2C62", size = 1.8) +
  geom_line(aes(y = rfb), color = "#1E5693", size = 1.8) +
  #geom_line(aes(y = trfb), color = "gold", size = 2) +
  #geom_segment(aes(y = affc, yend = rfb, x = data, xend = data, color = rfb-affc), size = 3) +

  geom_text(aes(
    y = affc, 
    label = if_else(data %in% data[c(7,11, 20, 24)], 
               paste0(
                 #lubridate::month(data), "/", 
                 #lubridate::year(data),":\n",
                 scales::percent(affc, accuracy = 0.1)
                 ), 
               NA)
      ), nudge_y = 0.01, nudge_x = 3e6, family = "Work sans", hjust = "left", color = "firebrick", fontface = "bold"
    ) +
  geom_text(aes(
    y = affc, 
    label = if_else(data %in% data[c(7,11, 20, 24)], 
                    paste0(
                      lubridate::month(data), "/", 
                      lubridate::year(data)
                      #scales::percent(affc, accuracy = 0.1)
                    ), 
                    NA)
  ), nudge_y = -0.01, nudge_x = 3e6, family = "Work sans", fontface = "italic", hjust = "left"
  ) +
  annotate(geom = "text", label = "Auditor Receita", color = "steelblue", x = dados_2$data[1] + 3.5e6, y = 1.01, hjust = "left", fontface = "bold", vjust = "bottom", family = "Work sans", ) +
  annotate(geom = "text", label = "Auditor Tesouro", color = "firebrick", x = dados_2$data[1] + 3.5e6, y = 0.96, hjust = "left", vjust = "bottom", fontface = "bold", family = "Work sans") +
  annotate(geom = "text", label = "95%", x = dados_2$data[1] - 3.5e6, y = 0.94, hjust = "right", vjust = "top", fontface = "bold", family = "Work sans", fill = "white", label.size = NA) +
  annotate(geom = "text", label = paste0(lubridate::month(dados_2$data[1]), "/", lubridate::year(dados_2$data[1])), x = dados_2$data[1] +  3e6, y = 0.94, hjust = "left", fontface = "italic", vjust = "top", family = "Work sans") +
  geom_point(aes(y = if_else(
    data %in% data[c(1,7,11, 20, 24)],
    affc, NA)), 
    size = 2) +
  scale_y_continuous(
    limits = c(0.6,1.01), breaks = c(0.2, 0.4, 0.6, 0.8, 1), labels = scales::percent) +
  scale_x_datetime(minor_breaks = dados_2$data[c(1,7,11, 20, 24)], expand = expansion(add = c(3e7, 4e7))) +
  labs(title = "Salários de entrada: RFB x STN",
       subtitle = "Em percentuais do salário de entrada da RFB", y = NULL, x = NULL) +
  scale_color_continuous_sequential(palette = "reds") +
  theme_minimal() +
  theme(
    #legend.position = "none",
    text = element_text(family = "Work sans"),
    plot.title = element_text(face = "bold")
    )

ggsave("entrada.png", width = 7, height = 4, bg = "white")

# gráfico topo
ggplot(dados_3, aes(x = data)) + 
  geom_ribbon(aes(ymin = affc, ymax = rfb), fill = "#F6E2DF", color = "transparent") +
  geom_hline(yintercept = .95, linetype = "dashed") +
  #geom_step
  geom_line(aes(y = affc), color = "#BF2C62", size = 1.8) +
  geom_line(aes(y = rfb), color = "#1E5693", size = 1.8) +
  #geom_line(aes(y = trfb), color = "gold", size = 2) +
  #geom_segment(aes(y = affc, yend = rfb, x = data, xend = data, color = rfb-affc), size = 3) +
  
  geom_text(aes(
    y = affc, 
    label = if_else(data %in% data[c(7,11, 20, 24)], 
                    paste0(
                      #lubridate::month(data), "/", 
                      #lubridate::year(data),":\n",
                      scales::percent(affc, accuracy = 0.1)
                    ), 
                    NA)
  ), nudge_y = 0.01, nudge_x = 3e6, family = "Work sans", hjust = "left", color = "firebrick", fontface = "bold"
  ) +
  geom_text(aes(
    y = affc, 
    label = if_else(data %in% data[c(7,11, 20, 24)], 
                    paste0(
                      lubridate::month(data), "/", 
                      lubridate::year(data)
                      #scales::percent(affc, accuracy = 0.1)
                    ), 
                    NA)
  ), nudge_y = -0.01, nudge_x = 3e6, family = "Work sans", fontface = "italic", hjust = "left"
  ) +
  annotate(geom = "text", label = "Auditor Receita", color = "steelblue", x = dados_2$data[1] + 3.5e6, y = 1.01, hjust = "left", fontface = "bold", vjust = "bottom", family = "Work sans", ) +
  annotate(geom = "text", label = "Auditor Tesouro", color = "firebrick", x = dados_2$data[1] + 3.5e6, y = 0.96, hjust = "left", vjust = "bottom", fontface = "bold", family = "Work sans") +
  annotate(geom = "text", label = "95%", x = dados_2$data[1] - 3.5e6, y = 0.94, hjust = "right", vjust = "top", fontface = "bold", family = "Work sans", fill = "white", label.size = NA) +
  annotate(geom = "text", label = paste0(lubridate::month(dados_2$data[1]), "/", lubridate::year(dados_2$data[1])), x = dados_2$data[1] +  3e6, y = 0.94, hjust = "left", fontface = "italic", vjust = "top", family = "Work sans") +
  geom_point(aes(y = if_else(
    data %in% data[c(1,7,11, 20, 24)],
    affc, NA)), 
    size = 2) +
  scale_y_continuous(
    limits = c(0.6,1.01), breaks = c(0.2, 0.4, 0.6, 0.8, 1), labels = scales::percent) +
  scale_x_datetime(minor_breaks = dados_2$data[c(1,7,11, 20, 24)], expand = expansion(add = c(3e7, 4e7))) +
  labs(title = "Salários finais: RFB x STN",
       subtitle = "Em percentuais do salário final da RFB", y = NULL, x = NULL) +
  scale_color_continuous_sequential(palette = "reds") +
  theme_minimal() +
  theme(
    #legend.position = "none",
    text = element_text(family = "Work sans"),
    plot.title = element_text(face = "bold")
  )

ggsave("saida.png", width = 6, height = 3, bg = "white")
