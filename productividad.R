library(dplyr)
library(ggplot2)
library(pwt10)



# Producto por hora trabajada -----------------------------------------------------


data("pwt10.0")

pwt <- pwt10.0 %>%
  mutate(prod = rgdpo / (emp * avh))

# Último año disponible con prod no-NA para USA
latest_year <- pwt %>%
  filter(isocode == "USA", !is.na(prod)) %>%
  summarise(y = max(year)) %>%
  pull(y)

# Valor de productividad de USA por año
usa_by_year <- pwt %>%
  filter(isocode == "USA") %>%
  select(year, prod_us = prod)

# Normalización contra USA
pwt_rel <- pwt %>%
  left_join(usa_by_year, by = "year") %>%
  mutate(prod_rel = prod / prod_us)

## países de interés
paises <- c("URY","ARG","BRA","CHL","MEX","ESP","DEU","GBR","CHN","KOR","USA")

df_series <- pwt_rel %>%
  filter(isocode %in% paises) %>%
  tidyr::drop_na(prod_rel)

# último punto de cada serie
last_pts <- df_series %>%
  group_by(isocode) %>%
  filter(year == max(year[!is.na(prod_rel)])) %>%
  ungroup()

ggplot(df_series, aes(x = year, y = prod_rel, color = isocode)) +
  geom_line(linewidth = 0.9) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  # resaltar Uruguay en negro
  geom_line(
    data = subset(df_series, isocode == "URY"),
    aes(x = year, y = prod_rel),
    color = "black", linewidth = 1.4
  ) +
  # etiquetas de país en el extremo derecho
  geom_text_repel(
    data = last_pts,
    aes(label = isocode),
    nudge_x = 3, hjust = 0, direction = "y",
    segment.alpha = 0.5, size = 4,
    min.segment.length = 0
  ) +
  # dejamos más espacio a la derecha (para etiquetas)
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.15))) +
  labs(
    x = "Año",
    y = "Productividad relativa (EE.UU.=1)",
    title = "Evolución de la productividad relativa - PWT 10.0",
    color = "País"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",              # ocultamos leyenda (ya están las etiquetas)
    plot.title = element_text(hjust = 0.5) # título centrado
  )





# Evolucion TFP ---------------------------------------------------------------


# Tomamos ctfp (TFP en niveles comparables)
pwt <- pwt10.0 %>% select(isocode, country, year, ctfp)

# Normalizamos contra USA por año (relativo a EE.UU.)
usa_ctfp <- pwt %>% filter(isocode == "USA") %>% select(year, ctfp_us = ctfp)

pwt_rel <- pwt %>%
  left_join(usa_ctfp, by = "year") %>%
  mutate(tfp_rel = ctfp / ctfp_us)

# --- Serie de tiempo con colores y etiquetas al final ---
df_series <- pwt_rel %>% filter(isocode %in% paises) %>% tidyr::drop_na(tfp_rel)

last_pts <- df_series %>%
  group_by(isocode) %>%
  filter(year == max(year[!is.na(tfp_rel)])) %>%
  ungroup()

ggplot(df_series, aes(x = year, y = tfp_rel, color = isocode)) +
  # todas las series en colores finos
  geom_line(linewidth = 0.9, alpha = 0.8) +
  # resaltar Uruguay en negro
  geom_line(
    data = subset(df_series, isocode == "URY"),
    aes(x = year, y = tfp_rel),
    color = "black", linewidth = 1.4
  ) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  # etiquetas al final
  ggrepel::geom_text_repel(
    data = last_pts, aes(label = isocode),
    nudge_x = 3, hjust = 0, direction = "y",
    segment.alpha = 0.5, size = 4, min.segment.length = 0, show.legend = FALSE
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.01, 0.15))) +
  labs(
    x = "Año", y = "TFP relativa (EE.UU.=1)",
    title = "Evolución de la TFP relativa a EE.UU. (ctfp, PWT 10.0)",
    color = "País"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

