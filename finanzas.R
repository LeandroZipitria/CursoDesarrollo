library(WDI)
library(dplyr)
library(ggplot2)
library(ggrepel)

# Descarga de datos
datos <- WDI(
  country = "all",
  indicator = c(credit_gdp = "FS.AST.PRVT.GD.ZS",
                gdp_ppp_pc = "NY.GDP.PCAP.PP.KD"),
  start = 1980, end = 2020
)

# Filtrar y reorganizar
df <- datos %>%
  filter(year %in% c(1980, 2020)) %>%
  select(iso3c, country, year, credit_gdp, gdp_ppp_pc) %>%
  tidyr::pivot_wider(
    names_from = year,
    values_from = c(credit_gdp, gdp_ppp_pc),
    names_sep = "_"
  ) %>%
  rename(
    credit1980 = credit_gdp_1980,
    gdp2020 = gdp_ppp_pc_2020
  ) %>%
  filter(!is.na(credit1980), !is.na(gdp2020), credit1980 > 0, gdp2020 > 0) %>%
  mutate(
    log_credit1980 = log(credit1980),
    log_gdp2020 = log(gdp2020)
  )

df_filtrado <- df %>%
  filter(log_credit1980 > 0)

# Modelo de regresión
modelo <- lm(log_credit1980 ~ log_gdp2020, data = df_filtrado)
r2 <- summary(modelo)$r.squared

# Scatterplot con R²
ggplot(df_filtrado, aes(x = log_gdp2020, y = log_credit1980, label = iso3c)) +
  geom_point(alpha = 0.7, color = "grey40") +
  geom_point(
    data = subset(df_filtrado, iso3c == "URY"),
    aes(x = log_gdp2020, y = log_credit1980),
    color = "red", size = 3
  ) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  ggrepel::geom_text_repel(size = 4, max.overlaps = 100) +
  labs(
    x = "Log PIB per cápita PPP (2020, USD const. 2017)",
    y = "Log Crédito al sector privado (% PIB, 1980)",
    title = "Crédito/PIB (1980) vs PIB per cápita (2020)",
    caption = "Fuente: World Bank WDI"
  ) +
  annotate("text",
           x = min(df_filtrado$log_gdp2020, na.rm = TRUE),  # esquina izquierda
           y = max(df_filtrado$log_credit1980, na.rm = TRUE), # parte superior
           label = paste0("R² = ", round(r2, 3)),
           hjust = 0, vjust = 1, size = 5, color = "black") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # título centrado y más grande
    axis.title = element_text(size = 14, face = "bold"),               # títulos de ejes
    axis.text = element_text(size = 12),                               # números de los ejes
    plot.caption = element_text(size = 10)                             # fuente más grande
  )

# Guardar el gráfico en A4 apaisado JPG dentro de la carpeta "figuras"
ggsave(
  filename = "../../figuras/credito_pib.jpeg",
  width = 15, height = 10.5, units = "cm",
  dpi = 300, device = "jpg"
)



# Serie filtrada por PIB en 1980 ------------------------------------------


library(WDI)
library(dplyr)
library(ggplot2)
library(ggrepel)

## 1) Traer un solo año por dataset y luego unir
datos_1980 <- WDI(
  country = "all",
  indicator = c(
    credit1980 = "FS.AST.PRVT.GD.ZS",  # Crédito/PIB
    gdp1980    = "NY.GDP.PCAP.KD"      # PIB pc real (NO PPP) para 1980 -> mejor cobertura
  ),
  start = 1980, end = 1980
) %>%
  select(iso3c, country, credit1980, gdp1980)

datos_2020 <- WDI(
  country = "all",
  indicator = c(
    gdp2020 = "NY.GDP.PCAP.PP.KD"      # PIB pc PPP (2017 USD const) para 2020
  ),
  start = 2020, end = 2020
) %>%
  select(iso3c, gdp2020)

## 2) Unir y limpiar (positivos para poder logaritmar)
df <- datos_1980 %>%
  inner_join(datos_2020, by = "iso3c") %>%
  filter(!is.na(credit1980), !is.na(gdp1980), !is.na(gdp2020),
         credit1980 > 0, gdp1980 > 0, gdp2020 > 0) %>%
  mutate(
    log_credit1980 = log(credit1980),
    log_gdp1980    = log(gdp1980),
    log_gdp2020    = log(gdp2020)
  )

cat("Filas disponibles:", nrow(df), "\n")  # chequeo rápido

## 3) Regresión: log(Crédito/PIB 1980) ~ log(PIB pc 1980)
modelo_1980 <- lm(log_credit1980 ~ log_gdp1980, data = df)

## 4) Guardar residuos
df <- df %>% mutate(resid_1980 = resid(modelo_1980))

## 5) Graficar residuos vs log(PIB pc PPP 2020)
ggplot(df, aes(x = log_gdp2020, y = resid_1980, label = iso3c)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.7, color = "grey40") +
  # Uruguay destacado en rojo
  geom_point(
    data = subset(df, iso3c == "URY"),
    aes(x = log_gdp2020, y = resid_1980),
    color = "red", size = 3
  ) +
  ggrepel::geom_text_repel(size = 4, max.overlaps = 100) +
  labs(
    x = "Log PIB per cápita PPP (2020, USD const. 2017)",
    y = "Residuos de log(Crédito/PIB, 1980)\ncontrolando por log(PIB pc, 1980)",
    title = "Residuos (1980) vs Log PIB pc PPP (2020)",
    caption = "Fuente: World Bank WDI; modelo: log(Crédito/PIB 1980) ~ log(PIB pc 1980, NO PPP)"
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title  = element_text(size = 14, face = "bold"),
    axis.text   = element_text(size = 12),
    plot.caption= element_text(size = 10)
  )
