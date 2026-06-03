# ==============================================================================
# PROYECTO: Cartografía de Precio Promedio del Jitomate (Mapa Hexagonal)
# AÑO: 2026 | VARIABLE: Precio promedio del jitomate (13 kg)
# ==============================================================================

# 1. PAQUETERÍAS ---------------------------------------------------------------
options(timeout = 600)
install.packages(c("terra", "sf", "sp", "raster", "maps"))
install.packages("devtools")
devtools::install_github('diegovalle/mxmaps')
library(mxmaps)
library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
1:
  
# 2. DIRECTORIO E IMPORTACIÓN --------------------------------------------------
setwd("C:/Users/anava/Downloads/SCHP")
getwd()
data_hex <- readxl::read_excel("base_map_hex_jitomate.xlsx", sheet = "data")

# 3. PROCESAMIENTO -------------------------------------------------------------
# Las columnas de precio son las 5 últimas (enero a mayo 2026).
# Se calcula el promedio ignorando los estados sin dato ("-" → NA).
cols_precio <- names(data_hex)[7:ncol(data_hex)]  # Identificar las columnas de precio (todas menos las 6 primeras de metadatos)
data_hex[cols_precio] <- lapply(data_hex[cols_precio], function(x) suppressWarnings(as.numeric(x)))  # Convertir a numérico (los "-" se convierten en NA automáticamente)
data_hex$value <- rowMeans(data_hex[cols_precio], na.rm = TRUE) # Calcular el precio promedio por estado (ene-may 2026)
data_hex$value[is.nan(data_hex$value)] <- NA  # Los estados sin ningún dato quedan como NaN → convertir a NA explícito
# Cargar el marco espacial base integrado en la paquetería mxmaps
data("df_mxstate_2020")
df_mxstate_2020$value <- data_hex$value

# 4. CONSTRUCCIÓN DEL MAPA ---------------------------------------------------------------
mapa_jit <- MXHexBinChoropleth$new(df_mxstate_2020)
mapa_jit$label_size <- 1.8
mapa_jit$set_num_colors(1)

mapa_jit$ggplot_scale <- scale_fill_stepsn(
  colors      = c("#FFF5F0","#FEE5D9", "#FDDBC7", "#F4A582", "#D6604D", "#B2182B", "#67001F"),
  breaks      = c(100, 300, 500, 700, 900, 1100),
  limits      = c(0, 1250),
  na.value    = "grey80",
  name        = "Precio (MXN)",
  show.limits = TRUE,
  labels      = scales::label_comma()
)

grafico_final <- mapa_jit$render()

# . TEMA, TÍTULO Y LEYENDA ----------------------------------------------------

grafico_final <- grafico_final +
  labs(
    title    = "Precio promedio del jitomate (Caja de 13 kg)",
    subtitle = "Por estado, enero–mayo 2026  •  Pesos mexicanos (MXN)",
    caption  = "Fuente: Elaboración propia con datos obtenidos del SNIIM.\nEstados en gris: sin información disponible."
  ) +
  theme(
    # Títulos
    plot.title    = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, hjust = 0.5, color = "grey30", margin = margin(b = 8)),
    plot.caption  = element_text(size = 7, hjust = 0, color = "grey45", margin = margin(t = 6)),
    
    # Fondo
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    panel.border     = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    
    # Margen derecho amplio para "empujar" el mapa hacia el centro
    # y crear espacio para la leyenda en el Golfo de México
    plot.margin = margin(t = 5, r = 120, b = 5, l = 20),
    
    # Leyenda dentro del panel, zona derecha (Golfo de México)
    legend.position      = c(0.95, 0.55),
    legend.justification = c(1, 0.5),
    legend.direction     = "vertical",
    legend.background    = element_rect(fill = alpha("white", 0.90), color = "#B2182B", linewidth = 0.4),
    legend.title         = element_text(size = 7, face = "bold"),
    legend.text          = element_text(size = 6.5),
    legend.key.width     = unit(0.5, "cm"),
    legend.key.height    = unit(0.40, "cm"),
    legend.margin        = margin(6, 8, 6, 8)
  )

# 7. ANOTACIÓN AÑO -------------------------------------------------------------
grafico_final <- grafico_final +
  annotate(
    "text", x = Inf, y = -Inf,
    label    = "2026",
    size     = 3.5, fontface = "bold", color = "#67001F",
    hjust    = 1.5, vjust    = -1.5
  )

# 8. EXPORTACIÓN ---------------------------------------------------------------
print(grafico_final)

ggsave(
  filename = "mapa_hexagonal_jitomate.png",
  plot     = grafico_final,
  width    = 9,
  height   = 6.5,
  dpi      = 300,
  bg       = "white"
)

message("Mapa exportado correctamente.")
