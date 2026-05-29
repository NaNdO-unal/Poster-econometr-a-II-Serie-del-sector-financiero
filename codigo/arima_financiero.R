# ============================================================
# Valor Agregado del Sector Financiero y de Seguros - Colombia
# Metodología Box-Jenkins | Serie empalmada base 2015 equivalente
# 103 observaciones trimestrales: 2000Q1 - 2025Q4
# ============================================================
# NOTA METODOLÓGICA SOBRE EL EMPALME:
#   - Obs 1-20 (2000Q1-2004Q4): retroproyectadas desde base 2005 del DANE
#     aplicando factor de conversión 0.2311 (CV del ratio = 2.62% en traslape
#     de 16 trimestres, 2005Q1-2008Q4). La base 2005 agrupa establecimientos
#     financieros, seguros, inmobiliario y servicios a empresas (SCN 1993),
#     cobertura más amplia que el agregado K de base 2015 (SCN 2008/CIIU Rev. 4).
#   - Obs 21-103 (2005Q1-2025Q4): serie observada base 2015 del DANE.
#   Implicaciones estadísticas: (i) quiebre metodológico implícito en 2005Q1,
#   (ii) menor volatilidad artificial en el período retroproyectado
#   (std tasas 2000-2004 = 0.90% vs 2005-2024 = 3.11%), (iii) diferencia de
#   cobertura sectorial absorbida en el factor de conversión.
# ============================================================

# === 1. LIBRERÍAS =============================================

# Rutas relativas
library(fs)
library(here)

#Lectura de Excel
library(readxl)

# Tidyverse: manejo y graficación de datos
library(readr)
library(dplyr)
library(ggplot2)
library(ggtime)

# Tidyverts: series de tiempo modernas
library(tsibble)
library(feasts)
library(fable)

# Pruebas estadísticas
library(tseries)   # jarque.bera.test
library(FinTS)     # ArchTest
library(lmtest)    # coeftest
library(urca)      # ur.df, ur.kpss

# Para que la función ARIMA por defecto sea fable::ARIMA
ARIMA <- fable::ARIMA

# === 2. RUTAS RELATIVAS =======================================

# Fija la raíz del proyecto a partir de la ubicación de ESTE script.
# here::i_am() busca hacia arriba en el árbol de directorios hasta
# encontrar la ruta que coincida. Esto hace que el código sea
# reproducible en cualquier computador sin cambiar rutas manualmente.
here::i_am("codigo/arima_financiero.R")

# Ruta al directorio de datos (relativa a la raíz del proyecto)
DATA_DIR <- fs::path(here::here("datos"))

# Ruta al archivo de datos
ruta_datos <- fs::path(DATA_DIR, "serie_empalmada_100obs.xlsx")

# === 3. FUNCIONES AUXILIARES ==================================

# Muestra múltiples gráficos ggplot en una grilla de nrow x ncol
grilla <- function(..., nrow, ncol) {
  graficos <- list(...)
  if (length(graficos) > nrow * ncol)
    stop("La cantidad de graficos supera el tamaño de la grilla.")
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(
    layout = grid::grid.layout(nrow = nrow, ncol = ncol)
  ))
  for (i in seq_along(graficos)) {
    fila    <- ceiling(i / ncol)
    columna <- ((i - 1) %% ncol) + 1
    print(graficos[[i]],
          vp = grid::viewport(layout.pos.row = fila, layout.pos.col = columna))
  }
  grid::popViewport()
}

# Extrae un parámetro de la tabla de coeficientes de fable
obtener_parametro <- function(tabla_coef, modelo, termino, columna) {
  valor <- tabla_coef |>
    filter(.model == modelo, term == termino) |>
    pull({{ columna }})
  if (length(valor) == 0) return(NA_real_)
  valor[[1]]
}

formato_estimacion    <- function(v, d = 3) if (is.na(v)) "" else sprintf(paste0("%.", d, "f"), v)
formato_error_estandar <- function(v, d = 3) if (is.na(v)) "" else paste0("(", sprintf(paste0("%.", d, "f"), v), ")")

# Imprime tabla de pronóstico con formato
imprimir_tabla_pronostico <- function(titulo, tabla, d = 3) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
  cat(titulo, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")
  print(as.data.frame(
    tabla |> mutate(across(where(is.numeric), ~ sprintf(paste0("%.", d, "f"), .x)))
  ), row.names = FALSE, quote = FALSE)
}

# Extrae residuos de un modelo fable descartando los iniciales
residuos_modelo <- function(fit, nombre_modelo, p, q) {
  n_inicial <- max(p, q, 1)
  fit |>
    select(all_of(nombre_modelo)) |>
    augment() |>
    as_tibble() |>
    slice((n_inicial + 1):n()) |>
    select(fecha, .resid)
}

# === 4. CARGA Y PREPARACIÓN DE DATOS ==========================

datos_raw <- read_excel(ruta_datos)

cat("=== Estructura del archivo cargado ===\n")
print(head(datos_raw))
print(tail(datos_raw))
cat(sprintf("Total observaciones: %d\n", nrow(datos_raw)))
cat(sprintf("Obs retroproyectadas (base 2005): %d\n",
            sum(datos_raw$fuente == "base2005_retroproyectada")))
cat(sprintf("Obs observadas (base 2015): %d\n",
            sum(datos_raw$fuente == "base2015")))

# Construir índice temporal trimestral a partir de la columna "periodo"
# Formato: "2000_I", "2000_II", "2000_III", "2000_IV"
datos_raw <- datos_raw |>
  mutate(
    anio = as.integer(sub("_.*", "", periodo)),
    trim = sub(".*_", "", periodo),
    trim_num = case_when(
      trim == "I"   ~ 1,
      trim == "II"  ~ 2,
      trim == "III" ~ 3,
      trim == "IV"  ~ 4
    ),
    fecha = yearquarter(paste0(anio, " Q", trim_num))
  )

# Crear tsibble (objeto estándar del ecosistema tidyverts/fable)
va_fin_tbl <- datos_raw |>
  select(fecha, va_financiero_b2015_equiv, fuente) |>
  as_tsibble(index = fecha)

cat("\n=== Primeras y últimas observaciones de la serie ===\n")
print(head(va_fin_tbl))
print(tail(va_fin_tbl))

# Estadísticas descriptivas
cat("\n=== Estadísticas descriptivas ===\n")
print(summary(va_fin_tbl$va_financiero_b2015_equiv))

# ============================================================
# PASO 1: IDENTIFICACIÓN
# ============================================================

cat("\n\n===== PASO 1: IDENTIFICACIÓN =====\n")

# --- 1.1 Gráfica de la serie en niveles ---

print(
  ggtime::autoplot(va_fin_tbl, va_financiero_b2015_equiv, linewidth = 0.7) +
    geom_vline(xintercept = as.numeric(yearquarter("2005 Q1")),
               color = "red", linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = yearquarter("2005 Q3"),
             y = min(va_fin_tbl$va_financiero_b2015_equiv) * 1.1,
             label = "Punto de\nempalme", color = "red", size = 3) +
    ggtitle("VA Sector Financiero y Seguros - Colombia\n(miles de millones COP base 2015 equiv.)") +
    xlab("Trimestre") + ylab("Miles de millones COP") +
    theme_light()
)
# --- 1.1.1 Gráfica corregida ---

# 1. Filtramos la tabla para que empiece desde 2000 Q1
va_fin_filtrada <- va_fin_tbl %>% 
  filter(fecha >= yearquarter("2000 Q1")) # Asegúrate de cambiar 'Trimestre' por el nombre real de tu columna de tiempo

# 2. Graficamos la serie recortada
print(
  ggtime::autoplot(va_fin_filtrada, va_financiero_b2015_equiv, linewidth = 0.7) +
    
    # Al quitar as.numeric(), ggplot detecta correctamente el trimestre en el eje X
    geom_vline(xintercept = yearquarter("2005 Q1"), 
               color = "red", linetype = "dashed", linewidth = 0.8) +
    
    # Ajustamos el texto para que se alinee con la nueva posición de la línea
    annotate("text", x = yearquarter("2005 Q3"), 
             y = min(va_fin_filtrada$va_financiero_b2015_equiv) * 1.1, 
             label = "Punto de\nempalme", color = "red", size = 3, hjust = 0) +
    
    ggtitle("VA Sector Financiero y Seguros - Colombia\n(miles de millones COP base 2015 equiv.)") +
    xlab("Trimestre") + 
    ylab("Miles de millones COP") + 
    theme_light()
)
# --- 1.2 Transformación logarítmica ---
# Justificación: la serie muestra crecimiento tendencial con varianza
# que aumenta en el tiempo. El log estabiliza la varianza y facilita
# la interpretación de los cambios como tasas de crecimiento.

va_fin_tbl <- va_fin_tbl |>
  mutate(log_va = log(va_financiero_b2015_equiv))

print(
  ggtime::autoplot(va_fin_tbl, log_va, linewidth = 0.7) +
    geom_vline(xintercept = as.numeric(yearquarter("2005 Q1")),
               color = "red", linetype = "dashed", linewidth = 0.8) +
    ggtitle("Log VA Sector Financiero y Seguros") +
    xlab("Trimestre") + ylab("Log(miles de millones COP)") +
    theme_light()
)
# --- 1.2.1 Transformación logaritmica con grafica corregida ---

# 1. Creamos el logaritmo y filtramos para que comience en 2000 Q1
va_fin_log_filtrada <- va_fin_tbl %>% 
  mutate(log_va = log(va_financiero_b2015_equiv)) %>% 
  filter(fecha >= yearquarter("2000 Q1")) # <- Cambia 'Trimestre' si se llama distinto

# 2. Graficamos la serie logarítmica corregida
print(
  ggtime::autoplot(va_fin_log_filtrada, log_va, linewidth = 0.7) +
    
    # Eliminamos as.numeric() para que la línea se posicione correctamente en 2005
    geom_vline(xintercept = yearquarter("2005 Q1"), 
               color = "red", linetype = "dashed", linewidth = 0.8) +
    
    ggtitle("Log VA Sector Financiero y Seguros") +
    xlab("Trimestre") + 
    ylab("Log(miles de millones COP)") + 
    theme_light()
)
# --- 1.3 FAC y FACP de la serie en niveles (log) ---

g_fac_niveles <- va_fin_tbl |>
  ACF(log_va, lag_max = 20) |>
  ggtime::autoplot() +
  ggtitle("FAC - Log VA Financiero (niveles)") +
  ylim(-1, 1) + theme_light()

g_facp_niveles <- va_fin_tbl |>
  PACF(log_va, lag_max = 20) |>
  ggtime::autoplot() +
  ggtitle("FACP - Log VA Financiero (niveles)") +
  ylim(-1, 1) + theme_light()

grilla(g_fac_niveles, g_facp_niveles, nrow = 1, ncol = 2)

# --- 1.4 Pruebas de raíz unitaria sobre log(serie) ---

cat("\n=== Prueba ADF - Niveles log ===\n")
# type = "trend": incluye constante y tendencia (apropiado para serie con tendencia)
adf_niv <- ur.df(va_fin_tbl$log_va, type = "trend", selectlags = "AIC")
print(summary(adf_niv))

adf_stat  <- adf_niv@teststat[1, "tau3"]
adf_crit5 <- adf_niv@cval["tau3", "5pct"]
if (adf_stat < adf_crit5) {
  cat("ADF (niveles): Rechazamos H0 -> Serie estacionaria.\n")
} else {
  cat("ADF (niveles): No rechazamos H0 -> Serie NO estacionaria. Diferenciamos.\n")
}

cat("\n=== Prueba KPSS - Niveles log ===\n")
# type = "tau": incluye tendencia determinística
kpss_niv <- ur.kpss(va_fin_tbl$log_va, type = "tau", lags = "short")
print(summary(kpss_niv))

kpss_stat  <- kpss_niv@teststat[1]
kpss_crit5 <- kpss_niv@cval["critical values", "5pct"]
if (kpss_stat > kpss_crit5) {
  cat("KPSS (niveles): Rechazamos H0 -> Serie NO estacionaria. Diferenciamos.\n")
} else {
  cat("KPSS (niveles): No rechazamos H0 -> Serie estacionaria.\n")
}

# --- 1.5 Primera diferencia del log ---

va_fin_tbl <- va_fin_tbl |>
  mutate(diff_log_va = difference(log_va))

print(
  va_fin_tbl |>
    filter(!is.na(diff_log_va)) |>
    ggtime::autoplot(diff_log_va, linewidth = 0.7) +
    geom_vline(xintercept = as.numeric(yearquarter("2005 Q1")),
               color = "red", linetype = "dashed", linewidth = 0.8) +
    ggtitle("Primera diferencia de Log VA Financiero\n(aprox. tasa de crecimiento trimestral)") +
    xlab("Trimestre") + ylab("Δlog(VA)") +
    theme_light()
)

### --- 1.5.1 Correción grafica primera diferencia del log ---

# 1. Aseguramos la estructura, creamos la diferencia y filtramos desde 2000 Q1
va_fin_diff_filtrada <- va_fin_tbl %>% 
  fill_gaps() %>% # Evita el error de gaps implícitos
  mutate(diff_log_va = difference(log_va)) %>% 
  filter(fecha >= yearquarter("2000 Q1"), !is.na(diff_log_va)) # Cambia 'Trimestre' si se llama distinto

# 2. Graficamos la serie estacionaria (tasa de crecimiento)
print(
  ggtime::autoplot(va_fin_diff_filtrada, diff_log_va, linewidth = 0.7) +
    
    # Quitamos as.numeric() para ubicar la línea en el año 2005 exacto
    geom_vline(xintercept = yearquarter("2005 Q1"), 
               color = "red", linetype = "dashed", linewidth = 0.8) +
    
    ggtitle("Primera diferencia de Log VA Financiero\n(aprox. tasa de crecimiento trimestral)") +
    xlab("Trimestre") + 
    ylab("Δlog(VA)") + 
    theme_light()
)

cat("\n=== Prueba ADF - Primera diferencia log ===\n")
adf_diff <- ur.df(
  va_fin_tbl |> filter(!is.na(diff_log_va)) |> pull(diff_log_va),
  type = "drift", selectlags = "AIC"
)
print(summary(adf_diff))

adf_diff_stat  <- adf_diff@teststat[1, "tau2"]
adf_diff_crit5 <- adf_diff@cval["tau2", "5pct"]
if (adf_diff_stat < adf_diff_crit5) {
  cat("ADF (1ª diferencia): Rechazamos H0 -> Serie diferenciada es estacionaria.\n")
} else {
  cat("ADF (1ª diferencia): No rechazamos H0 -> Considerar segunda diferencia.\n")
}

cat("\n=== Prueba KPSS - Primera diferencia log ===\n")
kpss_diff <- ur.kpss(
  va_fin_tbl |> filter(!is.na(diff_log_va)) |> pull(diff_log_va),
  type = "mu", lags = "short"
)
print(summary(kpss_diff))

kpss_diff_stat  <- kpss_diff@teststat[1]
kpss_diff_crit5 <- kpss_diff@cval["critical values", "5pct"]
if (kpss_diff_stat > kpss_diff_crit5) {
  cat("KPSS (1ª diferencia): Rechazamos H0 -> Serie diferenciada NO estacionaria.\n")
} else {
  cat("KPSS (1ª diferencia): No rechazamos H0 -> Serie diferenciada es estacionaria.\n")
}

# --- 1.6 FAC y FACP de la primera diferencia (para lectura de p y q) ---

serie_diff <- va_fin_tbl |> filter(!is.na(diff_log_va))

g_fac_diff <- serie_diff |>
  ACF(diff_log_va, lag_max = 20) |>
  ggtime::autoplot() +
  ggtitle("FAC - Primera diferencia log VA") +
  ylim(-1, 1) + xlab("Rezago") + ylab("FAC") + theme_light()

g_facp_diff <- serie_diff |>
  PACF(diff_log_va, lag_max = 20) |>
  ggtime::autoplot() +
  ggtitle("FACP - Primera diferencia log VA") +
  ylim(-1, 1) + xlab("Rezago") + ylab("FACP") + theme_light()

grilla(g_fac_diff, g_facp_diff, nrow = 1, ncol = 2)

# LECTURA DE FAC/FACP (completar después de ver los gráficos):
# - Si FAC corta abruptamente en rezago q y FACP decae: sugiere MA(q) -> ARIMA(0,1,q)
# - Si FACP corta abruptamente en rezago p y FAC decae: sugiere AR(p) -> ARIMA(p,1,0)
# - Si ambas decaen: sugiere ARMA(p,q) -> ARIMA(p,1,q)
# Los órdenes propuestos a continuación son TENTATIVAS y deben ajustarse
# con base en la lectura real de las gráficas.

# ============================================================
# PASO 2: ESTIMACIÓN
# ============================================================

cat("\n\n===== PASO 2: ESTIMACIÓN =====\n")

# Se estiman 3 modelos candidatos.
# IMPORTANTE: los órdenes (p,d,q) DEBEN sustentarse en la lectura
# de la FAC y FACP del Paso 1. Ajustar según los gráficos reales.
# PDQ(0,0,0) es OBLIGATORIO para evitar que fable estime SARIMA automáticamente.
# Los datos están desestacionalizados, por tanto D=0 es correcto.

nombres_modelos <- c("ARIMA(1,1,0)", "ARIMA(0,1,1)", "ARIMA(1,1,1)")

fit_va <- va_fin_tbl |>
  model(
    "ARIMA(1,1,0)" = fable::ARIMA(log(va_financiero_b2015_equiv) ~ 0 + pdq(1, 1, 0) + PDQ(0, 0, 0)),
    "ARIMA(0,1,1)" = fable::ARIMA(log(va_financiero_b2015_equiv) ~ 0 + pdq(0, 1, 1) + PDQ(0, 0, 0)),
    "ARIMA(1,1,1)" = fable::ARIMA(log(va_financiero_b2015_equiv) ~ 0 + pdq(1, 1, 1) + PDQ(0, 0, 0))
  )

# Resumen individual de cada modelo
cat("\n--- Resultados de estimación por modelo ---\n")
for (nombre in nombres_modelos) {
  cat("\n", nombre, "\n", sep = "")
  print(report(fit_va |> select(all_of(nombre))))
}

# Tabla comparativa AIC / BIC
cat("\n--- Tabla comparativa de criterios de información ---\n")
tabla_ic <- glance(fit_va) |>
  select(.model, AIC, BIC) |>
  arrange(AIC)
print(tabla_ic)
cat("Nota: Se selecciona el modelo con menor AIC (y BIC como criterio secundario).\n")

# ============================================================
# PASO 3: VALIDACIÓN
# ============================================================

cat("\n\n===== PASO 3: VALIDACIÓN =====\n")

# Tabla de p-valores de todas las pruebas por modelo
tabla_validacion <- list()

for (nombre in nombres_modelos) {
  # Extraer órdenes del nombre del modelo (para descartar residuos iniciales)
  ordenes <- as.integer(regmatches(nombre, gregexpr("[0-9]", nombre))[[1]])
  p_ord <- ordenes[1]; d_ord <- ordenes[2]; q_ord <- ordenes[3]

  residuos_tbl <- residuos_modelo(fit_va, nombre, p_ord, q_ord)
  res <- residuos_tbl$.resid
  res2_tbl <- residuos_tbl |> mutate(res2 = .resid^2)

  # ── Gráficas de diagnóstico
  g_res <- ggplot(residuos_tbl, aes(x = fecha, y = .resid)) +
    geom_line(linewidth = 0.4) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    ggtitle(paste("Residuos -", nombre)) +
    xlab("Trimestre") + ylab("Residuo") + theme_light()

  g_fac_res <- residuos_tbl |>
    as_tsibble(index = fecha) |>
    ACF(.resid, lag_max = 20) |>
    ggtime::autoplot() +
    ggtitle(paste("FAC residuos -", nombre)) +
    ylim(-1, 1) + theme_light()

  g_fac_res2 <- res2_tbl |>
    as_tsibble(index = fecha) |>
    ACF(res2, lag_max = 20) |>
    ggtime::autoplot() +
    ggtitle(paste("FAC res² -", nombre)) +
    ylim(-1, 1) + theme_light()

  grilla(g_res, g_fac_res, g_fac_res2, nrow = 1, ncol = 3)

  # ── Q-Q plot
  gg_qq <- ggplot(residuos_tbl, aes(sample = .resid)) +
    stat_qq(color = "black", size = 1) +
    stat_qq_line(color = "red", linewidth = 0.6) +
    ggtitle(paste("Q-Q plot -", nombre)) +
    xlab("Cuantiles teóricos") + ylab("Cuantiles muestrales") +
    theme_light()
  print(gg_qq)

  # ── Prueba Ljung-Box (H0: no autocorrelación en residuos)
  lb_5  <- Box.test(res, lag = 5,  type = "Ljung-Box", fitdf = p_ord + q_ord)$p.value
  lb_10 <- Box.test(res, lag = 10, type = "Ljung-Box", fitdf = p_ord + q_ord)$p.value
  lb_20 <- Box.test(res, lag = 20, type = "Ljung-Box", fitdf = p_ord + q_ord)$p.value

  # ── Prueba ARCH (H0: no efectos ARCH en residuos)
  arch_1 <- FinTS::ArchTest(res, lags = 1)$p.value
  arch_2 <- FinTS::ArchTest(res, lags = 2)$p.value
  arch_5 <- FinTS::ArchTest(res, lags = 5)$p.value

  # ── Prueba Jarque-Bera (H0: normalidad de residuos)
  jb <- jarque.bera.test(res)$p.value

  tabla_validacion[[nombre]] <- data.frame(
    Modelo   = nombre,
    `LB(5)`  = lb_5,
    `LB(10)` = lb_10,
    `LB(20)` = lb_20,
    `ARCH(1)` = arch_1,
    `ARCH(2)` = arch_2,
    `ARCH(5)` = arch_5,
    `JB`      = jb,
    check.names = FALSE
  )
}

tabla_val_final <- bind_rows(tabla_validacion)
cat("\n--- Tabla resumen de pruebas de validación (p-valores) ---\n")
print(tabla_val_final |> mutate(across(where(is.numeric), ~ round(.x, 4))))
cat("\nInterpretación:\n")
cat("  LB:   p > 0.05 -> No rechazamos H0 -> residuos sin autocorrelación (DESEABLE)\n")
cat("  ARCH: p > 0.05 -> No rechazamos H0 -> no efectos ARCH (DESEABLE)\n")
cat("  JB:   p > 0.05 -> No rechazamos H0 -> residuos normales (DESEABLE)\n")

# ============================================================
# PASO 4: PRONÓSTICO (10 trimestres adelante)
# ============================================================

cat("\n\n===== PASO 4: PRONÓSTICO =====\n")

pronosticos <- fit_va |>
  forecast(h = 10) |>
  hilo(level = 95) |>
  fabletools::unpack_hilo("95%")

# Función para extraer tabla de pronóstico por modelo
tabla_pron_modelo <- function(nombre) {
  pronosticos |>
    filter(.model == nombre) |>
    as_tibble() |>
    select(
      Trimestre = fecha,
      Pronostico = .mean,
      IC_inferior = `95%_lower`,
      IC_superior = `95%_upper`
    )
}

# Imprimir tablas
for (nombre in nombres_modelos) {
  imprimir_tabla_pronostico(
    paste("Pronóstico 10 trimestres -", nombre),
    tabla_pron_modelo(nombre)
  )
}

# Nota: fable revierte automáticamente log() al hacer forecast()
# Los valores pronosticados están en nivel original (miles de millones COP)

# Gráfica conjunta: histórico + pronóstico de los 3 modelos
colores_modelos <- c(
  "ARIMA(1,1,0)" = "#E63946",
  "ARIMA(0,1,1)" = "#2A9D8F",
  "ARIMA(1,1,1)" = "#E9C46A"
)

print(
  ggplot() +
    geom_line(
      data = va_fin_tbl,
      aes(x = fecha, y = va_financiero_b2015_equiv),
      color = "black", linewidth = 0.5
    ) +
    geom_line(
      data = pronosticos,
      aes(x = fecha, y = .mean, color = .model),
      linewidth = 1
    ) +
    geom_vline(xintercept = as.numeric(yearquarter("2005 Q1")),
               color = "red", linetype = "dashed", linewidth = 0.7) +
    scale_color_manual(values = colores_modelos) +
    ggtitle("Pronóstico VA Sector Financiero y Seguros - Colombia\n10 trimestres adelante") +
    xlab("Trimestre") + ylab("Miles de millones COP (base 2015 equiv.)") +
    labs(color = "Modelo") +
    theme_minimal()
)

cat("\nFin del script. Revise las gráficas y ajuste los órdenes en el Paso 2\n")
cat("según la lectura real de la FAC y FACP del Paso 1.\n")

### ---Subir la información a Git---
git config --global user.name "NaNdO-unal"
git config --global user.email "arrodriguezsa@unal.edu.co"
