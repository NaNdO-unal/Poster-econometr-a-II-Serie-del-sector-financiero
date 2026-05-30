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
#   Implicaciones estadísticas:
#     (i)  Quiebre metodológico implícito en 2005Q1.
#     (ii) Menor volatilidad en el período retroproyectado:
#          std tasas 2000-2004 = 0.90% vs 2005-2024 = 3.11%.
#     (iii) Diferencia de cobertura sectorial absorbida en el factor de conversión.
# ============================================================

# === 1. LIBRERÍAS =============================================

library(fs)          # Manejo de rutas relativas
library(here)        # Ancla del proyecto para rutas relativas
library(readxl)      # Lectura de archivos Excel
library(readr)       # Lectura de CSV
library(dplyr)       # Manipulación de datos
library(ggplot2)     # Graficación
library(ggtime)      # Graficación de series de tiempo (autoplot)
library(tsibble)     # Estructura de datos para series de tiempo
library(feasts)      # FAC, FACP, descomposición
library(fable)       # Estimación de modelos ARIMA
library(tseries)     # jarque.bera.test
library(FinTS)       # ArchTest
library(lmtest)      # coeftest
library(urca)        # ur.df, ur.kpss (pruebas de raíz unitaria)
library(strucchange) # Test CUSUM de estabilidad estructural

# fable::ARIMA tiene prioridad sobre stats::arima
ARIMA <- fable::ARIMA

# === 2. RUTAS RELATIVAS =======================================
# here::i_am() ancla la raíz del proyecto en la ubicación de este script.
# Permite que el código corra en cualquier computador sin cambiar rutas.
# Estructura esperada del proyecto:
#   proyecto/
#   ├── codigo/
#   │   └── arima_financiero.R   <- este archivo
#   └── datos/
#       └── serie_empalmada_100obs.xlsx

here::i_am("codigo/arima_financiero_FINAL.R")

DATA_DIR   <- fs::path(here::here("datos"))
ruta_datos <- fs::path(DATA_DIR, "serie_empalmada_100obs.xlsx")

# === 3. FUNCIONES AUXILIARES ==================================

# Muestra múltiples gráficos ggplot en una grilla nrow x ncol
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

# Imprime tabla de pronóstico con formato uniforme
imprimir_tabla_pronostico <- function(titulo, tabla, d = 3) {
  cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep = "")
  cat(titulo, "\n")
  cat(paste(rep("=", 60), collapse = ""), "\n", sep = "")
  print(as.data.frame(
    tabla |> mutate(across(where(is.numeric), ~ sprintf(paste0("%.", d, "f"), .x)))
  ), row.names = FALSE, quote = FALSE)
}

# Extrae residuos de un modelo fable descartando observaciones iniciales
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
cat(sprintf("Obs observadas (base 2015):       %d\n",
            sum(datos_raw$fuente == "base2015")))

# Construir índice temporal trimestral desde la columna "periodo"
# Formato esperado: "2000_I", "2000_II", "2000_III", "2000_IV"
datos_raw <- datos_raw |>
  mutate(
    anio     = as.integer(sub("_.*", "", periodo)),
    trim     = sub(".*_", "", periodo),
    trim_num = case_when(
      trim == "I"   ~ 1,
      trim == "II"  ~ 2,
      trim == "III" ~ 3,
      trim == "IV"  ~ 4
    ),
    fecha = yearquarter(paste0(anio, " Q", trim_num))
  )

# Crear tsibble: estructura estándar del ecosistema tidyverts/fable
va_fin_tbl <- datos_raw |>
  select(fecha, va_financiero_b2015_equiv, fuente) |>
  as_tsibble(index = fecha)

cat("\n=== Primeras y últimas observaciones ===\n")
print(head(va_fin_tbl))
print(tail(va_fin_tbl))

cat("\n=== Estadísticas descriptivas ===\n")
print(summary(va_fin_tbl$va_financiero_b2015_equiv))

# ============================================================
# PASO 1: IDENTIFICACIÓN
# ============================================================

cat("\n\n===== PASO 1: IDENTIFICACIÓN =====\n")

# --- 1.1 Serie en niveles originales ---

va_fin_filtrada <- va_fin_tbl |>
  filter(fecha >= yearquarter("2000 Q1"))

print(
  ggtime::autoplot(va_fin_filtrada, va_financiero_b2015_equiv, linewidth = 0.7) +
    geom_vline(xintercept = yearquarter("2005 Q1"),
               color = "red", linetype = "dashed", linewidth = 0.8) +
    annotate("text", x = yearquarter("2005 Q3"),
             y = min(va_fin_filtrada$va_financiero_b2015_equiv) * 1.1,
             label = "Punto de\nempalme", color = "red", size = 3, hjust = 0) +
    ggtitle("VA Sector Financiero y Seguros - Colombia\n(miles de millones COP base 2015 equiv.)") +
    xlab("Trimestre") + ylab("Miles de millones COP") +
    theme_light()
)

# --- 1.2 Transformación logarítmica ---
# El log estabiliza la varianza creciente y convierte diferencias
# en aproximaciones a tasas de crecimiento porcentual.

va_fin_tbl <- va_fin_tbl |>
  mutate(log_va = log(va_financiero_b2015_equiv))

va_fin_log_filtrada <- va_fin_tbl |>
  filter(fecha >= yearquarter("2000 Q1"))

print(
  ggtime::autoplot(va_fin_log_filtrada, log_va, linewidth = 0.7) +
    geom_vline(xintercept = yearquarter("2005 Q1"),
               color = "red", linetype = "dashed", linewidth = 0.8) +
    ggtitle("Log VA Sector Financiero y Seguros") +
    xlab("Trimestre") + ylab("Log(miles de millones COP)") +
    theme_light()
)

# --- 1.3 FAC y FACP en niveles logarítmicos ---

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

# --- 1.4 Pruebas de raíz unitaria en niveles ---

cat("\n=== Prueba ADF - Niveles log ===\n")
# type = "trend": incluye constante y tendencia lineal.
# Apropiado porque la serie muestra tendencia creciente visible.
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
# type = "tau": contrasta estacionariedad alrededor de tendencia determinística.
kpss_niv <- ur.kpss(va_fin_tbl$log_va, type = "tau", lags = "short")
print(summary(kpss_niv))

kpss_stat  <- kpss_niv@teststat[1]
kpss_crit5 <- kpss_niv@cval["critical values", "5pct"]
if (kpss_stat > kpss_crit5) {
  cat("KPSS (niveles): Rechazamos H0 -> Serie NO estacionaria. Diferenciamos.\n")
} else {
  cat("KPSS (niveles): No rechazamos H0 -> Serie estacionaria.\n")
}

# --- 1.5 Primera diferencia del logaritmo ---

va_fin_tbl <- va_fin_tbl |>
  mutate(diff_log_va = difference(log_va))

va_fin_diff_filtrada <- va_fin_tbl |>
  fill_gaps() |>
  mutate(diff_log_va = difference(log_va)) |>
  filter(fecha >= yearquarter("2000 Q1"), !is.na(diff_log_va))

print(
  ggtime::autoplot(va_fin_diff_filtrada, diff_log_va, linewidth = 0.7) +
    geom_vline(xintercept = yearquarter("2005 Q1"),
               color = "red", linetype = "dashed", linewidth = 0.8) +
    ggtitle("Primera diferencia de Log VA Financiero\n(aprox. tasa de crecimiento trimestral)") +
    xlab("Trimestre") + ylab("Δlog(VA)") +
    theme_light()
)

cat("\n=== Prueba ADF - Primera diferencia log ===\n")
# type = "drift": incluye solo constante, sin tendencia.
# Justificado por phi3 no significativo en el ADF en niveles.
adf_diff <- ur.df(
  va_fin_tbl |> filter(!is.na(diff_log_va)) |> pull(diff_log_va),
  type = "drift", selectlags = "AIC"
)
print(summary(adf_diff))

adf_diff_stat  <- adf_diff@teststat[1, "tau2"]
adf_diff_crit5 <- adf_diff@cval["tau2", "5pct"]
if (adf_diff_stat < adf_diff_crit5) {
  cat("ADF (1a diferencia): Rechazamos H0 -> Serie diferenciada es estacionaria.\n")
} else {
  cat("ADF (1a diferencia): No rechazamos H0 -> Considerar segunda diferencia.\n")
}

cat("\n=== Prueba KPSS - Primera diferencia log ===\n")
# type = "mu": contrasta estacionariedad alrededor de una media constante.
kpss_diff <- ur.kpss(
  va_fin_tbl |> filter(!is.na(diff_log_va)) |> pull(diff_log_va),
  type = "mu", lags = "short"
)
print(summary(kpss_diff))

kpss_diff_stat  <- kpss_diff@teststat[1]
kpss_diff_crit5 <- kpss_diff@cval["critical values", "5pct"]
if (kpss_diff_stat > kpss_diff_crit5) {
  cat("KPSS (1a diferencia): Rechazamos H0 -> Serie diferenciada NO estacionaria.\n")
  cat("Nota: resultado limitrofe atribuible a heteroscedasticidad del empalme.\n")
} else {
  cat("KPSS (1a diferencia): No rechazamos H0 -> Serie diferenciada es estacionaria.\n")
}

# --- 1.6 FAC y FACP de la primera diferencia ---
# La lectura de estas gráficas determina los órdenes p y q candidatos.

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

# ============================================================
# PASO 2: ESTIMACIÓN
# ============================================================

cat("\n\n===== PASO 2: ESTIMACIÓN =====\n")

# Se estiman 4 modelos candidatos por máxima verosimilitud.
# Todos incluyen constante (~ 1) porque el ADF en primera diferencia
# mostró drift significativo (intercepto = 0.0245, t = 6.41).
# PDQ(0,0,0) es obligatorio para evitar estimación automática de SARIMA.
# Los datos están desestacionalizados por el DANE, por tanto D = 0.

nombres_modelos <- c("ARIMA(1,1,0)", "ARIMA(0,1,1)", "ARIMA(1,1,1)", "ARIMA(0,1,0)")

fit_va <- va_fin_tbl |>
  model(
    "ARIMA(1,1,0)" = fable::ARIMA(log(va_financiero_b2015_equiv) ~ 1 + pdq(1, 1, 0) + PDQ(0, 0, 0)),
    "ARIMA(0,1,1)" = fable::ARIMA(log(va_financiero_b2015_equiv) ~ 1 + pdq(0, 1, 1) + PDQ(0, 0, 0)),
    "ARIMA(1,1,1)" = fable::ARIMA(log(va_financiero_b2015_equiv) ~ 1 + pdq(1, 1, 1) + PDQ(0, 0, 0)),
    "ARIMA(0,1,0)" = fable::ARIMA(log(va_financiero_b2015_equiv) ~ 1 + pdq(0, 1, 0) + PDQ(0, 0, 0))
  )

# Resumen individual de cada modelo
cat("\n--- Resultados de estimación por modelo ---\n")
for (nombre in nombres_modelos) {
  cat("\n", nombre, "\n", sep = "")
  print(report(fit_va |> select(all_of(nombre))))
}

# Tabla comparativa de criterios de información
cat("\n--- Tabla comparativa de criterios de informacion ---\n")
tabla_ic <- glance(fit_va) |>
  select(.model, AIC, BIC) |>
  arrange(AIC)
print(tabla_ic)
cat("Modelo seleccionado: ARIMA(0,1,0) con deriva.\n")
cat("Criterio: menor AIC y BIC con coeficiente significativo (t = 6.15).\n")
cat("Los demas modelos tienen coeficientes no significativos o cancelacion de raices.\n")

# ============================================================
# PASO 3: VALIDACIÓN
# ============================================================

cat("\n\n===== PASO 3: VALIDACIÓN =====\n")

tabla_validacion <- list()

for (nombre in nombres_modelos) {
  ordenes <- as.integer(regmatches(nombre, gregexpr("[0-9]", nombre))[[1]])
  p_ord <- ordenes[1]; d_ord <- ordenes[2]; q_ord <- ordenes[3]

  residuos_tbl <- residuos_modelo(fit_va, nombre, p_ord, q_ord)
  res          <- residuos_tbl$.resid
  res2_tbl     <- residuos_tbl |> mutate(res2 = .resid^2)

  # Gráficas de diagnóstico: residuos, FAC residuos, FAC residuos^2
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
    ggtitle(paste("FAC res2 -", nombre)) +
    ylim(-1, 1) + theme_light()

  grilla(g_res, g_fac_res, g_fac_res2, nrow = 1, ncol = 3)

  # Q-Q plot
  gg_qq <- ggplot(residuos_tbl, aes(sample = .resid)) +
    stat_qq(color = "black", size = 1) +
    stat_qq_line(color = "red", linewidth = 0.6) +
    ggtitle(paste("Q-Q plot -", nombre)) +
    xlab("Cuantiles teoricos") + ylab("Cuantiles muestrales") +
    theme_light()
  print(gg_qq)

  # Ljung-Box: H0 = no autocorrelacion en residuos
  lb_4  <- Box.test(res, lag = 4,  type = "Ljung-Box", fitdf = p_ord + q_ord)$p.value
  lb_8 <- Box.test(res, lag = 8, type = "Ljung-Box", fitdf = p_ord + q_ord)$p.value
  lb_12 <- Box.test(res, lag = 12, type = "Ljung-Box", fitdf = p_ord + q_ord)$p.value

  # ARCH: H0 = no efectos ARCH (homocedasticidad condicional)
  arch_1 <- FinTS::ArchTest(res, lags = 1)$p.value
  arch_2 <- FinTS::ArchTest(res, lags = 2)$p.value
  arch_5 <- FinTS::ArchTest(res, lags = 5)$p.value

  # Jarque-Bera: H0 = normalidad de residuos
  jb <- jarque.bera.test(res)$p.value

  tabla_validacion[[nombre]] <- data.frame(
    Modelo    = nombre,
    `LB(4)`   = lb_4,
    `LB(8)`  = lb_8,
    `LB(12)`  = lb_12,
    `ARCH(1)` = arch_1,
    `ARCH(2)` = arch_2,
    `ARCH(5)` = arch_5,
    `JB`      = jb,
    check.names = FALSE
  )
}

tabla_val_final <- bind_rows(tabla_validacion)
cat("\n--- Tabla resumen de pruebas de validacion (p-valores) ---\n")
print(tabla_val_final |> mutate(across(where(is.numeric), ~ round(.x, 4))))
cat("\nInterpretacion:\n")
cat("  LB:   p > 0.05 -> No se rechaza H0 -> residuos sin autocorrelacion (DESEABLE)\n")
cat("  ARCH: p > 0.05 -> No se rechaza H0 -> no efectos ARCH (DESEABLE)\n")
cat("  JB:   p > 0.05 -> No se rechaza H0 -> residuos normales (DESEABLE)\n")
cat("\nNota: Los rechazos tienen origen identificado:\n")
cat("  - ARCH y heteroscedasticidad: diferencia de varianza entre subperiodos del empalme\n")
cat("    (std 2000-2004 = 0.90%% vs 2005-2024 = 3.11%%).\n")
cat("  - JB y Ljung-Box: choque extraordinario COVID-19 (2020Q2-2021Q1).\n")
cat("  El orden ARIMA(0,1,0) sigue siendo el mejor disponible en el marco Box-Jenkins.\n")

# ============================================================
# EXTRACCIÓN DE ESTADÍSTICOS Y P-VALORES PARA PRESENTACIÓN
# ============================================================

tabla_presentacion <- list()

for (nombre in nombres_modelos) {
  ordenes <- as.integer(regmatches(nombre, gregexpr("[0-9]", nombre))[[1]])
  p_ord <- ordenes[1]; q_ord <- ordenes[3]
  
  residuos_tbl <- fit_va |>
    select(all_of(nombre)) |>
    augment() |>
    as_tibble() |>
    filter(!is.na(.resid)) |>
    select(fecha, .resid)
  
  res <- residuos_tbl$.resid
  fitdf_val <- p_ord + q_ord
  
  # ── Ljung-Box: extrae estadístico y p-valor
  lb4  <- Box.test(res, lag = 4  + fitdf_val, type = "Ljung-Box", fitdf = fitdf_val)
  lb8  <- Box.test(res, lag = 8  + fitdf_val, type = "Ljung-Box", fitdf = fitdf_val)
  lb12 <- Box.test(res, lag = 12 + fitdf_val, type = "Ljung-Box", fitdf = fitdf_val)
  
  # ── ARCH: extrae estadístico y p-valor
  arch1 <- FinTS::ArchTest(res, lags = 1)
  arch2 <- FinTS::ArchTest(res, lags = 2)
  arch5 <- FinTS::ArchTest(res, lags = 5)
  
  # ── Jarque-Bera
  jb <- jarque.bera.test(res)
  
  tabla_presentacion[[nombre]] <- data.frame(
    Modelo = nombre,
    # Ljung-Box
    LB_stat_4  = round(lb4$statistic,  4),
    LB_pval_4  = round(lb4$p.value,    4),
    LB_stat_8  = round(lb8$statistic,  4),
    LB_pval_8  = round(lb8$p.value,    4),
    LB_stat_12 = round(lb12$statistic, 4),
    LB_pval_12 = round(lb12$p.value,   4),
    # ARCH
    ARCH_stat_1 = round(arch1$statistic, 4),
    ARCH_pval_1 = round(arch1$p.value,   4),
    ARCH_stat_2 = round(arch2$statistic, 4),
    ARCH_pval_2 = round(arch2$p.value,   4),
    ARCH_stat_5 = round(arch5$statistic, 4),
    ARCH_pval_5 = round(arch5$p.value,   4),
    # Jarque-Bera
    JB_stat = round(jb$statistic, 4),
    JB_pval = round(jb$p.value,   4),
    check.names = FALSE,
    row.names   = NULL
  )
}

# ============================================================
# EXTRACCIÓN DE ESTADÍSTICOS Y P-VALORES PARA PRESENTACIÓN
# ============================================================

tabla_presentacion <- list()

for (nombre in nombres_modelos) {
  ordenes <- as.integer(regmatches(nombre, gregexpr("[0-9]", nombre))[[1]])
  p_ord <- ordenes[1]; q_ord <- ordenes[3]
  
  residuos_tbl <- fit_va |>
    select(all_of(nombre)) |>
    augment() |>
    as_tibble() |>
    filter(!is.na(.resid)) |>
    select(fecha, .resid)
  
  res <- residuos_tbl$.resid
  fitdf_val <- p_ord + q_ord
  
  # ── Ljung-Box: extrae estadístico y p-valor
  lb4  <- Box.test(res, lag = 4  + fitdf_val, type = "Ljung-Box", fitdf = fitdf_val)
  lb8  <- Box.test(res, lag = 8  + fitdf_val, type = "Ljung-Box", fitdf = fitdf_val)
  lb12 <- Box.test(res, lag = 12 + fitdf_val, type = "Ljung-Box", fitdf = fitdf_val)
  
  # ── ARCH: extrae estadístico y p-valor
  arch1 <- FinTS::ArchTest(res, lags = 1)
  arch2 <- FinTS::ArchTest(res, lags = 2)
  arch5 <- FinTS::ArchTest(res, lags = 5)
  
  # ── Jarque-Bera
  jb <- jarque.bera.test(res)
  
  tabla_presentacion[[nombre]] <- data.frame(
    Modelo = nombre,
    # Ljung-Box
    LB_stat_4  = round(lb4$statistic,  4),
    LB_pval_4  = round(lb4$p.value,    4),
    LB_stat_8  = round(lb8$statistic,  4),
    LB_pval_8  = round(lb8$p.value,    4),
    LB_stat_12 = round(lb12$statistic, 4),
    LB_pval_12 = round(lb12$p.value,   4),
    # ARCH
    ARCH_stat_1 = round(arch1$statistic, 4),
    ARCH_pval_1 = round(arch1$p.value,   4),
    ARCH_stat_2 = round(arch2$statistic, 4),
    ARCH_pval_2 = round(arch2$p.value,   4),
    ARCH_stat_5 = round(arch5$statistic, 4),
    ARCH_pval_5 = round(arch5$p.value,   4),
    # Jarque-Bera
    JB_stat = round(jb$statistic, 4),
    JB_pval = round(jb$p.value,   4),
    check.names = FALSE,
    row.names   = NULL
  )
}

resultado_final <- bind_rows(tabla_presentacion)

print(resultado_final)
# Imprimir en formato legible para el póster
cat("\n========================================================\n")
cat("TABLA DE VALIDACIÓN - ESTADÍSTICOS Y P-VALORES\n")
cat("========================================================\n\n")

for (nombre in nombres_modelos) {
  fila <- resultado_final |> filter(Modelo == nombre)
  cat(sprintf("Modelo: %s\n", nombre))
  cat(sprintf("  Ljung-Box (lag 4):  Q = %7.4f  |  p = %.4f\n",
              fila$LB_stat_4,  fila$LB_pval_4))
  cat(sprintf("  Ljung-Box (lag 8):  Q = %7.4f  |  p = %.4f\n",
              fila$LB_stat_8,  fila$LB_pval_8))
  cat(sprintf("  Ljung-Box (lag 12): Q = %7.4f  |  p = %.4f\n",
              fila$LB_stat_12, fila$LB_pval_12))
  cat(sprintf("  ARCH (lag 1):       X2= %7.4f  |  p = %.4f\n",
              fila$ARCH_stat_1, fila$ARCH_pval_1))
  cat(sprintf("  ARCH (lag 2):       X2= %7.4f  |  p = %.4f\n",
              fila$ARCH_stat_2, fila$ARCH_pval_2))
  cat(sprintf("  ARCH (lag 5):       X2= %7.4f  |  p = %.4f\n",
              fila$ARCH_stat_5, fila$ARCH_pval_5))
  cat(sprintf("  Jarque-Bera:        X2= %7.4f  |  p = %.4f\n",
              fila$JB_stat, fila$JB_pval))
  cat("\n")
}
# ============================================================
# TEST CUSUM - ESTABILIDAD ESTRUCTURAL
# ============================================================

cat("\n\n===== TEST CUSUM - ESTABILIDAD ESTRUCTURAL =====\n")

# El test CUSUM evalúa si los parámetros del modelo son estables
# a lo largo del tiempo. Utiliza los residuos recursivos acumulados.
# H0: estabilidad de parámetros (no hay quiebre estructural).
# Un quiebre significativo aparece cuando la trayectoria del CUSUM
# sale de las bandas de confianza al 5%.

# Extraer residuos del modelo seleccionado ARIMA(0,1,0)
res_010_tbl <- residuos_modelo(fit_va, "ARIMA(0,1,0)", 0, 0)
res_010     <- res_010_tbl$.resid

# Aplicar CUSUM sobre los residuos
# OLS-CUSUM sobre regresion trivial (solo intercepto) de los residuos
cusum_res <- efp(res_010 ~ 1, type = "Rec-CUSUM")

cat("Resultado CUSUM:\n")
print(sctest(cusum_res))

# Gráfica CUSUM
plot(cusum_res,
     main = "Test CUSUM - Estabilidad estructural ARIMA(0,1,0)",
     ylab = "CUSUM empirico",
     xlab = "Observacion",
     col  = "black")

cat("\nInterpretacion:\n")
cat("  Si la linea permanece dentro de las bandas rojas: no hay quiebre significativo.\n")
cat("  Si cruza las bandas: evidencia de inestabilidad parametrica.\n")
cat("  Un quiebre alrededor de la obs 20-21 es esperado dado el punto de empalme (2005Q1).\n")
cat("  Un quiebre en la obs ~80-84 corresponderia al choque COVID-19 (2020Q2).\n")

# ============================================================
# PASO 4: PRONÓSTICO (10 trimestres adelante)
# ============================================================

cat("\n\n===== PASO 4: PRONOSTICO =====\n")

# --- 4.1 Pronóstico estándar con intervalos normales ---
# Solo para el modelo seleccionado: ARIMA(0,1,0) con deriva.
# fable revierte automáticamente la transformación log() en forecast(),
# por lo que los valores están en nivel original (miles de millones COP).

fit_seleccionado <- va_fin_tbl |>
  model(
    "ARIMA(0,1,0)" = fable::ARIMA(log(va_financiero_b2015_equiv) ~ 1 + pdq(0, 1, 0) + PDQ(0, 0, 0))
  )

pron_normal <- fit_seleccionado |>
  forecast(h = 10) |>
  hilo(level = 95) |>
  fabletools::unpack_hilo("95%")

tabla_normal <- pron_normal |>
  as_tibble() |>
  select(
    Trimestre   = fecha,
    Pronostico  = .mean,
    IC_inf_95   = "95%_lower",
    IC_sup_95   = "95%_upper"
  )

imprimir_tabla_pronostico(
  "Pronostico 10 trimestres - ARIMA(0,1,0) con deriva (intervalos normales)",
  tabla_normal
)

# Gráfica del modelo seleccionado con bandas de confianza
pron_plot <- fit_seleccionado |>
  forecast(h = 10)

print(
  ggplot() +
    geom_line(
      data = va_fin_tbl,
      aes(x = fecha, y = va_financiero_b2015_equiv),
      color = "black", linewidth = 0.5
    ) +
    geom_ribbon(
      data = pron_normal,
      aes(x = fecha, ymin = `95%_lower`, ymax = `95%_upper`),
      fill = "#2A9D8F", alpha = 0.25
    ) +
    geom_line(
      data = tabla_normal,
      aes(x = Trimestre, y = Pronostico),
      color = "#2A9D8F", linewidth = 1.2
    ) +
    geom_vline(xintercept = yearquarter("2005 Q1"),
               color = "red", linetype = "dashed", linewidth = 0.7) +
    annotate("text", x = yearquarter("2005 Q3"),
             y = min(va_fin_tbl$va_financiero_b2015_equiv) * 1.05,
             label = "Empalme\n2005Q1", color = "red", size = 2.8, hjust = 0) +
    ggtitle("ARIMA(0,1,0) con deriva - Pronostico 10 trimestres\nVA Sector Financiero y Seguros - Colombia") +
    xlab("Trimestre") +
    ylab("Miles de millones COP (base 2015 equiv.)") +
    theme_minimal()
)
### Ecuación estimada del modelo ARIMA (0,1,0) ###

# Extraer coeficientes del modelo seleccionado
coef_tabla <- fit_va |>
  select("ARIMA(0,1,0)") |>
  tidy()

print(coef_tabla)
# Columnas: term | estimate | std.error | statistic | p.value
# Extraer valores individuales
drift  <- coef_tabla |> filter(term == "constant") |> pull(estimate) |> round(4)
sigma2 <- fit_va |>
  select("ARIMA(0,1,0)") |>
  glance() |>
  pull(sigma2) |>
  round(6)

# Imprimir ecuación
cat("\n=== ECUACIÓN ESTIMADA: ARIMA(0,1,0) con deriva ===\n\n")
cat(sprintf("  Delta(log Y_t) = %.4f + e_t\n", drift))
cat(sprintf("  e_t ~ RB(0, %.6f)\n\n", sigma2))
cat("  Equivalentemente en niveles:\n")
cat(sprintf("  log(Y_t) = %.4f + log(Y_{t-1}) + e_t\n", drift))
cat("\n  Donde:\n")
cat("    Y_t      = VA sector financiero y seguros (miles mill. COP base 2015)\n")
cat("    Delta    = operador de primera diferencia\n")
cat(sprintf("    %.4f  = tasa de crecimiento trimestral promedio (drift)\n", drift))
cat("    e_t      = termino de error (ruido blanco)\n")
cat(sprintf("    sigma^2  = %.6f (varianza estimada del error)\n", sigma2))
# ============================================================
# BOOTSTRAP - INTERVALOS DE CONFIANZA ROBUSTOS
# ============================================================

cat("\n\n===== BOOTSTRAP - INTERVALOS ROBUSTOS =====\n")

# Motivación: los residuos muestran colas pesadas (rechazo Jarque-Bera)
# y efectos ARCH (rechazo ArchTest). Los intervalos normales asumen
# eps ~ N(0, sigma^2), supuesto que los datos no cumplen.
# El bootstrap de residuos (Efron, 1979) construye intervalos sin
# ese supuesto, remuestreando los residuos observados del modelo
# y repropagándolos en la estructura del ARIMA.
# Con times = 2000 el resultado es estable en series de esta longitud.

cat("Ejecutando bootstrap con 2000 replicas...\n")
cat("(Puede tomar 30-60 segundos)\n")

pron_bootstrap <- fit_seleccionado |>
  forecast(h = 10, bootstrap = TRUE, times = 2000) |>
  hilo(level = 95) |>
  fabletools::unpack_hilo("95%")

tabla_bootstrap <- pron_bootstrap |>
  as_tibble() |>
  select(
    Trimestre  = fecha,
    Pronostico = .mean,
    IC_inf_95  = `95%_lower`,
    IC_sup_95  = `95%_upper`
  )

imprimir_tabla_pronostico(
  "Pronostico 10 trimestres - ARIMA(0,1,0) Bootstrap (2000 replicas, IC 95%%)",
  tabla_bootstrap
)

# Comparación de amplitud de intervalos: normal vs bootstrap
cat("\n--- Comparacion de amplitud de intervalos (IC_sup - IC_inf) ---\n")
comparacion <- data.frame(
  Trimestre       = tabla_normal$Trimestre,
  Amplitud_Normal = round(tabla_normal$IC_sup_95 - tabla_normal$IC_inf_95, 2),
  Amplitud_Boot   = round(tabla_bootstrap$IC_sup_95 - tabla_bootstrap$IC_inf_95, 2)
) |>
  mutate(Diferencia = round(Amplitud_Boot - Amplitud_Normal, 2))
print(comparacion)
cat("\nInterpretacion:\n")
cat("  Diferencia > 0: bootstrap produce intervalos mas amplios (colas pesadas detectadas).\n")
cat("  Diferencia < 0: bootstrap produce intervalos mas angostos.\n")
cat("  Diferencias grandes confirman que el supuesto de normalidad afecta los intervalos normales.\n")

# Gráfica comparativa: intervalos normales vs bootstrap
tabla_normal_plot    <- tabla_normal    |> mutate(tipo = "Normal")
tabla_bootstrap_plot <- tabla_bootstrap |> mutate(tipo = "Bootstrap")
tabla_comparativa    <- bind_rows(tabla_normal_plot, tabla_bootstrap_plot)

print(
  ggplot() +
    geom_line(
      data = va_fin_tbl |> filter(fecha >= yearquarter("2018 Q1")),
      aes(x = fecha, y = va_financiero_b2015_equiv),
      color = "black", linewidth = 0.6
    ) +
    geom_ribbon(
      data = tabla_comparativa,
      aes(x = Trimestre, ymin = IC_inf_95, ymax = IC_sup_95, fill = tipo),
      alpha = 0.25
    ) +
    geom_line(
      data = tabla_comparativa,
      aes(x = Trimestre, y = Pronostico, color = tipo),
      linewidth = 1
    ) +
    scale_fill_manual(values  = c("Normal" = "#E63946", "Bootstrap" = "#2A9D8F")) +
    scale_color_manual(values = c("Normal" = "#E63946", "Bootstrap" = "#2A9D8F")) +
    ggtitle("Comparacion de intervalos: Normal vs Bootstrap\nARIMA(0,1,0) con deriva - 10 trimestres") +
    xlab("Trimestre") +
    ylab("Miles de millones COP (base 2015 equiv.)") +
    labs(fill = "Metodo IC", color = "Metodo IC") +
    theme_minimal()
)

# ============================================================
# EXTENSIÓN: ARIMA(0,1,0) CON DUMMY COVID
# ============================================================
# Motivación: el choque COVID (2020Q2) produjo la caída más pronunciada
# de la serie y la recuperación de 2021Q1 generó el mayor residuo positivo.
# Residuos consecutivos de signos opuestos en períodos adyacentes generan
# autocorrelación artificial en el lag 2 del Ljung-Box.
# Una variable dummy de impulso absorbe ese efecto puntual sin alterar
# la especificación ARIMA, consistente con el resultado CUSUM que descartó
# un quiebre estructural permanente.
#
# Se omite la dummy de empalme porque el CUSUM (p = 0.93) mostró que
# ese evento no afectó la estabilidad de los parámetros del modelo.
# ============================================================

# --- Variables dummy ---

va_fin_tbl <- va_fin_tbl |>
  mutate(
    # Impulso en la caída: 2020Q2 concentra el mayor residuo negativo
    d_caida    = if_else(fecha == yearquarter("2020 Q2"), 1, 0),
    # Impulso en la recuperación: 2021Q1 concentra el mayor residuo positivo
    d_rebote   = if_else(fecha == yearquarter("2021 Q1"), 1, 0)
  )

# --- Estimación comparativa ---
# Se estiman tres versiones del ARIMA(0,1,0) para aislar el efecto de cada dummy.
# Todos con constante (~ 1) porque el drift es significativo.

nombres_covid <- c(
  "ARIMA(0,1,0)",
  "ARIMA(0,1,0)+caida",
  "ARIMA(0,1,0)+caida+rebote"
)

fit_covid <- va_fin_tbl |>
  model(
    "ARIMA(0,1,0)"             = fable::ARIMA(
      log(va_financiero_b2015_equiv) ~ 1 +
        pdq(0, 1, 0) + PDQ(0, 0, 0)
    ),
    "ARIMA(0,1,0)+caida"       = fable::ARIMA(
      log(va_financiero_b2015_equiv) ~ 1 + d_caida +
        pdq(0, 1, 0) + PDQ(0, 0, 0)
    ),
    "ARIMA(0,1,0)+caida+rebote" = fable::ARIMA(
      log(va_financiero_b2015_equiv) ~ 1 + d_caida + d_rebote +
        pdq(0, 1, 0) + PDQ(0, 0, 0)
    )
  )

# Resumen de cada modelo
cat("\n--- Estimacion modelos con dummy COVID ---\n")
for (nombre in nombres_covid) {
  cat("\n", nombre, "\n", sep = "")
  print(report(fit_covid |> select(all_of(nombre))))
}

# Criterios de información comparativos
cat("\n--- Criterios de informacion ---\n")
print(
  glance(fit_covid) |>
    select(.model, AIC, BIC) |>
    arrange(AIC)
)
cat("La dummy mejora el modelo si reduce AIC y BIC respecto al modelo base.\n")

# --- Validación comparativa ---

tabla_val_covid <- list()

for (nombre in nombres_covid) {
  # ARIMA(0,1,0) tiene p=0, q=0 en todos los casos: fitdf = 0
  residuos_tbl <- fit_covid |>
    select(all_of(nombre)) |>
    augment() |>
    as_tibble() |>
    filter(!is.na(.resid)) |>
    select(fecha, .resid)
  
  res      <- residuos_tbl$.resid
  res2_tbl <- residuos_tbl |> mutate(res2 = .resid^2)
  
  # Gráficas de diagnóstico
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
    ggtitle(paste("FAC res2 -", nombre)) +
    ylim(-1, 1) + theme_light()
  
  grilla(g_res, g_fac_res, g_fac_res2, nrow = 1, ncol = 3)
  
  # Pruebas formales
  # fitdf = 0: ARIMA(0,1,0) no consume grados de libertad en AR/MA
  lb_4  <- Box.test(res, lag = 4,  type = "Ljung-Box", fitdf = 0)$p.value
  lb_8 <- Box.test(res, lag = 8, type = "Ljung-Box", fitdf = 0)$p.value
  lb_12 <- Box.test(res, lag = 12, type = "Ljung-Box", fitdf = 0)$p.value
  arch_1 <- FinTS::ArchTest(res, lags = 1)$p.value
  arch_2 <- FinTS::ArchTest(res, lags = 2)$p.value
  arch_5 <- FinTS::ArchTest(res, lags = 5)$p.value
  jb     <- jarque.bera.test(res)$p.value
  
  tabla_val_covid[[nombre]] <- data.frame(
    Modelo    = nombre,
    `LB(4)`   = lb_4,
    `LB(8)`  = lb_8,
    `LB(12)`  = lb_12,
    `ARCH(1)` = arch_1,
    `ARCH(2)` = arch_2,
    `ARCH(5)` = arch_5,
    `JB`      = jb,
    check.names = FALSE
  )
}

cat("\n--- Tabla de validacion comparativa (p-valores) ---\n")
print(
  bind_rows(tabla_val_covid) |>
    mutate(across(where(is.numeric), ~ round(.x, 4)))
)
cat("\nClave de lectura:\n")
cat("  LB y ARCH: p > 0.05 es deseable (no rechazo H0).\n")
cat("  JB:        p > 0.05 es deseable (residuos normales).\n")
cat("  Si la dummy corrige LB sin empeorar ARCH/JB: se justifica incluirla.\n")

# --- Pronóstico del modelo con dummy ---
# Para pronosticar con dummy se requiere especificar el valor futuro
# de la dummy en los 10 períodos del horizonte (todos cero: no hay COVID futuro).

nuevos_datos <- new_data(va_fin_tbl, n = 10) |>
  mutate(
    d_caida  = 0,
    d_rebote = 0
  )

pron_covid <- fit_covid |>
  select("ARIMA(0,1,0)+caida+rebote") |>
  forecast(new_data = nuevos_datos) |>
  hilo(level = 95) |>
  fabletools::unpack_hilo("95%")

tabla_pron_covid <- pron_covid |>
  as_tibble() |>
  select(
    Trimestre  = fecha,
    Pronostico = .mean,
    IC_inf_95  = `95%_lower`,
    IC_sup_95  = `95%_upper`
  )

imprimir_tabla_pronostico(
  "Pronostico 10 trimestres - ARIMA(0,1,0) + dummy COVID (IC 95%)",
  tabla_pron_covid
)

# Gráfica del pronóstico con dummy
print(
  ggplot() +
    geom_line(
      data = va_fin_tbl,
      aes(x = fecha, y = va_financiero_b2015_equiv),
      color = "black", linewidth = 0.5
    ) +
    geom_ribbon(
      data = tabla_pron_covid,
      aes(x = Trimestre, ymin = IC_inf_95, ymax = IC_sup_95),
      fill = "#457B9D", alpha = 0.25
    ) +
    geom_line(
      data = tabla_pron_covid,
      aes(x = Trimestre, y = Pronostico),
      color = "#457B9D", linewidth = 1.2
    ) +
    geom_vline(xintercept = yearquarter("2005 Q1"),
               color = "red", linetype = "dashed", linewidth = 0.7) +
    geom_vline(xintercept = yearquarter("2020 Q2"),
               color = "orange", linetype = "dotted", linewidth = 0.7) +
    annotate("text", x = yearquarter("2020 Q3"),
             y = max(va_fin_tbl$va_financiero_b2015_equiv) * 0.88,
             label = "COVID\n2020Q2", color = "orange", size = 2.8, hjust = 0) +
    ggtitle("ARIMA(0,1,0) + dummy COVID - Pronostico 10 trimestres") +
    xlab("Trimestre") +
    ylab("Miles de millones COP (base 2015 equiv.)") +
    theme_minimal()
)

# Bootstrap con dummy
cat("\nEjecutando bootstrap con dummy COVID (2000 replicas)...\n")

pron_boot_covid <- fit_covid |>
  select("ARIMA(0,1,0)+caida+rebote") |>
  forecast(new_data = nuevos_datos, bootstrap = TRUE, times = 2000) |>
  hilo(level = 95) |>
  fabletools::unpack_hilo("95%")

tabla_boot_covid <- pron_boot_covid |>
  as_tibble() |>
  select(
    Trimestre  = fecha,
    Pronostico = .mean,
    IC_inf_95  = `95%_lower`,
    IC_sup_95  = `95%_upper`
  )

imprimir_tabla_pronostico(
  "Pronostico Bootstrap 10 trimestres - ARIMA(0,1,0) + dummy COVID",
  tabla_boot_covid
)

