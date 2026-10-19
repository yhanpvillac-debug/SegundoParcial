#############################################################
######                                                 ######
#                 UNIVERSIDAD DEL QUINDÍO                   #
#                  PROGRAMA DE ECONOMÍA                     #
#                      ECONOMETRIA                          #
######                                                 ######
#############################################################

# BY: Yhan Pablo Villa Cuartas
# BY: Bryana Estiven Agudelo Arbelaez
# yhanp.villac@uqvirtual.edu.co
# +57 3137145342

# EXAMEN PARCIAL II - Modelo de regresión lineal múltiple
# Base: nls_panel.dta | Año: 1987
# Modelo: lwage = b0 + b1*educ + b2*exper + b3*tenure + b4*union + b5*black + u

# ==================================================================
# LIBRERÍAS
# ==================================================================

library(haven)    # leer .dta
library(dplyr)    # manipulación de datos
library(tidyr)    # pivot_longer
library(psych)    # describe()
library(writexl)  # exportar a Excel
library(readxl)   # leer Excel

# ==================================================================
# PUNTO 1: BASE DE DATOS Y FILTRO DE UN AÑO (1987)
# ==================================================================

nls <- read_dta("C:/Users/yhanp/Downloads/nls_panel.dta")

glimpse(nls)
table(nls$year)

nls87 <- nls %>% filter(year == 87)

nrow(nls87)   # 716 observaciones

# ==================================================================
# PUNTO 2: ESTADÍSTICOS DESCRIPTIVOS (posición, centro y dispersión)
# ==================================================================

vars <- nls87 %>%
  select(lwage, educ, exper, tenure, union, black)

describe(vars)

desc <- vars %>%
  summarise(across(everything(), list(
    media   = ~mean(.x),
    mediana = ~median(.x),
    min     = ~min(.x),
    Q1      = ~quantile(.x, 0.25),
    Q3      = ~quantile(.x, 0.75),
    max     = ~max(.x),
    desv    = ~sd(.x),
    var     = ~var(.x),
    rango   = ~max(.x) - min(.x),
    CV      = ~sd(.x) / mean(.x)
  ))) %>%
  pivot_longer(everything(),
               names_to = c("variable", ".value"),
               names_sep = "_")

print(desc, n = Inf)

# Variable ficticia union
table(nls87$union)
prop.table(table(nls87$union))

# ==================================================================
# PUNTO 3: MODELO, HIPÓTESIS Y CONTRASTE
# Variable explicada: lwage | Regresores: educ, exper, tenure,
# union (ficticia), black (ficticia)
# H0: b_union = 0   vs   H1: b_union > 0   (una cola, 5 %)
# ==================================================================

modelo <- lm(lwage ~ educ + exper + tenure + union + black, data = nls87)
s      <- summary(modelo)
s

gl <- modelo$df.residual   # n - k - 1 = 710

t_union <- s$coefficients["union", "t value"]
p_union <- pt(t_union, df = gl, lower.tail = FALSE)   # valor p de una cola
t_crit  <- qt(0.95, gl)

c(t = t_union, t_critico = t_crit, p_una_cola = p_union)
ifelse(t_union > t_crit, "Se rechaza H0", "No se rechaza H0")

# ==================================================================
# PUNTO 4: ESTIMACIÓN EN EXCEL
# Se exporta la base de 1987; la regresión se estimó en Excel con la
# herramienta Análisis de datos (resultados en la hoja "Hoja1").
# ==================================================================

write_xlsx(nls87, "nls_1987.xlsx")

# ==================================================================
# PUNTO 5 (PARTE A): CONTRASTE DE LOS RESULTADOS DE EXCEL CON R
# Objetivo: verificar que la estimación hecha en Excel (herramienta
# Análisis de datos) coincide con la estimación de R (lm).
# ==================================================================

archivo <- "C:/Users/yhanp/Downloads/parcial2_regresion_excel..xlsx"

# 1. Datos desde Excel y modelo en R -------------------------

datos_xl <- read_excel(archivo, sheet = "Datos")
modelo_xl <- lm(lwage ~ educ + exper + tenure + union + black, data = datos_xl)
s_xl      <- summary(modelo_xl)

# 2. Resultados de Excel (salida de Análisis de datos) -------

coef_xl <- read_excel(archivo, sheet = "Hoja1", range = "A16:E22")
names(coef_xl) <- c("variable", "coef_xl", "ee_xl", "t_xl", "p_xl")

glob_xl <- read_excel(archivo, sheet = "Hoja1", range = "B4:B8",
                      col_names = FALSE)[[1]]
# glob_xl = correlación múltiple, R2, R2 ajustado, error típico, n

F_xl <- read_excel(archivo, sheet = "Hoja1", range = "E12",
                   col_names = FALSE)[[1]]

# 3. Comparación de coeficientes -----------------------------

comparacion <- data.frame(
  variable   = coef_xl$variable,
  coef_R     = s_xl$coefficients[, 1],  coef_Excel = coef_xl$coef_xl,
  ee_R       = s_xl$coefficients[, 2],  ee_Excel   = coef_xl$ee_xl,
  t_R        = s_xl$coefficients[, 3],  t_Excel    = coef_xl$t_xl,
  p_R        = s_xl$coefficients[, 4],  p_Excel    = coef_xl$p_xl
)
comparacion$dif_coef <- abs(comparacion$coef_R - comparacion$coef_Excel)
comparacion$dif_ee   <- abs(comparacion$ee_R   - comparacion$ee_Excel)
comparacion$dif_t    <- abs(comparacion$t_R    - comparacion$t_Excel)
comparacion$dif_p    <- abs(comparacion$p_R    - comparacion$p_Excel)

print(comparacion, digits = 6)

# 4. Comparación de estadísticos globales --------------------

globales <- data.frame(
  medida = c("R2", "R2 ajustado", "Error típico", "Observaciones", "F"),
  R      = c(s_xl$r.squared, s_xl$adj.r.squared, s_xl$sigma, nobs(modelo_xl),
             s_xl$fstatistic[1]),
  Excel  = c(glob_xl[2], glob_xl[3], glob_xl[4], glob_xl[5], F_xl)
)
globales$diferencia <- abs(globales$R - globales$Excel)
print(globales, digits = 6)

# 5. Veredicto ------------------------------------------------

tol <- 1e-6
coinciden <- all(comparacion[, c("dif_coef","dif_ee","dif_t","dif_p")] < tol) &&
  all(globales$diferencia < tol)
cat(ifelse(coinciden,
           "R y Excel coinciden en todos los resultados (tolerancia 1e-6).\n",
           "Hay diferencias entre R y Excel: revisar las tablas anteriores.\n"))

# ==================================================================
# PUNTO 5 (PARTE B): LECTURA DE LA BONDAD DE AJUSTE DEL MODELO
# ==================================================================

# B1-B2: ajuste

s$r.squared; s$adj.r.squared; s$sigma

# B3: prueba F global (H0: b1 = b2 = b3 = b4 = b5 = 0)

s$fstatistic                                                               # F, gl1, gl2
pf(s$fstatistic[1], s$fstatistic[2], s$fstatistic[3], lower.tail = FALSE)  # valor p
qf(0.95, s$fstatistic[2], s$fstatistic[3])                                 # valor crítico al 5 %

# B4: significancia individual (pruebas t) e intervalos de confianza

s$coefficients
confint(modelo)

