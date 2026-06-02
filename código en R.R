# ============================================================
# TRABAJO ARCH/GARCH - SERIE TEMPORAL: MALTA
# Inés Fernández Fontán
# ============================================================

## ---- 1. INSTALACIÓN Y CARGA DE PAQUETES ----
paquetes <- c("tidyverse", "lubridate", "car", "urca", "tseries",
              "astsa", "forecast", "lmtest", "dynlm", "nlme",
              "xts", "rugarch", "kableExtra", "knitr", "MASS",
              "FinTS", "fGarch", "nortest", "readxl")

for (p in paquetes) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

## ---- 2. CARGA DE DATOS ----
library(readxl)


# Carga saltándose las 2 primeras filas y usando la columna por posición
bd <- read_excel("C:/Users/inesf/Documents/MÁSTER/UCM/Series Temporales/Trabajo Individual/BD-(G)ARCH.xlsx",
                 skip = 2,        # salta filas 1 y 2 (Código + País)
                 col_names = FALSE)

# Vemos la estructura
head(bd)
str(bd)
names(bd)

# Malta está en la columna 24:
malta_raw <- bd[[24]]   
anios      <- bd[[1]]

# Eliminamos NAs si los hay
malta_raw <- na.omit(malta_raw)
length(malta_raw)  # Verificamos cuántas observaciones tenemos

# Creamos dataframe conjunto y limpiamos NAs
datos_completos <- data.frame(anio = anios, malta = malta_raw)
datos_completos <- na.omit(datos_completos)


## ---- 3. CONVERTIR A SERIE TEMPORAL ----
# Verificamos los años disponibles en Excel
cat("Primer año:", min(datos_completos$anio, na.rm = TRUE), "\n")
cat("Último año:", max(datos_completos$anio, na.rm = TRUE), "\n")
cat("Nº de observaciones:", nrow(datos_completos), "\n")

anio_inicio <- 1960

Malta <- ts(malta_raw, start = anio_inicio, frequency = 1)
Malta




# PARA LA TABLA EN LÁTEX
start(Malta)   # primer año
end(Malta)     # último año
length(Malta) # no. de observaciones
mean(Malta, na.rm = TRUE)  # media
sd(Malta, na.rm = TRUE) # desviación típica











## ---- 4. ANÁLISIS EXPLORATORIO ----
# Gráfico de la serie
par(mfrow = c(1,1))
plot(Malta, main = "Serie Temporal: Malta", 
     ylab = "Valor", xlab = "Año", col = "steelblue", lwd = 2)

# Estadísticos
summary(Malta)
sd(Malta)

## ---- 5. COMPROBACIÓN DE ESTACIONARIEDAD ----
# Test ADF (Augmented Dickey-Fuller)
# H0: la serie tiene raíz unitaria (no estacionaria)
# H1: la serie es estacionaria
adf.test(Malta)
# El p-value = 0.1671 > 0.05 → No rechazamos H0 → La serie NO es estacionaria 


# Graficamos ACF y PACF de la serie original para confirmarlo
par(mfrow = c(1,2))
acf(Malta, lag.max = 20, main = "ACF - Malta original")
pacf(Malta, lag.max = 20, main = "PACF - Malta original")
par(mfrow = c(1,1))

# ACF (izquierda)
#   Las barras decrecen lentamente desde valores altos (~0.8) hacia valores negativos
#   Hay muchos lags significativos (superan las bandas azules de confianza)
#   El patrón es de decaimiento lento y gradual
#   ACF decrece muy lentamente --> No estacionariedad / raíz unitaria

# PACF (derecha)
#   El lag 1 es claramente significativo.
#   Aparece un pico negativo aislado alrededor del lag 7–8
#   pero su magnitud es pequeña y apenas sobresale de la banda de confianza.
#   En conjunto, el patrón sugiere un componente AR de orden bajo (AR(1) o similar).

# Cuando la ACF decae lentamente sin cortarse, la serie tiene memoria larga → necesita diferenciación (d=1)



# ---- DIFERENCIAMOS la serie (d=1) (diferenciación regular) ----
dMalta <- diff(Malta,lag=1, differences = 1)

# Test ADF sobre la serie diferenciada
adf.test(dMalta)
# El p-value < 0.01 < 0.05 → Rechazamos H0 → La serie diferenciada SÍ es estacionaria
# El aviso p-value smaller than printed p-value significa que el p-value real es incluso menor
# que 0.01 (por ejemplo 0.003), R simplemente lo redondea a 0.01. Es una buena noticia, refuerza 
# el resultado.

# Graficamos la serie diferenciada
plot(dMalta, main = "Serie Malta diferenciada (d=1)", 
     ylab = "Diferencia", xlab = "Año", 
     col = "steelblue", lwd = 2)
# Gráfico de la serie diferenciada
# Fluctúa alrededor de cero --> Confirma estacionariedad
# Sin tendencia visible --> La diferenciación eliminó la tendencia
# Volatilidad irregular --> Mayor variabilidad en 1975-1985, menor después
# Picos extremos en ~1980 --> Posible heterocedasticidad / variabilidad irregular (motiva el modelo GARCH)



# ACF y PACF de la serie diferenciada (para identificar p y q del ARIMA)
par(mfrow = c(1,2))
acf(dMalta, lag.max = 20, main = "ACF - Malta diferenciada")
pacf(dMalta, lag.max = 20, main = "PACF - Malta diferenciada")
par(mfrow = c(1,1))

# ACF (izquierda)
#  Lag 1: pico negativo, sobrepasa un poco el límite de significación
#  El resto: dentro de las bandas → no significativos
#  No hay patrón de decaimiento lento → serie estacionaria confirmada

# PACF (derecha)
#  Lag 1: pico negativo, barra significativa 
#  El resto: dentro de las bandas

# Como hemos diferenciado 1 vez --> d=1
# Como hay pico en lag=1 en el PACF --> p=1 --> podría mostrar un AR(1)
# Como hay pico en lag=1 en el ACF --> q=1 (q=0 podría ser otra opción ya que el pico no es muy pronunciado)
# Posibles candidatos (ARIMA(p,d,q)): ARIMA(1,1,1), ARIMA(1,1,0)
# p>=q !!!

# probar ARIMA(1,1,1) --> calibrar el GARCH(1,1) --> ver las limitaciones







# Continuamos con auto.arima para confirmar
fitAuto <- auto.arima(dMalta, allowdrift = FALSE, trace = TRUE,
                      stepwise = FALSE, approximation = FALSE)
summary(fitAuto)
coeftest(fitAuto)
# El auto.arima escoge el modelo ARIMA(0,0,1) para la serie diferenciada
# Por lo que el auto.arima escoge el modelo ARIMA(0,1,1) para la serie original
# Pero no tiene sentido escoger un ARIMA(0,1,1) porque para poder calibrar un GARCH
# se debe cumplir que p>=q




## ---- 6. IDENTIFICACIÓN DEL MODELO ARIMA ----
# PROBAMOS CON UN ARIMA(1,1,1)
fitARIMA <- arima(Malta, order = c(1,1,1), method = "ML")
summary(fitARIMA)
coeftest(fitARIMA)

# Obtenemos los siguientes coeficientes:
#  ar1=0.5735 muy significativo
#  ma1 = -1.0000 muy significativo

# Ese ma1 tan cercano a −1 es típico cuando la serie
# tiene una fuerte componente de tendencia que se elimina
# al diferenciar. No es un problema para nosotros.

# Obtenemos además una varianza residual de
# sigma^2 = 6.04 --> varianza moderada






## ---- 7. VALIDACIÓN DEL MODELO ARIMA ----

# --- 7a. Independencia de residuos (Test de Ljung-Box) ---
checkresiduals(fitARIMA)

# H0: residuos independientes (no hay autocorrelación)
# H1: residuos NO independientes

# p-value = 0.5417 > 0.05 → No rechazamos H0 → Los residuos son independientes (ruido blanco)
# Esto significa que el ARIMA(1,1,1) captura bien la estructura de la media de la serie.







# --- 7b. Homocedasticidad (Breusch-Pagan) ---
res <- fitARIMA$residuals
n <- length(res)
regresor <- 1:n
bp_test <- lmtest::bptest(res ~ regresor)
print(bp_test)

# H0: homocedasticidad (varianza constante) → revisar con ArchTest igualmente
# H1: heterocedasticidad → procedemos con GARCH

# Como p-value = 0.1742 > 0.05 → No rechazamos H0 → Los residuos son homocedásticos (varianza constante)

# Según el test de Breusch-Pagan, no se detecta heterocedasticidad lineal
# (tendencia en la varianza), pero NO detecta heterocedasticidad condicional
# del tipo ARCH, que es agrupamiento de volatilidad (clusters). Por eso es 
# imprescindible hacer el ArchTest a continuación.

# Según este test, la varianza no depende linealmente del tiempo.
# Pero esto NO descarta que exista heterocedasticidad tipo ARCH





# --- 7c. Test ARCH ---
# H0 (p > 0.05) = NO hay efecto ARCH
# H1 (p < 0.05) = SÍ hay efecto ARCH --> necesitamos GARCH

library(FinTS)
ArchTest1 <- ArchTest(res, lags = 1, demean = TRUE)
print(ArchTest1)
# p-value = 0.2167 > 0.05 → No rechazamos H0 → No se detectan efectos ARCH en lag 1
# El test con lag=1 no es suficiente. 

ArchTest2 <- ArchTest(res, lags = 2, demean = TRUE)
print(ArchTest2)
# p-value = 0.4335 > 0.05 → No se detectan efectos ARCH en lag=2 tampoco.

ArchTest3 <- ArchTest(res, lags = 3, demean = TRUE)
print(ArchTest3)
# p-value = 3.386e-05 < 0.001 → Rechazamos H0 → Existen efectos ARCH significativos en lag=3

ArchTest4 <- ArchTest(res, lags = 4, demean = TRUE)
print(ArchTest4)
# p-value = 0.0001048 < 0.001 → Rechazamos H0 → Existen efectos ARCH significativos en lag=4

# Aunque ARCH(1) y ARCH(2) no son significativos, la significatividad en lag 3 y 4
# justifica estimar un modelo GARCH(1,1)







# --- 7d. Estructura de autocorrelación de residuos^2 ---
# Residuos al cuadrado
rescuad <- resid(fitARIMA)^2
plot(rescuad, type = "l", 
     main = "Residuos al cuadrado - ARIMA(1,1,1)",
     col = "red", ylab = "Residuos²")

# Interpretación de la gráfica
# 1960-1970 : Residuos^2 pequeños, varianza baja y estable
# 1970-1985 : Picos muy elevados (máximo en ~1980)
# 1985-2020 : Vuelta a valores bajos, varianza reducida
# 2020-2023 : Ligero repunte al final

# Hay Heterocedasticidad condicional 
# Los picos se agrupan en el tiempo (1975-1985) 
# La varianza NO es constante a lo largo del tiempo --> Heterocedasticidad condicional
# Periodos de alta volatilidad seguidos de alta volatilidad --> Patrón ARCH/GARCH 
# Periodos de baja volatilidad seguidos de baja volatilidad --> Memoria en la varianza

# Este gráfico es la evidencia de que necesitamos un modelo GARCH.
# Si la varianza fuera constante, los residuos²
# serían una línea horizontal sin picos agrupados.

# Contexto económico del pico en ~1980:
# Los picos coinciden con las crisis del petróleo (1973 y 1979)
# y la inestabilidad económica global de los 70-80, que afectó
# especialmente a economías pequeñas como Malta.



# Reseteamos márgenes
par(mfrow = c(1,2))
# ACF y PACF de residuos al cuadrado
acf(rescuad, lag.max = 20, main = "ACF - Residuos²")
pacf(rescuad, lag.max = 20, main = "PACF - Residuos²")
par(mfrow = c(1,1))

# ACF - Residuos²
#  Solo hay barra significativa en lag 3-4 (yo diría lag 3)

# PACF - Residuos²
#  Hay barra significativa positiva en lag 2-3 (yo diría lag 3)
#  Hay otra barra significativa negativa en lag 6-7 (yo diría lag 6)

# Al haber más de un lag relevante, esto encaja con lo que
# vimos anteriormente en los ARCH tests (significativos a
# partir de orden 3)

# Por tanto, es razonable calibrar un modelo GARCH(1,1)






# --- 7c. Normalidad ---
library(nortest)
lillie.test(res)

# H0: los residuos siguen una distribución normal
# H1: los residuos NO son normales

# p-value = 0.000533 < 0.05 → Rechazamos H0 → Los residuos NO siguen una distribución normal

# La normalidad no es requisito para el modelo GARCH
# De hecho, la no normalidad es una señal adicional de que puede haber
# efectos ARCH (volatilidad agrupada), lo que justifica aún más el modelo GARCH.












## ---- 9. CALIBRACIÓN DEL MODELO GARCH(1,1) ----
library(rugarch)

# Especificación GARCH(1,1) con media ARIMA(1,1,1)
ug_spec <- ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE)
)

# Estimación del modelo
ugfit <- ugarchfit(spec = ug_spec, data = Malta)
ugfit

# El modelo ha convergido correctamente
# Modelo estimado: ARIMA(1,1,1) + GARCH(1,1)
# Distribución: normal
# Log-likelihood: -127.05
# AIC ≈ 4.22

# Interpretación de los coeficientes

# Media (ARIMA)
#   mu = 2.35, muy significativo → crecimiento medio anual positivo
#   ar1 = 0.27, no significativo → el componente AR pierde fuerza una vez modelada la varianza
#   ma1 = 0.34, casi significativo (p ≈ 0.067) → efecto MA moderado
# Cuando introducimos GARCH, parte de la estructura que antes capturaba el ARIMA pasa a la varianza

# Varianza (GARCH)
#   omega = 0.722, significativo --> Varianza base positiva
#   alpha1 = 0.603, significativo --> Fuerte efecto ARCH: los shocks recientes influyen mucho en la volatilidad.
#   beta1 = 0.355, significativo --> Persistencia moderada de la volatilidad.

# Persistencia total: 
#   alpha1 + beta1 = 0.60286 + 0.35480 = 0.95766 < 1
#   Es un valor muy alto --> la volatilidad es persistente aunque no explosiva











## ---- 10. ANÁLISIS DE LA VARIANZA CONDICIONAL ----
# Varianza condicional estimada
ug_var <- ugfit@fit$var

# Residuos al cuadrado vs varianza condicional
ug_res <- (ugfit@fit$residuals)^2

plot(ug_res, type = "l", 
     main = "Residuos² vs Varianza Condicional",
     ylab = "Valor", col = "black")
lines(ug_var, col = "green", lwd = 2)
legend("topright", legend = c("Residuos²", "Varianza condicional"),
       col = c("black", "green"), lty = 1, lwd = c(1,2))




# Interpretación Residuos² vs Varianza Condicional GARCH(1,1)
# Pico enorme ~índice 20 --> Corresponde al periodo ~1980 (crisis del petróleo)
# Línea verde (varianza condicional) --> Sigue y suaviza los picos de los residuos²
# Después del pico --> Ambas líneas vuelven a valores bajos
# Cola final --> Ligero repunte (~2020-2023)

# El GARCH funciona correctamente
# La línea verde sigue los picos negros --> El GARCH captura los periodos de alta volatilidad
# La línea verde es más suave que los residuos² --> El GARCH suaviza la volatilidad (no sobreajusta)
# La varianza condicional no es constante --> Confirma heterocedasticidad condicional modelizada
# Vuelve a valores bajos tras el shock --> La volatilidad es estacionaria (alpha1+beta1=0.958<1)

# El modelo GARCH está capturando exactamente lo que debe: la varianza condicional alta cuando hay shocks y baja en periodos tranquilos










## ---- 11. PREDICCIONES ----
# Pronóstico a 5 años
ugfore <- ugarchforecast(ugfit, n.ahead = 5)
ugfore

# Series → predicción de la media (el valor esperado de Malta en los próximos años)
# Sigma → predicción de la desviación estándar condicional (volatilidad futura)

# La media prevista baja desde 4.75 hasta 2.36 → el modelo anticipa una desaceleración
# La volatilidad prevista sube ligeramente (3.30 → 3.45) → la incertidumbre aumenta un poco con el horizonte



# Gráfico de predicciones
par(mfrow = c(1,1))
plot(ugfore, which = 1)  # media
plot(ugfore, which = 3)  # sigma



# La gráfica de la izquierda muestra la serie histórica
# (línea azul), la predicción de la media (línea roja)
# y las bandas de confianza +-1 \sigma (desviación típica)

# La predicción no continúa la tendencia creciente del
# pasado, sino que corrige hacia valores más bajos.
# El primer año predicho (T+1) cae desde el último dato
# observado. Los siguientes años muestran una suave
# convergencia hacia un nivel estable, alrededor de 2–3.



# La gráfica de la derecha muestra la volatilidad histórica
# (línea azul) y la volatilidad futura (línea roja).

# Se observa que la volatilidad predicha aumenta drásticamente
# en los primeros años del horizonte y luego se estabiliza
# en torno al 3.4.

# La subida brusca que observamos en la gráfica de la 
# volatilidad pronosticada es normal ya que la serie tiene
# pocos datos, hay shocks fuertes en el pasado (como los 
# picos de 1975–1985) y la suma de alpha1+beta1=0.96 es muy
# cercano a 1.

# Esto indica que los episodios de alta volatilidad
# observados en el pasado continúan afectando a la incertidumbre
# futura, y que la serie no vuelve rápidamente a niveles de 
# estabilidad, sino que mantiene un nivel de riesgo moderado 
# y persistente.

