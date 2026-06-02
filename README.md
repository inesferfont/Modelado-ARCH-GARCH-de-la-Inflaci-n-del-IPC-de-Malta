# Modelado ARIMA–GARCH de la Inflación del IPC en Malta

Este repositorio contiene el análisis completo de la serie temporal de la tasa interanual de inflación del Índice de Precios al Consumidor (IPC) de Malta (1960–2022), utilizando modelos ARIMA para la dinámica de la media y modelos GARCH para la volatilidad condicional.

El estudio se ha realizado en R e incluye análisis exploratorio, pruebas de estacionariedad, selección de modelos, diagnóstico de residuos, detección de heterocedasticidad y predicción.

---

## Objetivo del trabajo

El objetivo es modelizar la evolución de la inflación en Malta separando:

- **Estructura de la media** → mediante modelos ARIMA  
- **Estructura de la volatilidad** → mediante modelos ARCH/GARCH  

Esto permite capturar tanto la dinámica temporal como los periodos de alta volatilidad observados en la serie.

---

## Datos

- Variable: inflación interanual del IPC de Malta  
- Periodo: 1960–2022  
- Frecuencia: anual  
- Nº observaciones: 63  
- Fuente: base de datos proporcionada en clase (World Bank / WDI)

---

## Metodología

El análisis sigue los siguientes pasos:

1. **Análisis exploratorio de la serie temporal**
2. **Test de estacionariedad (ADF)**
3. **Diferenciación para lograr estacionariedad**
4. **Identificación y estimación de modelos ARIMA**
5. **Diagnóstico de residuos**
   - Autocorrelación (Ljung–Box)
   - Normalidad (Lilliefors)
   - Heterocedasticidad (Breusch–Pagan y ARCH-LM)
6. **Detección de volatilidad agrupada**
7. **Estimación de modelo GARCH(1,1)**
8. **Pronósticos a corto plazo (5 años)**

---

## Modelos utilizados

### Modelo de la media
- ARIMA(1,1,1)

Este modelo captura la estructura temporal de la serie diferenciada y elimina la autocorrelación en la media.

---

### Modelo de volatilidad
- GARCH(1,1) con media ARMA(1,1)

Permite modelizar la **heterocedasticidad condicional**, capturando episodios de alta volatilidad.

---

## Principales resultados

- La serie **no es estacionaria en niveles**, pero sí tras una diferenciación (d = 1).
- El modelo ARIMA(1,1,1) ajusta adecuadamente la media (residuos sin autocorrelación).
- Se detecta **heterocedasticidad condicional (efectos ARCH)**.
- El modelo GARCH(1,1) es estadísticamente significativo:
  - Alta persistencia de volatilidad (α + β ≈ 0.96)
- Se observan episodios de alta volatilidad asociados a shocks macroeconómicos (años 70–80).
- Los residuos no son normales, lo que refuerza la necesidad de modelos GARCH.

---

## Predicciones

Se generan pronósticos a 5 años:

- La **media prevista** muestra una corrección a la baja y posterior estabilización.
- La **volatilidad prevista** se mantiene persistente y ligeramente creciente.

---

## Contenido del repositorio
├── código en R/ 
├── Excel con series temporales/
├── memoria/ # Informe en PDF / LaTeX
└── README.md


---

## Herramientas utilizadas

- R
- Paquetes:
  - `forecast`
  - `tseries`
  - `FinTS`
  - `rugarch`
  - `lmtest`
  - `nortest`
- Modelado estadístico de series temporales
- Análisis de volatilidad financiera

---

## Conclusión

El modelo combinado **ARIMA(1,1,1) + GARCH(1,1)** proporciona una representación adecuada de la serie de inflación de Malta, capturando tanto la dinámica de la media como la persistencia de la volatilidad.

Este tipo de modelos es especialmente útil para series macroeconómicas con shocks estructurales y periodos de inestabilidad.

