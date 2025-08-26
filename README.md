# Turismo-vs-Petroleo-Dependencia-Economica-y-Desarrollo
proyecto de ciencia de datos

🌍 Turismo vs Petróleo: Dependencia Económica y Desarrollo (1995–2018)

## 📌 Objetivo
Analizar cómo la dependencia económica de los países en **turismo internacional** o en **exportaciones de petróleo crudo** influye en su evolución socioeconómica, medida a través de:

- PIB per cápita (constante)  
- Índice de Desarrollo Humano (IDH)  
- Tasa de desempleo  

---

## ❓ Pregunta de investigación
¿Los países altamente dependientes del turismo evolucionan mejor o peor que los países dependientes del petróleo, en términos de crecimiento económico y bienestar social?

---

## 📊 Fuentes de datos
- **Banco Mundial (WDI):** PIB per cápita, desempleo, exportaciones (% PIB)  
- **UNWTO (Turismo Internacional):** Ingresos por turismo  
- **UN Comtrade Database:** Exportaciones de crudo  
- **PNUD:** Índice de Desarrollo Humano (IDH)  

**Período de análisis:** 1995 – 2018  
**Muestra:** 6 países dependientes del petróleo y 6 países dependientes del turismo.  
(No se incluyeron los años de pandemia por su impacto atípico en la economía global).  

---

## ⚙️ Metodología
1. Limpieza y transformación de los datos.  
2. Deflactación de exportaciones y turismo (base 10).  
3. Cálculo del grado de dependencia económica (peso del sector en el PIB).  
4. Clasificación de países: **Dependencia Petróleo** vs **Dependencia Turismo**.  
5. Modelos de regresión con **efectos fijos (país y año)** y **errores agrupados por país**.  
6. Comparación de resultados.  

---

## 📂 Código en R

### 1. Cargar librerías
```r
library(tidyverse)
library(fixest)
library(readxl)
2. Importar datos
r
Copiar
Editar
# Cargar base en formato Excel
turismovspetroleo <- read_excel("turismovspetroleo.xlsx")

# Vista rápida
head(turismovspetroleo)
3. Transformaciones de variables
r
Copiar
Editar
# Deflactar y log-transformar variables clave
turismovspetroleo_cons <- turismovspetroleo %>%
  mutate(
    pib_perca_log = log(PIB_percapita_constante),
    log_desemp = log(Desempleo + 1),
    peso_sector_pib = Exportaciones_sector / PIB_total,
    dependencia = factor(dependencia, levels = c("petroleo", "turismo"))
  )
4. Modelos por dependencia (separados)
r
Copiar
Editar
# PETRÓLEO
m1_idh_pet <- feols(IDH ~ peso_sector_pib + pib_perca_log + log_desemp | pais + anio,
                    data = filter(turismovspetroleo_cons, dependencia == "petroleo"),
                    cluster = ~pais)

m1_pib_pet <- feols(pib_perca_log ~ peso_sector_pib + log_desemp | pais + anio,
                    data = filter(turismovspetroleo_cons, dependencia == "petroleo"),
                    cluster = ~pais)

m1_desemp_pet <- feols(log_desemp ~ peso_sector_pib + pib_perca_log | pais + anio,
                       data = filter(turismovspetroleo_cons, dependencia == "petroleo"),
                       cluster = ~pais)

# TURISMO
m1_idh_tur <- feols(IDH ~ peso_sector_pib + pib_perca_log + log_desemp | pais + anio,
                    data = filter(turismovspetroleo_cons, dependencia == "turismo"),
                    cluster = ~pais)

m1_pib_tur <- feols(pib_perca_log ~ peso_sector_pib + log_desemp | pais + anio,
                    data = filter(turismovspetroleo_cons, dependencia == "turismo"),
                    cluster = ~pais)

m1_desemp_tur <- feols(log_desemp ~ peso_sector_pib + pib_perca_log | pais + anio,
                       data = filter(turismovspetroleo_cons, dependencia == "turismo"),
                       cluster = ~pais)
5. Resultados individuales

# Comparar modelos de países petroleros
etable(m1_idh_pet, m1_pib_pet, m1_desemp_pet, se = "cluster", cluster = "pais", tex = FALSE)

# Comparar modelos de países turísticos
etable(m1_idh_tur, m1_pib_tur, m1_desemp_tur, se = "cluster", cluster = "pais", tex = FALSE)
6. Modelos conjuntos con interacción

# Interacción: turismo vs petróleo
m_idh_log <- feols(IDH ~ peso_sector_pib * dependencia + log_desemp + pib_perca_log | pais + anio,
                   data = turismovspetroleo_cons, cluster = ~pais)

m_pib_log <- feols(pib_perca_log ~ peso_sector_pib * dependencia + log_desemp | pais + anio,
                   data = turismovspetroleo_cons, cluster = ~pais)

m_desemp_log <- feols(log_desemp ~ peso_sector_pib * dependencia + pib_perca_log | pais + anio,
                      data = turismovspetroleo_cons, cluster = ~pais)

## resultados del los modelos en r:

                                            m_idh_log        m_pib_log
Dependent Var.:                                   IDH    pib_perca_log
                                                                      
peso_sector_pib                      -0.0669 (0.0487)  0.2046 (0.4046)
log_desemp                           -0.0524 (0.0418) -0.1890 (0.1467)
pib_perca_log                        0.0932. (0.0440)                 
peso_sector_pib x dependenciaTURISMO 0.2763. (0.1456)  0.5148 (0.6516)

Fixed-Effects:                       ---------------- ----------------
pais                                              Yes              Yes
anio                                              Yes              Yes

____________________________________ ________________ ________________
S.E.: Clustered                              by: pais         by: pais
Observations                                      288              288
R2                                            0.89700          0.98757
Within R2                                     0.18204          0.10239

                                         m_desemp_log
Dependent Var.:                            log_desemp
                                                     
peso_sector_pib                       0.0730 (0.1823)
log_desemp                                           
pib_perca_log                        -0.4030 (0.2333)
peso_sector_pib x dependenciaTURISMO  1.602* (0.5858)

Fixed-Effects:                       ----------------
pais                                              Yes
anio                                              Yes

____________________________________ ________________
S.E.: Clustered                              by: pais
Observations                                      288
R2                                            0.87760
Within R2                                     0.15449

---
Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' '

# Interpretación de Resultados: Turismo vs. Petróleo  

## 📊 Resumen de los Modelos

### Modelo IDH (`m_idh_log`)
- **peso_sector_pib**: no es significativo (coef. ≈ -0.067).  
- **log_desemp**: efecto negativo sobre el IDH (coef. ≈ -0.052, aunque débil).  
- **pib_perca_log**: positivo y casi significativo (coef. ≈ 0.0932).  
- **Interacción con Turismo**: coef. 0.2763 → el turismo aumenta el IDH en comparación con el petróleo.  

### Modelo PIB per cápita (`m_pib_log`)
- **peso_sector_pib**: positivo (0.20), pero con gran error estándar → no concluyente.  
- **log_desemp**: negativo (−0.189), sin significancia estadística.  
- **Interacción con Turismo**: coef. 0.51 → el turismo se asocia con más PIB per cápita frente al petróleo.  

### Modelo Desempleo (`m_desemp_log`)
- **pib_perca_log**: coef. −0.403 → cuando el PIB per cápita aumenta, el desempleo tiende a bajar.  
- **Interacción con Turismo**: coef. 1.602* → el turismo incrementa el desempleo en comparación con el petróleo.  

## 🔎 Interpretación Substantiva  

### Turismo
**Mejora el bienestar general** (calidad de vida, servicios, educación, infraestructura):  
- Genera divisas (ingresos de visitantes internacionales).  
- Dinamiza sectores como transporte, comercio, construcción, hoteles y restaurantes.  
- Incentiva inversión en conectividad, cultura y desarrollo local.  
- En varios países, se ha vinculado con aumentos en el **IDH** (salud, educación y nivel de vida).  

**Problema laboral:**  
- Trabajos estacionales (alta demanda en temporada, caída en baja).  
- Alta informalidad (guías, comercio callejero, transportes alternativos).  
- Sueldos bajos frente a sectores extractivos.  
- Menor protección social (contratos temporales).  
➡️ Se traduce en más **desempleo/inestabilidad**, incluso si hay mayor ingreso agregado.  

---

### Petróleo
**Beneficios:**  
- Genera ingresos fiscales elevados y empleos estables.  
- Salarios superiores al promedio nacional.  

**Problemas:**  
- Impacto limitado en el **IDH** porque beneficia a una fracción reducida de la población.  
- Alta dependencia produce **"enfermedad holandesa"** → menor diversificación económica.  
- Efectos negativos en sostenibilidad y equidad social.  

---

## 📌 Conclusión
- **Turismo** → mayor bienestar general (**IDH y PIB per cápita** positivos), pero con mayor **precariedad laboral**.  
- **Petróleo** → más empleos estables y salarios altos, pero con un impacto **menos inclusivo** en calidad de vida amplia.  

👉 Aunque el modelo tiene valores  debiles y algunos resultados poca significativos,este modelo  general y basico,nos da una idea,asi como en la representacion de los gráficos que **el turismo contribuye más al bienestar colectivo que el sector petróleo, sus ingresos a lo largo del tiempo son mas estable,una tendencia  alcista mas sostenida en el tiempo,sin fluctuaciones tan marcadas.Se espera mejorar el modelo,incluyendo otros factores que no se tuvieron en cuenta,asi como variables,con el objetivo de mejorarlo y evaluar mejores resultados.

