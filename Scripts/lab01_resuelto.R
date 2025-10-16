#!/usr/bin/env Rscript
# ------------------------------------------------------------
# Laboratorio 1 — Empezar con R y RStudio (Versión final)
# Parte I: objetos, operaciones, vectores y gráficos
# Parte II: clasificación de variables (respuesta conceptual)
# ------------------------------------------------------------

# ========== PARTE I: R y RStudio ==========

# a) Asignaciones y total mensual
celular     <- 300
transporte  <- 240
comestibles <- 1527
gimnasio    <- 400
alquiler    <- 1500
otros       <- 1833

# Total mensual
total <- celular + transporte + comestibles + gimnasio + alquiler + otros
print(total)  # 5800

# b) Gasto en un semestre (5 meses)
total_semestre <- total * 5
print(total_semestre)  # 29000

# c) Gasto en un año escolar (10 meses)
total_anual <- total * 10
print(total_anual)  # 58000

# d) Vector y gráficos de barras
gastos <- c(celular, transporte, comestibles, gimnasio, alquiler, otros)
nombres <- c("Celular","Transporte","Comestibles","Gimnasio","Alquiler","Otros")

# Gráfico de barras simple
barplot(gastos, names.arg = nombres, main = "Gastos mensuales", ylab = "Monto ($)")

# Orden decreciente y gráfico
gastos_orden <- sort(gastos, decreasing = TRUE)
barplot(gastos_orden, main = "Gastos (orden decreciente)", ylab = "Monto ($)")

# ========== PARTE II: Variables (conceptual) ==========
# Problema 1: Clasificaciones
# 1 Nombre: cualitativa nominal
# 2 Fecha de nacimiento: cuantitativa (fecha/tiempo)
# 3 Edad: cuantitativa discreta
# 4 Dirección: cualitativa nominal
# 5 Teléfono: cualitativa nominal (identificador)
# 6 Área de estudio: cualitativa nominal
# 7 Grado (primero, segundo, ...): cualitativa ordinal
# 8 Puntaje (0–100): cuantitativa continua
# 9 Calificación (A–F): cualitativa ordinal
# 10 Tiempo (min): cuantitativa continua
# 11 Número de hermanos: cuantitativa discreta

# Problema 2: Ejemplo "Países"
# Cuantitativas: población, PIB, superficie, densidad, esperanza_vida, #aeropuertos, Tmedia
# Cualitativas: continente, idioma, sistema_gobierno, bandera, religión, etnia_principal, ONU

# Problema 3: 1=Twitter,2=Email,3=SMS,4=Facebook,5=Blog => cualitativa nominal (números como códigos)

# Problema 4: ver síntesis en el informe; histogramas aplican a variables cuantitativas.
# ------------------------------------------------------------
