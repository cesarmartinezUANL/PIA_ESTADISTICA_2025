#!/usr/bin/env Rscript
# ------------------------------------------------------------
# Asignación 3 — Comparación de Petal.Length (versicolor vs virginica)
# Incluye: filtro, descriptivos, F-test, t-test, tamaño de efecto y figura
# ------------------------------------------------------------

# 0) Datos
data(iris)  # dataset base de R

# Subconjunto de especies
data_sub <- subset(iris, Species %in% c("versicolor","virginica"))
data_sub <- droplevels(data_sub)

# 1) Descriptivos mínimos por especie
by(data_sub$Petal.Length, data_sub$Species, summary)
tapply(data_sub$Petal.Length, data_sub$Species, sd)
tapply(data_sub$Petal.Length, data_sub$Species, IQR)

# (Opcional) Tabla compacta con aggregate
res <- aggregate(Petal.Length ~ Species, data = data_sub, FUN = function(x) c(
  n      = length(x),
  mean   = mean(x),
  median = median(x),
  sd     = sd(x),
  min    = min(x),
  q1     = quantile(x, .25),
  q3     = quantile(x, .75),
  max    = max(x),
  IQR    = IQR(x)
))
res <- do.call(data.frame, res)
names(res) <- c("Species","n","mean","median","sd","min","q1","q3","max","IQR")
print(res)

# 2) Homogeneidad de varianzas (F-test simple)
vt <- var.test(Petal.Length ~ Species, data = data_sub)
print(vt)

# 3) t de dos muestras (decisión guiada por F-test)
tt <- t.test(Petal.Length ~ Species, data = data_sub, var.equal = (vt$p.value > 0.05))
print(tt)

# 4) Tamaño de efecto (Cohen's d) con varianzas iguales
t_val <- as.numeric(tt$statistic)
n1 <- sum(data_sub$Species == "versicolor")
n2 <- sum(data_sub$Species == "virginica")
d_cohen <- t_val * sqrt(1/n1 + 1/n2)
cat(sprintf("Cohen's d (signo indica dirección): %.3f\n", d_cohen))

# (Opcional) Hedges' g (corrección pequeña muestra)
J <- 1 - 3/(4*(n1+n2) - 9)
g_hedges <- d_cohen * J
cat(sprintf("Hedges' g: %.3f\n", g_hedges))

# 5) Figura comparativa (boxplot base R)
png("petal_length_boxplot.png", width = 1600, height = 1200, res = 200)
par(mar = c(4.5,4.5,2,1))
boxplot(Petal.Length ~ Species, data = data_sub,
        main = "Longitud de pétalo por especie",
        xlab = "Especie", ylab = "Petal.Length (cm)", outline = FALSE)
stripchart(Petal.Length ~ Species, data = data_sub,
           vertical = TRUE, method = "jitter", pch = 19, cex = .7,
           col = adjustcolor("black", .6), add = TRUE)
mtext("Figura 1. Boxplot con puntos (jitter). Mediana (línea), caja (Q1–Q3), bigotes (1.5×IQR).", side = 1, line = 4, cex = .8)
dev.off()

# Pie de figura recomendado:
# "Figura 1. Boxplot de la longitud del pétalo (cm) en Iris versicolor (n=50) e Iris virginica (n=50);
#  la línea central indica la mediana; las cajas, Q1–Q3; los bigotes, 1.5×IQR."
# ------------------------------------------------------------
