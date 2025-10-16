#!/usr/bin/env Rscript
# ------------------------------------------------------------
# ANOVA — Concentración de estroncio (mg/ml) en 5 cuerpos de agua (n=6 por sitio)
# Tareas: ANOVA de una vía, LSD (α=0.05), Tukey HSD, gráfica y reporte de pares significativos
# ------------------------------------------------------------

# 1) Datos (del cuadro de la asignación)
sitio  <- factor(rep(c("Grayson","Beaver","Angler","Appletree","Rock"), each = 6),
                 levels = c("Grayson","Beaver","Angler","Appletree","Rock"))
estroncio <- c(
  28.2,33.2,36.4,34.6,29.1,31.0,   # Grayson
  39.6,40.8,37.9,37.1,43.6,42.4,   # Beaver
  46.3,42.1,43.5,48.8,43.7,40.1,   # Angler
  41.0,44.1,46.4,40.2,38.6,36.3,   # Appletree
  56.3,54.1,59.4,62.7,60.0,57.3    # Rock
)
dat <- data.frame(Site = sitio, Sr = estroncio)

# 2) ANOVA de una vía
fit <- aov(Sr ~ Site, data = dat)
cat("\n=== Tabla de ANOVA ===\n")
print(summary(fit))

# 3) Medias por grupo y MS_error (para LSD/Tukey manual)
means <- tapply(dat$Sr, dat$Site, mean)
n <- 6
k <- length(means)
N <- n * k
ms_error <- summary(fit)[[1]]["Residuals","Mean Sq"]
df_error <- summary(fit)[[1]]["Residuals","Df"]

cat("\nMedias por sitio:\n"); print(round(means, 3))
cat(sprintf("\nMS_error = %.4f, gl_error = %d\n", ms_error, df_error))

# 4) Prueba LSD (α = 0.05, dos colas)
alpha <- 0.05
t_crit <- qt(1 - alpha/2, df = df_error)
LSD <- t_crit * sqrt(2 * ms_error / n)
cat(sprintf("\n=== Prueba LSD (α=%.2f) ===\n", alpha))
cat(sprintf("t_crit = %.3f, LSD = %.4f\n", t_crit, LSD))

# Comparaciones por pares (LSD)
pairs <- combn(names(means), 2, simplify = FALSE)
cat("\nPares significativos por LSD:\n")
for (p in pairs) {
  diff <- abs(means[p[1]] - means[p[2]])
  sig <- ifelse(diff > LSD, "SI", "no")
  cat(sprintf("%-10s vs %-10s | |Δ|= %.3f  -> %s\n", p[1], p[2], diff, sig))
}

# 5) Prueba de Tukey HSD (usando función base y también umbral manual)
cat("\n=== Tukey HSD (α=0.05) ===\n")
print(TukeyHSD(fit))

# Umbral HSD manual: q_crit * sqrt(MS_error/n)
# (El valor crítico q_crit puede obtenerse de tables; para k=5, gl_error=25, q_crit ≈ 4.11)
q_crit <- 4.11
HSD <- q_crit * sqrt(ms_error / n)
cat(sprintf("\nq_crit ≈ %.2f (k=%d, gl=%d), HSD = %.4f\n", q_crit, k, df_error, HSD))

cat("\nPares que superan HSD (manual):\n")
for (p in pairs) {
  diff <- abs(means[p[1]] - means[p[2]])
  sig <- ifelse(diff > HSD, "SI", "no")
  cat(sprintf("%-10s vs %-10s | |Δ|= %.3f  -> %s\n", p[1], p[2], diff, sig))
}

# 6) Gráfica (boxplot con puntos)
png("anova_estroncio_boxplot.png", width = 1600, height = 1200, res = 200)
par(mar = c(5,5,3,2))
boxplot(Sr ~ Site, data = dat, main = "Concentración de estroncio por sitio",
        xlab = "Sitio", ylab = "Estroncio (mg/ml)", outline = FALSE)
stripchart(Sr ~ Site, data = dat, vertical = TRUE, method = "jitter",
           pch = 19, cex = .7, col = adjustcolor("black", .6), add = TRUE)
mtext("Figura. Boxplot con puntos (jitter): mediana (línea), caja (Q1–Q3), bigotes (1.5×IQR).", side = 1, line = 4, cex = .8)
dev.off()

# ------------------------------------------------------------
