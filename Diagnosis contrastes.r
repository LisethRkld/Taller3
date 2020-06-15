# ------------------------------------------------------------------
# Repetici�n de contrastes
# ------------------------------------------------------------------

# ------------------------------------------------------------------
# Valores iniciales
set.seed(1) # Fijar semilla para reproductibilidad
n <- 50
nsim <- 100
estadistico <- numeric(nsim)
pvalor <- numeric(nsim)


# ------------------------------------------------------------------
# Realizar contrastes
for(isim in 1:nsim) {
  u <- runif(n)
  tmp <- ks.test(u, "punif",  min=0, max=1)
  estadistico[isim] <- tmp$statistic
  pvalor[isim] <- tmp$p.value
}
#Probar a cambiar por u <- rnorm(n) y tmp <- ks.test(u, "pnorm",  mean = mean(u), sd = sd(u))
# ------------------------------------------------------------------
# Proporci�n de rechazos
cat("\nProporci�n de rechazos al 1% =", mean(pvalor < 0.01), "\n")
cat("Proporci�n de rechazos al 5% =", mean(pvalor < 0.05), "\n")
cat("Proporci�n de rechazos al 10% =", mean(pvalor < 0.1), "\n")


# ------------------------------------------------------------------
# An�lisis del estad�stico contraste

# Histograma
hist(estadistico, freq=FALSE)

#...

# ------------------------------------------------------------------
# An�lisis de los p-valores
# (si todo 'correcto' con distribuci�n uniforme)

# Histograma
hist(pvalor, freq=FALSE)
curve(dunif(x,0,1), add=TRUE)   #abline(h=1)

# Test de Kolmogorov-Smirnov
ks.test(pvalor, "punif",  min=0, max=1)

