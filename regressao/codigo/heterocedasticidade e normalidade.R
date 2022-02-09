# https://daviddalpiaz.github.io/appliedstats/
# Applied Statistics with R


library(readxl)
initech <- read_excel("~/GitHub/aulas/regressao/codigo/dados/initech.xlsx")

initech$grau_pagamento <- as.factor(initech$grau_pagamento)   
initech$sexo <- as.factor(initech$sexo)  
initech$raca <- as.factor(initech$raca) 
initech$casado <- as.factor(initech$casado)

# Model diagnostics
library(performance)
# Check for heteroskedasticity
# Linear models assume constant error variance (homoskedasticity).
# The check_heteroscedasticity() functions assess if this assumption has been violated:
modelo <- lm(salario  ~ anos_trabalho+desempenho+sexo+raca, data = initech)
check_normality(modelo)
#> Warning: Non-normality of residuals detected (p < .001).
check_heteroscedasticity(modelo)
#> Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.031).

# checking model assumptions
check_model(modelo)
check_model(modelo, panel = FALSE)

library(lmtest)
bptest(modelo)
shapiro.test(resid(modelo))

qqnorm(resid(modelo), main = "Normal Q-Q Plot", col = "darkgrey")
qqline(resid(modelo), col = "dodgerblue", lwd = 2)

plot(fitted(modelo), resid(modelo), col = "dodgerblue",
     pch = 20, cex = 1.5, xlab = "Fitted", ylab = "Residuals")
abline(h = 0, lty = 2, col = "darkorange", lwd = 2)

#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
# Trasnformação LOG
modelo1 <- lm(log(salario)  ~ anos_trabalho+desempenho+sexo+raca, data = subset(initech,salario>1300))
check_normality(modelo1)
#> Warning: Non-normality of residuals detected (p < .001).
check_heteroscedasticity(modelo1)
#> Warning: Heteroscedasticity (non-constant error variance) detected (p = 0.031).


#-----------------------------------------------------
#-----------------------------------------------------
#-----------------------------------------------------
# Trasnformação BOX COX

library(MASS)
boxcox(modelo, lambda = seq(-5, 5, by = 0.1), plotit = TRUE)
modelo_boxcox = lm((((salario ^ 0.7) - 1) / 0.7) ~ anos_trabalho+desempenho+sexo+raca, data = subset(initech,salario>5000&salario<15000))

check_normality(modelo_boxcox)
check_heteroscedasticity(modelo_boxcox)
check_model(modelo_boxcox, panel = FALSE)

library(lmtest)
bptest(modelo_boxcox)
shapiro.test(resid(modelo_boxcox))
