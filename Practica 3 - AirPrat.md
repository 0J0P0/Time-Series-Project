# AirPrat

## 1.

**Carregueu el fitxer que conte la serie. Definiu les dades llegides com a objecte de tipus ts (time series) indicant l'origen i la frequencia de la serie.**

```{r}
ser <- ts(read.table("../Data/airbcn.dat")/1000,start=1990,freq=12)
```

## 2.

**Feu la representacio grafica de la serie temporal. Descriviu els aspectes mes rellevants que s'observen a simple vista.**

```{r}
plot(ser,main="Miles de pasajeros de lineas aereas internacionales (BCN-Prat)")
abline(v=1990:2020, lty=3, col=4)
plot(decompose(ser))
```

A partir de la serie temporal se puede describir una tendencia creciente, donde claramente se ve un aumento de la varaibilidad de las observaciones con el paso del tiempo. Además se observa claramente un patrón estacional que corresponde con un aumento en el número de pasajeros en los meses de verano y un descenso en temporada de invierno.

Igualmente se puede apreciar un ligero descenso en el año 2008, muy posiblemente por la crisis financiera que sufrió en España (2008-2014). Aunque rapidamente se ve el crecimiento de la tendencia en los años posteriores.

## 3.

**Apliqueu les transformacions adients per convertir la serie en estacionaria.**

### 3.1 Variancia constante

A modo de comprobar una variabilidad en la serie se realizan gráfico de las medias y variancias de grupos de observaciones así como también un gráfico de los boxplot por periodos.

```{r}
# Calculate the mean and variance of consecutive groups of 8-12 observations.
m <- apply(matrix(ser, nrow=12), 2, mean)
v <- apply(matrix(ser, nrow=12), 2, var)
# Plot the variance against the mean of each group.
plot(v~m, ylab="Variance", xlab="Mean")
```

Hay un claro aumento de la variancia de la serie entre los valores inciales y finales de la serie.

```{r}
# Measurement of dispersion, ignoring outilers.
boxplot(ser~floor(time(ser)))
```

Con el boxplot por grupos se ve aún con más claridad, puesto que la altura de las cajas del IQR aumenta a medida que el tiempo. De lo cual se concluye que se requiere de una trasnformación obtener una variancia constante.

```{r}
lnser <- log(ser)
plot(lnser,main="Log de pasajeros de lineas aereas internacionales (BCN-Prat)")
abline(v=1990:2020, lty=3, col=4)
```

Tras este cambio de escala después de la transformación logaritmica se observan en ambos gráficos ya una misma variancia para los grupos de observaciones.

```{r}
# Measurement of dispersion, ignoring outilers.
boxplot(lnser~floor(time(lnser)))
```

Se puede observar que los IQR de los boxplot de los grupos tienen aproximadamente la misma altura.

### 3.2 Patrón estacional

Para buscar un posible patrón estacional de orden 12, se grafican las subseries de la serie original por meses para observar la distribución de cada una de las medias.

```{r}
monthplot(lnser)
```

```{r}
# Each line is a year
ts.plot(matrix(lnser, nrow=12), col=1:8)
```

Vease que hay un incremento de la variancia en los dos trimestres intermedios del año, con una media máxima en el mes de agosto, seguramente debido a la alta afluencia de pasajeros con motivo de las vacaciones de verano.

Se requiere de una diferenciación estacional para eliminar el patrón de la serie.

```{r}
d12lnser = diff(lnser, 12)
plot(d12lnser,main="Serie después de la diferenciación estacional")
abline(v=1990:2020, lty=3, col=4)
```

Después de aplicar este filtro lineal las medias de cada subserie se alinean horizontalmente.

```{r}
monthplot(d12lnser)
```

### 3.3 Media constante

Se aplica una diferenciación regular hasta que la media de la serie se pueda considerar constante.

```{r}
d1d12lnser = diff(d12lnser)
plot(d1d12lnser,main="Serie depués de la diferenciación regular")
abline(v=1990:2020, lty=3, col=4)
abline(h=mean(d1d12lnser), col=2) # mean of the series.
```

Tras esta diferenciación la media de la serie parece bastante constante ya, se ha de mirar si no se ha incrementado la variancia.

```{r}
var(d12lnser)
var(d1d12lnser)
```

Repetir hasta que se detecte un incremento de la variancia de la serie.

```{r}
d1d1d12lnser = diff(d1d12lnser)
plot(d1d1d12lnser,main="Serie depués de la diferenciación regular")
abline(v=1990:2020, lty=3, col=4)
abline(h=mean(d1d1d12lnser), col=2) # mean of the series.
```

Ya se ha detectado una diferenciación regular inecesaria.

```{r}
var(d1d12lnser)
var(d1d1d12lnser)
```

Ha habido un incremento en la variancia de la serie, por lo tanto la última diferenciación regular ha sido inecesaria y solo se ha de aplicar la primera.

### 3.4 Serie estacionaria resultante

```{r}
plot(d1d12lnser,main="Serie Estacionaria")
abline(v=1990:2020, lty=3, col=4)
abline(h=mean(d1d12lnser), col=2) # mean of the series.
```

Tras aplicar las transformaciones, la serie estacionaria $W_t$ queda de la siguiente forma:

$$W_t = (1-B)(1-B^{12})\log{X_t}$$ Con $d = 1$ y $D = 1$.

## 4.

**Per a la serie transformada, representeu l'ACF i la PACF.**

### 4.1 ACF y PACF muestral

```{r}
# justificar el lag
par(mfrow=c(1, 2)) # plot two graphics
acf(d1d12lnser, ylim=c(-1, 1), lag.max=60, lwd=2, col=c(2, rep(1, 11)))
pacf(d1d12lnser, ylim=c(-1, 1), lag.max=60, lwd=2, col=c(rep(1, 11), 2))
```

Se empieza considerando que los retrasos finitos, que marcan un patrón de decrecimiento, se encruentran en el gráfico del **PACF** muestral, tanto para el caso regular como el estacional.

Para determinar los valores de `p, P` y `q, Q` se necesita contar el número de retrasos hasta la última barra significativamente diferente de 0. $\rho(h) \neq 0$. Con lo que la última barra debe encontrarse fuera de las bandas del intervalo de confianza para poder rechazar la hipotesis nula de significación de $\rho(h)$.

$$H_0: \rho(h) = 0 \\ H_1: \rho(h) \neq 0$$

Teniendo esto en cuenta, para el caso estacional se podría proponer tanto un modelo *AR(2)* como *AR(4)*, pero en este segundo caso se considera que el cuarto retraso estacional sobre sale por muy poco de las bandas de confianza, considerando el retraso como no significativo dentro del 5% de caso como fruto del azar hacia el modelo. Tampoco sería un candidato deseable puesto que conlleva una mayor complejidad al modelo, lo cual inestabiliza las predicciones en un futuro. No se ha considerado modelo del tipo *MA(Q)* debido a los infinitos retardos no nulos del gráfico **ACF**.

Para el caso regular, también se considera un modelo *AR(2)*, ya que los siguientes retrasos significativos pueden ser satélites del componente estacional, además que dentro de los cinco o seis primero no se detecta ningún otro retardo significativo al estar dentro de las bandas de confianza. También se puede considerar un modelo *MA(1)* si únicamente se tienen en cuenta alrededor de los primeros cinco retrasos, ya que el resto podrían estar sujetos una vez más al componente estacional.

Con lo que se obtienen los siguientes valores para el modelo ARIMA: $P = 2, \ Q = 0, \ p = 2, \ q = 1$.

## 5.

**En base a l'ACF i PACF mostral, proposeu almenys dos models per a cada serie, justificant la proposta.**

Por lo tanto, los dos modelos propuestos son:

$$\mbox{ARIMA}(2,1,0)(2,1,0)_{12} \\ \mbox{ARIMA}(0,1,1)(2,1,0)_{12}$$

### 5.1 Modelo 1 propuesto

Considerese el modelo $\mbox{ARIMA}(2,1,0)(2,1,0)_{12}$ como el primer modelo propuesto.

$$(1-\phi_1 B - \phi_2 B^2)(1 - \Phi_1 B^{12} - \Phi_2 B^{24})(1-B^{12})(1-B)\log{(X_t - \mu_1)} = Z_t$$

### 5.2 Modelo 2 propuesto

Considerese el modelo $\mbox{ARIMA}(0,1,1)(2,1,0)_{12}$ como el segundo modelo propuesto.

$$(1 - \Phi_1 B^{12} - \Phi_2 B^{24})(1-B^{12})(1-B)\log{(X_t - \mu_2)} = (1 + \theta_1 B)Z_t$$

## 6.

**Feu l'estimacio dels models proposats i verifiqueu la significacio dels coeficients i que els residus tenen un ACF compatible amb un soroll blanc. Si hi ha algun coeficient no significatiu, elimineu-lo del model.**

### 6.1 Estimación modelo 1

Como se ha obtenido ya una serie $W_t$ estacionaria, tiene un solo valor de para la media de la serie $\hat{\mu_1}$, con lo que se procede a estimarla.

```{r}
mod1 <- arima(d1d12lnser, order=c(2, 0, 0), 
              seasonal=list(order=c(2,0,0), period=12))
mod1
```

Se comprueba la significación de $\hat{\mu_1}$ mediante su $t-$ratio para determinar si es significante para el modelo, es decir, si se acepta la hipotesis nula de que la media es igual a 0.

$$H_0: \hat{\mu_1} = 0 \\ H_1: \hat{\mu_1} \neq 0$$ donde el $t-$ratio viene dado por

$$t = \frac{\hat{\mu_1} - 0}{S_{W_t}} \sim N(0,1)$$

asintoticamente normal al tratarse de un estimador calculado a partir del estimador de máxima verosimilitud.

```{r}
cat("Is the mean significant?", abs(mod1$coef[5]/sqrt(diag(mod1$var.coef)[5])) > 2)
```

Al no tener significación la media de la serie $\hat{\mu_1}$, se puede aplicar la función `ARIMA` al logaritmo de la serie orignal aplicandole las diferenciaciones como parámetros a la función, de esta manera se evitan deshacer las transformaciones después y se puede predecir directamente sobre el logaritmo de la serie.

```{r}
mod1 <- arima(lnser, order=c(2,1,0), seasonal=list(order=c(2,1,0), period=12))
mod1
```

Verificamos que la decisión de retirar la media del modelo ha sido la adecuada en terminos del AIC del nuevo ajuste. Además como $\hat{\mu_1}$ no es significativa, se ve que los valores de los estimadores del modelo no han variado en gran escala, al igual que sus desviaciones estandar.

#### 6.1.1 Signifiación de los parámetros modelo 1

```{r}
cat("\nT-ratios:",round(mod1$coef/sqrt(diag(mod1$var.coef)),2))
```

Se observa que el parámetro $\hat{\phi_2}$ está al borde del intervalo de confianza, por lo que ajustaremos un modelo eliminandolo y se compararán ambos modelos en terminos de su AIC.

```{r}
mod1b <- arima(lnser, order=c(2,1,0), seasonal=list(order=c(2,1,0),
                period=12), fixed=c(NA, 0, NA, NA))
mod1b
```

En vista de que el AIC ha aumentado tras eliminar $\hat{\phi_2}$, se considerara que el modelo incial es más adecuado.

El resto de parámetros si son significativos, como se ha visto anteriormente.

#### 6.1.2 ACF del residuo modelo 1

```{r}
par(mfrow=c(1, 2)) # plot two graphics
acf(resid(mod1), ylim=c(-1, 1), lag.max=60, lwd=2, col=c(2, rep(1, 11)))
pacf(resid(mod1), ylim=c(-1, 1), lag.max=60, lwd=2, col=c(rep(1, 11), 2))
```

Se observa que los retrasos que llegan a sobresalir de los intervalos de confianza, representan aproximadamente el 5% por lo que se puede afirmar que es un ACF compatible con una ACF de *White Noise*.

De igual manera se observa que no aparecen retardos cerca del origen, lo que indica que no hacen falta más parámetros al modelo. Si se desea tener un **PACF** de los residuos aun más acorde al *White Noise*, si que se podrían probar modelos con más parámetros, como por ejemplo un *AR(3)*, pero se ha decidido optar por la simplicidad del modelo.

Los retrasos estacionales no son significativos, por lo que el modelo explica adecuadamente el componente estacional.

### 6.2 Estimación modelo 2

Nuevamente se comprubeba la significación de la media $\hat{\mu_2}$, esta vez para el segundo modelo propuesto.

```{r}
mod2 <- arima(d1d12lnser, order=c(0,0,1), seasonal=list(order=c(2,0,0), period=12))
mod2
```

Se comprueba la significación de $\hat{\mu_2}$ mediante su $t-$ratio para determinar si es significante para el modelo, es decir, si se acepta la hipotesis nula de que la media es igual a 0.

$$H_0: \hat{\mu_2} = 0 \\ H_1: \hat{\mu_2} \neq 0$$

donde el $t-$ratio viene dado por

$$t = \frac{\hat{\mu_1} - 0}{S_{W_t}} \sim N(0,1)$$

asintoticamente normal al tratarse de un estimador calculado a partir del estimador de máxima verosimilitud.

```{r}
cat("Is the mean significant?", abs(mod2$coef[4]/sqrt(diag(mod2$var.coef)[4])) > 2)
```

Al no tener significación la media de la serie $\hat{\mu_2}$, se puede aplicar la función `ARIMA` al logaritmo de la serie orignal aplicandole las diferenciaciones como parámetros a la función, de esta manera se evitan deshacer las transformaciones después y se puede predecir directamente sobre el logaritmo de la serie.

```{r}
mod2 <- arima(lnser, order=c(0,1,1), seasonal=list(order=c(2,1,0), period=12))
mod2
```

Verificamos que la decisión de retirar la media del modelo ha sido la adecuada en terminos del AIC del nuevo ajuste. Además como $\hat{\mu_1}$ no es significativa, se ve que los valores de los estimadores del modelo no han variado en gran escala, al igual que sus desviaciones estandar.

#### 6.2.1 Signifiación de los parámetros modelo 2

```{r}
cat("\nT-ratios:",round(mod2$coef/sqrt(diag(mod2$var.coef)),2))
```

Para el segundo modelo propuesto, tanto los parámetros de la parte estacional como regular son significativos al tener un $t$-ratio fuera del intervalo $[2,-2]$ para rechazar la hipótesis nula.

#### 6.2.2 ACF del residuo modelo 2

```{r}
par(mfrow=c(1, 2)) # plot two graphics
acf(resid(mod2), ylim=c(-1, 1), lag.max=60, lwd=2, col=c(2, rep(1, 11)))
pacf(resid(mod2), ylim=c(-1, 1), lag.max=60, lwd=2, col=c(rep(1, 11), 2))
```

El segundo modelo presenta una independencia entre los retrasos casi idéntica al primer modelo propuesto. Por lo tanto es lógico afirmar que presenta un ACF similar al ACF de un *White Noise*.

De igual manera se observa que no aparecen retardos cerca del origen, lo que indica que no hacen falta más parámetros al modelo. Una vez más, si se desea tener un **ACF** de los residuos aun más acorde al *White Noise*, si que se podrían probar modelos con más parámetros, como por ejemplo un *MA(2)*, pero como ya se ha mencionado, se ha decidido optar por la simplicidad del modelo.

Se encuentran componentes estacionales ligeramente por encima de las bandas de confianza, pero al estar lejos del origen, dichos retardos pueden ser obviados.

# 7.

**Indiqueu quin model proposarıeu, fent servir el criteri de l'AIC.**

```{r}
mod1$aic
mod2$aic
```

En base al criterio del AIC, se escogería el segundo modelo $\mbox{ARIMA}(0,1,1)(2,1,0)_{12}$ propuesto anteriormente:

$$(1 - \Phi_1 B^{12} - \Phi_2 B^{24})(1-B^{12})(1-B)\log{X_t} = (1 + \theta_1 B)Z_t$$

$$(1 +0.6358 B^{12} +0.4189 B^{24})(1-B^{12})(1-B)\log{X_t} = (1 -0.3676 B)Z_t$$ con $Z_t \sim N(0, 0.00228)$.

```{r}
mod2
```
