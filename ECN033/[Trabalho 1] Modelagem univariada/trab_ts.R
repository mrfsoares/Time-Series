# https://dadosabertos.bcb.gov.br/dataset/20633-concessoes-de-credito---pessoas-fisicas---total#:~:text=Conceito%3A%20Valor%20das%20novas%20opera%C3%A7%C3%B5es,no%20segmento%20de%20cr%C3%A9dito%20direcionado.

# Escolhendo diretorio
getwd()
setwd(choose.dir())


# ETAPA 1. Análise gráfica -----------------------------------------------------
# ------------------------------------------------------------------------------
if(!require(GetBCBData)){install.packages("GetBCBData")&require(GetBCBData);require(GetBCBData)}
if(!require(tseries)){install.packages("tseries")&require(tseries);require(tseries)}
if(!require(forecast)){install.packages("forecast")&require(forecast);require(forecast)}
if(!require(lmtest)){install.packages("lmtest")&require(lmtest);require(lmtest)}
if(!require(ggplot2)){install.packages("ggplot2")&require(ggplot2);require(ggplot2)}


# Leitura dos dados
## Concessões de crédito - Pessoas físicas - Total
cred <- ts(
  GetBCBData::gbcbd_get_series(
    id = 20633,
    first.date = "2012-01-01",
    last.date = "2019-12-31")[,2], start = c(2012, 01), freq = 12
)

cred

# Plot
main = 'Concessões de crédito mensal para pessoas físicas no Brasil'
plot.ts(cred,
        main=main, 
        ylab='R$ (milhões)', xlab=NULL, sub = 'Fonte: BCB')
grid()


## Tomando LOG
main = 'Log das concessões de crédito mensal para pessoas físicas no Brasil'
cred <- log(cred)
plot.ts(cred,
        main=main, 
        ylab='R$ (milhões)', xlab=NULL, sub = 'Fonte: BCB')
grid()


## Decomposição sazonal
par(mfrow=c(1,1))
plot(decompose(cred))
grid()

# Os dados não são estacionários:::impossível estimar todos os momentos da série trivialmente
# Presença de tendência e sazonalidade --- SARIMA()

# Olhando FAC e FACP da série em log
## FAC e FACP
par(mfrow=c(1,2))
acf(cred,lag.max=36,main='FAC',
    xlab='defasagem',ylab='autocorrelações')
pacf(cred,lag.max=36,main='FACP',
     xlab='defasagem',ylab='autocorrelações')


# HAJA VISTA QUE A SÉRIE É NÃO ESTACIONÁRIO, VAMOS ESTACIONARIZA-LA
par(mfrow=c(1,1))
main = '[Primeira Diferença]\n Log das concessões de crédito mensal para pessoas físicas no Brasil'
plot.ts(diff(cred),
        main=main, 
        ylab=NULL, xlab=NULL, sub = 'Fonte: BCB')
abline(h=mean(diff(cred)), col = "blue") # representação da media no gráfico
grid()

# ao que tudo indica, tomar a 1 diferença resolve problema de não estac.

## FAC e FACP
par(mfrow=c(1,2))
acf(diff(cred), main='FAC: Primeira Diferença')
pacf(diff(cred), main='FACP: Primeira Diferença')
par(mfrow = c(1,1))

## observa-se problema com spikes: sazonalidade. aplicar transformação sazonal


dcred <- diff(cred) # primeira diferença
ddcred <- diff(dcred, lag = 12) # transformação sazonal

par(mfrow=c(1,1))
main = '[Primeira Diferença e Diferenciação Sazonal]\n Log das concessões de crédito mensal para pessoas físicas no Brasil'
plot.ts(ddcred,
        main=main, 
        ylab=NULL, xlab=NULL, sub = 'Fonte: BCB')
abline(h=mean(diff(cred)), col = "blue") # representação da media no gráfico
grid()

## FAC e FACP
par(mfrow=c(1,2))
acf(ddcred, main='FAC: Primeira Diferença e Diferenciação Sazonal')
pacf(ddcred, main='FACP: Primeira Diferença e Diferenciação Sazonal')
par(mfrow = c(1,1))


# Etapa 2. Testes de raiz unitária ---------------------------------------------
# ------------------------------------------------------------------------------

## ____ Teste ADF (H0: não estacionário [possui raíz unitária])

if(!require(urca)){install.packages("urca")&require(urca);require(urca)}

### Em primeira diferença 
summary(ur.df(dcred, type=c("trend"),lags=13,
        selectlags = "BIC"))  # não rejeita H0 . há pelo menos I(1)
summary(ur.df(dcred, type=c("drift"),lags=13,
        selectlags = "BIC"))  # não rejeita H0 . há pelo menos I(1)
summary(ur.df(dcred, type=c("none"),lags=13,
        selectlags = "BIC"))  # não rejeita H0 . há pelo menos I(1)

### Em primeira diferença e diferenciação sazonal
summary(ur.df(ddcred, type=c("trend"),lags=13,
              selectlags = "BIC"))  # rejeita H0 . estacionáraia
summary(ur.df(ddcred, type=c("drift"),lags=13,
              selectlags = "BIC"))   # rejeita H0 . estacionáraia
summary(ur.df(ddcred, type=c("none"),lags=13,
              selectlags = "BIC")) # rejeita H0 . estacionáraia


# Se t < ?? rejeitamos H0
# Conclui-se que a série em primeira diferença e diferenciação sazonal 
# é estacionária!



## ____Teste de PP (H0: não estacionário [possui raíz unitária])
### Em primeira diferença
PP.test(dcred)

### Em primeira diferença e diferenciação sazonal
PP.test(ddcred)

# Se DF < p-value rejeitamos H0
# Conclui-se que a série em primeira diferença e diferenciação sazonal 
# é estacionária!



## ____ Teste KPSS (H0: estacionário [não possui raíz unitária])
### Em primeira diferença 
summary(ur.kpss(dcred, type="tau", lags="short"))

### Em primeira diferença e diferenciação sazonal
summary(ur.kpss(ddcred, type="tau", lags="short"))


# T > critical values: rejeita H0 [não estacionaria]
# Conclui-se que a série em primeira diferença e diferenciação sazonal 
# é estacionária!



# ETAPA 3. Estimação -----------------------------------------------------------
# ------------------------------------------------------------------------------

# Obs.: há componente sazonal. Estimar SARIMA

# MODELOS CANDIDATOS:
# SARIMA(2,1,2)(2,1,1)
# SARIMA(0,1,2)(2,1,1)
# SARIMA(2,1,1)(2,1,1)
# SARIMA(2,1,0)(2,1,1)
# SARIMA(2,1,2)(2,1,0)
# SARIMA(1,1,2)(2,1,0)
# SARIMA(0,1,2)(2,1,0)
# SARIMA(2,1,1)(2,1,0)
# SARIMA(2,1,0)(2,1,0)


# Candidatos a melhor modelo SARIMA 
fit0 = coeftest(Arima(cred, order=c(2,1,2), seasonal=list(order=c(2,1,1), period=12, lambda = 0))); fit0 
fit1 = coeftest(Arima(cred, order=c(0,1,2), seasonal=list(order=c(2,1,1), period=12, lambda = 0))); fit1 
fit2 = coeftest(Arima(cred, order=c(2,1,1), seasonal=list(order=c(2,1,1), period=12, lambda = 0))); fit2  
fit3 = coeftest(Arima(cred, order=c(2,1,0), seasonal=list(order=c(2,1,1), period=12, lambda = 0))); fit3 
fit4 = coeftest(Arima(cred, order=c(2,1,2), seasonal=list(order=c(2,1,0), period=12, lambda = 0))); fit4 
fit5 = coeftest(Arima(cred, order=c(1,1,2), seasonal=list(order=c(2,1,0), period=12, lambda = 0))); fit5 
fit6 = coeftest(Arima(cred, order=c(0,1,2), seasonal=list(order=c(2,1,0), period=12, lambda = 0))); fit6 
fit7 = coeftest(Arima(cred, order=c(2,1,1), seasonal=list(order=c(2,1,0), period=12, lambda = 0))); fit7
fit8 = coeftest(Arima(cred, order=c(2,1,0), seasonal=list(order=c(4,1,0), period=12, lambda = 0))); fit8
fit9 = coeftest(auto.arima(cred)); fit9


# Criterios de informacao
mod0 = Arima(cred, order=c(2,1,2), seasonal=list(order=c(2,1,1), period=12, lambda = 0)); mod0 
mod1 = Arima(cred, order=c(0,1,2), seasonal=list(order=c(2,1,1), period=12, lambda = 0)); mod1 
mod2 = Arima(cred, order=c(2,1,1), seasonal=list(order=c(2,1,1), period=12, lambda = 0)); mod2  
mod3 = Arima(cred, order=c(2,1,0), seasonal=list(order=c(2,1,1), period=12, lambda = 0)); mod3 
mod4 = Arima(cred, order=c(2,1,2), seasonal=list(order=c(2,1,0), period=12, lambda = 0)); mod4 
mod5 = Arima(cred, order=c(1,1,2), seasonal=list(order=c(2,1,0), period=12, lambda = 0)); mod5 
mod6 = Arima(cred, order=c(0,1,2), seasonal=list(order=c(2,1,0), period=12, lambda = 0)); mod6 
mod7 = Arima(cred, order=c(2,1,1), seasonal=list(order=c(2,1,0), period=12, lambda = 0)); mod7
mod8 = Arima(cred, order=c(2,1,0), seasonal=list(order=c(4,1,0), period=12, lambda = 0)); mod8
mod9 = auto.arima(cred); mod9


AIC(mod0, mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9) 
BIC(mod0, mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9) 

# Se avaliará o 8, pois foram os que apresentou menor AIC e BIC.



# ETAPA 4. Verificação do modelo ajustado --------------------------------------
# ------------------------------------------------------------------------------

# Analisando os residuos

# Testes dos residuos
## Estabilidade
# Plots
# MOD 8
par(mfrow = c(1,1))
autoplot(mod8, main="Modelo 8")


## Autocorrelacao                   Teste de Ljung-Box| H0: os residuos sao iid
# MOD 8
tsdiag(mod8)
res8 <- residuals(mod8)
Box.test(res8,lag=12,type="Ljung-Box")
Box.test(res8,lag=24,type="Ljung-Box")
Box.test(res8,lag=36,type="Ljung-Box")


## Normalidade                      Teste de Jarque-Bera| H0: normalidade dos residuos
# MOD 8
par(mfrow=c(1,3))
hist(res8, freq=F, ylab='Densidade', xlab='Residuos', main='Residuos - Modelo 8')
plot(density(res8, kernel = c("gaussian")), main="Residuos - Modelo 8")   # Funcao de densidade estimada
qqnorm(res8, ylab='Quantis amostrais', xlab='Quantis teoricos', main='Quantil-Quantil - Modelo 8')
qqline(res8, col = "red")
shapiro.test(res8)
jarque.bera.test(res8)


## Teste de heteroscedasticidade     Teste ARCH| H0: os residuos nao possuem efeitos auto-regressivos de heteroscedasticidade condicional
if(!require(FinTS)){install.packages("FinTS")&require(FinTS);require(FinTS)}

ArchTest(mod8$residuals,lags = 12)



# ETAPA 4. Previsão ------------------------------------------------------------
# ------------------------------------------------------------------------------

main__ <- 'Previsão das concessões de crédito mensal para pessoas físicas no Brasil'
main_ <- '\npara o ano de 2020 a partir de um ARIMA(2,1,0)(4,1,0)[12]'
main = paste(main__, main_)
plot(forecast(object = mod8, h=12, level = 0.95),
     main=main, ylab='R$ (milhões)')
grid()

# graficamente parece ter funcionado bem


# Analise de métricas de previsão
accuracy(mod8)

