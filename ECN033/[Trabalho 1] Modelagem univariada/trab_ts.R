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

### 1. 
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


# Tomando LOG
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
# SARIMA(1,1,2)(2,1,1)
# SARIMA(0,1,2)(2,1,1)
# SARIMA(2,1,1)(2,1,1)
# SARIMA(2,1,0)(2,1,1)
# SARIMA(2,1,2)(2,1,0)
# SARIMA(1,1,2)(2,1,0)
# SARIMA(0,1,2)(2,1,0)
# SARIMA(2,1,1)(2,1,0)
# SARIMA(2,1,0)(2,1,0)


# Criterios de informacao
mod0 = Arima(ddcred, order=c(2,1,2), seasonal=list(order=c(2,1,1), period=12)); mod0 
mod1 = Arima(ddcred, order=c(1,1,2), seasonal=list(order=c(2,1,1), period=12)); mod1 
mod2 = Arima(ddcred, order=c(0,1,2), seasonal=list(order=c(2,1,1), period=12)); mod2  
mod3 = Arima(ddcred, order=c(2,1,1), seasonal=list(order=c(2,1,1), period=12)); mod3 
mod4 = Arima(ddcred, order=c(2,1,0), seasonal=list(order=c(2,1,1), period=12)); mod4 
mod5 = Arima(ddcred, order=c(2,1,2), seasonal=list(order=c(2,1,0), period=12)); mod5 
mod6 = Arima(ddcred, order=c(1,1,2), seasonal=list(order=c(2,1,0), period=12)); mod6 
mod7 = Arima(ddcred, order=c(0,1,2), seasonal=list(order=c(2,1,0), period=12)); mod7
mod8 = Arima(ddcred, order=c(2,1,1), seasonal=list(order=c(2,1,0), period=12)); mod8
mod9 = Arima(ddcred, order=c(2,1,0), seasonal=list(order=c(2,1,0), period=12)); mod9
mod10 = auto.arima(ddcred); mod10


AIC(mod0, mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10) 
BIC(mod0, mod1, mod2, mod3, mod4, mod5, mod6, mod7, mod8, mod9, mod10) 

# Vamos avaliar os modelos 3 e 10, pois foram os que apresentaram menor AIC e BIC.


## PARTE 3 #################################################################################
#  VERIFICAO DO MODELO AJUSTADO                                                         #                                   
###########################################################################################
# Analisamos os residuos

# Testes dos residuos
## Estabilidade
# Plots
# MOD 3
autoplot(mod3, main="Modelo 3")

# MOD 10
autoplot(mod10, main="Modelo 10")

par(mfrow = c(1,1))

## Autocorrelacao                   Teste de Ljung-Box| H0: os residuos sao iid
# MOD 3
tsdiag(mod3)
res3<-residuals(mod3)
Box.test(res3,lag=12,type="Ljung-Box")
Box.test(res3,lag=24,type="Ljung-Box")
Box.test(res3,lag=36,type="Ljung-Box")

# MOD 10
tsdiag(mod10)
res10<-residuals(mod10)
Box.test(res10,lag=12,type="Ljung-Box")
Box.test(res10,lag=24,type="Ljung-Box")
Box.test(res10,lag=36,type="Ljung-Box")



## Normalidade                      Teste de Jarque-Bera| H0: normalidade dos residuos
# MOD 3
par(mfrow=c(3,2))
hist(res3, freq=F, ylab='Densidade', xlab='Residuos', main='Residuos - Modelo 3')
plot(density(res3, kernel = c("gaussian")), main="Residuos - Modelo 3")   # Funcao de densidade estimada
qqnorm(res3, ylab='Quantis amostrais', xlab='Quantis teoricos', main='Quantil-Quantil - Modelo 3')
qqline(res3, col = "red")
shapiro.test(res3)
jarque.bera.test(res3)

# MOD 10
hist(res10, freq=F, ylab='Densidade', xlab='Residuos', main='Residuos - Modelo 10')
plot(density(res10, kernel = c("gaussian")), main="Residuos - Modelo 10")   # Funcao de densidade estimada
qqnorm(res10, ylab='Quantis amostrais', xlab='Quantis teoricos', main='Quantil-Quantil - Modelo 10')
qqline(res10, col = "red")
shapiro.test(res10)
jarque.bera.test(res10)



## Teste de heteroscedasticidade     Teste ARCH| H0: os residuos nao possuem efeitos auto-regressivos de heteroscedasticidade condicional
if(!require(FinTS)){install.packages("FinTS")&require(FinTS);require(FinTS)}

ArchTest(mod3$residuals,lags = 12)

ArchTest(mod10$residuals,lags = 12)

## PARTE 4 #################################################################################
#  PREVISAO                                                                               #                                   
###########################################################################################

## Testes de Acuracia

# Teste de acuracia: dentro da amostra (amostra inteira como treino)
# OBS: Escolhemos o modelo que tem os menores desvios (RMSE)
accuracy(mod3)
accuracy(mod10) 
#-------------------------------------------------------------
## Testes de acuracia: fora da amostra (usando jan/2013 - nov/2015 como teste)

# Definido as series de treino e teste
m_previsao = 35
ddcred.test = tail(ddcred, m_previsao)
ddcred.train = head(ddcred, length(ddcred) - length(ddcred.test))

# Modelo 3
mod3.train = Arima(ddcred.train, order=c(2,1,1), seasonal=list(order=c(2,1,1), period=12)); mod3.train

fc_mod3.train = forecast(mod3.train, h=length(ddcred.test))
fc_mod3.train
par(mfrow=c(1,1))
plot(fc_mod3.train)

accuracy(fc_mod3.train$mean,ddcred.test)

ddcred_test3 <- ts(data.frame(cbind(fcst=fc_mod3.train$mean,obs=ddcred.test)))
plot3 <- autoplot(ddcred_test3[,2], series = "Observado") + 
  autolayer(ddcred_test3[,1], series = "Previsao") +
  labs(title = "Modelo 3",
       x = "Periodos",
       y = "",
       color = "Previsao")
plot3

# Modelo 10
mod10.train = Arima(ddcred.train, order=c(2,0,0), seasonal=list(order=c(2,0,0), period=12)); mod10.train

fc_mod10.train = forecast(mod10.train, h=length(ddcred.test))
fc_mod10.train
par(mfrow=c(1,1))
plot(fc_mod10.train)

accuracy(fc_mod10.train$mean,ddcred.test)

df_test10 <- ts(data.frame(cbind(fcst=fc_mod10.train$mean,obs=ddcred.test)))
plot10 <- autoplot(df_test10[,2], series = "Observado") + 
  autolayer(df_test10[,1], series = "Previsao") +
  labs(title = "Modelo 10",
       x = "Periodos",
       y = "",
       color = "Previsao")
plot10



# Comparativo dos testes de acuracia entre os 2 modelos fora da amostra
accuracy(fc_mod3.train$mean,ddcred.test) # Novamente apresenta menor RMSE
accuracy(fc_mod10.train$mean,ddcred.test)

## Graficos de previsao do melhor modelo
fc_mod3 = forecast(mod3, h = 12)

plot(fc_mod3, 
     main = "Previsao da concessão de crédito",
     xlab = "Periodo",
     include = m_previsao, 
     showgap = F, 
     fcol = "red",
     flty = "dashed")
