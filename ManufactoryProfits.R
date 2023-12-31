# Data and Package Import

pacman::p_load("Mcomp", "forecast", "ggplot2", "tseries", "knitr", "gridExtra",
               "forecTheta")

data(M3)
id <- 2726
M3[[id]]
h <- M3[[id]]$h
treino <- M3[[id]]$x
teste <- M3[[id]]$xx


# Time Series Plot

autoplot(treino) +
  labs(x = "Ano", y = "Valores (US$)", title = "Lucro médio do setor de manufatura") +
  theme_bw()


# MSTL decomposition

decomp.mstl <- mstl(treino)
autoplot(decomp.mstl) +
  labs(x = "Ano", title = "Decomposição via MSTL") +
  theme_bw()


# Stationarity

kpss.test(treino)
ndiffs(treino)

x <- data.frame("KPSS" = c("<0.01", "1"),
                row.names = c("p-valor", "ndiffs"))

kable(x)


# First diff

treino_d <- diff(treino)

x <- autoplot(treino) +
  labs(x = "Ano", y = "Valores", title = "Série Original") +
  theme_bw()

y <- autoplot(treino_d) +
  labs(x = "Ano", y = "Valores", title = "Primeira Diferença") +
  theme_bw()

grid.arrange(x, y, newpage = F, nrow = 2)

kpss.test(treino_d)

x <- data.frame("KPSS" = c(">0.1", "0"), 
                row.names = c("p-valor", "ndiffs"))

kable(x)

# Seasonal Diff

nsdiffs(treino_d)

x <- data.frame("nsdiffs" = c(1))

kable(x)

treino_d2 <- diff(treino_d, lag = 12)

x <- autoplot(treino_d) +
  labs(x = "Ano", y = "Valores", title = "Primeira Diferença") +
  theme_bw()

y <- autoplot(treino_d2) +
  labs(x = "Ano", y = "Valores", title = "Diferença Sazonal") +
  theme_bw()

grid.arrange(x, y, newpage = F, nrow = 2)


# ACF and PACF

par(mfrow = c(1, 2))
acf(treino_d2, lag.max = 5*12, main = "")
pacf(treino_d2, lag.max = 5*12, main = "")


# ARIMA Models

mod1 <- arima(treino, order = c(0, 1, 1), seasonal = c(0, 1, 0))
mod2 <- arima(treino, order = c(0, 1, 3), seasonal = c(0, 1, 0))
mod3 <- arima(treino, order = c(1, 1, 1), seasonal = c(0, 1, 0))
mod4 <- arima(treino, order = c(1, 1, 3), seasonal = c(0, 1, 0))

mod5 <- arima(treino, order = c(0, 1, 1), seasonal = c(1, 1, 0))
mod6 <- arima(treino, order = c(0, 1, 3), seasonal = c(1, 1, 0))
mod7 <- arima(treino, order = c(1, 1, 1), seasonal = c(1, 1, 0))
mod8 <- arima(treino, order = c(1, 1, 3), seasonal = c(1, 1, 0))

mod9 <- arima(treino, order = c(0, 1, 1), seasonal = c(0, 1, 1))
mod10 <- arima(treino, order = c(0, 1, 3), seasonal = c(0, 1, 1))
mod11 <- arima(treino, order = c(1, 1, 1), seasonal = c(0, 1, 1))
mod12 <- arima(treino, order = c(1, 1, 3), seasonal = c(0, 1, 1))

mod13 <- arima(treino, order = c(0, 1, 1), seasonal = c(1, 1, 1))
mod14 <- arima(treino, order = c(0, 1, 3), seasonal = c(1, 1, 1))
mod15 <- arima(treino, order = c(1, 1, 1), seasonal = c(1, 1, 1))
mod16 <- arima(treino, order = c(1, 1, 3), seasonal = c(1, 1, 1))

coeff <- data.frame(phi1 = c("", "", -0.177, -0.755,
                             "", "", -0.142, -0.135,
                             "", "", -0.142, -0.802,
                             "", "", -0.142, -0.584),
                    
                    theta1 = c(-0.43, -0.514, -0.306, 0.312,
                               -0.403, -0.436, -0.299, -0.305, 
                               -0.366, -0.363, -0.255, 0.454, 
                               -0.364, -0.361, -0.253, 0.217),
                    
                    theta2 = c("", 0.04, "", -0.376,
                               "", 0.001, "", -0.063,
                               "", 0.031, "", -0.280,
                               "", 0.029, "", -0.188),
                    
                    theta3 = c("", 0.277, "", 0.312,
                               "", 0.289, "", 0.291,
                               "", 0.282, "", 0.266,
                               "", 0.282, "", 0.278),
                    
                    varPhi1 = c("", "", "", "",
                                -0.473, -0.457, -0.470, -0.459,
                                "", "", "", "",
                                -0.059, -0.062, -0.058, -0.032),
                    
                    varTheta1 = c("", "", "", "",
                                  "", "", "", "",
                                  -0.659, -0.646, -0.653, -0.641,
                                  -0.617, -0.603, -0.612, -0.646),
                    
                    row.names = c("Modelo 1", "Modelo 2", "Modelo 3",
                                  "Modelo 4", "Modelo 5", "Modelo 6",
                                  "Modelo 7", "Modelo 8", "Modelo 9", 
                                  "Modelo 10", "Modelo 11", "Modelo 12", 
                                  "Modelo 13", "Modelo 14", "Modelo 15", 
                                  "Modelo 16"))

kable(coeff, caption = "Parâmetros Estimados dos Modelos")

# Parsimony

n <- length(treino)
k <- c(1, 3, 2, 4,
       2, 4, 3, 5,
       2, 4, 3, 5,
       3, 5, 4, 6)
aic.arima <- unlist(lapply(list(mod1,mod2,mod3,mod4,
                                mod5, mod6, mod7, mod8,
                                mod9, mod10, mod11, mod12,
                                mod13, mod14, mod15, mod16), AIC))
aicc.arima <- aic.arima + (2*k^2 + 2*k)/(n - k - 1)

x <- data.frame("AIC" = aic.arima, "AICc" = aicc.arima, 
                row.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4",
                              "Modelo 5", "Modelo 6", "Modelo 7", "Modelo 8",
                              "Modelo 9", "Modelo 10", "Modelo 11", "Modelo 12",
                              "Modelo 13", "Modelo 14", "Modelo 15", "Modelo 16"))

kable(x, caption = "Critérios de Informação de Akaike")


# ETS models

ets1 <- ets(treino, model = "AAA", damped = F)
ets2 <- ets(treino, model = "AAA", damped = T)
ets3 <- ets(treino, model = "AMA", damped = F, restrict = F)
ets4 <- ets(treino, model = "AMA", damped = T, restrict = F)
ets5 <- ets(treino, model = "MAA", damped = F)
ets6 <- ets(treino, model = "MAA", damped = T)
ets7 <- ets(treino, model = "MMA", damped = F, restrict = F)
ets8 <- ets(treino, model = "MMA", damped = T, restrict = F)
ets9 <- ets(treino, model = "MAM", damped = F)
ets10 <- ets(treino, model = "MAM", damped = T)
ets11 <- ets(treino, model = "MMM", damped = F)
ets12 <- ets(treino, model = "MMM", damped = T)

df <- data.frame("AIC" = c(ets1$aic, ets2$aic, ets3$aic, ets4$aic,
                           ets5$aic, ets6$aic, ets7$aic, ets8$aic,
                           ets9$aic, ets10$aic, ets11$aic, ets12$aic),
                 "AICc" = c(ets1$aicc, ets2$aicc, ets3$aicc, ets4$aicc,
                            ets5$aicc, ets6$aicc, ets7$aicc, ets8$aicc,
                            ets9$aicc, ets10$aicc, ets11$aicc, ets12$aicc),
                 row.names = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4", 
                               "Modelo 5", "Modelo 6", "Modelo 7", "Modelo 8", 
                               "Modelo 9", "Modelo 10", "Modelo 11", "Modelo 12"))

kable(df, caption = "Critérios de Informação de Akaike")

df <- data.frame("alpha" = 0.6425, "beta" = 0.0253, gamma = "0.0001",
                 "l0" = 3441.5265, "b0" = 1.055)

kable(df, caption = "Coeficientes estimados do Modelo ETS(M, Md, M)")

df <- data.frame("S-11" = 1.0235, "S-10" = 1.0073, "S-9" = 1.0007, "S-8" = 1.007,
                 "S-7" = 0.9921, "S-6" = 0.9904)

kable(df, caption = "Parâmetros sazonais iniciais")

df <- data.frame("S-5" = 1.0035, "S-4" = 0.9975, "S-3" = 0.9926, "S-2" = 0.9977,
                 "S-1" = 0.9902, "S0" = 0.9976)

kable(df, caption = "Parâmetros sazonais iniciais")


# Residual Analysis

res_sarima <- window(mod10$residuals, start = time(treino)[13])


x <- autoplot(res_sarima) +
  labs(x = "Ano", y = "Resíduos") +
  ggtitle("Estacionariedade")+
  theme_bw()

y <- data.frame(res_sarima) %>%
  ggplot(aes(sample = res_sarima)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Quantis Teóricos", y = "Quantis Amostrais") +
  ggtitle("Normalidade")+
  theme_bw()


grid.arrange(x, y, newpage = F, nrow = 1)

par(mfrow = c(1, 2))
acf(res_sarima, lag.max = 7*4, main = "Autocorrelações")
pacf(res_sarima, lag.max = 7*4, main = "Autocorrelações Parciais")


a <- kpss.test(res_sarima)
b <- shapiro.test(res_sarima)
c <- Box.test(res_sarima, lag = 19, type = "Ljung-Box", fitdf = 4)

x <- data.frame("Teste" = c("KPSS", "Shapiro-Wilk", "Ljung-Box"),
                "Hipótese" = c("Estacionariedade", "Normalidade", "Independência"),
                "Estatística" = round(c(a$statistic, b$statistic, c$statistic), 3),
                "p-valor" = round(c(a$p.value, b$p.value, c$p.value), 3),
                row.names = NULL)

kable(x, caption = "Testes de Hipóteses")

res_ets <- ets12$residuals


x <- autoplot(res_ets) +
  labs(x = "Ano", y = "Resíduos") +
  ggtitle("Estacionariedade")+
  theme_bw()

y <- data.frame(res_ets) %>%
  ggplot(aes(sample = res_ets)) +
  stat_qq() +
  stat_qq_line() +
  labs(x = "Quantis Teóricos", y = "Quantis Amostrais") +
  ggtitle("Normalidade")+
  theme_bw()


grid.arrange(x, y, newpage = F, nrow = 1)

par(mfrow = c(1, 2))
acf(res_ets, lag.max = 7*4, main = "Autocorrelações")
pacf(res_ets, lag.max = 7*4, main = "Autocorrelações Parciais")

a <- kpss.test(res_ets)
b <- shapiro.test(res_ets)
c <- Box.test(res_ets, lag = 15, type = "Ljung-Box", fitdf = 3)

x <- data.frame("Teste" = c("KPSS", "Shapiro-Wilk", "Ljung-Box"),
                "Hipótese" = c("Estacionariedade", "Normalidade", "Independência"),
                "Estatística" = round(c(a$statistic, b$statistic, c$statistic), 3),
                "p-valor" = round(c(a$p.value, b$p.value, c$p.value), 3),
                row.names = NULL)

kable(x, caption = "Testes de Hipóteses")


# Prediction capability

origem <- length(treino) - 14
erros_sarima <- data.frame()
erros_ets <- data.frame()
mes_fim <- 7
ano_fim <- 1991

for (i in 1:10) {
  serie_janela <- ts(treino[1:origem], start = c(1983, 1),
                     end = c(ano_fim, mes_fim), frequency = 12)
  
  
  janela_ets <- ets(serie_janela, model = "MMM", damped = T)
  janela_sarima <- arima(serie_janela, order = c(0, 1, 3), seasonal = c(0, 1, 1))
  
  previsao_ets <- forecast(janela_ets, h = 5)
  previsao_sarima <- forecast(janela_sarima, h = 5)
  
  errosEts_janela <- treino[(origem + 1):(origem + 5)] -  previsao_ets$mean
  errosSarima_janela <- treino[(origem + 1):(origem + 5)] - previsao_sarima$mean 
  
  erros_ets <- rbind(erros_ets, errosEts_janela)
  erros_sarima <- rbind(erros_sarima, errosSarima_janela)
  
  origem <- origem + 1
  mes_fim <- mes_fim + 1
  
  if (mes_fim == 13){
    mes_fim <- 1
    ano_fim <- 1992
  }
}

colnames(erros_ets) <- c("e1", "e2", "e3", "e4", "e5")
colnames(erros_sarima) <- c("e1", "e2", "e3", "e4", "e5")

row.names(erros_ets) <- c("Passo 1", "Passo 2", "Passo 3", "Passo 4", "Passo 5",
                          "Passo 6","Passo 7","Passo 8","Passo 9","Passo 10")
row.names(erros_sarima) <- c("Passo 1", "Passo 2", "Passo 3", "Passo 4", "Passo 5",
                             "Passo 6","Passo 7","Passo 8","Passo 9","Passo 10")


erro_medio <- data.frame("Horizonte" = c(1:5),
                         "erros" = c(sum(abs(erros_sarima$e1)) / 10,
                                     sum(abs(erros_sarima$e2)) / 10,
                                     sum(abs(erros_sarima$e3)) / 10,
                                     sum(abs(erros_sarima$e4)) / 10,
                                     sum(abs(erros_sarima$e5)) / 10,
                                     sum(abs(erros_ets$e1)) / 10,
                                     sum(abs(erros_ets$e2)) / 10,
                                     sum(abs(erros_ets$e3)) / 10,
                                     sum(abs(erros_ets$e4)) / 10,
                                     sum(abs(erros_ets$e5)) / 10),
                         "modelo" = c(rep("SARIMA", 5), rep("ETS", 5)))

kable(erros_sarima, caption = "Validação cruzada por janela deslizante do Modelo SARIMA")

kable(erros_ets, caption = "Validação cruzada por janela deslizante do Modelo ETS")

ggplot(erro_medio, aes(x = erro_medio$Horizonte, y = erro_medio$erros))+
  geom_point(aes(color = erro_medio$modelo))+
  geom_line(aes(color = erro_medio$modelo))+
  scale_color_manual(values=c("#b32020", "#2036b3"), 
                     name="Modelo",
                     labels=c("ETS", "SARIMA"))+
  theme_bw()+
  labs(x = "Horizonte de Previsão", y = "Erro Absoluto Médio do Modelo",
       title = "Erro Médio dos Modelos em cada Horizonte")


# Predictions

pp_sarima <- mod10 %>% predict(n.ahead = 18)
pS_90 <- mod10 %>% forecast (h = 18 , level = 90)
pS_95 <- mod10 %>% forecast (h = 18 , level = 95)
pS_99 <- mod10 %>% forecast (h = 18 , level = 99)

x_sarima <- data.frame("LI99%" = round(pS_99$lower, 2),
                       "LI95%" = round(pS_95$lower, 2),
                       "LI90%" = round(pS_90$lower, 2),
                       "previsao" = round(pp_sarima$pred, 2),
                       "LS90%" = round(pS_90$upper, 2),
                       "LS95%" = round(pS_95$upper, 2),
                       "LS99%" = round(pS_99$upper, 2))

colnames(x_sarima) <- c("LI 99%", "LI 95%", "LI 90%", "Prev. Pontual",
                        "LS 90%", "LS 95%", "LS 99%")

kable(x_sarima, caption = "Previsões do Modelo SARIMA")

pE_90 <- ets12 %>% forecast (h = 18 , level = 90)
pE_95 <- ets12 %>% forecast (h = 18 , level = 95)
pE_99 <- ets12 %>% forecast (h = 18 , level = 99)

x_ets <- data.frame("LI99%" = round(pE_99$lower, 2),
                    "LI95%" = round(pE_95$lower, 2),
                    "LI90%" = round(pE_90$lower, 2),
                    "previsao" = round(pE_90$mean, 2),
                    "LS90%" = round(pE_90$upper, 2),
                    "LS95%" = round(pE_95$upper, 2),
                    "LS99%" = round(pE_99$upper, 2))

colnames(x_ets) <- c("LI 99%", "LI 95%", "LI 90%", "Prev. Pontual",
                     "LS 90%", "LS 95%", "LS 99%")

kable(x_ets, caption = "Previsões do Modelo ETS")


x <- autoplot(ts(c(treino, teste), start = c(1983, 1), frequency = 12)) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsões Pontuais - Modelo SARIMA")+
  theme_bw() +
  autolayer(pS_90$mean, series = "Previsão") +
  scale_colour_manual(values = c("Previsão" = "#2036b3"), breaks = c("Previsão"),
                      name = "")

y <- autoplot(ts(c(treino, teste), start = c(1983, 1), frequency = 12)) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsões Pontuais - Modelo ETS")+
  theme_bw() +
  autolayer(pE_90$mean, series = "Previsão") +
  scale_colour_manual(values = c("Previsão" = "#b32020"), breaks = c("Previsão"),
                      name = "")

grid.arrange(x, y, nrow = 2)


xS <- autoplot(treino) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsão de 90% - Modelo SARIMA") +
  theme_bw() +
  autolayer(pS_90, series = "Previsão") +
  autolayer(teste, series = "Observações") +
  scale_colour_manual(values = c("Previsão" = "#2036b3", "Observações" = "black"),
                      breaks = c("Previsão", "Observações"), name = "")

xE <- autoplot(treino) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsão de 90% - Modelo ETS") +
  theme_bw() +
  autolayer(pE_90, series = "Previsão") +
  autolayer(teste, series = "Observações") +
  scale_colour_manual(values = c("Previsão" = "#b32020", "Observações" = "black"),
                      breaks = c("Previsão", "Observações"), name = "")

yS <- autoplot(treino) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsão de 95% - Modelo SARIMA") +
  theme_bw() +
  autolayer(pS_95, series = "Previsão") +
  autolayer(teste, series = "Observações") +
  scale_colour_manual(values = c("Previsão" = "#2036b3", "Observações" = "black"),
                      breaks = c("Previsão", "Observações"), name = "")

yE <- autoplot(treino) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsão de 95% - Modelo ETS") +
  theme_bw() +
  autolayer(pE_95, series = "Previsão") +
  autolayer(teste, series = "Observações") +
  scale_colour_manual(values = c("Previsão" = "#b32020", "Observações" = "black"),
                      breaks = c("Previsão", "Observações"), name = "")

zS <- autoplot(treino) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsão de 99% - Modelo SARIMA") +
  theme_bw() +
  autolayer(pS_99, series = "Previsão") +
  autolayer(teste, series = "Observações") +
  scale_colour_manual(values = c("Previsão" = "#2036b3", "Observações" = "black"),
                      breaks = c("Previsão", "Observações"), name = "")

zE <- autoplot(treino) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsão de 99% - Modelo ETS") +
  theme_bw() +
  autolayer(pE_99, series = "Previsão") +
  autolayer(teste, series = "Observações") +
  scale_colour_manual(values = c("Previsão" = "#b32020", "Observações" = "black"),
                      breaks = c("Previsão", "Observações"), name = "")

grid.arrange(xS, xE, yS, yE, zS, zE, nrow = 3, newpage = F)


# M model

M <- (pS_90$mean + pE_90$mean)/2

kable(data.frame("Horizonte" = c(1:18), "Prev. Pontual" = M),
      caption = "Previsões do Modelo M")

autoplot(ts(c(treino, teste), start = c(1983, 1), frequency = 12)) +
  xlab("Ano") +
  ylab("Valor") +
  ggtitle("Previsões Pontuais - Modelo M")+
  theme_bw() +
  autolayer(M, series = "Previsão") +
  scale_colour_manual(values = c("Previsão" = "#e6bf00"),
                      breaks = c("Previsão"), name = "")


# Accuracy

autoarima_pred <- forecast(auto.arima(treino), h = h)
ses_pred <- ses(treino, h = h)
holt_pred <- holt(treino , h = h)
autoets_pred <- forecast(ets(treino), h = h)
stlf_pred <- stlf(treino, h = h)
bats_pred <- forecast(bats(treino), h = h)
tbats_pred <- forecast(tbats(treino), h = h)

lista <- list(pS_95, pE_95, autoarima_pred, ses_pred, holt_pred, 
              autoets_pred, stlf_pred, bats_pred, tbats_pred)

mae <- unlist(lapply(lista, function(x) return(mean(abs(teste - x$mean)))))

MAE <- data.frame("MAE" = mae)
MAE <- rbind(mean(abs(teste - M)), MAE)

row.names(MAE) <- c("Modelo M", "Modelo SARIMA Manual", "Modelo ETS Manual",
                    "auto.arima", "SES", "Holt", "ETS Automático",
                    "stlf", "bats", "tbats")

kable(MAE, caption = "Comparação de benchmarks dos modelos")