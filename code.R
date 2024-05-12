# __author__ = ["David Cardona-Vasquez"]
# __copyright__ = "Copyright 2023-2024, Graz University of Technology"
# __credits__ = ["David Cardona-Vasquez"]
# __license__ = "MIT"
# __maintainer__ = "David Cardona-Vasquez"
# __status__ = "Development"



library(readxl)
library(ggplot2)
library(ggfortify)
library(vars)
library(xts)
library(aTSA)
library(tseries)
library(lubridate)


options(OutDec = '.')


# Data loading section and preprocessing

datos.dc <- read_excel("datos_nver_2018_up.xlsx", sheet = "datos_dc")
datos.nces <- read_excel("datos_nver_2018_up.xlsx", sheet = "datos_nces")
datos.cap <- read_excel("datos_nver_2018_up.xlsx", sheet = "datos_cap")

datos.todos <- cbind(datos.dc, datos.nces[, -1])

datos.ts <- as.ts(xts::xts(order.by = as.Date(datos.todos$fecha, "%Y-%m-%d"), 
                           x = datos.todos[,-c(1,10, 11, 17)]))
head(datos.ts)


# Transform to GWh
datos.ts[, 13] <- datos.ts[, 13]/1000000
datos.ts[, 3] <- datos.ts[, 3]/1000000


# Testing for unit root, change 13 with the desired variable
# 13 > ncres generation
# 3  > large hydro availability
# 2  > weighted average hydro price
# 5  > weighted average spot price
# k  > lags to test ADF
adf.test(datos.ts[,13], k = 360)


lista <- c(13, 3, 2, 5)
rezagos_elegidos <- 21
tipo <- "both"
var <- VAR(datos.ts[, lista], p=rezagos_elegidos, type = tipo)
ser_test <- serial.test(var, lags.pt = 40
                        , type ="PT.asymptotic")
ser_test

ser_test <- serial.test(var, lags.pt = 40, type ="BG")
ser_test

# VEC model estimation
lista <- c(13, 3, 2, 5)
rm(co_obj)
co_obj <- ca.jo(datos.ts[, lista], type='trace', K = rezagos_elegidos, ecdet = "none",
                spec='longrun', dumvar = datos.cap[, 10])
summary(co_obj)


# Estimating cointegration equations
co_obj_eq <- cajorls(co_obj, r=3)
#summary(co_obj_eq$rlm)
(co_obj_eq$beta)




# Transforming the VEC model for manipulation.
vec_var <- vec2var(co_obj, r=3)

resid_df <- data.frame(resid(vec_var))
pred_df <- data.frame(fitted(vec_var))



# autocorrelation of residuals
autoplot(acf(resid_df$resids.of.gen_frnc, plot = FALSE), 
         conf.int.fill = '#0000FF', 
         conf.int.value = 0.8, conf.int.type = 'ma', ylim = c(-0.25, 0.25)) + 
  ggtitle("Residual Autocorrelation - NCRES Generation") + 
  theme(plot.title = element_text(hjust = 0.5))

autoplot(acf(resid_df$resids.of.disp_hidro, plot = FALSE), 
         conf.int.fill = '#0000FF', 
         conf.int.value = 0.8, conf.int.type = 'ma', ylim = c(-0.25, 0.25)) + 
  ggtitle("Residual Autocorrelation - Hydro Availability") + 
  theme(plot.title = element_text(hjust = 0.5))

autoplot(acf(resid_df$resids.of.pofe_hidro, plot = FALSE), 
         conf.int.fill = '#0000FF', 
         conf.int.value = 0.8, conf.int.type = 'ma', ylim = c(-0.25, 0.25)) + 
  ggtitle("Residual Autocorrelation - Hydro Bid Price") + 
  theme(plot.title = element_text(hjust = 0.5))

autoplot(acf(resid_df$resids.of.pbolsa, plot = FALSE), 
         conf.int.fill = '#0000FF', 
         conf.int.value = 0.8, conf.int.type = 'ma', ylim = c(-0.25, 0.25)) + 
  ggtitle("Residual Autocorrelation - Spot Price") + 
  theme(plot.title = element_text(hjust = 0.5))


days <- seq(as.Date("2018/1/22"), as.Date("2022/7/31"), "days")
resid_df['days'] <- days


# Residuals plots
p <- ggplot(resid_df, aes(x=days, y=resids.of.gen_frnc)) +
  geom_line() + 
  ggtitle("Residuals - NCRES Generation") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("") + ylab("") + scale_x_date(date_breaks = "6 month", date_labels = "%b-%Y")
p

p <- ggplot(resid_df, aes(x=days, y=resids.of.disp_hidro)) +
  geom_line() + 
  ggtitle("Residuals - Hydro Availability") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("") + ylab("") + scale_x_date(date_breaks = "6 month", date_labels = "%b-%Y")
p

p <- ggplot(resid_df, aes(x=days, y=resids.of.pofe_hidro)) +
  geom_line() + 
  ggtitle("Residuals - Hydro Bid Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("") + ylab("") + scale_x_date(date_breaks = "6 month", date_labels = "%b-%Y")
p

p <- ggplot(resid_df, aes(x=days, y=resids.of.pbolsa)) +
  geom_line() + 
  ggtitle("Residuals - Spot Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("") + ylab("") + scale_x_date(date_breaks = "6 month", date_labels = "%b-%Y")
p



# Prediction vs real for spot price
real <- datos.ts[22:1673,5]
pred <- fitted(vec_var)[,4]

pbolsa_df <- data.frame(real, pred, days)
pgg <- ggplot(pbolsa_df, aes(x=days)) +
  
  geom_line( aes(y=real, color='Real')) + 
  geom_point( aes(y=pred, color='Forecast')) +
  scale_color_manual(name = "Series", values = c("Real" = "black", 
                                                 "Forecast" = "black")) +
  scale_y_continuous(
    name = "COP/kWh",
  ) + 
  xlab("") +
  theme(
    axis.title.y = element_text(color = 'black'),
    axis.title.y.right = element_text(color = 'black'),
    plot.title = element_text(hjust = 0.5)
  ) +
  ggtitle("Spot Price and Forecast - (COP/kWh)")
pgg


# calculating volatility of real spot prices for the whole dataset
vol_real <- sd(log(real[2:1651])-log(real[1:1650]))
# calculating volatility of one-step ahead predicted spot prices
vol_pred <- sd(log(pred[2:1651])-log(pred[1:1650]))


# impulse-response functions and plotting
# shock to ncres generation
aux <- irf(vec_var, impulse="gen_frnc", response = "pbolsa", 
           runs=200, n.ahead = 365)
# shock to weighted-average large hydro bid prices
aux2 <- irf(vec_var, impulse="pofe_hidro", response = "pbolsa", 
           runs=200, n.ahead = 365)
# shock to large-hydro's available energy
aux3 <- irf(vec_var, impulse="disp_hidro", response = "pbolsa", 
            runs=200, n.ahead = 365)


idx <- seq(1, 366)
gen <- data.frame(idx, aux$irf, aux$Lower, aux$Upper)
colnames(gen) <- c("idx", "pbolsa", "inferior", "superior")
pofe <- data.frame(idx, aux2$irf, aux2$Lower, aux2$Upper)
colnames(pofe) <- c("idx", "pbolsa", "inferior", "superior")
disp <- data.frame(idx, aux3$irf, aux3$Lower, aux3$Upper)
colnames(disp) <- c("idx", "pbolsa", "inferior", "superior")

sd <- sd(vec_var$resid[, 1])
p1<-ggplot(data=gen, aes(x=idx, y=pbolsa)) + geom_line() + xlab("days after shock")
p1<-p1+geom_ribbon(aes(ymin=gen$inferior, ymax=gen$superior), linetype=2, alpha=0.1)
p1<-p1+ theme(plot.title = element_text(hjust = 0.5)) +labs(y="COP/kWh") + ggtitle("Spot Price Response to Shock in NCRES")
p1<-p1+scale_x_continuous(breaks = round(seq(min(gen$idx), max(gen$idx), by = 20),1))
p1<-p1+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
p1

sd <- sd(vec_var$resid[, 2])
p1<-ggplot(data=pofe, aes(x=idx, y=pbolsa)) + geom_line() + xlab("days after shock")
p1<-p1+geom_ribbon(aes(ymin=pofe$inferior, ymax=pofe$superior), linetype=2, alpha=0.1)
p1<-p1+ theme(plot.title = element_text(hjust = 0.5)) +labs(y="COP/kWh") + ggtitle("Spot Price Response to Shock in Bid Price")
p1<-p1+scale_x_continuous(breaks = round(seq(min(pofe$idx), max(pofe$idx), by = 20),1))
p1<-p1+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
p1


sd <- sd(vec_var$resid[, 3])
p1<-ggplot(data=disp, aes(x=idx, y=pbolsa)) + geom_line() + xlab("days after shock")
p1<-p1+geom_ribbon(aes(ymin=disp$inferior, ymax=disp$superior), linetype=2, alpha=0.1)
p1<-p1+ theme(plot.title = element_text(hjust = 0.5))+labs(y="COP/kWh") + ggtitle("Spot Price Response to Shock in Hydropower Availability")
p1<-p1+scale_x_continuous(breaks = round(seq(min(disp$idx), max(disp$idx), by = 20),1))
p1<-p1+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
p1




# calculating volatilities afeter NCRES shock
i <- 365
vol_pred_360 <- sd(log(pred[(1651-i+1):1651])
                   -log(pred[(1651-i):1650]))

pred_shock <- pred[(1651-365):1651] + aux$irf$gen_frnc

vol_pred_shock <- sd(log(pred_shock[2:366]) - log(pred_shock[1:365]))

vol_real_360 <- sd(log(real[(1651-i+1):1651])
                   -log(real[(1651-i):1650]))




# Scenario evaluation with 2019 Expansion plan
fulfillment <- 1

new_pv <- 5888 * fulfillment
new_wind <- 4127 * fulfillment
new_large_hydro <- 1699
new_small_hydro <- 475 * fulfillment




# all the code onwards can be greatly improved for readability

i <- 360

ult_datos <- datos.ts[(1673-i-21):nrow(datos.ts), lista]
ult_datos_cap <- datos.cap[(1673-i-21):1673, ]


aux_wind <- ult_datos_cap$wind + new_wind*1e3
aux_pv <- ult_datos_cap$pv + new_pv*1e3
aux_large_hydro <- ult_datos_cap$large_hydro + new_large_hydro*1e3
aux_small_hydro <- ult_datos_cap$small_hydro + new_small_hydro*1e3

aux_fernc <- (aux_wind * ult_datos_cap$gen_cap_wind*24)/1e6
aux_fernc <- (aux_pv * ult_datos_cap$gen_cap_pv*24)/1e6 + aux_fernc
aux_fernc <- (aux_small_hydro * ult_datos_cap$gen_cap_hydro*24)/1e6 + aux_fernc

aux_large_hydro <- (aux_large_hydro*ult_datos_cap$gen_cap_large_hydro*24)/1e6


ult_datos[, 1] <- aux_fernc
ult_datos[, 2] <- aux_large_hydro


mat <- cbind(ult_datos[, 1], ult_datos[, 2], ult_datos[, 3], ult_datos[, 4])
mat <- cbind(mat, matrix(1, nrow = nrow(mat), ncol = 1))

ncres_gen <- lag.xts(ult_datos[, 1], k=1)
hydro_ava <- lag.xts(ult_datos[, 2], k=1)
offer_p <- lag.xts(ult_datos[, 3], k= 1)
spot_p <- lag.xts(ult_datos[, 4], k=1)
aux_dummy <- datos.cap[(1673-i-21):nrow(datos.ts), 10]

mat <- cbind(mat, ncres_gen, hydro_ava, offer_p, spot_p)

for(i in c(2:21))
{
  
  ncres_gen <- lag.xts(ult_datos[, 1], k=i)
  hydro_ava <- lag.xts(ult_datos[, 2], k=i)
  offer_p <- lag.xts(ult_datos[, 3], k= i)
  spot_p <- lag.xts(ult_datos[, 4], k=i)
  
  mat <- cbind(mat, ncres_gen, hydro_ava, offer_p, spot_p)
}

mat <- mat[22:nrow(mat), ]


# Removing the dummies for later
rm(co_obj)
co_obj <- ca.jo(datos.ts[, lista], type='trace', K = rezagos_elegidos, ecdet = "none",
                spec='longrun')
summary(co_obj)
co_obj_eq <- cajorls(co_obj, r=3)
#summary(co_obj_eq$rlm)
(co_obj_eq$beta)

vec_var <- vec2var(co_obj, r=3)

vec2 <- vec_var

colnames(mat) <- colnames(vec2$datamat)
vec2$datamat <- mat

n_ahead = 60
vec2$obs <- nrow(mat)
vec2$totobs <- vec2$obs + vec2$p

prediction <- predict(vec2, n.ahead = n_ahead)

preds <- prediction$fcst$pbolsa[,3]
preds_shock <- preds + gen$pbolsa[1:(n_ahead)]
vol1 <- sd(log(abs(preds[2:n_ahead]/preds[1:(n_ahead-1)])))
vol2 <- sd(log(abs(preds_shock[2:n_ahead]/preds_shock[1:(n_ahead-1)])))

