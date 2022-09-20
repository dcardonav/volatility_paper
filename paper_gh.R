library(readxl)
library(ggplot2)
library(ggfortify)
library(vars)
library(xts)
library(aTSA)
library(tseries)
library(lubridate)


options(OutDec = '.')


datos.dc <- read_excel("datos_nver_2018.xlsx", sheet = "datos_dc")
datos.nces <- read_excel("datos_nver_2018.xlsx", sheet = "datos_nces")
datos.cap <- read_excel("datos_nver_2018.xlsx", sheet = "datos_cap")

datos.todos <- cbind(datos.dc, datos.nces[, -1])

aux1 <- diff(datos.todos$volutil)
aux2 <- diff(datos.todos$volutil_ener)
aux3 <- diff(datos.todos$genide_hidro)
aux4 <- diff(datos.todos$genide_sol)
aux5 <- diff(datos.todos$gen_frnc)
datos.todos <- datos.todos[-1, ]
datos.todos["diff_vol"] <- aux1
datos.todos["diff_vol_ener"] <- aux2
datos.todos["diff_genide_hidro"] <- aux3
datos.todos["diff_genide_sol"] <- aux4
datos.todos["diff_genide_frnc"] <- aux5

datos.ts <- as.ts(xts::xts(order.by = as.Date(datos.todos$fecha, "%Y-%m-%d"), 
                           x = datos.todos[,-c(1,10, 11, 17)]))
head(datos.ts)


adf.test(datos.ts[,13], k = 4)

lista <- c(13, 3, 2, 5)
datos.ts[, lista[c(1, 2)]] <- datos.ts[, lista[c(1, 2)]]/1e6
rezagos <- 21
var <- VARselect(datos.ts[, lista], lag.max = rezagos, type = "trend")
var
var <- VARselect(datos.ts[, lista], lag.max = rezagos, type = "both")
var
var <- VARselect(datos.ts[, lista], lag.max = rezagos, type = "const")
var
var <- VARselect(datos.ts[, lista], lag.max = rezagos, type = "none")
var


rezagos_elegidos <- 21
tipo <- "both"
var <- VAR(datos.ts[, lista], p=rezagos_elegidos, type = tipo)
ser_test <- serial.test(var, lags.pt = 40
                        , type ="PT.asymptotic")
ser_test

ser_test <- serial.test(var, lags.pt = 40, type ="BG")
ser_test

lista <- c(13, 3, 2, 5)
rm(co_obj)
co_obj <- ca.jo(datos.ts[, lista], K = rezagos_elegidos, ecdet = "none",
                spec='transitory')
summary(co_obj)

co_obj_eq <- cajorls(co_obj, r=3)
summary(co_obj_eq$rlm)
(co_obj_eq$beta)


vec_var <- vec2var(co_obj, r=3)

resid_df <- data.frame(resid(vec_var))
pred_df <- data.frame(fitted(vec_var))


autoplot(acf(resid_df$resids.of.gen_frnc, plot = FALSE), 
         conf.int.fill = '#0000FF', 
         conf.int.value = 0.8, conf.int.type = 'ma') + 
  ggtitle("Residual Autocorrelation - NCES Generation") + 
  theme(plot.title = element_text(hjust = 0.5))

autoplot(acf(resid_df$resids.of.disp_hidro, plot = FALSE), 
         conf.int.fill = '#0000FF', 
         conf.int.value = 0.8, conf.int.type = 'ma') + 
  ggtitle("Residual Autocorrelation - Hydro Availability") + 
  theme(plot.title = element_text(hjust = 0.5))

autoplot(acf(resid_df$resids.of.pofe_hidro, plot = FALSE), 
         conf.int.fill = '#0000FF', 
         conf.int.value = 0.8, conf.int.type = 'ma') + 
  ggtitle("Residual Autocorrelation - Hydro Offer Price") + 
  theme(plot.title = element_text(hjust = 0.5))

autoplot(acf(resid_df$resids.of.pbolsa, plot = FALSE), 
         conf.int.fill = '#0000FF', 
         conf.int.value = 0.8, conf.int.type = 'ma') + 
  ggtitle("Residual Autocorrelation - Spot Price") + 
  theme(plot.title = element_text(hjust = 0.5))


days <- seq(as.Date("2018/1/23"), as.Date("2022/7/31"), "days")
resid_df['days'] <- days

p <- ggplot(resid_df, aes(x=days, y=resids.of.gen_frnc)) +
  geom_line() + 
  ggtitle("Residuals - NCES Generation") + 
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
  ggtitle("Residuals - Hydro Offer Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("") + ylab("") + scale_x_date(date_breaks = "6 month", date_labels = "%b-%Y")
p

p <- ggplot(resid_df, aes(x=days, y=resids.of.pbolsa)) +
  geom_line() + 
  ggtitle("Residuals - Spot Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  xlab("") + ylab("") + scale_x_date(date_breaks = "6 month", date_labels = "%b-%Y")
p


real <- datos.ts[22:1672,5]
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

vol_real <- sd(log(real[2:1651])-log(pred[1:1650]))
vol_pred <- sd(log(pred[2:1651])-log(pred[1:1650]))

aux <- irf(vec_var, impulse="gen_frnc", response = "pbolsa", 
           runs=200, n.ahead = 365)

aux2 <- irf(vec_var, impulse="pofe_hidro", response = "pbolsa", 
           runs=200, n.ahead = 365)

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
p1<-p1+ theme(plot.title = element_text(hjust = 0.5)) +labs(y="COP/kWh") + ggtitle("Spot Price Response to Shock in NCES")
p1<-p1+scale_x_continuous(breaks = round(seq(min(gen$idx), max(gen$idx), by = 20),1))
p1<-p1+theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
p1

sd <- sd(vec_var$resid[, 2])
p1<-ggplot(data=pofe, aes(x=idx, y=pbolsa)) + geom_line() + xlab("days after shock")
p1<-p1+geom_ribbon(aes(ymin=pofe$inferior, ymax=pofe$superior), linetype=2, alpha=0.1)
p1<-p1+ theme(plot.title = element_text(hjust = 0.5)) +labs(y="COP/kWh") + ggtitle("Spot Price Response to Shock in Offer Price")
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


i <- 365
vol_pred_360 <- sd(log(pred[(1651-i+1):1651])
                   -log(pred[(1651-i):1650]))


pred_shock <- pred[(1651-365):1651] + aux$irf$gen_frnc


vol_pred_shock <- sd(log(pred_shock[2:366]) - log(pred_shock[1:365]))

vol_real_360 <- sd(log(real[(1651-i+1):1651])
                   -log(real[(1651-i):1650]))




# Scenario evaluation
fulfillment <- 1

new_pv <- 5888 * fulfillment
new_wind <- 4127 * fulfillment
new_large_hydro <- 1699
new_small_hydro <- 475 * fulfillment




i <- 360

ult_datos <- datos.ts[(1672-i-21):nrow(datos.ts), lista]
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

vec2 <- vec_var

colnames(mat) <- colnames(vec2$datamat)
vec2$datamat <- mat
 
vec2$obs <- nrow(mat)
vec2$totobs <- vec2$obs + vec2$p
prediction <- predict(vec2, n.ahead = 31)

preds <- prediction$fcst$pbolsa[,3]
vol1 <- sd(log(abs(preds[2:31]/preds[1:30])))
preds_shock <- preds + gen$pbolsa[1:31]
vol2 <- sd(log(abs(preds_shock[2:31]/preds_shock[1:30])))

