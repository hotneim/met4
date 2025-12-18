
# ---------------------------------- #
# R-SCRIPT                           #
# MET4 - EMPIRISKE METODER           #
# NHH NORGES HANDELSH?YSKOLE         #
#                                    #
# MODUL 3: Regresjon                 #
# ---------------------------------- #

## DEL I: Enkel Regresjon

library(readxl)
library(ggplot2)

# Bileksempel
cars <- read_excel("Xm16-02.xls")
head(cars)

ggplot(cars) +
  geom_point(aes(x = Odometer, y = Price)) +
  ggtitle("Kj?relengde vs. Pris")

# M?t din nye bestevenn: lm()-funksjonen
reg <- lm(Price ~ Odometer, data = cars)
summary(reg)

# Plott regresjonslinjen
x <- seq(15, 50, by = 0.5)
pred <- predict(reg, newdata = data.frame(Odometer = x))
regresjonslinjen <- data.frame(Odometer = x, fit = pred)
ggplot(cars) +
  geom_point(aes(x = Odometer, y = Price)) +
  geom_line(aes(x = Odometer, y = fit), data = regresjonslinjen) +
  ggtitle("Med estimert regresjonslinje")

# Konfidensintervall for regresjonslinjen
x <- seq(15, 50, by = 0.5)
pred <- predict(reg, newdata = data.frame(Odometer = x),
                         interval="confidence", level = 0.95)
conf_interval <- data.frame(x = x, pred)
ggplot(cars) +
  geom_point(aes(x = Odometer, y = Price)) +
  geom_line(aes(x = x, y = fit), data = conf_interval) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr),
              data = conf_interval, alpha = .3) +
  ggtitle("Med konfidensintervall for regresjonslinjen")

# Pediksjonsintervall for nye observasjoner
pred <- predict(reg, newdata = data.frame(Odometer = x),
                interval="prediction", level = 0.95)
pred_interval <- data.frame(x = x, pred)
ggplot(cars) +
  geom_point(aes(x = Odometer, y = Price)) +
  geom_line(aes(x = Odometer, y = fit), data = regresjonslinjen) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr),
              data = conf_interval, alpha = .3) +
  geom_ribbon(aes(x = x, ymin = lwr, ymax = upr),
              data = pred_interval, alpha = .3) +
    ggtitle("...og med prediksjonsidensintervall for regresjonslinjen i tillegg")


# Residualplott for ? se etter heteroskedastisitet
str(reg)

residualer <- data.frame(residualer = reg$residuals,
                         predikert = reg$fitted.values)

ggplot(residualer, aes(x = predikert, y = residualer)) +
  geom_point()

# Residualplott for ? se etter autokorrelasjon
residualer <- data.frame(residualer = reg$residuals,
                         t = 1:length(reg$residuals))

ggplot(residualer, aes(x = t, y = residualer)) +
  geom_point() +
  geom_line()

# Autokorrelasjonsplott
acf(reg$residuals)

# Histogram over residualer for ? sjekke normalitetsantakelsen.
residualer <- data.frame(residualer = reg$residuals)
ggplot(residualer, aes(x = residualer)) +
  geom_histogram()

# QQ-plott, observasjonene ligger langs en rett linje hvis de er normalfordelte
ggplot(residualer, aes(sample = residualer)) +
  stat_qq() +
  stat_qq_line()

# Plott av regresjonslinje og ulike diagnoseplott, ferdig pyntet.
x <- seq(15, 50, by = 0.5)
pred <- predict(reg, newdata = data.frame(Odometer = x))
regresjonslinjen <- data.frame(Odometer = x, fit = pred)
ggplot(cars) +
  geom_point(aes(x = Odometer, y = Price)) +
  geom_line(aes(x = Odometer, y = fit), data = regresjonslinjen) +
  ggtitle("Bildata med estimert regresjonslinje") +
  xlab("Kj?relengde") +
  ylab("Pris") +
  theme_classic()

# Residualplott
residualer <- data.frame(residualer = reg$residuals,
                         predikert = reg$fitted.values,
                         t = 1:length(reg$residuals))

ggplot(residualer, aes(x = predikert, y = residualer)) +
  geom_point() +
  xlab("Predikert verdi") +
  ylab("Observert residual") +
  ggtitle("Predikert verdi mot residual") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()

# Histogram
ggplot(residualer, aes(x = residualer)) +
  geom_histogram(bins = 8) +
  xlab("Residualer") +
  ylab("") +
  ggtitle("Histogram for observerte residualer") +
  theme_classic()

# QQ-plott
ggplot(residualer, aes(sample = residualer)) +
  stat_qq() +
  stat_qq_line() +
  xlab("Teoretiske kvantiler") +
  ylab("Observerte kvantiler") +
  ggtitle("QQ-plott") +
  theme_classic()

# Stargazer for regresjonstabell
library(stargazer)
stargazer(reg, type = "text")
stargazer(reg, type = "html", out = "regresjonstabell.html")

# Innflytelsesrike observasjoner

oslo <- data.frame(fritak = c(4, 4.5, 3.8, 5.2, 0, 6, 2.8, 7.5, 14.1),
                   lesing = c(48, 52, 53, 53.5, 50, 52, 50, 52, 65))

oslofit1 <- lm(lesing ~ fritak, data = oslo)
oslofit2 <- lm(lesing ~ fritak, data = oslo[-9,])

stargazer(oslofit1, oslofit2, type = "text")

infl <- influence.measures(oslofit1)

infl$infmat
infl$is.inf

# Lager enkelt plott av Cooks avstand
cooks_avstand <- data.frame(obs = 1:nrow(oslo),
                            cook = infl$infmat[, "cook.d"],
                            cook_infl = infl$is.inf[, "cook.d"])

ggplot(cooks_avstand, aes(x = obs, y = cook)) +
  geom_col()

## CASE: APPLE OG NETFLIX

library(readr)
library(stargazer)
library(ggplot2)
library(readxl)

# Leser datasett
nasdaq <- read_excel("Xr04-NASDAQ.xls")

# Kj?rer regresjonene
apple   <- lm(Index ~ AAPL, data = nasdaq)
netflix <- lm(Index ~ NFLX, data = nasdaq)

# Lager en pen regresjonstabell
stargazer(apple, netflix, type = "text")

# Plott av regresjonslinje og ulike diagnoseplott:
x <- seq(-0.15, 0.2, by = 0.005)
pred <- predict(apple, newdata = data.frame(AAPL = x))
regresjonslinjen <- data.frame(AAPL = x, fit = pred)
ggplot(nasdaq) +
  geom_point(aes(x = AAPL, y = Index)) +
  geom_line(aes(x = AAPL, y = fit), data = regresjonslinjen) +
  ggtitle("AAPL mot Index") +
  xlab("Apple") +
  ylab("Index") +
  theme_classic()

residualer <- data.frame(residualer = apple$residuals,
                         predikert = apple$fitted.values,
                         t = 1:length(apple$residuals))

# Residualplott
ggplot(residualer, aes(x = predikert, y = residualer)) +
  geom_point() +
  xlab("Predikert verdi") +
  ylab("Observert residual") +
  ggtitle("Predikert verdi mot residual") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic()

# Histogram
ggplot(residualer, aes(x = residualer)) +
  geom_histogram(bins = 12) +
  xlab("Residualer") +
  ylab("") +
  ggtitle("Histogram for observerte residualer") +
  theme_classic()

# QQ-plott
ggplot(residualer, aes(sample = residualer)) +
  stat_qq() +
  stat_qq_line() +
  xlab("Teoretiske kvantiler") +
  ylab("Observerte kvantiler") +
  ggtitle("QQ-plott") +
  theme_classic()

# Autokorrelasjon
acf(apple$residuals)

# Innflytelsesrike observasjoner
infl <- influence.measures(apple)

infl$is.inf

cooks_avstand <- data.frame(obs = 1:nrow(nasdaq),
                            cook = infl$infmat[, "cook.d"],
                            cook_infl = infl$is.inf[, "cook.d"])

ggplot(cooks_avstand, aes(x = obs, y = cook)) +
  geom_col()

## DEL II: Multippel regresjon

# G?r R^2 opp ved ? forklare Nasdaq-indeksen med b?de Netflix og Apple?
begge <- lm(Index ~ AAPL + NFLX, data = nasdaq)
stargazer(apple, netflix, begge, type = "text")

# La oss generere noen tilfeldige tall, og se hva som skjer:
nasdaq_ny <- data.frame(nasdaq,
                        tilfeldig = runif(n = nrow(nasdaq)))
med_random <- lm(Index ~ AAPL + NFLX + tilfeldig, data = nasdaq_ny)
stargazer(begge, med_random, type = "text")

# Last inn skoledata
skoledata <- read_excel("skoledata.xls", na = "NA")

# Perfekt autokorrelasjon: lager en ny variabel som er 2*folketall:
skoledata2 <- data.frame(skoledata,
                         folketall2 = 2*skoledata$folketall)

reg <- lm(lesing ~ folketall + folketall2, data = skoledata2)

# Tre regresjoner: en med folketall, en med antall og en med begge (+ mobbing):
reg1 <- lm(lesing ~ log(folketall), data = skoledata)
reg2 <- lm(lesing ~ log(antall), data = skoledata)
reg3 <- lm(lesing ~ log(folketall) + log(antall) + mobbing, data = skoledata)

stargazer(reg1, reg2, reg3, type = "text")

#install.packages("car")
library(car)
vif(reg3)

## DEL 3: MODELLBYGGING

library(readxl)
library(stargazer)
library(ggplot2)

# Laster inn skoledata
skoledata <- read_excel("skoledata.xls")
head(skoledata)

# Forklarer lesing vha folketall og driftsutgifter hhv.
reg1 <- lm(lesing ~ folketall, data = skoledata)
reg2 <- lm(lesing ~ driftsutgifter, data = skoledata)

# Forklarer lesing vha *b?de* f.tall og dr.utgifter
reg3 <- lm(lesing ~ folketall + driftsutgifter,
           data = skoledata)

# Lager en regresjonstabell
stargazer(reg1, reg2, reg3, type = "text")

# Noen plott
ggplot(skoledata) +
  geom_point(aes(x = folketall, y = lesing)) +
  xlab("Folketall") +
  ylab("Lesescore") +
  theme_classic()

ggplot(skoledata) +
  geom_point(aes(x = driftsutgifter, y = lesing)) +
  xlab("Driftsutgifter") +
  ylab("Lesescore") +
  theme_classic()


# log(folketall) mot lesing
ggplot(skoledata) +
geom_point(aes(x = log(folketall), y = lesing)) +
  xlab("log(Folketall)") +
  ylab("Lesescore") +
  theme_classic()

# Bruker log(folketall) som forklaringsvariabel
reg4 <- lm(lesing ~ log(folketall) + driftsutgifter,
           data = skoledata)
summary(reg4)

# Fjerner driftsutgifter, lager ny tabell
reg5 <- lm(lesing ~ log(folketall),
           data = skoledata)
stargazer(reg4, reg5, type = "text")

# Eksempel p? ? lage en ny kolonne med kvadrater i skoledatasettet:
skoledata2 <- cbind(skoledata,
                    antall_kvadert = skoledata$antall^2)

# Bruker en dummyvariabel
reg6 <- lm(lesing ~ log(folketall) + nynorsk, data = skoledata)
summary(reg6)

# Legger til interaksjonsledd
reg7 <- lm(lesing ~ log(folketall)*nynorsk, data = skoledata)
summary(reg7)

# Lager stargazertabell
stargazer(reg6, reg7, type = "text")

