# Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(broom)
library(openintro)

# Import data
Exp1 <- readxl::read_excel("Exp_1_totals.xlsx")
str(Exp1)
Exp1 <- Exp1 %>%
  filter(Edad1 >= 18 , TIME_total < 100 , TIME_total >= 5)
colSums(is.na(Exp1))
is.na(Exp1)
str(Exp1)
names(Exp1)
Exp1ex <- Exp1 %>%
  dplyr::select(Edad1, Genero1, NivelEducativo1, Estrato1, InfectProb1, InfectProbLoved1,
                SeverProb1, SeverProbLoved1, chooserandom1, vaccinate1, DAP111, DAP121,
                covidpositive1, living1, Vaccinated1, VAX, Solidarity,
                VSA, Disp_vac)
str(Exp1ex)
colnames(Exp1ex) <- c("Edad", "Genero", "Edu", "Estrato", "Prob_in", "Prob_in_otro", "Prob_UCI",
                      "Prob_UCI_otro", "Nudge", "Num_per_UCI", "No_free_vac", "Mand_vac",
                      "Posit_covid", "Posit_covid_otro", "Vacunado", "VAX",
                      "Solidaridad", "VSA", "Disp_vac")
names(Exp1ex)
Exp1ex[c(2,3,4,9,13,14,15)] <- lapply(Exp1ex[c(2,3,4,9,13,14,15)], factor)
sapply(Exp1ex, class)


# Data visualization
dotPlot(Exp1ex$Edad, at = 1,
        xlab = 'Edad',
        ylab = '',
        pch = 20,
        col = COL[1, 3],
        cex = 2,
        xlim = c(18, 60),
        ylim = c(0.95, 1.05),
        axes = TRUE)
M <- mean(Exp1ex$Edad)
polygon(M + c(-1, 1, 0) * 1,
        c(0.95, 0.95, 0.98),
        border = COL[4],
        col = COL[4])
Exp2.1 %>%
  ggplot(aes(Edad)) +
  geom_histogram(binwidth = 5)

Exp2.1 %>%
  ggplot(aes(Genero)) +
  geom_bar()

Exp2.1 %>%
  ggplot(aes(Edu)) +
  geom_bar()

Exp2.1 %>%
  ggplot(aes(Estrato)) +
  geom_bar()

Exp2.1 %>%
  ggplot(aes(Prob_in)) +
  geom_histogram(binwidth = 5)

Exp2.1 %>%
  ggplot(aes(Prob_in_otro)) +
  geom_histogram(binwidth = 5)

Exp2.1 %>%
  ggplot(aes(Prob_UCI)) +
  geom_histogram(binwidth = 5)

Exp2.1 %>%
  ggplot(aes(Prob_UCI_otro)) +
  geom_histogram(binwidth = 5)

Exp2.1 %>%
  ggplot(aes(factor(Nudge))) +
  geom_bar()

Exp2.1 %>%
  ggplot(aes(Num_per_UCI)) +
  geom_histogram(binwidth = 5)

Exp2.1 %>%
  ggplot(aes(No_free_vac)) +
  geom_histogram(binwidth = .5) +
  scale_x_log10()

Exp2.1 %>%
  ggplot(aes(Mand_vac)) +
  geom_histogram(binwidth = .5) +
  scale_x_log10()

Exp2.1 %>%
  ggplot(aes(factor(Posit_covid))) +
  geom_bar()

Exp2.1 %>%
  ggplot(aes(factor(Posit_covid_otro))) +
  geom_bar()

Exp2.1 %>%
  ggplot(aes(factor(Vacunado))) +
  geom_bar()

Exp2.1 %>%
  ggplot(aes(Disp_vac)) +
  geom_histogram(binwidth = 5)

Exp2.1 %>%
  ggplot(aes(Solidaridad)) +
  geom_histogram(binwidth = 5)

Exp2.1 %>%
  ggplot(aes(VSA)) +
  geom_histogram(binwidth = 5)

Exp2.1 %>%
  ggplot(aes(VAX)) +
  geom_histogram(binwidth = 5)

# X = Probabilidad de UCI, Y = Disposición a vacunarse
model_1 <- lm(Disp_vac ~ Prob_UCI, data = Exp2.1)
summary(model_1) # Significativo, R^2 = .03
Exp2.1 %>%
  ggplot(aes(x = Prob_UCI, y = Disp_vac)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw() # Relación negativa

model_1.diag.metrics <- augment(model_1)
head(model_1.diag.metrics)
ggplot(model_1.diag.metrics, aes(Prob_UCI, Disp_vac)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = Prob_UCI, yend = .fitted), color = "red", size = 0.3)

plot(model_1, 1)
plot(model_1, 3)
plot(model_1, 2)
plot(model_1, 5)
plot(model_1, 4) # Cumple supuestos

model_1b <- lm(log(Disp_vac) ~ Prob_UCI, data = Exp2.1)
summary(model_1b) # Significativo, R^2 = .01
plot(model_1b, 1)
plot(model_1b, 3)
plot(model_1b, 2)
plot(model_1b, 5)
plot(model_1b, 4)

# X = Probabilidad de UCI, Y = Probabilidad infección
model_2.2 <- lm(Prob_in ~ Prob_UCI, data = Exp2.1)
summary(model_2.2) # Significativo, R^2 = .13
Exp2.1 %>%
  ggplot(aes(x = Prob_UCI, y = Prob_in)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw() # Relación positiva

#Exploring mediation effect Y = Disposición a vacunarse, X = VAX, M = Prob_UCI
model_3.2 <- lm(Disp_vac ~ Prob_in + Prob_UCI, data = Exp2.1)
summary(model_3.2) # Significativo, R^2 = .025
scatter3d(formula = Disp_vac ~ Prob_in + Prob_UCI, data = Exp2.1)

results <- mediate(model_2.2, model_3.2, treat = "Prob_UCI", 
                   mediator = "Prob_in", boot=TRUE, sims=500)
summary(results) # The mediation effect is not statistically significant

# X = VAX, Y = Disposición a vacunarse
model_1c <- lm(Disp_vac ~ VAX, data = Exp2.1)
summary(model_1c) # Significativo, R^2 = .36
ggplot(Exp2.1, aes(x = VAX, y = Disp_vac)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "Actitud ante vacunación", y = "Disposición a vacunarse")
  theme_bw()

model_1c.diag.metrics <- augment(model_1c)
head(model_1c.diag.metrics)
ggplot(model_1c.diag.metrics, aes(VAX, Disp_vac)) +
  geom_point() +
  stat_smooth(method = lm, se = FALSE) +
  geom_segment(aes(xend = VAX, yend = .fitted), color = "red", size = 0.3)

plot(model_1c, 1)
plot(model_1c, 3)
plot(model_1c, 2)
plot(model_1c, 5)
plot(model_1c, 4)  # Cumple supuestos

newExp2.1 <- Exp2.1 %>%
  dplyr::select(Disp_vac, VAX, Estrato, Vacunado,
         VSA)
newExp2.1$Estrato <- factor(Exp2.1$Estrato)
newExp2.1$Vacunado <- factor(Exp2.1$Vacunado)

plot(newExp2.1)

model_1d <- lm(Disp_vac ~ VAX + factor(Vacunado) + factor(Estrato) + VSA, data = Exp2.1)
summary(model_1d) # Significativo, R^2 = .46

# Stepwise backward regression
intercept_only <- lm(Disp_vac ~ 1, data = Exp2.1)
all <- lm(Disp_vac ~ ., data = Exp2.1)
backward <- step(all, direction='backward', scope=formula(all), trace=0)


model_1e <- lm(Disp_vac ~ Solidaridad, data = Exp2.1)
summary(model_1e)

# X = Edad, Y = Probabilidad de UCI
model_2.1 <- lm(Edad ~ Prob_UCI, data = Exp2.1)
summary(model_2.1) # No significativo, R^2 = .008
Exp2.1 %>%
  ggplot(aes(x = Prob_UCI, y = Edad)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw() # Relación positiva

# X = Nivel educativo, Y = Disposición a vacunarse
Exp2$NivelEducativo1 <- factor(Exp2$NivelEducativo1)
contrasts(Exp2$NivelEducativo1)
model.0 <- lm(disposicion_vacunarse ~ NivelEducativo1, data = Exp2)
summary(model.0) # No significativo, R^2 = .003
ggplot(Exp2, aes(x = NivelEducativo1, y = disposicion_vacunarse)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw()

ggplot(Exp2, aes(x = NivelEducativo1, y = disposicion_vacunarse)) +
  geom_boxplot()

# X = Nivel educativo, Y = Probabilidad de UCI
model.M <- lm(SeverProb1 ~ NivelEducativo1, data = Exp2)
summary(model.M) # Significativo, R^2 = .024

#Exploring mediation effect Y = Disposición a vacunarse
model.Y <- lm(disposicion_vacunarse ~ NivelEducativo1 + SeverProb1, data = Exp2)
summary(model.Y) # Significativo, R^2 = .025

results <- mediate(model.M, model.Y, treat = "NivelEducativo1", 
                   mediator = "SeverProb1", boot=TRUE, sims=500)
summary(results) # The mediation effect is not statistically significant

# X = Probabilidad de UCI, Y = Disposición a vacunarse
model.0_2 = lm(disposicion_vacunarse ~ SeverProb1, data = Exp2)
summary(model.0_2)
plot(x = Exp2$SeverProb1, y = Exp2$disposicion_vacunarse, pch = 16, col = "blue")
abline(model.0_2)
plot(model.0_2$residuals, pch = 16, col = "red")
cooks.distance(model.0_2) #looking for outliers
plot(cooks.distance(model.0_2), pch = 16, col = "blue")

# X = Probabilidad UCI, Y = Nivel educativo - Ordinal logistic regression
model.M_2 = lm(NivelEducativo1 ~ SeverProb1, data = Exp2)
summary(model.M_2)

# X = Nudge, Y = Disposición a vacunarse
Exp2$chooserandom1 <- factor(Exp2$chooserandom1)
contrasts(Exp2$chooserandom1)
model.M2 <- lm(disposicion_vacunarse ~ chooserandom1, data = Exp2)
summary(model.M2) # No significativa. Nudge tipo 1 sería más efectivo

# X = Edad, Y = Probabilidad de UCI
lmProbHos_age = lm(SeverProb1 ~ Edad1, data = Exp2)
summary(lmProbHos_age)
plot(x = Edad, y = SeverProb, pch = 16, col = "purple")
abline(lmProbHos_age) # No significativa

# X = Género, Y = Probabilidad de UCI
Exp2$Genero1 <- factor(Exp2$Genero1)
contrasts(Exp2$Genero1)
lmProbHos_gen <- lm(SeverProb1 ~ Genero1, data = Exp2)
summary(lmProbHos_gen)# No significativa

# X = Probabilidad de contagio, Y = Disposición a vacunarse
lmDispVac3 = lm(disposicion_vacunarse ~ InfectProb1, data = Exp2)
summary(lmDispVac3) # No significativa
plot(x = InfecProb, y = DispVac, pch = 16, col = "yellow")
abline(lmDispVac2)
plot(lmDispVac2$residuals, pch = 16, col = "red")

# X = Probabilidad de UCI otro, Y = Disposición a vacunarse
SeverProb2 <- Exp2$SeverProbLoved1
lmDispVac3 = lm(DispVac ~ SeverProb2, data = Exp2)
summary(lmDispVac3)
plot(x = SeverProb2, y = DispVac, pch = 16, col = "green")
abline(lmDispVac3)
plot(lmDispVac3$residuals, pch = 16, col = "red")

# X = Probabilidad de contagio otro, Y = Disposición a vacunarse
InfecProb2 <- Exp2$InfectProbLoved1
lmDispVac4 = lm(DispVac ~ InfecProb2, data = Exp2)
summary(lmDispVac4)
plot(x = InfecProb2, y = DispVac, pch = 16, col = "brown")
abline(lmDispVac4)
plot(lmDispVac4$residuals, pch = 16, col = "red")

summary(DispVac)
ggplot(Exp2, aes(x = DispVac)) +
  geom_histogram()