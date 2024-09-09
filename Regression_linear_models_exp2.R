# Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(olsrr)

# Import data
Exp2 <- readxl::read_excel("Exp_2_totals.xlsx")
str(Exp2)
Exp2 <- Exp2 %>%
  filter(Edad1 >= 18 , TIME_total < 100 , TIME_total >= 5)
colSums(is.na(Exp2))
is.na(Exp2)
str(Exp2)
names(Exp2)
Exp2ex <- Exp2 %>%
  dplyr::select(Edad1, Genero1, NivelEducativo1, Estrato1, InfectProb1, InfectProbLoved1,
                SeverProb1, SeverProbLoved1, chooserandom1, vaccinate1, DAP111, DAP121,
                covidpositive1, living1, Vaccinated1, VAX, Solidarity,
                VSA, Disp_vac)
str(Exp2ex)
colnames(Exp2ex) <- c("Edad", "Genero", "Edu", "Estrato", "Prob_in", "Prob_in_otro", "Prob_UCI",
                      "Prob_UCI_otro", "Nudge", "Num_per_UCI", "No_free_vac", "Mand_vac",
                      "Posit_covid", "Posit_covid_otro", "Vacunado", "VAX",
                      "Solidaridad", "VSA", "Disp_vac")
names(Exp2ex)
Exp2ex[c(2,3,4,9,13,14,15)] <- lapply(Exp2ex[c(2,3,4,9,13,14,15)], factor)
sapply(Exp2ex, class)

Exp2ex <- Exp2ex %>%
  dplyr::mutate(No_free_vac_D = cut(Exp2ex$No_free_vac,
                                    breaks = c(-1, 0.1, 10000),
                                    labels = c("No", "Yes")))

model_1.2.1 <- lm(Disp_vac ~ Vacunado, data = Exp2ex)
summary(model_1.2.1) # Significativo, R^2 = .12
Exp2ex %>%
  ggplot(aes(x = Vacunado, y = Disp_vac)) +
  geom_boxplot() +
  theme_bw() # Relación negativa

# Stepwise simple linear regression based on highest increase in R^2
# Threshold as a fixed value of 0.05 (p-value)
# Y = Disposición a vacunarse, X = VAX
model_1 <- lm(Disp_vac ~ VAX, data = Exp2ex)
summary(model_1) # Significativo, R^2 = .2268
ggplot(Exp2ex, aes(x = VAX, y = Disp_vac)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "Actitud ante vacunación", y = "Disposición a vacunarse")

# Y = Disposición a vacunarse, X = VAX + Vacunado
model_2 <- lm(Disp_vac ~ VAX + Vacunado, data = Exp2ex)
summary(model_2) # Significativo, R^2 = .2778

# Y = Disposición a vacunarse, X = VAX + Vacunado + Estrato
model_3 <- lm(Disp_vac ~ VAX + Vacunado + Estrato, data = Exp2ex)
summary(model_3) # Significativo, R^2 = .3124
anova(model_3)

# Y = Disposición a vacunarse, X = VAX + Vacunado + Estrato + Solidaridad
model_4 <- lm(Disp_vac ~ VAX + Vacunado + Estrato + Solidaridad, data = Exp2ex)
summary(model_4) # Significativo, R^2 = .3311

# Y = Disposición a vacunarse, X = VAX + Vacunado + Estrato + Solidaridad
# - Probabilidad de UCI
model_5 <- lm(Disp_vac ~ VAX + Vacunado + Estrato +
                Solidaridad + Prob_UCI, data = Exp2ex) # AIC = 1528.58
summary(model_5) # Significativo, R^2 = .3415

# Y = Disposición a vacunarse, X = VAX - Vacunado + Estrato + Solidaridad
# - Probabilidad de UCI - Género
model_6 <- lm(Disp_vac ~ VAX + Vacunado + Estrato +
                Solidaridad + Prob_UCI + Genero, data = Exp2ex) # AIC = 1529.36
summary(model_6) # Significativo, R^2 = .3473

# Models comparison
models2 <- list(model_1, model_2, model_3, model_4, model_5,
               model_6, model_10)
model2.names <- c("model_1", "model_2", "model_3", "model_4", "model_5",
                 "model_6", "model_10")
aictab(cand.set = models2, modnames = model2.names)

# Stepwise backward regression
# Threshold determined by AIC
intercept_only_ex2 <- lm(Disp_vac ~ 1, data = Exp2ex)
all_ex2 <- lm(Disp_vac ~ ., data = Exp2ex)
backward_ex2 <- step(all_ex2, direction="backward", scope=formula(all_ex2), trace=0)

backward_ex2$anova

#view final model
backward_ex2$coefficients

# Y = Disposición a vacunarse, X = Genero + Estrato - Prob_in - Num_per_UCI
# - Vacunado + VAX + Solidaridad
model_7 <- lm(Disp_vac ~ Genero + Estrato + Prob_in + Num_per_UCI + 
                Vacunado + VAX + Solidaridad, data = Exp2ex)
summary(model_7) # Significativo, R^2 = .3538

# Stepwise forward regression
# Threshold determined by AIC
forward_ex2 <- step(intercept_only_ex2, direction="forward", scope=formula(all_ex2), trace=0)

forward_ex2$anova

#view final model
forward_ex2$coefficients

# Y = Disposición a vacunarse, X = VAX - Vacunado + Educación + Solidaridad
# + Estrato - Probabilidad de UCI
model_8 <- lm(Disp_vac ~ VAX + Vacunado + Edu + Solidaridad + 
                Estrato + Prob_UCI, data = Exp2ex)
summary(model_8) # Significativo, R^2 = .3543




# Using the package olsrr

# Stepwise forward regression
# Threshold determined by p-value
ols_step_forward_p(all_ex2, penter = 0.05) # AIC = 1528.9155
model_9 <- lm(Disp_vac ~ VAX + Vacunado + Solidaridad + 
                Estrato, data = Exp2ex)
summary(model_9) # Significativo, R^2 = .3311

# Stepwise backward regression
# Threshold determined by p-value
ols_step_backward_p(all_ex2, prem = 0.05) # AIC = 1528.9155
model_9 <- lm(Disp_vac ~ VAX + Vacunado + Solidaridad + 
                Estrato, data = Exp2ex)
summary(model_9) # Significativo, R^2 = .3311

#Stepwise regression
# Threshold determined by p-value
ols_step_both_p(all_ex2, penter = 0.05, prem = 0.05) # AIC = 1528.9155
model_9 <- lm(Disp_vac ~ VAX + Vacunado + Solidaridad + 
                Estrato, data = Exp2ex)
summary(model_9) # Significativo, R^2 = .3311

# Stepwise forward regression
# Threshold determined by AIC
ols_step_forward_aic(all_ex2) # AIC = 1526.773
model_8 <- lm(Disp_vac ~ VAX + Vacunado + Edu + Solidaridad + 
                Estrato + Prob_UCI, data = Exp2ex)
summary(model_8) # Significativo, R^2 = .3543

# Stepwise backward regression
# Threshold determined by AIC
ols_step_backward_aic(all_ex2) # AIC = 1525.979
model_7 <- lm(Disp_vac ~ Genero + Estrato + Prob_in + Num_per_UCI + 
                Vacunado + VAX + Solidaridad, data = Exp2ex)
summary(model_7) # Significativo, R^2 = .3538
anova(model_7)

# Proposal of final model
model_2 <- lm(Disp_vac ~ VAX + Vacunado, data = Exp2ex) # AIC = 1536.953
summary(model_2) # Significativo, R^2 = .2778
anova(model_2)
ols_step_forward_aic(model_2)
ols_step_backward_aic(model_2)

plot(model_2, 1)
plot(model_2, 2)
ols_plot_resid_hist(model_2)
ols_test_normality(model_2)
VIF(model_2)
VIF(all_ex2)



# Import data for analyses with subscales
Exp2 <- readxl::read_excel("Exp_2_totals.xlsx")
str(Exp2)
Exp2 <- Exp2 %>%
  filter(Edad1 >= 18 , TIME_total < 100 , TIME_total >= 5)
colSums(is.na(Exp2))
is.na(Exp2)
str(Exp2)
names(Exp2)
Exp2 <- Exp2 %>%
  dplyr::mutate(mistrust = VAX1 + VAX2 + VAX3, natural_immu = VAX10 + VAX11 + VAX12, commercial = VAX7 + VAX8 + VAX9,
                future_eff = VAX4 + VAX5 + VAX6)
Exp2ex <- Exp2 %>%
  dplyr::select(Edad1, Genero1, NivelEducativo1, Estrato1, InfectProb1, InfectProbLoved1,
                SeverProb1, SeverProbLoved1, chooserandom1, vaccinate1, DAP111, DAP121,
                covidpositive1, living1, Vaccinated1, Solidarity,
                VSA, Disp_vac, mistrust, natural_immu, commercial, future_eff)

colnames(Exp2ex) <- c("Edad", "Genero", "Edu", "Estrato", "Prob_in", "Prob_in_otro", "Prob_UCI",
                      "Prob_UCI_otro", "Nudge", "Num_per_UCI", "No_free_vac", "Mand_vac",
                      "Posit_covid", "Posit_covid_otro", "Vacunado",
                      "Solidaridad", "VSA", "Disp_vac", "mistrust", "natural_immu",
                      "commercial", "future_eff")

Exp2ex[c(2,3,4,9,13,14,15)] <- lapply(Exp2ex[c(2,3,4,9,13,14,15)], factor)
sapply(Exp2ex, class)


all_ex2 <- lm(Disp_vac ~ ., data = Exp2ex)

# Stepwise forward regression
# Threshold determined by AIC
ols_step_forward_aic(all_ex2) # AIC = 1498.884
model_sub_2.1 <- lm(Disp_vac ~ mistrust + Vacunado + natural_immu + Edu + 
                      Mand_vac + commercial, data = Exp2ex)
summary(model_sub_2.1) # Significativo, R^2 = .4275
anova(model_sub_2.1)

# Stepwise backward regression
# Threshold determined by AIC
ols_step_backward_aic(all_ex2) # AIC = 1498.047
model_sub_2.2 <- lm(Disp_vac ~ mistrust + Vacunado + Edu + 
                      Mand_vac + commercial, data = Exp2ex)
summary(model_sub_2.2) # Significativo, R^2 = .4272
anova(model_sub_2.2)

# Proposal of final model with subscales
model_sub_f2 <- lm(Disp_vac ~ mistrust + Vacunado + commercial, data = Exp2ex) # AIC = 1585.614
summary(model_sub_f2) # Significativo, R^2 = .4003
anova(model_sub_f2)
