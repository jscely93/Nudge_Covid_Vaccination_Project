# Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(olsrr)

# Import data
Exp1 <- readxl::read_excel("final_data_exp1_inv_original.xlsx")
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

Exp1ex <- Exp1ex %>%
  dplyr::mutate(No_free_vac_D = cut(Exp1ex$No_free_vac,
                                    breaks = c(-1, 0.1, 10000),
                                    labels = c("No", "Yes")))

# Stepwise simple linear regression based on highest increase in R^2
# Threshold as a fixed value of 0.05 (p-value)
# Y = Disposición a vacunarse, X = VAX
model_1.1 <- lm(Disp_vac ~ VAX, data = Exp1ex)
summary(model_1.1) # Significativo, R^2 = .3945

ggplot(Exp1ex, aes(x = VAX, y = Disp_vac)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) +
  labs(x = "Actitud ante vacunación", y = "Disposición a vacunarse")

# Y = Disposición a vacunarse, X = VAX - Vacunado
model_2.1 <- lm(Disp_vac ~ VAX + Vacunado, data = Exp1ex)
summary(model_2.1) # Significativo, R^2 = .4307

# Y = Disposición a vacunarse, X = VAX - Vacunado + VSA
model_3.1 <- lm(Disp_vac ~ VAX + Vacunado + VSA, data = Exp1ex)
summary(model_3.1) # Significativo, R^2 = .453

# Y = Disposición a vacunarse, X = VAX - Vacunado + VSA + Estrato
model_4.1 <- lm(Disp_vac ~ VAX + Vacunado + VSA + Estrato, data = Exp1ex)
summary(model_4.1) # Significativo, R^2 = .4624

# Y = Disposición a vacunarse, X = VAX - Vacunado + VSA + Estrato
# + Posit_covid_otro
model_5.1 <- lm(Disp_vac ~ VAX + Vacunado + VSA +
                Estrato + Posit_covid_otro, data = Exp1ex)
summary(model_5.1) # Significativo, R^2 = .473

# Y = Disposición a vacunarse, X = VAX - Vacunado + VSA + Estrato
# + Posit_covid_otro - Genero
model_6.1 <- lm(Disp_vac ~ VAX + Vacunado + VSA +
                  Estrato + Posit_covid_otro + Genero, data = Exp1ex) # AIC = 1575.85
summary(model_6.1) # Significativo, R^2 = .4811

# Anova table
anova(lm(Disp_vac ~ VAX + Vacunado + VSA +
           Estrato + Posit_covid_otro + Genero, data = Exp1ex))

# Models comparison
models <- list(model_1.1, model_2.1, model_3.1, model_4.1, model_5.1,
                model_6.1, model_10.1)
model.names <- c("model_1.1", "model_2.1", "model_3.1", "model_4.1", "model_5.1",
                 "model_6.1", "model_10.1")
aictab(cand.set = models, modnames = model.names)


# Stepwise backward regression
# Threshold determined by AIC
intercept_only_ex1 <- lm(Disp_vac ~ 1, data = Exp1ex)
all_ex1 <- lm(Disp_vac ~ ., data = Exp1ex)
backward_ex1 <- step(all_ex1, direction="backward", scope=formula(all_ex1), trace=0)

backward_ex1$anova

#view final model
backward_ex1$coefficients

# Y = Disposición a vacunarse, X = VAX + Vacunado + Estrato + Solidaridad
# - Probabilidad de UCI - Género
model_7.1 <- lm(Disp_vac ~ Genero + Estrato + Prob_in_otro + Prob_UCI + 
                Num_per_UCI + Posit_covid + Posit_covid_otro + Vacunado + 
                  VAX + VSA, data = Exp1ex)
summary(model_7.1) # Significativo, R^2 = .4911

# Stepwise forward regression
# Threshold determined by AIC
forward_ex1 <- step(intercept_only_ex1, direction="forward", scope=formula(all_ex1), trace=0)

forward_ex1$anova

#view final model
forward_ex1$coefficients

# Y = Disposición a vacunarse, X = VAX - Vacunado + VSA + Posit_covid_otro
# - Prob_UCI + Prob_in_otro - Genero
model_8.1 <- lm(Disp_vac ~ VAX + Vacunado + VSA + Posit_covid_otro + 
                Prob_UCI + Prob_in_otro + Genero, data = Exp1ex)
summary(model_8.1) # Significativo, R^2 = .4756



# Using the package olsrr

# Stepwise forward regression
# Threshold determined by p-value
ols_step_forward_p(all_ex1, penter = 0.05) # AIC = 1575.6914
model_9.1 <- lm(Disp_vac ~ VAX + Vacunado + VSA + Posit_covid_otro, data = Exp1ex)
summary(model_9.1) # Significativo, R^2 = .4609

# Stepwise backward regression
# Threshold determined by p-value
ols_step_backward_p(all_ex1, prem = 0.05) # AIC = 1575.6914
model_9.1 <- lm(Disp_vac ~ VAX + Vacunado + VSA + Posit_covid_otro, data = Exp1ex)
summary(model_9.1) # Significativo, R^2 = .4609

#Stepwise regression
# Threshold determined by p-value
ols_step_both_p(all_ex1, penter = 0.05, prem = 0.05) # AIC = 1575.6914
model_9.1 <- lm(Disp_vac ~ VAX + Vacunado + VSA + Posit_covid_otro, data = Exp1ex)
summary(model_9.1) # Significativo, R^2 = .4609

# Stepwise forward regression
# Threshold determined by AIC
ols_step_forward_aic(all_ex1) # AIC = 1573.542
model_8.1 <- lm(Disp_vac ~ VAX + Vacunado + VSA + Posit_covid_otro + 
                  Prob_UCI + Prob_in_otro + Genero, data = Exp1ex)
summary(model_8.1) # Significativo, R^2 = .4756

# Stepwise backward regression
# Threshold determined by AIC
ols_step_backward_aic(all_ex1) # AIC = 1573.565
model_7.1 <- lm(Disp_vac ~ Genero + Estrato + Prob_in_otro + Prob_UCI + 
                  Num_per_UCI + Posit_covid + Posit_covid_otro + Vacunado + 
                  VAX + VSA, data = Exp1ex)
summary(model_7.1) # Significativo, R^2 = .4911
anova(model_7.1)


# Proposal of final model
model_2.1 <- lm(Disp_vac ~ VAX + Vacunado, data = Exp1ex) # AIC = 1585.614
summary(model_2.1) # Significativo, R^2 = .4307
anova(model_2.1)
ols_step_forward_aic(model_2.1)
ols_step_backward_aic(model_2.1)

plot(model_2.1, 1)
plot(model_2.1, 2)
ols_plot_resid_hist(model_2.1)
ols_test_normality(model_2.1)
VIF(model_2.1)
VIF(all_ex1)

# Y = Disposición a vacunarse, X = VAX + Vacunado + Estrato
model_10.1 <- lm(Disp_vac ~ VAX + Vacunado + Estrato, data = Exp1ex)
summary(model_10.1) # Significativo, R^2 = .4446
anova(model_10.1)

# Checking assumptions

model_1.1.1 <- lm(Disp_vac ~ Vacunado, data = Exp1ex)
summary(model_1.1.1) # Significativo, R^2 = .03
Exp1ex %>%
  ggplot(aes(x = Vacunado, y = Disp_vac)) +
  geom_boxplot() +
  theme_bw() # Relación negativa

model_1.1.2 <- lm(Disp_vac ~ Num_per_UCI, data = Exp1ex)
summary(model_1.1.2) # Significativo, R^2 = .03
Exp1ex %>%
  ggplot(aes(x = Num_per_UCI, y = Disp_vac)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)
  theme_bw() # Relación negativa
  
model_1.1.3 <- lm(Disp_vac ~ No_free_vac_D, data = Exp1ex)
summary(model_1.1.3) # Significativo, R^2 = .03
Exp1ex %>%
  ggplot(aes(x = No_free_vac_D, y = Disp_vac)) +
  geom_boxplot() +
  theme_bw() # Relación negativa

plot(model_6.1, 1)
plot(model_6.1, 2)

# Import data for analyses with subscales
Exp1 <- readxl::read_excel("Exp_1_totals.xlsx")
str(Exp1)
Exp1 <- Exp1 %>%
  filter(Edad1 >= 18 , TIME_total < 100 , TIME_total >= 5)
colSums(is.na(Exp1))
is.na(Exp1)
str(Exp1)
names(Exp1)
Exp1 <- Exp1 %>%
  dplyr::mutate(mistrust = VAX1 + VAX2 + VAX3, natural_immu = VAX10 + VAX11 + VAX12, commercial = VAX7 + VAX8 + VAX9,
                future_eff = VAX4 + VAX5 + VAX6)
Exp1ex <- Exp1 %>%
  dplyr::select(Edad1, Genero1, NivelEducativo1, Estrato1, InfectProb1, InfectProbLoved1,
                SeverProb1, SeverProbLoved1, chooserandom1, vaccinate1, DAP111, DAP121,
                covidpositive1, living1, Vaccinated1, Solidarity,
                VSA, Disp_vac, mistrust, natural_immu, commercial, future_eff)

colnames(Exp1ex) <- c("Edad", "Genero", "Edu", "Estrato", "Prob_in", "Prob_in_otro", "Prob_UCI",
                      "Prob_UCI_otro", "Nudge", "Num_per_UCI", "No_free_vac", "Mand_vac",
                      "Posit_covid", "Posit_covid_otro", "Vacunado",
                      "Solidaridad", "VSA", "Disp_vac", "mistrust", "natural_immu",
                      "commercial", "future_eff")

Exp1ex[c(2,3,4,9,13,14,15)] <- lapply(Exp1ex[c(2,3,4,9,13,14,15)], factor)
sapply(Exp1ex, class)


all_ex1 <- lm(Disp_vac ~ ., data = Exp1ex)

# Stepwise forward regression
# Threshold determined by AIC
ols_step_forward_aic(all_ex1) # AIC = 1555.316
model_sub_1.1 <- lm(Disp_vac ~ mistrust + natural_immu + Vacunado + VSA + 
                  Num_per_UCI + Posit_covid + commercial, data = Exp1ex)
summary(model_sub_1.1) # Significativo, R^2 = .5155
anova(model_sub_1.1)

# Stepwise backward regression
# Threshold determined by AIC
ols_step_backward_aic(all_ex1) # AIC = 1573.565
model_sub_1.2 <- lm(Disp_vac ~ mistrust + natural_immu + Vacunado + VSA + 
                  Posit_covid + Num_per_UCI + commercial + Prob_in_otro + Prob_UCI, data = Exp1ex)
summary(model_sub_1.2) # Significativo, R^2 = .5183
anova(model_sub_1.2)

# Proposal of final model with subscales
model_sub_f1 <- lm(Disp_vac ~ mistrust + Vacunado + commercial, data = Exp1ex) # AIC = 1585.614
summary(model_sub_f1) # Significativo, R^2 = .4653
anova(model_sub_f1)
