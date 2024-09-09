# Load packages
library(readxl)
library(dplyr)
library(mediation)

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

Exp1ex <- Exp1ex %>%
  dplyr::mutate(No_free_vac_D = cut(Exp1ex$No_free_vac,
                                    breaks = c(-1, 0.1, 10000),
                                    labels = c("No", "Yes")))

#Exploring mediation effect Y = Disposición a vacunarse, X = VAX, M = Prob_UCI

model.0 <- lm(Disp_vac ~ VAX, data = Exp1ex)
summary(model.0) # Significativo, R^2 = .39

model.M <- lm(Prob_UCI ~ VAX, data = Exp1ex)
summary(model.M) # Significativo

model.Y <- lm(Disp_vac ~ VAX + Prob_UCI, data = Exp1ex)
summary(model.Y) # Significativo

results <- mediate(model.M, model.Y, treat = "VAX", mediator = "Prob_UCI",
                   boot=TRUE, sims=500)
summary(results) # No significativo

#Exploring mediation effect Y = Disposición a vacunarse, X = Prob_UCI, M = VAX

model.01 <- lm(Disp_vac ~ Prob_UCI, data = Exp1ex)
summary(model.01) # Significativo, R^2 = .034

model.M1 <- lm(VAX ~ Prob_UCI, data = Exp1ex)
summary(model.M1) # Significativo

model.Y1 <- lm(Disp_vac ~ Prob_UCI + VAX, data = Exp1ex)
summary(model.Y1) # No significativo

results1 <- mediate(model.M1, model.Y1, treat = "Prob_UCI", mediator = "VAX",
                   boot=TRUE, sims=500)
summary(results1) # Significativo
# Las actitudes antivacunas median la relación entre la severidad percibida y la disposición a vacunarse.
