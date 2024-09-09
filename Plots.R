# Load packages
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)
library(papaja)
library(gtsummary)
library(flextable)

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

Estrato_count <- Exp1ex %>%
  dplyr::count(Estrato) %>%
  dplyr::mutate(Porcentaje = round(n/sum(n) * 100))


jpeg("Estrato.jpg", width = 350, height = 350)

pl <- ggplot(data = Estrato_count, aes(x = Estrato, y = Porcentaje))
pl <- pl + geom_col(fill = "#00688B")
pl <- pl + geom_text(aes(x = Estrato, y = Porcentaje, label = paste0(Porcentaje, "%"), vjust = -0.5))
pl <- pl + theme_apa()
pl

dev.off()

niv_edu_count <- Exp1ex %>%
  dplyr::count(Edu) %>%
  dplyr::mutate(Porcentaje = round(n/sum(n) * 100))

jpeg("Nivel_Educativo.jpg", width = 350, height = 350)

pl <- ggplot(data = niv_edu_count, aes(x = Edu, y = Porcentaje))
pl <- pl + geom_col(fill = "#00688B")
pl <- pl + geom_text(aes(x = Edu, y = Porcentaje, label = paste0(Porcentaje, "%"), vjust = -0.5))
pl <- pl + theme_apa()
pl <- pl + labs(x = "Nivel Educativo")
pl <- pl + scale_x_discrete(labels = c("Secundaria", "Técnico", "Pregrado", "Posgrado"))
pl

dev.off()


head(Exp1ex)
str(Exp1ex)
levels(Exp1ex$Genero) <- c("Masculino", "Femenino", "Otro")
levels(Exp1ex$Edu) <- c("Secundaria", "Técnico", "Pregrado", "Posgrado")

sm_trial <- Exp1ex %>%
  select(Edad, Genero, Edu, Estrato)

tbl_summary_1 <- sm_trial %>%
  tbl_summary(statistic = all_continuous() ~ "{mean} ({sd})", label = c(Edu ~ "Nivel Educativo", Genero ~ "Género")) %>%
  modify_header(label ~ "**Característica**") %>%
  modify_footnote(
    all_stat_cols() ~ "M (SD); n (%)"
  )
tbl_summary_1 %>% as_flex_table() %>%
save_as_docx(tbl_summary_1, path = "demographics.docx")
tbl_summary_1