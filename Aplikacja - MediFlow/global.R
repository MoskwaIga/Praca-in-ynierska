####
library(tidyverse)
####

# Wczytywanie danych
data <- read_csv2("HR-Employee-Attrition.csv")

## Tworzenie zmiennych globalnych -------------------
## Wskaźnik rotacji pracowników 
attritionRate <- round((sum(data$Rotacja == "Tak") / nrow(data)) * 100, 2)

## Procent osób rotujących, które pracowały w godzinach nadliczbowych
overtimeAttrition <- data %>% 
  filter(Rotacja=="Tak") %>% 
  summarise(PercentOvertime = round(mean(Nadgodziny=="Tak")*100,0))

## Obliczanie średniej oceny pracy pracowników
workRateAvg <- data %>%
  summarise(AvgWorkRate = round(mean(ZadowolenieZPracy), 2))

## Obliczanie średniego wieku pracowników
avgAgeAttrition <- data %>% 
  summarise(AvgAge=round(mean(Wiek)))

## Obliczanie ilości rotujących osob
numberOfAttrPpl <- data %>% 
  filter (Rotacja=="Tak") %>% 
  summarise(Count = n())

## Obliczanie średniego dochodu 
avgIncome <- data %>% 
  summarise(AvgIncome = round(mean(MiesięcznyDochód),2))

## Obliczanie średniego zaangażowania 
avgInvolvement <- data %>% 
  summarise(AvgInvolvment = round(mean(ZaangażowanieWPracę),2))
# -------------
customSentence_share <- function(numItems, type) {
  paste("Love it? Share it!")
}

## -----------
dropdownMenuCustom <- function(customSentence = NULL, icon = NULL, ...) {
  items <- list(...)
  
  numItems <- length(items)
  
  tags$li(
    class = "dropdown messages-menu",  # Ustal klasę dla typu "messages"
    a(
      href = "#", 
      class = "dropdown-toggle", 
      `data-toggle` = "dropdown", 
      icon,
      tags$span(class = "label label-primary", numItems)  
    ), 
    tags$ul(
      class = "dropdown-menu", 
      tags$li(
        class = "header", 
        customSentence(numItems)
      ), 
      tags$li(
        tags$ul(class = "menu", items)
      )
    )
  )
}

## ========== zmiana typu danych ======== 
data$GrupaLataNaObecnymStanowisku <- factor(data$GrupaLataNaObecnymStanowisku,
                                            levels = c("0-4", "5-8", "9-12", "13-16", "17+"))
data$PodróżeSłużbowe <- factor(data$PodróżeSłużbowe,
                               levels = c("Brak podróży", "Rzadkie podróże", "Częste podróże"))
data$Rotacja <- factor(data$Rotacja, 
                       levels = c("Nie", "Tak"))

data$ZadowolenieZeŚrodowiska <- factor(data$ZadowolenieZeŚrodowiska,
                                       levels = c("Niezadowolony", "Średnio zadowolony", "Zadowolony"),
                                       ordered=TRUE)



# ---------------------
choices <- c(
             "Podróże służbowe" = "PodróżeSłużbowe",
             "Lata na obecnym stanowisku" = "GrupaLataNaObecnymStanowisku",
             "Zadowolenie ze środowiska" = "ZadowolenieZeŚrodowiska",
             "Grupa dochodowa" = "GrupaDochodowa",
             "Stan cywilny" = "StanCywilny",
             "Grupa wiekowa" = "GrupaWiekowa")



