## ===== Wczytywanie potrzebnych bibliotek ======
library(tidyverse)
library(rms)

## ===== Wczytanie danych do ramki danych ======
theData <- read_csv("HR-Employee-Attrition-oryginal.csv")

## ====== EDA ===============
# Sprawdzenie struktury danych
str(theData)

# Sprawdzanie zmiennych numerycznych 
names(theData)[sapply(theData, is.numeric)]

# Sprawdzanie zmiennych tekstowych
names(theData)[sapply(theData,function(x) !is.numeric(x))]

# Podsumowanie zmiennych
summary(theData)

# Sprawdzenie brakujących wartości 
anyNA(theData)

# Sprawdzanie duplikatów
sum(duplicated(theData))

# Wyświetlenie 6 pierwszych wierszy
head(theData)

# Usunięcie zbędnych stałych EmployeeCount, Over18 oraz StandardHours
theData <- theData %>% select(-EmployeeCount, -Over18, -StandardHours, -EmployeeNumber)

## ===== Zamiana języka danych =======
theData <- theData %>% 
  mutate(across(c(Department, BusinessTravel, JobRole, Gender, MaritalStatus, OverTime, Attrition), ~ case_match(
    .,
    # Zmiana nazw w kolumnie Department
    "Sales" ~ "Sprzedaż",
    "Human Resources" ~ "HR",
    "Research & Development" ~ "Badania i rozwój",
    
    # Zmiana nazw w kolumnie BusinessTravel
    "Travel_Rarely" ~ "Rzadkie podróże",
    "Travel_Frequently" ~ "Częste podróże",
    "Non-Travel" ~ "Brak podróży",
    
    # Zmiana nazw w kolumnie JobRole
    "Sales Executive" ~ "Kierownik handlowy",
    "Research Scientist" ~ "Naukowiec",
    "Laboratory Technician" ~ "Technik laboratoryjny",
    "Manufacturing Director" ~ "Dyrektor ds. produkcji",
    "Healthcare Representative" ~ "Przedstawiciel służby zdrowia",
    "Manager" ~ "Menedżer",
    "Sales Representative" ~ "Przedstawiciel handlowy",
    "Research Director" ~ "Dyrektor ds. badań",
    
    # Zmiana nazw w kolumnie Gender
    "Female" ~ "Kobieta",
    "Male" ~ "Mężczyzna",
    
    # Zmiana nazw w kolumnie MaritialStatus
    "Single" ~ "Wolny/Wolna",
    "Married" ~ "Żonaty/Zamężna",
    "Divorced" ~ "Rozwiedziony/Rozwiedziona", 
    
    # Zmiana nazw w kolumnie Attrition oraz OverTime
    "Yes" ~ "Tak",
    "No" ~ "Nie", 
    
    .default = .
  ))) %>% 
  rename(Dział = Department, PodróżeSłużbowe = BusinessTravel, Stanowisko = JobRole, Płeć = Gender,
         StanCywilny = MaritalStatus, Rotacja = Attrition, ZadowolenieZPracy = JobSatisfaction,
         MiesięcznyDochód = MonthlyIncome, Nadgodziny = OverTime, Wiek = Age, 
         ZaangażowanieWPracę = JobInvolvement, StawkaDzienna = DailyRate,
         Wykształcenie = Education, KierunekStudiów = EducationField, 
         ZadowolenieZeŚrodowiska = EnvironmentSatisfaction, 
         StawkaNaGodzinę = HourlyRate, PoziomStanowiska = JobLevel, MiesięcznaStawka = MonthlyRate,
         IlośćFirm = NumCompaniesWorked, ProcentWzrostuPensji = PercentSalaryHike,
         OcenaWydajności = PerformanceRating, ZadowolenieZRelacji = RelationshipSatisfaction, 
         PoziomOpcjiNaAkcje = StockOptionLevel, PrzepracowaneLata = TotalWorkingYears, 
         IlośćSzkoleńWZeszłymRoku = TrainingTimesLastYear, LataWFirmie = YearsAtCompany, 
         LataNaObecnymStanowisku = YearsInCurrentRole, LataOdOstatniegoAwansu = YearsSinceLastPromotion,
         LataZObecnymMenadżerem = YearsWithCurrManager, OdległośćOdDomu = DistanceFromHome)



# ============== Wizualizacja danych w celu zrozumienia zmiennych ========================
## Wiek
ggplot(theData, aes(x=Wiek)) +   geom_histogram(bins = 40, fill = "lightblue", color = "white") +
  scale_x_continuous(breaks = seq(min(theData$Wiek), max(theData$Wiek), by = 1)) +
  labs(title = "Histogram wieku", x = "Wiek", y = "Częstotliwość") +
  theme_minimal()

## Rozkład pracowników ze względu na departament 
departamenty_procentowo <- theData %>%
  count(Dział, name = "Liczba") %>%
  mutate(Procent = round((Liczba / sum(Liczba)) * 100,1))


ggplot(departamenty_procentowo, aes(x="", y=Liczba, fill=Dział)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Procent,"%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Rozkład departamentów", x = NULL, y = NULL) +
  theme_void()

## Rozkład pracowników ze względu na kierunek ukończonych studiów

edukacja_procentowo <- theData %>% 
  count(KierunekStudiów, name = "Liczba") %>% 
  mutate(Procent = round((Liczba / sum(Liczba))*100,1))

ggplot(edukacja_procentowo, aes(x="", y=Liczba, fill=KierunekStudiów)) + 
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(Procent, "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Rozkład kierunku ukończonych studiów", x = NULL, y = NULL) +
  theme_void()      

## Rozkład pracowników ze względu na stanowisko

### Tworzę ramkę danych zawierającą stanowiska i ich ilości w firmie
ilosc_danego_stanowiska <- theData %>% count(Stanowisko, name = "Ilość") %>% arrange(desc(Ilość))

### Zmieniam typ danej JobRole na czynnik oraz porządkuje na podstawie ilości wystąpień
ilosc_danego_stanowiska$Stanowisko <- factor(ilosc_danego_stanowiska$Stanowisko,
                                             levels = ilosc_danego_stanowiska$Stanowisko[order(-ilosc_danego_stanowiska$Ilość)])

ggplot(ilosc_danego_stanowiska, aes(x=Stanowisko, y=Ilość)) + geom_col(fill="lightgreen") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90")) + 
  labs(x="Stanowisko", y="Ilość obsadzonych stanowisk w firmie", 
       title="Rozkład pracowników ze względu na stanowisko")

## Zależność pomiędzy miesięcznym przychodem a zajmowanym stanowiskiem i płcią

ggplot(theData, aes(x=Stanowisko, y=MiesięcznyDochód, fill=Płeć)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "grey80"),
        panel.grid.minor = element_line(color = "grey90")) + 
  labs(x="Stanowisko", y="Miesięczny przychód", 
       title="Zależność pomiędzy miesięcznym przychodem a zajmowanym stanowiskiem i płcią")


## Zależność pomiędzy miesięcznym przychodem a zajmowanym stanowiskiem 

ggplot(theData, aes(x=MiesięcznyDochód, y=Stanowisko)) + geom_point(color="orange") + 
  theme_minimal() + labs(x="Miesięczny przychód", y="Stanowisko", 
                         title="Zależność pomiędzy miesięcznym przychodem a stanowiskiem")

## Zależność pomiędzy miesięcznym przychodem a wiekiem

ggplot(theData, aes(x=Wiek, y=MiesięcznyDochód)) + geom_point(color="purple") + 
  geom_hline(yintercept = min(theData$MiesięcznyDochód), 
             linetype = "dashed", color = "red", size = 1) +  # linia dla minimum
  geom_hline(yintercept = max(theData$MiesięcznyDochód), 
             linetype = "dashed", color = "darkgreen", size = 1) + # linia dla maximum
  theme_minimal() + labs(x="Wiek", y="Miesięczny przychód", 
                         title="Zależność pomiędzy miesięcznym przychodem a wiekiem")

## =================== Wybór zmiennych ============================== 
predyktory <- names(theData)[names(theData) != "Rotacja"]

RegLogit_1zm <- function(ramka, zmiennaY, zmienneX) {
  # Regresja logistyczna: Rotacja (Tak/Nie) z 1 zmienną objaśniającą
  # Zwraca statystyki: R2, C -pole pod krzywą ROC, Dxy
  
  formulaRegLog <- paste0(zmiennaY, " ~ ", zmienneX)
  stat_reg1zm <- data.frame(zmienneX, R2 = 0, C = 0, Dxy = 0, Brier = 0, coeff = 0, pvalue = 0)                    
  
  for (i in seq_along(zmienneX)) {
    model <- lrm(formula(formulaRegLog[i]), data = ramka)
    stats <- model[["stats"]]
    stat <- stats[c("R2", "C", "Dxy", "Brier")]
    stat <- c(stat, "coeff" = as.numeric(model$coefficients[2]))
    
    # Wyciągnij współczynniki i ich błędy standardowe
    coefs <- coef(model)
    se <- sqrt(diag(vcov(model)))
    
    # Oblicz statystyki z (statystyki Walda)
    z_stats <- coefs / se
    # Oblicz p-wartości
    p_values <- 2 * (1 - pnorm(abs(z_stats)))
    
    stat_reg1zm[i, -1] <- c(round(stat, 2), pvalue = p_values[[2]])
  }
  stat_reg1zm %>% arrange(-C)
}

wynik_reg1zm <- RegLogit_1zm(theData, "Rotacja", predyktory) %>% arrange(-R2)
reg_1zm <- wynik_reg1zm |> filter(pvalue < 0.05)

# Usuwanie nieistotnych zmiennych (I)
istotne_zmienne <- reg_1zm$zmienneX
do_usuniecia <- c("OdległośćOdDomu", "StawkaDzienna", "IlośćSzkoleńWZeszłymRoku",
                  "WorkLifeBalance")
istotne_zmienne <- setdiff(istotne_zmienne, do_usuniecia)

theData <- theData[, c("Rotacja", istotne_zmienne)]

Formula <- function(zmienneX) {
  formula(paste0("Rotacja ~ ", paste0(zmienneX, collapse = " + "), sep = " "))
}

rownanie <- Formula(istotne_zmienne)
mod_lrm <- lrm(rownanie, data = theData)
fastbw(mod_lrm)
fastbw(mod_lrm, aics = 10000)
finalne_zmienne <- c("Wiek", "LataNaObecnymStanowisku", "PodróżeSłużbowe", "ZadowolenieZPracy",
                     "ZaangażowanieWPracę", "ZadowolenieZeŚrodowiska", "MiesięcznyDochód", "StanCywilny",
                     "Nadgodziny")

theData <- theData[, c("Rotacja", finalne_zmienne)]

## ============= Grupowanie zmiennych (tworzenie zmiennych pomocniczych) ======================
# Przedziały wiekowe
theData <- theData %>%
  mutate(GrupaWiekowa = cut(Wiek, 
                            breaks = c(0, 18, 25, 35, 45, 55, 65, Inf), 
                            labels = c("0-18", "19-25", "26-35", "36-45", "46-55", "56-65", "65+"),
                            right = TRUE))

# Przedziały dochodowe
theData <- theData %>%
  mutate(GrupaDochodowa = cut(MiesięcznyDochód, 
                              breaks = c(0, 2000, 4000, 6000, 8000, 10000, Inf), 
                              labels = c("0-2000", "2001-4000", "4001-6000", "6001-8000", "8001-10000", "10000+"),
                              right = TRUE))

# Przedziały lat na obecnym stanowisku 
theData <- theData %>%
  mutate(GrupaLataNaObecnymStanowisku = cut(LataNaObecnymStanowisku, 
                                            breaks = c(-Inf, 4, 8, 12, 16, Inf), 
                                            labels = c("0-4", "5-8", "9-12", "13-16", "17+"),
                                            right = TRUE))

theData$ZadowolenieZeŚrodowiska <- case_when(
  theData$ZadowolenieZeŚrodowiska == 1 ~ "Niezadowolony",
  theData$ZadowolenieZeŚrodowiska %in% 2:3 ~ "Średnio zadowolony",
  theData$ZadowolenieZeŚrodowiska == 4 ~ "Zadowolony"
)

## =========== Zapisanie danych w pliku zewnętrznym ==================
write_csv2(theData, "HR-Employee-Attrition.csv")

