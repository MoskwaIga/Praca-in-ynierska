
server <- function(input, output, session) {
  
  ## I. Główny dashboard -----------------
  i_prog <- 1
  tot_step <- 25
  
  # Pasek postępu
  withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
    # Zwiększ pasek postępu i zaaktualizuj tekst
    incProgress( i_prog/tot_step, detail = NULL)
  })
  
  i_prog <- i_prog + 1
  
  ## Główne statystyki ----------------
  ## Wskaźnik rotacji
  output$attrRate <- renderText({
    paste0(attritionRate, "%")
  })
  
  output$numberOfAttrPpl <- renderText({
    paste0(numberOfAttrPpl)
  })
  
  ## Średni przychód osoby rotującej
  output$averageIncome <- renderText({
    paste0("$",avgIncome)
  })
  
  ## Wskaźnik nadgodzin
  output$overTime <- renderText({
    liczba_nadgodzin <- sum(data$Nadgodziny == "Tak")
    liczba_pracownikow <- nrow(data)
    
    # Oblicz procent pracowników w nadgodzinach
    procent_nadgodzin <- (liczba_nadgodzin / liczba_pracownikow) * 100
    
    # Zwróć wynik z dokładnością do 2 miejsc po przecinku
    paste0(round(procent_nadgodzin, 2), "%")
  })
  
  ## Średnie zadowolenie z pracy 
  output$averageSatisfacition <- renderText({
    paste0(workRateAvg)
  })
  
  ## Średnie zaangażowanie w pracę
  output$averageInvolvment <- renderText({
    paste0(avgInvolvement)
  })
  # Pasek postępu 
  withProgress(message = 'Loading...', value = (i_prog-1)/tot_step, {
    # Zwiększ pasek postępu i zaaktualizuj tekst
    incProgress( i_prog/tot_step, detail = NULL)
  })
  
  i_prog <- i_prog + 1
  
  ## W Y K R E S Y ----------------------
  get_display_name <- function(var, choices) {
    display_name <- names(choices[choices == var])
  }
  
  ## -----------------------
  ## Wykres wskaźnika rotacji
  output$attrRatePlot <- renderHighchart({
    shinyjs::runjs("$('#loading-spinner').hide();") # Ukryj spinnera po załadowaniu wykresu
    
    req(input$filter_var)  # Sprawdzanie czy zmienna jest dostępna

    attrRateData <- data %>%
      group_by(!!sym(input$filter_var)) %>%
      summarise(
        AttritionRate = round((sum(Rotacja == "Tak") / n()) * 100, 2)
      )
    
      ## W Y K R E S ------
      hchart(attrRateData, "column", hcaes(x = !!sym(input$filter_var), y = AttritionRate)) %>%
        hc_title(text = "Wskaźnik rotacji") %>%
        hc_yAxis(title = list(text = "Wskaźnik rotacji (%)"), labels = list(format = "{value}%")) %>%
        hc_xAxis(title = list(text = get_display_name(input$filter_var, choices))) %>%
        hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}%'))) %>%
        hc_colors(c("#128870")) %>%
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Wskaźnik rotacji</b>: {point.y}%') %>%
      hc_exporting(
        enabled = TRUE,
        buttons = list(
          contextButton = list(
            menuItems = c("viewFullscreen", "separator",
                          "downloadPNG", "downloadJPEG", 
                          "downloadPDF", "downloadSVG"))))
  })
  
  ## ----------------
  output$attritionCountPlot <- renderHighchart({
    req(input$filter_var, input$attritionFilter)  # Sprawdzamy, czy są wybrane zmienne
    
    # Filtrowanie danych na podstawie zaznaczonych opcji w checkboxie
    filteredData <- data %>%
      filter(Rotacja %in% input$attritionFilter) %>%
      group_by(!!sym(input$filter_var)) %>%
      summarise(Count = n()) # Liczenie pracowników
    
    # Ustawianie tytułu w zależności od wyboru checkboxów
    chartTitle <- if (length(input$attritionFilter) == 2) {
      "Liczba wszystkich pracowników"  # Kiedy zaznaczone są oba checkboxy (Tak i Nie)
    } else {
      switch(
        input$attritionFilter,
        "Tak" = "Liczba rotujących pracowników",
        "Nie" = "Liczba nierotujących pracowników"
      )
    }
    
    ## Tworzenie wykresu
    hchart(filteredData, "column", hcaes(x = !!sym(input$filter_var), y = Count)) %>%
      hc_title(text = chartTitle) %>%
      hc_yAxis(title = list(text = "Liczba pracowników")) %>%
      hc_xAxis(title = list(text = get_display_name(input$filter_var, choices))) %>%
      hc_plotOptions(column = list(dataLabels = list(enabled = TRUE, format = '{point.y}'))) %>%
      hc_colors(c("#333333")) %>%
      hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Liczba pracowników</b>: {point.y}') %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                     "downloadPNG", "downloadJPEG",
                                                                     "downloadPDF", "downloadSVG"))))
  })
  

  ## ------------------
  ## Wykres przedstawia rozrzut miesięcznego dochodu dla wybranego czynnika, 
  ## ponieważ jednym z czynników jest "Grupa dochodowa" zmienna nie może się wtedy zmieniać dla tego wykresu
  
  
  # Reaktywna zmienna do zapamiętania poprzedniego wyboru
  previous_var <- reactiveVal(NULL)
  
  observe({
    # Sprawdzamy, czy bieżąca zmienna to nie "Grupa dochodowa"
    if (input$filter_var != "GrupaDochodowa") {
      previous_var(input$filter_var)  # Aktualizujemy poprzednią zmienną, gdy wybrana jest inna niż "IncomeGroup"
    }
  })

  output$monthlyIncomeScatterPlot <- renderHighchart({
      req(input$filter_var, input$attritionFilter)
      
      # Sprawdzamy, czy wybrana zmienna to "Grupa dochodowa", jeśli tak, nie zmieniamy jej
      selected_var <- if (input$filter_var == "GrupaDochodowa") previous_var() else input$filter_var
      
      # Filtrowanie danych na podstawie zaznaczonych opcji w checkboxie
      filteredData <- data %>%
        filter(Rotacja %in% input$attritionFilter) %>% 
        select(!!sym(selected_var), MiesięcznyDochód, Rotacja)
      
      # Ustawianie tytułu w zależności od wyboru checkboxów
      chartTitle <- if (length(input$attritionFilter) == 2) {
        "Rozrzut miesięcznego dochodu - wszyscy pracownicy"  # Kiedy zaznaczone są oba checkboxy (Tak i Nie)
      } else {
        switch(
          input$attritionFilter,
          "Tak" = "Rozrzut miesięcznego dochodu - rotujący pracownicy",
          "Nie" = "Rozrzut miesięcznego dochodu - nierotujący pracownicy"
        )
      }
      
      ## Tworzenie wykresu
      hchart(filteredData, "scatter", hcaes(x = !!sym(selected_var), y = MiesięcznyDochód, group = Rotacja)) %>%
        hc_title(text = chartTitle) %>%
        hc_yAxis(title = list(text = "Miesięczny dochód")) %>%
        hc_xAxis(title = list(text = get_display_name(selected_var, choices)),
                 categories = levels(data[[selected_var]])) %>%  
        hc_legend(title = list(text = "Rotacja"), enabled = TRUE) %>%
        hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>Miesięczny dochód:</b> {point.y}') %>%
        hc_colors(c("#128870", "#333333")) %>%  # Kolory dla rotujących i nierotujących
        hc_exporting(enabled = TRUE, 
                     buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                       "downloadPNG", "downloadJPEG", 
                                                                       "downloadPDF", "downloadSVG"))))
    })
  
  
  ## -----------------
  ## Wykres pudełko-wąsy miesięcznego dochodu 
  output$monthlyIncomeBoxPlot <- renderHighchart({
    req(input$filter_var, input$attritionFilter)
    
    # Reaktywna zmienna dla "Grupa dochodowa"
    selected_var <- if (input$filter_var == "GrupaDochodowa") previous_var() else input$filter_var
    
    # Funkcja pomocnicza do obliczania statystyk dla wykresu pudełkowego
    calculate_boxplot_stats <- function(data, selected_var) {
      data %>%
        group_by(!!sym(selected_var), Rotacja) %>%
        summarise(
          min = min(MiesięcznyDochód, na.rm = TRUE),
          q1 = quantile(MiesięcznyDochód, 0.25, na.rm = TRUE),
          median = median(MiesięcznyDochód, na.rm = TRUE),
          q3 = quantile(MiesięcznyDochód, 0.75, na.rm = TRUE),
          max = max(MiesięcznyDochód, na.rm = TRUE)
        ) %>%
        ungroup()
    }
    
    # Filtrujemy dane na podstawie zaznaczonych opcji w checkboxie
    filteredData <- data %>%
      filter(Rotacja %in% input$attritionFilter)
    
    # Obliczamy dane do wykresu pudełkowego
    boxplot_data <- calculate_boxplot_stats(filteredData, selected_var)
    
    # Ustawianie tytułu w zależności od wyboru checkboxów
    chartTitle <- if (length(input$attritionFilter) == 2) {
      "Wykres pudełkowy - wszyscy pracownicy"
    } else {
      switch(
        input$attritionFilter,
        "Tak" = "Wykres pudełkowy - rotujący pracownicy",
        "Nie" = "Wykres pudełkowy - nierotujący pracownicy"
      )
    }
    
    # Tworzymy wykres pudełkowy
    hchart(boxplot_data, "boxplot", hcaes(x = !!sym(selected_var), 
                                          low = min, 
                                          q1 = q1, 
                                          median = median, 
                                          q3 = q3, 
                                          high = max, 
                                          group = Rotacja)) %>%
      hc_title(text = chartTitle) %>%
      hc_yAxis(title = list(text = "Miesięczny dochód")) %>%
      hc_xAxis(title = list(text = get_display_name(selected_var, choices)),
               categories = levels(data[[selected_var]])) %>%  
      hc_legend(title = list(text = "Rotacja"), enabled = TRUE) %>%
      hc_tooltip(
        pointFormat = '
                     <b>Mediana:</b> {point.median}<br>
                     <b>Q1:</b> {point.q1}<br>
                     <b>Q3:</b> {point.q3}<br>
                     <b>Min:</b> {point.low}<br>
                     <b>Max:</b> {point.high}'
      ) %>%
      hc_colors(c("#128870", "#333333")) %>%  # Kolory dla rotujących i nierotujących
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                     "downloadPNG", "downloadJPEG", 
                                                                     "downloadPDF", "downloadSVG"))))
  })
  
  
  ## Wykres nadgodzin - Pie Chart (Tylko Nadgodziny "Tak")
  output$overTimePlot <- renderHighchart({
    
    req(input$attritionFilter, input$filter_var)  # Wymaga zaznaczenia opcji
    
    # Filtrujemy dane w zależności od wyboru checkboxów i wybieramy tylko pracowników z nadgodzinami
    filteredData <- data %>%
      filter(Rotacja %in% input$attritionFilter, Nadgodziny == "Tak") %>%
      group_by(!!sym(input$filter_var)) %>%
      summarise(Count = n()) %>%
      mutate(Percentage = round(Count / sum(Count) * 100))  # Obliczanie procentowego udziału
    
    # Ustawiamy tytuł wykresu w zależności od wyboru rotacji
    chartTitle <- if (length(input$attritionFilter) == 2) {
      "% pracowników pracujących na nadgodzinach"
    } else {
      switch(
        input$attritionFilter,
        "Tak" = "% rotujących pracowników pracujących na nadgodziny",
        "Nie" = "% nierotujących pracowników pracujących na nadgodziny"
      )
    }
    # Ustawienie kolorów
    # Pierwszy kolor to #128870, drugi to #333333, a reszta będzie dopasowana
    customColors <- c("#128870", "#333333")
    
    # Jeśli jest więcej niż 2 kategorie, dodajemy inne kolory
    numCategories <- n_distinct(filteredData[[input$filter_var]])  # Liczba unikalnych kategorii
    if (numCategories > 2) {
      additionalColors <- RColorBrewer::brewer.pal(min(numCategories - 2, 8), "Set1")  # Dodatkowe kolory
      customColors <- c(customColors, additionalColors)  # Łączenie kolorów
    }
    
    # Tworzymy wykres kołowy
    hchart(filteredData, "pie", hcaes(x = !!sym(input$filter_var), y = Percentage)) %>%
      hc_title(text = chartTitle) %>%
      hc_tooltip(pointFormat = '<span style="color:{point.color}">●</span> <b>% pracowników</b>: {point.y}%') %>%
      hc_plotOptions(pie = list(
        dataLabels = list(enabled = TRUE, format = '{point.name}: {point.percentage:.1f}%')  # Etykieta z procentem
      )) %>%
      hc_colors(customColors) %>%  # Używamy własnej palety kolorów
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator",
                                                                     "downloadPNG", "downloadJPEG", 
                                                                     "downloadPDF", "downloadSVG"))))
  })
  
  ## ----------------------
  ## Wykres przedstawiający zadowolenie z pracy dla pracowników rotujących i nierotujących
  output$satisfactionPlot <- renderHighchart({
    req(input$filter_var, input$attritionFilter)  # Wymaga zaznaczenia opcji
    
    # Filtrujemy dane w zależności od wyboru rotacji
    filteredData <- data %>%
      filter(Rotacja %in% input$attritionFilter)
    
    # Obliczanie średniej oceny pracy zarówno dla rotujących, jak i nierotujących pracowników
    avgWorkRate <- filteredData %>%
      group_by(Rotacja, !!sym(input$filter_var)) %>%
      summarise(AvgWorkRate = round(mean(ZadowolenieZPracy), 2))
    
    # Ustawiamy tytuł wykresu w zależności od wyboru rotacji
    chartTitle <- if (length(input$attritionFilter) == 2) {
      "Średnie zadowolenie z pracy - wszyscy pracownicy"
    } else {
      switch(
        input$attritionFilter,
        "Tak" = "Średnie zadowolenie z pracy - rotujący pracownicy",
        "Nie" = "Średnie zadowolenie z pracy - nierotujący pracownicy"
      )
    }
    
    # Tworzenie heatmapy
    hchart(avgWorkRate, "heatmap", hcaes(x = !!sym(input$filter_var), y = Rotacja, value = AvgWorkRate)) %>%
      hc_title(text = chartTitle) %>%
      hc_yAxis(title = list(text = "Rotacja")) %>%
      hc_xAxis(title = list(text = get_display_name(input$filter_var, choices))) %>%
      hc_colorAxis(minColor = "#ffffff", maxColor = "#128870") %>%  # Zielony do białego
      hc_tooltip(pointFormat = '<b>Średnie zadowolenie z pracy</b>: {point.value}') %>%
      hc_legend(title = list(text = "Średnie zadowolenie z pracy")) %>%
      hc_plotOptions(heatmap = list(dataLabels = list(enabled = TRUE, format = '{point.value}'))) %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                     "downloadPNG", "downloadJPEG", 
                                                                     "downloadPDF", "downloadSVG"))))
  })

  
  ## --------------
  ## Wykres przedstawiający zaangażowanie w pracę pracowników rotujących i nierotujących
  output$involvmentPlot <- renderHighchart({
    req(input$filter_var, input$attritionFilter)  # Wymaga zaznaczenia opcji
    
    # Filtrujemy dane w zależności od wyboru rotacji
    filteredData <- data %>%
      filter(Rotacja %in% input$attritionFilter)
    
    # Obliczanie średniego zaangażowania w pracę zarówno dla rotujących, jak i nierotujących pracowników
    avgInvolvmentRate <- filteredData %>%
      group_by(Rotacja, !!sym(input$filter_var)) %>%
      summarise(AvgInvolvmentRate = round(mean(ZaangażowanieWPracę), 2))
    
    # Ustawiamy tytuł wykresu w zależności od wyboru rotacji
    chartTitle <- if (length(input$attritionFilter) == 2) {
      "Średnie zaangażowanie w pracę - wszyscy pracownicy"
    } else {
      switch(
        input$attritionFilter,
        "Tak" = "Średnie zaangażowanie w pracę - rotujący pracownicy",
        "Nie" = "Średnie zaangażowanie w pracę - nierotujący pracownicy"
      )
    }
    
    # Tworzenie heatmapy
    hchart(avgInvolvmentRate, "heatmap", hcaes(x = !!sym(input$filter_var), y = Rotacja, value = AvgInvolvmentRate)) %>%
      hc_title(text = chartTitle) %>% 
      hc_yAxis(title = list(text = "Rotacja")) %>%
      hc_xAxis(title = list(text = get_display_name(input$filter_var, choices))) %>%
      hc_colorAxis(minColor = "#ffffff", maxColor = "#128870") %>%  # Zielony do białego
      hc_tooltip(pointFormat = '<b>Średnie zaangażowanie w pracę</b>: {point.value}') %>%
      hc_legend(title = list(text = "Średnie zaangażowanie w pracę")) %>%
      hc_plotOptions(heatmap = list(dataLabels = list(enabled = TRUE, format = '{point.value}'))) %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(menuItems = c("viewFullscreen", "separator", 
                                                                     "downloadPNG", "downloadJPEG", 
                                                                     "downloadPDF", "downloadSVG"))))
  })
  
  # 2. V A L U E   B O X Y ----------
  ## Pasek postępu
  withProgress(message = "Loading...", value = (i_prog-1)/tot_step,{
    # Zwiększ pasek postępu i zaaktualizuj tekst
    incProgress(i_prog/tot_step, detail = NULL)
  })
  
  i_prog <- i_prog + 1
  

  
  ## 1. Wskaźnik rotacji
  output$attritionRate <- renderValueBox({
    valueBox(
      paste0(attritionRate, "%"), "Wskaźnik rotacji pracowników", icon = icon("rotate"), 
      color = "olive")
  })
  
  ## 2. Liczba rotujących pracowników 
  output$numberofAttrPpl <- renderValueBox({
    valueBox(
      paste0(numberOfAttrPpl$Count), "Ilość rotujących osób", icon = icon("venus-mars"), color = "teal")
  })
  
  ## 3. Liczba pracowników
  output$numberofPpl <- renderValueBox({
    
    # Liczba wszystkich pracowników w danych
    numberOfEmployees <- nrow(data)  # Liczymy wszystkie wiersze w zbiorze danych
    
    valueBox(
      paste0(numberOfEmployees), "Ilość pracowników", icon = icon("venus-mars"), color = "fuchsia"
    )
  })
  
  ## 4. Średni wiek osób rotujących 
  output$avgAttrAge <- renderValueBox({
    
    # Filtrujemy dane tylko dla rotujących pracowników
    avgAgeAttrition <- data %>%
      filter(Rotacja == "Tak") %>%  # Zakładając, że "Tak" oznacza rotujących
      summarise(AverageAge = mean(Wiek, na.rm = TRUE)) %>%  # Obliczamy średni wiek, ignorując wartości NA
      pull(AverageAge)  # Wyciągamy wartość średniego wieku
    
    valueBox(
      paste0(round(avgAgeAttrition, 0)), "Średnia wieku rotujących pracowników", icon = icon("cake-candles"),
      color = "lime"
    )
  })
  
  ## 5. Średni miesięczny dochód rotującej osoby
  output$avgAttrIncome <- renderValueBox({
    
    # Filtrujemy dane, aby wziąć pod uwagę tylko rotujących pracowników
    avgIncomeAttrition <- data %>%
      filter(Rotacja == "Tak") %>%  # Pracownicy rotujący
      summarise(AvgIncome = mean(MiesięcznyDochód, na.rm = TRUE)) %>%  # Obliczamy średni przychód
      pull(AvgIncome)  # Pobieramy wynik średniego przychodu
    
    # Wyświetlamy wynik w ValueBox
    valueBox(
      paste0("$", round(avgIncomeAttrition, 2)), "Średni miesięczny dochód rotujących pracowników",
      icon = icon("hand-holding-dollar"), color = "navy"
    )
  })
  
  ## 6. Średnie zadowolenie z pracy osoby rotującej 
  output$avgSatisfactionAttrRate <- renderValueBox({
    
    # Filtrujemy dane, aby wziąć pod uwagę tylko rotujących pracowników
    avgSatisfactionAttr <- data %>%
      filter(Rotacja == "Tak") %>%  # Pracownicy rotujący
      summarise(AvgSatisfaction = mean(ZadowolenieZPracy, na.rm = TRUE)) %>%  # Obliczamy średnie zadowolenie z pracy
      pull(AvgSatisfaction)  # Pobieramy wynik średniego zadowolenia
    
    # Wyświetlamy wynik w ValueBox
    valueBox(
      round(avgSatisfactionAttr, 2), "Średnie zadowolenie z pracy rotujących pracowników",
      icon = icon("smile"), color = "yellow"
    )
  })
  
  ## 7. Średnie zaangażowanie w pracę osoby rotującej 
  output$avgInvolvementAttrRate <- renderValueBox({
    
    # Filtrujemy dane, aby wziąć pod uwagę tylko rotujących pracowników
    avgInvolvementAttr <- data %>%
      filter(Rotacja == "Tak") %>%  # Pracownicy rotujący
      summarise(AvgInvolvement = mean(ZaangażowanieWPracę, na.rm = TRUE)) %>%  # Obliczamy średnie zaangażowanie
      pull(AvgInvolvement)  # Pobieramy wynik średniego zaangażowania
    
    # Wyświetlamy wynik w ValueBox
    valueBox(
      round(avgInvolvementAttr, 2), "Średnie zaangażowanie w pracę rotujących pracowników",
      icon = icon("users"), color = "orange"
    )
  })
  
  ## 8. % osób, które rotowały i pracowały w godzinach nadliczbowych
  output$leftAndOverWorked <- renderValueBox({
    valueBox(
      paste0(overtimeAttrition, "%"), 
      "Osoby, które opuściły firmę i pracowały w godzinach nadliczbowych",
      icon = icon("clock"), color = "purple"
    )
  })
  
  ## 9. Liczba lat na danej pozycji - osoby rotującej
  output$avgYearsAtCurrentPos <- renderValueBox({
    
    # Filtrujemy dane, aby wziąć pod uwagę tylko rotujących pracowników
    avgYearsAtPos <- data %>%
      filter(Rotacja == "Tak") %>%  # Pracownicy rotujący
      summarise(AvgYearsAtPos = mean(LataNaObecnymStanowisku, na.rm = TRUE)) %>%  # Obliczamy średnią liczbę lat
      pull(AvgYearsAtPos)  # Pobieramy wynik średniej liczby lat
    
    # Wyświetlamy wynik w ValueBox
    valueBox(
      round(avgYearsAtPos, 0), "Średnia liczba lat na obecnej pozycji rotujących pracowników",
      icon = icon("briefcase"), color = "maroon"
    )
  })
  
  ## 10. Kategoria podróży, w której rotuje najwięcej osób
  output$traveling <- renderValueBox({
    
    # Obliczamy liczbę rotujących pracowników w każdej kategorii podróży
    travelCategory <- data %>%
      filter(Rotacja == "Tak") %>%  # Filtrujemy tylko rotujących pracowników
      group_by(PodróżeSłużbowe) %>%  # Grupa według kategorii podróży
      summarise(Count = n()) %>%  # Liczymy liczbę rotujących pracowników w każdej kategorii
      arrange(desc(Count)) %>%  # Sortujemy malejąco według liczby rotujących
      slice(1)  # Wybieramy kategorię z największą liczbą rotujących
    
    # Pobieramy nazwę kategorii, w której rotuje najwięcej osób
    topCategory <- travelCategory$PodróżeSłużbowe
    
    # Wyświetlamy wynik w formularzu ValueBox
    valueBox(
      topCategory, "Kategoria podróży, w której rotuje najwięcej osób",
      icon = icon("plane"), color = "blue"
    )
  })
  
  ## 11. Kateoria zadowolenia ze środowiska, w której rotuje 
  output$environmentSatisfaction <- renderValueBox({
    # Filtrujemy dane tylko dla rotujących pracowników
    rotatingEmployees <- data %>%
      filter(Rotacja == "Tak")  # Filtrujemy tylko rotujących pracowników
    
    # Grupujemy dane po kategorii zadowolenia ze środowiska i liczymy liczbę pracowników w każdej kategorii
    satisfactionCounts <- rotatingEmployees %>%
      group_by(ZadowolenieZeŚrodowiska) %>%
      summarise(Count = n()) %>%
      arrange(desc(Count))  # Sortujemy malejąco według liczby pracowników
    
    # Wybieramy kategorię z najwyższą liczbą rotujących pracowników
    topCategory <- satisfactionCounts[1,]
    
    # Wyświetlamy kategorię z największą liczbą rotujących pracowników
    valueBox(
      topCategory$ZadowolenieZeŚrodowiska,  # Kategoria z najwyższą liczbą
      paste("Kategoria zadowolenia ze środowiska, w której rotuje najwięcej osób"),
      icon = icon("smile-beam"),  # Ikona, którą możesz zmienić według potrzeby
      color = "green"  # Kolor valueBox
    )
  })
  
  ## 12. Status związku, w którym rotuje najwięcej osób
  output$maritalStatus <- renderValueBox({
    
    # Filtrujemy dane tylko dla rotujących pracowników, grupujemy po statusie cywilnym i liczymy liczbę
    topMaritalStatus <- data %>%
      filter(Rotacja == "Tak") %>%  # Filtrujemy tylko rotujących pracowników
      group_by(StanCywilny) %>%  # Grupujemy po statusie cywilnym
      summarise(Count = n()) %>%  # Liczymy liczbę pracowników w każdej kategorii
      arrange(desc(Count)) %>%  # Sortujemy malejąco według liczby
      slice(1)  # Wybieramy pierwszą kategorię (z największą liczbą)
    
    # Wyświetlamy wynik w valueBox
    valueBox(
      topMaritalStatus$StanCywilny,  # Status cywilny z najwyższą liczbą
      "Status związku, w którym rotuje najwięcej osób",
      icon = icon("ring"),
      color = "fuchsia"
    )
  })
  
  
}

