
library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(shinycssloaders)
library(plotly)
library(tidyr)
library(rsconnect)


source("./data_preparation.R")


server <- function(input, output, session) {
  
  
  output$zar_plot <- renderPlot({
    
    
    df_zar <-
      df_uni %>% 
      select(U_UCZELNIA_SKROT, U_E_ZAR_P5, U_ROKDYP) %>% 
      filter(!is.na(U_E_ZAR_P5)) %>%
      filter(U_ROKDYP == input$rok) %>% 
      group_by(U_UCZELNIA_SKROT) %>% 
      summarise(sredaaa = mean(U_E_ZAR_P5)) %>% 
      arrange(-sredaaa) %>% 
      head(50)
    
    df_zar <- df_zar[input$zakres_uczelni[1]:input$zakres_uczelni[2],]
    
    
    ggplot(df_zar, aes(x=reorder(factor(U_UCZELNIA_SKROT),-sredaaa), y= sredaaa ))+
      geom_col() +
      theme_bw() +
      labs(title = "Uczelnie z najwyższymi średnimi zarobkami absolwentów",
           x = "Uczelnia",
           y = "Średnie zarobki absolwentów [zł]")  +
      theme(text = element_text(size = 20))
  }, bg = "transparent")
  
  output$zar_kie_plot <- renderPlot({
   
    df_to_show <-
      df_stu %>% 
        select(P_KIERUNEK_NAZWA,P_UCZELNIA_SKROT, P_ROKDYP,P_E_ZAR_P3, P_E_ZAR_P5, P_E_ZAR)  %>% 
        filter(!(is.na(P_KIERUNEK_NAZWA) | P_KIERUNEK_NAZWA=="")) %>% 
        group_by(P_KIERUNEK_NAZWA, P_ROKDYP) %>% 
        summarise(srednia_p3 = mean(P_E_ZAR_P3, na.rm = TRUE),
                  srednia_p5 = mean(P_E_ZAR_P5, na.rm = TRUE),
                  srednia_p = mean(P_E_ZAR, na.rm = TRUE)) %>% 
        arrange(-srednia_p3) %>% 
        filter(P_KIERUNEK_NAZWA %in% input$kierunek)
  
      
      ggplot(df_to_show, aes_string(x= "P_ROKDYP", y= input$kiedy_zarobki, color = "P_KIERUNEK_NAZWA")) + 
      geom_smooth(method = "loess",size =3, se=FALSE) +
      labs(title = paste("Średnia zarobków dla danych 
                         kierunków w zależności od roku ukończenia studiów 
                         i liczby lat od otrzymania dyplomu "),
           x = "Rok ukończenia studiów", y= "Średnie zarobki absolwentów [zł]",
           color = "Kierunek studiów") +
      theme(text = element_text(size = 20),
            plot.title = element_text(hjust= 0.5))
    
    
    
    
  }, bg = "transparent")
  
  
}


ui1 <- fluidPage(titlePanel("TOPOWE UCZELNIE I KIERUNKI STUDIÓW ZE WZGLĘDU NA ZAROBKI ABSOLWENTÓW"),
            
                 splitLayout(
                   
                   verticalLayout(
                     splitLayout(
                       checkboxGroupInput(
                         inputId = "kierunek",
                         label = "Kierunek:",
                         c("Informatyka",
                           "Prawo",
                           "Logopedia",
                           "Psychologia",
                           "Ekonomia", "Filologia", "Farmacja", "Fizyka", "Historia", "Socjologia"),
                         selected = c("Informatyka")),
                       
                       radioButtons(
                         inputId = "kiedy_zarobki",
                         label = "Okres zarobkow",
                         c("Od razu po studiach" = "srednia_p",
                           "3 lata po studiach" = "srednia_p3",
                           "5 lat po studiach" = "srednia_p5"),
                         selected = c("srednia_p5"))
                     )
                     ,
       
                     plotOutput("zar_kie_plot"))
                   ,
                   
                   verticalLayout(
                     selectInput(
                       inputId = "rok",
                       label = "Rok:",
                       choices = c(
                         "2014" = "2014",
                         "2015" = "2015",
                         "2016" = "2016")),

                     
                     plotOutput("zar_plot"),
                     sliderInput("zakres_uczelni", 
                                 "Zakres:",
                                 min = 1,
                                 max = 50,
                                 value = range(1,10))
                   )
                   
                   
                   )
     
                   )

app_ui <- navbarPage(
  title = "Aplikacja Shiny",
  theme = bslib::bs_theme(bootswatch = "slate"),
  tabPanel("Top uczelnie i kierunki", ui1)
)


shinyApp(app_ui, server)

#rsconnect::deployApp("C:/Users/Uzytkownik/Desktop/IiAD/sem3/Twd/hw5/app")
#rsconnect::setAccountInfo(name='kubitam', token='35BEF4AE42A227A9E4297424D427540B', secret='+drS3DRzaJ6DGJ0AIBlVOG4ESHjRGmJoFX1UjFZk')
