library(shinydashboard)
library(maps)
library(ggplot2)
library(readxl)


date = unique(Covid_19$Date)
date = as.Date(date,format = '%d/%m/%Y')

nibo = as.Date(Covid_19$Date, format = '%d/%m/%Y')
Covid_19$Date = nibo


 
deptnames <- unique(Covid_19$Nom.departement[order(Covid_19$Nom.departement)])

skin <- Sys.getenv("DASHBOARD_SKIN")
skin <- tolower(skin)
if (skin == "")
    skin <- "purple"


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Tableaux", tabName = "tableaux", icon = icon("bar-chart-o")),
        menuItem("Cartes", icon = icon("th"), tabName = "cartes"),
        menuItem("Machine Learning", icon = icon("th"), tabName = "ml"),
        menuItem("Lien drole", icon = icon("file-code-o"),
                 href = "https://youtu.be/wm86z5LsxqE"
        )
    )
)

body <- dashboardBody(
    tabItems(
        tabItem("tableaux",
                fluidRow(
                    box(
                        title = "Presentation du Dataset",
                        width = 4,
                        background = "light-blue",
                        "Le dataset presente les informations relatives au covid 19 en France par departements et par jour du 18/03/2020 au 11/12/2021."
                    ),
                    
                ),
                
                fluidRow(
                    dateRangeInput("dates", label = h3("Intervalle de temps:"),
                                   start = min(date),
                                   
                                   end   = max(date),
                                   
                                   min = min(date), 
                                   max = max(date)
                                   
                                   ),
                    sidebarPanel(
                        
                        
                        selectInput("dept",
                                    
                                    "Departement:",
                                    
                                    choices = deptnames),
                        box(
                            title = "Choisir le sexe pour plus d info : ",
                            width = 4, solidHeader = TRUE,
                            selectInput("sex2", "Sexe :",
                                        list(Homme = "Homme", Femme = "Femme", Tous = "Tous")
                            )
                        )),
                    
                    mainPanel(plotOutput("plot1")),
                    
                    hr(),
                    tabBox(
                        height = 300,
                        tabPanel("Hospitalisations",
                                 plotOutput("scatter1", height = 230)
                        ),
                        tabPanel("Soins intensifs",
                                 plotOutput("scatter2", height = 230)
                        ),
                        tabPanel("Deces",
                                 plotOutput("scatter3", height = 230)
                        )
                    )
                )
        ),
        tabItem("cartes",
                fluidRow(
                    box(
                        title = "Presentation du Dataset
                        ",
                        width = 4,
                        background = "light-blue",
                        "Le dataset presente les informations relatives au covid 19 en France par departements et par jour du 18/03/2020 au 11/12/2021."
                    ),
                    titlePanel("Infos par departements"),
                    sidebarLayout(
                        
                        sidebarPanel(
                            
                            
                            selectInput("dept2",
                                        
                                        "Departement:",
                                        
                                        choices = deptnames
                                        ),
                            box(
                                title = "Choisir le sexe pour plus d info : ",
                                width = 4, solidHeader = TRUE,
                                selectInput("sex", "Sexe :",
                                             list(Homme = "Homme", Femme = "Femme", Tous = "Tous")
                                )
                            ),
                            dateInput("date", "Date :",
                                      value = "2020-03-18",
                                      min = min(date),
                                      max = max(date),
                                      
                            )
                            
                        
                        ),
                        mainPanel(
                            
                        
                            
                            plotOutput("mapPlot"),
                            
                            textOutput('text'),
                            textOutput('text2'),
                            textOutput('text3'),
                            textOutput('text4')
                            
                        )
                        
                    ),
                )
        ),tabItem("ml",
                  fluidRow(
                      box(
                          title = "Presentation du Dataset
                        ",
                          width = 4,
                          background = "light-blue",
                          "Le dataset presente les informations relatives au covid 19 en France par departements et par jour du 18/03/2020 au 11/12/2021."
                      ),
                      hr(),
                      
                      mainPanel(
                          box(
                              title = "MACHINE LEANING
                        ",
                              width = 4,
                              background = "purple",
                              "Ci dessous, le modele multi-lineaire."
                          ),
                          hr(),
                          img(src = "Rplot04.png", height = 500, width = 500),
                          hr(),
                          box(
                              title = "MACHINE LEANING
                        ",
                              width = 4,
                              background = "purple",
                              "Ci dessous, le modele lineaire simple."
                          ),
                          hr(),
                          img(src = "Rplot05.png", height = 500, width = 500),
                          hr(),
                          img(src = "Rplot07.png", height = 500, width = 500),
                      )
                  )
        )
    )
)

header <- dashboardHeader(
    title = "INFOS COVID-19 FRANCE"
)

ui <- dashboardPage(header, sidebar, body, skin = skin)

server <- function(input, output) {
    
    output$startdate <- renderText({ as.character(input$dates[1]) })
    
    output$enddate <- renderText({ as.character(input$dates[2]) })
    
    output$plot1 <- renderPlot({
        ggplot(data = Covid_19) +
            geom_line(aes(x = Date, y = Nb.actuellement.hospitalises ), 
                      color = "#09557f",
                      alpha = 0.6,
                      size = 0.6) +
            labs(x = "Date", 
                 y = "Nbr hospitalises",
                 title = "Nombre d'hospitalisation sur la periode selectionnee :") +
            theme_minimal() + xlim(input$dates[1],input$dates[2])
            
    })
    
    output$scatter1 <- renderPlot({
        ggplot(Covid_19, aes(x=date, y=Covid_19$Nb.actuellement.hospitalises[Covid_19$Sexe == input$sex2 & Covid_19$Nom.departement == input$dept]))+ geom_line(color ="blue", data = ~ subset(., Nom.departement == input$dept, Sexe == input$sex))+ xlab('Jour') + ylab('Nombre de personnes')+ xlim(input$dates[1],input$dates[2])
    })
    
    output$scatter2 <- renderPlot({
        ggplot(Covid_19, aes(x=date, y=Covid_19$Nb.actuellement.en.soins.intensifs[Covid_19$Sexe == input$sex2 & Covid_19$Nom.departement == input$dept]))+ geom_line(color = "green", data = ~ subset(., Nom.departement == input$dept, Sexe == input$sex))+ xlab('Jour') + ylab('Nombre de personnes')+ xlim(input$dates[1],input$dates[2])
    })
    
    output$scatter3 <- renderPlot({
        ggplot(Covid_19, aes(x=date, y=Covid_19$Total.Deces[Covid_19$Sexe == input$sex2 & Covid_19$Nom.departement == input$dept]))+ geom_line(color = "purple", data = ~ subset(., Nom.departement == input$dept, Sexe == input$sex)) + xlab('Jour') + ylab('Nombre de personnes')+ xlim(input$dates[1],input$dates[2])
    })
    
    output$mapPlot <- renderPlot({
        map(database = "france", fill=F, xlim=c(-5.14, 9.56), ylim=c(41.36, 51.1))
        map(database = "france", regions=input$dept2, fill=T, xlim=c(-5.14, 9.56), ylim=c(41.36, 51.1), add=F)
    })
    
    output$date <- ({ renderText({ as.character(input$date) })
        
    })
    
    output$sex <- ({ renderText({ as.character(input$sexe) })
        
    })
    
    output$sex2 <- ({ renderText({ as.character(input$sexe) })
        
    })
    

    output$text <- ({ renderText({ paste("le nombre de ", input$sex," hospitalises :",as.character(Covid_19$Nb.actuellement.hospitalises[ Covid_19$Sexe == input$sex & Covid_19$Date == input$date & Covid_19$Nom.departement == input$dept2])," le ",input$date," en ", input$dept2)})
        
    })
    
    output$text2 <- ({ renderText({ paste("le nombre de ", input$sex," en soin intensifs :",as.character(Covid_19$Nb.actuellement.en.soins.intensifs [ Covid_19$Sexe == input$sex & Covid_19$Date == input$date & Covid_19$Nom.departement == input$dept2])," le ",input$date," en ", input$dept2)})
        
    })
    
    output$text3 <- ({ renderText({ paste("le nombre de ", input$sex," retourne a domicile :",as.character(Covid_19$Total.retour.a.domicile [ Covid_19$Sexe == input$sex & Covid_19$Date == input$date & Covid_19$Nom.departement == input$dept2])," le ",input$date," en ", input$dept2)})
        
    })
    
    output$text4 <- ({ renderText({ paste("le nombre de ", input$sex," decede :",as.character(Covid_19$Total.Deces [ Covid_19$Sexe == input$sex & Covid_19$Date == input$date & Covid_19$Nom.departement == input$dept2])," le ",input$date," en ", input$dept2)})
        
    })
    
}

shinyApp(ui, server)