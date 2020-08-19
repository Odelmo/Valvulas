library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(influxdbr)
library(xts)
library(shinythemes)
library(ggplot2)
library(ggdark)
library(plotly)
library(ggthemes)
library(stringr)
library(extrafont)
library(ggpubr)
library(shinyWidgets)
loadfonts(device = "win")

host_influx = '10.174.12.248'
field_keys = '*'
measurement = 'Brassagem'
database = 'Valvulas'

InfluxDB_Connection <- function(host, databse, field_keys, measurement) {
    con <- influx_connection(host = host)
    
    result <- influx_select(con = con,
                            db = database,
                            field_keys = field_keys,
                            measurement = measurement)
    
    df <- as.data.frame(result)
    
    return(df)
}

Tempo_Medio_Acionamentos <- function(dataframe){
    
    df_tm <- gather(df, 'Tags','Tempo_Medio', starts_with('Tempo_Medio'))
    
    for (i in 1:nrow(df_tm)){
        df_tm$Tags[i] = str_sub(df_tm$Tags[i], start = 13)
    }
    
    df_tm <- subset(df_tm,select = c(DateTime,Tags,Tempo_Medio))
    
    return(df_tm)
    
}


df <- InfluxDB_Connection(host_influx, database, field_keys, measurement)
df <- cbind(DateTime = rownames(df), df)
df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S")

df_tm <-Tempo_Medio_Acionamentos(df)
df_tm$Tempo_Medio <- df_tm$Tempo_Medio/1000

#plotting theme for ggplot2
theme <- theme(
    text=element_text(family="Comic Sans MS"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 18, hjust = 0.5)
)

title <- tags$a(href = 'https://www.google.com',
                tags$img(src = 'ambev.png', height = '505', width = '505'), 'Teste Logo')

ui <- fluidPage(navbarPage(theme = shinytheme("darkly"),
                     'Sala de Valvulas!',
                     tabPanel('Tempo de Acionamento',
                          sidebarLayout(
                              sidebarPanel(
                                  dateInput("startdate", "Data de Inicio:", value = "2020-01-01", format = "dd-mm-yyyy",
                                            min = "2009-01-01", max = "2019-08-26"),
                                  dateInput("enddate", "Data de Fim:", value = "2020-12-31", format = "dd-mm-yyyy",
                                            min = "2009-01-02", max = "2019-08-27"),
                                  selectInput('valvula','Selecione a Valvula:',
                                              list(VOC311081 = 'VOC311081',
                                                   VOC311294 = 'VOC311294'), multiple = F),
                                  selectInput("plot.type","Tipo de Plotagem:",
                                              list(Linha = "geom_line", DotGraph = "geom_point")
                                  ),
                              ),
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel('Brassagem',
                                               h4('Acompanhamento Individual'),
                                               plotlyOutput(outputId = 'plot'),
                                               h5('Acompanhamento Geral'),
                                               plotlyOutput(outputId = 'bplot')
                                      ),
                                      tabPanel('Adegas','Trabalhando em Brassagem, Aguarde'),
                                      tabPanel('Filtração','Trabalhando em Brassagem, Aguarde')
                                  )
                                  
                              )
                          )
                 ),
                 tabPanel('Numero de Acionamentos',
                          verbatimTextOutput('acionamentos'))
                 
    )
)


server <- function(input, output) { 
    
    output$plot <- renderPlotly({
        df_plot <- filter(df_tm, Tags == input$valvula)
        p <- ggplot(data = df_plot, aes(x = DateTime,y = Tempo_Medio)) +
            geom_line() + xlab('Data') + ylab('Tempo') + ggtitle('Gráfico de Acompanhamento') +
            dark_theme_gray() + theme
        
        ggplotly(p)
    })
    
    output$bplot <- renderPlotly({
        p1 <- plot_ly() %>% 
            add_trace(data = df_tm,
                      y = ~Tempo_Medio,
                      color = ~Tags,
                      type = "box") %>% 
            layout(title = "Tempo por Valvula",
                   xaxis = list(title = "Valvulas"))
        p1
    })
}


shinyApp(ui, server)