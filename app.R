library(shiny)
library(tidyverse)
library(lubridate)

##### Dados

dados = read.csv("admin_Calldetials_CallQueueStatistics.csv",
                 header = F)

### Manipulação dos dados

dados1 = dados[4:nrow(dados),]
colnames(dados1) = dados1[1,]
dados1 = dados1[-1,] %>% mutate(HORA = hour(`Tempo de Inicio`))

# Quantidade de Ligações por Hora

df_calls_hour = dplyr::count(dados1,HORA,Conectado) 

library(plyr)

# Soma aculumada de cada hora

df_calls_hour <- ddply(df_calls_hour, "HORA",
                       transform, label_ypos=cumsum(n)) 
df_calls_hour 


df_calls_hour$Conectado = ifelse(df_calls_hour$Conectado == "no","Não atendidas","Atendidas")


##### UI

ui <- fluidPage(
  
    titlePanel("Call Center - Ciro"),

    sidebarLayout(
      sidebarPanel(
        helpText("Selecione a fila e o intervalo de horário desejado"),
        
        selectInput(inputId = "fila",label = "Fila:",
                    choices = list("Todas as filas","6500","6501","6502",
                                   "6503","6504","6505",
                                   "6506","6508")),
        
        sliderInput(inputId = "horario",label = "Horário:",
                    min = 0,max = 23,value = c(7,22)),
        
        strong("Elaborado por: "),
        a("Wallyson Venâncio Ferreira",
          href = "https://api.whatsapp.com/send?phone=62992173431"),
        br(),
        strong("Email: "),
        div("wallysonvenancio@discente.ufg.br", style = "color:red")
      ),
      mainPanel(
        plotOutput(outputId = "grafico"))
      
    ))

# Define server logic required to draw a histogram
server <- function(input, output) {
    
  df = reactive({
    dplyr::count(filter(dados1,Filas == input$fila),HORA,Conectado) %>% 
      ddply("HORA",transform, label_ypos=cumsum(n)) %>%
      dplyr::mutate(Conectado = case_when(Conectado == "no" ~ "Não atendidas",
                                          TRUE ~ "Atendidas"))
  })
  
  final_df <- reactive({
    if (input$fila != "Todas as filas") return(df())
    df_calls_hour
  })
  
    
    output$grafico = renderPlot({
        
      cor_titulo = switch(input$fila,
                          "Todas as filas" = "#a6190f",
                          "6500" = "#9ba305",
                          "6501" = "#319403",
                          "6502" = "#04874a",
                          "6503" = "#059ea6",
                          "6504" = "#5209b3",
                          "6505" = "#9d05ab",
                          "6506" = "#a30359",
                          "6508" = "#080808")
      
      
        ggplot(data = final_df(),aes(x=HORA,y = n,fill = Conectado))+
          geom_bar(stat="identity")+
          geom_label(aes(label = n),
                     position = position_stack(vjust = 0.5),
                     size = 3.5,
                     colour = 'white',fontface = "bold")+
          scale_fill_manual(values=c("#126496","red"))+
          scale_x_continuous(breaks = seq(0,23,1),
                             limits = c(input$horario[1]-0.5,input$horario[2]+0.5))+
          scale_y_continuous(limits = c(0,max(final_df()$label_ypos)+10),
                             breaks = seq(0,max(final_df()$label_ypos)+10,10))+
          labs(x = "Hora",y = "Quantidade de Ligações",fill = "Ligações",
               title = input$fila)+
          theme(panel.grid.major = element_line(colour = "gray99",
                                                linewidth = 1,
                                                linetype = "blank"),
                panel.grid.minor = element_line(size = 1),
                legend.text = element_text(size = 12,colour = "gray0"),
                legend.title = element_text(size = 12),
                panel.background = element_rect(fill = "gray97"),
                legend.key = element_rect(fill = "firebrick2"),
                legend.background = element_rect(fill = "gray96"),
                legend.position = c(0.9, 0.95),
                plot.title = element_text(size = 30,
                                          face = "bold", colour = cor_titulo))


      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
