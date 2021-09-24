# setwd("G:/Meu Drive/SRH/Acompanhamento de Processos/Shiny")

#extrafont::loadfonts(device = "win")
library(shiny)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
#import_roboto_condensed()
library(plotly)
library(lubridate)
library(shinythemes)
library(shinycssloaders)
library(stringr)
#library(stringi)
library(gghighlight)
library(tidyr)
#library(openxlsx)
library(rsconnect)

#Carregando o dado

dados <- read.csv("dados_shiny.csv", header = TRUE, encoding = 'UTF-8')
nomes <- read.csv("nomes.csv", header = TRUE, encoding = 'UTF-8')
#dados$ipo <- stri_enc_toutf8(dados$tipo, is_unknown_8bit = FALSE)

dados$dia <- as.Date(dados$dia)
dados[is.na(dados)] <- "nao.atribuido"
nomes <- rbind(nomes, c("nao.atribuido","nao.atribuido"))

dados <- merge(dados, nomes, by="cpf")

prop <- as.data.frame(ifelse(dados$nome== "nao.atribuido", "Não atribuido","Atribuido"))
names(prop) <- "atribuido"
dados <- cbind(dados, prop)

rm(nomes, prop)

#Definindo o UI

ui <- fluidPage(
  
  navbarPage(title = "Acompanhamento Processos - SRH / SEAD"), 
             theme = shinytheme("spacelab"),
  collapsible = TRUE,
  inverse = TRUE,
  windowTitle = "Acompanhamento Processos - SRH/SEAD",
  position = "fixed-top",
  fluid = FALSE,
  header = tags$style(
    ".navbar-right {
                       float: right !important;
                       }",
    "body {padding-top: 75px;}"),
  
             
  #Criando o nome da janela
    # list(tags$head(HTML('<link rel="icon", href="MyIcon.png", 
    #                                  type="image/png" />'))),
    # div(style="padding: 1px 0px; width: '100%'",
    #     titlePanel(
    #       title="", windowTitle="Acompanhamento de Processos - SRH / SEAD"
    #     )
    # ),
  
  #Dando um titulo a página
  
  # headerPanel("Acompanhamento de Processos - SRH / SEAD", windowTitle = "Acompanhamento de Processos - SRH / SEAD"),
  
  #Criando a página principal
  
  sidebarLayout(
    sidebarPanel( style = "position:fixed;width:25%;",
                  titlePanel("Parâmetros"),
                  
                  # Selecionando o período de visualização
                  dateRangeInput(inputId = "periodo",
                                 label = "Defina o período",
                                 start = min(dados$dia),
                                 end = max(dados$dia),
                                 format = "yyyy-mm-dd",
                                 startview = "month",
                                 language = "pt-BR",
                                 separator = " para ",
                                 width = NULL,
                                 min = min(dados$dia),
                                 max = max(dados$dia)),
                  
                  #Selecionando a pasta
                  
                  selectInput(inputId = "pasta",
                              label = "Escolha a pasta",
                              choices = list("SRH" = "SRH",
                                             "1ª Câmara" = "1CAMARA",
                                             "2ª Câmara" = "2CAMARA",
                                             "COPAC" = "COPAC",
                                             "CPI" = "CPI"),
                              selected = "SRH",
                              multiple = FALSE,
                              selectize = TRUE,
                              width = NULL,
                              size = NULL),
                  helpText("Obs.: Selecione nova pasta e período para atualizar as informações apresentadas"),
                  
                  hr(),
                  titlePanel("Informações do Servidor"),
                  #selecionando a pessoa 
                  
                  selectInput( inputId = "pessoa",
                               label = "Escolha o funcionário",
                               choices = NULL, 
                               selected = NULL,
                               multiple = FALSE,
                               selectize = TRUE,
                               width = NULL,
                               size = NULL),
                  
                  helpText("Obs.: Os servidores disponíveis para seleção estão de acordo com a pasta selecionada")
                  
    ),
    
    #Painel principal
    
    mainPanel(
      
      
      plotlyOutput(outputId = "pastas_boxplot", height = 450),
      helpText("Dica:  Cada quartil do gráfico de boxplot representa 25% das observações.
               Começando de baixo para cima temos: o mínimo, o 1º quartil, a mediana, 
               o 3º quartil e o valor máximo observado. Dessa forma, a mediana representa o valor
               encontrado quando os dados são dividos em 50%. Além disso, quando duas linhas estão
               próximas indica concentração dos valores naquele intervalo. 
               No caso deste gráfico, caso a mediana esteja próxima ao 3º quartil, 
               significa que a pasta observada passa mais dias com um grande 
               número de processos do que com poucos. Quando a linha é muito fina (quase não é possível 
               distinguir os quartis), significa que há pouca ou nenhuma variação. Caso haja círculos, 
               esses  representam valores incomuns (que não são frequentes). 
               - Passe o 'mouse' sobre a pasta de interesse para verificar os valores. "),
      plotlyOutput(outputId = "pastas_linha", height = 380),
      
      hr(),
      h3("Sobre os processos da pasta selecionada"),
      
      fluidRow(
        column(4,align="right",
               h4(textOutput(outputId= "prop_pasta"))
        ),
        column(width = 8,
               plotOutput(outputId = "graf_prop", height = 180))
      ),
      
      plotlyOutput(outputId = "graf_processos_pasta", height = 420),
      
      #fluidRow(
      #column(9,
      helpText("Obs.: abaixo a tabela completa dos valores do processo na pasta para a data final selecionada"),
      #),
      #column(3,
      #downloadButton(outputId = "download_excel","Baixar em Excel"),
      #)  
      #),
      
      hr(),
      DT::dataTableOutput("Tab_processos_pasta"),
      hr(),
      
      h3(paste("Sobre a distribuição de processos entre os servidores da pasta")),
      
      helpText("Obs.: Linha azul: mediana, Linha vermelha: média"),
      plotlyOutput(outputId = "pessoas_boxplot", height = 420),
      plotlyOutput(outputId = "pessoas_linha", height = 420), 
      
      hr(),
      h3(paste("Sobre os processos do servidor selecionado:")),
      br(),
      h4("Boxplot de variação da quantidade e linha de evolução temporal do servidor"),
      helpText("Obs.: Linha azul: mediana, Linha vermelha: média"),
      
      fluidRow( column(4,
                       plotlyOutput(outputId = "pessoa_boxplot", height = 350)
      ),
      column(8,
             plotlyOutput(outputId = "pessoa_linha", height = 350))
      
      )
      
    )
  )
)
  


server <- function(input, output, session){
  
  #Selecionando o período 
  periodo_finder <- reactive({
    req(input$periodo)
    periodo_finder <- as.Date(input$periodo, format="%Y-%b-%d")
    periodo_finder
  })
  
  #Selecionando os dados de acordo com a pasta e periodo informado
  
  d.pasta_finder <- reactive({
    req(input$pasta)
    req(input$periodo)
    
    filter(dados, pasta == input$pasta)%>%
      filter(dia >= periodo_finder()[1], dia <= periodo_finder()[2])
  })
  
  observeEvent (d.pasta_finder(), {
    choices <- unique(d.pasta_finder()$nome)
    updateSelectInput(inputId = "pessoa", choices = choices)
  })
  
  nome_pessoa <- reactive({
    req(input$pessoa)
    filter(d.pasta_finder(), nome == input$pessoa)
  })
  
  #Selecionando o Funcionário
  pessoa_finder <- reactive({
    req(input$pessoa)
    pessoa_finder <- input$pessoa
    pessoa_finder
  })
  
  #Organizando o conjunto de dados com os processos atribuidos
  
  dados_finder <- reactive({
    req(input$pasta)
    req(input$periodo)
    
    dados%>%
      filter(dia >= periodo_finder()[1], dia <= periodo_finder()[2])%>%
      #filter(nome!="nao.atribuido")%>%
      group_by(dia, pasta)%>%
      count()%>%
      arrange(desc(n))
  })
  
  #Boxplot dos processos por pasta
  
  output$pastas_boxplot <-  renderPlotly({
    
    input$pasta
    input$periodo
    
    isolate({
      
      dados_finder()%>%
        mutate( type= ifelse(pasta == input$pasta, "Highlighted","Normal")) %>%
        ggplot(aes(x=pasta, y=n, fill=type, alpha=type))+
        geom_boxplot()+
        scale_fill_manual(values = c("olivedrab1","snow3"))+
        scale_alpha_manual(values = c(1,0.01))+
        theme_ipsum(base_family = "Arial")+
        theme(legend.position = "none", 
              plot.title = element_text(size=12))+
        labs(title = "Variação da quantidade de processos por pasta
             ao longo do período selecionado", x="", y="" )
      
    })
  })
  
  # Gráfico de linha temporal
  
  output$pastas_linha <-  renderPlotly({
    req(input$pasta)
    req(input$periodo)
    
    isolate({
      
      pasta_selecionada <- dados_finder()%>%
        select(dia, pasta, n) %>%
        filter(pasta == input$pasta)
      
      pasta_outras <- dados_finder()%>%
        select(dia, pasta, n) %>%
        filter(pasta != input$pasta)
      
      ggplot()+
        geom_line(data=pasta_selecionada, aes(x=dia, y=n, group=pasta), colour="midnightblue", lwd=1.5)+
        geom_line(data=pasta_outras, aes(x=dia, y=n, group=pasta, colour=as.factor(pasta)))+
        scale_color_grey() +
        geom_point(data=pasta_selecionada, aes(x=dia, y=n, group=pasta), colour="midnightblue", size= 2)+
        xlab("")+
        ylab("")+
        theme_ipsum(base_family="Arial")+
        theme(axis.text.x = element_text(angle = 45, hjust=0, family = "Arial"), 
              legend.position = "top",
              plot.title = element_text(size = 12))+
        labs(title = paste("Evolução dos processos da pasta",
                           input$pasta,
                           "ao longo do período \n"), 
             colour= "Outras pastas")
      
    })
  })
  
  #Calculando a proporção
  
  c.prop_pasta <- reactive({
    input$pasta
    input$periodo
    
    num <- d.pasta_finder()%>%
      filter(nome != "nao.atribuido")%>%
      filter(dia == periodo_finder()[2])%>%
      count(nome)
    
    num2 <- d.pasta_finder()%>%
      filter(dia == periodo_finder()[2])
    
    prop <- round(sum(num$n)/length(num2$cpf) * 100,2)
    prop
  })
  
  # Texto informativo da proporção
  output$prop_pasta <- renderText(paste(" De acordo com a última data informada, ",
                                        format(as.Date(parse_date_time(periodo_finder()[2], "Y%-m%-d%")),"%d %B"),", ", c.prop_pasta(),"% dos processos da",
                                        input$pasta, "estão atribuidos entre os servidores.") )
  # Gráfico da proporção
  output$graf_prop <- renderPlot({
    input$pasta
    input$periodo
    
    num2 <- d.pasta_finder()%>%
      filter(dia == periodo_finder()[2])
    
    prop <- as.data.frame(ifelse(num2$nome== "nao.atribuido", "Não atribuido","Atribuido"))
    
    names(prop) <- "Processos"
    
    prop <- as.data.frame(table(prop$Processos))
    
    ggplot(prop, aes(y=Freq,x="", fill=Var1, order=Var1))+
      geom_bar(position = position_fill(reverse = TRUE), stat="identity")+
      scale_fill_manual(values=c("midnightblue", "lightsteelblue"))+
      #scale_y_continuous(labels = scales::percent())+
      coord_flip()+
      ylab("Pontos percentuais")+
      xlab("")+
      theme_ipsum(base_family = "Arial")+
      theme(legend.position = "top")+
      labs(fill="Processos")
    
    
    
  })
  
  #Criando a tabela dos processos por pasta
  
  tab_processos_pasta <- reactive({
    req(input$pasta)
    req(input$periodo)
    
    d.tipo.p <-  dados%>% 
      filter(pasta == input$pasta)%>%
      filter(dia == periodo_finder()[2])%>%
      group_by(tipo, atribuido)%>%
      reshape2::dcast(formula=  tipo ~ atribuido)
    
    d.tipo.p$total <- apply(d.tipo.p[,2:3], 1, sum)
    
    d.tipo.p <- d.tipo.p%>%
      arrange(desc(total))
    
    d.tipo.p
    
    
    
  })
  
  #Gráfico de Barras tipos de processo
  
  output$graf_processos_pasta <- renderPlotly({
    input$pasta
    input$periodo
    
    isolate({
      
      grafico <-  tab_processos_pasta()[1:5,1:4] %>% 
        pivot_longer(cols = c(Atribuido,`Não atribuido`), names_to = 'type', values_to = 'qte') %>% 
          ggplot(aes(x = qte, 
                   y = reorder(tipo, qte, function(x){ sum(x) }),
                   fill = type)) +
        geom_col(position=position_stack(reverse = TRUE), width = 0.75)+
        labs(x = ' ', y = ' ') +
        scale_fill_manual(values = c('midnightblue','lightsteelblue'),
                          labels = c('Atribuído','Não atribuído'),
                          name = '') +
        theme_ipsum(base_family = "Arial")+
        theme(legend.position = 'top', axis.text.y = element_text(size = rel(.85)),
              plot.title = element_text(size = 12))+
        scale_y_discrete(labels = function(y) str_wrap(y, width = 50))+
        labs(title=paste("Top 5 processos mais frequentes \nna pasta", input$pasta, "em", format(as.Date(parse_date_time(periodo_finder()[2], "Y%-m%-d%")),"%d %B")))
        
      ggplotly(grafico, tooltip = "x")
        
    })
  })
  
  # Output da tabela 
  
  output$Tab_processos_pasta <- DT::renderDataTable( server = FALSE,
                                                     DT:: datatable(tab_processos_pasta(),
                                                                    extensions = c('KeyTable','Buttons'),
                                                                    options=list(pageLength = 10,
                                                                                 dom="lftprB",
                                                                                 buttons = list(
                                                                                   list(extend = "pdf", text = "Baixar em Pdf", filename = "tabela",
                                                                                        exportOptions = list(
                                                                                          modifier = list(page = "all")
                                                                                        )
                                                                                   ),
                                                                                   list(extend = "excel", text = "Baixar em Excel", filename = "tabela",
                                                                                        exportOptions = list(
                                                                                          modifier = list(page = "all")
                                                                                        ),
                                                                                        keys=TRUE
                                                                                   )
                                                                                 )
                                                                    )
                                                     )
                                                     
  )
  
  #Baixando a tabela
  
  #output$download_excel <- downloadHandler(
  #  
  #  filename = function(){paste("qtd_processos_",input$periodo_finder()[2],"_",input$pasta,".xlsx", sep = "")},
  #  content = function(con){
  #    writexl::write.xlsx(tab_processos_pasta(), con)
  #  }
  #)
  
  #Dados Boxplot e linhas 
  
  BL.P_finder <- reactive({
    
    req(input$pasta)
    req(input$periodo)
    
    filter(dados, pasta %in% input$pasta)%>%
      filter(dia >= periodo_finder()[1], dia <= periodo_finder()[2])%>%
      filter(nome!="nao.atribuido")%>%
      group_by(dia, nome)%>%
      count()%>%
      arrange(dia)
    
  })
  
  #Gráfico de Boxplot dos servidores
  
  output$pessoas_boxplot <-  renderPlotly({
    
    input$pasta
    input$periodo
    
    isolate({
      BL.P_finder()%>%
        select(dia, nome, n)%>%
        
        ggplot()+
        geom_boxplot(aes(x=as.factor(nome), y=n))+
        theme_ipsum(base_family = "Arial")+
        theme(axis.text.x = element_text(angle = 45, size = rel(.75)), 
              legend.position = "none",
              plot.title = element_text(size = 12))+
        labs(title="Variação da quantidade de processos por pessoa", 
             x="", y="")+
        geom_hline(yintercept=mean(BL.P_finder()$n), col="red", show.legend = TRUE)+
        geom_hline(yintercept = median(BL.P_finder()$n), col="blue", show.legend = TRUE)
    })
  })
  
  output$pessoas_linha <- renderPlotly({
    input$pasta
    input$periodo
    
    isolate({
      ggplot(data=BL.P_finder(), aes(x=dia, y=n, group=nome))+
        geom_point(aes(colour= as.factor(nome)))+
        geom_line(aes(colour= as.factor(nome)), lwd=1)+
        geom_hline(yintercept = mean(BL.P_finder()$n), size=.5, col="red")+
        geom_hline(yintercept = median(BL.P_finder()$n), size=.5, col="blue")+
        xlab("Data")+
        ylab("Quantidade")+
        theme_ipsum(base_family="Arial")+
        theme(axis.text.x = element_text(angle = 45, hjust=0, family = "Arial"), 
              plot.title = element_text(size = 12))+
        labs(title = "Evolução dos processos atribuidos ao longo do período ", 
             color="Servidor ")
      
    })
  })
  
  # Boxplot pessoa
  
  output$pessoa_boxplot <-  renderPlotly({
    
    input$pasta
    input$periodo
    
    ev.pessoa <- BL.P_finder()%>%
      filter(nome == input$pessoa)
    
    isolate({
      ev.pessoa%>%
        ggplot(aes(x=as.factor(nome), y=n))+
        geom_hline(yintercept=mean(BL.P_finder()$n), col="red")+
        geom_hline(yintercept = median(BL.P_finder()$n), col="blue")+
        geom_boxplot()+
        labs(x="", y="")+
        theme_ipsum(base_family = "Arial")+
        theme(axis.text.x = element_text(angle= 45, size = rel(.8)), 
              legend.position = "none")
      })
  })
  
  
  # Linha temporal pessoa
  
  output$pessoa_linha <- renderPlotly({
    input$pasta
    input$periodo
    
    ev.pessoa <- BL.P_finder()%>%
      filter(nome == input$pessoa)
    
    isolate({
     ev.pessoa%>%
        ggplot(aes(x=dia, y=n, group=nome))+
        geom_point(aes(colour= as.factor(nome)))+
        geom_line(aes(colour= as.factor(nome)), lwd=1)+
        geom_hline(yintercept = mean(BL.P_finder()$n), size=.5, col="red")+
        geom_hline(yintercept = median(BL.P_finder()$n), size=.5, col="blue")+
        labs(color="Servidor ", x="", y="")+
        theme_ipsum(base_family="Arial")+
        theme(axis.text.x = element_text(angle = 45, family = "Arial"))
        
    })
  })
  
  
  
  
  
}

shinyApp(ui=ui, server=server)
