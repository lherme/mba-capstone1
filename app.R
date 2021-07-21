library(shiny)
library(shinyWidgets)
library(dplyr)
library(shinythemes)
library(stringr)



#Carregamento de dados
load("cpopg.RData")

#Função para remover NAs
paste3 <- function(...,sep=", ") {
    L <- list(...)
    L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
    ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
               gsub(paste0(sep,sep),sep,
                    do.call(paste,c(L,list(sep=sep)))))
    is.na(ret) <- ret==""
    ret
}

#trata dado de distribuição

cpopg <- cpopg %>% 
    dplyr::mutate(Distribuição = str_sub(Distribuição, end = -8))


# Definição do UI
ui <- fluidPage(theme = shinytheme("united"),

    # Título do app
    h3(id="justica_simples", "Justiça Simples"), tags$style(HTML("#justica_simples{font-size: 24px}")),
    h1(id="titulo", "Busca Processo"), tags$style(HTML("#titulo{font-size: 24px}")),

    # Barra lateral
    sidebarLayout(
        sidebarPanel(
            selectInput("var", 
                        label = "Buscar por:",
                        choices = c("Número do processo", 
                                    "Nome completo"),
                        selected = "Número do processo"),
            searchInput(
                inputId = "info",
                # label = "Enter your search :",
                placeholder = "0000000-00.0000.0.00.0000",
                btnSearch = icon("search"),
                btnReset = icon("remove"),
                width = "100%"
            )
        ),

        # Mostra dados do painel principal
        mainPanel(
            textOutput("foro"),
            textOutput("sel_processo"),
            textOutput("distribuicao"),
            textOutput("nome"),
           # div(style="width:500px;",fluidRow(verbatimTextOutput("historico", placeholder = TRUE)))
           br(),
           verbatimTextOutput("historico")

        )
        
    ),
    tags$head(tags$style("#nome{font-size: 14px;
                                 font-weight: 600;
                                 }"
    )
    ),
    tags$head(tags$style("#distribuicao{font-size: 14px;
                                 font-weight: 400;
                                 }"
    )
    ),
    tags$head(tags$style("#sel_processo, #foro {font-size: 14px;
                                 font-weight: 400;
                                 }"
    )
    ),
)

# Definição do Servidor
server <- function(input, output, session) {
    
 
    output$foro <- renderText({
        if (is.element(input$info,cpopg[[2]]) == FALSE){
            paste0("")
        } else {
        paste0("Seu processo está tramitando no ", cpopg[cpopg[[2]]==input$info, 6])
        }
    })
    
    output$sel_processo <- renderText({
        if (is.element(input$info,cpopg[[2]]) == FALSE) {
            paste0("")
        } else {
        paste0("Número do processo: ", input$info)
        }
    })
    
    output$distribuicao <- renderText({
        if (is.element(input$info,cpopg[[2]]) == FALSE) {
            paste0("")
        } else {
        paste0("Seu processo foi iniciado em ", cpopg[cpopg[[2]]==input$info, 9])
        }
    })
    
    output$nome <- renderText({
        if (is.element(input$info,cpopg[[2]]) == FALSE) {
            paste0("")
        } else {
        paste0(cpopg[cpopg[[2]]==input$info, 15][[1]][[2]][1])
        }
    })
    
    # Linha temporal do processo
    output$historico <- renderPrint({
        
        if (input$info == "") {
            paste0("Aguardando preenchimento.")
        } else if (is.element(input$info,cpopg[[2]]) == FALSE) {
            paste0("Processo não encontrado.")
        } else {
            
       dt <- cpopg[cpopg[[2]]==input$info, 16]
       df_text <- c()
     
       for (i in 1:sapply(cpopg[cpopg[[2]]==input$info, 16], nrow)) {

           dt_data <- dt[[1]][i, 1]
           dt_movimento <- dt[[1]][i, 2]
           
           if (dt[[1]][i, 2] == "Decisão") {
               
               dt_descricao <- dt[[1]][i, 3]
           } else {
               dt_descricao <- NULL
           }
           
           df_text[i] <- paste3(dt_data, dt_movimento, dt_descricao)
    
       }
       
       df_text
        }
    })
    
}

# Executa o app
shinyApp(ui = ui, server = server)
