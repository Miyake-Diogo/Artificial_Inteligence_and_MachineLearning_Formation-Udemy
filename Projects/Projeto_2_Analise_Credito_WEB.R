# FORMAÇÃO IA E ML - UDEMY
# Projeto 2 - Aplicação Web: Avaliação de crédito
###################################################

# Instalação dos pacotes Shiny e Bnlearn
install.packages("shiny")
install.packages("bnlearn")
# Carregamento das bibliotecas

library(shiny)
library(bnlearn)
# Criação das variaveis idade, risco e modelo, absorvendo 
idade =unique(insurance$Age)
risco = unique(insurance$RiskAversion)
modelo = unique(insurance$MakeModel)

# Criação da interface de usuário, usando as funções do shiny
ui <- fluidPage(
        titlePanel("Avalicao Riscos Seguradora ABC"),
        sidebarLayout(
                sidebarPanel( # criação do painel
                        # criação dos inputs como idade e escolhas
                        selectInput("Idade","Selecione a Idade:",choices = idade),
                        selectInput("Risco","Gosto de Risco:",choices = risco),
                        selectInput("Modelo","Modelo do Veiculo:",choices = modelo),
                        actionButton("Processar","Processar")
                ),
                # Painel principal e suas saidas
                mainPanel(
                        
                        plotOutput("coolplot"),
                        br(),
                        
                        # Resultado
                        h1(textOutput("Resultado"))
                        
                )
        )
)
# selectInput("Condisele","Selecione Codicao Economica:",choices = condicaoeconomica)

# Criação do server
server <- function(input, output) {

        # Função calcular 
        calcula <- function(){
                
                # Uso do high climbing 
                res <- hc(insurance)
                output$coolplot = renderPlot({plot(res)})
                
                # criação do fitness do modelo
                modelo <- bn.fit(res, data = insurance)
                
                # população dos inputs nas variáveis
                varidade = input$Idade
                varrisco = input$Risco
                varmodelo = input$Modelo
                
                # Validação dos dados e preparação para saída
                ev = paste("(Age== '", varidade, "' & RiskAversion=='", varrisco, "' & MakeModel == '", varmodelo, "')",",n=10000000", sep = "")
                
                cmd = paste("cpquery(modelo, event =( Accident=='Moderate' | Accident=='Severe' ),evidence=", ev, ")", sep = "")
                # Transformação e validação do valor
                ret = eval(parse(text = cmd))
                
                
                
                # Retorno do valor
                return(ret)
        }
        
        # Função para processamento dos dados inputados
        observeEvent(input$Processar, {
                
                # Função paste para interpolação dos dados
                xc =  paste("Risco Avaliado: " ,round(calcula(), digits = 2 ))
                # Saida dos resultados
                output$Resultado = renderText({ xc})
                
        })
}
# Mostrar a interface
shinyApp(ui = ui, server = server)

