# FORMA��O IA E ML - UDEMY
# Projeto 2 - Aplica��o Web: Avalia��o de cr�dito
###################################################

# Instala��o dos pacotes Shiny e Bnlearn
install.packages("shiny")
install.packages("bnlearn")
# Carregamento das bibliotecas

library(shiny)
library(bnlearn)
# Cria��o das variaveis idade, risco e modelo, absorvendo 
idade =unique(insurance$Age)
risco = unique(insurance$RiskAversion)
modelo = unique(insurance$MakeModel)

# Cria��o da interface de usu�rio, usando as fun��es do shiny
ui <- fluidPage(
        titlePanel("Avalicao Riscos Seguradora ABC"),
        sidebarLayout(
                sidebarPanel( # cria��o do painel
                        # cria��o dos inputs como idade e escolhas
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

# Cria��o do server
server <- function(input, output) {

        # Fun��o calcular 
        calcula <- function(){
                
                # Uso do high climbing 
                res <- hc(insurance)
                output$coolplot = renderPlot({plot(res)})
                
                # cria��o do fitness do modelo
                modelo <- bn.fit(res, data = insurance)
                
                # popula��o dos inputs nas vari�veis
                varidade = input$Idade
                varrisco = input$Risco
                varmodelo = input$Modelo
                
                # Valida��o dos dados e prepara��o para sa�da
                ev = paste("(Age== '", varidade, "' & RiskAversion=='", varrisco, "' & MakeModel == '", varmodelo, "')",",n=10000000", sep = "")
                
                cmd = paste("cpquery(modelo, event =( Accident=='Moderate' | Accident=='Severe' ),evidence=", ev, ")", sep = "")
                # Transforma��o e valida��o do valor
                ret = eval(parse(text = cmd))
                
                
                
                # Retorno do valor
                return(ret)
        }
        
        # Fun��o para processamento dos dados inputados
        observeEvent(input$Processar, {
                
                # Fun��o paste para interpola��o dos dados
                xc =  paste("Risco Avaliado: " ,round(calcula(), digits = 2 ))
                # Saida dos resultados
                output$Resultado = renderText({ xc})
                
        })
}
# Mostrar a interface
shinyApp(ui = ui, server = server)

