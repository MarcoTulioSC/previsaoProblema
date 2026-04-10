library(shiny)
library(dplyr)
library(ggplot2)

# Preparar dados
dados <- Abertura_de_Fichas_Ouvidoria_Solicitação_Manutenção |>
    mutate(
        data = as.Date(data, format = "%d/%m/%Y"),
        mes = format(data, "%Y-%m")
    ) |>
    filter(!is.na(tipoProblema))

# Interface
ui <- fluidPage(
    titlePanel("Previsão de Problemas com Tendência"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                "meses_futuros",
                "Meses à frente:",
                min = 1,
                max = 6,
                value = 1
            )
        ),
        
        mainPanel(
            plotOutput("grafico"),
            tableOutput("tabela")
        )
    )
)

# Servidor
server <- function(input, output) {
    
    previsao <- reactive({
        
        dados |>
            count(mes, tipoProblema) |>
            group_by(tipoProblema) |>
            arrange(mes) |>
            mutate(t = row_number()) |>
            summarise(
                modelo = list(lm(n ~ t)),
                ultimo_t = max(t),
                .groups = "drop"
            ) |>
            rowwise() |>
            mutate(
                lambda = predict(modelo, newdata = data.frame(t = ultimo_t + input$meses_futuros))
            ) |>
            ungroup() |>
            mutate(
                lambda = ifelse(lambda < 0, 0, lambda),
                prob = lambda / sum(lambda) * 100
            ) |>
            arrange(desc(lambda)) |>
            slice_head(n = 5)
    })
    
    # Gráfico
    output$grafico <- renderPlot({
        
        previsao() |>
            ggplot(aes(x = reorder(tipoProblema, lambda), y = lambda)) +
            geom_bar(stat = "identity") +
            coord_flip() +
            labs(
                title = paste("Previsão para", input$meses_futuros, "mês(es) à frente"),
                x = "Tipo de problema",
                y = "Ocorrência esperada"
            )
    })
    
    # Tabela
    output$tabela <- renderTable({
        previsao()
    })
}

# Rodar app
shinyApp(ui, server)
