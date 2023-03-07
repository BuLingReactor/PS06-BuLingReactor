#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)

dataset <- read_delim("human-development-index-vs-gdp-per-capita.csv")
names(dataset)[names(dataset) == "Human Development Index"] <- "HDI"
names(dataset)[names(dataset) == "Population (historical estimates)"] <- "Population"

dataset <- dataset %>%
    group_by(Code) %>%
    fill(Continent, .direction = "up") %>%
    fill(Continent) %>%
    ungroup() %>%
    group_by(Year)

dataset <- subset(dataset, complete.cases(HDI, `GDP per capita, PPP (constant 2017 international $)`))

# Define UI for application that draws a histogram
ui <- fluidPage(
        tabsetPanel(
            tabPanel("About", tabPanel(
                "Conclusion",
                HTML("<ul>
                  <li>This app is about <b>GDP per capita</b>, <b>Human Development Index(HDI)</b> in different countries 
                  across the world. through the years.</li>
                  <li>These data are from <i>1990</i> to <i>2020</i> </li>
                </ul>"),
                textOutput("textMain"),
                tableOutput("tableMain"))),
            tabPanel("Plot", mainPanel(
                titlePanel("GDP per capita vs HDI"),
                sidebarLayout(
                    sidebarPanel(
                        checkboxInput("lineShow", "Show trend line"),
                        sliderInput("Year", "Year",
                                    min = 1990,
                                    max = 2020,
                                    value = 2020),
                        
                        sliderInput("Population", "Population more than (10^n)",
                                    min = 0,
                                    max = 9,
                                    step = 0.1,
                                    value = 5)
                    ),
                         
                         # Show a plot of the generated distribution
                    mainPanel(textOutput("textPlot"), plotOutput("distPlot")),
                )
            )),
            tabPanel("Table", mainPanel(
                titlePanel("HDI and GDP per capita for each contients cross the years"),
                sidebarLayout(
                    sidebarPanel(
                        sliderInput("yearTable", "Year",
                                    min = 1990,
                                    max = 2020,
                                    value = c(1990, 2020)),
                        
                        selectInput("Variable", "Calc mean by",
                                    c("Continent", "Year"))
                    ),
                    mainPanel(tableOutput("table"),
                    textOutput("textPlotTable"))
                )
            ))
    ),
    
    # Application title
    
    # Sidebar with a slider input for number of bins 
)

# Define server logic required to draw a histogram
library(ggrepel)
server <- function(input, output) {
    
    output$textMain <- renderText(
        paste("The dataset contains", nrow(dataset), "records of entities and has",
              ncol(dataset), "variables.\n Here's a small sample of the data:")
    )
    
    output$tableMain <- renderTable({
        dataset %>% ungroup() %>% sample_n(5)
    })
    
    output$textPlot <- renderText({
        dataset <- dataset %>%
            filter(Population >= 10 ^ input$Population, Year == input$Year)
        
        paste("Data in year", input$Year, ", showing", nrow(dataset),
              "entities that population more than", as.integer(10 ^ input$Population))
    })
    
    output$textPlotTable <- renderText({
        dataset %>%
            group_by(!!sym(input$Variable)) %>%
            filter(!is.na(Continent), Year >= input$yearTable[1],
                   Year <= input$yearTable[2])
        
        paste("Data from year", input$yearTable[1], "to", input$yearTable[2],
              "using data from", length(unique(dataset$Entity)), "entities")
    })
    
    output$table <- renderTable({
        dataset$Year <- as.integer(dataset$Year)
        dataset %>%
            group_by(!!sym(input$Variable)) %>%
            filter(!is.na(Continent), Year >= input$yearTable[1],
                   Year <= input$yearTable[2]) %>%
            summarise(meanHDI = weighted.mean(HDI, w = Population),
                      meanGDP = weighted.mean(`GDP per capita, PPP (constant 2017 international $)`, w = Population))
    })
    
    output$distPlot <- renderPlot({
        
        dataset <- dataset %>%
            filter(Population >= 10 ^ input$Population, Year == input$Year)
        ggplot(dataset, aes(x = `GDP per capita, PPP (constant 2017 international $)`, y = HDI, group = Year)) +
            geom_point(aes(size = Population, color = Year)) + 
            geom_text_repel(aes(label = Entity)) + 
            geom_smooth(alpha = as.integer(input$lineShow) * 0.4,
                        linetype = as.integer(input$lineShow)) +
            ylim(0, 1)
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #      xlab = 'Waiting time to next eruption (in mins)',
        #      main = 'Histogram of waiting times')
    }, height = 800, width = 1200)
}

# Run the application 
shinyApp(ui = ui, server = server)
