library(shiny)
library(dplyr)
library(ggplot2)
theme_update(text = element_text(size=15))
library(reshape2)
library(RColorBrewer)

ui <- fluidPage(titlePanel("DeGlee"),
                tabsetPanel(
                  tabPanel(title = "Model",
                           sidebarLayout(
                             sidebarPanel(
                               sliderInput(
                                 inputId = "kD",
                                 label = "kD (m2/d):",
                                 min = 100,
                                 max = 1000,
                                 value = 250,
                                 step = 1,
                                 width = '100%'
                               ),
                               sliderInput(
                                 inputId = "c",
                                 label = "c (d):",
                                 min = 100,
                                 max = 1000,
                                 value = 250,
                                 step = 1,
                                 width = '100%'
                               ),
                               sliderInput(
                                 inputId = "Q",
                                 label = "Q0 (m3/d):",
                                 min = 10,
                                 max = 1000,
                                 value = 100,
                                 step = 1,
                                 width = '100%'
                               ),
                               checkboxInput("logscale_raxis", "Log scale", value = FALSE),
                               checkboxInput("Qr", "Q(r) graph", value = FALSE),
                               sliderInput(
                                 inputId = "rmax",
                                 label = "Max value r-axis (m)",
                                 min = 10,
                                 max = 1000,
                                 value = 100,
                                 step = 1,
                                 width = '100%'
                               )
                             ),
                             mainPanel(plotOutput("plot"), )
                           )),
                  tabPanel(title = "Ranges",
                           sidebarLayout(
                             sidebarPanel(
                               sliderInput(
                                 "kDrange",
                                 label = "Range kD (m2/d)",
                                 min = 1,
                                 max = 10000,
                                 value = c(100, 1000),
                                 step = 1,
                                 width = '100%'
                               ),
                               sliderInput(
                                 "crange",
                                 label = "Range c (d)",
                                 min = 10,
                                 max = 10000,
                                 value = c(100, 1000),
                                 step = 1,
                                 width = '100%'
                               ),
                               sliderInput(
                                 "Qrange",
                                 label = "Range Q0 (m3/d)",
                                 min = 1,
                                 max = 50000,
                                 value = c(1, 1000),
                                 step = 1,
                                 width = '100%'
                               ),
                               sliderInput(
                                 "rmax_range",
                                 label = "Range max value r-axis (m)",
                                 min = 1,
                                 max = 25000,
                                 value = c(10, 1000),
                                 step = 10,
                                 width = '100%'
                               )
                             ),
                             mainPanel()
                           )),
                  # Horizontal line ----
                  tabPanel(title = "Documentation",
                           includeHTML("www/DeGlee.html"))
                ))

server <- function(input, output, session) {
  labda <- function(kD, c) {
    sqrt(kD * c)
  }
  deGlee <- function(r, Q0, kD, c) {
    K0 <- besselK(r / labda(kD, c), nu = 0)
    (Q0 / (2 * pi * kD)) * K0
  }
  Huisman <- function(r, Q0, kD, c) {
    labda <- labda(kD, c)
    K1 <- besselK(r / labda, nu = 1)
    Q0 * (r / labda) * K1
  }
  
  # Adjust ranges
  observeEvent(input$kDrange, {
    updateSliderInput(session,
                      "kD",
                      min = input$kDrange[1],
                      max = input$kDrange[2])
  })
  observeEvent(input$crange, {
    updateSliderInput(session,
                      "c",
                      min = input$crange[1],
                      max = input$crange[2])
  })
  observeEvent(input$Qrange, {
    updateSliderInput(session,
                      "Q",
                      min = input$Qrange[1],
                      max = input$Qrange[2])
  })
  observeEvent(input$rmax_range, {
    updateSliderInput(session,
                      "rmax",
                      min = input$rmax_range[1],
                      max = input$rmax_range[2])
  })
  
  # Get (reactive) values for calculation
  rmin <- reactive({
    1
  })
  rstep <- reactive({
    25
  })
  r <- reactive({
    log_rmin <- log10(rmin())
    log_rmax <- log10(input$rmax)
    log_r <- seq(log_rmin, log_rmax, length.out = rstep())
    return(10 ^ log_r)
  })
  
  Q <- eventReactive(input$Q, {
    input$Q
  })
  
  s <- reactive({
    deGlee(r(), input$Q, input$kD, input$c)
  })
  
  Qr <- reactive({
    Huisman(r(), input$Q, input$kD, input$c)
  })
  
  dfm <- reactive({
    data.frame(Distance = r(),
               Drawdown = s(),
               Qr = Qr()) %>% reshape2::melt(id.vars = "Distance")
  })
  
  labda_str <- reactive({
    x <- labda(input$kD, input$c) %>% round(0) %>% as.character()
  })
  
  output$plot <- renderPlot({
    cbr <- brewer.pal(n = 3, name = 'Set1')
    dfm <- dfm()
    if (input$Qr) {
      ok <- dfm$variable == "Qr"
    } else {
      ok <- dfm$variable == "Drawdown"
    }
    dfm[] <- dfm[ok, ]
    plt <- ggplot(data = dfm,
                  aes(x = Distance, y = value, colour = variable)) +
      geom_line(colour = cbr[2], size = 1) +
      xlab("Distance r (m)")
    
    if (input$Qr) {
      plt <- plt + ggtitle("Q(r)") +
        ylab("Q(r) (m3/d)")
    } else {
      plt <-
        plt + ggtitle(paste("Drawdown S(r) (", "\u03bb", "=", labda_str(), "m)")) +
        ylab("Drawdown S(r) (m)")
    }
    if (input$logscale_raxis) {
      plt <- plt + scale_x_continuous(trans = 'log10')
    }
    return(plt)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
