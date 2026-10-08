# Project 4: Lists - Movie Watchlist
# Setup once: install.packages("shiny")   |   Run: click "Run App" in RStudio
library(shiny)

ui <- fluidPage(
  titlePanel("Movie Watchlist (Lists)"),
  sidebarLayout(
    sidebarPanel(
      textInput("mt", "Title"),
      numericInput("my", "Year", 2020),
      numericInput("mr", "Rating (0-10)", 8, 0, 10, 0.1),
      checkboxInput("mw", "Watched?"),
      actionButton("madd", "Add movie"),
      hr(),
      selectInput("mrm", "Remove a movie", choices = NULL),
      actionButton("mdel", "Remove")
    ),
    mainPanel(tableOutput("mtable"), verbatimTextOutput("mtext"))
  )
)

server <- function(input, output, session) {
  # A list of lists: each movie is a list with different data types
  wl <- reactiveVal(list(
    list(title = "Inception", year = 2010, rating = 8.8, watched = TRUE),
    list(title = "Interstellar", year = 2014, rating = 8.7, watched = FALSE),
    list(title = "Parasite", year = 2019, rating = 8.5, watched = FALSE)))

  observe(updateSelectInput(session, "mrm",
          choices = sapply(wl(), function(m) m$title)))

  observeEvent(input$madd, {
    req(nzchar(input$mt))
    wl(c(wl(), list(list(title = input$mt, year = input$my,
                         rating = input$mr, watched = input$mw))))
  })

  observeEvent(input$mdel, {
    wl(wl()[sapply(wl(), function(m) m$title) != input$mrm])
  })

  output$mtable <- renderTable({
    if (length(wl()) == 0) return(NULL)
    data.frame(
      Title  = sapply(wl(), function(m) m$title),
      Year   = sapply(wl(), function(m) as.integer(m$year)),
      Rating = sapply(wl(), function(m) m$rating),
      Status = sapply(wl(), function(m) if (m$watched) "Watched" else "To watch"))
  })

  output$mtext <- renderText({
    if (length(wl()) == 0) return("List is empty")
    r <- sapply(wl(), function(m) m$rating)
    paste0("Total movies: ", length(wl()),
           "\nFirst movie: ", wl()[[1]]$title,
           "\nAverage rating: ", round(mean(r), 2))
  })
}

shinyApp(ui, server)
