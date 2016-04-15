library(RCurl)
library(jsonlite)
library(shiny)

rep_library <- data.frame(fromJSON("https://api.github.com/repos/ThomasK81/PeRseus/git/trees/master"))
rep_library <- rep_library[grep("phi", rep_library[, "tree.path"]), ]
rep_library <- rep_library[grep("tree", rep_library[, "tree.type"]), ]

base <- "https://raw.githubusercontent.com/ThomasK81/PeRseus/master/"
authorwork <- "phi0448/phi001"
corpus_link <- paste(base, authorwork, ".rds", sep="")
corpus_parsed_link <- paste(base, authorwork, "_parsed", ".rds", sep="")
corpus <- readRDS(gzcon(url(corpus_link)))
corpus_parsed <- readRDS(gzcon(url(corpus_parsed_link)))


server <- function(input, output) {
}

ui <- fluidPage(
  
  titlePanel("Reader by Thomas Koentges"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("author", "Choose an author:", 
                  choices = rep_library[, "tree.path"]),
      selectInput("carart", "Cartoons, Articles, or both?", 
                  choices = c("Cartoons", "Articles", "Both"),
                  selected = "Both"
      ),
      selectInput("meansum", "Mean or Sum", 
                  choices = c("Mean", "Sum"),
                  selected = "Sum"
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Topics Sums/Means", htmlOutput("view")),
        tabPanel("Topic Model", htmlOutput("topicmodels")),
        tabPanel("Tables", htmlOutput("view2")),
        tabPanel("Authorities", htmlOutput("view_authority"))
      )
    )))

shinyApp(ui = ui, server = server)
