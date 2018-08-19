# ============================== Library Application ============================== #

# Setting working directory
# ==============================
filepath <- c("/Users/nkaveti/Documents/thinksmart RR&L/Git/thiNksmaRt/")
setwd(filepath)

# Loading required packages
# ==============================
library(data.table)
library(openxlsx)
library(shiny)
library(shinydashboard)
library(rhandsontable)
library(bit64)

# Reading data
# ==============================
sheets <- c("Civils", "IIT", "Neet", "General", "SSC", "IBPS", "State", "Maths", "OTH")
meta_data <- c()
for(s in sheets){
  temp <- as.data.table(read.xlsx("Library Books.xlsx", sheet = s, na.strings = c("", " ")))
  temp[, Category := s]
  meta_data <- rbind(meta_data, temp)
}
# meta_data <- read.xlsx("Library Books.xlsx", sheet = "Civils", na.strings = c("", " "))
aspirants_data <- fread("Aspirants data.csv")
# aspirants_data[, DOB := as.Date(DOB)]
aspirants_data[, DOJ := as.Date(DOJ)]

# UI
# ==============================
dbheader <- dashboardHeader(title = "ThinkSmart Library")
dbsidebar <-  dashboardSidebar(sidebarMenu(
  menuItem("Aspirant Details", tabName = "aspirant_details"),
  menuItem("Books Catalog", tabName = "catalog")
)
)
dbbody <- dashboardBody(
  tabItems(
    tabItem(tabName = "aspirant_details",
            fluidRow(
              box(width = "100%" , height = "100%", dataTableOutput('aspirant_table'))
            )
    ),
    tabItem(tabName = "catalog",
            fluidRow(
              box(width = "100%" , height = "100%", selectInput(inputId = "type", label = "Category", choices = sheets, width = "30%"), dataTableOutput('book_catalog'))
            ))
  )
)

ui <- dashboardPage(dbheader, dbsidebar, dbbody)

# Server
# ==============================
server <- shinyServer(function(input, output, session){
  get_aspirant_data <- reactive({
    aspirants_data <- fread("Aspirants data.csv")
    # aspirants_data[, DOB := as.Date(DOB)]
    aspirants_data[, DOJ := as.Date(DOJ)]
    aspirants_data
  })
  
  updated_mdata <- reactiveValues(mdata = NULL)
  output$aspirant_table <- renderDataTable({
    df <- get_aspirant_data()
    df
  })
  
  output$book_catalog <- renderDataTable({
    df <- meta_data[Category == input$type, .(TSBN, ISBN, Library.Card.No, Name.of.the.Title, Author, Publication, Category)]
    colnames(df)[colnames(df) == "Library.Card.No"] <- "Card No."
    colnames(df)[colnames(df) == "Name.of.the.Title"] <- "Book Title"
    df
  })
})

# Running the App
# ==============================
shinyApp(ui = ui, server = server)
