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
              box(width = "100%" , height = "100%", rHandsontableOutput('aspirant_table'), br(), actionButton(inputId = "update", label = "Update"))
            )
    ),
    tabItem(tabName = "catalog",
            fluidRow(
              box(width = "100%" , height = "100%", selectInput(inputId = "type", label = "Category", choices = sheets, width = "30%"), dataTableOutput('book_catalog'), actionButton(inputId = "add_new_row", label = "Add"))
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
  output$aspirant_table <- renderRHandsontable({
    if(is.null(input$aspirant_table)){
      df <- get_aspirant_data()
      updated_mdata$mdata <<- df
    }else{
      df <- hot_to_r(input$aspirant_table)
      aspirants_data[, DOJ := as.Date(DOJ)]
      updated_mdata$mdata <<- df
    }
    print(updated_mdata$mdata)
    rhandsontable(df, useTypes = TRUE)
  })
  
  observeEvent(input$update, {
    if(!is.null(updated_mdata$mdata)){
      print("TRUE")
      fwrite(as.data.table(updated_mdata$mdata), "Aspirants data.csv")
    }
  })
  
  output$book_catalog <- renderDataTable({
    df <- meta_data[Category == input$type, .(TSBN, ISBN, Library.Card.No, Name.of.the.Title, Author, Publication, Category)]
    colnames(df)[colnames(df) == "Library.Card.No"] <- "Card No."
    colnames(df)[colnames(df) == "Name.of.the.Title"] <- "Book Title"
    df
  })
  
  modal_modify<-modalDialog(
    fluidPage(
      h3(strong("Add New Record"),align="center"),
      hr(),
      dataTableOutput('row_modif'),
      actionButton("save_changes","Save changes"),
      
      tags$script(HTML("$(document).on('click', '#save_changes', function () {
                       var list_value=[]
                       for (i = 0; i < $( '.new_input' ).length; i++)
                       {
                       list_value.push($( '.new_input' )[i].value)
                       }
                       Shiny.onInputChange('newValue', list_value)
});"))
    ),
    size="l"
      )
  
  output$row_modif<-renderDataTable({
    # selected_row=as.numeric(gsub("modify_","",input$lastClickId))
    # old_row=vals$Data[selected_row]
    
    row_change=list()
    for (i in colnames(meta_data))
    {
      if (is.numeric(meta_data[[i]]))
      {
        row_change[[i]]<-paste0('<input class="new_input" type="number" id=new_',i,'><br>')
      }
      else
        row_change[[i]]<-paste0('<input class="new_input" type="text" id=new_',i,'><br>')
    }
    row_change=as.data.table(row_change)
    setnames(row_change,colnames(meta_data))
    DT=rbind(meta_data, row_change)
    # rownames(DT)<-c("Current values","New values")
    DT
    
  },escape=F,options=list(dom='t',ordering=F),selection="none"
  )
  
  
  
  observeEvent(input$add_new_row, {
    showModal(modal_modify)
    newValue=lapply(input$newValue, function(col) {
      if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
        as.numeric(as.character(col))
      } else {
        col
      }
    })
  })
  
  })

# Running the App
# ==============================
shinyApp(ui = ui, server = server)
