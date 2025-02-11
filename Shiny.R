
library(shiny)
library(tidyverse)
library(tidymodels)
library(shinyjs)
library(grid)
library(gridExtra)
library(GGally)
library(gt)
library(DT)

### Notes to self
# in variable QC add sliding scale of original counts, final counts, and weights
# Same sliding scale for missingness
# Sliding for reference panel.  Need to sample to be proper subset of buckets in reference

extract_element <- function(array, name_vec) {
  num_dim <- length(dim(array))
  array_names <- dimnames(array)
  index <- mapply(function(x, y) return(which(x==y)), array_names, name_vec)
  return(array[matrix(index, 1)])
}

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Demographic Weight Calculator"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Input: File upload
      fileInput("file1", "Choose CSV or TXT File",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv", ".txt")),
      
      # Input: Checkbox if the file has a header
      checkboxInput("header", "Header", TRUE),
      
      # Input: Select separator for TXT files
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t",
                               Whitespace=""),
                   selected = ","),
      
      # Input: Select demographic variables for weighting
      uiOutput("var_select"),
      
      # Input: Select Quality Control option
      uiOutput("QC_select"),
      
      # Input: Select Quality Control variable for weighting if variable
      uiOutput("QC_select_var"),
      
      # Input: Select Quality Control variable for weighting if variable
      uiOutput("QC_select_miss"),
      
      # Input: Select Quality Control variable for weighting if variable
      uiOutput("QC_ref"),
      
      # Input: Select Quality Control variable threshold lower limit
      uiOutput("Thresh_lower"),
      
      # Input: Select Quality Control variable threshold upper limit
      uiOutput("Thresh_upper"),
      
      # Button: Action button to trigger data display
      actionButton("show_data", "Show Selected Data"),
      
      # Button: Trigger weight calculation
      actionButton("calculate", "Calculate Weights"),
      
      # Button: Trigger cross tabs table
      actionButton("crosstabs", "Cross-tabulated Data"),
      
      # Button: Clear output
      actionButton("clearButton", "Clear Output"),
      
      # Button: Download the resulting data
      downloadButton("downloadData", "Download Weights")
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      #tableOutput("contents"),
      #tableOutput("weights")
      uiOutput("contents_ui"),
      uiOutput("summary_ui"),
      uiOutput("crosstabs_ui"),
      
    )
  )
)

# Define server logic required to calculate weights
server <- function(input, output, session) {
  
  # Reactive expression to read the data from the uploaded file
  data <- reactive({
    req(input$file1)
    ext <- tools::file_ext(input$file1$name)
    if (ext == "csv") {
      read.csv(input$file1$datapath, header = input$header)
    } else if (ext == "txt") {
      read.table(input$file1$datapath, sep = input$sep, header = input$header)
    } else {
      stop("Unsupported file type")
    }
  })
  
  wdat <- reactiveValues()
  
  # Update demographic variable choices based on the uploaded data
  output$var_select <- renderUI({
    req(data())
    
    checkboxGroupInput("demo_vars", "Select Demographic Variables:",
                       choices = colnames(data()))
    # selectInput("demo_vars",
    #             "Select Demographic Variables:",
    #             choices = names(data()),
    #             selected = names(data())[1],
    #             multiple = TRUE)
  })
  
  output$QC_select <- renderUI({
    req(data())
    # radioButtons("QC_vars", "Quality Control Variable:",
    #              choices = colnames(data()))
    
    radioButtons("QC_opts", "Quality Control Variable:",
                 choices = list("Quality Control Variable" = "var", 
                                "Missingness" = "miss", 
                                "Reference Panel" = "ref")
                 )
  })
  
  output$QC_select_miss <- renderUI({
    req(data())
    req(input$QC_opts)
    if(input$QC_opts=="miss"){
      checkboxGroupInput("QC_miss", "Quality Control Variable:",
                   choices = colnames(data()))
    }
  })
  
  output$QC_ref <- renderUI({
    req(data())
    req(input$QC_opts)
    if(input$QC_opts=="ref"){
      fileInput("reffile", "Choose CSV or TXT File",
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain", 
                           ".csv", 
                           "text/plain", 
                           ".txt"))
    }
  })
  
  ref_data <- reactive({
    req(input$reffile)  # Make sure a file is uploaded before proceeding
    
    # Read the file based on its extension
    ext <- tools::file_ext(input$reffile$name)
    if (ext == "csv") {
      read.csv(input$reffile$datapath)
    } else if (ext == "txt") {
      read.table(input$reffile$datapath, header = TRUE)
    } else {
      stop("Invalid file type. Please upload a CSV or TXT file.")
    }
  })
  
  output$QC_select_var <- renderUI({
    req(data())
    req(input$QC_opts)
    if(input$QC_opts=="var"){
      radioButtons("QC_vars", "Quality Control Variable:",
                 choices = colnames(data()))
    }
  })
  
  
  
  output$Thresh_lower <- renderUI({
    req(data())
    req(input$QC_vars)
    if(input$QC_opts=="var"){
      numericInput("thresh_lower", "Enter a Lower QC Variable Limit:", value = -10)
    }
  })
  
  output$Thresh_upper <- renderUI({
    req(data())
    req(input$QC_vars)
    if(input$QC_opts=="var"){
      numericInput("thresh_upper", "Enter an Upper QC Variable Limit:", value = 10)
    }
  })
  
  observeEvent(input$show_data, {
    
    output$contents_ui <- renderUI({
      dataTableOutput("contents_table")  # Create a DT output placeholder
    })
    
    output$contents_table <- renderDataTable({
      req(data())  # Ensure data is available
      
      # Convert data to a format suitable for DT
      df <- tibble::tibble(data()) %>% 
        dplyr::select(all_of(input$demo_vars)) %>% 
        table() %>% 
        as.data.frame() %>% 
        group_by(!!!syms(input$demo_vars)) %>% 
        ungroup()
      
      # Create DT table with pagination & styling
      datatable(
        df, 
        options = list(
          pageLength = 10,        # Number of rows per page
          lengthMenu = c(5, 10, 20, 50), # Dropdown for number of rows
          scrollX = TRUE,         # Enable horizontal scrolling
          autoWidth = TRUE        # Adjust column widths
        )
      ) %>%
        formatStyle(
          'Freq',  # Apply color to the frequency column
          backgroundColor = styleInterval(
            c(5, 10, 20),  # Color intervals
            c('red', 'yellow', 'lightgreen', 'darkgreen')  # Color scheme
          ),
          color = 'black'
        )
    })
  })
  
  observeEvent(input$calculate, {
    output$summary_ui <- renderUI({
      tableOutput("weightsSummary")
    })
    
    if(input$QC_opts=="var"){
      ref_data <- reactive({
        req(input$reffile)  # Ensure a file is uploaded
        read.csv(input$file$datapath,
                 header = input$header,
                 sep = input$sep)
      })
      
        weight_data <- reactive({
          req(input$demo_vars)
          req(input$QC_vars)
          req(input$thresh_lower)
          req(input$thresh_upper)
          req(input$calculate)
          df <- as.data.frame(data())
          t0 <- table(df %>% dplyr::select(all_of(input$demo_vars)))
          # df <- df %>% dplyr::filter(meanfd > input$thresh_lower)
          # df <- df %>% dplyr::filter(meanfd < input$thresh_upper)
          df <- df %>% dplyr::filter(between(!!sym(input$QC_vars), input$thresh_lower, input$thresh_upper))
          t1 <- table(df %>% dplyr::select(all_of(input$demo_vars)))
          tdiff <- t0-t1
          tprop <- tdiff/t0
          tratio <- t0/t1
          name_frame <- dimnames(tdiff)
          name_mat <- expand.grid(name_frame)
          weight_groups <- apply(name_mat, 1, extract_element, array=tratio)
          weight_groups[which(is.na(weight_groups))] <- 1
          weight_groups[which(is.infinite(weight_groups))] <- 1
          name_mat$weights <- weight_groups
          t_weights <- inner_join(df, name_mat, by=input$demo_vars)
          t0 <- tibble::tibble(data()) %>% dplyr::select(all_of(input$demo_vars)) %>% table(.) %>% data.frame(.)
          t1 <- t_weights %>% dplyr::select(all_of(input$demo_vars)) %>% table(.) %>% data.frame(.)
          t2 <- dplyr::inner_join(t0, t1, by=input$demo_vars)
          cnames <- colnames(t2)
          cnames[which(cnames=="Freq.x")] <- "Original_Count"
          cnames[which(cnames=="Freq.y")] <- "Filtered_Count"
          colnames(t2) <- cnames
          t3 <- t_weights %>% group_by(!!!syms(input$demo_vars)) %>% summarise(across("weights", mean, na.rm=T))
          wdat$crosstabs <- inner_join(t2, t3, by=input$demo_vars)
          t_weights
        })
        
        output$weightsSummary <- renderTable({
          req(weight_data())
          req(input$demo_vars)
          req(input$QC_vars)
          req(input$thresh_lower)
          req(input$thresh_upper)
          req(input$calculate)
          NULL
        })
    }
    
    if(input$QC_opts=="miss"){
      weight_data <- reactive({
        req(input$demo_vars)
        req(input$QC_miss)
        req(input$calculate)
        df <- as.data.frame(data())
        t0 <- table(df %>% dplyr::select(all_of(input$demo_vars)))
        df <- df %>% dplyr::filter(across(input$QC_miss, ~ !is.na(.)))
        t1 <- table(df %>% dplyr::select(all_of(input$demo_vars)))
        tdiff <- t0-t1
        tprop <- tdiff/t0
        tratio <- t0/t1
        name_frame <- dimnames(tdiff)
        name_mat <- expand.grid(name_frame)
        weight_groups <- apply(name_mat, 1, extract_element, array=tratio)
        weight_groups[which(is.na(weight_groups))] <- 1
        weight_groups[which(is.infinite(weight_groups))] <- 1
        name_mat$weights <- weight_groups
        t_weights <- inner_join(df, name_mat, by=input$demo_vars)
        t0 <- tibble::tibble(data()) %>% dplyr::select(all_of(input$demo_vars)) %>% table(.) %>% data.frame(.)
        t1 <- t_weights %>% dplyr::select(all_of(input$demo_vars)) %>% table(.) %>% data.frame(.)
        t2 <- dplyr::inner_join(t0, t1, by=input$demo_vars)
        cnames <- colnames(t2)
        cnames[which(cnames=="Freq.x")] <- "Original_Count"
        cnames[which(cnames=="Freq.y")] <- "Filtered_Count"
        colnames(t2) <- cnames
        t3 <- t_weights %>% group_by(!!!syms(input$demo_vars)) %>% summarise(across("weights", mean, na.rm=T))
        wdat$crosstabs <- inner_join(t2, t3, by=input$demo_vars)
        t_weights
      })
      
      output$weightsSummary <- renderTable({
        req(weight_data())
        req(input$demo_vars)
        req(input$QC_miss)
        req(input$QC_vars)
        req(input$calculate)
        NULL
      })
    }
    
    if(input$QC_opts=="ref"){
      
      weight_data <- reactive({
        req(input$demo_vars)
        req(input$calculate)

        df <- as.data.frame(data())
        t0 <- table(df %>% dplyr::select(all_of(input$demo_vars)))
        dfref <- as.data.frame(ref_data())
        t1 <- table(dfref %>% dplyr::select(all_of(input$demo_vars)))
        tdiff <- t0-t1
        tprop <- tdiff/t0
        tratio <- t0/t1
        name_frame <- dimnames(tdiff)
        name_mat <- expand.grid(name_frame)
        weight_groups <- apply(name_mat, 1, extract_element, array=tratio)
        weight_groups[which(is.na(weight_groups))] <- 1
        weight_groups[which(is.infinite(weight_groups))] <- 1
        name_mat$weights <- weight_groups
        t_weights <- inner_join(dfref, name_mat, by=input$demo_vars)
        t0 <- tibble::tibble(data()) %>% dplyr::select(all_of(input$demo_vars)) %>% table(.) %>% data.frame(.)
        t1 <- t_weights %>% dplyr::select(all_of(input$demo_vars)) %>% table(.) %>% data.frame(.)
        t2 <- dplyr::inner_join(t0, t1, by=input$demo_vars)
        cnames <- colnames(t2)
        cnames[which(cnames=="Freq.x")] <- "Original_Count"
        cnames[which(cnames=="Freq.y")] <- "Reference_Count"
        colnames(t2) <- cnames
        t3 <- t_weights %>% group_by(!!!syms(input$demo_vars)) %>% summarise(across("weights", mean, na.rm=T))
        wdat$crosstabs <- inner_join(t2, t3, by=input$demo_vars)
        t_weights

      })
      
      output$weightsSummary <- renderTable({
        req(weight_data())
        req(input$demo_vars)
        req(input$QC_miss)
        req(input$QC_vars)
        req(input$calculate)

      })
    }
    
  })
  
  observeEvent(input$crosstabs, {
    output$crosstabs_ui <- render_gt(wdat$crosstabs %>% gt() %>% data_color(columns = weights, method = "numeric", palette = "plasma"))
    # output$crosstabs_ui <- render_gt(wdat$crosstabs %>% gt() %>% data_color(columns = race, method = "numeric", palette = "plasma"))
  })
  
  # Clear the table when the clear button is clicked
  observeEvent(input$clearButton, {
    output$contents_ui <- renderUI({
      NULL
    })
    
    output$summary_ui <- renderUI({
      NULL
    })
    
    output$crosstabs_ui <- renderUI({
      NULL
    })
  })
  
  # Allow the user to download the weighted data as a CSV file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("weights_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      if(condition==TRUE){
        write_csv(weights(), file)
      }
      else{
        
      }
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

