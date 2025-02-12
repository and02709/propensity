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

#intervals <- seq(1, 200, by = 1)  # Breakpoints (1, 2, 3, ..., 99)
#colors <- colorRampPalette(c("red", "orange", "yellow", "green", "darkgreen"))(length(intervals) + 1)  # Smooth gradient

intervals_contents <- seq(1, 200, by = 2)
colors_contents <- c("#0d0887", "#130789", "#1b068d", "#20068f", "#260591", "#2a0593", "#2f0596", 
  "#330597", "#38049a", "#3e049c", "#41049d", "#46039f", "#4903a0", "#4e02a2", 
  "#5102a3", "#5601a4", "#5901a5", "#5e01a6", "#6300a7", "#6600a7", "#6a00a8", 
  "#6e00a8", "#7201a8", "#7501a8", "#7a02a8", "#7e03a8", "#8104a7", "#8606a6", 
  "#8808a6", "#8d0ba5", "#8f0da4", "#9410a2", "#9613a1", "#9a169f", "#9e199d", 
  "#a11b9b", "#a51f99", "#a72197", "#ab2494", "#ad2793", "#b12a90", "#b32c8e", 
  "#b6308b", "#ba3388", "#bc3587", "#bf3984", "#c13b82", "#c43e7f", "#c6417d", 
  "#c9447a", "#cc4778", "#cd4a76", "#d04d73", "#d24f71", "#d5536f", "#d6556d", 
  "#d9586a", "#da5b69", "#dd5e66", "#df6263", "#e16462", "#e3685f", "#e56a5d", 
  "#e76e5b", "#e87059", "#ea7457", "#eb7655", "#ed7a52", "#ef7e50", "#f0804e", 
  "#f2844b", "#f3874a", "#f58b47", "#f68d45", "#f79143", "#f89540", "#f9983e", 
  "#fa9c3c", "#fb9f3a", "#fca338", "#fca636", "#fdab33", "#fdae32", "#fdb22f", 
  "#feb72d", "#feba2c", "#febe2a", "#fdc229", "#fdc627", "#fdca26", "#fcce25", 
  "#fcd225", "#fbd724", "#f9dc24", "#f8df25", "#f7e425", "#f6e826", "#f4ed27", 
  "#f3f027", "#f1f525", "#f0f921")  # Plasma colormap applied to intervals
text_contents <- colorRampPalette(c("#FFFFFF", "#DDDDDD",  "#333333", "#000000"))(length(intervals_contents) + 1)

intervals_crosstabs <- seq(0.5, 1.5, by = 0.01)
intervals_crosstabs <- intervals_crosstabs[-length(intervals_crosstabs)]
colors_crosstabs <- colorRampPalette(c("#0d0887", "#6a00a8", "#b12a90", "#e16462", "#ed7a52", "#fdab33", "#f0f921"))(length(intervals_crosstabs) + 1)

text_crosstabs <- colorRampPalette(c("#FFFFFF", "#DDDDDD",  "#333333", "#000000"))(length(intervals_contents) + 1)

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
      uiOutput("crosstabs_ui")
      
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
      
      # Check if user selected demographic variables
      if (is.null(input$demo_vars) || length(input$demo_vars) == 0) {
        return(NULL)  # Return NULL if no variables are selected
      }
      
      # Use count() instead of table() to maintain column integrity
      df <- data() %>% 
        dplyr::select(all_of(input$demo_vars)) %>% 
        dplyr::count(across(all_of(input$demo_vars)), name = "Frequency")
      
      # Create DT table with pagination & styling
      datatable(
        df,
        options = list(
          autoWidth = TRUE,
          columnDefs = list(
            list(width = "100px", targets = "_all")
          )
        )
      ) %>%
        formatStyle(
          'Frequency',  # Apply color to the frequency column
          backgroundColor = styleInterval(intervals_contents, colors_contents),  # Map many colors to small intervals
          color = styleInterval(intervals_contents, text_contents)
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

    output$crosstabs_ui <- renderUI({
        dataTableOutput("crosstabs_table")
      })
    
    output$crosstabs_table <- renderDataTable({
      req(wdat$crosstabs)
      
      datatable(
        wdat$crosstabs,
        options = list(
          autoWidth = TRUE,
          columnDefs = list(
            list(width = "100px", targets = "_all")
          )
        )
      ) %>% 
        formatStyle(
          'weights',
          backgroundColor = styleInterval(intervals_crosstabs, colors_crosstabs),
          color = styleInterval(intervals_crosstabs, text_crosstabs)
        )
    })  
  })
  
  
  
  # observeEvent(input$crosstabs, {
  #   output$crosstabs_ui <- render_gt(wdat$crosstabs %>% gt() %>% data_color(columns = weights, method = "numeric", palette = "plasma"))
  #   # output$crosstabs_ui <- render_gt(wdat$crosstabs %>% gt() %>% data_color(columns = race, method = "numeric", palette = "plasma"))
  # })
  
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

