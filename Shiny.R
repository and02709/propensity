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

memory.limit(size = 8192)

extract_element <- function(array, name_vec) {
  num_dim <- length(dim(array))
  array_names <- dimnames(array)
  index <- mapply(function(x, y) return(which(x==y)), array_names, name_vec)
  return(array[matrix(index, 1)])
}

output_checks <- function(data){
  flag <- 1
  data <- as.data.frame(data)
  if(length(which(data$weights > 1.5)) > 0){
    flag <- 0
    showNotification("There are weights greater than 1.5")
  } 
  if(length(which(data$weights < 0.5)) > 0){
    flag <- 0
    showNotification("There are weights less than 0.5")
  }
  return(flag)
}

#intervals <- seq(1, 200, by = 1)  # Breakpoints (1, 2, 3, ..., 99)
#colors <- colorRampPalette(c("red", "orange", "yellow", "green", "darkgreen"))(length(intervals) + 1)  # Smooth gradient

# Intervals for binning
intervals_contents <- seq(1, 200, by = 2)

# Plasma colormap manually defined
colors_contents <- c(
  "#0d0887", "#130789", "#1b068d", "#20068f", "#260591", "#2a0593", "#2f0596", 
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
  "#f3f027", "#f1f525", "#f0f921"
)

# Reverse direction (light → dark)
colors_contents <- rev(colors_contents)

# Text overrides
light_text_colors <- c(
  "#0d0887", "#130789", "#1b068d", "#20068f", "#260591", "#2a0593", "#2f0596", 
  "#330597", "#38049a", "#3e049c", "#41049d", "#46039f", "#4903a0", "#4e02a2", 
  "#5102a3", "#5601a4", "#5901a5", "#5e01a6", "#6300a7", "#6600a7", "#6a00a8", 
  "#6e00a8", "#7201a8", "#7501a8", "#7a02a8", "#7e03a8", "#8104a7", "#8606a6", 
  "#8808a6", "#8d0ba5", "#8f0da4", "#9410a2", "#9613a1", "#9a169f", "#9e199d", 
  "#a11b9b", "#a51f99", "#a72197", "#ab2494", "#ad2793", "#b12a90", "#b32c8e", 
  "#b6308b", "#ba3388", "#bc3587", "#bf3984", "#c13b82", "#c43e7f", "#c6417d", 
  "#c9447a", "#cc4778", "#cd4a76", "#d04d73", "#d24f71", "#d5536f", "#d6556d", 
  "#d9586a", "#da5b69", "#dd5e66", "#df6263", "#e16462", "#e3685f", "#e56a5d"
)

dark_text_colors <- c(
  "#e76e5b", "#e87059", "#ea7457", "#eb7655", "#ed7a52", "#ef7e50", "#f0804e", 
  "#f2844b", "#f3874a", "#f58b47", "#f68d45", "#f79143", "#f89540", "#f9983e", 
  "#fa9c3c", "#fb9f3a", "#fca338", "#fca636", "#fdab33", "#fdae32", "#fdb22f", 
  "#feb72d", "#feba2c", "#febe2a", "#fdc229", "#fdc627", "#fdca26", "#fcce25", 
  "#fcd225", "#fbd724", "#f9dc24", "#f8df25", "#f7e425", "#f6e826", "#f4ed27", 
  "#f3f027", "#f1f525", "#f0f921"
)

# Total bins
n_colors <- length(intervals_contents) + 1

# Start with all text white
text_contents <- rep("#FFFFFF", n_colors)

# Apply overrides
for (i in seq_along(colors_contents)) {
  bg <- colors_contents[i]
  if (bg %in% dark_text_colors) {
    text_contents[i] <- "#000000"
  } else if (bg %in% light_text_colors) {
    text_contents[i] <- "#FFFFFF"
  } else {
    text_contents[i] <- "#999999"  # fallback (optional)
  }
}



# intervals_crosstabs <- seq(0.5, 1.5, by = 0.01)
# intervals_crosstabs <- intervals_crosstabs[-length(intervals_crosstabs)]
# colors_crosstabs <- colorRampPalette(c("#0d0887", "#6a00a8", "#b12a90", "#e16462", "#ed7a52", "#fdab33", "#f0f921"))(length(intervals_crosstabs) + 1)
# 
# n_colors <- length(intervals_crosstabs) + 1
# cutoff <- ceiling(n_colors * 0.95)
# 
# light_text_colors <- colorRampPalette(c("#FFFFFF", "#EEEEEE", "#CCCCCC"))(cutoff)
# dark_text_colors <- colorRampPalette(c("#444444", "#222222", "#000000"))(n_colors - cutoff)
# 
# text_crosstabs <- c(light_text_colors, dark_text_colors)

# Intervals
intervals_crosstabs <- seq(0.5, 1.5, by = 0.01)
intervals_crosstabs <- intervals_crosstabs[-length(intervals_crosstabs)]

# Color palette
colors_crosstabs <- colorRampPalette(c(
  "#0d0887", "#6a00a8", "#b12a90", "#e16462", "#ed7a52", "#fdab33", "#f0f921"
))(length(intervals_crosstabs) + 1)

# Total number of colors
n_colors <- length(colors_crosstabs)

# --- Define a cutoff point where background becomes light enough for dark text ---
# You can pick the cutoff by:
# 1. Fixed percentage
cutoff_index <- ceiling(n_colors * 0.6)

# 2. OR: Based on RGB similarity to a known transition color (optional)
# rgb_matrix <- col2rgb(colors_crosstabs)
# reference_rgb <- col2rgb("#e76e5b")  # transition point
# dists <- apply(rgb_matrix, 2, function(col) sum((col - reference_rgb)^2))
# cutoff_index <- which.min(dists)

# Assign text colors: light before cutoff, dark after
text_crosstabs <- c(
  rep("#FFFFFF", cutoff_index),
  rep("#000000", n_colors - cutoff_index)
)




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
      uiOutput("diagnostics_ui"),
      uiOutput("summary_ui"),
      uiOutput("crosstabs_ui"),
      uiOutput("missingness_ui"),
      uiOutput("heatmap_ui"),
      
      # Gradient legend
      tags$br(),
      tags$div(
        style = "height: 20px; 
             background: linear-gradient(to right, #f0f921, #fdab33, #ed7a52, #e16462, #b12a90, #6a00a8, #0d0887);
             margin-top: 20px; border: 1px solid #ccc;"
      ),
      tags$div("← Lighter yellow is bad  Darker blue is good →", 
               style = "text-align: center; font-weight: bold; margin-top: 5px;")
      
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
    
    # Render heatmap UI and plot when show_data is clicked
    output$heatmap_ui <- renderUI({
      plotOutput("heatmap_plot")
    })
    
    output$heatmap_plot <- renderPlot({
      req(data())
      df <- data()
      
      if (length(input$demo_vars) != 2) {
        showNotification("Please select exactly two variables for heatmap", type = "error")
        return(NULL)
      }
      
      selected_data <- df[, input$demo_vars, drop = FALSE]
      
      # Create a count table
      count_df <- selected_data %>%
        dplyr::count(across(everything())) %>%
        rename(Freq = n)
      
      colnames(count_df)[1:2] <- c("Var1", "Var2")
      
      # Bin Freq values into the plasma scale
      count_df <- count_df %>%
        mutate(
          bin_index = cut(Freq, breaks = c(-Inf, intervals_contents, Inf), labels = FALSE),
          fill_color = colors_contents[bin_index],
          text_color = text_contents[bin_index]
        )
      
      ggplot(count_df, aes(x = Var1, y = Var2)) +
        geom_tile(aes(fill = fill_color), color = "white") +
        geom_text(aes(label = Freq, color = text_color), size = 4) +
        scale_fill_identity() +
        scale_color_identity() +
        labs(
          title = "Heatmap of Demographic Counts",
          x = input$demo_vars[1],
          y = input$demo_vars[2]
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    })
  })

  output$diagnostics_ui <- renderUI({
    req(data())
    req(input$show_data)
    plotOutput("data_diagnostics")
  })
  
  output$data_diagnostics <- renderPlot({
    req(data())
    req(input$demo_vars)
    req(input$show_data)
    validate(need(length(input$demo_vars) >= 2, "Select at least two variables for diagnostics"))
    GGally::ggpairs(data()[, input$demo_vars, drop = FALSE])
  })
  
  output$missingness_ui <- renderUI({
    req(data())
    req(input$show_data)
    verbatimTextOutput("missingness_report")
  })
  
  output$missingness_report <- renderText({
    req(input$show_data)
    df <- data()
    miss_summary <- sapply(df, function(col) mean(is.na(col)))
    high_missing <- miss_summary[miss_summary > 0.1]
    
    if (length(high_missing) > 0) {
      paste("Variables with >10% missing data:\n",
            paste(names(high_missing),
                  sprintf("%.1f%%", 100 * high_missing),
                  collapse = "\n"))
    } else {
      "No variables exceed 10% missingness."
    }
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
    
    output$diagnostics_ui <- renderUI({
      NULL
    })
    
    output$missingness_ui <- renderUI({
      NULL
    })
    
    output$heatmap_ui <- renderUI({
      NULL
    })
  })
  
  # Download Handler
  output$downloadData <- downloadHandler(
    
    filename = function() {
      paste("crosstabs_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if(output_checks(wdat$crosstabs)){
        write.csv(wdat$crosstabs, file, row.names = FALSE)
      } else{
        showNotification("Weight values too extreme")
      }
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

