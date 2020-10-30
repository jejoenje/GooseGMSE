library(shiny)
library(DT)
library(markdown)
library(rmarkdown)
library(knitr)
#library(kableExtra)
source('goose_predict_gui.R')

#DATE = format(Sys.time(), "%d %B %Y")
DATE = "October 2020"
APPTITLE = "Goose-GMSE"
APPVERSION = "1.2.4"
appheader = paste(APPTITLE, " (v. ", APPVERSION, ", ", DATE, ")", sep = "")

plot <- FALSE
past <- FALSE
resamp <- TRUE
extinct <- FALSE  
prev_params <- NULL

assign("plot", plot, envir = globalenv())
assign("past", past, envir = globalenv())
assign("resamp", resamp, envir = globalenv())
assign("extinct", extinct, envir = globalenv())
assign("prev_params", prev_params, envir = globalenv())

progress_i <- 0
assign("progress_i", progress_i, envir = globalenv())
updateProgress <- function() {
    value <- progress$getValue()
    value <- value + (progress$getMax() - value) / 5
}

valInput <- function(val_filename) {
    val_file <- load_input(val_filename)
    return(validate_input(val_file))
}

cull_table_format <- htmltools::withTags(table(
    class = 'display',
    thead(
        tr(
            th('Year'),
            th('Projected mean populaton size'),
            th('Mean culled'),
            th('SD culled'),
            th('Min. culled'),
            th('Max. culled')
        )
    )
))


ui <- fluidPage(
  
  titlePanel(
      appheader, windowTitle = APPTITLE
  ),
  
  sidebarLayout(
    sidebarPanel(
      br(),
      fileInput("input_name", "Choose data file (XLS, XLSX or CSV)",
                accept = c("application/vnd.ms-excel", 
                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", ".xls", ".xlsx",
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
        
      sliderInput("yrs_in", "Number of years to project:",
                  min = 1, max = 15,
                  value = 2),
      
      sliderInput("sims_in", "Number of simulations:",
                  min = 5, max = 1000, value = 5, step=5),
      
      sliderInput("target_in", "Population target:",
                  min = 5000, max = 50000,
                  value = 32000, step=1000),
      
      sliderInput("maxHB_in", "Max. cull per year:",
                  min = 0, max = 10000, value = 2000, step = 100),
    
      actionButton('run_in', 'Run simulations'),
      br(),
      #uiOutput("download"),
      downloadButton("report", "Generate report"),
      br(),
      br(),
      img(src = "GMSE_logo_name.png", width='50%')
      
    ),
    
    mainPanel(
      
      tabsetPanel(id = "inTabset",
        tabPanel(title = "Introduction", value = "intro_tab", 
                 includeMarkdown("Introduction.Rmd")
                 ),

        tabPanel(title = "Help", value = "help_tab", 
                 includeMarkdown("Help.Rmd")
                 ),
        tabPanel(title = "Output", value = "out_tab",
                    h2("Goose-GMSE simulations results"),
                    paste("Generated on", Sys.time()),
                    br(),
                    h3("Output"),
                    plotOutput('plot', height='600px', width='800px'),
                    htmlOutput('text_summary'),
                    h3("Input parameters"),
                    br(),
                    dataTableOutput('in_summary'),
                    br(),
                    br(),
                    h4("Numbers culled per projected year"),
                    dataTableOutput('out_culls'),
                    br(),
                    br()
                 )
            
        )
        

      )
    )
  )



server <- function(session, input, output) {
  
  # "Output" tab is hidden initially
  hideTab(inputId = "inTabset", target = "out_tab")
  
  ### Show main output tab when run button is pressed
  observeEvent(input$run_in, {
      showTab(inputId = "inTabset", target = "out_tab")
      updateTabsetPanel(session=session, inputId="inTabset", selected="out_tab") 
    })

  ### calcPlot() runs when run button is pressed.
  ### Runs main simulations (gmse_goose_multiplot() given input variables)
  calcPlot <- eventReactive(input$run_in, {
    
    ### Clears any existing output files from working directory
    clearExistingOutput()
    clearGlobalVars()
    
    validate(
        need(try(input$input_name), "Please select a base data input file first.")
    )

    #updateTabsetPanel(session=session, inputId="inTabset", selected="out_tab")
    
    progress_i <- 0
    assign("progress_i", progress_i, envir = globalenv()) 
    progress <- shiny::Progress$new(session = session, min = 0, max = input$sims_in*(input$yrs_in+1))
    on.exit(progress$close())
    assign("progress", progress, envir = globalenv())
      
    progress$set(message = "Running simulations...", value = 0)

    # This runs the simulations using the input vals and plots the main output graph:    
    sims <- gmse_goose_multiplot(
        data_file=input$input_name$datapath, 
        iterations=input$sims_in, 
        proj_yrs = input$yrs_in, 
        max_HB=input$maxHB_in, 
        manage_target = input$target_in)

    # This re-plots the simulations but only for the projected range:

    #gmse_print_multiplot(goose_multidata = sims, manage_target = input$target_in, proj_yrs = input$yrs_in)
    
    #assign("sims", sims, envir = globalenv())
    #assign("input", input, envir = globalenv())
    input_list <- data.frame(datapath=input$input_name$datapath, 
                             sims_in=input$sims_in, 
                             yrs_in=input$yrs_in, 
                             maxHB_in=input$maxHB_in, 
                             target_in=input$target_in)

    save(input_list, file='input.Rdata')
    save(sims, file='sims.Rdata')
    
    updateTabsetPanel(session=session, inputId="inTabset", selected="out_tab")
    
  })
  
  ### genSummaryText() runs when run button is pressed.
  ### Generates summary output text from simulations.
  genSummaryText <- eventReactive(input$run_in, { 
    validate(
      need(file.exists("sims.Rdata"), "No output found, simulations failed. Please reload app and re-run.")
    )
    output_summary <- genSummary()
    save(output_summary, file='output_summary.Rdata')
    print(output_summary)
    
  })
  
  
  
  ### cullTable() runs when calcPlot() has been run.
  ### Generates summary table of mean population estimates and mean numbers culled (across simulations)
  cullTable <- eventReactive(input$run_in, {

    #validate(need(try(input$input_name), ""))
    validate(
      need(file.exists("sims.Rdata"), "No output found, simulations failed. Please reload app and re-run.")
    )
    #updateTabsetPanel(session=session, inputId="inTabset", selected="out_tab2") 
    load("sims.Rdata")
    load("input.Rdata")
    input = input_list
    res <- gmse_goose_summarise(sims, input)
    
    cull_summary <- cbind( (res$last_obs_yr+1):(res$end_yr) ,
                           floor(res$proj_y_mn),
                           floor(res$mean_HB),
                           floor(res$sd_HB),
                           floor(res$min_HB),
                           floor(res$max_HB))
    cull_summary <- as.data.frame(cull_summary)
    save(cull_summary, file='cull_summary.Rdata')
    coln <- c('Year','Projected mean populaton size','Mean culled','SD culled','Min. culled','Max. culled')
    datatable(cull_summary, colnames=coln, rownames=FALSE, options = list(dom = 't'))
    
  })
  
  inTable <- eventReactive(input$run_in, {

    validate(
      need(file.exists("input.Rdata"), "No output found, simulations failed. Please reload app and re-run.")
    )
    load("input.Rdata")
    in_summary <- genInputSummary(input = input_list)
    save(in_summary, file='in_summary.Rdata')
    
    datatable(in_summary, colnames=names(in_summary), rownames=FALSE, options = list(dom = 't'))
    
  })
  
  ### Legacy function for generating original data table for output:
  # origTable <- eventReactive(input$run_in, {
  #     validate(need(try(input$input_name), ""))
  # 
  #     orig_data <- goose_clean_data(input$input_name$datapath)
  #     orig_data[,6:9] <- round(orig_data[,6:9],2)
  #     orig_data[,11] <- round(orig_data[,11],2)
  #     save(orig_data, file='orig_data.Rdata')
  # 
  #     datatable(orig_data, colnames=names(orig_data), rownames=FALSE, options = list(dom = 't'))
  # 
  # })
  
  output$plot <- renderPlot({
    calcPlot()
  }, res = 100)
  
  output$text_summary <- renderPrint({
    genSummaryText()
  })
  
  output$in_summary <- renderDataTable({
      inTable()
  })
  
  output$out_culls <- renderDataTable({
    cullTable()
  })

  ### Legacy code for including original data table in output:
  # output$out_orig <- renderDataTable({
  #     origTable()
  # })
  
  output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.docx",
      content = function(file = filename) {
          # Copy the report file to a temporary directory before processing it, in
          # case we don't have write permissions to the current working dir (which
          # can happen when deployed).
           tempReport <- file.path(tempdir(), "goosegmse_output.Rmd")
           file.copy("goosegmse_output.Rmd", tempReport, overwrite = TRUE)
          
          # Knit the document, passing in the `params` list, and eval it in a
          # child of the global environment (this isolates the code in the document
          # from the code in this app).
          rmarkdown::render("goosegmse_output.Rmd", output_file = file,
                            envir = new.env(parent = globalenv()))

          
      }
  )
  
  # output$download <- renderUI({
  #     if(exists('sims')) {
  #         downloadButton("report", "Generate report")
  #     }
  # })    
  
}

shinyApp(ui = ui, server = server)