#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#

library(shiny)
source("helpers.r")

shinyServer(function(input, output) {
    
    ## getting population mapping
    ## only do this once, when the app starts
    population_mapping <- get_population()
    
    ############
    ## first tab: signals from the API
    ############
    ## initialize corrPlot to make sure Spinner will only run after the button is pressed
    output$corrPlot <- renderPlot(NULL)
    
    ## getting reactive inputs
    ## reactive events values will be cached and only invalidated and recomputed once runButton is clicked on
    descr <- eventReactive(input$runButton, {
        ## writing description of plots and signals in the plot
        descr <- paste("Correlating ", input$response, 
                       "averaged from ", as.character(input$date_var - input$window + 1), " to ",
                       as.character(input$date_var), " at ", input$geo_type, " level against:<ul>")
        for(i in 1:length(input$signals_to_plot)){
            descr <- paste(descr, "<li>", input$signals_to_plot[i], " from ", char_to_date(as.character(input$date_var - input$window - input$lag + 1)), 
                           " to ", char_to_date(as.character(input$date_var - input$lag)), "</li>", sep ="")
        }
        descr <- paste(descr, "</ul>", sep = "")
        descr
    })
    response <- eventReactive(input$runButton, {
        ## getting response variable from API
        resp <- strsplit(input$response, split = ": ") %>% unlist()
        get_response(resp, char_to_date(as.character(input$date_var)), input$window, input$geo_type)
    })
    sensors <- eventReactive(input$runButton, {
        ## getting sensors from API 
        signals_to_plot <- strsplit(input$signals_to_plot, split = ": ") %>% unlist() %>% matrix(ncol = 2, byrow = T)
        get_sensors(signals_to_plot, char_to_date(as.character(input$date_var)), input$lag, input$window, input$geo_type)
    })
    geo_type <- eventReactive(input$runButton, {
        ## saving geo_type so it doesn't change when the user clicks on it; it has to wait until the button is clicked on
        input$geo_type
    })
    
    ## finally, script that is re-run when button is clicked but also re-runs every time the input$* that are called in the script changes 
    plot_to_show <- reactiveValues()
    observeEvent(input$runButton, {
        plot_to_show$plot <- plot_corr(sensors(), response(), population_mapping, geo_type(), !input$fixedAxis)
        output$description <- renderText({descr()})
        output$corrPlot <- renderPlot({plot_to_show$plot})
    }, ignoreInit = TRUE)
    output$download <- downloadHandler(
        filename = function(){paste('corrPlot-', Sys.Date(), '.png', sep='')},
        content = function(file) {ggsave(file, plot = plot_to_show$plot)}
    )
    
    
    
    ############
    ## second tab: signals from csv file
    ############
    ## initialize corrPlot to make sure Spinner will only run after the button is pressed
    output$corrPlot_csv <- renderPlot(NULL)
    
    geo_type_csv <- eventReactive(input$runButton_csv, {
        ## saving geo_type so it doesn't change when the user clicks on it; it has to wait until the button is clicked on
        input$geo_type_csv
    })
    ## getting reactive inputs
    ## reactive events values will be cached and only invalidated and recomputed once runButton is clicked on
    signals_df <- eventReactive(input$runButton_csv, {
        ## gets uploaded file and reads csv
        signals_df <- read.csv(input$signal_csv$datapath, header = T) %>% select(geo_value, value, time_value, source, signal)
        ## check that colnames on uploaded file are correct
        if(sum(names(signals_df) %in% c("geo_value", "time_value", "value", "source", "signal")) != 5) stop("file does not have correct column names")
        
        if(geo_type_csv() %in% c("county", "msa")) signals_df$geo_value <- sprintf("%05d", signals_df$geo_value)
        
        signals_df
    })
    date_var_csv <- eventReactive(input$runButton_csv, {
        ## sets date_var based on max available date and lag input
        max(as.Date(signals_df()$time_value)) + input$lag_csv
    })
    window_csv <- eventReactive(input$runButton_csv, {
        ## sets window based on date col of csv input
        length(unique(as.Date(signals_df()$time_value)))
    })
    descr_csv <- eventReactive(input$runButton_csv, {
        ## writing description of plots and signals in the plot
        signal_names <- apply(signals_df() %>% select(source, signal), 1, paste, collapse = ": ") %>% unique()
        descr <- paste("Correlating ", input$response_csv, 
                       "averaged from ", as.character(date_var_csv() - window_csv() + 1), " to ",
                       as.character(date_var_csv()), " at ", input$geo_type_csv, " level against:<ul>")
        for(i in 1:length(signal_names)){
            descr <- paste(descr, "<li>", signal_names[i], " from ", char_to_date(as.character(date_var_csv() - window_csv() - input$lag_csv + 1)), 
                           " to ", char_to_date(as.character(date_var_csv() - input$lag_csv)), "</li>", sep ="")
        }
        descr <- paste(descr, "</ul>", sep = "")
        descr
    })
    response_csv <- eventReactive(input$runButton_csv, {
        ## getting response variable from API
        resp <- strsplit(input$response_csv, split = ": ") %>% unlist()
        get_response(resp, char_to_date(as.character(date_var_csv())), window_csv(), input$geo_type_csv)
    })
    sensors_csv <- eventReactive(input$runButton_csv, {
        ## treats dataframe from uploaded file in signals_df() to get averages
        signals_df() %>% 
            select(geo_value, time_value, value, source, signal) %>% 
            group_by(geo_value, source, signal) %>% 
            summarise(avgsignal = mean(value))
    })
    
    ## finally, script that is re-run when button is clicked but also re-runs every time the input$* that are called in the script changes 
    plot_to_show_csv <- reactiveValues()
    observeEvent(input$runButton_csv, {
        plot_to_show_csv$plot <- plot_corr(sensors_csv(), response_csv(), population_mapping, geo_type_csv(), !input$fixedAxis_csv)
        output$description_csv <- renderText({descr_csv()})
        output$corrPlot_csv <- renderPlot({plot_to_show_csv$plot})
        output$download_csv <- downloadHandler(
            filename = function(){paste('corrPlot-', Sys.Date(), '.png', sep='')},
            content = function(file) {ggsave(file, plot = plot_to_show_csv$plot)}
        )
    }, ignoreInit = TRUE)
    
})