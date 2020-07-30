#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

library(shiny)
library(shinythemes)
library(shinyWidgets)
source('helpers.r')

signal_meta <- apply(covidcast_meta() %>% select(data_source, signal) %>% distinct(), 1, 
                     function(vec){paste(vec, collapse = ": ")})
likely_responses <- c("jhu-csse: confirmed_incidence_num", 
                      "jhu-csse: confirmed_incidence_prop", 
                      "jhu-csse: deaths_incidence_num", 
                      "jhu-csse: deaths_incidence_prop", 
                      "indicator-combination: confirmed_incidence_prop" , 
                      "indicator-combination: deaths_incidence_prop", 
                      "indicator-combination: confirmed_incidence_num",  
                      "indicator-combination: deaths_incidence_num" )

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("united"),
                  
                  # Application title
                  titlePanel("Signal Rank Correlations"),
                  
                  navbarPage("",
                             tabPanel(icon("home"), 
                                      fluidRow(column(
                                        h3("Signal correlations app for Delphi Covid-19 indicators"), 
                                        "These indicators are presented in the ",
                                        tags$a(href="https://covidcast.cmu.edu/", "interactive map"), 
                                        " and you can directly access them via", 
                                        tags$a(href = "https://cmu-delphi.github.io/delphi-epidata/api/covidcast.html", "Delphi's COVIDcast API"), 
                                        ("."),
                                        hr(),
                                        h4("Signals from API"),
                                        p(),
                                        "Here we compute rank correlations between one response signal and multiple sensors available in the 
                                        COVIDcast API, considering averages for user-specified dates. For each specified ", 
                                        code("sensor: signal"), ",",
                                        tags$ol(
                                          tags$li("Average the signal over a ", code("window"), 
                                                  "-day period, from ", code("date - window - lag + 1"), " to ", code("date - lag"), "
                                          for each ", code("grographical granularity"), " in which this indicator is available, giving us a vector x."),
                                          tags$li("Average the ", code("response variable"), " indicator over a ", code("window"), 
                                          "-day period, from ", code("date - window + 1"), " to ", code("date"), "
                                          for each ", code("grographical granularity"), " in which this indicator is available, giving us a vector y."), 
                                          tags$li("Compute the Spearman or rank correlation between x and y for the geographical locations available in both vectors. ")
                                        ),
                                        tags$img(src="corrdiagram.png", width = 350, length = 350),
                                        p(),
                                        "To elaborate, we are actually going to compute the rank correlation for a population sweep cut of x and y:",
                                        tags$ul(
                                          tags$li("A population cut of x and y means that we will further subset these vectors to only consider counties that have population at least p."), 
                                          tags$li("A population sweep cut means that we will consider all population cuts over all possible values of p.")
                                        ),
                                        "In the plots, the horizontal axis shows the population threshold (on a log scale), the vertical axis shows the corresponding rank correlation. ",
                                        "Here is an example of a ", strong("bad correlation plot"),", that is, what the rank correlation plot of two uncorrelated signals looks like. Note that the correlation values are around 0 (red line). A good signal will have high and positive correlations. ",
                                        p(),
                                        tags$img(src="uncorr_toy_plot.png", width = 350, length = 350),
                                        br(),br(),
                                        h4("Upload csv"),
                                        p(),
                                        "Here we compute rank correlations between one response signal and signals from an user-uploaded csv file following the same algorithm from the", 
                                        tags$i("Signals from API"), " tab. The csv file", strong("must have")," the following columns:", 
                                        tags$ul(
                                          tags$li("geo_value: grographical location. Numerical FIPS county, MSA code, lower case state abbreviation;"), 
                                          tags$li("time_value: signal date. String of the format YYYY-MM-DD;"), 
                                          tags$li("value: signal value for specified geographical location and date;"), 
                                          tags$li("source: string with source name;"), 
                                          tags$li("signal: string with signal name.")
                                        ),
                                        p(),
                                        "Here, ", code("window"), " will be considered the number of available days in the csv file and ", 
                                        code("date"), "will be considered the last available signal date ", code("+ lag"), ". ",
                                        width = 12)
                                      )
                             ),
                            
                             tabPanel("Signals from API",
                                      
                                      # Sidebar with a slider input for number of bins
                                      sidebarLayout(
                                        sidebarPanel(
                                          radioGroupButtons(
                                            inputId = "geo_type",
                                            label = h4("Geographical granularity"), 
                                            choices = c("county", "msa", "state"), 
                                            justified = TRUE,
                                            checkIcon = list(
                                              yes = icon("ok", 
                                                         lib = "glyphicon"))
                                          ), 
                                          dateInput("date_var", label = h4("Date for response"), value = "2020-04-17"), 
                                          numericInput("lag", label = h4("Lag of days for correlation"), value = "0"),
                                          numericInput("window", label = h4("Window (number of days to average)"), value = "7"),
                                          pickerInput(
                                            inputId = "response",
                                            label = h4("Response variable"), 
                                            choices = list(
                                              common = likely_responses,
                                              other = signal_meta), 
                                            selected = c("jhu-csse: confirmed_incidence_prop")
                                          ), 
                                          multiInput(
                                            inputId = "signals_to_plot",
                                            label = h4("Signals to correlate with response"), 
                                            choices = signal_meta, 
                                            selected = c("doctor-visits: smoothed_cli",
                                                         "fb-survey: smoothed_cli",
                                                         "fb-survey: smoothed_hh_cmnty_cli",
                                                         "google-survey: smoothed_cli", 
                                                         "ght: smoothed_search", 
                                                         "indicator-combination: nmf_day_doc_fbc_fbs_ght")
                                          ), 
                                          actionButton("runButton", "Create plots")
                                        ),
                                        
                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          awesomeCheckbox(
                                            inputId = "fixedAxis",
                                            label = "Fixed axis", 
                                            value = TRUE
                                          ), 
                                          htmlOutput("description"), 
                                          plotOutput("corrPlot") %>% addSpinner(spin ="fading-circle"), 
                                          downloadButton("download", "Download Plot")
                                        )
                                      )
                             ), 
                             tabPanel("Upload csv", 
                                      # Sidebar with a slider input for number of bins
                                      sidebarLayout(
                                        sidebarPanel(
                                          radioGroupButtons(
                                            inputId = "geo_type_csv",
                                            label = h4("Geographical granularity"), 
                                            choices = c("county", "msa", "state"), 
                                            justified = TRUE,
                                            checkIcon = list(
                                              yes = icon("ok", 
                                                         lib = "glyphicon"))
                                          ), 
                                          numericInput("lag_csv", label = h4("Lag of days for correlation"), value = "0"),
                                          pickerInput(
                                            inputId = "response_csv",
                                            label = h4("Response variable"), 
                                            choices = list(
                                              common = likely_responses,
                                              other = signal_meta), 
                                            selected = c("jhu-csse: confirmed_incidence_prop")
                                          ), 
                                          fileInput(
                                            inputId = "signal_csv",
                                            label = h4("CSV file with signals to correlate with response. Must have columns geo_value, time_value, value, source, signal."), 
                                            accept = c(
                                              "text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")
                                          ), 
                                          actionButton("runButton_csv", "Create plots")
                                        ),
                                        
                                        # Show a plot of the generated distribution
                                        mainPanel(
                                          awesomeCheckbox(
                                            inputId = "fixedAxis_csv",
                                            label = "Fixed axis", 
                                            value = TRUE
                                          ), 
                                          htmlOutput("description_csv"),
                                          plotOutput("corrPlot_csv") %>% addSpinner(spin ="fading-circle"), 
                                          downloadButton("download_csv", "Download Plot")
                                        )
                                      )
                                      
                             )
                  )
))
