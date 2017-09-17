library(shiny)
library(DT)

shinyUI(navbarPage("Anchoveta Web Application: UI Development by Niko Hartline",
                   #########
                   ##ABOUT##
                   #########
                   tabPanel("About",
                            fluidRow(column(1,""),
                                     column(10,align="justified",
                                            h2("Anchoveta Web App Sample"),
                                            p("The Anchoveta Web Application was developed to provide an easily accessible view of economic data and model outputs comparing four different potential management strategies:"),
                                            HTML("<ul><li>Status Quo</li><li>Open Market (OM) Quota</li><li>Closed Market (CM) Quota</li><li>Transferable Quotas</li></ul>"),
                                            p("The web app was designed for a user to control a wide range of variables affecting Peruvian anchovy fisheries such as quota levels, operational costs, and discard (waste) rates. Changes to these variables would then influence the outcomes of the fisheries under the four presented management strategies, allowing a user (purposed for use by the Peruvian government) to view economic effects of possible policy options to reduce likelihood of fisheries collapse."),
                                            p(strong("The application as displayed here is only a partial representation of the app, showcasing the work I contributed to the Anchoveta Fish App. Collaborators included Juliano Palacios and Renato Molina.")),
                                            p(strong("The app was originally developed in Spanish for use in Peru. In this version of the app, titles and input boxes have been translated to English.")),
                                            hr(),br()
                                     ),
                                     column(1,"")
                            )
                   ),
                   ###########
                   ##Summary##
                   ###########
                   tabPanel("Summary",
                            fluidRow(column(1,""),
                                     column(12,align="center",textInput("autor", label="Author", value = "")),
                                     column(12,align="center",downloadButton('downloadReport',label="Download Report"))),
                            p(),
                            selectInput(
                              "Summary", "Select a Variable",
                              c(Valor = "Value",
                                Empleo = "Labor",
                                Produccion = "Production")),
                            fluidRow(column(6,align="center",h4("Harvest Sector"),
                                            conditionalPanel(
                                              condition = "input.Summary == 'Value'",plotOutput("plotFsummary1")),
                                            conditionalPanel(
                                              condition = "input.Summary == 'Labor'",plotOutput("plotFsummary2")
                                            ),
                                            conditionalPanel(
                                              condition = "input.Summary == 'Production'",plotOutput("plotFsummary3")
                                            )
                            ),
                            column(6,align="center",h4("Processing Sector"),
                                   conditionalPanel(
                                     condition = "input.Summary == 'Value'",plotOutput("plotPsummary1")),
                                   conditionalPanel(
                                     condition = "input.Summary == 'Labor'",plotOutput("plotPsummary2")
                                   ),
                                   conditionalPanel(
                                     condition = "input.Summary == 'Production'",plotOutput("plotPsummary3")
                                   )))
                   ),
                   ##############
                   ##Processing##
                   ##############
                   tabPanel("Processing Sector",align="center",h2("Control Panel"),
                            fluidRow(
                              #########
                              ##INPUTS#
                              #########
                              column(6,align="center",h3("Inputs"),
                                     tabsetPanel(type="tabs",
                                                 tabPanel("Discard Rates", align="left",
                                                          p("En esta sección usted podrá especificar el porcentaje (%) de la captura que se descartará para Consumo Humano Indirecto (CHI). También podrá determinar qué porcentaje de Produccion se destina a cada uno de los productos finales derivados de anchoveta."),
                                                          p("El valor automático está en máximo descarte"),
                                                          hr(),
                                                          p("Por favor seleccione niveles apropiados de:" ),
                                                          
                                                          sliderInput("LandingDiscard", "Percent of Catch Discarded (%):", 10,
                                                                      min = 0, max = 10),
                                                          
                                                          sliderInput("ProcDiscard", "Percent of Processing Discarded (%):", 40,
                                                                      min = 0, max = 40)
                                                 ),
                                                 tabPanel("Processing Breakdown", align="left",
                                                          p("En esta sección usted podrá especificar el porcentaje de la captura designado a cada producto final de consumo humano directo."),
                                                          p("El valor automático está ajustado a la distribución actual de los productos."),
                                                          hr(),
                                                          p("Por favor seleccione niveles apropiados de:"),
                                                          
                                                          radioButtons("ProcessPctg", label = h4("Percent Canned, Frozen, Cured, or Fresh"),
                                                                       choices = list("Enlatado (79%), Congelado (10%), Curado (10%), y Fresco/Salado (0.4%)" = T, "Enlatado (25%), Congelado (25%), Curado (25%), y Fresco/Salado (25%)" = F),selected = T))
                                     ) ),
                              column(6,align="center",h3("Outputs"),
                                     ###########
                                     ##OUTPUTS##
                                     ###########
                                     tabsetPanel(type="tabs",
                                                 tabPanel("Consumer Product Sector",align='left',
                                                          hr(),
                                                          em("Status Quo: Libre acceso para embarcaciones CHD "),br(),
                                                          em("Cuota MC: Cuota con limitaciones de mercado"),br(),
                                                          em("Cuota MA: Cuota sin limitaciones de mercado"),br(),
                                                          em("Transferible: Cuotas transferibles"),
                                                          selectInput(
                                                            "Processing", "Seleccione un tipo de salida",
                                                            c(Valor = "Value",
                                                              Empleo = "Labor",
                                                              Produccion = "Production")),
                                                          conditionalPanel(
                                                            condition = "input.Processing == 'Value'",
                                                            plotOutput("plotProcessing1"),
                                                            dataTableOutput('tableProcessing1'),
                                                            em("(Todos los valores en millones de USD)")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Processing == 'Labor'",
                                                            plotOutput("plotProcessing2"),
                                                            dataTableOutput('tableProcessing2'),
                                                            em("(Todos los valores en miles de puestos de trabajo)")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Processing == 'Production'",
                                                            plotOutput("plotProcessing3"),
                                                            dataTableOutput('tableProcessing3'),
                                                            em("(Todos los valores en millones de toneladas)")
                                                          )
                                                 ),
                                                 tabPanel("Artisanal Fishmeal and Oil",align='left',
                                                          hr(),
                                                          em("Status Quo: Libre acceso para embarcaciones CHD "),br(),
                                                          em("Cuota MC: Cuota con limitaciones de mercado"),br(),
                                                          em("Cuota MA: Cuota sin limitaciones de mercado"),br(),
                                                          em("Transferible: Cuotas transferibles"),
                                                          selectInput(
                                                            "Discard", "Seleccione un tipo de salida",
                                                            c(Valor = "Value",
                                                              Empleo = "Labor",
                                                              Produccion = "Production")),
                                                          conditionalPanel(
                                                            condition = "input.Discard == 'Value'",
                                                            plotOutput("plotDiscard1"),
                                                            dataTableOutput('tableDiscard1'),
                                                            em("(Todos los valores en millones de USD)")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Discard == 'Labor'",
                                                            plotOutput("plotDiscard2"),
                                                            dataTableOutput('tableDiscard2'),
                                                            em("(Todos los valores en miles de puestos de trabajo)")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Discard == 'Production'",
                                                            plotOutput("plotDiscard3"),
                                                            dataTableOutput('tableDiscard3'),
                                                            em("(Todos los valores en miles de toneladas)")
                                                          )
                                                 ),
                                                 tabPanel("Industrial Fishmeal and Oil",align='left',
                                                          hr(),
                                                          em("Status Quo: Libre acceso para embarcaciones CHD "),br(),
                                                          em("Cuota MC: Cuota con limitaciones de mercado"),br(),
                                                          em("Cuota MA: Cuota sin limitaciones de mercado"),br(),
                                                          em("Transferible: Cuotas transferibles"),
                                                          selectInput(
                                                            "Industrial", "Seleccione un tipo de salida",
                                                            c(Valor = "Value",
                                                              Empleo = "Labor",
                                                              Produccion = "Production")),
                                                          conditionalPanel(
                                                            condition = "input.Industrial == 'Value'",
                                                            plotOutput("plotIndustrial1")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Industrial == 'Labor'",
                                                            plotOutput("plotIndustrial2")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.Industrial == 'Production'",
                                                            plotOutput("plotIndustrial3")
                                                          )
                                                 ),
                                                 tabPanel("Summary",align='left',
                                                          hr(),
                                                          em("Status Quo: Libre acceso para embarcaciones CHD "),br(),
                                                          em("Cuota MC: Cuota con limitaciones de mercado"),br(),
                                                          em("Cuota MA: Cuota sin limitaciones de mercado"),br(),
                                                          em("Transferible: Cuotas transferibles"),
                                                          p(),
                                                          checkboxGroupInput("ProcessingSummary", label = "Seleccione los productos",
                                                                             choices = list("Enlatado" = "PC", "Congelado" = "PF", "Curado" = "PU","Fresco/Salado"="PS","Harina Artesanal de Alta Calidad"="DH","Harina Artesanal de Baja Calidad"="DL","Aceite Artesanal"="DA","Harina Industrial"="FM","Aceite Industrial"="FO"),
                                                                             selected = "PC"),
                                                          selectInput(
                                                            "PSummary", "Seleccione un tipo de salida",
                                                            c(Valor = "Value",
                                                              Empleo = "Labor",
                                                              Produccion = "Production")),
                                                          conditionalPanel(
                                                            condition = "input.PSummary == 'Value'",
                                                            plotOutput("plotPSUMM1"),
                                                            dataTableOutput('tableSUMM1'),
                                                            em("(Todos los valores en millones de USD)")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.PSummary == 'Labor'",
                                                            plotOutput("plotPSUMM2"),
                                                            dataTableOutput('tableSUMM2'),
                                                            em("(Todos los valores en miles de puestos de trabajo)")
                                                          ),
                                                          conditionalPanel(
                                                            condition = "input.PSummary == 'Production'",
                                                            plotOutput("plotPSUMM3"),
                                                            dataTableOutput('tableSUMM3'),
                                                            em("(Todos los valores en millones de toneladas)")
                                                          )
                                                 )
                                     )
                              )
                            )
                   )
                   
                   
))

