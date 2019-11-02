# ---
# Titulo: "Analisis de accidentalidad en el municipio de Medellin."
# Autores:"ALEJANDRA GRACIANO"
#         "JULIAN QUINTANA"
#         "SERGIO VALENCIA"
# Fecha: "12/10/2019"
# ---

library(shiny)
library(leaflet)
library(sf)
library(dplyr, warn.conflicts = FALSE)


Accidentalidad <- read.csv("Accidentalidad_Total_3.csv",header=TRUE, sep=",")

Accidentalidad$HORA <- gsub(" ","",Accidentalidad$HORA,fixed=TRUE)
Accidentalidad$HORA <- gsub(".","",Accidentalidad$HORA,fixed=TRUE)
Accidentalidad$HORA <- gsub("am","AM",Accidentalidad$HORA,fixed=TRUE)
Accidentalidad$HORA <- gsub("pm","PM",Accidentalidad$HORA,fixed=TRUE)
Accidentalidad$Indicacion <- substr(Accidentalidad$HORA,nchar(as.character(Accidentalidad$HORA)) - 1,nchar(as.character(Accidentalidad$HORA)))
Accidentalidad$HORA <- substr(Accidentalidad$HORA, 1,nchar(as.character(Accidentalidad$HORA))-2)
Accidentalidad$HORA <- ifelse(nchar(as.character(Accidentalidad$HORA))==7,substr(Accidentalidad$HORA, 1,4),Accidentalidad$HORA)
Accidentalidad$HORA <- ifelse(nchar(as.character(Accidentalidad$HORA))==8,substr(Accidentalidad$HORA, 1,5),Accidentalidad$HORA)
Accidentalidad$HORA2 <- substr(Accidentalidad$HORA, 1, nchar(as.character(Accidentalidad$HORA))-3)
Accidentalidad$HORA2 <- ifelse((Accidentalidad$Indicacion == "PM" & as.numeric(Accidentalidad$HORA2) != 12),as.numeric(Accidentalidad$HORA2) + 12,Accidentalidad$HORA2)
Accidentalidad$HORA2 <- ifelse((Accidentalidad$Indicacion == "AM" & as.numeric(Accidentalidad$HORA2) == 12),as.numeric(Accidentalidad$HORA2) - 12,Accidentalidad$HORA2)

Accidentalidad$COMUNA <- gsub("Corregimiento de ","",Accidentalidad$COMUNA,fixed=TRUE)
Accidentalidad$COMUNA <- gsub("de ","",Accidentalidad$COMUNA,fixed=TRUE)

annos <- Accidentalidad$f_accidente
annos <- as.Date(annos, "%Y")
annos <- format(annos,"%Y")
annos <- unique(annos)

# Define UI for application
ui <- fluidPage(
    
    h1(span("Análisis de la accidentalidad en el Municipio de Medellín", style = "font-weight: 600"), 
       style = "font-family: 'Source Sans Pro';
        color: #fff; text-align: center;
        background-color:#4682B4;
        padding: 35px"),
    br(),
    
    sidebarLayout(
        column(width = 3,
            wellPanel(style = "background: #F0F8FF",
                selectInput("Comuna_Seleccionada","Comuna a analizar:",c("Seleccione una comuna:",unique(as.character(Accidentalidad$COMUNA)))),
                selectInput("Barrio_Seleccionado","Barrio a analizar:",c("Seleccione un barrio:",unique(as.character(Accidentalidad$BARRIO)))),
                selectInput("Mes_Analizado","Mes a analizar:",c("Seleccione un mes:",unique(as.character(unique(Accidentalidad$MES))))),
                selectInput("Anno_Analizado","Año a analizar:",c("Seleccione un año:",unique(as.character(annos)))),
                dateRangeInput('Fechas_Seleccionadas',label = 'Filtrar entre fecha de accidentes.',start = min(as.Date(Accidentalidad$f_accidente)) , end = max(as.Date(Accidentalidad$f_accidente))),
                sliderInput("Horas_Seleccionada", "Horas Accidentes", 0, 24, value = c(0, 24),sep = "")
            ),
            
            wellPanel(style = "background: #F0F8FF",
                radioButtons("Condicion","Seleccione la condicion especial a analizar:",
                             choices = c("Ninguna." = "N", "Analizar por mes." = "M","No considerar festivos." = "F","Vacaciones final año (15 Dic - 15 Ene)." = "VF","Vacaciones mitad año (15 Jun - 15 Jul)." = "VM"),selected = "N")
            )
        ),
    
        mainPanel(
            column(width = 12,
                wellPanel(style = "background: #F0F8FF",
                    fluidRow(
                        column(width = 12,
                            plotOutput(outputId = "Grafico4", height = "400px")
                        )
                    )
                ),
                wellPanel(style = "background: #F0F8FF",
                    fluidRow(
                        column(width = 6,
                            plotOutput(outputId = "Grafico", height = "400px")
                        ),
                        column(width = 6,
                            leafletOutput("Mapa",height = "400px")
                        )
                    )
                ),
                wellPanel(style = "background: #F0F8FF",
                    fluidRow(
                        column(width = 6,
                               plotOutput(outputId = "Grafico2", height = "400px")
                        ),
                        column(width = 6,
                               plotOutput(outputId = "Grafico3", height = "400px")
                        )
                    )
                ),
                wellPanel(style = "background: #F0F8FF",
                    fluidRow(
                        DT::dataTableOutput("table")
                    )
                )
            )
        )
    ),
    
    h3(span("Accidentalidad en el Municipio de Medellín", style = "font-weight: 600"), 
       style = "font-family: 'Source Sans Pro';
        color: #000000; text-align: center;
        background-color:#ADD8E6;
        padding: 35px"),
    
    h4(span("Realizado por: Alejandra Graciano, Julián Muñoz, Sergio Valencia", style = "font-weight: 200"), 
       style = "font-family: 'Source Sans Pro';
        color: #000000; text-align: center;
        background-color:#87CEFA;
        padding: 35px"),
    br()
)

# Define server logic required
server <- function(input, output,session) {
    
    # Filtrar información con base en los filtros
    observe({
        
        data <- Accidentalidad
        
        # Indicar las fechas seleccionadas con los filtros
        if (input$Mes_Analizado != "Seleccione un mes:") {
            data <- data[data$MES == input$Mes_Analizado,]
        }
        
        if (input$Anno_Analizado != "Seleccione un año:") {
            data <- data[data$PERIODO == input$Anno_Analizado,]
            
            updateDateRangeInput(session, "Fechas_Seleccionadas", 
                                 label = "Filtrar entre fecha de accidentes.", 
                                 start = min(as.Date(data$f_accidente)),
                                 end = max(as.Date(data$f_accidente)))
        }
        
        # Indicar si no existe los filtros principales, generar el filtro por rango
        if (input$Anno_Analizado == "Seleccione un año:" & input$Mes_Analizado == "Seleccione un mes:") {
            dateRangeInput('Fechas_Seleccionadas',
                           label = 'Filtrar entre fecha de accidentes.',
                           start = min(as.Date(Accidentalidad$f_accidente)) , end = max(as.Date(Accidentalidad$f_accidente)))
        }
        
        
        # indicar los barrios de la comuna seleccionada.
        if (input$Comuna_Seleccionada != "Seleccione una comuna:" & input$Barrio_Seleccionado == "Seleccione un barrio:") {
            
            data <- data[data$COMUNA == input$Comuna_Seleccionada,]
            
            # Indicar la informacion de la comuna seleccionada
            updateSelectInput(session,"Barrio_Seleccionado",
                              "Barrio a analizar:",
                              c("Seleccione un barrio:", unique(as.character(data$BARRIO))),
                              selected = NULL)
            
            # Indicar la información cuando no se tiene comuna seleccionada
        } else if (input$Comuna_Seleccionada == "Seleccione una comuna:" & input$Barrio_Seleccionado == "Seleccione un barrio:") {

            updateSelectInput(session,"Barrio_Seleccionado",
                              "Barrio a analizar:",
                              c("Seleccione un barrio:", unique(as.character(Accidentalidad$BARRIO))),
                              selected = NULL)
            
        } else if (input$Comuna_Seleccionada == "Seleccione una comuna:" & input$Barrio_Seleccionado != "Seleccione un barrio:") {
            
            updateSelectInput(session,"Barrio_Seleccionado",
                              "Barrio a analizar:",
                              c(input$Barrio_Seleccionado, unique(as.character(Accidentalidad$BARRIO))),
                              selected = NULL)
        }
        
    })
    
    
    # Filter data based on selections
    output$table <- DT::renderDataTable(DT::datatable({
        
        data <- Accidentalidad
        
        # Analisis condiciones especiales
        if (input$Condicion == "N") {
            
        } else if (input$Condicion == "F") {
            data <- data[(data$Festivo == "No Festivo"),]
            
        } else if (input$Condicion == "VF") {
            data <- data[(data$MES == 12 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 1 & data$DIA >= 1 & data$DIA <= 15),]
            
        } else if (input$Condicion == "VM") {
            data <- data[(data$MES == 6 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 7 & data$DIA >= 1 & data$DIA <= 15),]
        
        }
        
        
        if (input$Comuna_Seleccionada != "Seleccione una comuna:") {
            data <- data[data$COMUNA == input$Comuna_Seleccionada,]
        }
        
        if (input$Barrio_Seleccionado != "Seleccione un barrio:") {
            data <- data[data$BARRIO == input$Barrio_Seleccionado,]
        }
        
        # Indicar el rango de fechas seleccionadas.
        Fecha_Inicial <- as.Date(min(input$Fechas_Seleccionadas), origin = "1970-01-01")
        Fecha_Final <- as.Date(max(input$Fechas_Seleccionadas), origin = "1970-01-01")
        
        if (input$Condicion != "M") {
            data <- data[(as.Date(data$f_accidente) >= Fecha_Inicial & as.Date(data$f_accidente) <= Fecha_Final),]
            
        } else if (input$Condicion == "M") {
            if (input$Anno_Analizado != "Seleccione un año:") {
                data <- data[(data$PERIODO == input$Anno_Analizado),]
            } else {
                
            }
            
            if (input$Mes_Analizado != "Seleccione un mes:") {
                data <- data[(data$MES == input$Mes_Analizado),]
            } else {
                
            }
            
        }
        
        # Indicar las horas seleccionadas
        Hora_Inicial <- input$Horas_Seleccionada[1]
        Hora_Final <- input$Horas_Seleccionada[2]
        
        data <- data[(as.numeric(data$HORA2) >= Hora_Inicial & as.numeric(data$HORA2) <= Hora_Final),]
        data$Longitud <- substr(data$Longitud, 1, 10)
        data$Latitud <- substr(data$Latitud, 1, 10)
        data <- subset(data,select = c("f_accidente","BARRIO","COMUNA","DIRECCION","CLASE","GRAVEDAD","DIA_NOMBRE","HORA","Indicacion","Latitud","Longitud"))
        
        names(data) <- c("FECHA","BARRIO","COMUNA","DIRECCION","CLASE","GRAVEDAD","DIA","HORA","AM/PM","LATITUD","LONGITUD")
        
        data
    }))
    
    
    # Dibujar el grafico de barra
    output$Grafico <- renderPlot({
        
        data <- Accidentalidad
        
        # Analisis condiciones especiales
        if (input$Condicion == "N") {
            
        } else if (input$Condicion == "F") {
            data <- data[(data$Festivo == "No Festivo"),]
            
        } else if (input$Condicion == "VF") {
            data <- data[(data$MES == 12 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 1 & data$DIA >= 1 & data$DIA <= 15),]
            
        } else if (input$Condicion == "VM") {
            data <- data[(data$MES == 6 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 7 & data$DIA >= 1 & data$DIA <= 15),]
            
        }
        
        if (input$Comuna_Seleccionada != "Seleccione una comuna:") {
            data <- data[data$COMUNA == input$Comuna_Seleccionada,]
        }
        
        if (input$Barrio_Seleccionado != "Seleccione un barrio:") {
            data <- data[data$BARRIO == input$Barrio_Seleccionado,]
        }
        
        # Indicar el rango de fechas seleccionadas.
        Fecha_Inicial <- as.Date(min(input$Fechas_Seleccionadas), origin = "1970-01-01")
        Fecha_Final <- as.Date(max(input$Fechas_Seleccionadas), origin = "1970-01-01")
        
        if (input$Condicion != "M") {
            data <- data[(as.Date(data$f_accidente) >= Fecha_Inicial & as.Date(data$f_accidente) <= Fecha_Final),]
        
        } else if (input$Condicion == "M") {
            if (input$Anno_Analizado != "Seleccione un año:") {
                data <- data[(data$PERIODO == input$Anno_Analizado),]
            } else {

            }
            
            if (input$Mes_Analizado != "Seleccione un mes:") {
                data <- data[(data$MES == input$Mes_Analizado),]
            } else {
                
            }
            
        }
        
        
        # Indicar las horas seleccionadas
        Hora_Inicial <- input$Horas_Seleccionada[1]
        Hora_Final <- input$Horas_Seleccionada[2]
        
        data <- data[(as.numeric(data$HORA2) >= Hora_Inicial & as.numeric(data$HORA2) <= Hora_Final),]
        
        if (input$Condicion == "M") {
            Conteo <- aggregate(X~PERIODO+MES,data=data,FUN="length")
            Conteo2 <- table(data$MES,data$PERIODO)
            limite_y = max(Conteo$X)
            barplot(Conteo2,beside=TRUE, las = 1, main = "Accidentalidad Semanal vs Periodo Analizado.",
                    xlab = "Periodo anual.", ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = unique(Conteo$MES), args.legend=list(x = "topright", bty="n"))
        } else {
            Conteo <- aggregate(X~PERIODO+DIA_NOMBRE,data=data,FUN="length")
            Conteo2 <- table(data$DIA_NOMBRE,data$PERIODO)
            limite_y = max(Conteo$X)
            barplot(Conteo2,beside=TRUE, las = 1, main = "Accidentalidad Semanal vs Periodo Analizado.",
                    xlab = "Periodo anual.", ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = unique(Conteo$DIA_NOMBRE), args.legend=list(x = "topright", bty="n"))
        }
        

        
    })
    
    # Dibujar el grafico de barra
    output$Grafico2 <- renderPlot({
        
        data <- Accidentalidad
        
        # Analisis condiciones especiales
        if (input$Condicion == "N") {
            
        } else if (input$Condicion == "F") {
            data <- data[(data$Festivo == "No Festivo"),]
            
        } else if (input$Condicion == "VF") {
            data <- data[(data$MES == 12 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 1 & data$DIA >= 1 & data$DIA <= 15),]
            
        } else if (input$Condicion == "VM") {
            data <- data[(data$MES == 6 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 7 & data$DIA >= 1 & data$DIA <= 15),]
            
        }
        
        if (input$Comuna_Seleccionada != "Seleccione una comuna:") {
            data <- data[data$COMUNA == input$Comuna_Seleccionada,]
        }
        
        if (input$Barrio_Seleccionado != "Seleccione un barrio:") {
            data <- data[data$BARRIO == input$Barrio_Seleccionado,]
        }
        
        # Indicar el rango de fechas seleccionadas.
        Fecha_Inicial <- as.Date(min(input$Fechas_Seleccionadas), origin = "1970-01-01")
        Fecha_Final <- as.Date(max(input$Fechas_Seleccionadas), origin = "1970-01-01")
        
        if (input$Condicion != "M") {
            data <- data[(as.Date(data$f_accidente) >= Fecha_Inicial & as.Date(data$f_accidente) <= Fecha_Final),]
            
        } else if (input$Condicion == "M") {
            if (input$Anno_Analizado != "Seleccione un año:") {
                data <- data[(data$PERIODO == input$Anno_Analizado),]
            } else {
                
            }
            
            if (input$Mes_Analizado != "Seleccione un mes:") {
                data <- data[(data$MES == input$Mes_Analizado),]
            } else {
                
            }
            
        }
        
        # Indicar las horas seleccionadas
        Hora_Inicial <- input$Horas_Seleccionada[1]
        Hora_Final <- input$Horas_Seleccionada[2]
        
        data <- data[(as.numeric(data$HORA2) >= Hora_Inicial & as.numeric(data$HORA2) <= Hora_Final),]
        
        if (input$Condicion == "M") {
            Conteo <- aggregate(X~MES+CLASE,data=data,FUN="length")
            Conteo2 <- table(data$CLASE,data$MES)
            limite_y = max(Conteo$X)
            barplot(Conteo2,beside=TRUE, las = 1, main = "Accidentalidad por Tipo vs Periodo Analizado.",
                    xlab = "Periodo mensual.", ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = unique(Conteo$CLASE), args.legend=list(x = "topright", bty="n"))
        } else {
            Conteo <- aggregate(X~PERIODO+CLASE,data=data,FUN="length")
            Conteo2 <- table(data$CLASE,data$PERIODO)
            limite_y = max(Conteo$X)
            barplot(Conteo2,beside=TRUE, las = 1, main = "Accidentalidad por Tipo vs Periodo Analizado.",
                    xlab = "Periodo anual.", ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = unique(Conteo$CLASE), args.legend=list(x = "topright", bty="n"))
        }
        
    })
    
    # Dibujar el grafico de barra
    output$Grafico3 <- renderPlot({
        
        data <- Accidentalidad
        
        # Analisis condiciones especiales
        if (input$Condicion == "N") {
            
        } else if (input$Condicion == "F") {
            data <- data[(data$Festivo == "No Festivo"),]
            
        } else if (input$Condicion == "VF") {
            data <- data[(data$MES == 12 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 1 & data$DIA >= 1 & data$DIA <= 15),]
            
        } else if (input$Condicion == "VM") {
            data <- data[(data$MES == 6 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 7 & data$DIA >= 1 & data$DIA <= 15),]
            
        }
        
        if (input$Comuna_Seleccionada != "Seleccione una comuna:") {
            data <- data[data$COMUNA == input$Comuna_Seleccionada,]
        }
        
        if (input$Barrio_Seleccionado != "Seleccione un barrio:") {
            data <- data[data$BARRIO == input$Barrio_Seleccionado,]
        }
        
        # Indicar el rango de fechas seleccionadas.
        Fecha_Inicial <- as.Date(min(input$Fechas_Seleccionadas), origin = "1970-01-01")
        Fecha_Final <- as.Date(max(input$Fechas_Seleccionadas), origin = "1970-01-01")
        
        # Indicar las horas seleccionadas
        Hora_Inicial <- input$Horas_Seleccionada[1]
        Hora_Final <- input$Horas_Seleccionada[2]
        
        data <- data[(as.numeric(data$HORA2) >= Hora_Inicial & as.numeric(data$HORA2) <= Hora_Final),]
        
        if (input$Condicion != "M") {
            data <- data[(as.Date(data$f_accidente) >= Fecha_Inicial & as.Date(data$f_accidente) <= Fecha_Final),]
            
        } else if (input$Condicion == "M") {
            if (input$Anno_Analizado != "Seleccione un año:") {
                data <- data[(data$PERIODO == input$Anno_Analizado),]
            } else {
                
            }
            
            if (input$Mes_Analizado != "Seleccione un mes:") {
                data <- data[(data$MES == input$Mes_Analizado),]
            } else {
                
            }
            
        }
        
        
        if (input$Condicion == "M") {
            Conteo <- aggregate(X~MES+GRAVEDAD,data=data,FUN="length")
            Conteo2 <- table(data$GRAVEDAD,data$MES)
            limite_y = max(Conteo$X)
            barplot(Conteo2,beside=TRUE, las = 1, main = "Gravedad del Accidente vs Periodo Analizado.",
                    xlab = "Periodo mensual.", ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = unique(Conteo$GRAVEDAD), args.legend=list(x = "topright", bty="n"))
        
            } else {
            Conteo <- aggregate(X~PERIODO+GRAVEDAD,data=data,FUN="length")
            Conteo2 <- table(data$GRAVEDAD,data$PERIODO)
            limite_y = max(Conteo$X)
            barplot(Conteo2,beside=TRUE, las = 1, main = "Gravedad del Accidente vs Periodo Analizado.",
                    xlab = "Periodo anual.", ylab = "Numero de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = unique(Conteo$GRAVEDAD), args.legend=list(x = "topright", bty="n"))
        }
        
    })
    
    output$Grafico4 <- renderPlot({
        
        data <- Accidentalidad
        
        # Analisis condiciones especiales
        if (input$Condicion == "N") {
            
        } else if (input$Condicion == "F") {
            data <- data[(data$Festivo == "No Festivo"),]
            
        } else if (input$Condicion == "VF") {
            data <- data[(data$MES == 12 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 1 & data$DIA >= 1 & data$DIA <= 15),]
            
        } else if (input$Condicion == "VM") {
            data <- data[(data$MES == 6 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 7 & data$DIA >= 1 & data$DIA <= 15),]
            
        }
        
        # Indicar el rango de fechas seleccionadas.
        Fecha_Inicial <- as.Date(min(input$Fechas_Seleccionadas), origin = "1970-01-01")
        Fecha_Final <- as.Date(max(input$Fechas_Seleccionadas), origin = "1970-01-01")
        
        # Indicar las horas seleccionadas
        Hora_Inicial <- input$Horas_Seleccionada[1]
        Hora_Final <- input$Horas_Seleccionada[2]
        
        data <- data[(as.numeric(data$HORA2) >= Hora_Inicial & as.numeric(data$HORA2) <= Hora_Final),]
        
        if (input$Condicion != "M") {
            data <- data[(as.Date(data$f_accidente) >= Fecha_Inicial & as.Date(data$f_accidente) <= Fecha_Final),]
            
        } else if (input$Condicion == "M") {
            if (input$Anno_Analizado != "Seleccione un año:") {
                data <- data[(data$PERIODO == input$Anno_Analizado),]
            } else {
                
            }
            
            if (input$Mes_Analizado != "Seleccione un mes:") {
                data <- data[(data$MES == input$Mes_Analizado),]
            } else {
                
            }
            
        }
        
        if (input$Condicion == "M") {
            Conteo <- aggregate(X~COMUNA,data=data,FUN="length")
            Conteo3 <- aggregate(X~COMUNA+MES,data=data,FUN="length")
            Conteo2 <- table(data$MES,data$COMUNA)
            limite_y = max(Conteo$X)
            par(mar=c(10, 6, 3, 1))
            barplot(Conteo2, las = 2, main = "Número de accidentes vs Comunas.",
                    ylab = "Número de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = row.names(Conteo2), args.legend=list(x = "topright", bty="n"))
            
        } else {
            Conteo <- aggregate(X~COMUNA,data=data,FUN="length")
            Conteo3 <- aggregate(X~COMUNA+PERIODO,data=data,FUN="length")
            Conteo2 <- table(data$PERIODO,data$COMUNA)
            limite_y = max(Conteo$X)
            par(mar=c(10, 6, 3, 1))
            barplot(Conteo2, las = 2, main = "Número de accidentes vs Comunas.",
                    ylab = "Número de accidentes.", ylim = c(0,limite_y*1.1),
                    legend.text = row.names(Conteo2), args.legend=list(x = "topright", bty="n"))
        }
        
    })
    
    
    # Dibujar el mapa con los accidentes
    output$Mapa <- renderLeaflet({
        
        data <- Accidentalidad
        
        # Analisis condiciones especiales
        if (input$Condicion == "N") {
            
        } else if (input$Condicion == "F") {
            data <- data[(data$Festivo == "No Festivo"),]
            
        } else if (input$Condicion == "VF") {
            data <- data[(data$MES == 12 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 1 & data$DIA >= 1 & data$DIA <= 15),]
            
        } else if (input$Condicion == "VM") {
            data <- data[(data$MES == 6 & data$DIA >= 15 & data$DIA <= 31 | data$MES == 7 & data$DIA >= 1 & data$DIA <= 15),]
            
        }
        
        # Indicar el rango de fechas seleccionadas.
        Fecha_Inicial <- as.Date(min(input$Fechas_Seleccionadas), origin = "1970-01-01")
        Fecha_Final <- as.Date(max(input$Fechas_Seleccionadas), origin = "1970-01-01")
        
        data <- data[(as.Date(data$f_accidente) >= Fecha_Inicial & as.Date(data$f_accidente) <= Fecha_Final),]
        
        # Indicar las horas seleccionadas
        Hora_Inicial <- input$Horas_Seleccionada[1]
        Hora_Final <- input$Horas_Seleccionada[2]
        
        data <- data[(as.numeric(data$HORA2) >= Hora_Inicial & as.numeric(data$HORA2) <= Hora_Final),]
        
        # Grafico de los accidentes teniendo en cuenta el barrio seleccionado
        if (input$Barrio_Seleccionado != "Seleccione un barrio:") {
            
            data <- data[data$BARRIO == input$Barrio_Seleccionado,]
            
            # Encontrar los valores extremos para graficar
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION)
            map
        
            # Grafico de los accidentes si no hay nada seleccionado
        } else if (input$Comuna_Seleccionada != "Seleccione una comuna:") {
            
            data <- data[data$COMUNA == input$Comuna_Seleccionada,]
            
            # Encontrar los valores extremos para graficar
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions=markerClusterOptions())
            map
        
        } else if (input$Comuna_Seleccionada == "Seleccione una comuna:" & input$Barrio_Seleccionado == "Seleccione un barrio:") {
            
            data <- data
            
            # Encontrar los valores extremos para graficar
            Longitud <- data$Longitud
            Latitud <- data$Latitud
            lng1 = min(Longitud)
            lat1 = min(Latitud)
            lng2 = max(Longitud)
            lat2 = max(Latitud)
            
            map <- leaflet()
            map <- addProviderTiles(map,provider = "OpenStreetMap.Mapnik")
            map <- fitBounds(map, 
                             lng1=lng1, 
                             lat1=lat1, 
                             lng2=lng2, 
                             lat2=lat2)
            map <- addMarkers(map,
                              lat=Latitud,
                              lng=Longitud,
                              popup=data$DIRECCION,
                              clusterOptions=markerClusterOptions())
            map
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
