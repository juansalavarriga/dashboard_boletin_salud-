library(readxl)
library(shiny)
library(leaflet)
library(rgdal)
library(tidyr)
library(dplyr)
library(ggplot2)
library(statar)
library(shinythemes)
library(haven)
library(expss)
library(plotly)





ui <- bootstrapPage(
     navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">BOLETIN SALUD</a>'), id="nav",
               windowTitle = "Boletin de Salud",
               
               
               
               
               tabPanel("Portada",
                        uiOutput("img"),
               
               
               absolutePanel(id = "logo1", class = "card", bottom = 20, left = 60, width = 80, fixed=TRUE, draggable = FALSE, height = "auto",
                             actionButton("twitter_share", label = "", icon = icon("twitter"),style='padding:5px',
                                          onclick = sprintf("window.open('%s')", 
                                                            "https://twitter.com/Evidencia_pe"))),
               
               absolutePanel(id = "logo2", class = "card", bottom = 20, left = 20, width = 30, fixed=TRUE, draggable = FALSE, height = "auto",
                             actionButton("facebook_share", label = "", icon = icon("facebook"),style='padding:5px',
                                          onclick = sprintf("window.open('%s')", 
                                                            "https://www.facebook.com/evidencia.pe")))
               
               
               
               ),
               
               tabPanel("Diagnostico sector salud",
                        div(class="outer",
                            leafletOutput("mymap1",height = "100vh"),
                        
                            
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 90, left = 65, width = 500,
                                          draggable = TRUE, height = "300px",
                                          
                                          span(tags$i(h5("Evolucion del presupuesto asignado al sector salud nivel nacional en el periodo (2009-2021)")), style="color:#045a8d"),
                                          plotlyOutput("presupuesto_plot", height="80%", width="100%")
                                          
                            ),
                            absolutePanel(id = "controls", class = "panel panel-default",
                                          top = 400, left = 65, width = 500,
                                          draggable = TRUE, height = "300px",
                                          
                                          span(tags$i(h5("Evolucion de la ejecucion del sector salud nivel nacional en el periodo (2009-2021)")), style="color:#045a8d"),
                                          plotlyOutput("ejecucion_plot", height="80%", width="100%")
                                          
                            )
                        )
                            
                            
                        
               ),
               tabPanel("Acceso a seguro de salud",
                        fluidRow(
                        column(8,
                                 sidebarPanel(
                                     checkboxInput("Zona","Zona",FALSE),
                                     checkboxInput("Sexo", "Sexo",FALSE),
                                     uiOutput("select_var1"),
                                     uiOutput("select_var2")
                                 ))),
                        fluidRow(
                        column(12,
                               tableOutput("table1"),
                               plotlyOutput("seguro_plot")
                        ))
               ),
               tabPanel("Salud mental",
                        fluidRow(
                          column(8,
                                 sidebarPanel(
                                   uiOutput("select_var3")
                                 ))),
                        fluidRow(
                          column(12,
                                 plotlyOutput("diagnostico_plot")
                          ))
               ),
               tabPanel("Salud infantil",
                        fluidRow(
                          column(12,
                                 sidebarPanel(
                                   uiOutput("select_var4")
                                 ))),
                        fluidRow(
                          column(12,
                                 
                                 plotlyOutput("salud_inf__plot")
                                 
                          ),
                          column(12,
                                 splitLayout(cellWidths = c("50%", "50%"), tableOutput("table2"),plotlyOutput("variacion__plot"))
                          
                          
                          )
                        )
                                 
                                 
                                
                                 
                          
               ),
               tabPanel("Salud adulto mayor",
                        fluidRow(
                          column(8,
                                 sidebarPanel(
                                   checkboxInput("zona2","zona2",FALSE),
                                   checkboxInput("sexo4", "sexo4",FALSE),
                                   uiOutput("select_region3")
                                 ))),
                        fluidRow(
                          column(12,
                                 plotlyOutput("adulto_plot")
                          ))
               ),
               tabPanel("Estadisticas COVID-19",
                        fluidRow(
                          column(8,
                                 sidebarPanel(
                                   checkboxInput("edad","edad",FALSE),
                                   checkboxInput("sexo2", "sexo2",FALSE),
                                   uiOutput("select_region"),
                                   uiOutput("select_provincia"),
                                   uiOutput("select_distrito")
                                 ))),
                        fluidRow(
                          column(12,
                                 plotlyOutput("contagios_plot")
                          )),
                        fluidRow(
                          column(8,
                                 sidebarPanel(
                                   checkboxInput("edad2","edad2",FALSE),
                                   checkboxInput("sexo3", "sexo3",FALSE),
                                   uiOutput("select_region2"),
                                   uiOutput("select_provincia2"),
                                   uiOutput("select_distrito2")
                                 ))),
                        fluidRow(
                          column(12,
                                 plotlyOutput("fallecidos_plot")
                          ))
                        
               )
              
    )          
)





### SHINY SERVER ###

server = function(input, output, session) {
    
    output$presupuesto_plot<-renderPlotly({
        
            plot_ly(Presupuesto, x = ~Year, y = ~Porcentaje)%>%
            add_lines()%>%
            layout(xaxis = list(title = "Fecha"), 
                   yaxis = list(title = "Porcentaje del Presupuesto total"))

    })
    
    
    
    
    output$ejecucion_plot<-renderPlotly({
        
        plot_ly(Ejecucion, x = ~Year, y = ~Porcentaje)%>%
            add_lines()%>%
            layout(xaxis = list(title = "Fecha"), 
                   yaxis = list(title = "Ejecucion del presupuesto asignado"))
        
    })
    


    output$mymap1 <- renderLeaflet({
        leaflet(peru_reg)  %>%
            addProviderTiles(providers$Stamen.TerrainBackground)%>%
            setView(-75.015152,-9.189967,zoom=4.5) %>%
            addPolygons(weight = 1,
                        stroke = TRUE,
                        group = "Ejecucion Regional",
                        color = "white",
                        fillOpacity = 1,
                        dashArray = "3",
                        label = ~NOMBDEP,
                        fillColor = ~mypal1(Ejecucion_regional$Porcentaje),
                        popup = paste("Region: ", peru_reg@data[["NOMBDEP"]], "<br>",
                                      "Value: ", Ejecucion_regional$Porcentaje, "<br>"),
                        highlight = highlightOptions(
                            weight = 2,
                            dashArray = "",
                            color = "grey",
                            bringToFront = TRUE
                        )) %>%
            addPolygons(weight = 1,
                        stroke = TRUE,
                        group = "Densidad recursos humanos (Medicos)",
                        color = "white",
                        fillOpacity = 1,
                        dashArray = "3",
                        label = ~NOMBDEP,
                        fillColor = ~mypal2(Densidad_regional$Medicos),
                        popup = paste("Region: ", peru_reg@data[["NOMBDEP"]], "<br>",
                                      "Value: ", Densidad_regional$Medicos, "<br>"),
                        highlight = highlightOptions(
                            weight = 2,
                            dashArray = "",
                            color = "grey",
                            bringToFront = TRUE
                        )) %>%
            addPolygons(weight = 1,
                        stroke = TRUE,
                        group = "Densidad recursos humanos (Enfermeros)",
                        color = "white",
                        fillOpacity = 1,
                        dashArray = "3",
                        label = ~NOMBDEP,
                        fillColor = ~mypal3(Densidad_regional$Enfermeros),
                        popup = paste("Region: ", peru_reg@data[["NOMBDEP"]], "<br>",
                                      "Value: ", Densidad_regional$Enfermeros, "<br>"),
                        highlight = highlightOptions(
                            weight = 2,
                            dashArray = "",
                            color = "grey",
                            bringToFront = TRUE
                        )) %>%
            addPolygons(weight = 1,
                        stroke = TRUE,
                        group = "Densidad recursos humanos (Obstetras)",
                        color = "white",
                        fillOpacity = 1,
                        dashArray = "3",
                        label = ~NOMBDEP,
                        fillColor = ~mypal4(Densidad_regional$Obstetras),
                        popup = paste("Region: ", peru_reg@data[["NOMBDEP"]], "<br>",
                                      "Value: ", Densidad_regional$Obstetras, "<br>"),
                        highlight = highlightOptions(
                            weight = 2,
                            dashArray = "",
                            color = "grey",
                            bringToFront = TRUE
                        )) %>%
            addPolygons(weight = 1,
                        stroke = TRUE,
                        group = "Densidad recursos humanos (Total)",
                        color = "white",
                        fillOpacity = 1,
                        dashArray = "3",
                        label = ~NOMBDEP,
                        fillColor = ~mypal5(Densidad_regional$`Densidad total`),
                        popup = paste("Region: ", peru_reg@data[["NOMBDEP"]], "<br>",
                                      "Value: ", Densidad_regional$`Densidad total`, "<br>"),
                        highlight = highlightOptions(
                            weight = 2,
                            dashArray = "",
                            color = "grey",
                            bringToFront = TRUE
                        )) %>%
            addPolygons(weight = 1,
                        stroke = TRUE,
                        group = "Capacidad instalada",
                        color = "white",
                        fillOpacity = 1,
                        dashArray = "3",
                        label = ~NOMBDEP,
                        fillColor = ~mypal6(capacidad_instalada$Porcentaje),
                        popup = paste("Region: ", peru_reg@data[["NOMBDEP"]], "<br>",
                                      "Value: ", capacidad_instalada$Porcentaje, "<br>"),
                        highlight = highlightOptions(
                            weight = 2,
                            dashArray = "",
                            color = "grey",
                            bringToFront = TRUE
                        )) %>%
            
            addLayersControl(
                position = "bottomright",
                overlayGroups = c("Ejecucion regional", "Densidad recursos humanos (Medicos)", 
                                  "Densidad recursos humanos (Enfermeros)", "Densidad recursos humanos (Obstetras)", 
                                  "Densidad recursos humanos (Total)", "Capacidad instalada"),
                options = layersControlOptions(collapsed = FALSE))
        
    })
    
    output$img <- renderUI({
      tags$img(src = "https://evidencia-pe.com/wp-content/uploads/2021/07/logo-new.png")
    })
    
    output$select_var1 <- renderUI({
        
        selectizeInput('var1', "Todo el periodo", 
                       choices = c("Todo el periodo" = "", c(seq(2008,2020, by=1))))
        
    })
    
    
    output$select_var2 <- renderUI({
        
        selectizeInput('var2', "Tipo seguro", 
                       choices = c("Tipo seguro" = "", c("SIS","ESSALUD","SEGURO PRIVADO")))
        
    })
    
    output$select_var3 <- renderUI({
      
      selectizeInput('var3', "Todo el periodo", 
                     choices = c("Todo el periodo" = "", c(seq(2009,2019, by=1))))
      
    }) 
    
    
    
    
    output$select_region <- renderUI({
      
      selectizeInput('region', "Seleccione region", 
                     choices = c("Seleccione region" = "", levels(as.factor(data1$DEPARTAMENTO))))
      
    })
    
    
    output$select_provincia <- renderUI({
      
      selectizeInput('provincia', "Seleccione provincia", 
                     choices = c("Seleccione provincia" = "", levels(as.factor(data1$PROVINCIA[data1$DEPARTAMENTO==input$region]))))
      
    })
    
    output$select_distrito <- renderUI({
      
      selectizeInput('distrito', "Seleccione distrito", 
                     choices = c("Seleccione distrito" = "",  levels(as.factor(data1$DISTRITO[data1$PROVINCIA== input$provincia & data1$DEPARTAMENTO==input$region]))))
      
    }) 
    output$select_region2 <- renderUI({
      
      selectizeInput('region2', "Seleccione region", 
                     choices = c("Seleccione region" = "", levels(as.factor(data2$DEPARTAMENTO))))
      
    })
    
    
    output$select_provincia2 <- renderUI({
      
      selectizeInput('provincia2', "Seleccione provincia", 
                     choices = c("Seleccione provincia" = "", levels(as.factor(data2$PROVINCIA[data2$DEPARTAMENTO==input$region2]))))
      
    })
    
    output$select_distrito2 <- renderUI({
      
      selectizeInput('distrito2', "Seleccione distrito", 
                     choices = c("Seleccione distrito" = "",  levels(as.factor(data2$DISTRITO[data2$PROVINCIA== input$provincia2 & data2$DEPARTAMENTO==input$region2]))))
      
    }) 
    output$select_region3 <- renderUI({
      
      selectizeInput('region3', "Seleccione region", 
                     choices = c("Seleccione region" = "", levels(as.factor(data_adulto$region))))
      
    })
    output$select_var4 <- renderUI({
      
      selectizeInput('var4', "Elija indicador", 
                     choices = c("Elija indicador" = "", c("Anemia","Desnutricion")),
                     selected= "Anemia")
      
    })
    
    
    tipos<-reactive({
        if(input$var2==""){
            
            
            if(input$var1==""){
                
                if (input$Zona==FALSE){
                    if(input$Sexo==FALSE){
                        
                        
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$asegurado,seguro$year), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1)%>%
                            ungroup()%>%
                            select(percent)
                        tipo_elegido<-as.data.frame(t(tipo_elegido))
                        colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                        
                        
                    }else{
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$asegurado,seguro$year,seguro$sexo), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","sexo","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year,sexo)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1)%>%
                            ungroup()%>%
                            select(sexo,percent)
                        
                        tipo_elegido_h<-filter(tipo_elegido,tipo_elegido$sexo=="Hombre")
                        tipo_elegido_m<-filter(tipo_elegido,tipo_elegido$sexo=="Mujer")
                        
                        tipo_elegido_h<-as.data.frame(t(tipo_elegido_h))
                        tipo_elegido_m<-as.data.frame(t(tipo_elegido_m))
                        
                        
                        tipo_elegido<-rbind(tipo_elegido_h,tipo_elegido_m)
                        colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                        
                    }
                    
                }else{
                    if(input$Sexo==FALSE){
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$asegurado,seguro$year,seguro$zona), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","zona","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year,zona)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1)%>%
                            ungroup()%>%
                            select(zona,percent)
                        
                        
                        tipo_elegido_u<-filter(tipo_elegido,tipo_elegido$zona=="Urbano")
                        tipo_elegido_r<-filter(tipo_elegido,tipo_elegido$zona=="Rural")
                        
                        tipo_elegido_u<-as.data.frame(t(tipo_elegido_u))
                        tipo_elegido_r<-as.data.frame(t(tipo_elegido_r))
                        
                        
                        tipo_elegido<-rbind(tipo_elegido_u,tipo_elegido_r)
                        colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                        
                        
                        
                    }else{
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$asegurado,seguro$year,seguro$zona,seguro$sexo), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","zona","sexo","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year,zona,sexo)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1)%>%
                            ungroup()%>%
                            select(zona,sexo,percent)%>%
                          mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
                          select(categoria,percent)
                        
                        
                        
                        tipo_elegido_u_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Urbano")
                        tipo_elegido_r_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Rural")
                        
                        tipo_elegido_u_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Urbano")
                        tipo_elegido_r_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Rural")
                        
                        tipo_elegido_u_m<-as.data.frame(t(tipo_elegido_u_m))
                        tipo_elegido_r_m<-as.data.frame(t(tipo_elegido_r_m))
                        
                        tipo_elegido_u_h<-as.data.frame(t(tipo_elegido_u_h))
                        tipo_elegido_r_h<-as.data.frame(t(tipo_elegido_r_h))
                        
                        tipo_elegido<-rbind(tipo_elegido_u_h,tipo_elegido_u_m,tipo_elegido_r_h,tipo_elegido_r_m)
                        colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                    } 
                    
                    
                }
            } else{
                
                if (input$Zona==FALSE){
                    if(input$Sexo==FALSE){
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$asegurado,seguro$year), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1 & year==input$var1)%>%
                            ungroup()%>%
                            select(percent)
                        tipo_elegido<-as.data.frame(t(tipo_elegido))
                        colnames(tipo_elegido)<-input$var1
                        
                        
                    }else{
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$asegurado,seguro$year,seguro$sexo), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","sexo","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year,sexo)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1& year==input$var1)%>%
                            ungroup()%>%
                            select(sexo,percent)
                        
                        tipo_elegido_h<-filter(tipo_elegido,tipo_elegido$sexo=="Hombre")
                        tipo_elegido_m<-filter(tipo_elegido,tipo_elegido$sexo=="Mujer")
                        
                        tipo_elegido_h<-as.data.frame(t(tipo_elegido_h))
                        tipo_elegido_m<-as.data.frame(t(tipo_elegido_m))
                        
                        
                        tipo_elegido<-rbind(tipo_elegido_h,tipo_elegido_m)
                        colnames(tipo_elegido)<-input$var1
                        
                    }
                    
                }else{
                    if(input$Sexo==FALSE){
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$asegurado,seguro$year,seguro$zona), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","zona","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year,zona)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1& year==input$var1)%>%
                            ungroup()%>%
                            select(zona,percent)
                        
                        
                        tipo_elegido_u<-filter(tipo_elegido,tipo_elegido$zona=="Urbano")
                        tipo_elegido_r<-filter(tipo_elegido,tipo_elegido$zona=="Rural")
                        
                        tipo_elegido_u<-as.data.frame(t(tipo_elegido_u))
                        tipo_elegido_r<-as.data.frame(t(tipo_elegido_r))
                        
                        
                        tipo_elegido<-rbind(tipo_elegido_u,tipo_elegido_r)
                        colnames(tipo_elegido)<-input$var1
                        
                        
                        
                    }else{
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$asegurado,seguro$year,seguro$zona,seguro$sexo), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","zona","sexo","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year,zona,sexo)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1& year==input$var1)%>%
                            ungroup()%>%
                            select(zona,sexo,percent)%>%
                          mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
                          select(categoria,percent)
                        
                        
                        
                        tipo_elegido_u_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Urbano")
                        tipo_elegido_r_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Rural")
                        
                        tipo_elegido_u_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Urbano")
                        tipo_elegido_r_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Rural")
                        
                        tipo_elegido_u_m<-as.data.frame(t(tipo_elegido_u_m))
                        tipo_elegido_r_m<-as.data.frame(t(tipo_elegido_r_m))
                        
                        tipo_elegido_u_h<-as.data.frame(t(tipo_elegido_u_h))
                        tipo_elegido_r_h<-as.data.frame(t(tipo_elegido_r_h))
                        
                        tipo_elegido<-rbind(tipo_elegido_u_h,tipo_elegido_u_m,tipo_elegido_r_h,tipo_elegido_r_m)
                        colnames(tipo_elegido)<-input$var1
                        
                        
                        
                      
                    } 
                    
                    
                }           
                
                
                
                
                
                
            }       
         
            
            
            
        }else{

        if(input$var2=="SIS"){
            
            
            if(input$var1==""){
                
                if (input$Zona==FALSE){
                    if(input$Sexo==FALSE){
                        
                        
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$SIS,seguro$year), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1)%>%
                            ungroup()%>%
                            select(percent)
                        tipo_elegido<-as.data.frame(t(tipo_elegido))
                        colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                        
                        
                    }else{
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$SIS,seguro$year,seguro$sexo), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","sexo","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year,sexo)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1)%>%
                            ungroup()%>%
                            select(sexo,percent)
                        
                        tipo_elegido_h<-filter(tipo_elegido,tipo_elegido$sexo=="Hombre")
                        tipo_elegido_m<-filter(tipo_elegido,tipo_elegido$sexo=="Mujer")
                        
                        tipo_elegido_h<-as.data.frame(t(tipo_elegido_h))
                        tipo_elegido_m<-as.data.frame(t(tipo_elegido_m))
                        
                        
                        tipo_elegido<-rbind(tipo_elegido_h,tipo_elegido_m)
                        colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                        
                    }
                    
                }else{
                    if(input$Sexo==FALSE){
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$SIS,seguro$year,seguro$zona), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","zona","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year,zona)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1)%>%
                            ungroup()%>%
                            select(zona,percent)
                        
                        
                        tipo_elegido_u<-filter(tipo_elegido,tipo_elegido$zona=="Urbano")
                        tipo_elegido_r<-filter(tipo_elegido,tipo_elegido$zona=="Rural")
                        
                        tipo_elegido_u<-as.data.frame(t(tipo_elegido_u))
                        tipo_elegido_r<-as.data.frame(t(tipo_elegido_r))
                        
                        
                        tipo_elegido<-rbind(tipo_elegido_u,tipo_elegido_r)
                        colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                        
                        
                        
                    }else{
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$SIS,seguro$year,seguro$zona,seguro$sexo), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","zona","sexo","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year,zona,sexo)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1)%>%
                            ungroup()%>%
                            select(zona,sexo,percent)%>%
                          mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
                          select(categoria,percent)
                        
                        
                        
                        tipo_elegido_u_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Urbano")
                        tipo_elegido_r_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Rural")
                        
                        tipo_elegido_u_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Urbano")
                        tipo_elegido_r_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Rural")
                        
                        tipo_elegido_u_m<-as.data.frame(t(tipo_elegido_u_m))
                        tipo_elegido_r_m<-as.data.frame(t(tipo_elegido_r_m))
                        
                        tipo_elegido_u_h<-as.data.frame(t(tipo_elegido_u_h))
                        tipo_elegido_r_h<-as.data.frame(t(tipo_elegido_r_h))
                        
                        tipo_elegido<-rbind(tipo_elegido_u_h,tipo_elegido_u_m,tipo_elegido_r_h,tipo_elegido_r_m)
                        colnames(tipo_elegido)<-input$var1
                       
                    } 
                    
                    
                }
            } else{
                
                if (input$Zona==FALSE){
                    if(input$Sexo==FALSE){
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$SIS,seguro$year), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1 & year==input$var1)%>%
                            ungroup()%>%
                            select(percent)
                        tipo_elegido<-as.data.frame(t(tipo_elegido))
                        colnames(tipo_elegido)<-input$var1
                        
                        
                    }else{
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$SIS,seguro$year,seguro$sexo), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","sexo","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year,sexo)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1& year==input$var1)%>%
                            ungroup()%>%
                            select(sexo,percent)
                        
                        tipo_elegido_h<-filter(tipo_elegido,tipo_elegido$sexo=="Hombre")
                        tipo_elegido_m<-filter(tipo_elegido,tipo_elegido$sexo=="Mujer")
                        
                        tipo_elegido_h<-as.data.frame(t(tipo_elegido_h))
                        tipo_elegido_m<-as.data.frame(t(tipo_elegido_m))
                        
                        
                        tipo_elegido<-rbind(tipo_elegido_h,tipo_elegido_m)
                        colnames(tipo_elegido)<-input$var1
                        
                    }
                    
                }else{
                    if(input$Sexo==FALSE){
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$SIS,seguro$year,seguro$zona), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","zona","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year,zona)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1& year==input$var1)%>%
                            ungroup()%>%
                            select(zona,percent)
                        
                        
                        tipo_elegido_u<-filter(tipo_elegido,tipo_elegido$zona=="Urbano")
                        tipo_elegido_r<-filter(tipo_elegido,tipo_elegido$zona=="Rural")
                        
                        tipo_elegido_u<-as.data.frame(t(tipo_elegido_u))
                        tipo_elegido_r<-as.data.frame(t(tipo_elegido_r))
                        
                        
                        tipo_elegido<-rbind(tipo_elegido_u,tipo_elegido_r)
                        colnames(tipo_elegido)<-input$var1
                        
                        
                        
                    }else{
                        tipo_elegido<-aggregate(seguro$factor07,list(seguro$SIS,seguro$year,seguro$zona,seguro$sexo), sum)
                        colnames(tipo_elegido)<-c("Seguro","year","zona","sexo","personas")
                        
                        tipo_elegido<-tipo_elegido%>%
                            group_by(year,zona,sexo)%>%
                            mutate(percent = round(personas/sum(personas),2))%>%
                            filter(Seguro==1& year==input$var1)%>%
                            ungroup()%>%
                            select(zona,sexo,percent)%>%
                          mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
                          select(categoria,percent)
                        
                        
                        
                        tipo_elegido_u_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Urbano")
                        tipo_elegido_r_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Rural")
                        
                        tipo_elegido_u_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Urbano")
                        tipo_elegido_r_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Rural")
                        
                        tipo_elegido_u_m<-as.data.frame(t(tipo_elegido_u_m))
                        tipo_elegido_r_m<-as.data.frame(t(tipo_elegido_r_m))
                        
                        tipo_elegido_u_h<-as.data.frame(t(tipo_elegido_u_h))
                        tipo_elegido_r_h<-as.data.frame(t(tipo_elegido_r_h))
                        
                        tipo_elegido<-rbind(tipo_elegido_u_h,tipo_elegido_u_m,tipo_elegido_r_h,tipo_elegido_r_m)
                        colnames(tipo_elegido)<-input$var1
                    } 
                    
                    
                }           
                
                
                
                
                
                
            }       
        
        
        
        
        
        
        
        
        
        
        
    }
            if(input$var2=="ESSALUD"){
                
                
                
                
                
                
                
                if(input$var1==""){
                    
                    if (input$Zona==FALSE){
                        if(input$Sexo==FALSE){
                            
                            
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$ESSALUD,seguro$year), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1)%>%
                                ungroup()%>%
                                select(percent)
                            tipo_elegido<-as.data.frame(t(tipo_elegido))
                            colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                            
                            
                        }else{
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$ESSALUD,seguro$year,seguro$sexo), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","sexo","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year,sexo)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1)%>%
                                ungroup()%>%
                                select(sexo,percent)
                            
                            tipo_elegido_h<-filter(tipo_elegido,tipo_elegido$sexo=="Hombre")
                            tipo_elegido_m<-filter(tipo_elegido,tipo_elegido$sexo=="Mujer")
                            
                            tipo_elegido_h<-as.data.frame(t(tipo_elegido_h))
                            tipo_elegido_m<-as.data.frame(t(tipo_elegido_m))
                            
                            
                            tipo_elegido<-rbind(tipo_elegido_h,tipo_elegido_m)
                            colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                            
                        }
                        
                    }else{
                        if(input$Sexo==FALSE){
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$ESSALUD,seguro$year,seguro$zona), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","zona","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year,zona)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1)%>%
                                ungroup()%>%
                                select(zona,percent)
                            
                            
                            tipo_elegido_u<-filter(tipo_elegido,tipo_elegido$zona=="Urbano")
                            tipo_elegido_r<-filter(tipo_elegido,tipo_elegido$zona=="Rural")
                            
                            tipo_elegido_u<-as.data.frame(t(tipo_elegido_u))
                            tipo_elegido_r<-as.data.frame(t(tipo_elegido_r))
                            
                            
                            tipo_elegido<-rbind(tipo_elegido_u,tipo_elegido_r)
                            colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                            
                            
                            
                        }else{
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$ESSALUD,seguro$year,seguro$zona,seguro$sexo), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","zona","sexo","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year,zona,sexo)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1)%>%
                                ungroup()%>%
                                select(zona,sexo,percent)%>%
                              mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
                              select(categoria,percent)
                            
                            
                            
                            tipo_elegido_u_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Urbano")
                            tipo_elegido_r_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Rural")
                            
                            tipo_elegido_u_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Urbano")
                            tipo_elegido_r_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Rural")
                            
                            tipo_elegido_u_m<-as.data.frame(t(tipo_elegido_u_m))
                            tipo_elegido_r_m<-as.data.frame(t(tipo_elegido_r_m))
                            
                            tipo_elegido_u_h<-as.data.frame(t(tipo_elegido_u_h))
                            tipo_elegido_r_h<-as.data.frame(t(tipo_elegido_r_h))
                            
                            tipo_elegido<-rbind(tipo_elegido_u_h,tipo_elegido_u_m,tipo_elegido_r_h,tipo_elegido_r_m)
                       
                            colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                        } 
                        
                        
                    }
                } else{
                    
                    if (input$Zona==FALSE){
                        if(input$Sexo==FALSE){
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$ESSALUD,seguro$year), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1 & year==input$var1)%>%
                                ungroup()%>%
                                select(percent)
                            tipo_elegido<-as.data.frame(t(tipo_elegido))
                            colnames(tipo_elegido)<-input$var1
                            
                            
                        }else{
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$ESSALUD,seguro$year,seguro$sexo), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","sexo","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year,sexo)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1& year==input$var1)%>%
                                ungroup()%>%
                                select(sexo,percent)
                            
                            tipo_elegido_h<-filter(tipo_elegido,tipo_elegido$sexo=="Hombre")
                            tipo_elegido_m<-filter(tipo_elegido,tipo_elegido$sexo=="Mujer")
                            
                            tipo_elegido_h<-as.data.frame(t(tipo_elegido_h))
                            tipo_elegido_m<-as.data.frame(t(tipo_elegido_m))
                            
                            
                            tipo_elegido<-rbind(tipo_elegido_h,tipo_elegido_m)
                            colnames(tipo_elegido)<-input$var1
                            
                        }
                        
                    }else{
                        if(input$Sexo==FALSE){
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$ESSALUD,seguro$year,seguro$zona), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","zona","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year,zona)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1& year==input$var1)%>%
                                ungroup()%>%
                                select(zona,percent)
                            
                            
                            tipo_elegido_u<-filter(tipo_elegido,tipo_elegido$zona=="Urbano")
                            tipo_elegido_r<-filter(tipo_elegido,tipo_elegido$zona=="Rural")
                            
                            tipo_elegido_u<-as.data.frame(t(tipo_elegido_u))
                            tipo_elegido_r<-as.data.frame(t(tipo_elegido_r))
                            
                            
                            tipo_elegido<-rbind(tipo_elegido_u,tipo_elegido_r)
                            colnames(tipo_elegido)<-input$var1
                            
                            
                            
                        }else{
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$ESSALUD,seguro$year,seguro$zona,seguro$sexo), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","zona","sexo","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year,zona,sexo)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1& year==input$var1)%>%
                                ungroup()%>%
                                select(zona,sexo,percent)%>%
                              mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
                              select(categoria,percent)
                            
                            
                            
                            tipo_elegido_u_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Urbano")
                            tipo_elegido_r_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Rural")
                            
                            tipo_elegido_u_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Urbano")
                            tipo_elegido_r_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Rural")
                            
                            tipo_elegido_u_m<-as.data.frame(t(tipo_elegido_u_m))
                            tipo_elegido_r_m<-as.data.frame(t(tipo_elegido_r_m))
                            
                            tipo_elegido_u_h<-as.data.frame(t(tipo_elegido_u_h))
                            tipo_elegido_r_h<-as.data.frame(t(tipo_elegido_r_h))
                            
                            tipo_elegido<-rbind(tipo_elegido_u_h,tipo_elegido_u_m,tipo_elegido_r_h,tipo_elegido_r_m)
                            colnames(tipo_elegido)<-input$var1
                        } 
                        
                        
                    }           
                    
                    
                    
                    
                    
                    
                }       
                
                
                
                
                
                
                
                
                
                
                
                
                
            }
            if(input$var2=="SEGURO PRIVADO"){
                
                
                
                
                if(input$var1==""){
                    
                    if (input$Zona==FALSE){
                        if(input$Sexo==FALSE){
                            
                            
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$S_PRIVADO,seguro$year), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1)%>%
                                ungroup()%>%
                                select(percent)
                            tipo_elegido<-as.data.frame(t(tipo_elegido))
                            colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                            
                            
                        }else{
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$S_PRIVADO,seguro$year,seguro$sexo), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","sexo","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year,sexo)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1)%>%
                                ungroup()%>%
                                select(sexo,percent)
                            
                            tipo_elegido_h<-filter(tipo_elegido,tipo_elegido$sexo=="Hombre")
                            tipo_elegido_m<-filter(tipo_elegido,tipo_elegido$sexo=="Mujer")
                            
                            tipo_elegido_h<-as.data.frame(t(tipo_elegido_h))
                            tipo_elegido_m<-as.data.frame(t(tipo_elegido_m))
                            
                            
                            tipo_elegido<-rbind(tipo_elegido_h,tipo_elegido_m)
                            colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                            
                        }
                        
                    }else{
                        if(input$Sexo==FALSE){
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$S_PRIVADO,seguro$year,seguro$zona), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","zona","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year,zona)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1)%>%
                                ungroup()%>%
                                select(zona,percent)
                            
                            
                            tipo_elegido_u<-filter(tipo_elegido,tipo_elegido$zona=="Urbano")
                            tipo_elegido_r<-filter(tipo_elegido,tipo_elegido$zona=="Rural")
                            
                            tipo_elegido_u<-as.data.frame(t(tipo_elegido_u))
                            tipo_elegido_r<-as.data.frame(t(tipo_elegido_r))
                            
                            
                            tipo_elegido<-rbind(tipo_elegido_u,tipo_elegido_r)
                            colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                            
                            
                            
                        }else{
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$S_PRIVADO,seguro$year,seguro$zona,seguro$sexo), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","zona","sexo","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year,zona,sexo)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1)%>%
                                ungroup()%>%
                                select(zona,sexo,percent)%>%
                              mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
                              select(categoria,percent)
                            
                            
                            
                            tipo_elegido_u_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Urbano")
                            tipo_elegido_r_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Rural")
                            
                            tipo_elegido_u_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Urbano")
                            tipo_elegido_r_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Rural")
                            
                            tipo_elegido_u_m<-as.data.frame(t(tipo_elegido_u_m))
                            tipo_elegido_r_m<-as.data.frame(t(tipo_elegido_r_m))
                            
                            tipo_elegido_u_h<-as.data.frame(t(tipo_elegido_u_h))
                            tipo_elegido_r_h<-as.data.frame(t(tipo_elegido_r_h))
                            
                            tipo_elegido<-rbind(tipo_elegido_u_h,tipo_elegido_u_m,tipo_elegido_r_h,tipo_elegido_r_m)
                          
                            colnames(tipo_elegido)<-c(seq(2008,2020, by=1))
                        } 
                        
                        
                    }
                } else{
                    
                    if (input$Zona==FALSE){
                        if(input$Sexo==FALSE){
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$S_PRIVADO,seguro$year), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1 & year==input$var1)%>%
                                ungroup()%>%
                                select(percent)
                            tipo_elegido<-as.data.frame(t(tipo_elegido))
                            colnames(tipo_elegido)<-input$var1
                            
                            
                        }else{
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$S_PRIVADO,seguro$year,seguro$sexo), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","sexo","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year,sexo)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1& year==input$var1)%>%
                                ungroup()%>%
                                select(sexo,percent)
                            
                            tipo_elegido_h<-filter(tipo_elegido,tipo_elegido$sexo=="Hombre")
                            tipo_elegido_m<-filter(tipo_elegido,tipo_elegido$sexo=="Mujer")
                            
                            tipo_elegido_h<-as.data.frame(t(tipo_elegido_h))
                            tipo_elegido_m<-as.data.frame(t(tipo_elegido_m))
                            
                            
                            tipo_elegido<-rbind(tipo_elegido_h,tipo_elegido_m)
                            colnames(tipo_elegido)<-input$var1
                            
                        }
                        
                    }else{
                        if(input$Sexo==FALSE){
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$S_PRIVADO,seguro$year,seguro$zona), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","zona","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year,zona)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1& year==input$var1)%>%
                                ungroup()%>%
                                select(zona,percent)
                            
                            
                            tipo_elegido_u<-filter(tipo_elegido,tipo_elegido$zona=="Urbano")
                            tipo_elegido_r<-filter(tipo_elegido,tipo_elegido$zona=="Rural")
                            
                            tipo_elegido_u<-as.data.frame(t(tipo_elegido_u))
                            tipo_elegido_r<-as.data.frame(t(tipo_elegido_r))
                            
                            
                            tipo_elegido<-rbind(tipo_elegido_u,tipo_elegido_r)
                            colnames(tipo_elegido)<-input$var1
                            
                            
                            
                        }else{
                            tipo_elegido<-aggregate(seguro$factor07,list(seguro$S_PRIVADO,seguro$year,seguro$zona,seguro$sexo), sum)
                            colnames(tipo_elegido)<-c("Seguro","year","zona","sexo","personas")
                            
                            tipo_elegido<-tipo_elegido%>%
                                group_by(year,zona,sexo)%>%
                                mutate(percent = round(personas/sum(personas),2))%>%
                                filter(Seguro==1& year==input$var1)%>%
                                ungroup()%>%
                              mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
                              select(categoria,percent)
                            
                            
                            
                            tipo_elegido_u_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Urbano")
                            tipo_elegido_r_m<-filter(tipo_elegido,tipo_elegido$categoria=="M - Rural")
                            
                            tipo_elegido_u_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Urbano")
                            tipo_elegido_r_h<-filter(tipo_elegido,tipo_elegido$categoria=="H - Rural")
                            
                            tipo_elegido_u_m<-as.data.frame(t(tipo_elegido_u_m))
                            tipo_elegido_r_m<-as.data.frame(t(tipo_elegido_r_m))
                            
                            tipo_elegido_u_h<-as.data.frame(t(tipo_elegido_u_h))
                            tipo_elegido_r_h<-as.data.frame(t(tipo_elegido_r_h))
                            
                            tipo_elegido<-rbind(tipo_elegido_u_h,tipo_elegido_u_m,tipo_elegido_r_h,tipo_elegido_r_m)
                            colnames(tipo_elegido)<-input$var1
                        } 
                        
                        
                    }           
                    
                    
                    
                    
                    
                    
                }       
                
                
                
               
                
                
                
                
                
                 
                
            }
    
            
            
            
        }
        
        
        tipo_elegido
    }) 
    
    output$table1 <- renderTable({ 
        tipos()
    })
    
    
    
    data<-reactive ({
      
      if(input$var2==""){
        
        if (input$Zona==FALSE){
          if(input$Sexo==FALSE){
            
            
            
            data<-aggregate(seguro$factor07,list(seguro$asegurado,seguro$year), sum)
            colnames(data)<-c("Seguro","year","personas")
            
            data<-data%>%
              group_by(year)%>%
              mutate(percent = round(personas/sum(personas),2))%>%
              filter(Seguro==1)%>%
              ungroup()%>%
              select(percent,year)
        
            
          }else{
            
            data<-aggregate(seguro$factor07,list(seguro$asegurado,seguro$year,seguro$sexo), sum)
            colnames(data)<-c("Seguro","year","sexo","personas")
            
            data<-data%>%
              group_by(year,sexo)%>%
              mutate(percent = round(personas/sum(personas),2))%>%
              filter(Seguro==1)%>%
              ungroup()%>%
              select(sexo,percent,year)
                   }
          
        }else{
          if(input$Sexo==FALSE){
            
            data<-aggregate(seguro$factor07,list(seguro$asegurado,seguro$year,seguro$zona), sum)
            colnames(data)<-c("Seguro","year","zona","personas")
            
            data<-data%>%
              group_by(year,zona)%>%
              mutate(percent = round(personas/sum(personas),2))%>%
              filter(Seguro==1)%>%
              ungroup()%>%
              select(zona,percent,year)
     
          }else{
            
            data<-aggregate(seguro$factor07,list(seguro$asegurado,seguro$year,seguro$zona,seguro$sexo), sum)
            colnames(data)<-c("Seguro","year","zona","sexo","personas")
            
            data<-data%>%
              group_by(year,zona,sexo)%>%
              mutate(percent = round(personas/sum(personas),2))%>%
              filter(Seguro==1)%>%
              ungroup()%>%
              select(zona,sexo,percent,year)%>%
              mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
              select(categoria,percent,year)
              
              } 
          
          
        }
        
        
      }else{
        
        if(input$var2=="SIS"){
          
          if (input$Zona==FALSE){
            if(input$Sexo==FALSE){
              
              data<-aggregate(seguro$factor07,list(seguro$SIS,seguro$year), sum)
              colnames(data)<-c("Seguro","year","personas")
              
              data<-data%>%
                group_by(year)%>%
                mutate(percent = round(personas/sum(personas),2))%>%
                filter(Seguro==1)%>%
                ungroup()%>%
                select(percent,year)
         
            }else{
              
              data<-aggregate(seguro$factor07,list(seguro$SIS,seguro$year,seguro$sexo), sum)
              colnames(data)<-c("Seguro","year","sexo","personas")
              
              data<-data%>%
                group_by(year,sexo)%>%
                mutate(percent = round(personas/sum(personas),2))%>%
                filter(Seguro==1)%>%
                ungroup()%>%
                select(sexo,percent,year)
       
              
            }
            
          }else{
            if(input$Sexo==FALSE){
              
              
              
              data<-aggregate(seguro$factor07,list(seguro$SIS,seguro$year,seguro$zona), sum)
              colnames(data)<-c("Seguro","year","zona","personas")
              
              data<-data%>%
                group_by(year,zona)%>%
                mutate(percent = round(personas/sum(personas),2))%>%
                filter(Seguro==1)%>%
                ungroup()%>%
                select(zona,percent,year)
          
              
            }else{
              
              
              
              data<-aggregate(seguro$factor07,list(seguro$SIS,seguro$year,seguro$zona,seguro$sexo), sum)
              colnames(data)<-c("Seguro","year","zona","sexo","personas")
              
              data<-data%>%
                group_by(year,zona,sexo)%>%
                mutate(percent = round(personas/sum(personas),2))%>%
                filter(Seguro==1)%>%
                ungroup()%>%
                select(zona,sexo,percent,year)%>%
                mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
                select(categoria,percent,year)
              
            } 
            
            
          }
          
        }
        if(input$var2=="ESSALUD"){
          
          if (input$Zona==FALSE){
            if(input$Sexo==FALSE){
              
              data<-aggregate(seguro$factor07,list(seguro$ESSALUD,seguro$year), sum)
              colnames(data)<-c("Seguro","year","personas")
              
              data<-data%>%
                group_by(year)%>%
                mutate(percent = round(personas/sum(personas),2))%>%
                filter(Seguro==1)%>%
                ungroup()%>%
                select(percent,year)
         
            }else{
              
              data<-aggregate(seguro$factor07,list(seguro$ESSALUD,seguro$year,seguro$sexo), sum)
              colnames(data)<-c("Seguro","year","sexo","personas")
              
              data<-data%>%
                group_by(year,sexo)%>%
                mutate(percent = round(personas/sum(personas),2))%>%
                filter(Seguro==1)%>%
                ungroup()%>%
                select(sexo,percent,year)
             }
            
          }else{
            if(input$Sexo==FALSE){
              
              
              
              data<-aggregate(seguro$factor07,list(seguro$ESSALUD,seguro$year,seguro$zona), sum)
              colnames(data)<-c("Seguro","year","zona","personas")
              
              data<-data%>%
                group_by(year,zona)%>%
                mutate(percent = round(personas/sum(personas),2))%>%
                filter(Seguro==1)%>%
                ungroup()%>%
                select(zona,percent,year)
              
            
            }else{
              
              
              
              data<-aggregate(seguro$factor07,list(seguro$ESSALUD,seguro$year,seguro$zona,seguro$sexo), sum)
              colnames(data)<-c("Seguro","year","zona","sexo","personas")
              
              data<-data%>%
                group_by(year,zona,sexo)%>%
                mutate(percent = round(personas/sum(personas),2))%>%
                filter(Seguro==1)%>%
                ungroup()%>%
                select(zona,sexo,percent,year)%>%
                mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
                select(categoria,percent,year)
              
            } 
            
            
          }
          
          
          
          
        }
        if(input$var2=="SEGURO PRIVADO"){
          
          if (input$Zona==FALSE){
            if(input$Sexo==FALSE){
              
              data<-aggregate(seguro$factor07,list(seguro$S_PRIVADO,seguro$year), sum)
              colnames(data)<-c("Seguro","year","personas")
              
              data<-data%>%
                group_by(year)%>%
                mutate(percent = round(personas/sum(personas),2))%>%
                filter(Seguro==1)%>%
                ungroup()%>%
                select(percent,year)
              
            }else{
              
              data<-aggregate(seguro$factor07,list(seguro$S_PRIVADO,seguro$year,seguro$sexo), sum)
              colnames(data)<-c("Seguro","year","sexo","personas")
              
              data<-data%>%
                group_by(year,sexo)%>%
                mutate(percent = round(personas/sum(personas),2))%>%
                filter(Seguro==1)%>%
                ungroup()%>%
                select(sexo,percent,year)
              
            }
            
          }else{
            if(input$Sexo==FALSE){
              
              
              
              data<-aggregate(seguro$factor07,list(seguro$S_PRIVADO,seguro$year,seguro$zona), sum)
              colnames(data)<-c("Seguro","year","zona","personas")
              
              data<-data%>%
                group_by(year,zona)%>%
                mutate(percent = round(personas/sum(personas),2))%>%
                filter(Seguro==1)%>%
                ungroup()%>%
                select(zona,percent,year)
              
              
            }else{
              
              
              
              data<-aggregate(seguro$factor07,list(seguro$S_PRIVADO,seguro$year,seguro$zona,seguro$sexo), sum)
              colnames(data)<-c("Seguro","year","zona","sexo","personas")
              
              data<-data%>%
                group_by(year,zona,sexo)%>%
                mutate(percent = round(personas/sum(personas),2))%>%
                filter(Seguro==1)%>%
                ungroup()%>%
                select(zona,sexo,percent,year)%>%
                mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
                select(categoria,percent,year)
               
              
            } 
            
            
          }
          
          
          
          
        }
        
        
        
        
      }
      data
      
    }) 
    
    output$seguro_plot<-renderPlotly({
      data<-data()
      
      if (input$Zona==FALSE){
        if(input$Sexo==FALSE){
          
          plot_ly(data, x = ~year, y = ~percent)%>%
            add_lines()%>%
            layout(xaxis = list(title = "Fecha"), 
                   yaxis = list(title = "Porcentaje"))
          
          
        }else{
          
          
          plot_ly(data, x = ~year, y = ~percent, color = ~sexo)%>%
            add_lines()%>%
            layout(xaxis = list(title = "Fecha"), 
                   yaxis = list(title = "Porcentaje"))
        }
        
      }else{
        if(input$Sexo==FALSE){
          
          
          plot_ly(data, x = ~year, y = ~percent, color = ~zona)%>%
            add_lines()%>%
            layout(xaxis = list(title = "Fecha"), 
                   yaxis = list(title = "Porcentaje"))
          
          
          
          
        }else{
          
          data<-data()
          plot_ly(data, x = ~year, y = ~percent, color = ~categoria)%>%
            add_lines()%>%
            layout(xaxis = list(title = "Fecha"), 
                   yaxis = list(title = "Porcentaje"))
          
          
        } 
        
        
      }
      
      
    }) 
    
  
    pacientes<-reactive ({
      if(input$var3==""){
      pacientes<-diagnosticos%>%
        filter(Codigo=="F20"|Codigo=="F32"|Codigo=="F33"|Codigo=="F31"|Codigo=="F84"|Codigo=="F90"|Codigo=="F42"|Codigo=="F41")

      }else{
      
        pacientes<-diagnosticos%>%
          filter(year==input$var3) 
        
        }
      pacientes
      
      })
    output$diagnostico_plot<-renderPlotly({
      pacientes<-pacientes()
      if(input$var3==""){
    plot_ly(pacientes,x = ~year, y = ~Personas, color = ~descripcion, type = "bar")
      }else{
        plot_ly(pacientes,labels = ~descripcion,values = ~Personas, type = "pie")
         
      }
    }) 
    
    
    
    data_contagios<-reactive({
      if(input$distrito==""){
        if(input$provincia==""){
          if(input$region==""){
            data1
          }else{
            contagios<-data1%>%
              filter(DEPARTAMENTO==input$region)
            contagios
          }          
          
        }else{
          contagios<-data1%>%
            filter(PROVINCIA==input$provincia)
          contagios
          
        }
      }else{
        contagios<-data1%>%
          filter(DISTRITO==input$distrito)
        contagios
      }
        })
    
    
    output$contagios_plot<-renderPlotly({
      
      data_contagios<-data_contagios()
      
      if (input$edad==FALSE){
        if(input$sexo2==FALSE){
          
          
    contagios<-data_contagios%>%
      select(FECHA)%>%
      group_by(FECHA)%>%
      add_count(FECHA)%>%
      rename(CASOS_CONFIRMADOS=n)%>%
      distinct()%>%
      arrange(FECHA)%>%
      ungroup()
    
    
    plot_ly(contagios,x = ~FECHA, y = ~CASOS_CONFIRMADOS)
        }else{
          contagios<-data_contagios%>%
            select(FECHA,SEXO)%>%
            group_by(FECHA)%>%
            add_count(SEXO)%>%
            rename(CASOS_CONFIRMADOS=n, FECHA=FECHA)%>%
            distinct()%>%
            arrange(FECHA)%>%
            ungroup()%>%
            filter(SEXO!="")
          
          plot_ly(contagios,x = ~FECHA, y = ~CASOS_CONFIRMADOS, color = ~SEXO, type = "bar")
          
        }}else{
          if(input$sexo2==FALSE){
            contagios<-data_contagios%>%
              select(FECHA,Rango_edad)%>%
              group_by(FECHA)%>%
              add_count(Rango_edad)%>%
              rename(CASOS_CONFIRMADOS=n, FECHA=FECHA)%>%
              distinct()%>%
              arrange(FECHA)%>%
              ungroup()%>%
              filter(Rango_edad!="")
            
            plot_ly(contagios,x = ~FECHA, y = ~CASOS_CONFIRMADOS, color = ~Rango_edad, type = "bar")
          }else{
            contagios<-data_contagios%>%
              select(FECHA,Rango_edad,SEXO)%>%
              group_by(FECHA)%>%
              add_count(Rango_edad,SEXO)%>%
              rename(CASOS_CONFIRMADOS=n, FECHA=FECHA)%>%
              distinct()%>%
              arrange(FECHA)%>%
              ungroup()%>%
              filter(SEXO!="" & Rango_edad!="")%>%
              mutate(categoria=paste(substr(SEXO,1,1),"-",Rango_edad))
            
            plot_ly(contagios,x = ~FECHA, y = ~CASOS_CONFIRMADOS, color = ~categoria, type = "bar")
            }
          
        }
    })
    
    data_fallecidos<-reactive({
      if(input$distrito2==""){
        if(input$provincia2==""){
          if(input$region2==""){
            data2
          }else{
            fallecidos<-data2%>%
              filter(DEPARTAMENTO==input$region2)
            fallecidos
          }          
          
        }else{
          fallecidos<-data2%>%
            filter(PROVINCIA==input$provincia2)
          fallecidos
          
        }
      }else{
        fallecidos<-data2%>%
          filter(DISTRITO==input$distrito2)
        fallecidos
      }
    })
    
    
    output$fallecidos_plot<-renderPlotly({
      
      data_fallecidos<-data_fallecidos()
      
      if (input$edad2==FALSE){
        if(input$sexo3==FALSE){
          
          
          fallecidos<-data_fallecidos%>%
            select(FECHA)%>%
            group_by(FECHA)%>%
            add_count(FECHA)%>%
            rename(FALLECIDOS=n)%>%
            distinct()%>%
            arrange(FECHA)%>%
            ungroup()
          
          
          plot_ly(fallecidos,x = ~FECHA, y = ~FALLECIDOS)
        }else{
          fallecidos<-data_fallecidos%>%
            select(FECHA,SEXO)%>%
            group_by(FECHA)%>%
            add_count(SEXO)%>%
            rename(FALLECIDOS=n, FECHA=FECHA)%>%
            distinct()%>%
            arrange(FECHA)%>%
            ungroup()%>%
            filter(SEXO!="")
          
          plot_ly(fallecidos,x = ~FECHA, y = ~FALLECIDOS, color = ~SEXO, type = "bar")
          
        }}else{
          if(input$sexo3==FALSE){
            fallecidos<-data_fallecidos%>%
              select(FECHA,Rango_edad)%>%
              group_by(FECHA)%>%
              add_count(Rango_edad)%>%
              rename(FALLECIDOS=n, FECHA=FECHA)%>%
              distinct()%>%
              arrange(FECHA)%>%
              ungroup()%>%
              filter(Rango_edad!="")
            
            plot_ly(fallecidos,x = ~FECHA, y = ~FALLECIDOS, color = ~Rango_edad, type = "bar")
          }else{
            fallecidos<-data_fallecidos%>%
              select(FECHA,Rango_edad,SEXO)%>%
              group_by(FECHA)%>%
              add_count(Rango_edad,SEXO)%>%
              rename(FALLECIDOS=n, FECHA=FECHA)%>%
              distinct()%>%
              arrange(FECHA)%>%
              ungroup()%>%
              filter(SEXO!="" & Rango_edad!="")%>%
              mutate(categoria=paste(substr(SEXO,1,1),"-",Rango_edad))
            
            plot_ly(fallecidos,x = ~FECHA, y = ~FALLECIDOS, color = ~categoria, type = "bar")
          }
          
        }
    })
    adulto_mayor<-reactive({
          if(input$region3==""){
            data_adulto
          }else{
            adulto<-data_adulto%>%
              filter(region==input$region3)
            adulto
          }
    })
    
    output$adulto_plot<-renderPlotly({
      
      adulto_mayor<-adulto_mayor()
      
      if (input$zona2==FALSE){
        if(input$sexo4==FALSE){
          
          data<-aggregate(adulto_mayor$factor07,list(adulto_mayor$p401,adulto_mayor$year),sum)
          
          colnames(data) <- c("enfermo","year","personas") 
          
          data<-data%>%
            group_by(year)%>%
            mutate(percent = round((personas/sum(personas))*100,1))%>%
            filter(enfermo=="si")%>%
            ungroup()%>%
            select(percent,year)
          
          plot_ly(data,x = ~year, y = ~percent, type = 'scatter', mode = 'lines')
        }else{
          
          
          data<-aggregate(adulto_mayor$factor07,list(adulto_mayor$p401,adulto_mayor$year,adulto_mayor$p207),sum)
          
          colnames(data) <- c("enfermo","year","sexo","personas") 
          
          data<-data%>%
            group_by(year,sexo)%>%
            mutate(percent = round((personas/sum(personas))*100,1))%>%
            filter(enfermo=="si")%>%
            ungroup()%>%
            select(percent,year,sexo)
          
          plot_ly(data,x = ~year, y = ~percent,color=~sexo, type = 'scatter', mode = 'lines')
          
          
        }}else{
          if(input$sexo4==FALSE){
            
            data<-aggregate(adulto_mayor$factor07,list(adulto_mayor$p401,adulto_mayor$year,adulto_mayor$zona),sum)
            
            colnames(data) <- c("enfermo","year","zona","personas") 
            
            data<-data%>%
              group_by(year,zona)%>%
              mutate(percent = round((personas/sum(personas))*100,1))%>%
              filter(enfermo=="si")%>%
              ungroup()%>%
              select(percent,year,zona)
            
            plot_ly(data,x = ~year, y = ~percent,color=~zona, type = 'scatter', mode = 'lines')
          }else{
            
            
            data<-aggregate(adulto_mayor$factor07,list(adulto_mayor$p401,adulto_mayor$year,adulto_mayor$zona,adulto_mayor$p207),sum)
            
            colnames(data) <- c("enfermo","year","zona","sexo","personas") 
            
            data<-data%>%
              group_by(year,zona,sexo)%>%
              mutate(percent = round((personas/sum(personas))*100,1))%>%
              filter(enfermo=="si")%>%
              ungroup()%>%
              mutate(categoria=paste(substr(sexo,1,1),"-",zona))%>%
              select(percent,year,categoria)
            
            plot_ly(data,x = ~year, y = ~percent,color=~categoria, type = 'scatter', mode = 'lines')
            
          }
          
        }
    })
    
    salud_inf<-reactive({
      if (input$var4==""){
        ""
      }else{
        if (input$var4=="Anemia"){
          Anemia_reg
        }else{
        if (input$var4=="Desnutricion"){
          Desnutricion_reg
        }
      }
      }
    }) 
    
    output$table2 <- renderTable({ 
      salud_inf()
    })
    
    
    output$salud_inf__plot<-renderPlotly({
      

      if (input$var4==""){
        ""
      }else{
        if (input$var4=="Anemia"){
          plot_ly(Anemia,x = ~year, y = ~percent,color=~categoria, type = 'scatter', mode = 'lines')
        }else{if (input$var4=="Desnutricion"){
          plot_ly(Desnutricion,x = ~year, y = ~percent,color=~categoria, type = 'scatter', mode = 'lines')
        }
        }
      }
        
    })
  
    output$variacion__plot<-renderPlotly({
      
      
      if (input$var4==""){
        ""
      }else{
        if (input$var4=="Anemia"){
          plot_ly(Anemia_reg,x = ~Region, y = ~Variacion, type = "bar")
        }else{if (input$var4=="Desnutricion"){
          plot_ly(Desnutricion_reg,x = ~Region, y = ~Variacion, type = "bar")
        }
        }
      }
      
    })
      
    
}

#runApp(shinyApp(ui, server), launch.browser = TRUE)
shinyApp(ui, server)
