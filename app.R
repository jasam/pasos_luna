# Load libraries
library(shinydashboard)
library(shiny)
library(ggplot2)
library(data.table)
library(lubridate)
library(zoo)
library(leaflet)

# Global vars ----
sep = ","
ALL = "todas"

# Filter weeks
weeks = seq(1,10)

# Boxplot with values ----
fun_mean = function(x){
    return(data.frame(y=round(mean(x)),label=round(mean(x,na.rm=T))))}


# Returns data set converted ----
create_data = function(file_path, users, weeks, header=FALSE, file_4 = FALSE) {
    
    # Read data
    dt = fread(file_path, 
               sep = sep,
               colClasses="character",
               header = header
    )
    
    # Normalize names
    names(dt)[1] = "semana"
    names(dt)[2] = "fecha"
    names(dt)[3] = users[1]
    names(dt)[4] = users[2]
    names(dt)[5] = users[3]
    
    if (file_4) {
        names(dt)[6] = users[4]
    }
    
    # Casting date
    dt[, fecha := dmy(fecha)]
    
    # LOCF
    dt[semana == "", semana := NA]
    dt[, semana := na.locf(semana, na.rm=FALSE)]
    
    # Long table
    dt_melt = melt(dt, id.vars = c("fecha", "semana"), 
                   measure.vars = users)
    
    # Filter weeks
    dt_melt = dt_melt[semana %in% weeks]
    
    # Casting
    dt_melt$value = as.numeric(dt_melt$value)
    
    return(dt_melt)
}

# Team 1 ----
# It's important the same order respect to excel
users = c("jescobar", "jruiz", "gmontoya")

# Team 1
file_path = "Pasos a la luna - Team 1.csv"

dt = create_data(file_path, users, weeks, TRUE)

dt_full = dt

# Team 2 ----

# It's important the same order respect to excel
users = c("oxiques", "jrey", "jgomez")

# Team 2
file_path = "Pasos a la luna - Team 2.csv"

dt = create_data(file_path, users, weeks, TRUE)
dt_full = rbind(dt_full, dt)

# Team 3 ----
# It's important the same order respect to excel
users = c("smerchan", "kolivar", "jsoto")

# Team 3
file_path = "Pasos a la luna - Team 3.csv"

dt = create_data(file_path, users, weeks, TRUE)
dt_full = rbind(dt_full, dt)

# Team 4 ----
# It's important the same order respect to excel

users = c("jrugeles", "jbotero", "dvargas", "dgomez")

# Team 4
file_path = "Pasos a la luna - Team 4.csv"

dt = create_data(file_path, users, weeks, TRUE, TRUE)
dt_full = rbind(dt_full, dt)

dt_full = dt_full[!is.na(fecha)]

dt_full_st = dt_full[, .(max = max(value, na.rm=TRUE),
                         min = min(value, na.rm=TRUE),
                         sum = sum(value, na.rm=TRUE),
                         mean = mean(value, na.rm=TRUE),
                         median = median(value, na.rm=TRUE),
                         sd = sd(value, na.rm=TRUE),
                         count = sum(!is.na(value))),
                     by=c("variable", "semana")]

# Mark team
dt_full_st$team = 1

dt_full_st[variable == "oxiques" | variable == "jrey" |
               variable == "jgomez", team := 2]

dt_full_st[variable == "smerchan" | variable == "kolivar" |
               variable == "jsoto", team := 3]

dt_full_st[variable == "jrugeles" | variable == "jbotero" |
               variable == "dvargas" | variable == "dgomez", team := 4]

# by team
dt_full_st_gr = dt_full_st[, .(sum = sum(sum, na.rm=TRUE)),
                           by=c("team", "semana")]

dt_full_st_gr$team = as.factor(dt_full_st_gr$team)

# for UI
weeks_ui = c(ALL, unique(dt_full$semana))
walkers = unique(dt_full$variable)



ui <- dashboardPage(
    dashboardHeader(title = "DKRT analytics \n pasos a la luna"),
    dashboardSidebar(selectInput(inputId = "week",
                                 label = "Seleccionar semana",
                                 choices = weeks_ui),
                     
                     selectInput(inputId = "walker",
                                 label = "Seleccionar caminante",
                                 choices = walkers)),
    
    dashboardBody(
        fluidRow(
            tabsetPanel(
                tabPanel(title = "Equipo", 
                         plotOutput("bars_team"),
                         h5("Nota: el equipo 4 tiene un descuento del 25% sobre el total de pasos"),
                         plotOutput("box_walkers"),
                         plotOutput("trend_team"),
                         plotOutput("lm_per_walkers"),
                         plotOutput("lines_team")
                         ),
                tabPanel(title = "Caminante",
                         plotOutput("bars_walkers"),
                         plotOutput("box_per_walker"),
                         plotOutput("lm_per_walker")
                         ),
                tabPanel(title = "Premios",
                         valueBoxOutput("rate"),
                         valueBoxOutput("rate2"),
                         valueBoxOutput("rate3")
                         ),
                tabPanel(title = "Historia del viaje",
                         leafletOutput("mymap")
                ),
                tabPanel(title = "About",
                         includeMarkdown("about.md")
                         
               )
            )
        )
)  )


server <- function(input, output) {
    
    output$bars_team <- renderPlot({
        
        week = input$week
        
        if (week == ALL) {
            dt_sub = dt_full_st_gr[, .(sum = sum(sum, na.rm=TRUE)), by = c("team")]
        }
        
        else{
            dt_sub = dt_full_st_gr[semana == week]
        }
        
        dt_sub[team == 4, sum := round(sum * .75)]
        
        title = paste0("Pasos por equipo total semana: ", week)
        x = "Team"
        y =  "Cantidad de pasos"
        pl <- ggplot(dt_sub, aes(reorder(team, sum), sum, fill=team))
        pl <- pl + geom_col(show.legend = FALSE)
        pl <- pl + coord_flip()
        pl <- pl + geom_text(aes(label=sum))
        pl <- pl + labs(title=title, x=x, y=y)
        pl
    })    
    
    output$lines_team <- renderPlot({
        
        title = "Pasos por equipo por persona por team por semana"
        x = "Equipos"
        y =  "Cantidad de pasos"
        ggplot(data=dt_full_st, aes(x=variable, y=sum, group=semana)) +
            geom_line(aes(color=semana), size=1.2) +
            geom_point(aes(color=semana), color="red", size=3) +
            labs(title=title, x=x, y=y) +
            facet_grid(team ~ ., scales = "free")
    })
    
    output$trend_team <- renderPlot({
        
        title = "Pasos por equipo por semana"
        x = "Equipos"
        y =  "Cantidad de pasos"
        ggplot(data=dt_full_st_gr, aes(x=team, y=sum, group=semana)) +
            geom_line(aes(color=semana), size=1.2) +
            geom_point(aes(color=semana), color="red", size=3) +
            labs(title=title, x=x, y=y)
        
    })
    
    output$box_walkers <- renderPlot({
        
        week = input$week
        
        if (week == ALL) {
            dt_sub = dt_full
        }
        
        else{
            dt_sub = dt_full[semana == week]
        }
        
        title = paste0("Pasos promedio y boxplot semana: ", week)
        x = "Caminantes"
        y =  "Cantidad de pasos"
        p = ggplot(dt_sub, aes(variable, value, fill = variable))
        p = p + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
        p = p + stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) 
        p = p + stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)
        p = p + labs(title=title, x=x, y=y)
        p
        
    })
    
    output$bars_walkers <- renderPlot({
        
        
        dt_sub = dt_full[, .(sum = sum(value, na.rm=TRUE)), 
                         by = c("variable")] 
        dt_sub[, per := round((sum / sum(sum)) * 100)]
        
        title = "Pasos por persona acumulado y porcentaje aporte a pasos totales team DRKT"
        x = "Persona"
        y =  "Cantidad de pasos"
        pl <- ggplot(dt_sub, aes(reorder(variable, sum), sum, fill=variable))
        pl <- pl + geom_col(show.legend = FALSE)
        pl <- pl + coord_flip()
        pl <- pl + geom_text(aes(label=paste0(sum, "->", sprintf("%1.0f%%", per, colour="black"))))
        pl <- pl + labs(title=title, x=x, y=y)
        pl
        
    })
    
    output$box_per_walker <- renderPlot({
        
        walker = input$walker
        
        dt_sub = dt_full[variable == walker]
        
        title = paste0("Pasos todas las semanas caminante: ", walker)
        x = "Semana"
        y =  "Cantidad de pasos"
        p = ggplot(dt_sub, aes(semana, value, fill = semana))
        p = p + geom_boxplot(outlier.colour = "red", outlier.shape = 1)
        p = p + stat_summary(fun.y = mean, geom="point",colour="darkred", size=2) 
        p = p + stat_summary(fun.data = fun_mean, geom="text", vjust=-0.7)
        p = p + labs(title=title, x=x, y=y)
        p
        
    })
    
    output$lm_per_walker <- renderPlot({
        
        walker = input$walker
        
        dt_sub = dt_full[variable == walker]
        
        title = paste0("Tendencia por caminante: ", walker)
        x = "Dias"
        y =  "Cantidad pasos"
        
        ggplot(dt_sub, aes(x=fecha, y=value)) + 
            geom_point() + 
            stat_smooth(method=lm) +
            labs(title=title, x=x, y=y)
        
    })
    
    output$lm_per_walkers <- renderPlot({
        
        title = paste0("Tendencia todos los caminantes")
        x = "Dias"
        y =  "Cantidad pasos"
        
        ggplot(dt_full, aes(x=fecha, y=value)) + 
            geom_point() + 
            stat_smooth(method=lm) +
            labs(title=title, x=x, y=y) + 
            facet_grid(. ~ variable)
        
    })
    
    output$rate <- renderValueBox({
        
        dt_sub = dt_full_st_gr[, .(sum = sum(sum, na.rm=TRUE)), by = c("team")]
        
        dt_sub[team == 4, sum := round(sum * .75)]
        
        dt_sub = dt_sub[order(-sum)]
        
        team = dt_sub[, "team"][1]
        steps = dt_sub[, "sum"][1]
            
        valueBox(
            value = paste0("Team ", team),
            subtitle = paste0("Equipo con mas pasos: ", steps),
            icon = icon("area-chart"),
            color = "aqua"
        )
    })
    
    output$rate2 <- renderValueBox({
        
        dt_sub = dt_full[, .(sum = sum(value, na.rm=TRUE)), 
                         by = c("variable")]
        dt_sub = dt_sub[order(-sum)]
        steps = dt_sub[, "sum"][1]
        wal = dt_sub[, "variable"][1]
        
        valueBox(
            value = wal,
            subtitle = paste0("Caminante con mas pasos totales: ", steps),
            icon = icon("area-chart"),
            color = "yellow"
        )
    })
    
    output$rate3 <- renderValueBox({
        
        dt_full = dt_full[order(-value)]
        steps = dt_full[, "value"][1]
        wal = dt_full[, "variable"][1]
        
        valueBox(
            value = wal,
            subtitle = paste0("Caminante con mas pasos en un dia: ", steps),
            icon = icon("area-chart"),
            color = "orange"
        )
    })
    
    output$mymap <- renderLeaflet({
        greenLeafIcon <- makeIcon(
            iconUrl = "https://i.pinimg.com/564x/35/ed/d9/35edd99d3c739386a2fc1ead210d7490.jpg (14 kB) ",
            iconWidth = 40, iconHeight = 40,
            iconAnchorX = 5, iconAnchorY = 5,
            shadowWidth = 5, shadowHeight = 5,
            shadowAnchorX = 4, shadowAnchorY = 62
        )
        
        
        m <- leaflet() %>%
            addTiles() %>%  # Add default OpenStreetMap map tiles
            addMarkers(lng=-74.053444, lat=4.686361, popup="Start", icon = greenLeafIcon) %>%
            addMarkers(lng=-75.118018, lat=5.586098, popup="1") %>%
            addMarkers(lng=-73.740655, lat=6.904978, popup="2") %>%
            addMarkers(lng=-72.942981, lat=10.683568, popup="3") %>%
            addMarkers(lng=-71.998677, lat=13.191946, popup="4") %>%
            addMarkers(lng=-71.852391, lat=17.439671, popup="5") %>%
            addMarkers(lng=-73.783639, lat=19.956977, popup="6") %>%
            addMarkers(lng=-78.235774, lat=21.548220, popup="7") %>%
            addMarkers(lng=-81.711985, lat=24.423493, popup="8") %>%
            addMarkers(lng=-81.335499, lat=27.149993, popup="9") %>%
            addMarkers(lng=-75.566554, lat=6.264492, popup="Start")
        
        
        m  # Print the map
        seattle_geojson = list(
            type = "Feature",
            geometry = list(
                type = "MultiPolygon",
                coordinates = list(list(list(
                    c(-74.053444, 4.686361),
                    c(-75.118018, 5.586098)
                )))
            ),
            properties = list(
                name = "Ballard",
                population = 48000,
                # You can inline styles if you want
                style = list(
                    fillColor = "yellow",
                    weight = 2,
                    color = "#000000 "
                )
            ),
            id = "ballard"
        )
        
        seattle_geojson2 = list(
            type = "Feature",
            geometry = list(
                type = "MultiPolygon",
                coordinates = list(list(list(
                    c(-75.118018, 5.586098),
                    c(-75.566554, 6.264492)
                )))
            ),
            properties = list(
                name = "Ballard",
                population = 48000,
                # You can inline styles if you want
                style = list(
                    fillColor = "yellow",
                    weight = 2,
                    color = "#000000 "
                )
            ),
            id = "ballard"
        )
        m %>% setView(-75.118018, 5.586098, zoom = 13) %>% addGeoJSON(seattle_geojson)%>% addGeoJSON(seattle_geojson2)    })
}

# Run the application
shinyApp(ui = ui, server = server)
