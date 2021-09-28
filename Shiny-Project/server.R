library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggwordcloud)
library(rgdal)
library(stringi)

# Popdata <- Pop[sample.int(nrow(Pop), 10000),]
# # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# # will be drawn last and thus be easier to see
# Popdata <- Popdata[order(Popdata$code),]

monthStart <- function(x) {
  x <- as.POSIXlt(x)
  x$mday <- 1
  as.Date(x)
}

server <- function(input, output, session) {
  
  ## Interactive Map ###########################################
  # Create the map
  output$map <- renderLeaflet({
    leaflet(world_spdf) %>%
    addTiles() %>% 
    setView(lng = 10, lat = 37.45, zoom = 2)
    # addPolygons(fillColor=~mypalette(POP2005), stroke=FALSE)
  })

  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  
  sliderMonth <- reactiveValues()
  observe({
    type <- input$type
    data_m <- as.POSIXct(input$slider, tz="GMT")
    mon <- month(data_m)
    ye <- year(data_m)
    Data <- world_spdf@data %>% filter(Year==ye, Mon==mon)# %>% arrange(id)
    # print(Data)
    
    sliderMonth$Month <- as.character(monthStart(data_m))
   
    mybins <- c(1,0.75,0.5,0.25,0,-0.25,-0.5,-0.75,-1)
    cPal <- colorBin(palette=rev(brewer.pal(9, "PuOr")), domain=world_spdf@data$ratio, 
                         na.color="transparent", bins=mybins)
    mytext <- paste(
      "Country: ", Data$NAME,"<br/>", 
      "Population(M): ", Data$POP2005, "<br/>",
      "Total tweets: ", round(Data$total), "<br/>",
      "Sentiment: ", round(Data$ratio, 3), 
      sep="") %>%
      lapply(htmltools::HTML)
    
    if (type == "Points"){
      leafletProxy("map", data=Data) %>%
        clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
        addCircleMarkers(layerId=~NAME, lng=~LON, 
                         lat=~LAT, fillColor=~cPal(ratio),
                         radius=~log(total*10), stroke=TRUE, fillOpacity=0.9, color="white",
                         weight=0.3, label=mytext, labelOptions = labelOptions( 
                           style = list("font-weight" = "normal", padding = "3px 8px"), 
                           textsize = "13px", 
                           direction = "auto"
                         )
        ) %>%
        addLegend( pal=cPal, values=~ratio, opacity=0.9, title = "Sentiment ratio", 
                   position = "bottomleft" )

    }else if(type=="Regions"){
      leafletProxy("map", data=world_spdf) %>%
        # clearShapes() %>%
        clearMarkers() %>%
        clearControls() %>%
        addPolygons(fillColor=cPal(Data$ratio), stroke=TRUE, fillOpacity=0.9, color="white",
                    weight=0.3, label=mytext, labelOptions = labelOptions( 
                      style = list("font-weight" = "normal", padding = "3px 8px"), 
                      textsize = "13px", 
                      direction = "auto"
                    )
        ) %>%
        addLegend( pal=cPal, values=~ratio, opacity=0.9, title = "Sentiment ratio", 
                   position = "bottomleft" )
      
    }
    output$SliderText <- renderText({sliderMonth$Month})
    output$histPo <- renderPlot({
        ggplot(hashtag %>% filter(Year==ye, Month==mon), aes(label = stri_trans_general(Hashtag, "Latin-ASCII"), size = hash_count, col = as.character(hash_count))) +
          geom_text_wordcloud(area_corr_power = 0.05, rm_outside = TRUE) +
          scale_size_area(max_size = 5)+
          theme_minimal()
    })
  })

  
  
  # Show a popup at the given location
  # showPopInfo <- function(id, lat, lon, year) {
  #   content <- as.character(tagList(
  #     tags$h4("Country:", as.character(Pop$name)),
  #     tags$strong(HTML(sprintf("%s, %f %f",
  #                              as.character(Pop$code), Pop[id, year-1987], Pop[id, 206+year-1993]
  #     ))), tags$br(),
  #     sprintf("Median household income: %f", Pop$T1994), tags$br(),
  #     sprintf("Percent of adults with BA: %f", Pop$T1995), tags$br(),
  #     sprintf("Adult population: %f", Pop$T1994)
  #   ))
  #   leafletProxy("map") %>% addPopups(lon, lat, content, layerId = id)
  # }
  # 
  # observe({
  #   year <- input$year
  #   click <- input$map_marker_click
  #   # if(is.na(click))
  #     # click$id=data.frame(id=nrow(Pop), lon=NA, lat=NA) 
  #   output$histPo <- renderPlot({
  #       ggplot()+geom_histogram(x=as.vector(unlist(Pop[click$id ,7:31])),
  #         main = "Population (1994-2018)",
  #         xlab = "Popation",
  #         col = 'coral')
  #     })
  # })
  # 
  # observe({
  #   year <- input$year
  #   click <- input$map_shape_click
  #   if (is.null(click))
  #     return()
  #   isolate({
  #     showPopInfo(click$id, click$lat, click$lon, year)
  #   })
  # })



  ## Background ###########################################
  # logistic <- function(t, r, p0, pm){
  #   return(pm/(1+(pm/p0-1)*exp(-r*t)))
  # }
  # observe({
  #   output$plot_2_l <- renderPlot({
  #     p<-ggplot(data=tibble(t=0:10), aes(t))
  #     for(i in seq(0.1,1.5,0.1)){
  #       p <- p+stat_function(fun=logistic, args=list(r=2,p0=i,pm=1), 
  #                            col=rgb(i*2/3,0.5,0.5))
  #     }
  #     p
  #   })
  # })
  
  
  
 
  # observe({
  #   country <- input$country
  #   year_0 <- input$year0
  #   till <- 2050
  #   c_i <- which(Pop$name==country)
  #   num <- as.vector(unlist(c(Pop[c_i, (6+year_0[1]-1993):(6+year_0[2]-1993)])))
  #   n <- length(num)
  #   rowdata <- data.frame(year=c(as.numeric(year_0[1]):as.numeric(year_0[2])), Popution=num)
  #   colnames(rowdata) <- c("year","Population")
  #   getPred <- Logis.pred(num, year_0, till)
  #   predData <- data.frame(year=getPred[1], Population=getPred[2])
  #   colnames(predData) <- c("year","Population")
  #   
  #   error <- (predData$Population[1:(n-1)]-rowdata$Population[2:n])/rowdata$Population[2:n]
  #   ep <- data.frame(data=predData$Population[1:(n-1)], error=error)
  #   colnames(ep) <- c("data", "error")
    
    
  # })
  
  ########### wait 
  # observe({
  #   year <- if (is.null(input$states)) character(0) else {
  #     cleantable 
  #   }
  #   stillSelected <- isolate(input$year)
  #   updateSelectizeInput(session, "year", choices = c(1994:2018),
  #     selected = stillSelected, server = TRUE)
  # })
}
