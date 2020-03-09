
server <- function(input, output) {
  
  
  ########## Graph for Map #############
  
  lfdt <- reactiveValues(m=NULL,d=NULL,nb=NULL,sign = NULL)
  
  observeEvent(
    eventExpr = list(input$cri, input$radio, input$acc),
    {
      countd = data %>% filter(Accomodates <= input$acc) %>% 
        filter(Room.Type %in% input$radio ) %>%
        select (Neighbourhood,lat,long) %>% 
        group_by(Neighbourhood,lat,long) %>% 
        summarise(count=length(Neighbourhood))
      countd = as.data.frame(countd)
      
      priced = data %>% filter(Accomodates <= input$acc) %>%
        filter(Room.Type %in% input$radio ) %>%
        select (Neighbourhood,lat,long,Price) %>% 
        group_by(Neighbourhood,lat,long) %>% 
        summarise(price=round(mean(Price),0))
      priced = as.data.frame(priced)
      
      
      if  (input$cri == "Listing Count") {lfdt$d = countd; lfdt$m = countd$count ; lfdt$nb = countd$Neighbourhood ; lfdt$sign = " "} 
      else   {lfdt$d = priced; lfdt$m = priced$price ; lfdt$nb = priced$Neighbourhood ; lfdt$sign = "$" }
      
    }
  )
  
  output$geo <- renderLeaflet({ 
    
    color_pal <- colorBin(palette = c('#FFF6E6', '#79CCCD','#F16664'), 
                          domain = lfdt$m, bins = 5, reverse = F)
    
    
    leaflet(lfdt$d) %>% addTiles() %>%
      setView(lng = -73.98928, lat = 40.75042, zoom = 12) %>%
      addCircleMarkers(radius = 10,color = ~color_pal(lfdt$m),label= paste(lfdt$nb," : ",lfdt$sign,lfdt$m,sep = " "),fillOpacity = 0.9,
      labelOptions = labelOptions( textsize='16px' ) )%>%
      addProviderTiles("CartoDB.DarkMatter",
                       options = providerTileOptions(noWrap = TRUE))
    
  }
  )
  
  ########## Graph with Marker Click #############
  
  click <- reactiveValues(
    nh = NULL,  # nh = neighbourhood
    pr = NULL, rt = NULL,  # pr = price, rt = room type
    ac = NULL # accomodates
  )
  
  # what happens when marker clicked
  #####################################
  observeEvent(
    eventExpr = input$geo_marker_click,
    {
      aa <- input$geo_marker_click
      # isolate the row in the data containing the selected property
      dataclick <- data %>% 
        filter(lat == aa$lat & long == aa$lng) #<--need to filter on lat and long
      # filter(Neighbourhood == aa$Neighbourhood)
      # populate click with the various attributes from the selected row
      click$nh <- dataclick$Neighbourhood
      click$pr <- dataclick$Price
      click$rt <- dataclick$Room.Type
      click$ac <- dataclick$Accomodates
    }
  )
  # boxplot triggered by marker click
  #####################################
  output$propPlot <- renderPlot(
    {
      if (!is.null(click$nh)) {
        z = data %>% filter(Neighbourhood == click$nh)
        
        ggplot(z,aes(x=click$pr,..scaled..,color=click$rt,fill=click$rt))+
          geom_density(alpha = 0.5)+scale_fill_manual(values=c('#79CCCD', '#007D8C','#F16664'))+
          scale_color_manual(values=c('#79CCCD', '#007D8C','#F16664'))+  
          xlim(0,400) + labs(x="Price",y="Distribution",title = "Price by Room Type")+ 
          theme_minimal(base_size = 13) + theme(legend.position = "bottom")+
          theme(
            axis.text = element_text(colour = "white"),
            axis.title.x = element_text(colour = "white", size=rel(1.5)),
            axis.title.y = element_text(colour = "white",size=rel(1.5)),
            panel.background = element_rect(fill="black",colour = "black"),
            panel.grid = element_blank(),
            plot.background = element_rect(fill="black",colour = "black"),
            legend.key = element_rect(fill = "black",colour = "black"),
            legend.box.background = element_blank(),
            legend.background = element_blank(),
            legend.text = element_text(colour="white",size = rel(1)),
            plot.title = element_text(color="white", size= rel(2),hjust = 0.5)
          )   
        
      } 
    } , bg = "transparent"
  )
  # scatter plot triggered by marker click
  #####################################
  output$sctplot <- renderPlot(
    {
      if (!is.null(click$nh)) {
        y = data %>% filter(Neighbourhood == click$nh) 
        
        ggplot(y,aes(y=click$pr,x = click$ac, color = click$rt, fill = click$rt)) +
          geom_point(position = "jitter",size=3) + geom_smooth(size=1)+scale_fill_manual(values=c('#79CCCD', '#007D8C','#F16664'))+
          scale_color_manual(values=c('#79CCCD', '#007D8C','#F16664'))+theme_minimal(base_size = 13)+
          theme(legend.position = "bottom")+labs(x="# of Accomodation",y="Price",title = "Price by Accomodation")+
          theme(
            axis.text = element_text(colour = "white"),
            axis.title.x = element_text(colour = "white", size=rel(1.5)),
            axis.title.y = element_text(colour = "white",size=rel(1.5)),
            panel.background = element_rect(fill="black",colour = "black"),
            panel.grid = element_blank(),
            plot.background = element_rect(fill="black",colour = "black"),
            legend.key = element_rect(fill = "black",colour = "black"),
            legend.box.background = element_blank(),
            legend.background = element_blank(),
            legend.text = element_text(colour="white",size = rel(1)),
            plot.title = element_text(color="white", size= rel(2),hjust = 0.5)
          )   
      }
    } , bg = "transparent"
  )
  
  ########## TEXT with Marker Click #############
  text <- reactiveValues(
    nh = NULL)  # nh = neighbourhood
  
  observeEvent(
    eventExpr = input$geo_marker_click,
    {
      ab <- input$geo_marker_click
      # isolate the row in the data containing the selected property
      datatext <- data %>% 
        filter(lat == ab$lat & long == ab$lng)
      
      # I created data2 at the top of the script   
      data2 = data2 %>%  filter(Neighbourhood == datatext$Neighbourhood[1])
      text$nh = data2$Neighbourhood
      print(text$nh)
    }
  )
  
  output$nb_name <- renderText(
    if (!is.null(text$nh))
    { text$nh
    })  

  url <- a(h5("Github Link to Code"),style="color:#007D8C", href="https://github.com/kerimbirgun/R-Shiny-Airbnb-Project")
  output$tab <- renderUI({
    tagList(url)


  })  
  
}


