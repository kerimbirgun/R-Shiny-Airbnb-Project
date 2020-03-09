
ui <- fluidPage(theme = shinytheme("cyborg"),tags$style("
              body {
    -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
    zoom: 0.7; /* Other non-webkit browsers */
    zoom: 70%; /* Webkit browsers */
}
              "),
                p(h2(div(id="dummy",img(height = 65,
                                         width = 65,
                                         src = "Airbnblogo.PNG"),"Pricing Analysis"))), 
                fluidRow(
                  column(align ="center", 2,br(),br(),br(),
                         radioGroupButtons("cri", 
                                           choices = list("Mean Price" = "Mean Price",
                                                          "Listing Count" = "Listing Count"), selected = "Mean Price"),br(),br(),br(),
                         knobInput("acc",label = h4("Minimum Accomodation Capacity"),min = 0,max=16,width = 150,height = 150, rotation = c("clockwise", "anticlockwise"),
                                   fgColor = "#007D8C",inputColor = "#007D8C",lineCap = "round",bgColor ="dark grey",value = 5),br(),br(),br(),
                         radioButtons("radio", label = h4("Room Type"),
                                      choices = list("Entire Apartment" = "Entire Apartment", "Private room" = "Private room", "Shared room" = "Shared room"), 
                                      selected = "Entire Apartment"), br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),uiOutput("tab")
                  ),
                  column(align ="center", 4,
                         h4("Click on Geo-Markers to Activate Graphs"),br(),
                         leafletOutput("geo",height = 800)), 
                  column(1),
                  column(align ="center", 4,h4(textOutput("nb_name")),br(),
                         plotOutput("propPlot"),br(),br(), plotOutput("sctplot"))
                  
                ))



