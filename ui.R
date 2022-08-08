dashboardPage(
  

  # SKIN
  skin = "blue",
  
  # theme = "macarons.json",
  
  # DASHBOARD HEADER
  dashboardHeader(
    title = "UEFA Champions League",
    titleWidth = 350
    
    
  ),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      
      menuItem("Top Team Goal", tabName = "page1", icon = icon("futbol")),
      menuItem("Team Match Result", tabName = "page2", icon = icon("search", lib = "glyphicon")),
      menuItem("Head to Head", tabName = "page3", icon = icon("screenshot", lib = "glyphicon")),
      menuItem("Group Points", tabName = "page4", icon = icon("stats", lib = "glyphicon"))
      
    )
  ),
  dashboardBody(
    
  
    # tags$head(tags$style(HTML(".small-box {height: 220px}"))),
    tabItems(
      
      # ---------------------------- PAGE 1 ---------------------------------
      tabItem(tabName = "page1", 
              
      # fluidPage(theme = bs_theme(version = 4, bootswatch = "superhero"),
              #mainPanel(p("p creates a paragraph of text.")),
          
                      
        fluidRow(
          valueBoxOutput("vbox"),
          valueBoxOutput("vbox2"),
          valueBoxOutput("vbox3"),
        ),
        
        # ROW 1
        fluidRow(
          box(width = 6,
              selectInput(inputId = "input_yearadd",
                          label = "Select Year",
                          choices = unique(stats$year)  )
          ),
          box(width = 6,
              sliderInput(inputId= "bins",
                          label = "Number of rows:",
                          min = 1,
                          max = 20,
                          value = 10,
                          step = 1)),
        ),
          fluidRow(  
            box(width = 6, 
                
                echarts4rOutput("plot1add") ) ,
            
            box(width = 6, 
            
                echarts4rOutput("plot1") ) 
        ),
      # ROW 2
      
        fluidRow(
          # box(width = 2,
          #     sliderInput(inputId= "bins1",
          #                 label = "Number of rows:",
          #                 min = 1,
          #                 max = 20,
          #                 value = 10,
          #                 step = 1)),
          box(width = 6,
              
              echarts4rOutput("plot2add") ),
          
          box(width = 6,
           
                echarts4rOutput("plot2") )
            
            
          )
        )
      ,
      
      # ---------------------------- PAGE 2 ---------------------------------
      
      tabItem(tabName = "page2",
              
              fluidRow(
                valueBoxOutput("vboxres", width = 6),
                valueBoxOutput("vboxres2", width = 6)
              ),
      # ROW 1
      fluidRow(
        box(width = 12,
            selectInput(inputId = "input_team",
                        label = "Choose Team",
                        choices = unique(stats$team)  )
        )
      ),  
      
      # ROW 2
      
      fluidRow(
        box(width = 12, 
            
            echarts4rOutput("plot3") )

      ),
      
      # ROW 3
      
      fluidRow(
        box(width = 12,
            
            echarts4rOutput("plot4"))
      )
    ),
        
    # ---------------------------- PAGE 3 ---------------------------------
    
    tabItem(tabName = "page3",
            
            fluidRow(
              valueBoxOutput("vboxhth"),
              valueBoxOutput("vboxhth2"),
              valueBoxOutput("vboxhth3"),
            ),
     # ROW 1
       fluidRow(
         box(width = 4,
           selectInput(inputId = "input_team2",
                        label = "Choose Team 1",
                        selected = head(stats[stats$team=="Barcelona",2],n=1),
                        choices = unique(stats$team)  )
           ),
        
         box(width = 4,
             selectInput(inputId = "input_year2",
                         label = "Select Year",
                         selected = NULL,
                         choices = unique(stats$year))
         ) ,
        box(width = 4,
             selectInput(inputId = "input_team21",
                         label = "Choose Team 2",
                         selected = head(stats[stats$team=="Real Madrid",2],n=1),
                         choices = unique(stats$team)  )
             )
        ), 
     
     # ROW 2
            
      fluidRow(
        box(width = 12, 
                  
         echarts4rOutput("plot5") )
        ),
     
     # ROW 3
     
      fluidRow(
        box(width = 6,
            
            echarts4rOutput("plot6")),
        
        box(width = 6,
            
            echarts4rOutput("plot7"))
      )
     
     
    ),
    
    # ---------------------------- PAGE 4 ---------------------------------
    
    tabItem(tabName = "page4",
          
            fluidRow(
              
              valueBoxOutput("vboxchamp"),
              valueBoxOutput("vbox2champ"),
              valueBoxOutput("vbox3champ"),
            ),
            
            # ROW 1
            fluidRow(
              box(width = 6,
                  selectInput(inputId = "input_year3",
                              label = "Select Year",
                              choices = unique(stats$year)  )
              ),
              box(width = 6,
                  sliderInput(inputId= "binsg",
                              label = "Number of rows:",
                              min = 1,
                              max = 20,
                              value = 10,
                              step = 1)),
            ),  
            
            fluidRow(
              box(width = 6 ,
                  echarts4rOutput("plot8")),
              
            box(width = 6 ,
                echarts4rOutput("plot8add"))
            ),
            
            
    )
      
    
    
    
    
    
    
       
      ) # TAB ITEMS
    ) # DASHBOARD BODY
    
  
) # DASHBOARD PAGE
