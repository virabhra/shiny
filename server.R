# Main login screen
loginpage <- div(id = "loginpage", style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
                 wellPanel(
                   tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
                   textInput("userName", placeholder="Username", label = tagList(icon("user"), "Username")),
                   passwordInput("passwd", placeholder="Password", label = tagList(icon("unlock-alt"), "Password")),
                   br(),
                   div(
                     style = "text-align: center;",
                     actionButton("login", "SIGN IN", style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"),
                     shinyjs::hidden(
                       div(id = "nomatch",
                           tags$p("Oops! Incorrect username or password!",
                                  style = "color: red; font-weight: 600; 
                                            padding-top: 5px;font-size:16px;", 
                                  class = "text-center"))),
                     br(),
                     br(),
                     tags$code("Enter your userlevel userid and password")
                     
                   ))
)

playerChoices <- unique(allData$name)
yearChoices <- unique(allData$yearID)
metricChoices <- c("G", "AB", "R", "H", "Doubles", "Triples", "HR", "RBI", "BB", "SO")
teamChoices <- unique(allData$franchName)



credentials = data.frame(
  username_id = c("superuser", "user1"),
  passod   = sapply(c("superuser", "user1"),password_store),
  permission  = c("advanced", "basic"), 
  stringsAsFactors = F
)


server <- function(input, output, session) {
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({ 
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(length(which(credentials$username_id==Username))==1) { 
            pasmatch  <- credentials["passod"][which(credentials$username_id==Username),]
            pasverify <- password_verify(pasmatch, Password)
            if(pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
              shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
            }
          } else {
            shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade")
            shinyjs::delay(3000, shinyjs::toggle(id = "nomatch", anim = TRUE, time = 1, animType = "fade"))
          }
        } 
      }
    }    
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(a(icon("fa fa-sign-out"), "Logout", 
              href="javascript:window.location.reload(true)"),
            class = "dropdown", 
            style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;")
  })
  
  #############################  
  teamData <- reactive({
    
    filteredData <- subset(allData, franchName == input$teamSelect)
    
    final <- data.table(filteredData)[,list(G = sum(G),
                                            AB = sum(AB),
                                            R = sum(R),
                                            H = sum(H),
                                            Doubles = sum(Doubles),
                                            Triples = sum(Triples),
                                            HR = sum(HR),
                                            RBI = sum(RBI),
                                            BB = sum(BB),
                                            SO = sum(SO)),
                                      by = list(franchName, yearID)]
    
    return(final)
    
  })
  
  yearData <- reactive({
    
    filteredData <- subset(allData, yearID == input$year)
    
    final <- filteredData[order(filteredData[input$metricSelect], decreasing = T), ]
    final2 <- final[1:10,]
    
    final2$name <- factor(final2$name,levels = unique(final2$name))
    
    return(final2)
    
  })
  
  playerData <- reactive({
    
    filteredData <- subset(allData, name == input$playerSelect)
    
    return(filteredData)
    
  })
  
  playerCareerData <- reactive({
    
    data <- playerData()
    
    careerData <- data.table(data)[,list(G = sum(G),
                                         AB = sum(AB),
                                         R = sum(R),
                                         H = sum(H),
                                         Doubles = sum(Doubles),
                                         Triples = sum(Triples),
                                         HR = sum(HR),
                                         RBI = sum(RBI),
                                         BB = sum(BB),
                                         SO = sum(SO)),
                                   by = list(name)]
    
    return(careerData)
    
  })
  
  output$careerDataPlayer <- renderDT({
    
    data <- playerCareerData()
    
    datatable(data, options = list(pageLength = nrow(data)))
    
  })
  
  output$yearlyDataPlayer <- renderDT({
    
    data <- playerData()
    
    datatable(data, options = list(pageLength = 6), filter = 'top')
    
  })
  
  output$yearPlotPlayer <- renderPlot({
    
    plot <- ggplot(playerData(), aes_string(x = "yearID", y = input$metricSelect)) 
    plot <- plot + geom_col(fill = "blue") 
    plot <- plot + ggtitle(paste0(input$metricSelect, " Per Year For ", input$playerSelect))
    plot
    
  })
  
  output$teamData <- renderDT({
    
    data <- teamData()
    
    datatable(data, options = list(pageLength = 10), editable = TRUE, filter='top')
    
  })
  
  output$yearLeaderPlot <- renderPlot({
    
    plot <- ggplot(yearData())
    plot <- plot + geom_col(aes_string(x = "name", y = input$metricSelect), fill = "blue")
    plot <- plot + ggtitle(paste0(input$metricSelect, " Leaders For the Year ", input$year))
    plot
    
  })
  
  
  
  #####################################  
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE ){ 
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        sidebarMenu(
          menuItem(text = "Player Data", 
                   startExpanded = TRUE,
                   menuSubItem(text = "Data", tabName = "playerData"),
                   menuSubItem(text = "Plots", tabName = "playerPlot")
          ),
          menuItem(text = "Data Per Team/Year", tabName = "dataPerTeamYear"),
          menuItem(text = "Yearly Leaders", tabName = "yearlyleaders"),
          selectInput(inputId = "playerSelect", label = "Select a player:", choices = playerChoices, selected = "Barry Bonds"),
          selectInput("metricSelect", label = "Select which stat to display:", choices = metricChoices, selected = "HR"),
          selectInput("teamSelect", label = "Select which team to view", choices = teamChoices, selected = "New York Yankees"),
          selectInput("year", label = "Select which year to view", choices = yearChoices, selected = 2015)
        )
      }
      else{
        menuItem(text = "Data Per Team/Year", tabName = "dataPerTeamYear")
        selectInput("teamSelect", label = "Select which team to view", choices = teamChoices, selected = "New York Yankees", multiple = TRUE)
      }
    }
  })
  
  
  output$body <- renderUI({
    if (USER$login == TRUE ) {
      if (credentials[,"permission"][which(credentials$username_id==input$userName)]=="advanced") {
        tabItems(
          tabItem(
            tabName ="playerData", class = "active",
            DTOutput(outputId = "careerDataPlayer"),
            DTOutput(outputId = "yearlyDataPlayer")),
          
          tabItem(
            tabName ="playerPlot", class = "active",
            plotOutput("yearPlotPlayer")
          ),
          
          tabItem(
            tabName = "dataPerTeamYear", class = "active",
            DTOutput("teamData")
          ),
          tabItem(tabName = "yearlyleaders", class = "active",
                  plotOutput("yearLeaderPlot"))
        )
        
      } 
      else {
        tabItem(
          tabName ="dataPerTeamYear", class = "active",
          DTOutput("teamData"))
        
        
      }
      
    }
    else {
      loginpage
    }
  })
  
  
  
  
}