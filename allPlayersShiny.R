source('source_data.R')


ui <- dashboardPage(
    dashboardHeader(title = "NBA Field Goals Made", titleWidth = 350),
    dashboardSidebar(
        sidebarMenu(
            menuItem("2014-15", tabName = "by_zone"),
            menuItem("2015-16", tabName = "by_zone_y2"),
            menuItem("Comparison of Years", tabName = "y1_y2")
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "by_zone",
                    fluidRow(
                        box(title = "Select a Team", solidHeader = TRUE, status = "danger",
                        selectInput("team","",
                                    c('ATL', 'BKN', 'BOS', 'CHA', 'CHI','CLE','DAL', 'DEN', 'DET', 'GSW','HOU', 'IND', 'LAC', 'LAL', 'MEM',	
                                    'MIA', 'MIL', 'MIN', 'NOP', 'NYK', 'OKC', 'ORL', 'PHI','PHX', 'POR', 'SAC', 'SAS', 'TOR','UTA', 'WAS'))
                            ),
                        box(
                            title = "Select a Player", solidHeader = TRUE, status = "danger",
                            selectInput("player","", c())
                            )
                    ),
                    fluidRow(
                        box(
                            title = "Made/Missed Shots", solidHeader = TRUE, width = 12, status = "primary",
                            switchInput(inputId = "zn", label = "By Zone", value = FALSE),
                            plotOutput("court", click = "court_click"), verbatimTextOutput("info")
                        )
                    ),
                    fluidRow(
                        box(
                            title = "Variables", status = "warning", solidHeader = TRUE, width = 4,
                            dateRangeInput("date", "Date", start = "2014-10-28", end = "2015-06-16", min = "2014-10-28", max = "2015-06-16"),
                            selectInput("opp","Opposing Team",
                                        c('All','ATL', 'BKN', 'BOS', 'CHA', 'CHI','CLE','DAL', 'DEN', 'DET', 'HOU', 'IND', 'LAC', 'LAL', 'MEM',	
                                        'MIA', 'MIL', 'MIN', 'NOP', 'NYK', 'OKC', 'ORL', 'PHI','PHX', 'POR', 'SAC', 'SAS', 'TOR','UTA', 'WAS')),
                            selectInput("hoa","Home or Away", c('Both', 'Home', 'Away'))
                            ),
                        box(
                            title = "Zone Averages, Selected Games", solidHeader = TRUE, width = 4, tableOutput("table"), status = "warning"
                            ),
                        box(
                            title = "Zone Averages, Season", solidHeader = TRUE, width = 4, tableOutput("averages"), status = "warning"
                            )
                    )
            ),
            tabItem(
                tabName = "by_zone_y2", 
                fluidRow(
                    box(title = "Select a Team", solidHeader = TRUE, status = "danger",
                        selectInput("team_2","",
                                    c('ATL', 'BKN', 'BOS', 'CHA', 'CHI','CLE','DAL', 'DEN', 'DET', 'GSW','HOU', 'IND', 'LAC', 'LAL', 'MEM',	
                                      'MIA', 'MIL', 'MIN', 'NOP', 'NYK', 'OKC', 'ORL', 'PHI','PHX', 'POR', 'SAC', 'SAS', 'TOR','UTA', 'WAS'))
                    ),
                    box(
                        title = "Select a Player", solidHeader = TRUE, status = "danger",
                        selectInput("player_2","", c())
                    )
                ),
                fluidRow(
                    box(
                        title = "Made/Missed Shots", solidHeader = TRUE, width = 12, status = "primary",
                        switchInput(inputId = "zn_2", label = "By Zone", value = FALSE),
                        plotOutput("court_2", click = "court_click_2"), verbatimTextOutput("info_2")
                    )
                ),
                fluidRow(
                    box(
                        title = "Variables", status = "warning", solidHeader = TRUE, width = 4,
                        dateRangeInput("date_2", "Date", start = "2015-10-20", end = "2016-06-20", min = "2015-10-20", max = "2016-06-20"),
                        selectInput("opp_2","Opposing Team",
                                    c('All','ATL', 'BKN', 'BOS', 'CHA', 'CHI','CLE','DAL', 'DEN', 'DET', 'HOU', 'IND', 'LAC', 'LAL', 'MEM',	
                                      'MIA', 'MIL', 'MIN', 'NOP', 'NYK', 'OKC', 'ORL', 'PHI','PHX', 'POR', 'SAC', 'SAS', 'TOR','UTA', 'WAS')),
                        selectInput("hoa_2","Home or Away", c('Both', 'Home', 'Away'))
                    ),
                    box(
                        title = "Zone Averages, Selected Games", solidHeader = TRUE, width = 4, tableOutput("table_2"), status = "warning"
                    ),
                    box(
                        title = "Zone Averages, Season", solidHeader = TRUE, width = 4, tableOutput("averages_2"), status = "warning"
                    )
                )
            ),
            tabItem(
                tabName = "y1_y2", h2("")
            )
            
        )
    )
)

server <- function(input, output,session) {
    observe({
        x <- input$team
        
        # Can use character(0) to remove all choices
        if (is.null(x)) {
            x <- character(0)
        }
            
        
        temp <- all_data %>%
            filter(team == x)
        temp_players <- unique(as.character(temp$player))
        # Can also set the label and select items
        updateSelectInput(session, "player",
                          choices = temp_players
        )
    })
    
    observe({
        y <- input$team_2
        if (is.null(y)) {
            y <- character(0)
        }
        
        temp_2 <- all_data_2 %>%
            filter(team == y)
        temp_2_players <- unique(as.character(temp_2$player))
        # Can also set the label and select items
        updateSelectInput(session, "player_2",
                          choices = temp_2_players
        )
    })    
    
    output$court <- renderPlot({
    
        
        player_data <- all_data %>%
            filter(player == input$player) 
        
        
        if(input$zn == TRUE) {
            if(input$opp == "All" & input$hoa == "Both") {    
                all_both <- player_data %>%
                    filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw")
                ggplot(all_both, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = zone_type, shape = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
            }
            else if(input$opp != "All" & input$hoa == "Both") {    
                opp_both <-player_data %>%
                    filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & opp_team == input$opp)
                ggplot(opp_both, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = zone_type, shape = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
            }
            else if(input$hoa == "Home" & input$opp == "All") {
                home_all <- player_data %>%
                    filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Home")
                ggplot(home_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = zone_type, shape = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank()) 
            }
            else if(input$hoa == "Home" & input$opp != "All") {
                home_not_all <- player_data %>%
                    filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Home" & opp_team == input$opp)
                ggplot(home_not_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = zone_type, shape = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank()) 
            }           
            else if(input$hoa == "Away" & input$opp == "All") {
                away_all <- player_data %>%
                    filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Away")
                ggplot(away_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = zone_type, shape = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank()) 
            }
            else if(input$hoa == "Away" & input$opp != "All") {
                away_not_all <- player_data %>%
                    filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Away" & opp_team == input$opp)
                ggplot(away_not_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = zone_type, shape = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
            }
        }
        else {
            if(input$opp == "All" & input$hoa == "Both") {    
                all_both <- player_data %>%
                    filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw")
                ggplot(all_both, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
            }
            else if(input$opp != "All" & input$hoa == "Both") {    
                opp_both <-player_data %>%
                    filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & opp_team == input$opp)
                ggplot(opp_both, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
            }
            else if(input$hoa == "Home" & input$opp == "All") {
                home_all <- player_data %>%
                    filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Home")
                ggplot(home_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank()) 
            }
            else if(input$hoa == "Home" & input$opp != "All") {
                home_not_all <- player_data %>%
                    filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Home" & opp_team == input$opp)
                ggplot(home_not_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank()) 
            }           
            else if(input$hoa == "Away" & input$opp == "All") {
                away_all <- player_data %>%
                    filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Away")
                ggplot(away_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank()) 
            }
            else if(input$hoa == "Away" & input$opp != "All") {
                away_not_all <- player_data %>%
                    filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Away" & opp_team == input$opp)
                ggplot(away_not_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
            }
        }
            
    })  
    
    output$court_2 <- renderPlot({
        
        
        player_data <- all_data_2 %>%
            filter(player == input$player_2) 
        
        
        if(input$zn_2 == TRUE) {
            if(input$opp_2 == "All" & input$hoa_2 == "Both") {    
                all_both <- player_data %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw")
                ggplot(all_both, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = zone_type, shape = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
            }
            else if(input$opp_2 != "All" & input$hoa_2 == "Both") {    
                opp_both <-player_data %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & opp_team == input$opp_2)
                ggplot(opp_both, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = zone_type, shape = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
            }
            else if(input$hoa_2 == "Home" & input$opp_2 == "All") {
                home_all <- player_data %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Home")
                ggplot(home_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = zone_type, shape = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank()) 
            }
            else if(input$hoa_2 == "Home" & input$opp_2 != "All") {
                home_not_all <- player_data %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Home" & opp_team == input$opp_2)
                ggplot(home_not_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = zone_type, shape = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank()) 
            }           
            else if(input$hoa_2 == "Away" & input$opp_2 == "All") {
                away_all <- player_data %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Away")
                ggplot(away_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = zone_type, shape = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank()) 
            }
            else if(input$hoa_2 == "Away" & input$opp_2 != "All") {
                away_not_all <- player_data %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Away" & opp_team == input$opp_2)
                ggplot(away_not_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = zone_type, shape = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
            }
        }
        else {
            if(input$opp_2 == "All" & input$hoa_2 == "Both") {    
                all_both <- player_data %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw")
                ggplot(all_both, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
            }
            else if(input$opp_2 != "All" & input$hoa_2 == "Both") {    
                opp_both <-player_data %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & opp_team == input$opp_2)
                ggplot(opp_both, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
            }
            else if(input$hoa_2 == "Home" & input$opp_2 == "All") {
                home_all <- player_data %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Home")
                ggplot(home_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank()) 
            }
            else if(input$hoa_2 == "Home" & input$opp_2 != "All") {
                home_not_all <- player_data %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Home" & opp_team == input$opp_2)
                ggplot(home_not_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank()) 
            }           
            else if(input$hoa_2 == "Away" & input$opp_2 == "All") {
                away_all <- player_data %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Away")
                ggplot(away_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank()) 
            }
            else if(input$hoa_2 == "Away" & input$opp_2 != "All") {
                away_not_all <- player_data %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Away" & opp_team == input$opp_2)
                ggplot(away_not_all, aes(x=converted_x, y=converted_y)) + 
                    annotation_custom(court, 0, 50, 0, 47) +
                    geom_point(aes(color = result), alpha = 0.5, size = 3) +
                    xlim(0, 50) +
                    ylim(0, 47) +
                    coord_fixed() +
                    theme(axis.line=element_blank(),axis.text.x=element_blank(),
                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                          axis.title.x=element_blank(),
                          axis.title.y=element_blank(),
                          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),plot.background=element_blank())
            }
        }
        
    })
    
    output$info <- renderText({
        player_data <- all_data %>%
            filter(player == input$player)
        if (is.null(input$court_click))
            return(NULL)
        if(input$opp == "All" & input$hoa == "Both") {
            temp <- player_data %>%
                filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw") %>%
                filter(converted_x >= input$court_click$x-.5 & converted_x <= input$court_click$x+.5) %>%
                filter(converted_y >= input$court_click$y-.5 & converted_y <= input$court_click$y+.5)
            paste("Date:", temp$date, "\nDescription:", temp$description, "\n")
        }
        else if(input$opp != "All" & input$hoa == "Both") { 
            temp <- player_data %>%
                filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & opp_team == input$opp) %>%
                filter(converted_x >= input$court_click$x-.5 & converted_x <= input$court_click$x+.5) %>%
                filter(converted_y >= input$court_click$y-.5 & converted_y <= input$court_click$y+.5)
            paste("Date:", temp$date, "\nDescription:", temp$description, "\n")
        }
        else if(input$hoa == "Home" & input$opp == "All") {
            temp <- player_data %>%
                filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Home") %>%
                filter(converted_x >= input$court_click$x-.5 & converted_x <= input$court_click$x+.5) %>%
                filter(converted_y >= input$court_click$y-.5 & converted_y <= input$court_click$y+.5)
            paste("Date:", temp$date, "\nDescription:", temp$description, "\n")
        }
        else if(input$hoa == "Home" & input$opp != "All") {
            temp <- player_data %>%
                filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Home" & opp_team == input$opp) %>%
                filter(converted_x >= input$court_click$x-.5 & converted_x <= input$court_click$x+.5) %>%
                filter(converted_y >= input$court_click$y-.5 & converted_y <= input$court_click$y+.5)
            paste("Date:", temp$date, "\nDescription:", temp$description, "\n")
        }
        else if(input$hoa == "Away" & input$opp == "All") {
            temp <- player_data %>%
                filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Away") %>%
                filter(converted_x >= input$court_click$x-.5 & converted_x <= input$court_click$x+.5) %>%
                filter(converted_y >= input$court_click$y-.5 & converted_y <= input$court_click$y+.5)
            paste("Date:", temp$date, "\nDescription:", temp$description, "\n")
        }
        else if(input$hoa == "Away" & input$opp != "All") {
            temp <- player_data %>%
                filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Away" & opp_team == input$opp) %>%
                filter(converted_x >= input$court_click$x-.5 & converted_x <= input$court_click$x+.5) %>%
                filter(converted_y >= input$court_click$y-.5 & converted_y <= input$court_click$y+.5)
            paste("Date:", temp$date, "\nDescription:", temp$description, "\n")
        }
        
    })
    
    output$info_2 <- renderText({
        player_data <- all_data_2 %>%
            filter(player == input$player_2)
        if (is.null(input$court_click_2))
            return(NULL)
        if(input$opp_2 == "All" & input$hoa_2 == "Both") {
            temp <- player_data %>%
                filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw") %>%
                filter(converted_x >= input$court_click_2$x-.5 & converted_x <= input$court_click_2$x+.5) %>%
                filter(converted_y >= input$court_click_2$y-.5 & converted_y <= input$court_click_2$y+.5)
            paste("Date:", temp$date, "\nDescription:", temp$description, "\n")
        }
        else if(input$opp_2 != "All" & input$hoa_2 == "Both") { 
            temp <- player_data %>%
                filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & opp_team == input$opp_2) %>%
                filter(converted_x >= input$court_click_2$x-.5 & converted_x <= input$court_click_2$x+.5) %>%
                filter(converted_y >= input$court_click_2$y-.5 & converted_y <= input$court_click_2$y+.5)
            paste("Date:", temp$date, "\nDescription:", temp$description, "\n")
        }
        else if(input$hoa_2 == "Home" & input$opp_2 == "All") {
            temp <- player_data %>%
                filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Home") %>%
                filter(converted_x >= input$court_click_2$x-.5 & converted_x <= input$court_click_2$x+.5) %>%
                filter(converted_y >= input$court_click_2$y-.5 & converted_y <= input$court_click_2$y+.5)
            paste("Date:", temp$date, "\nDescription:", temp$description, "\n")
        }
        else if(input$hoa_2 == "Home" & input$opp_2 != "All") {
            temp <- player_data %>%
                filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Home" & opp_team == input$opp_2) %>%
                filter(converted_x >= input$court_click_2$x-.5 & converted_x <= input$court_click_2$x+.5) %>%
                filter(converted_y >= input$court_click_2$y-.5 & converted_y <= input$court_click_2$y+.5)
            paste("Date:", temp$date, "\nDescription:", temp$description, "\n")
        }
        else if(input$hoa_2 == "Away" & input$opp_2 == "All") {
            temp <- player_data %>%
                filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Away") %>%
                filter(converted_x >= input$court_click_2$x-.5 & converted_x <= input$court_click_2$x+.5) %>%
                filter(converted_y >= input$court_click_2$y-.5 & converted_y <= input$court_click_2$y+.5)
            paste("Date:", temp$date, "\nDescription:", temp$description, "\n")
        }
        else if(input$hoa_2 == "Away" & input$opp_2 != "All") {
            temp <- player_data %>%
                filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Away" & opp_team == input$opp_2) %>%
                filter(converted_x >= input$court_click_2$x-.5 & converted_x <= input$court_click_2$x+.5) %>%
                filter(converted_y >= input$court_click_2$y-.5 & converted_y <= input$court_click_2$y+.5)
            paste("Date:", temp$date, "\nDescription:", temp$description, "\n")
        }
        
    })
    
    output$table <- renderTable({
        data <- 
            if(input$zn == TRUE) {
                if(input$opp == "All" & input$hoa == "Both") {
                    all_data %>%
                        filter(player == input$player) %>%
                        filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw") %>%
                        group_by(zone_type) %>%
                        summarize(zone_PCT = sum(shot_made)/n()*100)
                }
                else if(input$opp != "All" & input$hoa == "Both") {
                    all_data %>%
                        filter(player == input$player) %>%
                        filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & opp_team == input$opp) %>%
                        group_by(zone_type) %>%
                        summarize(zone_PCT = sum(shot_made)/n()*100)
                }
                else if(input$hoa == "Home" & input$opp == "All") {
                    all_data %>%
                        filter(player == input$player) %>%
                        filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Home") %>%
                        group_by(zone_type) %>%
                        summarize(zone_PCT = sum(shot_made)/n()*100)
                }
                else if(input$hoa == "Home" & input$opp != "All") {
                    all_data %>%
                        filter(player == input$player) %>%
                        filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Home" & opp_team == input$opp) %>%
                        group_by(zone_type) %>%
                        summarize(zone_PCT = sum(shot_made)/n()*100)
                }
                else if(input$hoa == "Away" & input$opp == "All") {
                    all_data %>%
                        filter(player == input$player) %>%
                        filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Away") %>%
                        group_by(zone_type) %>%
                        summarize(zone_PCT = sum(shot_made)/n()*100)
                }
                else if(input$hoa == "Away" & input$opp != "All") {
                    all_data %>%
                        filter(player == input$player) %>%
                        filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Away" & opp_team == input$opp) %>%
                        group_by(zone_type) %>%
                        summarize(zone_PCT = sum(shot_made)/n()*100)
                }
            }
            else {
                if(input$opp == "All" & input$hoa == "Both") {
                    all_data %>%
                        filter(player == input$player) %>%
                        filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw") %>%
                        group_by(two_or_three) %>%
                        summarize(PCT = sum(shot_made)/n()*100)
                }
                else if(input$opp != "All" & input$hoa == "Both") {
                    all_data %>%
                        filter(player == input$player) %>%
                        filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & opp_team == input$opp) %>%
                        group_by(two_or_three) %>%
                        summarize(PCT = sum(shot_made)/n()*100)
                }
                else if(input$hoa == "Home" & input$opp == "All") {
                    all_data %>%
                        filter(player == input$player) %>%
                        filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Home") %>%
                        group_by(two_or_three) %>%
                        summarize(PCT = sum(shot_made)/n()*100)
                }
                else if(input$hoa == "Home" & input$opp != "All") {
                    all_data %>%
                        filter(player == input$player) %>%
                        filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Home" & opp_team == input$opp) %>%
                        group_by(two_or_three) %>%
                        summarize(PCT = sum(shot_made)/n()*100)
                }
                else if(input$hoa == "Away" & input$opp == "All") {
                    all_data %>%
                        filter(player == input$player) %>%
                        filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Away") %>%
                        group_by(two_or_three) %>%
                        summarize(PCT = sum(shot_made)/n()*100)
                }
                else if(input$hoa == "Away" & input$opp != "All") {
                    all_data %>%
                        filter(player == input$player) %>%
                        filter(date >= input$date[1] & date <= input$date[2] & event_type != "free throw" & h_or_a == "Away" & opp_team == input$opp) %>%
                        group_by(two_or_three) %>%
                        summarize(PCT = sum(shot_made)/n()*100)
                }
            }
            
        data
    })
    output$table_2 <- renderTable({
        data <- 
            if(input$zn_2 == TRUE) {
                if(input$opp_2 == "All" & input$hoa_2 == "Both") {
                    all_data_2 %>%
                        filter(player == input$player_2) %>%
                        filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw") %>%
                        group_by(zone_type) %>%
                        summarize(zone_PCT = sum(shot_made)/n()*100)
                }
                else if(input$opp_2 != "All" & input$hoa_2 == "Both") {
                    all_data_2 %>%
                        filter(player == input$player_2) %>%
                        filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & opp_team == input$opp_2) %>%
                        group_by(zone_type) %>%
                        summarize(zone_PCT = sum(shot_made)/n()*100)
                }
                else if(input$hoa_2 == "Home" & input$opp_2 == "All") {
                    all_data_2 %>%
                        filter(player == input$player_2) %>%
                        filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Home") %>%
                        group_by(zone_type) %>%
                        summarize(zone_PCT = sum(shot_made)/n()*100)
                }
                else if(input$hoa_2 == "Home" & input$opp_2 != "All") {
                    all_data_2 %>%
                        filter(player == input$player_2) %>%
                        filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Home" & opp_team == input$opp_2) %>%
                        group_by(zone_type) %>%
                        summarize(zone_PCT = sum(shot_made)/n()*100)
                }
                else if(input$hoa_2 == "Away" & input$opp_2 == "All") {
                    all_data_2 %>%
                        filter(player == input$player_2) %>%
                        filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Away") %>%
                        group_by(zone_type) %>%
                        summarize(zone_PCT = sum(shot_made)/n()*100)
                }
                else if(input$hoa_2 == "Away" & input$opp_2 != "All") {
                    all_data_2 %>%
                        filter(player == input$player_2) %>%
                        filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Away" & opp_team == input$opp_2) %>%
                        group_by(zone_type) %>%
                        summarize(zone_PCT = sum(shot_made)/n()*100)
                }
            }
        else {
            if(input$opp_2 == "All" & input$hoa_2 == "Both") {
                all_data_2 %>%
                    filter(player == input$player_2) %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw") %>%
                    group_by(two_or_three) %>%
                    summarize(PCT = sum(shot_made)/n()*100)
            }
            else if(input$opp_2 != "All" & input$hoa_2 == "Both") {
                all_data_2 %>%
                    filter(player == input$player_2) %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & opp_team == input$opp_2) %>%
                    group_by(two_or_three) %>%
                    summarize(PCT = sum(shot_made)/n()*100)
            }
            else if(input$hoa_2 == "Home" & input$opp_2 == "All") {
                all_data_2 %>%
                    filter(player == input$player_2) %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Home") %>%
                    group_by(two_or_three) %>%
                    summarize(PCT = sum(shot_made)/n()*100)
            }
            else if(input$hoa_2 == "Home" & input$opp_2 != "All") {
                all_data_2 %>%
                    filter(player == input$player_2) %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Home" & opp_team == input$opp_2) %>%
                    group_by(two_or_three) %>%
                    summarize(PCT = sum(shot_made)/n()*100)
            }
            else if(input$hoa_2 == "Away" & input$opp_2 == "All") {
                all_data_2 %>%
                    filter(player == input$player_2) %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Away") %>%
                    group_by(two_or_three) %>%
                    summarize(PCT = sum(shot_made)/n()*100)
            }
            else if(input$hoa_2 == "Away" & input$opp_2 != "All") {
                all_data_2 %>%
                    filter(player == input$player_2) %>%
                    filter(date >= input$date_2[1] & date <= input$date_2[2] & event_type != "free throw" & h_or_a == "Away" & opp_team == input$opp_2) %>%
                    group_by(two_or_three) %>%
                    summarize(PCT = sum(shot_made)/n()*100)
            }
        }
        
        data
    })
    output$averages <- renderTable({
        if(input$zn == TRUE) {
            all_data %>%
                filter(player == input$player) %>%
                group_by(zone_type) %>%
                summarize(zone_PCT = sum(shot_made)/n()*100)    
        }
        else {
            all_data %>%
                filter(player == input$player) %>%
                group_by(two_or_three) %>%
                summarize(zone_PCT = sum(shot_made)/n()*100)  
        }
        
    })
    
    output$averages_2 <- renderTable({
        if(input$zn_2 == TRUE) {
            all_data_2 %>%
                filter(player == input$player_2) %>%
                group_by(zone_type) %>%
                summarize(zone_PCT = sum(shot_made)/n()*100)    
        }
        else {
            all_data_2 %>%
                filter(player == input$player_2) %>%
                group_by(two_or_three) %>%
                summarize(zone_PCT = sum(shot_made)/n()*100)  
        }
        
    })
        
}

shinyApp(ui = ui, server = server)
