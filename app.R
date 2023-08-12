#Load packages ---------------------------------------------------

library(shiny)
library(tidyverse)
library(shinythemes)
library(gridExtra)
library(scales)
library(DT)


#Load data -------------------------------------------------------

TrackMan_Data <- data.frame()

csv_files <- list.files(pattern = "\\.csv$")

for (file in csv_files) {
  temp_data <- read.csv(file, stringsAsFactors = FALSE)
  TrackMan_Data <- bind_rows(TrackMan_Data, temp_data)
}

Pitchers_Gulls <- filter(TrackMan_Data, PitcherTeam == "NEW_GUL")

Hitters_Gulls <- filter(TrackMan_Data, BatterTeam == "NEW_GUL")

Catchers_Gulls <- filter(TrackMan_Data, CatcherTeam == "NEW_GUL")




#Special Graphics & Functions -----------------------------------------------------

topKzone = 3.5
botKzone = 1.6
inKzone = -.71
outKzone = 0.71
kZone = data.frame(
    x = c(inKzone, inKzone, outKzone, outKzone, inKzone)
    , y = c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

P_Topplate = geom_segment(aes(x=-.71, y=-.2, xend=.71, yend=-.2))
P_Lplateside = geom_segment(aes(x=-.71, y=-.2, xend=-.71, yend=0))
P_Rplateside =  geom_segment(aes(x=.71, y=-.2, xend=.71, yend=0))
P_Lplateback =  geom_segment(aes(x=-.71, y=0, xend=0, yend=.3))
P_Rplateback = geom_segment(aes(x=.71, y=0, xend=0, yend=.3))

H_Topplate = geom_segment(aes(x=-.71, y=.3, xend=.71, yend=.3))
H_Lplateside = geom_segment(aes(x=-.71, y=.3, xend=-.71, yend=.1))
H_Rplateside =  geom_segment(aes(x=.71, y=.3, xend=.71, yend=.1))
H_Lplateback =  geom_segment(aes(x=-.71, y=.1, xend=0, yend=-.2))
H_Rplateback = geom_segment(aes(x=.71, y=.1, xend=0, yend=-.2))



# User Interface ---------------------------------------------------

ui <- navbarPage(
    
    theme = shinytheme("cosmo"),
    "Newport Gulls",
   
    
    
    

    # Pitchers Tab-------------------------------------------
    
    tabPanel("Pitchers",
             sidebarPanel(  
                 selectInput("P_Pitcher_select", label = "Pitcher", choices = sort(unique(Pitchers_Gulls$Pitcher))),
                 
                 radioButtons("P_BatterHandness_select", label = "Batter Handness", choices = list("Both" = "", "LHH" = "Right", "RHH" = "Left")),
                 #LHH and RHH = opposite terms because inequality operator (!=) is being used because I can't get both to work without it
                 
                 checkboxInput("P_All_select", "Select All Games", value = FALSE),
                 
                 selectInput("P_Date_select", label = "Game Date", choices = unique(Pitchers_Gulls$Date), multiple = TRUE),
                 
                 selectInput("P_Opp_select", label = "Opponent", choices = sort(unique(Pitchers_Gulls$BatterTeam)), multiple = TRUE),
                 
                 width = 2,
                 
             ),
             
             mainPanel(   
                 
                 dataTableOutput("Pitcher_Totals"),
                 
                 fluidRow(
                     plotOutput("Pitcher_1", height = 600, width = 1200)
                 ),
                 
                 fluidRow(
                     plotOutput("Pitcher_2", height = 400, width = 1200)
                 )
                 
             ),
                 dataTableOutput("Pitcher_Table")
                 
                 
                 
                 
             
    ),
    
    
    
    
    
    
    # Hitters Tab -------------------------------------------------
    
    tabPanel("Hitters",
           sidebarPanel(  
             selectInput("H_Hitter_select", label = "Batter", choices = sort(unique(Hitters_Gulls$Batter))),
             
             radioButtons("H_PitcherHandness_select", label = "Pitcher Handness", choices = list("Both" = "", "LHP" = "Right", "RHP" = "Left")),
             #LHP and RHP = opposite terms because inequality operator (!=) is being used because I can't get both to work without it
             
             checkboxInput("H_All_select", "Select All Games", value = FALSE),
             
             selectInput("H_Date_select", label = "Game Date", choices = unique(Hitters_Gulls$Date), multiple = TRUE),
             
             selectInput("H_Opp_select", label = "Opponent", choices = sort(unique(Hitters_Gulls$PitcherTeam)), multiple = TRUE),
             
             width = 2
         
           ),
           
           mainPanel(   
               
               fluidRow(
                 dataTableOutput("Hitter_Table1"),
                   plotOutput("Hitter_1", height = 600, width = 1200)
               ),
        
           ),
               dataTableOutput("Hitter_Table2")
    
           ),
    
    
    
    
    
    #Catchers Tab ---------------------------------------------------
    
    tabPanel("Catchers",
             sidebarPanel(  
                 selectInput("C_Catcher_select", label = "Catcher", choices = sort(unique(Catchers_Gulls$Catcher))),
                 
                 
                 radioButtons("C_PitcherHandness_select", label = "Pitcher Handness", choices = list("Both" = "", "LHP" = "Right", "RHP" = "Left")),
                 #LHP and RHP = opposite terms because inequality operator (!=) is being used because I can't get both to work without it
                 
                 checkboxInput("C_All_select", "Select All Games", value = FALSE),
                 
                 selectInput("C_Date_select", label = "Game Date", choices = unique(Pitchers_Gulls$Date), multiple = TRUE),
                 
                 selectInput("C_Opp_select", label = "Opponent", choices = sort(unique(Pitchers_Gulls$BatterTeam)), multiple = TRUE),
                 
                 width = 2,
                 
             ),
             
             mainPanel(   
                 
                 fluidRow(
                     plotOutput("Catcher_1", height = 600, width = 1200)
                 ),
                 
                
                 
                 
                 
                 
             )
    ),
    
    
    
    
    #Pitching Leaderboard --------------------------------------
    
    tabPanel("Pitching Leaderboard",
            mainPanel(
        
                tabsetPanel(
                    tabPanel("Fastball",
                            
                    dataTableOutput("Fastball_Leaderboard")
                    
                            ),
                    
                    tabPanel("Breaking Balls",
                        
                             dataTableOutput("BreakingBall_Leaderboard")
                        
                    ),
                    
                    tabPanel("Offspeed",
                             
                             dataTableOutput("OffSpeed_Leaderboard")
                             
                    )
                 )
                )
            ),
    
    
    
    
    
    
    #Hitting Leaderboard -------------------------------------------
    
    tabPanel("Hitting Leaderboard",
             
             mainPanel(
             
             dataTableOutput("Hitting_Leaderboard")
             
             )
             ),
    
    
)






# Server logic ----------------------------------------------------

server <- function(input, output, session) {
    
    
    #Pitchers Tab -------------------------------------------------
  
    observe({
        
        updateSelectInput(
            session, "P_Date_select", choices = unique(Pitchers_Gulls$Date),
            selected = if(input$P_All_select) unique(Pitchers_Gulls$Date)
        )
    })
    
    output$Pitcher_Totals <- renderDataTable({
        
        Pitchers_Gulls %>%
            filter(Pitcher == input$P_Pitcher_select, BatterSide != input$P_BatterHandness_select, Date %in% c(input$P_Date_select) | BatterTeam %in% c(input$P_Opp_select)) %>%
            group_by(TaggedPitchType) %>%
            dplyr::summarise(NumPitches = n(),
                             MaxVelo = round(max(RelSpeed, na.rm = TRUE), digits = 1), 
                             AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1), 
                             MaxSpin = round(max(SpinRate, na.rm = TRUE), 0), 
                             AvgSpin = round(mean(SpinRate, na.rm = TRUE), 0), 
                             Extension = round(mean(Extension, na.rm = TRUE), 2), 
                             VertBreak = round(mean(VertBreak, na.rm = TRUE), 2), 
                             IVB = round(mean(InducedVertBreak, na.rm = TRUE), 2), 
                             HBreak = round(mean(HorzBreak, na.rm = TRUE), 2),
                             VAA = round(mean(VertApprAngle, na.rm = TRUE), 2),
                             SpinAxis = round(mean(SpinAxis, na.rm =TRUE), 0)) %>%
            mutate(Usage = percent(prop.table(NumPitches))) %>%
            relocate(Usage, .after = NumPitches) %>%
        rename(Pitch = TaggedPitchType) %>%
            datatable(options = list(paging = FALSE, searching = FALSE))
        
    })
    
    
    output$Pitcher_1 <- renderPlot({
        
        grid.arrange(Pitchers_Gulls %>%
                         filter(Pitcher == input$P_Pitcher_select, BatterSide != input$P_BatterHandness_select, Date %in% c(input$P_Date_select) | BatterTeam %in% c(input$P_Opp_select)) %>%
                         ggplot() + 
                            geom_point(aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType), size = 8, alpha = 0.95) + 
                            geom_path(aes(x,y), data = kZone) + 
                            theme_void() + 
                            coord_cartesian(xlim = c(-3,3), ylim = c(-0.5,5))  + 
                            theme(legend.title=element_blank(), legend.position = "bottom", plot.title = element_text( size=25, face="bold", hjust = 0.5), legend.text = element_text(size = 15)) + 
                            guides(col = guide_legend(nrow = 2))+
                            ggtitle("Pitch Type") + 
                            scale_color_manual(values = c(Fastball = "dodgerblue", Slider = "green", ChangeUp = "hotpink", Curveball = "red", Sinker = "orange", Cutter = "chocolate4", Splitter = "purple", Undefined = "darkgrey")) + 
                            geom_text(aes(x = PlateLocSide, y = PlateLocHeight, label = seq_along(PlateLocSide)), size = 4) +
                            P_Topplate + P_Lplateside + P_Rplateback + P_Lplateback + P_Rplateside, 
                     
                     Pitchers_Gulls %>%
                         filter(Pitcher == input$P_Pitcher_select, BatterSide != input$P_BatterHandness_select, Date %in% c(input$P_Date_select) | BatterTeam %in% c(input$P_Opp_select)) %>%
                         ggplot() + 
                            geom_point(aes(x = PlateLocSide, y = PlateLocHeight, color = PitchCall), size = 8, alpha = 0.95) + 
                            geom_path(aes(x,y), data = kZone) + 
                            theme_void() + 
                            coord_cartesian(xlim = c(-3,3), ylim = c(-0.5,5))  + 
                            theme(legend.title=element_blank(), legend.position = "bottom", plot.title = element_text( size=25, face="bold", hjust = 0.5), legend.text = element_text(size = 15)) + 
                            guides(col = guide_legend(nrow = 2))+
                            ggtitle("Pitch Result") + 
                         scale_color_manual(values = c(BallCalled = "dodgerblue", BallinDirt = "chocolate4", FoulBall = "green", InPlay = "red", StrikeCalled = "yellow", StrikeSwinging = "orange", HitByPitch = "purple")) + 
                           geom_text(aes(x = PlateLocSide, y = PlateLocHeight, label = seq_along(PlateLocSide)), size = 4) +
                            P_Topplate + P_Lplateside + P_Rplateback + P_Lplateback + P_Rplateside, 
                     
                     ncol = 2)
        
    })
    
    output$Pitcher_2 <-renderPlot({
        
        grid.arrange(
                Pitchers_Gulls %>%
                    filter(Pitcher == input$P_Pitcher_select, BatterSide != input$P_BatterHandness_select, Date %in% c(input$P_Date_select) | BatterTeam %in% c(input$P_Opp_select)) %>%
                            ggplot() + 
                            geom_point(aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType), size = 5, alpha = 0.75) + 
                            theme_bw() +
                            coord_cartesian(xlim = c(-25, 25), ylim = c(-20,30)) + 
                            theme(legend.title=element_blank(), legend.position = "bottom", plot.title = element_text( size=25, face="bold", hjust = 0.5), legend.text = element_text(size = 15)) + 
                            guides(col = guide_legend(nrow = 2))+
                            ggtitle("Break") + 
                            scale_color_manual(values = c(Fastball = "dodgerblue", Slider = "green", ChangeUp = "hotpink", Curveball = "red", Sinker = "orange", Cutter = "chocolate4", Splitter = "purple", Undefined = "darkgrey")) +
                            geom_hline(yintercept = 0, color = "black") + 
                            geom_vline(xintercept = 0, color = "black"),
                
                Pitchers_Gulls %>%
                    filter(Pitcher == input$P_Pitcher_select, BatterSide != input$P_BatterHandness_select, Date %in% c(input$P_Date_select) | BatterTeam %in% c(input$P_Opp_select)) %>%
                    ggplot() + 
                    geom_point(aes(x = RelSpeed, y = SpinRate, color = TaggedPitchType), size = 5, alpha = 0.75) + 
                    theme_bw() +
                    coord_cartesian(xlim = c(60,100), ylim = c(500,3000)) + 
                    theme(legend.title=element_blank(), legend.position = "bottom", plot.title = element_text( size=25, face="bold", hjust = 0.5), legend.text = element_text(size = 15))+ 
                    guides(col = guide_legend(nrow = 2))+  
                    ggtitle("Velo vs Spin Rate") + 
                    scale_color_manual(values = c(Fastball = "dodgerblue", Slider = "green", ChangeUp = "hotpink", Curveball = "red", Sinker = "orange", Cutter = "chocolate4", Splitter = "purple", Undefined = "darkgrey")), 
                
                Pitchers_Gulls %>%
                    filter(Pitcher == input$P_Pitcher_select, BatterSide != input$P_BatterHandness_select, Date %in% c(input$P_Date_select) | BatterTeam %in% c(input$P_Opp_select)) %>%
                    ggplot() + 
                    geom_point(aes(x = RelSide, y = RelHeight, color = TaggedPitchType), size = 5, alpha = 0.5) + 
                    theme_bw() +
                    coord_cartesian(xlim = c(-3.5,3.5), ylim = c(0,7.5)) + 
                    theme(legend.title=element_blank(), legend.position = "bottom", plot.title = element_text( size=25, face="bold", hjust = 0.5), legend.text = element_text(size = 15))+ 
                    guides(col = guide_legend(nrow = 2))+
                    ggtitle("Release Position") + 
                    scale_color_manual(values = c(Fastball = "dodgerblue", Slider = "green", ChangeUp = "hotpink", Curveball = "red", Sinker = "orange", Cutter = "chocolate4", Splitter = "purple", Undefined = "darkgrey")),  
                Pitchers_Gulls %>%
                    filter(Pitcher == input$P_Pitcher_select, BatterSide != input$P_BatterHandness_select, Date %in% c(input$P_Date_select) | BatterTeam %in% c(input$P_Opp_select)) %>%
                    ggplot() + 
                    geom_histogram(aes(x = SpinAxis, fill = TaggedPitchType, after_stat(sqrt(count))), binwidth = 15, color = "black") + 
                    coord_polar(start = pi) + 
                    scale_x_continuous(breaks = c(0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300, 330), labels = c('6:00', '7:00', '8:00', '9:00', '10:00', '11:00', '12:00', '1:00', '2:00', '3:00', '4:00', '5:00'), limits = c(0,360)) + 
                    theme_minimal() + 
                    theme(legend.title=element_blank(), legend.position = "bottom", plot.title = element_text( size=25, face="bold", hjust = 0.5), legend.text = element_text(size = 15)) +
                    guides(fill = guide_legend(nrow = 2))+
                    scale_fill_manual(values = c(Fastball = "dodgerblue", Slider = "green", ChangeUp = "hotpink", Curveball = "red", Sinker = "orange", Cutter = "chocolate4", Splitter = "purple", Undefined = "darkgrey")) +
                    ggtitle("Tilt") + 
                    theme(axis.title = element_blank(), axis.text.y = element_blank()),
                
                ncol = 4)
    })
    

    
    
    
    output$Pitcher_Table <- renderDataTable({
        
        Pitchers_Gulls %>%
            filter(Pitcher == input$P_Pitcher_select, BatterSide != input$P_BatterHandness_select, Date %in% c(input$P_Date_select) | BatterTeam %in% c(input$P_Opp_select)) %>%
            select(c("Date", "BatterTeam", "Batter", "BatterSide", "Balls", "Strikes", "PitchCall", "TaggedPitchType", "RelSpeed", "SpinRate", "Tilt", "Extension", "InducedVertBreak", "HorzBreak", "VertApprAngle", "ExitSpeed", "AutoHitType", "PlayResult")) %>%
        mutate(RelSpeed = round(RelSpeed, 1), SpinRate = round(SpinRate, 0), Extension = round(Extension, 2), InducedVertBreak = round(InducedVertBreak, 2), HorzBreak = round(HorzBreak, 2), ExitSpeed = round(ExitSpeed, 1), VertApprAngle = round(VertApprAngle, 2)) %>%
        rename(IVB = InducedVertBreak, Pitch = TaggedPitchType, HitType = AutoHitType, Side = BatterSide, Speed = RelSpeed, Opp = BatterTeam, Side = BatterSide, VAA = VertApprAngle, HBreak = HorzBreak)
    })
    
    
    

    
    
    
    
    
    #Hitters Tab ------------------------------------------------------
    
    observe({
        
        updateSelectInput(
            session, "H_Date_select", choices = unique(Hitters_Gulls$Date),
            selected = if(input$H_All_select) unique(Hitters_Gulls$Date)
        )
    })
    
    output$Hitter_Table1 <- renderDataTable({
      
      Hitters_Gulls %>%
        filter(PitchCall == "InPlay", Batter == input$H_Hitter_select, PitcherThrows != input$H_PitcherHandness_select, Date %in% c(input$H_Date_select) | PitcherTeam %in% c(input$H_Opp_select)) %>%
        dplyr::summarise(BallsInPlay = n(),
                         MaxExitVelo = round(max(ExitSpeed, na.rm = TRUE), digits = 1), 
                         AvgExitVelo = round(mean(ExitSpeed, na.rm = TRUE), 1),
                         HardHit = sum(ExitSpeed >= 94.99, na.rm = TRUE),
                         BestSpeed = round(mean(sort(ExitSpeed, decreasing = TRUE)[1:ceiling(length(ExitSpeed) / 2)]), 1),
                         MaxDistance = round(max(Distance, na.rm = TRUE), 0), 
                         AvgDistance = round(mean(Distance, na.rm = TRUE), 0), 
                         AvgLA = round(mean(Angle, na.rm = TRUE), 1),
                         SweetSpot = sum(Angle >= 8 & Angle <= 32, na.rm = TRUE)) %>%
        mutate(SwSpPct = round((SweetSpot / BallsInPlay) * 100, 2)) %>%
        mutate(HardHitRate = round((HardHit / BallsInPlay) * 100, 2)) %>%
        relocate(HardHitRate, .after = HardHit) %>%
        datatable(options = list(paging = FALSE, searching = FALSE))
      
    })
    

    output$Hitter_1 <- renderPlot({
        
        grid.arrange(Hitters_Gulls %>%
                         filter(Batter == input$H_Hitter_select, PitcherThrows != input$H_PitcherHandness_select, Date %in% c(input$H_Date_select) | PitcherTeam %in% c(input$H_Opp_select)) %>%
                         ggplot() +
                        geom_point(aes(x = -PlateLocSide, y = PlateLocHeight, color = AutoPitchType), size = 8, alpha = 0.95) +
                        geom_path(aes(x,y), data = kZone) +
                        theme_void() +
                        coord_cartesian(xlim = c(-3,3), ylim = c(-0.5,5))  +
                        theme(legend.title=element_blank(), legend.position = "bottom", plot.title = element_text( size=25, face="bold", hjust = 0.5), legend.text = element_text(size = 15)) +
                       guides(col = guide_legend(nrow = 2)) +
                        ggtitle("Pitch Type") +
                        scale_color_manual(values = c(Fastball = "dodgerblue", Slider = "green", ChangeUp = "hotpink", Curveball = "red", Sinker = "orange", Cutter = "chocolate4", Splitter = "purple", Undefined = "darkgrey")) +
                       geom_text(aes(x = -PlateLocSide, y = PlateLocHeight, label = seq_along(-PlateLocSide)), size = 4) +
                        H_Topplate + H_Lplateside + H_Rplateback + H_Lplateback + H_Rplateside, 
                     
                    Hitters_Gulls %>%
                         filter(Batter == input$H_Hitter_select, PitcherThrows != input$H_PitcherHandness_select, Date %in% c(input$H_Date_select) | PitcherTeam %in% c(input$H_Opp_select)) %>%
                        ggplot() +
                        geom_point(aes(x = -PlateLocSide, y = PlateLocHeight, color = PitchCall), size = 8, alpha = 0.95) +
                        geom_path(aes(x,y), data = kZone) +
                        theme_void() +
                        coord_cartesian(xlim = c(-3,3), ylim = c(-0.5,5))  +
                        theme(legend.title=element_blank(), legend.position = "bottom", plot.title = element_text( size=25, face="bold", hjust = 0.5), legend.text = element_text(size = 15)) + 
                      guides(col = guide_legend(nrow = 2)) +
                        ggtitle("Pitch Result") +
                        scale_color_manual(values = c(BallCalled = "dodgerblue", BallinDirt = "chocolate4", FoulBall = "green", InPlay = "red", StrikeCalled = "yellow", StrikeSwinging = "orange", HitByPitch = "purple")) +
                      geom_text(aes(x = -PlateLocSide, y = PlateLocHeight, label = seq_along(-PlateLocSide)), size = 4) +
                        H_Topplate + H_Lplateside + H_Rplateback + H_Lplateback + H_Rplateside, 
                     
                     ncol = 2)
        
    })
    
    
    output$Hitter_Table2 <- renderDataTable({
        
        Hitters_Gulls %>%
            filter(Batter == input$H_Hitter_select, PitcherThrows != input$H_PitcherHandness_select, Date %in% c(input$H_Date_select) | PitcherTeam %in% c(input$H_Opp_select)) %>%
            select(c("Date", "PitcherTeam", "Pitcher", "PitcherThrows", "Outs", "Balls", "Strikes", "PitchCall", "AutoPitchType", "RelSpeed", "AutoHitType", "PlayResult", "ExitSpeed", "Angle", "Distance")) %>%
        mutate(RelSpeed = round(RelSpeed, 1), ExitSpeed = round(ExitSpeed, 1), Angle = round(Angle, 1), Distance = round(Distance, 0)) %>%
        rename(Pitch = AutoPitchType, HitType = AutoHitType, Speed = RelSpeed, Opp = PitcherTeam, Throws = PitcherThrows)
        
    })
    


    
    
    
    #Catchers Tab -------------------------------------------------------
    
    observe({
        
        updateSelectInput(
            session, "C_Date_select", choices = unique(Catchers_Gulls$Date),
            selected = if(input$C_All_select) unique(Catchers_Gulls$Date)
        )
    })
    
    output$Catcher_1 <- renderPlot({
        
        grid.arrange(Pitchers_Gulls %>%
                         filter(BatterSide == "Left", PitchCall == "StrikeCalled" | PitchCall == "BallCalled", Catcher == input$C_Catcher_select, BatterSide != input$C_PitcherHandness_select, Date %in% c(input$C_Date_select) | BatterTeam %in% c(input$C_Opp_select)) %>%
                         ggplot() 
                     + geom_point(aes(x = -PlateLocSide, y = PlateLocHeight, color = PitchCall), size = 8, alpha = 0.95)  
                     + geom_path(aes(x,y), data = kZone) 
                     + theme_void() 
                     + coord_cartesian(xlim = c(-3,3), ylim = c(-0.5,5)) 
                     + theme(legend.title=element_blank(), legend.position = "bottom", plot.title = element_text( size=25, face="bold", hjust = 0.5), legend.text = element_text(size = 15)) 
                     + ggtitle("LHH") 
                     + scale_color_manual(values = c(BallCalled = "dodgerblue", StrikeCalled = "red")) 
                     + H_Topplate + H_Lplateside + H_Rplateback + H_Lplateback + H_Rplateside, 
                     
                     Pitchers_Gulls %>%
                         filter(BatterSide == "Right", PitchCall == "StrikeCalled" | PitchCall == "BallCalled", Catcher == input$C_Catcher_select, BatterSide != input$C_PitcherHandness_select, Date %in% c(input$C_Date_select) | BatterTeam %in% c(input$C_Opp_select)) %>%
                         ggplot() 
                     + geom_point(aes(x = -PlateLocSide, y = PlateLocHeight, color = PitchCall), size = 8, alpha = 0.95)  
                     + geom_path(aes(x,y), data = kZone) 
                     + theme_void() 
                     + coord_cartesian(xlim = c(-3,3), ylim = c(-0.5,5)) 
                     + theme(legend.title=element_blank(), legend.position = "bottom", plot.title = element_text( size=25, face="bold", hjust = 0.5), legend.text = element_text(size = 15)) 
                     + ggtitle("RHH") 
                     + scale_color_manual(values = c(BallCalled = "dodgerblue", StrikeCalled = "red")) 
                     + H_Topplate + H_Lplateside + H_Rplateback + H_Lplateback + H_Rplateside, 
                     
                     ncol = 2)
        
    })
    
    
    
    
    #Pitching Leaderboards Tab ---------------------------------------
    
    output$Fastball_Leaderboard <- renderDataTable({
        
        Pitchers_Gulls %>%
            filter(TaggedPitchType == "Fastball" | TaggedPitchType == "Sinker") %>%
            group_by(Pitcher, TaggedPitchType) %>%
            dplyr::summarise(NumPitches = n(), 
                             MaxVelo = round(max(RelSpeed, na.rm = TRUE), digits = 1), 
                             AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1), 
                             MaxSpin = round(max(SpinRate, na.rm = TRUE), 0), 
                             AvgSpin = round(mean(SpinRate, na.rm = TRUE), 0), 
                             Extension = round(mean(Extension, na.rm = TRUE), 2), 
                             VertBreak = round(mean(VertBreak, na.rm = TRUE), 2), 
                             IVB = round(mean(InducedVertBreak, na.rm = TRUE), 2), 
                             HorzBreak = round(abs(mean(HorzBreak, na.rm = TRUE)), 2),
                             VAA = round(mean(VertApprAngle, na.rm = TRUE), 2)) %>%
            datatable(options = list(paging = FALSE, searching = FALSE))
        
    })
    
    output$BreakingBall_Leaderboard <- renderDataTable({
        
        Pitchers_Gulls %>%
            filter(TaggedPitchType == "Slider" | TaggedPitchType == "Curveball" | TaggedPitchType == "Cutter") %>%
            group_by(Pitcher, TaggedPitchType) %>%
            dplyr::summarise(NumPitches = n(), 
                             MaxVelo = round(max(RelSpeed, na.rm = TRUE), digits = 1), 
                             AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1), 
                             MaxSpin = round(max(SpinRate, na.rm = TRUE), 0), 
                             AvgSpin = round(mean(SpinRate, na.rm = TRUE), 0), 
                             Extension = round(mean(Extension, na.rm = TRUE), 2), 
                             VertBreak = round(mean(VertBreak, na.rm = TRUE), 2), 
                             IVB = round(mean(InducedVertBreak, na.rm = TRUE), 2), 
                             HorzBreak = round(abs(mean(HorzBreak, na.rm = TRUE)), 2),
                             VAA = round(mean(VertApprAngle, na.rm = TRUE), 2),) %>%
            datatable(options = list(paging = FALSE, searching = FALSE))
        
    })
    
    output$OffSpeed_Leaderboard <- renderDataTable({
        
        Pitchers_Gulls %>%
            filter(TaggedPitchType == "Splitter" | TaggedPitchType == "ChangeUp") %>%
            group_by(Pitcher, TaggedPitchType) %>%
            dplyr::summarise(NumPitches = n(), 
                             MaxVelo = round(max(RelSpeed, na.rm = TRUE), digits = 1), 
                             AvgVelo = round(mean(RelSpeed, na.rm = TRUE), 1), 
                             MaxSpin = round(max(SpinRate, na.rm = TRUE), 0), 
                             AvgSpin = round(mean(SpinRate, na.rm = TRUE), 0), 
                             Extension = round(mean(Extension, na.rm = TRUE), 2), 
                             VertBreak = round(mean(VertBreak, na.rm = TRUE), 2), 
                             IVB = round(mean(InducedVertBreak, na.rm = TRUE), 2), 
                             HorzBreak = round(abs(mean(HorzBreak, na.rm = TRUE)), 2),
                             VAA = round(mean(VertApprAngle, na.rm = TRUE), 2),) %>%
            datatable(options = list(paging = FALSE, searching = FALSE))
        
    })
    
    
    
    
    #Hitting Leaderboards Tab ------------------------------------------
    
    output$Hitting_Leaderboard <- renderDataTable({
        
        Hitters_Gulls %>%
            filter(PitchCall == "InPlay") %>%
            group_by(Batter) %>%
            dplyr::summarise(BallsInPlay = n(),
                             MaxExitVelo = round(max(ExitSpeed, na.rm = TRUE), digits = 1), 
                             AvgExitVelo = round(mean(ExitSpeed, na.rm = TRUE), 1),
                             HardHit = sum(ExitSpeed >= 94.99, na.rm = TRUE),
                             BestSpeed = round(mean(sort(ExitSpeed, decreasing = TRUE)[1:ceiling(length(ExitSpeed) / 2)]), 1),
                             MaxDistance = round(max(Distance, na.rm = TRUE), 0), 
                             AvgDistance = round(mean(Distance, na.rm = TRUE), 0), 
                             AvgLaunchAngle = round(mean(Angle, na.rm = TRUE), 1),
                             SweetSpot = sum(Angle >= 8 & Angle <= 32, na.rm = TRUE)) %>%
            mutate(SwSpPct = round((SweetSpot / BallsInPlay) * 100, 2)) %>%
            mutate(HardHitRate = round((HardHit / BallsInPlay) * 100, 2)) %>%
            relocate(HardHitRate, .after = HardHit) %>%
            datatable(options = list(paging = FALSE, searching = FALSE))
        
    })
    
    
    
}




# Run the application ------------------------------------------------

shinyApp(ui = ui, server = server)
