library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)
library(caret)
library(gridExtra)
library(ggrepel)
library(DT)
library(plotly)
library(shinycssloaders)
library(GeomMLBStadiums)
library(stringr)

# Load your data
df <- read.csv("Fordham_Master.csv")
df <- df %>% filter(PitcherTeam %in% c("FOR_RAM", "FOR_RAM1"))

# Ensure Date column is in the Date format
df$Date <- as.Date(df$Date)

# Trim leading and trailing whitespaces and convert to lower case
df$Pitcher <- tolower(str_trim(df$Pitcher))

# Create RunValue column
df$RunValue <- df$PitchCall %>%
  map_dbl(~ switch(.,
                   'HomeRun' = 1.374328827219,
                   'Triple' = 1.05755624961515,
                   'Double' = 0.766083122898271,
                   'Single' = 0.467292970729251,
                   'BallCalled' = 0.0636883289483747,
                   'HitByPitch' = 0.0636883289483747,
                   'BallinDirt' = 0.0636883289483747,
                   'WildPitch' = 0.0636883289483747,
                   'FoulBall' = -0.0380502742575014,
                   'FoulBallNotFieldable' = -0.0380502742575014,
                   'FoulBallFieldable' = -0.0380502742575014,
                   'StrikeCalled' = -0.065092516089806,
                   'StrikeSwinging' = -0.118124935770601,
                   'Out' = -0.1955687665555,
                   'Sacrifice' = -0.236889645519856,
                   'FieldersChoice' = -0.1955687665555,
                   'Error' = -0.1955687665555,
                   'BallIntentional' = 0.0636883289483747,
                   'Undefined' = 0.000000000000000,
                   0.000000000000000))

# Apply checks to the dataframe
df <- df %>%
  mutate(HCheck = case_when(PlayResult %in% c('Single', 'Double', 'Triple', 'HomeRun') ~ TRUE, TRUE ~ FALSE),
         GBCheck = case_when(TaggedHitType %in% c('GroundBall') ~ TRUE, TRUE ~ FALSE),
         BBECheck = case_when(TaggedHitType %in% c('GroundBall', 'LineDrive', 'FlyBall', 'Popup') ~ TRUE, TRUE ~ FALSE),
         SwingCheck = case_when(PitchCall %in% c('FoulBall', 'FoulBallNotFieldable','StrikeSwinging', 'InPlay') ~ TRUE, TRUE ~ FALSE),
         WhiffCheck = case_when(PitchCall %in% c('StrikeSwinging') ~ TRUE, TRUE ~ FALSE),
         CSWCheck = case_when(PitchCall %in% c('StrikeSwinging', 'StrikeCalled') ~ TRUE, TRUE ~ FALSE),
         StrikeCheck = case_when(PitchCall %in% c('StrikeSwinging', 'FoulBall','FoulBallNotFieldable','InPlay', 'StrikeCalled') ~ TRUE, TRUE ~ FALSE),
         ZoneCheck = case_when(between(PlateLocHeight, 1.59, 3.41) & between(PlateLocSide, -1, 1) ~ TRUE, TRUE ~ FALSE),
         SweetSpotCheck = case_when(between(Angle, 10, 30) ~ TRUE, TRUE ~ FALSE),
         HardHitCheck = case_when(between(ExitSpeed, 95, 120) ~ TRUE, TRUE ~ FALSE),
         BarrelCheck = case_when(between(Angle, 10, 30)  & between(ExitSpeed, 95, 120) ~ TRUE, TRUE ~ FALSE),
         WhiffCheck = case_when(PitchCall %in% c('StrikeSwinging') ~ TRUE, TRUE ~ FALSE),
         SwingCheck = case_when(PitchCall %in% c('StrikeSwinging', 'InPlay', 'FoulBall','FoulBallNotFieldable') ~ TRUE, TRUE ~ FALSE),
         CalledStrikeCheck = case_when(PitchCall %in% c('StrikeCalled') ~ TRUE, TRUE ~ FALSE),
         BallCheck = case_when(PitchCall %in% c('BallCalled', 'HitByPitch') ~ TRUE, TRUE ~ FALSE),
         FoulCheck = case_when(PitchCall %in% c('FoulBall','FoulBallNotFieldable') ~ TRUE, TRUE ~ FALSE),
         SingleCheck = case_when(PlayResult %in% c('Single') ~ TRUE, TRUE ~ FALSE),
         DoubleCheck = case_when(PlayResult %in% c('Double') ~ TRUE, TRUE ~ FALSE),
         TripleCheck = case_when(PlayResult %in% c('Triple') ~ TRUE, TRUE ~ FALSE),
         HRCheck = case_when(PlayResult %in% c('HomeRun') ~ TRUE, TRUE ~ FALSE),
         SacCheck = case_when(PlayResult %in% c('Sacrifice') ~ TRUE, TRUE ~ FALSE),
         HBPCheck = case_when(PitchCall %in% c('HitByPitch') ~ TRUE, TRUE ~ FALSE),
         StrikeoutCheck = case_when(KorBB %in% c('Strikeout') ~ TRUE, TRUE ~ FALSE),
         WalkCheck = case_when(KorBB %in% c('Walk') ~ TRUE, TRUE ~ FALSE),
         BIPCheck = case_when(PlayResult %in% c('Undefined') ~ FALSE, TRUE ~ TRUE),
         ErrorCheck = if_else(PlayResult %in% c('Error'), TRUE, FALSE),
         ABCheck = StrikeoutCheck + BIPCheck - SacCheck,
         PACheck = StrikeoutCheck + WalkCheck + HBPCheck + BIPCheck)

ui <- dashboardPage(
  dashboardHeader(title = "Fordham Baseball Data Portal"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Location Plot", tabName = "location_plot", icon = icon("map-marker")),
      menuItem("Heatmaps", tabName = "heatmaps", icon = icon("fire")),
      menuItem("Hit Locations", tabName = "hit_locations", icon = icon("baseball-ball")),
      br(),
      br(),
      dateRangeInput("dateRange", "Select Date Range:", start = min(df$Date), end = max(df$Date)),
      uiOutput("pitcherSelect"),
      actionButton("generate", "Generate Report")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 6, title = "Pitch Type Plot", solidHeader = TRUE, plotOutput("pitchTypePlot") %>% withSpinner()),
                box(width = 6, title = "Average Movement Plot", solidHeader = TRUE, plotOutput("avgMovementPlot") %>% withSpinner())
              ),
              fluidRow(
                box(width = 12, title = "Pitch Type Averages Table", solidHeader = TRUE, DTOutput("pitchTypeTable") %>% withSpinner())
              ),
              fluidRow(
                box(width = 12, title = "Summary Table", solidHeader = TRUE, DTOutput("summaryTable") %>% withSpinner())
              ),
              fluidRow(
                box(width = 4, title = "Run Value", solidHeader = TRUE, DTOutput("runValueTable") %>% withSpinner()),
                box(width = 4, title = "Usage Percentage", solidHeader = TRUE, DTOutput("usagePercentageTable") %>% withSpinner()),
                box(width = 4, title = "Whiff Percentage", solidHeader = TRUE, DTOutput("whiffPercentageTable") %>% withSpinner())
              ),
              fluidRow(
                box(width = 4, title = "Average VertApprAngle", solidHeader = TRUE, DTOutput("avgVertApprAngleTable") %>% withSpinner()),
                box(width = 4, title = "Average Exit Speed", solidHeader = TRUE, DTOutput("avgExitSpeedTable") %>% withSpinner()),
                box(width = 4, title = "Average Angle", solidHeader = TRUE, DTOutput("avgAngleTable") %>% withSpinner())
              )
      ),
      tabItem(tabName = "location_plot",
              fluidRow(
                box(width = 12, title = "Location Plot", solidHeader = TRUE, 
                    plotOutput("locationPlot", height = "400px") %>% withSpinner())
              )
      ),
      tabItem(tabName = "heatmaps",
              fluidRow(
                uiOutput("heatmapRows")
              )
      ),
      tabItem(tabName = "hit_locations",
              fluidRow(
                box(width = 12, title = "Hit Locations", solidHeader = TRUE, 
                    plotOutput("hitLocationsPlot") %>% withSpinner())
              )
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    req(input$dateRange)
    date_data <- subset(df, Date >= input$dateRange[1] & Date <= input$dateRange[2])
    updateSelectInput(session, "pitcher", "Select Pitcher:", choices = unique(date_data$Pitcher))
  })
  
  output$pitcherSelect <- renderUI({
    selectInput("pitcher", "Select Pitcher:", choices = NULL)
  })
  
  observeEvent(input$generate, {
    req(input$dateRange, input$pitcher)
    pitcher_name <- input$pitcher
    
    # Convert pitcher_name to lowercase and trim whitespace
    pitcher_name <- tolower(str_trim(pitcher_name))
    
    pitcher_data <- df %>%
      filter(Date >= input$dateRange[1] & Date <= input$dateRange[2] & Pitcher == pitcher_name)
    
    if (nrow(pitcher_data) == 0) {
      showModal(modalDialog(
        title = "No Data",
        "No data available for the selected date range and pitcher."
      ))
      return()
    }
    
    first_name <- strsplit(pitcher_name, ", ")[[1]][2]
    last_name <- strsplit(pitcher_name, ", ")[[1]][1]
    formatted_pitcher_name <- paste(last_name, first_name)
    
    pitch_type_counts <- table(pitcher_data$TaggedPitchType)
    
    pitch_type_averages <- pitcher_data %>%
      filter(TaggedPitchType != "Undefined") %>%
      group_by(TaggedPitchType) %>%
      summarise(across(c(RelSpeed, SpinRate, InducedVertBreak, HorzBreak, RelHeight, RelSide, Extension), ~mean(., na.rm = TRUE)))
    
    pitch_type_max_velocity <- pitcher_data %>%
      filter(TaggedPitchType != "Undefined") %>%
      group_by(TaggedPitchType) %>%
      summarise(MaxVelo = max(RelSpeed, na.rm = TRUE))
    
    pitch_type_averages <- left_join(pitch_type_averages, pitch_type_max_velocity, by = "TaggedPitchType")
    
    colnames(pitch_type_averages)[colnames(pitch_type_averages) == "RelSpeed"] <- "AvgVelo"
    pitch_type_averages <- pitch_type_averages %>%
      select(TaggedPitchType, AvgVelo, MaxVelo, everything())
    
    # Round numeric columns to 2 decimal places
    pitch_type_averages <- pitch_type_averages %>% mutate(across(where(is.numeric), ~round(., 2)))
    
    strikes <- pitcher_data[pitcher_data$PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBall","FoulBallNotFieldable","InPlay"), ]
    strike_percentages <- prop.table(table(strikes$TaggedPitchType)) * 100
    strike_percentages[is.na(strike_percentages)] <- 0
    strike_percentages <- as.data.frame(strike_percentages)
    colnames(strike_percentages) <- c("Pitch Type", "Strike %")
    strike_percentages$`Strike %` <- round(strike_percentages$`Strike %`, 2)
    
    total_pitches <- nrow(pitcher_data)
    usage_percentages <- pitcher_data %>%
      filter(TaggedPitchType != "Undefined") %>%
      count(TaggedPitchType) %>%
      mutate(Usage = n / total_pitches * 100) %>%
      select(TaggedPitchType, Usage)
    usage_percentages <- usage_percentages %>%
      rename("Pitch Type" = TaggedPitchType, "Usage %" = Usage)
    usage_percentages$`Usage %` <- round(usage_percentages$`Usage %`, 2)
    
    total_swings <- pitcher_data %>%
      filter(PitchCall %in% c("StrikeSwinging", "FoulBall", "FoulBallNotFieldable", "InPlay")) %>%
      group_by(TaggedPitchType) %>%
      summarise(TotalSwings = n())
    
    whiffs <- pitcher_data %>%
      filter(PitchCall == "StrikeSwinging") %>%
      group_by(TaggedPitchType) %>%
      summarise(Whiffs = n())
    
    whiff_percentages <- left_join(whiffs, total_swings, by = "TaggedPitchType") %>%
      mutate(WhiffPercentage = Whiffs / TotalSwings * 100) %>%
      replace(is.na(.), 0)
    whiff_percentages <- whiff_percentages %>%
      select(TaggedPitchType, WhiffPercentage) %>%
      rename("Pitch Type" = TaggedPitchType, "Whiff %" = WhiffPercentage)
    whiff_percentages$`Whiff %` <- round(whiff_percentages$`Whiff %`, 2)
    
    TrackmanFile2 <- pitcher_data
    TrackmanFile <- TrackmanFile2 %>%
      filter(complete.cases(TaggedPitchType, HorzBreak, InducedVertBreak))
    
    means_df <- TrackmanFile %>%
      group_by(TaggedPitchType) %>%
      summarise(
        avg_HorzBreak = mean(HorzBreak, na.rm = TRUE),
        avg_InducedVertBreak = mean(InducedVertBreak, na.rm = TRUE)
      )
    
    sc_colors <- c(
      "Fastball" = "#D22D49",
      "Sinker" = "#933F2C",
      "Cutter" = "#933F2C", 
      "Slider" = "#FE9D00",
      "FourSeamFastBall" = "#D22D49",
      "Curveball" = "#00D1ED",
      "Splitter" = "#3BACAC",
      "ChangeUp" = "#1DBE3A"
    )
    
    output$pitchTypePlot <- renderPlot({
      ggplot(data = TrackmanFile, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
        geom_point(alpha = .5, size = 3, stroke = .5) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        geom_vline(xintercept = 0, linetype = "solid", color = "black") +
        scale_x_continuous(breaks = seq(-25, 25, 10), limits = c(-25, 25)) +
        scale_y_continuous(breaks = seq(-25, 25, 10), limits = c(-25, 25)) +
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
        guides(fill = FALSE, color = guide_legend(title = "Pitch Type", alpha = 1)) +
        labs(title = "Pitch Movement Profile (Pitcher's Perspective)",
             x = "Horizontal Break (inches)",
             y = "Induced Vertical Break (inches)",
             caption = "Data from Trackman") +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        scale_color_manual(values = sc_colors) +
        geom_point(data = means_df, aes(x = avg_HorzBreak, y = avg_InducedVertBreak, color = TaggedPitchType, fill = TaggedPitchType),
                   size = 5, alpha = 1, shape = 21, stroke = .5, color = "black") +
        scale_fill_manual(values = sc_colors)
    })
    
    output$avgMovementPlot <- renderPlot({
      ggplot(data = TrackmanFile, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
        geom_point(alpha = 0.3, size = 2) +
        geom_hline(yintercept = 0, linetype = "solid", color = "black") +
        geom_vline(xintercept = 0, linetype = "solid", color = "black") +
        scale_x_continuous(breaks = seq(-25, 25, 10), limits = c(-25, 25)) +
        scale_y_continuous(breaks = seq(-25, 25, 10), limits = c(-25, 25)) +
        theme(panel.border = element_rect(color = "black", fill = NA, size = 1)) +
        guides(fill = FALSE, color = guide_legend(title = "Pitch Type", alpha = 1)) +
        labs(title = "Pitch Movement Profile (Pitcher's Perspective)",
             x = "Horizontal Break (inches)",
             y = "Induced Vertical Break (inches)",
             caption = "Based on induced movement: 80% C.I.") +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        scale_color_manual(values = sc_colors) +
        geom_point(data = means_df, aes(x = avg_HorzBreak, y = avg_InducedVertBreak, color = TaggedPitchType, fill = TaggedPitchType),
                   size = 3, alpha = 1, shape = 21, stroke = .5) +
        scale_fill_manual(values = sc_colors) +
        stat_ellipse(data = TrackmanFile, aes(x = HorzBreak, y = InducedVertBreak, fill = TaggedPitchType),
                     geom = "polygon", alpha = 0.07, level = .80, type = "t")
    })
    
    strike_zone <- tibble(
      PlateLocSide = c(-0.85, -0.85, 0.85, 0.85, -0.85),
      PlateLocHeight = c(1.6, 3.5, 3.5, 1.6, 1.6)
    )
    
    output$locationPlot <- renderPlot({
      ggplot(data = TrackmanFile, aes(x = PlateLocSide, y = PlateLocHeight, color = TaggedPitchType)) +
        geom_point(alpha = 1, size = 4) +
        scale_color_manual(values = sc_colors) +
        geom_path(data = strike_zone, color = "black", linewidth = 1.3) +
        ylim(0.5, 4.25) +
        xlim(-1.7, 1.7) +
        coord_fixed() +
        labs(title = "Location Plot",
             x = "Plate Side (feet)",
             y = "Plate Height (feet)",
             caption = "Data from Trackman") +
        guides(fill = FALSE, color = guide_legend(title = "Pitch Type", alpha = 1)) +
        theme(plot.title = element_text(face = "bold", hjust = 0.5)) +
        geom_segment(aes(x = -0.2833333, xend = -0.2833333, y = 1.6, yend = 3.5), color = "black", linetype = "dashed") +
        geom_segment(aes(x = 0.2833333, xend = 0.2833333, y = 1.6, yend = 3.5), color = "black", linetype = "dashed") +
        geom_segment(aes(x = -0.85, xend = 0.85, y = 2.2, yend = 2.2), color = "black", linetype = "dashed") +
        geom_segment(aes(x = -0.85, xend = 0.85, y = 2.9, yend = 2.9), color = "black", linetype = "dashed")
    })
    
    output$pitchTypeTable <- renderDT({
      datatable(pitch_type_averages, options = list(pageLength = 5, dom = 't'))
    })
    
    output$usagePercentageTable <- renderDT({
      datatable(usage_percentages, options = list(pageLength = 5, dom = 't', columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    output$whiffPercentageTable <- renderDT({
      datatable(whiff_percentages, options = list(pageLength = 5, dom = 't', columnDefs = list(list(className = 'dt-center', targets = "_all"))))
    })
    
    # Create Run Value per 100 pitches table
    run_value_per_100 <- pitcher_data %>%
      filter(TaggedPitchType != "Undefined") %>%
      group_by(TaggedPitchType) %>%
      summarise(
        TotalPitches = n(),
        TotalRunValue = sum(RunValue, na.rm = TRUE),
        RunValuePer100 = (TotalRunValue / TotalPitches) * 100
      ) %>%
      select(TaggedPitchType, TotalPitches, RunValuePer100) %>%
      mutate(across(where(is.numeric), round, 2))
    
    output$runValueTable <- renderDT({
      datatable(run_value_per_100, options = list(pageLength = 5, dom = 't', columnDefs = list(list(width = '80px', targets = "_all"))))
    })
    
    # Create summary table
    summary_table <- pitcher_data %>%
      summarise(
        Pitches = n(),
        PA = sum(PACheck, na.rm = TRUE),
        BBE = sum(BBECheck, na.rm = TRUE),
        H = sum(HCheck, na.rm = TRUE),
        `1B` = sum(SingleCheck, na.rm = TRUE),
        `2B` = sum(DoubleCheck, na.rm = TRUE),
        `3B` = sum(TripleCheck, na.rm = TRUE),
        HR = sum(HRCheck, na.rm = TRUE),
        SO = sum(StrikeoutCheck, na.rm = TRUE),
        BB = sum(WalkCheck, na.rm = TRUE),
        HBP = sum(HBPCheck, na.rm = TRUE),
        Strikes = sum(StrikeCheck, na.rm = TRUE),
        Chases = sum(SwingCheck & !ZoneCheck, na.rm = TRUE),
        Whiffs = sum(WhiffCheck & SwingCheck, na.rm = TRUE)
      )
    
    output$summaryTable <- renderDT({
      datatable(summary_table, options = list(pageLength = 5, dom = 't'))
    })
    
    # Create average VertApprAngle table
    avg_vert_appr_angle <- pitcher_data %>%
      filter(!is.na(VertApprAngle)) %>%
      group_by(TaggedPitchType) %>%
      summarise(AvgVertApprAngle = mean(VertApprAngle, na.rm = TRUE)) %>%
      mutate(AvgVertApprAngle = round(AvgVertApprAngle, 2)) %>%
      arrange(desc(AvgVertApprAngle))
    
    output$avgVertApprAngleTable <- renderDT({
      datatable(avg_vert_appr_angle, options = list(pageLength = 5, dom = 't'))
    })
    
    # Create average ExitSpeed table
    avg_exit_speed <- pitcher_data %>%
      filter(!is.na(ExitSpeed)) %>%
      group_by(TaggedPitchType) %>%
      summarise(AvgExitSpeed = mean(ExitSpeed, na.rm = TRUE)) %>%
      mutate(AvgExitSpeed = round(AvgExitSpeed, 2)) %>%
      arrange(desc(AvgExitSpeed))
    
    output$avgExitSpeedTable <- renderDT({
      datatable(avg_exit_speed, options = list(pageLength = 5, dom = 't'))
    })
    
    # Create average Angle table
    avg_angle <- pitcher_data %>%
      filter(!is.na(Angle)) %>%
      group_by(TaggedPitchType) %>%
      summarise(AvgAngle = mean(Angle, na.rm = TRUE)) %>%
      mutate(AvgAngle = round(AvgAngle, 2)) %>%
      arrange(desc(AvgAngle))
    
    output$avgAngleTable <- renderDT({
      datatable(avg_angle, options = list(pageLength = 5, dom = 't'))
    })
    
    # Create heatmap plots for each pitch type
    output$heatmapRows <- renderUI({
      pitch_types <- unique(pitcher_data$TaggedPitchType)
      pitch_types <- pitch_types[pitch_types != "Undefined"]
      
      plot_list <- lapply(pitch_types, function(pitch_type) {
        count <- nrow(pitcher_data %>% filter(TaggedPitchType == pitch_type))
        if (count >= 5) {
          plot_output <- plotOutput(paste0("heatmap_", pitch_type), height = "400px") %>% withSpinner()
          box(width = 6, title = paste(pitch_type, "Heatmap"), solidHeader = TRUE, plot_output)
        }
      })
      
      do.call(fluidRow, plot_list)
    })
    
    # Render heatmap plots
    lapply(unique(pitcher_data$TaggedPitchType), function(pitch_type) {
      count <- nrow(pitcher_data %>% filter(TaggedPitchType == pitch_type))
      if (count >= 5) {
        output[[paste0("heatmap_", pitch_type)]] <- renderPlot({
          ggplot(pitcher_data %>% filter(TaggedPitchType == pitch_type), aes(x = PlateLocSide, y = PlateLocHeight)) +
            stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "black") +
            scale_fill_gradient(low = "white", high = "darkred") +
            annotate("rect", xmin = -1, xmax = 1,
                     ymin = 1.6, ymax = 3.4,
                     fill = NA, color = "black",
                     alpha = 0.1) +
            ylim(1, 4) + xlim(-1.8, 1.8) +
            theme_classic() +
            xlab("Horizontal Pitch Location") +
            ylab("Vertical Pitch Location") +
            ggtitle(paste(pitch_type, "Location Heat Map"), subtitle = "Pitcher's Perspective") +
            guides(fill = guide_colorbar(title = "Density"))
        }, width = 400, height = 400)
      }
    })
    
    # Plot hit locations
    output$hitLocationsPlot <- renderPlot({
      # Filter and transform data
      hit_locations <- pitcher_data %>%
        filter(PitchCall == "InPlay") %>%
        mutate(
          hc_x = sin(Bearing * pi / 180) * Distance,
          hc_y = cos(Bearing * pi / 180) * Distance
        )
      
      if (nrow(hit_locations) == 0) {
        return(NULL)
      }
      
      # Plot hit locations
      ggplot(hit_locations, aes(x = hc_x, y = hc_y, color = PlayResult)) +
        geom_point(size = 8) +
        scale_color_brewer(palette = "Set1") +
        geom_mlb_stadium(stadium_ids = 'yankees', stadium_transform_coords = TRUE, stadium_segments = 'all', color = 'black') +
        coord_fixed() +
        theme_minimal() +
        theme(panel.grid = element_blank()) +
        theme(axis.text = element_blank()) +
        labs(x = NULL, y = NULL, color = "Play Result", title = "Hit Locations")
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)

    


