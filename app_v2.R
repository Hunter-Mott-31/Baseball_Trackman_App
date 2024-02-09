library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)

# Combine data frame
game_files <- list.files(pattern = "*.csv")
meta_data <- bind_rows(lapply(game_files, read.csv))

# Mutate and filter data
meta_data <- meta_data %>%
  mutate(
    TaggedPitchType = ifelse(TaggedPitchType %in% c("FourSeamFastBall"), "Fastball", TaggedPitchType)
  ) %>%
  filter(
    TaggedPitchType != "Undefined",
    TaggedPitchType != "Other"
  ) %>%
  select(
    Pitcher, TaggedPitchType, PlayResult, SpinRate, RelSpeed, HorzBreak, InducedVertBreak,
    RelSide, RelHeight, ExitSpeed, Angle, Distance, Batter, PitchCall, Extension
  )

# Define unique pitchers and hitters
unique_pitchers <- unique(meta_data$Pitcher)
unique_hitters <- unique(meta_data$Batter)

# Define UI
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel(
    tags$h1(
      "CWL Analytics",
      span("(Accurate as of 2/4/24)", style = "font-size: 14px;")
    )
  ),
  navbarPage(
    theme = "cerulean",
    "Options : ",
    tabPanel("Pitcher",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("pitcherDropdown", "Select Pitcher", choices = unique(meta_data$Pitcher), multiple = FALSE),
                 selectInput("graphType", "Select Graph", choices = c("Spin Rate", "Velocity", "Pitch Movement", "Pitch Release", "DataTable"), selected = "Spin Rate"),
               ),
               mainPanel(
                 # Use conditionalPanel for each graph
                 conditionalPanel(
                   condition = "input.graphType == 'Spin Rate'",
                   plotOutput("spinRatePlot", height = "600px")  # Adjust height as needed
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'Velocity'",
                   plotOutput("relSpeedPlot", height = "600px"),  # Adjust height as needed
                   DTOutput("max_velocity_table")  # New DTOutput for max velocity table
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'Pitch Movement'",
                   plotOutput("pitch_movement_plot", height = "600px")  # Adjust height as needed
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'Pitch Release'",
                   plotOutput("pitch_release_plot", height = "600px")
                 ),
                 conditionalPanel(
                   condition = "input.graphType == 'DataTable'",
                   DTOutput("pitcher_data_table")  # New DTOutput for pitcher data table
                 )
               )
             )
    ),
    tabPanel("Hitter",
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("hitterDropdown", "Select Hitter", choices = unique_hitters, multiple = FALSE)
               ),
               mainPanel(
                 plotOutput("hit_scatter_plot", height = "600px"),
                 DTOutput("hitter_table")
               )
             )
    ),
    tabPanel("About",
             h2("Purpose of the App:"),
             p(
               "Welcome to the CWL Analytics Shiny app! This tool provides comprehensive statistics and insightful visualizations for California Winter League (CWL) players, utilizing Trackman data from the 2024 season. ",
               "Tailored for player development, the app enables athletes to track and compare performance metrics over time, enhancing their chances of being scouted for professional leagues. ",
               "Emphasizing objective statistics like exit velocity for hitters and pitcher's velocity, the app empowers CWL athletes to actively work towards improvement within their control. ",
               "While hitters may not control every outcome, maximizing the force behind their hits can influence results. Similarly, pitchers can manage factors like velocity, spin rate, and extension to enhance their performance."
             ),
             
             h2("Trackman Resources:"),
             p(
               "This app utilizes data from Trackman, a leading sports technology company that delivers advanced analytics for baseball. ",
               "Trackman's cutting-edge technology is extensively employed in professional baseball to capture crucial data on pitch velocity, spin rate, ",
               "and other key metrics. For more in-depth information about Trackman, please visit their official website at ",
               a("Trackman - Official Website", href = "https://www.trackman.com/baseball", target = "_blank"),
               ". Additionally, players can explore useful insights and guidance on improving their Trackman metrics by visiting the ",
               a("Simple Sabermetrics YouTube channel", href = "https://www.youtube.com/@SimpleSabermetrics", target = "_blank"),
               ". This channel provides valuable resources and tutorials for players looking to enhance their performance."
             ),
             h2("App Info:"),
             p(
               "The data used for this app was collected from CWL games using Trackman's system.",
               "Ensure you choose the necessary player type filters for accurate data."
             ),
             
             p(
               "This app was created by Hunter Mott using R Shiny. You can find the code on my GitHub: ",
               a("GitHub - Hunter Mott", href = "https://github.com/Hunter-Mott-31/", target="_blank"),
               ". Feel free to contact me with any comments or recommendations about the app. Stay tuned for future versions!"
             ),
             
             h3("Contact Information:"),
             p(
               "LinkedIn: ", a("Hunter Mott - LinkedIn", href = "https://www.linkedin.com/in/hunter-mott/", target="_blank"),
               "Email: h-mott@outlook.com"
             )
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Reactive expression to filter data based on selected pitcher
  selected_pitcher_data <- reactive({
    meta_data %>% 
      filter(Pitcher == input$pitcherDropdown)
  })
  
  # Render DataTable for pitcher data
  output$pitcher_data_table <- DT::renderDT({
    selected_pitcher <- selected_pitcher_data()
    
    # Calculate averages for each TaggedPitchType
    pitch_type_averages <- selected_pitcher %>%
      group_by(TaggedPitchType) %>%
      summarize(
        MaxVelocity = round(max(RelSpeed, na.rm = TRUE)),
        AvgVelocity = round(mean(RelSpeed, na.rm = TRUE)),
        AvgSpinRate = round(mean(SpinRate, na.rm = TRUE)),
        AvgHorzMovement = round(mean(HorzBreak, na.rm = TRUE)),
        AvgInducedVertMovement = round(mean(InducedVertBreak, na.rm = TRUE)),
        StrikePercentage = sum(PitchCall %in% c("StrikeCalled", "FoulBall", "StrikeSwinging")) /
          (sum(PitchCall %in% c("StrikeCalled", "FoulBall", "StrikeSwinging")) + sum(PitchCall %in% c("BallCalled", "BallInDirt", "HitByPitch", "BallIntentional"))),
        Strikes_total = sum(PitchCall %in% c("StrikeCalled", "FoulBall", "StrikeSwinging")),
        Balls_total = sum(PitchCall %in% c("BallCalled", "BallInDirt", "HitByPitch", "BallIntentional")),
        Extension_ft = round(mean(Extension),1)
      )
    
    # Create DataTable for pitcher data
    DT::datatable(
      pitch_type_averages,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE
      ),
      caption = tags$caption(
        HTML("<b style='font-size: 18px;'>Average Trackman Metrics by Pitch Type</b>")
      ),
      # Format the StrikePercentage column as percentage
      rownames = FALSE,
      class = "cell-border stripe",
      escape = FALSE
    ) %>%
      formatPercentage("StrikePercentage")
  })
  
  # Reactive expression to filter data based on selected hitter
  selected_hitter_data <- reactive({
    meta_data %>% 
      filter(Batter == input$hitterDropdown, !is.na(PlayResult), PlayResult != "Undefined")
  })
  
  # Render plot for average spin rate by pitch type
  output$spinRatePlot <- renderPlot({
    selected_pitcher <- selected_pitcher_data()
    
    # Calculate average spin rates by pitch type
    avg_spin_rates <- selected_pitcher %>%
      group_by(TaggedPitchType) %>%
      summarize(AvgSpinRate = mean(SpinRate, na.rm = TRUE))
    
    # Plot the average spin rates
    ggplot(avg_spin_rates, aes(x = TaggedPitchType, y = AvgSpinRate)) +
      geom_bar(stat = "identity", position = "dodge", fill = "darkblue", width = 0.5) +
      geom_text(aes(label = round(AvgSpinRate)),
                position = position_dodge(width = 0.5),
                vjust = -0.5, hjust = 0.5,
                size = 8, color = "black", fontface = "bold") +
      labs(
        title = paste("Average Spin Rate by Pitch Type for", input$pitcherDropdown),
        x = "Pitch Type",
        y = "Spin Rate (RPM)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black")
      ) +
      coord_cartesian(ylim = c(0, max(avg_spin_rates$AvgSpinRate) * 1.1))
  })
  
  # Render plot for average RelSpeed by pitch type
  output$relSpeedPlot <- renderPlot({
    selected_pitcher <- selected_pitcher_data()
    
    # Calculate average RelSpeed by pitch type
    avg_rel_speed <- selected_pitcher %>%
      group_by(TaggedPitchType) %>%
      summarize(AvgRelSpeed = mean(RelSpeed, na.rm = TRUE))
    
    # Plot the average RelSpeed
    ggplot(avg_rel_speed, aes(x = TaggedPitchType, y = AvgRelSpeed)) +
      geom_bar(stat = "identity", position = "dodge", fill = "red", width = 0.5) +
      geom_text(aes(label = round(AvgRelSpeed)),
                position = position_dodge(width = 0.5),
                vjust = -0.5, hjust = 0.5,
                size = 8, color = "black", fontface = "bold") +
      labs(
        title = paste("Average Velocity by Pitch Type for", input$pitcherDropdown),
        x = "Pitch Type",
        y = "Velocity (MPH)"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 24, face = "bold"),
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 16, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black")
      ) +
      coord_cartesian(ylim = c(0, max(avg_rel_speed$AvgRelSpeed) * 1.1))
  })
  
  # Render max velocity table
  output$max_velocity_table <- DT::renderDT({
    selected_pitcher <- selected_pitcher_data()
    
    # Calculate max velocity for each pitcher
    max_velocity_data <- selected_pitcher %>%
      group_by(Pitcher) %>%
      summarize(MaxVelocity = max(RelSpeed, na.rm = TRUE))
    
    # Create a DataTable without search and fixed number of entries
    DT::datatable(
      max_velocity_data,
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE
      )
    )
  })
  
  # Render plot for pitch movement
  output$pitch_movement_plot <- renderPlot({
    selected_pitcher <- selected_pitcher_data()
    
    ggplot(data = selected_pitcher, aes(x = HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) + 
      labs(x = "HB (Horizontal Break)", y = "IVB (InducedVertBreak)", color = "TaggedPitchType", title = "Pitch Movement") + 
      xlim(-30, 30) + ylim(-30, 30) +
      geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") + 
      geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
      geom_point(size = 3, na.rm = TRUE) +
      theme_bw() + 
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),  # Adjust axis text size
        axis.ticks = element_line(size = 0.5),  # Adjust axis ticks size
        panel.grid.major = element_line(color = "grey80", size = 0.2),  # Adjust grid lines size and color
      ) +
      scale_x_continuous(breaks = seq(-30, 30, 5)) +
      scale_y_continuous(breaks = seq(-30, 30, 5))
  })
  
  # Render plot for pitch release
  output$pitch_release_plot <- renderPlot({
    selected_pitcher <- selected_pitcher_data()
    
    ggplot(data = selected_pitcher, aes(x = RelSide, y = RelHeight, color = TaggedPitchType)) +
      labs(x = "Horizontal Release Point", y = "Vertical Release Point", color = "TaggedPitchType", title = "Release") + 
      xlim(-4, 4) + ylim(2, 7) +
      geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") + 
      geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
      geom_point(size = 3, na.rm = TRUE) +
      theme_bw() + 
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 16),  # Adjust axis text size
        axis.ticks = element_line(size = 0.5),  # Adjust axis ticks size
        panel.grid.major = element_line(color = "grey80", size = 0.2),  # Adjust grid lines size and color
      )
  })
  
  # Render plot for hitter scatter plot
  output$hit_scatter_plot <- renderPlot({
    selected_hitter <- selected_hitter_data()
    
    ggplot(data = selected_hitter, aes(x = ExitSpeed, y = Angle, color = PlayResult)) +
      geom_point(size = 3, na.rm = TRUE) + 
      labs(x = "Exit Velocity", y = "Launch Angle", color = "Play Result", title = "Hitter Scatter Plot") +
      theme_bw() +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.ticks = element_line(size = 0.5),
        panel.grid.major = element_line(color = "grey80", size = 0.2)
      )
  })
  
  # Render DataTable for hitter data
  output$hitter_table <- DT::renderDT({
    selected_hitter <- selected_hitter_data()
    
    # Calculate averages
    avg_exit_speed <- mean(selected_hitter$ExitSpeed, na.rm = TRUE)
    avg_launch_angle <- mean(selected_hitter$Angle, na.rm = TRUE)
    
    # Create a DataTable
    DT::datatable(
      cbind(
        Exit_Velo = round(selected_hitter$ExitSpeed),
        Launch_Angle = round(selected_hitter$Angle),
        Distance_Hit = round(selected_hitter$Distance),
        Pitch_Hit = selected_hitter$TaggedPitchType,
        Velo_Hit = round(selected_hitter$RelSpeed),
        Result = selected_hitter$PlayResult
        
      ),
      rownames = FALSE,
      options = list(
        dom = 't',
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE
      ),
      caption = tags$caption(
        HTML(paste(
          "<span style='font-size: 18px; color: black;'><b>Hitter Averages</b> - ",
          "<b style='font-weight: bold;'>Exit Velo:</b>",
          "<span style='font-weight: bold;'>", round(avg_exit_speed), "mph", "</span>",
          "<b style='font-weight: bold;'> Launch Angle:</b>",
          "<span style='font-weight: bold;'>", round(avg_launch_angle),"degrees" , "</span></span>"
       
        ))
      ))
  })
  
}
# Create Shiny object
shinyApp(ui = ui, server = server)
