# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)  # Add this line
library(leaflet)
library(dplyr)
library(hms)  # for handling times
library(ggplot2)  # for plotting
library(shinythemes)  # Add this line
library(lubridate)

# Get the names of all the CSV files in the directory
file_paths <- list.files(path = "data", pattern = "\\.csv$", full.names = TRUE)

# Function to read and process data
read_and_process_data <- function(file_path) {
  data <- read.csv(file_path, stringsAsFactors = FALSE)
  data$date <- as.Date(data$date, format = "%Y-%m-%d")
  data$time <- as_hms(data$time)  # Convert time to hms object
  return(data)
}

# Read and process each file
data_list <- lapply(file_paths, read_and_process_data)

# Extract dates
run_dates <- sapply(data_list, function(data) as.character(data$date[1]))


# UI
ui <- dashboardPage(
  
  dashboardHeader(title = "Runalyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selectInput("date", HTML("Select a run date: <i class='fa fa-calendar'></i>"), choices = run_dates, selected = tail(run_dates, 1))
      ,  # Add icon
      checkboxGroupInput("plots", "Select plots:", 
                         choices = c("Map" = "map", 
                                     "Summary" = "summary", 
                                     "Elevation" = "elevation_plot", 
                                     "Speed" = "speed_plot", 
                                     "Splits" = "pace_plot"), 
                         selected = c("map", "summary","elevation_plot", "speed_plot", "pace_plot")),
      menuItem("History",tabName ="history", icon = icon("history"))  # Add icon
    )
  ),
  dashboardBody(
    #theme = shinytheme("cerulean"),  # Add theme
    useShinyjs(),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 12, tabsetPanel(  # Add tabsetPanel
                  tabPanel("Map and Summary",
                           fluidRow(
                             column(width = 12, leafletOutput("map")),  # Full width for map
                             column(width = 12, tableOutput("summary"))  # Full width for summary
                           )
                  ),
                  tabPanel("Plots",
                           fluidRow(
                             column(width = 12, plotOutput("elevation_plot")),  # Full width for elevation plot
                             column(width = 12, plotOutput("speed_plot")),  # Full width for speed plot
                             column(width = 12, plotOutput("pace_plot"))  # Full width for speed plot
                           )
                  )
                ))
              )
      ),
    
    tabItem(tabName = "history",
            fluidRow(
              box(width = 12, tabsetPanel(  # Add tabsetPanel
                tabPanel("History",
                         fluidRow(column(width = 12, tags$div()),
                          # column(width = 12, leafletOutput("map")),  # Full width for map
                          # column(width = 12, tableOutput("summary"))  # Full width for summary
                          column("Past 7 days", width = 12, tableOutput("weeksummary")),
                          column("Past 31 days", width = 12, tableOutput("monthsummary")),
                          column("Past year", width = 12, tableOutput("yearsummary"))
                         )
                ),
                tabPanel("Custom",
                         fluidRow(
                           # Create a date input for the start date
                           column(6, dateInput("start_date", 
                                               label = "Start date",
                                               value = Sys.Date() - 30)),  # Default value is one month ago
                           
                           # Create a date input for the end date
                           column(6, dateInput("end_date", 
                                               label = "End date",
                                               value = Sys.Date()))  # Default value is today
                         ),
                         fluidRow(
                            tableOutput("customsummary")
                         ),
                         fluidRow(
                           
                           # Create a plot output
                           plotOutput("bar_plot"),
                           # Create a select input for the statistic to plot
                           selectInput("stat", 
                                       label = "Select a statistic to plot:", 
                                       choices = c("Total Time", "Total Distance", "Total Elevation Gain"))
                         )
                )
              ))
            )
            )
  )
)
)


# Server
server <- function(input, output) {
  # Your functions here...
  
 
  
  # Function to plot a run
  plot_run <- function(data) {
    # Calculate the time difference between consecutive data points
    data <- data %>%
      arrange(date, time) %>%
      mutate(time_diff = c(0, diff(time)))
    
    # Define a threshold for detecting pauses (e.g., 1 minute)
    pause_threshold <- as.numeric(hms::as_hms("00:01:00"))
    
    # Identify the points where a pause occurred
    pauses <- which(data$time_diff > pause_threshold)
    
    # Create a leaflet map
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addPolylines(lng = data$lng, lat = data$lat, color = "red")  # Add the run path
    
    # Add polylines for the pauses
    if (length(pauses) > 0) {
      for (i in pauses) {
        m <- m %>%
          addPolylines(lng = data$lng[(i-1):(i)], lat = data$lat[(i-1):(i)], color = "white")
      }
    }
    
    # Print the map
    print(m)
  }
  
  get_summary_stats <- function(data) {
    # Calculate the time difference between consecutive data points
    data <- data %>%
      arrange(date, time) %>%
      mutate(time_diff = c(0, diff(time)))
    
    # Define a threshold for detecting pauses (e.g., 1 minute)
    pause_threshold <- as.numeric(hms::as_hms("00:01:00"))
    
    # Create a new group variable that increments each time a pause is detected
    data <- data %>%
      mutate(pause = cumsum(time_diff > pause_threshold)) %>%
      group_by(date, pause)
    
    # Now calculate the statistics within each group
    data %>%
      summarise(
        total_distance_km = sum(sqrt(diff(lng)^2 + diff(lat)^2)) * 111.1,  # Convert degrees to kilometers
        total_time_h = as.numeric(hms::as_hms(max(time) - min(time))) / 3600,  # Convert time to hours
        total_ascent = sum(diff(elevation)[diff(elevation) > 0]),
        total_descent = sum(diff(elevation)[diff(elevation) < 0]),
        .groups = "drop"  # Drop the grouping
      ) %>%
      summarise(
        total_distance = paste(round(sum(total_distance_km), digits = 2),"km"),
        total_time = format(hms::as_hms(floor(sum(total_time_h) * 3600))),  # Convert time to hms format
        total_ascent = paste(round(sum(total_ascent), digits = 2),"m"),
        total_descent = paste(round(sum(total_descent), digits = 2),"m"),
        avg_pace = paste(sub("\\d{4}$", "",sub("^00:", "", (hms::as_hms((sum(total_time_h) / sum(total_distance_km)) * 3600)))),"/km"),
        avg_speed = paste(round(sum(total_distance_km) / sum(total_time_h), digits = 2), "km/h"),
        .groups = "drop"  # Drop the grouping
      )
  }
  
  # Function to calculate and plot pace at each instance of the run
  plot_split <- function(data) {
    # Calculate the distance and time between each pair of consecutive points
    # Calculate the cumulative distance at each point
    distances <- cumsum(sqrt(diff(data$lng)^2 + diff(data$lat)^2) * 111.1)  # Convert degrees to kilometers
    
    # Find the times at which each km is completed
    km_indices <- sapply(1:max(floor(distances)), function(km) {
      which.max(distances >= km)
    })
    
    # Adjust km_times to start from 0
    km_times <- data$time[km_indices] - min(data$time)
    
    # Calculate the split times
    split_times <- c(km_times[1], diff(km_times))  # Time in seconds
    
    # Convert split times to minutes per km
    split_times <- split_times / 60  # Convert time to numeric minutes
    
    # Create a data frame for plotting
    plot_data <- data.frame(time = lubridate::as_datetime(data$time), split_time = NA)
    plot_data$split_time[km_indices] <- split_times
    
    # Create a plot of the km splits
    plot(plot_data$time, plot_data$split_time, type = "h", xlab = "Time", ylab = "Split Time (min per km)", main = "Distance Splits")
    
    # Add shaded regions for pauses
    pauses <- which(diff(data$time) > as.difftime(1, units = "mins"))
    for (i in pauses) {
      rect(data$time[i], par("usr")[3], data$time[i + 1], par("usr")[4], col = rgb(0.5, 0.5, 0.5, alpha = 0.5), border = NA)
    }
  }
  
  # Function to calculate and plot speed at each instance of the run
  plot_instant_speed <- function(data) {
    # Calculate the distance and time between each pair of consecutive points
    distances <- sqrt(diff(data$lng)^2 + diff(data$lat)^2) * 111.1  # Convert degrees to kilometers
    times <- as.numeric(diff(data$time))/3600#, units = "hours")  # Convert time to numeric hours
    
    # Calculate the pace (time per km) at each instance
    instant_speed <- distances / times
    
    # Convert numeric hours back to POSIXct format for plotting
    times_posix <- lubridate::as_datetime(data$time[-1])
    
    # Create a plot of the instant pace
    plot(times_posix, instant_speed, type = "l", xlab = "Time", ylab = "Speed (km per hours)", main = "Instant Speed")
    
    # Add shaded regions for pauses
    pauses <- which(diff(data$time) > as.difftime(1, units = "mins"))
    for (i in pauses) {
      rect(data$time[i], par("usr")[3], data$time[i + 1], par("usr")[4], col = rgb(0.5, 0.5, 0.5, alpha = 0.5), border = NA)
    }
  }
  
  # Function to plot elevation over time
  plot_elevation <- function(data) {
    # Convert numeric hours back to POSIXct format for plotting
    times_posix <- lubridate::as_datetime(data$time)
    
    # Create a plot of the elevation
    plot(times_posix, data$elevation, type = "l", xlab = "Time", ylab = "Elevation (m)", main = "Elevation over Time")
    
    # Add shaded regions for pauses
    pauses <- which(diff(data$time) > as.difftime(1, units = "mins"))
    for (i in pauses) {
      rect(data$time[i], par("usr")[3], data$time[i + 1], par("usr")[4], col = rgb(0.5, 0.5, 0.5, alpha = 0.5), border = NA)
    }
  }
  
  
  
  
  
  
  
  # Function to aggregate data over a specified time period
  aggregate_data <- function(data_list, start_date, end_date) {
    # Filter the data_list to only include data within the specified date range
    filtered_data_list <- lapply(data_list, function(data) data[data$date >= start_date & data$date <= end_date, ])
    
    # Combine all the filtered data into one data frame
    aggregated_data <- do.call(rbind, filtered_data_list)
    
    return(aggregated_data)
  }
  
  # Function to get summary statistics for a specified time period
  get_summary_stats_for_period <- function(data_list, start_date, end_date) {
    # Aggregate the data over the specified time period
    aggregated_data <- aggregate_data(data_list, start_date, end_date)
    
    # Get the summary statistics for the aggregated data
    summary_stats <- get_summary_stats(aggregated_data)
    
    return(summary_stats)
  }
  
  # Now you can use these functions to get the summary statistics for the past 7 days, MTD, and YTD
  # For example, to get the summary statistics for the past 7 days:
  #start_date <- Sys.Date() - 300
  #end_date <- Sys.Date()
  #summary_stats_past_7_days <- get_summary_stats_for_period(data_list, start_date, end_date) 
  
  
  
  
  
  
  
  
  
  

  
  
  observe({
    # Find the index of the selected date
    index <- which(run_dates == input$date)
    
    
    
    #start_date <- as.Date(input$date)
    #end_date <- as.Date(input$date)
    
    
    data <- data_list[[index]]
    
    output$map <- renderLeaflet({
      plot_run(data)
    })
    outputOptions(output, "map", suspendWhenHidden = T)
    shinyjs::toggle(id = "map", condition = "map" %in% input$plots)
    
    output$summary <- renderTable({
      get_summary_stats(data)
    })
    outputOptions(output, "summary", suspendWhenHidden = T)
    shinyjs::toggle(id = "summary", condition = "summary" %in% input$plots)
    
    output$pace_plot <- renderPlot({
      plot_split(data)
    })
    outputOptions(output, "pace_plot", suspendWhenHidden = T)
    shinyjs::toggle(id = "pace_plot", condition = "pace_plot" %in% input$plots)
    
    output$speed_plot <- renderPlot({
      plot_instant_speed(data)
    })
    outputOptions(output, "speed_plot", suspendWhenHidden = T)
    shinyjs::toggle(id = "speed_plot", condition = "speed_plot" %in% input$plots)
    
    output$elevation_plot <- renderPlot({
      plot_elevation(data)
    })
    outputOptions(output, "elevation_plot", suspendWhenHidden = T)
    shinyjs::toggle(id = "elevation_plot", condition = "elevation_plot" %in% input$plots)
    
    output$weeksummary <- renderTable({
      get_summary_stats_for_period(data_list, as.Date(input$date) - 7, as.Date(input$date))
    })
    
    output$monthsummary <- renderTable({
      get_summary_stats_for_period(data_list, as.Date(input$date) - 31, as.Date(input$date))
    })
    
    output$yearsummary <- renderTable({
      get_summary_stats_for_period(data_list, as.Date(input$date) - 365, as.Date(input$date))
    })
    
    output$customsummary <- renderTable({
      get_summary_stats_for_period(data_list, as.Date(input$start_date), as.Date(input$end_date))
    })
    
    
    # Render the bar plot
    output$bar_plot <- renderPlot({
      
      
      # Define the mapping from input$stat options to column names
      stat_mapping <- c("Total Time" = "total_time_h", 
                        "Total Distance" = "total_distance_km", 
                        "Total Elevation Gain" = "total_ascent")
      
      # Aggregate the data over the specified time period
      aggregate_data(data_list, as.Date(input$start_date), as.Date(input$end_date)) %>%
        # Calculate the time difference between consecutive data points
        
        # Calculate the time difference between consecutive data points
        arrange(date, time) %>%
        mutate(time_diff = c(0, diff(time))) %>%
        # Define a threshold for detecting pauses (e.g., 1 minute)
        mutate(pause = cumsum(time_diff > as.numeric(hms::as_hms("00:01:00")))) %>%
        # Group by date and pause
        group_by(date, pause) %>%
        # Calculate the summary statistics for each run
        summarise(
          total_distance_km = sum(sqrt(diff(lng)^2 + diff(lat)^2)) * 111.1,  # Convert degrees to kilometers
          total_time_h = as.numeric(hms::as_hms(max(time) - min(time))) / 3600,  # Convert time to hours
          total_ascent = sum(diff(elevation)[diff(elevation) > 0]),
          total_descent = sum(diff(elevation)[diff(elevation) < 0]),
          .groups = "drop"  # Drop the grouping
        ) %>%
        # Group by week or day
        {if (as.Date(input$end_date) - as.Date(input$start_date) <= 31) mutate(., x = date) else mutate(., x = as.Date(cut(date, "week")))} %>%
        group_by(x) %>%
        # Summarise the statistics for each week or day
        summarise(
          total_distance_km = sum(total_distance_km),
          total_time_h = sum(total_time_h),
          total_ascent = sum(total_ascent),
          total_descent = sum(total_descent),
          .groups = "drop"  # Drop the grouping
        ) %>%
        
        # Select the statistic to plot
        ggplot(aes(x = x, y = get(stat_mapping[input$stat]))) +
        geom_bar(stat = "identity") +
        labs(y = input$stat) -> p  # Store the plot in p
      
      # Format the x-axis labels based on the date range
      if (as.Date(input$end_date) - as.Date(input$start_date) <= 31) {  # If the date range is less than or equal to 7 month
        p <- p + scale_x_date(date_breaks = "1 day", date_labels = "%d %b")  # Use a date break of 1 week
      }else if (as.Date(input$end_date) - as.Date(input$start_date) <= 200) {  # If the date range is less than or equal to 7 month
        p <- p + scale_x_date(date_breaks = "1 week", date_labels = "%d %b")  # Use a date break of 1 week
      } else if (as.Date(input$end_date) - as.Date(input$start_date) <= 400) {  # If the date range is less than or equal to 14 months
        p <- p + scale_x_date(date_breaks = "2 week", date_labels = "%d %b")  # Use a date break of 2 weeks
      } else {  # If the date range is more than 7 months
        p <- p + scale_x_date(date_breaks = "4 week", date_labels = "%d %b")  # Use a date break of 1 month
      }
      
      # Adding vertical line at the beginning of to the year
      p <- p + geom_vline(xintercept = as.numeric(as.Date(paste0(2021:2025, "-01-01"))), linetype = "dashed", color = "grey50")
      # Adding labels to the x and y axes
      p <- p + labs(x = "Date", y = input$stat) + ggtitle(paste("Bar plot of", input$stat)) +
        theme(plot.title = element_text(hjust = 0.5))  # Center the title
      
      # Print the plot
      print(p)
    })

    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
