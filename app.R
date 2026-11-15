
library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(janitor)
library(broom)
library(scales)

data_path <- "dataset_raw.csv"

df_raw <- readr::read_csv(data_path, show_col_types = FALSE)

df <- df_raw %>%
  janitor::clean_names() %>%
  dplyr::rename(
    complaint_type = problem_formerly_complaint_type,
    descriptor = problem_detail_formerly_descriptor
  ) %>%
  dplyr::mutate(
    created_dt = mdy_hms(created_date),
    closed_dt = mdy_hms(closed_date),
    resolution_hours = as.numeric(difftime(closed_dt, created_dt, units = "hours")),
    latitude = round(latitude, 2),
    longitude = round(longitude, 2),
    hour = lubridate::hour(created_dt),
    date = as_date(created_dt),
    month = lubridate::month(created_dt, label = TRUE, abbr = TRUE),
    month_num = lubridate::month(created_dt),
    weekday = lubridate::wday(created_dt, label = TRUE, abbr = FALSE, week_start = 1),
    weekday_num = lubridate::wday(created_dt, week_start = 1),
    is_weekend = if_else(weekday %in% c("Saturday", "Sunday"), 1, 0),
    day_night = if_else(hour >= 20 | hour < 6, "Night", "Day")
  )

daily_counts_base <- df %>%
  dplyr::count(date, name = "complaint_count") %>%
  dplyr::mutate(
    time_index = dplyr::row_number(),
    weekday = lubridate::wday(date, label = TRUE, abbr = FALSE, week_start = 1),
    weekday_num = lubridate::wday(date, week_start = 1),
    month = lubridate::month(date, label = TRUE, abbr = TRUE),
    month_num = lubridate::month(date),
    is_weekend = if_else(weekday %in% c("Saturday", "Sunday"), 1, 0)
  )

median_daily_count <- median(daily_counts_base$complaint_count, na.rm = TRUE)

daily_counts_base <- daily_counts_base %>%
  dplyr::mutate(
    high_complaint_day = if_else(complaint_count > median_daily_count, 1, 0)
  )

descriptor_choices <- sort(unique(df$descriptor))

ui <- dashboardPage(
  dashboardHeader(title = "Manhattan Noise 2025"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("table-columns")),
      menuItem("Visualizations", tabName = "viz", icon = icon("chart-column")),
      menuItem("Regression", tabName = "regression", icon = icon("square-poll-vertical"))
    ),
    hr(),
    selectInput(
      "descriptor_filter",
      "Descriptor",
      choices = c("All" = "All", descriptor_choices),
      selected = "All"
    ),
    radioButtons(
      "week_filter",
      "Day Type",
      choices = c("All", "Weekday", "Weekend"),
      selected = "All"
    ),
    selectInput(
      "plot_type",
      "Visualization",
      choices = c(
        "Daily Trend",
        "Complaints by Hour",
        "Complaints by Weekday",
        "Complaints by Descriptor",
        "Resolution Hours Histogram"
      ),
      selected = "Daily Trend"
    ),
    selectInput(
      "model_type",
      "Regression Model",
      choices = c("Linear Regression", "Logistic Regression"),
      selected = "Linear Regression"
    ),
    actionButton("run_model", "Run Model", icon = icon("play"))
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box h3 {font-size: 24px;}
        .content-wrapper, .right-side {background-color: #f7f7f7;}
        .box {border-radius: 8px;}
      "))
    ),
    tabItems(
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("total_complaints", width = 3),
          valueBoxOutput("avg_daily", width = 3),
          valueBoxOutput("median_resolution", width = 3),
          valueBoxOutput("weekend_pct", width = 3)
        ),
        fluidRow(
          box(
            width = 12,
            title = "Project Summary",
            status = "primary",
            solidHeader = TRUE,
            p("This Shiny dashboard provides a basic interactive view of Manhattan residential noise complaints handled by NYPD."),
            p("Use the sidebar to filter the data, switch among visualizations, and run either a linear regression or logistic regression model.")
            
          )
        )
      ),
      tabItem(
        tabName = "viz",
        fluidRow(
          box(
            width = 12,
            title = "Interactive Visualization",
            status = "primary",
            solidHeader = TRUE,
            plotOutput("main_plot", height = "500px")
          )
        )
      ),
      tabItem(
        tabName = "regression",
        fluidRow(
          box(
            width = 12,
            title = "Regression Output",
            status = "primary",
            solidHeader = TRUE,
            verbatimTextOutput("model_summary"),
            br(),
            tableOutput("model_table"),
            br(),
            htmlOutput("model_interpretation")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  filtered_df <- reactive({
    temp <- df

    if (input$descriptor_filter != "All") {
      temp <- temp %>% dplyr::filter(descriptor == input$descriptor_filter)
    }

    if (input$week_filter == "Weekday") {
      temp <- temp %>% dplyr::filter(is_weekend == 0)
    } else if (input$week_filter == "Weekend") {
      temp <- temp %>% dplyr::filter(is_weekend == 1)
    }

    temp
  })

  filtered_daily_counts <- reactive({
    temp <- filtered_df() %>%
      dplyr::count(date, name = "complaint_count") %>%
      dplyr::mutate(
        time_index = dplyr::row_number(),
        weekday = lubridate::wday(date, label = TRUE, abbr = FALSE, week_start = 1),
        weekday_num = lubridate::wday(date, week_start = 1),
        month = lubridate::month(date, label = TRUE, abbr = TRUE),
        month_num = lubridate::month(date),
        is_weekend = if_else(weekday %in% c("Saturday", "Sunday"), 1, 0)
      )

    if (nrow(temp) > 0) {
      med <- median(temp$complaint_count, na.rm = TRUE)
      temp <- temp %>%
        dplyr::mutate(high_complaint_day = if_else(complaint_count > med, 1, 0))
    } else {
      temp <- temp %>%
        dplyr::mutate(high_complaint_day = integer())
    }

    temp
  })

  output$total_complaints <- renderValueBox({
    valueBox(
      value = comma(nrow(filtered_df())),
      subtitle = "Total Complaints",
      icon = icon("volume-high"),
      color = "blue"
    )
  })

  output$avg_daily <- renderValueBox({
    dc <- filtered_daily_counts()
    avg_val <- if (nrow(dc) > 0) round(mean(dc$complaint_count, na.rm = TRUE), 1) else NA
    valueBox(
      value = avg_val,
      subtitle = "Average Daily Complaints",
      icon = icon("calendar-day"),
      color = "green"
    )
  })

  output$median_resolution <- renderValueBox({
    med_val <- round(median(filtered_df()$resolution_hours, na.rm = TRUE), 2)
    valueBox(
      value = med_val,
      subtitle = "Median Resolution Hours",
      icon = icon("clock"),
      color = "yellow"
    )
  })

  output$weekend_pct <- renderValueBox({
    temp <- filtered_df()
    pct <- round(mean(temp$is_weekend, na.rm = TRUE) * 100, 1)
    valueBox(
      value = paste0(pct, "%"),
      subtitle = "Weekend Complaint Share",
      icon = icon("calendar-week"),
      color = "purple"
    )
  })

  output$main_plot <- renderPlot({
    temp <- filtered_df()

    validate(
      need(nrow(temp) > 0, "No data available for the selected filters.")
    )

    if (input$plot_type == "Daily Trend") {
      plot_data <- filtered_daily_counts()

      ggplot(plot_data, aes(x = date, y = complaint_count)) +
        geom_line(linewidth = 0.8) +
        geom_smooth(se = FALSE, method = "loess", linewidth = 0.9) +
        labs(
          title = "Daily Complaint Trend",
          x = "Date",
          y = "Complaint Count"
        ) +
        theme_minimal()

    } else if (input$plot_type == "Complaints by Hour") {
      plot_data <- temp %>% dplyr::count(hour)

      ggplot(plot_data, aes(x = hour, y = n)) +
        geom_col() +
        scale_x_continuous(breaks = 0:23) +
        labs(
          title = "Complaint Volume by Hour",
          x = "Hour",
          y = "Count"
        ) +
        theme_minimal()

    } else if (input$plot_type == "Complaints by Weekday") {
      plot_data <- temp %>%
        dplyr::count(weekday, weekday_num) %>%
        dplyr::arrange(weekday_num)

      ggplot(plot_data, aes(x = reorder(weekday, weekday_num), y = n)) +
        geom_col() +
        labs(
          title = "Complaint Volume by Weekday",
          x = "Weekday",
          y = "Count"
        ) +
        theme_minimal()

    } else if (input$plot_type == "Complaints by Descriptor") {
      plot_data <- temp %>%
        dplyr::count(descriptor, sort = TRUE)

      ggplot(plot_data, aes(x = reorder(descriptor, n), y = n)) +
        geom_col() +
        coord_flip() +
        labs(
          title = "Complaint Volume by Descriptor",
          x = "Descriptor",
          y = "Count"
        ) +
        theme_minimal()

    } else {
      ggplot(
        temp %>% dplyr::filter(!is.na(resolution_hours)),
        aes(x = resolution_hours)
      ) +
        geom_histogram(bins = 40) +
        labs(
          title = "Distribution of Resolution Hours",
          x = "Resolution Hours",
          y = "Frequency"
        ) +
        theme_minimal()
    }
  })

  model_fit <- eventReactive(input$run_model, {
    dc <- filtered_daily_counts()

    validate(
      need(nrow(dc) > 10, "Not enough data to run the selected model.")
    )

    if (input$model_type == "Linear Regression") {
      lm(
        complaint_count ~ time_index + is_weekend + factor(month),
        data = dc
      )
    } else {
      glm(
        high_complaint_day ~ is_weekend + factor(month),
        family = binomial,
        data = dc
      )
    }
  })

  output$model_summary <- renderPrint({
    model_fit()
  })

  output$model_table <- renderTable({
    model <- model_fit()

    if (input$model_type == "Linear Regression") {
      broom::tidy(model) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))
    } else {
      broom::tidy(model, exponentiate = TRUE, conf.int = TRUE) %>%
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, 4)))
    }
  })

  output$model_interpretation <- renderUI({
    model <- model_fit()

    if (input$model_type == "Linear Regression") {
      coefs <- broom::tidy(model)
      wknd <- coefs %>% dplyr::filter(term == "is_weekend")

      if (nrow(wknd) == 1) {
        HTML(paste0(
          "<b>Interpretation:</b> In the linear regression model, the weekend coefficient is ",
          round(wknd$estimate, 2),
          ". This represents the estimated difference in daily complaint count between weekend and weekday days, holding the other model terms constant."
        ))
      } else {
        HTML("<b>Interpretation:</b> Linear regression output generated.")
      }
    } else {
      coefs <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
      wknd <- coefs %>% dplyr::filter(term == "is_weekend")

      if (nrow(wknd) == 1) {
        HTML(paste0(
          "<b>Interpretation:</b> In the logistic regression model, the weekend odds ratio is ",
          round(wknd$estimate, 2),
          ". Values above 1 indicate higher odds that a day is a high-complaint day on weekends compared with weekdays."
        ))
      } else {
        HTML("<b>Interpretation:</b> Logistic regression output generated.")
      }
    }
  })
}

shinyApp(ui = ui, server = server)
