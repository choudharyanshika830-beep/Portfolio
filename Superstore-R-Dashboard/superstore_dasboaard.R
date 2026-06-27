# ============================================================
#   Dataset : Sample - Superstore.csv
#   install.packages(c("shiny","shinydashboard","plotly","ggplot2",
#                      "DT","dplyr","lubridate","forecast","corrplot","scales")

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(DT)
library(dplyr)
library(lubridate)
library(forecast)
library(corrplot)
library(scales)
# ============================================================
# LOAD & CLEAN DATA
data <- read.csv("Sample - Superstore.csv", stringsAsFactors = FALSE)
# Make column names R-safe  ("Sub-Category" -> "Sub.Category", etc.)
names(data) <- make.names(names(data))
# Parse mixed date formats  (some rows use MM-DD-YYYY, others M/D/YYYY)
parse_mixed_dates <- function(x) {
  # try slash format first, then dash format
  d <- as.Date(x, format = "%m/%d/%Y")
  bad <- is.na(d)
  d[bad] <- as.Date(x[bad], format = "%m-%d-%Y")
  d
}
data$Order.Date <- parse_mixed_dates(data$Order.Date)
data$Ship.Date  <- parse_mixed_dates(data$Ship.Date)
# Derived columns
data$Month_Year <- format(data$Order.Date, "%Y-%m")
data$Year       <- format(data$Order.Date, "%Y")
# ============================================================
# UI
ui <- dashboardPage(
  skin = "blue",
  # ---------- HEADER ----------
  dashboardHeader(title = "Superstore Dashboard"),
  # ---------- SIDEBAR ----------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",         tabName = "dashboard",   icon = icon("th-large")),
      menuItem("Sales Analysis",    tabName = "sales",       icon = icon("chart-line")),
      menuItem("Customer Analysis", tabName = "customer",    icon = icon("users")),
      menuItem("Forecasting",       tabName = "forecast",    icon = icon("chart-area")),
      menuItem("Correlation",       tabName = "correlation", icon = icon("project-diagram")),
      menuItem("Data Table",        tabName = "table",       icon = icon("table"))
    ),
    br(),
    tags$hr(style = "border-color:#3c8dbc; margin:5px 10px;"),
    tags$p("FILTERS", style = "color:#aaa; font-size:11px; font-weight:bold; padding-left:15px; margin:5px 0;"),
    # Date range slider
    sliderInput(
      "daterange",
      "Date Range",
      min   = min(data$Order.Date),
      max   = max(data$Order.Date),
      value = c(min(data$Order.Date), max(data$Order.Date)),
      timeFormat = "%b %Y",
      step  = 30            # step in days (~1 month)
    ),
    
    selectInput("region",   "Region",
                choices  = c("All", sort(unique(data$Region))),
                selected = "All"),
    
    selectInput("category", "Category",
                choices  = c("All", sort(unique(data$Category))),
                selected = "All"),
    
    selectInput("segment",  "Segment",
                choices  = c("All", sort(unique(data$Segment))),
                selected = "All"),
    
    br(),
    actionButton("reset_btn", "  Reset Filters",
                 icon  = icon("undo"),
                 style = "margin-left:10px; width:85%; background:#3c8dbc; color:#fff; border:none; border-radius:4px;")
  ),
  # ---------- BODY ----------
  dashboardBody(
    
    tags$head(tags$style(HTML("
      .small-box { border-radius: 10px; }
      .box       { border-radius:  8px; }
      .irs-bar, .irs-bar-edge, .irs-single { background:#3c8dbc !important; border-color:#3c8dbc !important; }
    "))),
    tabItems(
      # ======================================================
      # TAB 1 — DASHBOARD
      # ======================================================
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("salesBox",     width = 3),
                valueBoxOutput("profitBox",    width = 3),
                valueBoxOutput("ordersBox",    width = 3),
                valueBoxOutput("customersBox", width = 3)
              ),
              
              fluidRow(
                box(title = "Monthly Sales Trend",  width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("salesTrend", height = 280)),
                
                box(title = "Profit by Category",   width = 6,
                    status = "success", solidHeader = TRUE,
                    plotlyOutput("profitCategory", height = 280))
              ),
              
              fluidRow(
                box(title = "Region-wise Sales",              width = 6,
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("regionSales", height = 280)),
                
                box(title = "Top 10 Sub-Categories by Sales", width = 6,
                    status = "danger",  solidHeader = TRUE,
                    plotlyOutput("topProducts", height = 280))
              )),
      # ======================================================
      # TAB 2 — SALES ANALYSIS
      # ======================================================
      tabItem(tabName = "sales",
              
              fluidRow(
                box(title = "Discount Impact on Profit", width = 12,
                    status = "danger",  solidHeader = TRUE,
                    plotlyOutput("discountImpact", height = 360))
              ),
              
              fluidRow(
                box(title = "Sales by Ship Mode",       width = 6,
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("shipMode", height = 280)),
                
                box(title = "Yearly Sales by Category", width = 6,
                    status = "success", solidHeader = TRUE,
                    plotlyOutput("yearlySales", height = 280))
              )
      ),
      # TAB 3 — CUSTOMER ANALYSIS
      tabItem(tabName = "customer",
              
              fluidRow(
                box(title = "Sales by Customer Segment", width = 6,
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("segmentPlot", height = 320)),
                
                box(title = "Top 10 Customers by Sales", width = 6,
                    status = "success", solidHeader = TRUE,
                    plotlyOutput("topCustomers", height = 320))
              ),
              
              fluidRow(
                box(title = "Profit by Segment & Category", width = 12,
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("segmentProfit", height = 280))
              )
      ),
      
      # ======================================================
      # TAB 4 — FORECASTING
      # ======================================================
      tabItem(tabName = "forecast",
              
              fluidRow(
                box(title = "12-Month Sales Forecast (Auto-ARIMA)", width = 12,
                    status = "primary", solidHeader = TRUE,
                    plotOutput("forecastPlot", height = 480))
              ),
              
              fluidRow(
                box(width = 12, status = "info",
                    tags$p(style = "color:#555;",
                           icon("info-circle"), " ",
                           "An Auto-ARIMA model is fitted on the monthly aggregated sales
               of the filtered period. Shaded bands = 80% and 95% confidence
               intervals for the next 12 months.")
                )
              )
      ),
      
      # ======================================================
      # TAB 5 — CORRELATION
      # ======================================================
      tabItem(tabName = "correlation",
              
              fluidRow(
                box(title = "Correlation Heatmap — Sales / Profit / Quantity / Discount",
                    width = 12, status = "danger", solidHeader = TRUE,
                    plotOutput("corrPlot", height = 480))
              )
      ),
      
      # ======================================================
      # TAB 6 — DATA TABLE
      # ======================================================
      tabItem(tabName = "table",
              
              fluidRow(
                box(title = "Filtered Dataset", width = 12,
                    status = "primary", solidHeader = TRUE,
                    DTOutput("dataTable"))
              )) )
  ))
# SERVER
# ===========================================================
server <- function(input, output, session) {
  
  # ── Reset button ─────────────────────────────────────────
  observeEvent(input$reset_btn, {
    updateSliderInput(session, "daterange",
                      value = c(min(data$Order.Date), max(data$Order.Date)))
    updateSelectInput(session, "region",   selected = "All")
    updateSelectInput(session, "category", selected = "All")
    updateSelectInput(session, "segment",  selected = "All")
  })
  
  # ── Reactive filtered dataset ────────────────────────────
  filtered_data <- reactive({
    df <- data
    
    # Date range from slider
    df <- df %>% filter(Order.Date >= input$daterange[1],
                        Order.Date <= input$daterange[2])
    
    if (input$region   != "All") df <- df %>% filter(Region   == input$region)
    if (input$category != "All") df <- df %>% filter(Category == input$category)
    if (input$segment  != "All") df <- df %>% filter(Segment  == input$segment)
    
    df
  })
  
  # ── Guard: friendly error when filters return 0 rows ─────
  safe_data <- reactive({
    df <- filtered_data()
    validate(need(nrow(df) > 0,
                  "No data matches the selected filters. Please adjust your selections."))
    df
  })
  
  # ============================================================
  # KPI VALUE BOXES
  # ============================================================
  
  output$salesBox <- renderValueBox({
    valueBox(
      value    = paste0("$", formatC(round(sum(safe_data()$Sales),  0), format = "d", big.mark = ",")),
      subtitle = "Total Sales",
      icon     = icon("dollar-sign"),
      color    = "blue"
    )
  })
  
  output$profitBox <- renderValueBox({
    valueBox(
      value    = paste0("$", formatC(round(sum(safe_data()$Profit), 0), format = "d", big.mark = ",")),
      subtitle = "Total Profit",
      icon     = icon("chart-line"),
      color    = "green"
    )
  })
  
  output$ordersBox <- renderValueBox({
    valueBox(
      value    = formatC(n_distinct(safe_data()$Order.ID),    format = "d", big.mark = ","),
      subtitle = "Total Orders",
      icon     = icon("shopping-cart"),
      color    = "yellow"
    )
  })
  
  output$customersBox <- renderValueBox({
    valueBox(
      value    = formatC(n_distinct(safe_data()$Customer.ID), format = "d", big.mark = ","),
      subtitle = "Unique Customers",
      icon     = icon("users"),
      color    = "red"
    )
  })
  
  # ============================================================
  # TAB 1 — DASHBOARD CHARTS
  # ============================================================
  
  output$salesTrend <- renderPlotly({
    trend <- safe_data() %>%
      group_by(Month_Year) %>%
      summarise(Sales = sum(Sales), .groups = "drop") %>%
      arrange(Month_Year)
    
    p <- ggplot(trend, aes(x = Month_Year, y = Sales, group = 1,
                           text = paste0("Month: ", Month_Year,
                                         "<br>Sales: $", formatC(round(Sales,0), format="d", big.mark=",")))) +
      geom_area(fill = "#2196F3", alpha = 0.2) +
      geom_line(color = "#2196F3", linewidth = 1.2) +
      geom_point(color = "#1565C0", size = 1.8) +
      theme_minimal() +
      labs(x = "Month", y = "Sales ($)") +
      theme(axis.text.x = element_text(angle = 90, size = 7))
    
    ggplotly(p, tooltip = "text") %>% layout(hovermode = "x unified")
  })
  
  output$profitCategory <- renderPlotly({
    cat_data <- safe_data() %>%
      group_by(Category) %>%
      summarise(Profit = round(sum(Profit), 0), .groups = "drop")
    
    p <- ggplot(cat_data, aes(x = reorder(Category, Profit), y = Profit,
                              fill = Category,
                              text = paste0(Category, "<br>Profit: $",
                                            formatC(Profit, format="d", big.mark=",")))) +
      geom_col(width = 0.55, show.legend = FALSE) +
      scale_fill_brewer(palette = "Set2") +
      coord_flip() +
      theme_minimal() +
      labs(x = NULL, y = "Profit ($)")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$regionSales <- renderPlotly({
    region_data <- safe_data() %>%
      group_by(Region) %>%
      summarise(Sales = round(sum(Sales), 0), .groups = "drop") %>%
      arrange(desc(Sales))
    
    p <- ggplot(region_data, aes(x = reorder(Region, Sales), y = Sales,
                                 fill = Region,
                                 text = paste0(Region, "<br>Sales: $",
                                               formatC(Sales, format="d", big.mark=",")))) +
      geom_col(width = 0.55, show.legend = FALSE) +
      scale_fill_brewer(palette = "Set1") +
      coord_flip() +
      theme_minimal() +
      labs(x = NULL, y = "Sales ($)")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$topProducts <- renderPlotly({
    prod_data <- safe_data() %>%
      group_by(Sub.Category) %>%
      summarise(Sales = round(sum(Sales), 0), .groups = "drop") %>%
      arrange(desc(Sales)) %>%
      head(10)
    
    p <- ggplot(prod_data, aes(x = reorder(Sub.Category, Sales), y = Sales,
                               fill = Sales,
                               text = paste0(Sub.Category, "<br>Sales: $",
                                             formatC(Sales, format="d", big.mark=",")))) +
      geom_col(width = 0.65, show.legend = FALSE) +
      scale_fill_gradient(low = "#90CAF9", high = "#1565C0") +
      coord_flip() +
      theme_minimal() +
      labs(x = NULL, y = "Sales ($)")
    
    ggplotly(p, tooltip = "text")
  })
  
  # ============================================================
  # TAB 2 — SALES ANALYSIS
  # ============================================================
  
  # ── Discount Impact ──────────────────────────────────────
  output$discountImpact <- renderPlotly({
    df <- safe_data()
    
    plot_df <- data.frame(
      Discount = df$Discount,
      Profit   = df$Profit,
      Category = df$Category
    )
    
    plot_ly(
      data      = plot_df,
      x         = ~Discount,
      y         = ~Profit,
      color     = ~Category,
      colors    = c("Furniture" = "#E53935",
                    "Office Supplies" = "#43A047",
                    "Technology" = "#1E88E5"),
      type      = "scatter",
      mode      = "markers",
      marker    = list(size = 5, opacity = 0.55),
      text      = ~paste0("Discount: ", round(Discount * 100, 0), "%",
                          "<br>Profit: $", round(Profit, 0),
                          "<br>Category: ", Category),
      hoverinfo = "text"
    ) %>%
      add_lines(
        x    = c(0, max(plot_df$Discount, na.rm = TRUE)),
        y    = c(0, 0),
        line = list(color = "red", dash = "dash", width = 1.5),
        name = "Break-even",
        inherit = FALSE,
        showlegend = TRUE
      ) %>%
      layout(
        xaxis     = list(title = "Discount Rate", tickformat = ".0%"),
        yaxis     = list(title = "Profit ($)",    tickformat = "$,.0f"),
        hovermode = "closest"
      )
  })
  
  output$shipMode <- renderPlotly({
    ship_data <- safe_data() %>%
      group_by(Ship.Mode) %>%
      summarise(Sales  = round(sum(Sales), 0),
                Orders = n_distinct(Order.ID), .groups = "drop")
    
    p <- ggplot(ship_data, aes(x = reorder(Ship.Mode, Sales), y = Sales,
                               fill = Ship.Mode,
                               text = paste0(Ship.Mode,
                                             "<br>Sales: $",  formatC(Sales, format="d", big.mark=","),
                                             "<br>Orders: ", Orders))) +
      geom_col(width = 0.55, show.legend = FALSE) +
      scale_fill_brewer(palette = "Paired") +
      coord_flip() +
      theme_minimal() +
      labs(x = NULL, y = "Sales ($)")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$yearlySales <- renderPlotly({
    yr_data <- safe_data() %>%
      group_by(Year, Category) %>%
      summarise(Sales = round(sum(Sales), 0), .groups = "drop")
    
    p <- ggplot(yr_data, aes(x = Year, y = Sales, fill = Category,
                             text = paste0(Category, " — ", Year,
                                           "<br>Sales: $", formatC(Sales, format="d", big.mark=",")))) +
      geom_col(position = "dodge", width = 0.65) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      labs(x = "Year", y = "Sales ($)", fill = "Category")
    
    ggplotly(p, tooltip = "text")
  })
  
  # ============================================================
  # TAB 3 — CUSTOMER ANALYSIS
  # ============================================================
  
  output$segmentPlot <- renderPlotly({
    seg_data <- safe_data() %>%
      group_by(Segment) %>%
      summarise(Sales = round(sum(Sales), 0), .groups = "drop")
    
    plot_ly(seg_data,
            labels           = ~Segment,
            values           = ~Sales,
            type             = "pie",
            hole             = 0.4,
            textinfo         = "label+percent",
            hovertemplate    = "%{label}<br>Sales: $%{value:,.0f}<extra></extra>") %>%
      layout(showlegend = TRUE,
             legend     = list(orientation = "h"))
  })
  
  output$topCustomers <- renderPlotly({
    cust_data <- safe_data() %>%
      group_by(Customer.Name) %>%
      summarise(Sales = round(sum(Sales), 0), .groups = "drop") %>%
      arrange(desc(Sales)) %>%
      head(10)
    
    p <- ggplot(cust_data, aes(x = reorder(Customer.Name, Sales), y = Sales,
                               fill = Sales,
                               text = paste0(Customer.Name, "<br>Sales: $",
                                             formatC(Sales, format="d", big.mark=",")))) +
      geom_col(width = 0.65, show.legend = FALSE) +
      scale_fill_gradient(low = "#A5D6A7", high = "#1B5E20") +
      coord_flip() +
      theme_minimal() +
      labs(x = NULL, y = "Sales ($)")
    
    ggplotly(p, tooltip = "text")
  })
  
  output$segmentProfit <- renderPlotly({
    seg_prof <- safe_data() %>%
      group_by(Segment, Category) %>%
      summarise(Profit = round(sum(Profit), 0), .groups = "drop")
    
    p <- ggplot(seg_prof, aes(x = Segment, y = Profit, fill = Category,
                              text = paste0(Segment, " — ", Category,
                                            "<br>Profit: $", formatC(Profit, format="d", big.mark=",")))) +
      geom_col(position = "dodge", width = 0.65) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      labs(x = "Segment", y = "Profit ($)", fill = "Category")
    
    ggplotly(p, tooltip = "text")
  })
  
  # ============================================================
  # TAB 4 — FORECASTING
  # ============================================================
  
  output$forecastPlot <- renderPlot({
    fc_data <- safe_data() %>%
      group_by(Month_Year) %>%
      summarise(Sales = sum(Sales), .groups = "drop") %>%
      arrange(Month_Year)
    
    validate(need(nrow(fc_data) >= 12,
                  "Not enough monthly data points to build a forecast (need at least 12 months). Please widen the date range or adjust other filters."))
    
    ts_data <- ts(fc_data$Sales, frequency = 12)
    fit     <- auto.arima(ts_data)
    fc      <- forecast(fit, h = 12)
    
    plot(fc,
         main  = "12-Month Sales Forecast (Auto-ARIMA)",
         xlab  = "Time (months from start of selected period)",
         ylab  = "Sales ($)",
         col   = "steelblue",
         fcol  = "darkblue",
         lwd   = 2)
    grid(col = "lightgrey", lty = "dotted")
  })
  
  # ============================================================
  # TAB 5 — CORRELATION HEATMAP
  # ============================================================
  
  output$corrPlot <- renderPlot({
    num_data    <- safe_data() %>% select(Sales, Quantity, Discount, Profit)
    corr_matrix <- cor(num_data)
    
    corrplot(corr_matrix,
             method      = "color",
             addCoef.col = "black",
             tl.col      = "black",
             tl.cex      = 1.2,
             number.cex  = 1.1,
             col         = colorRampPalette(c("#D32F2F", "white", "#1565C0"))(200),
             mar         = c(0, 0, 1, 0))
  })
  
  # ============================================================
  # TAB 6 — DATA TABLE
  # ============================================================
  
  output$dataTable <- renderDT({
    display <- safe_data() %>%
      select(Order.ID, Order.Date, Customer.Name, Segment,
             Region, State, Category, Sub.Category,
             Sales, Quantity, Discount, Profit) %>%
      arrange(desc(Order.Date))
    
    datatable(
      display,
      rownames   = FALSE,
      filter     = "top",
      extensions = "Buttons",
      options    = list(
        pageLength = 15,
        scrollX    = TRUE,
        dom        = "Bfrtip",
        buttons    = list("csv", "excel", "print")
      )
    ) %>%
      formatCurrency(c("Sales", "Profit"), "$", digits = 2) %>%
      formatPercentage("Discount", digits = 0) %>%
      formatStyle(
        "Profit",
        color      = styleInterval(0, c("red", "darkgreen")),
        fontWeight = "bold"
      )
  })
}
# ============================================================
# RUN
# ============================================================

shinyApp(ui, server)