library(shiny)
library(stringr)
library(dplyr)
library(DT)
library(openxlsx)

# ── PARSER ─────────────────────────────────────
parse_sastaticket <- function(raw_text) {

  txt <- str_replace_all(raw_text, "\r", "\n")
  txt <- str_replace_all(txt, "\n+", "\n")

  flight_rx <- "\\b(?:PA|PK|PF|9P|ER)-\\d{3,}\\b"

  lines <- unlist(str_split(txt, "\n"))
  lines <- str_trim(lines)
  lines <- lines[lines != ""]

  # ── Extract search date from header ──────────
  search_date <- NA_character_

  # Try "27th Apr", "25th Apr", "1 May" etc.
  date_rx <- "(\\d{1,2})(?:st|nd|rd|th)?\\s+(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)(?:\\s+(\\d{4}))?"
  for (line in head(lines, 20)) {
    m <- str_match(line, date_rx)
    if (!is.na(m[1,1])) {
      day   <- m[1,2]
      month <- m[1,3]
      year  <- ifelse(is.na(m[1,4]), format(Sys.Date(), "%Y"), m[1,4])
      search_date <- paste(day, month, year)
      break
    }
  }

  # ── Extract route ─────────────────────────────
  route_from <- NA_character_
  route_to   <- NA_character_
  route_rx   <- "([A-Za-z]+)\\s*(?:to|→|-)\\s*([A-Za-z]+)"
  for (line in head(lines, 20)) {
    m <- str_match(line, route_rx)
    if (!is.na(m[1,1])) {
      route_from <- str_trim(m[1,2])
      route_to   <- str_trim(m[1,3])
      break
    }
  }

  # ── Extract date strip (Sat, 25 Apr → PKR 15,050 etc.) ──
  date_prices <- list()
  date_strip_rx <- "(Mon|Tue|Wed|Thu|Fri|Sat|Sun),\\s+(\\d{1,2})\\s+(Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec)"
  for (i in seq_along(lines)) {
    m <- str_match(lines[i], date_strip_rx)
    if (!is.na(m[1,1])) {
      label <- paste0(m[1,2], " ", m[1,3], " ", m[1,4])
      # look ahead for PKR price
      for (j in (i+1):min(i+3, length(lines))) {
        p <- str_extract(lines[j], "PKR\\s*[\\d,]+")
        if (!is.na(p)) {
          date_prices[[label]] <- as.numeric(str_remove_all(p, "[^0-9]"))
          break
        }
      }
    }
  }

  # ── Parse individual flights ──────────────────
  flights <- list()
  current <- NULL

  for (line in lines) {

    if (str_detect(line, flight_rx)) {
      if (!is.null(current)) flights[[length(flights) + 1]] <- current

      airline <- case_when(
        str_detect(line, "PA-") ~ "Airblue",
        str_detect(line, "PK-") ~ "PIA",
        str_detect(line, "9P-") ~ "Fly Jinnah",
        str_detect(line, "PF-") ~ "Air Sial",
        str_detect(line, "ER-") ~ "Serene Air",
        TRUE                     ~ "Unknown"
      )

      current <- list(
        airline   = airline,
        flight_no = str_extract(line, flight_rx),
        dep_time  = NA,
        arr_time  = NA,
        duration  = NA,
        price     = NA,
        stops     = "Nonstop",
        meal      = FALSE
      )
      next
    }

    if (!is.null(current)) {

      # Times + duration — SastaTicket often runs them together on one line
      # e.g. "12:15 PM2h 5m02:20 PM" or separate lines
      if (is.na(current$dep_time) && str_detect(line, "\\d{1,2}:\\d{2}\\s*[AP]M")) {
        times <- str_extract_all(line, "\\d{1,2}:\\d{2}\\s*[AP]M")[[1]]
        if (length(times) >= 1) current$dep_time <- str_trim(times[1])
        if (length(times) >= 2) current$arr_time <- str_trim(times[length(times)])
      }

      # Duration — grab even from same line as times
      if (is.na(current$duration) && str_detect(line, "\\d+h\\s*\\d*m?")) {
        dur <- str_extract(line, "\\d+h\\s*\\d*m?")
        if (!is.na(dur)) current$duration <- str_trim(dur)
      }

      # Arrival time fallback — sometimes on its own line after dep+duration
      if (!is.na(current$dep_time) && is.na(current$arr_time) &&
          str_detect(line, "\\d{1,2}:\\d{2}\\s*[AP]M") &&
          !str_detect(line, current$dep_time)) {
        t <- str_extract(line, "\\d{1,2}:\\d{2}\\s*[AP]M")
        if (!is.na(t)) current$arr_time <- str_trim(t)
      }

      # Price — skip if line looks like a date-strip entry (has day name)
      if (is.na(current$price) && str_detect(line, "PKR") &&
          !str_detect(line, "(Mon|Tue|Wed|Thu|Fri|Sat|Sun),")) {
        price <- str_extract(line, "PKR\\s*[\\d,]+")
        if (!is.na(price)) current$price <- as.numeric(str_remove_all(price, "[^0-9]"))
      }

      # Stops
      if (str_detect(line, "Nonstop|Non-stop")) current$stops <- "Nonstop"
      else if (str_detect(line, "1 Stop"))       current$stops <- "1 Stop"
      else if (str_detect(line, "2 Stop"))       current$stops <- "2 Stops"

      # Meal
      if (str_detect(line, "(?i)meal")) current$meal <- TRUE
    }
  }

  if (!is.null(current)) flights[[length(flights) + 1]] <- current

  if (length(flights) == 0) return(NULL)

  df <- bind_rows(lapply(flights, as.data.frame))

  df <- df %>%
    filter(!is.na(price)) %>%
    filter(price >= 5000, price <= 500000) %>%
    distinct(flight_no, dep_time, .keep_all = TRUE) %>%
    arrange(price)

  list(
    flights      = df,
    search_date  = search_date,
    route_from   = route_from,
    route_to     = route_to,
    date_prices  = date_prices
  )
}

# ── TIME HELPERS ───────────────────────────────
parse_time <- function(t) as.POSIXct(t, format = "%I:%M %p")

split_time_ranges <- function(df) {
  df$time_obj <- parse_time(df$dep_time)
  hr <- as.numeric(format(df$time_obj, "%H"))
  list(
    morning   = df[!is.na(hr) & hr >= 6  & hr < 12, ],
    afternoon = df[!is.na(hr) & hr >= 12 & hr < 18, ],
    evening   = df[!is.na(hr) & hr >= 18 & hr < 24, ],
    night     = df[!is.na(hr) & hr >= 0  & hr < 6, ]
  )
}

# ── THEME ──────────────────────────────────────
sasta_blue   <- "#1a56a0"
sasta_orange <- "#f97316"



# ── UI ─────────────────────────────────────────
ui <- fluidPage(

  tags$head(tags$style(HTML(paste0("
    body { font-family: 'Segoe UI', sans-serif; background:#f1f5f9; margin:0; }
    .sasta-header {
      background:", sasta_blue, ";
      color: white;
      padding: 14px 20px;
      border-radius: 0 0 12px 12px;
      margin-bottom: 16px;
    }
    .sasta-logo { font-size:22px; font-weight:600; }
    .sasta-logo span { color:", sasta_orange, "; }
    .sasta-route { font-size:14px; opacity:0.85; margin-top:4px; }
    .sasta-date-badge {
      background:rgba(255,255,255,0.2);
      display:inline-block;
      padding:3px 12px;
      border-radius:99px;
      font-size:13px;
      margin-top:6px;
    }
    .date-strip {
      display:flex; gap:0; overflow-x:auto;
      background:", sasta_blue, ";
      border-radius:8px;
      margin-bottom:12px;
    }
    .date-pill {
      padding:8px 16px; text-align:center;
      color:white; font-size:12px; flex-shrink:0;
      border-bottom: 3px solid transparent;
    }
    .section-title { font-size:15px; font-weight:600; color:#1e293b; margin:14px 0 8px; }
    .summary-box {
      background:#e8f5e9; border-left:4px solid #2e7d32;
      padding:10px 14px; border-radius:6px;
      font-size:14px; margin-bottom:10px;
    }
    .stops-box {
      background:#fff3e0; border-left:4px solid #f57c00;
      padding:10px 14px; border-radius:6px;
    }
    .time-section { font-size:13px; font-weight:600; color:#475569; margin:8px 0 4px; }
    .btn-parse {
      background:", sasta_orange, "; color:white; border:none;
      padding:8px 20px; border-radius:6px; font-size:14px;
      cursor:pointer; margin-right:8px;
    }
    .btn-clear { background:#e2e8f0; border:none; padding:8px 16px; border-radius:6px; }
    textarea { border-radius:8px !important; border:1px solid #cbd5e1 !important; }
  ")))),

  # Header
  div(class = "sasta-header",
    div(class = "sasta-logo", "Sasta", tags$span("Ticket"), " ✈ Flight Parser"),
    uiOutput("route_display")
  ),

  div(style = "max-width:960px; margin:0 auto; padding:0 12px;",

    # Input area
    div(style = "background:#fff; border:1px solid #e2e8f0; border-radius:10px; padding:14px 16px; margin-bottom:10px;",
      tags$label("Paste SastaTicket page text here", style="font-weight:600; font-size:14px; display:block; margin-bottom:6px;"),
      tags$textarea(
        id = "raw_text",
        placeholder = "Copy the full SastaTicket results page and paste it here…",
        rows = 12,
        style = "width:100%; padding:10px; border:1px solid #cbd5e1; border-radius:8px; font-size:13px; font-family:monospace; resize:vertical; box-sizing:border-box;"
      ),
      br(),
      actionButton("go",    "Parse Flights", class = "btn-parse"),
      actionButton("clear", "Clear",         class = "btn-clear"),
      downloadButton("download_excel", "⬇ Excel", style = "margin-left:8px;")
    ),

    uiOutput("error_msg"),

    # Date strip
    uiOutput("date_strip_ui"),

    # Summary
    uiOutput("summary"),

    # Main table
    uiOutput("main_table_ui"),

    # Time boxes
    uiOutput("time_boxes"),

    # Stops section
    uiOutput("stops_table")
  )
)

# ── SERVER ─────────────────────────────────────
server <- function(input, output, session) {

  parsed_r  <- reactiveVal(NULL)
  err_r     <- reactiveVal("")

  observeEvent(input$go, {
    raw <- str_trim(input$raw_text)
    if (nchar(raw) < 50) {
      err_r("⚠ Please paste full SastaTicket page text.")
      parsed_r(NULL); return()
    }
    result <- parse_sastaticket(raw)
    if (is.null(result) || nrow(result$flights) == 0) {
      err_r("❌ No flights found. Make sure you copied the full page.")
      parsed_r(NULL)
    } else {
      err_r("")
      parsed_r(result)
    }
  })

  observeEvent(input$clear, {
    updateTextAreaInput(session, "raw_text", value = "")
    parsed_r(NULL); err_r("")
  })

  # Helpers
  flights_r  <- reactive({ p <- parsed_r(); if (is.null(p)) NULL else p$flights })
  nonstop_r  <- reactive({ df <- flights_r(); if (is.null(df)) NULL else filter(df, stops == "Nonstop") })
  stops_r    <- reactive({ df <- flights_r(); if (is.null(df)) NULL else filter(df, stops != "Nonstop") })

  # Route display in header
  output$route_display <- renderUI({
    p <- parsed_r()
    if (is.null(p)) return(div(class="sasta-route", "Paste your SastaTicket data to get started"))

    from <- if (!is.na(p$route_from)) p$route_from else "KHI"
    to   <- if (!is.na(p$route_to))   p$route_to   else "ISB"
    date_txt <- if (!is.na(p$search_date)) p$search_date else "—"

    tagList(
      div(class="sasta-route", paste(from, "→", to, "  |  One Way")),
      div(class="sasta-date-badge", paste("✈ Searching flights for:", date_txt))
    )
  })

  output$error_msg <- renderUI({
    if (err_r() != "") div(style="color:#dc2626;padding:8px;", err_r())
  })

  # Date strip from parsed header
  output$date_strip_ui <- renderUI({
    p <- parsed_r()
    if (is.null(p) || length(p$date_prices) == 0) return(NULL)

    pills <- lapply(names(p$date_prices), function(d) {
      div(class="date-pill",
          div(style="opacity:0.7;", d),
          div(style="font-weight:600;", paste0("PKR ", format(p$date_prices[[d]], big.mark=",")))
      )
    })

    tagList(
      div(class="section-title", "Price Calendar"),
      div(class="date-strip", pills)
    )
  })

  # Summary
  output$summary <- renderUI({
    df <- nonstop_r()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    p         <- parsed_r()
    date_txt  <- if (!is.na(p$search_date)) p$search_date else "selected date"
    min_price <- min(df$price)
    cheapest  <- df %>% filter(price == min_price) %>% slice(1)

    div(class="summary-box",
      tags$b(paste0("Best deal for ", date_txt, ":")),
      paste0(" ", cheapest$airline, " ", cheapest$flight_no,
             " @ PKR ", format(min_price, big.mark=","),
             " (dep. ", cheapest$dep_time, ")",
             if (cheapest$meal) " — includes meal" else "")
    )
  })

  # Main nonstop table
  output$main_table_ui <- renderUI({
    df <- nonstop_r()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    tagList(div(class="section-title", "All Nonstop Flights"), DTOutput("table"))
  })

  output$table <- renderDT({
    df <- nonstop_r()
    if (is.null(df)) return(NULL)
    df %>%
      mutate(
        meal  = ifelse(meal, "✓ Meal", ""),
        price = paste0("PKR ", format(price, big.mark=","))
      ) %>%
      select(Airline=airline, Flight=flight_no,
             Departure=dep_time, Arrival=arr_time,
             Duration=duration, Price=price, Meal=meal) %>%
      datatable(
        options = list(pageLength=15, dom="ftp", ordering=TRUE),
        rownames = FALSE,
        class = "compact stripe"
      )
  })

  # Time-of-day boxes
  output$time_boxes <- renderUI({
    df <- nonstop_r()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    ranges <- split_time_ranges(df)
    labels <- list(
      morning   = "🌅 Morning (6 AM – 12 PM)",
      afternoon = "🌞 Afternoon (12 PM – 6 PM)",
      evening   = "🌆 Evening (6 PM – 12 AM)",
      night     = "🌙 Night (12 AM – 6 AM)"
    )

    boxes <- lapply(names(labels), function(period) {
      d <- ranges[[period]]
      if (nrow(d) == 0) return(NULL)
      d <- d %>%
        arrange(price) %>%
        mutate(price = paste0("PKR ", format(price, big.mark=","))) %>%
        select(Airline=airline, Flight=flight_no, Dep=dep_time, Price=price)

      column(6,
        div(style="margin-bottom:12px;",
          div(class="time-section", labels[[period]]),
          DTOutput(paste0("tbl_", period))
        )
      )
    })

    output$tbl_morning   <- renderDT({ d <- ranges$morning;   if(nrow(d)==0) return(NULL); d %>% arrange(price) %>% mutate(price=paste0("PKR ",format(price,big.mark=","))) %>% select(Airline=airline,Flight=flight_no,Dep=dep_time,Price=price) %>% datatable(options=list(dom='t',pageLength=20),rownames=FALSE,class="compact") })
    output$tbl_afternoon <- renderDT({ d <- ranges$afternoon; if(nrow(d)==0) return(NULL); d %>% arrange(price) %>% mutate(price=paste0("PKR ",format(price,big.mark=","))) %>% select(Airline=airline,Flight=flight_no,Dep=dep_time,Price=price) %>% datatable(options=list(dom='t',pageLength=20),rownames=FALSE,class="compact") })
    output$tbl_evening   <- renderDT({ d <- ranges$evening;   if(nrow(d)==0) return(NULL); d %>% arrange(price) %>% mutate(price=paste0("PKR ",format(price,big.mark=","))) %>% select(Airline=airline,Flight=flight_no,Dep=dep_time,Price=price) %>% datatable(options=list(dom='t',pageLength=20),rownames=FALSE,class="compact") })
    output$tbl_night     <- renderDT({ d <- ranges$night;     if(nrow(d)==0) return(NULL); d %>% arrange(price) %>% mutate(price=paste0("PKR ",format(price,big.mark=","))) %>% select(Airline=airline,Flight=flight_no,Dep=dep_time,Price=price) %>% datatable(options=list(dom='t',pageLength=20),rownames=FALSE,class="compact") })

    tagList(div(class="section-title", "Flights by Time of Day"), fluidRow(boxes))
  })

  # Stops section
  output$stops_table <- renderUI({
    df <- stops_r()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    tagList(
      div(class="section-title", "Flights with Stops"),
      div(class="stops-box",
        DTOutput("tbl_stops")
      )
    )
  })

  output$tbl_stops <- renderDT({
    df <- stops_r()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df %>%
      mutate(price = paste0("PKR ", format(price, big.mark=","))) %>%
      select(Airline=airline, Flight=flight_no,
             Departure=dep_time, Duration=duration,
             Stops=stops, Price=price) %>%
      datatable(options=list(dom='t', pageLength=20), rownames=FALSE, class="compact")
  })

  # Excel download
  output$download_excel <- downloadHandler(
    filename = function() {
      p <- parsed_r()
      date_part <- if (!is.null(p) && !is.na(p$search_date))
        str_replace_all(p$search_date, " ", "_") else Sys.Date()
      paste0("flights_", date_part, ".xlsx")
    },
    content = function(file) {
      df <- flights_r()
      p  <- parsed_r()
      wb <- createWorkbook()

      # Info sheet
      addWorksheet(wb, "Search Info")
      info <- data.frame(
        Field = c("Route", "Search Date", "Parsed On"),
        Value = c(
          paste(p$route_from, "→", p$route_to),
          if (!is.na(p$search_date)) p$search_date else "Unknown",
          format(Sys.time(), "%Y-%m-%d %H:%M")
        )
      )
      writeData(wb, "Search Info", info)

      # Nonstop
      ns <- df %>% filter(stops == "Nonstop")
      addWorksheet(wb, "Nonstop")
      writeData(wb, "Nonstop", ns)

      # With stops
      st <- df %>% filter(stops != "Nonstop")
      if (nrow(st) > 0) {
        addWorksheet(wb, "With Stops")
        writeData(wb, "With Stops", st)
      }

      # Date price strip
      if (length(p$date_prices) > 0) {
        addWorksheet(wb, "Price Calendar")
        writeData(wb, "Price Calendar",
          data.frame(Date = names(p$date_prices),
                     Price_PKR = unlist(p$date_prices)))
      }

      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
