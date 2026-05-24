library(shiny)
library(leaflet)
library(BikeWise)

LABELS <- c("home", "work", "education", "friends", "sports", "music",
            "custom1", "custom2")

LABEL_ICONS <- c(
  home      = "home",
  work      = "briefcase",
  education = "graduation-cap",
  friends   = "users",
  sports    = "running",
  music     = "music",
  custom1   = "pencil",
  custom2   = "pencil"
)

LABEL_TITLES <- c(
  home      = "Home",
  work      = "Work",
  education = "Education",
  friends   = "Friends",
  sports    = "Sports",
  music     = "Music",
  custom1   = "Custom 1",
  custom2   = "Custom 2"
)

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: system-ui, sans-serif; }
    .bw-page { max-width: 480px; margin: 0 auto; padding: 24px 16px; }
    .bw-card-grid {
      display: flex;
      flex-wrap: wrap;
      gap: 12px;
      justify-content: center;
      margin: 24px 0;
    }
    .bw-card.btn, .bw-card.btn:active, .bw-card.btn.active {
      width: 130px;
      height: 110px;
      border: 2px solid #dee2e6 !important;
      border-radius: 12px !important;
      background: #fff !important;
      color: #495057 !important;
      box-shadow: none !important;
      display: inline-flex !important;
      flex-direction: column;
      align-items: center;
      justify-content: center;
      gap: 8px;
      font-size: 13px;
      font-weight: 500;
      padding: 0 !important;
      white-space: normal;
      transition: all 0.15s ease;
    }
    .bw-card.btn:hover, .bw-card.btn:focus {
      border-color: #0d6efd !important;
      color: #0d6efd !important;
      background: #f0f4ff !important;
    }
    .bw-card.btn.saved {
      border-color: #198754 !important;
      color: #198754 !important;
    }
    .bw-card.btn.saved:hover, .bw-card.btn.saved:focus {
      background: #f0faf4 !important;
    }
    .bw-card i { font-size: 28px; display: block; }
    .bw-info-box {
      margin-top: 16px;
      padding: 14px 16px;
      border-radius: 10px;
      border: 1px solid #dee2e6;
      background: #f8f9fa;
    }
    .bw-route-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      margin-bottom: 12px;
    }
    .bw-stats { display: flex; gap: 24px; margin-bottom: 12px; }
  "))),
  uiOutput("body")
)

# ── UI helpers ────────────────────────────────────────────────────────────────

login_ui <- function() {
  div(class = "bw-page",
    h2("BikeWise"),
    p("Enter your username and password to log in or create a new account."),
    textInput("username", "Username"),
    passwordInput("password", "Password"),
    actionButton("login_btn", "Continue", class = "btn-primary")
  )
}

rain_pref_ui <- function() {
  div(class = "bw-page",
    h2("How much rain can you handle?"),
    radioButtons(
      inputId  = "rain_pref",
      label    = NULL,
      choices  = list(
        "No rain at all"         = "none",
        "Light rain is fine"     = "light",
        "Moderate rain is fine"  = "moderate",
        "I cycle in any weather" = "heavy"
      ),
      selected = "none"
    ),
    actionButton("save_pref_btn", "Save and continue", class = "btn-primary")
  )
}

location_picker_ui <- function(mode, saved_labels, titles = LABEL_TITLES) {
  title <- if (mode == "from") {
    "Where are you cycling from?"
  } else {
    "Where are you cycling to?"
  }

  cards <- lapply(LABELS, function(lbl) {
    cls <- paste0("bw-card", if (lbl %in% saved_labels) " saved" else "")
    actionButton(
      inputId = paste0(mode, "_", lbl),
      label   = tagList(
        icon(LABEL_ICONS[[lbl]]),
        tags$span(titles[[lbl]])
      ),
      class   = cls
    )
  })

  div(class = "bw-page",
    if (mode == "to")
      actionLink("back_to_from_btn",
                 tagList(icon("arrow-left"), " Change start"),
                 style = "display:block; margin-bottom:8px;"),
    h3(title),
    div(class = "bw-card-grid", cards)
  )
}

route_ui <- function(from_label, to_label, titles = LABEL_TITLES) {
  div(class = "bw-page",
    div(class = "bw-route-header",
      h3(
        paste(titles[[from_label]], "→", titles[[to_label]]),
        style = "margin:0;"
      ),
      actionButton(
        "restart_btn", "New route",
        class = "btn-outline-secondary btn-sm"
      )
    ),
    leafletOutput("route_map", height = "360px"),
    uiOutput("rain_summary_ui")
  )
}

# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  page           <- reactiveVal("login")
  current_user   <- reactiveVal(NULL)
  from_coords    <- reactiveVal(NULL)
  to_coords      <- reactiveVal(NULL)
  pending_label  <- reactiveVal(NULL)
  pending_mode   <- reactiveVal(NULL)
  pending_coords <- reactiveVal(NULL)
  locs_version   <- reactiveVal(0L)

  # ── Saved locations (re-fetched when a new one is added) ─────────────────────

  saved_locations <- reactive({
    locs_version()
    req(current_user())
    tryCatch(
      get_locations(current_user()),
      error = function(e) {
        data.frame(
          user = character(), label = character(), address = character(),
          lat  = numeric(),   lon   = numeric(),
          stringsAsFactors = FALSE
        )
      }
    )
  })

  label_titles <- reactive({
    locs   <- saved_locations()
    titles <- LABEL_TITLES
    for (lbl in c("custom1", "custom2")) {
      row <- locs[locs$label == lbl, ]
      dn  <- if (nrow(row) > 0) row$display_name[1] else NA
      if (!is.na(dn)) titles[[lbl]] <- dn
    }
    titles
  })

  # ── Page renderer ─────────────────────────────────────────────────────────────

  output$body <- renderUI({
    switch(page(),
      login     = login_ui(),
      rain_pref = rain_pref_ui(),
      wherefrom = location_picker_ui(
        "from", saved_locations()$label, label_titles()
      ),
      whereto   = location_picker_ui(
        "to", saved_locations()$label, label_titles()
      ),
      route     = route_ui(
        from_coords()$label, to_coords()$label, label_titles()
      )
    )
  })

  # ── Login ─────────────────────────────────────────────────────────────────────

  observeEvent(input$login_btn, {
    req(input$username, input$password)
    result <- authenticate_user(input$username, input$password)
    switch(result,
      created       = {
        current_user(input$username)
        showNotification("Account created! Please set your rain preference.",
                         type = "message")
        page("rain_pref")
      },
      authenticated = {
        current_user(input$username)
        page("wherefrom")
      },
      wrong_password = showNotification(
        "Username already taken or password incorrect.", type = "error"
      )
    )
  })

  # ── Rain preference ───────────────────────────────────────────────────────────

  observeEvent(input$save_pref_btn, {
    req(current_user(), input$rain_pref)
    rain_preference(current_user(), input$rain_pref)
    page("wherefrom")
  })

  # ── Location card clicks ──────────────────────────────────────────────────────

  handle_location_click <- function(lbl, mode) {
    if (page() != paste0("where", mode)) return()

    locs  <- saved_locations()
    match <- locs[locs$label == lbl, ]

    pending_label(lbl)
    pending_mode(mode)

    if (nrow(match) > 0) {
      pending_coords(list(lat = match$lat[1], lon = match$lon[1], label = lbl))
      showModal(modalDialog(
        title = label_titles()[[lbl]],
        p("Current address:", strong(match$address[1])),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("change_address_btn", "Change address",
                       class = "btn-outline-secondary"),
          actionButton("use_saved_btn", "Use this", class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    } else if (lbl %in% c("custom1", "custom2")) {
      showModal(modalDialog(
        title = "Add a custom location",
        textInput("custom_label_name", "Name",
                  placeholder = "e.g. Gym, Parents' house"),
        textInput("new_address", "Address",
                  placeholder = "e.g. Dam Square, Amsterdam"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_address_btn", "Save & use",
                       class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    } else {
      showModal(modalDialog(
        title = paste("Add your", label_titles()[[lbl]], "address"),
        textInput("new_address", "Address",
                  placeholder = "e.g. Dam Square, Amsterdam"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_address_btn", "Save & use",
                       class = "btn-primary")
        ),
        easyClose = TRUE
      ))
    }
  }

  lapply(LABELS, function(lbl) {
    observeEvent(input[[paste0("from_", lbl)]],
                 handle_location_click(lbl, "from"), ignoreInit = TRUE)
    observeEvent(input[[paste0("to_",   lbl)]],
                 handle_location_click(lbl, "to"),   ignoreInit = TRUE)
  })

  # ── Use saved address ─────────────────────────────────────────────────────────

  observeEvent(input$use_saved_btn, {
    req(pending_coords(), pending_mode())
    removeModal()
    result <- pending_coords()
    if (pending_mode() == "from") {
      from_coords(result)
      page("whereto")
    } else {
      to_coords(result)
      page("route")
    }
  })

  observeEvent(input$change_address_btn, {
    req(pending_label())
    removeModal()
    is_custom <- pending_label() %in% c("custom1", "custom2")
    showModal(modalDialog(
      title = paste("Change your",
                    label_titles()[[pending_label()]],
                    if (is_custom) "location" else "address"),
      if (is_custom)
        textInput("custom_label_name", "Name",
                  value       = label_titles()[[pending_label()]],
                  placeholder = "e.g. Gym, Parents' house"),
      textInput("new_address", "New address",
                placeholder = "e.g. Dam Square, Amsterdam"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_address_btn", "Save & use",
                     class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  # ── Address modal submission ──────────────────────────────────────────────────

  observeEvent(input$confirm_address_btn, {
    req(pending_label(), pending_mode())
    address <- trimws(input$new_address)
    if (nchar(address) == 0) {
      showNotification("Please enter an address.", type = "warning")
      return()
    }

    is_custom   <- pending_label() %in% c("custom1", "custom2")
    custom_name <- if (is_custom) trimws(input$custom_label_name) else NULL
    if (is_custom && (is.null(custom_name) || nchar(custom_name) == 0)) {
      showNotification("Please enter a name.", type = "warning")
      return()
    }

    removeModal()

    coords <- tryCatch(
      save_location(current_user(), pending_label(), address, custom_name),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )

    if (is.null(coords)) return()
    locs_version(locs_version() + 1L)

    result <- list(lat = coords$lat, lon = coords$lon, label = pending_label())
    if (pending_mode() == "from") {
      from_coords(result)
      page("whereto")
    } else {
      to_coords(result)
      page("route")
    }
  })

  # ── Navigation ────────────────────────────────────────────────────────────────

  observeEvent(input$back_to_from_btn, {
    to_coords(NULL)
    page("wherefrom")
  })

  observeEvent(input$restart_btn, {
    from_coords(NULL)
    to_coords(NULL)
    page("wherefrom")
  })

  # ── Route and rain computation ────────────────────────────────────────────────

  route_data <- reactive({
    req(from_coords(), to_coords())
    tryCatch(
      bikeroute(
        from_coords()$lat, from_coords()$lon,
        to_coords()$lat,   to_coords()$lon
      ),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error")
        NULL
      }
    )
  })

  rain_data <- reactive({
    req(route_data(), current_user())
    pref <- rain_preference(current_user())
    if (pref == "heavy") return(NULL)
    tryCatch(
      raintracker(route_data()$timed_coords,
                  start_time     = Sys.time(),
                  threshold = pref),
      error = function(e) NULL
    )
  })

  # ── Route map ─────────────────────────────────────────────────────────────────

  output$route_map <- renderLeaflet({
    route  <- route_data()
    coords <- route$coordinates
    leaflet(coords) |>
      addTiles() |>
      addPolylines(lng = ~lon, lat = ~lat,
                   color = "#0d6efd", weight = 4, opacity = 0.8) |>
      addMarkers(
        lng = coords$lon[1], lat = coords$lat[1],
        popup = paste0("<b>From:</b> ", label_titles()[[from_coords()$label]])
      ) |>
      addMarkers(
        lng   = coords$lon[nrow(coords)],
        lat   = coords$lat[nrow(coords)],
        popup = paste0("<b>To:</b> ", label_titles()[[to_coords()$label]])
      )
  })

  # ── Rain summary ──────────────────────────────────────────────────────────────

  output$rain_summary_ui <- renderUI({
    route  <- route_data()
    result <- rain_data()

    stats <- div(class = "bw-stats",
      span(icon("clock"), " ", round(route$duration_min), " min"),
      span(icon("road"),  " ", round(route$distance_km, 1), " km")
    )

    rain_box <- if (is.null(result)) {
      div(style = "color:#198754;",
        icon("sun"), strong(" No rain limit set — ride whenever you like!")
      )
    } else if (!result$safe_to_go) {
      div(style = "color:#dc3545;",
        icon("cloud-showers-heavy"),
        strong(" Heavy rain all day — no dry window found in today's forecast.")
      )
    } else {
      # Departure shifted >15 min means rain is expected at the original time
      delay_min <- as.numeric(
        difftime(result$suggested_departure, Sys.time(), units = "mins")
      )
      if (delay_min <= 15) {
        tagList(
          div(style = "color:#198754;",
            icon("sun"), strong(" No rain on your route — good to go now!")
          ),
          if (!is.null(result$end_of_route_note))
            div(style = "margin-top:8px; color:#856404;",
              icon("exclamation-triangle"), " ", result$end_of_route_note)
        )
      } else {
        tagList(
          div(style = "color:#fd7e14;",
            icon("cloud-rain"),
            strong(paste0(" Rain expected — leave at ",
                          format(result$suggested_departure, "%H:%M"),
                          " for a dry ride."))
          ),
          div(style = "margin-top:4px; color:#6c757d; font-size:12px;",
            "Forecast resolution is 15 minutes — departure time is approximate."
          ),
          if (!is.null(result$end_of_route_note))
            div(style = "margin-top:8px; color:#856404;",
              icon("exclamation-triangle"), " ", result$end_of_route_note)
        )
      }
    }

    div(class = "bw-info-box", stats, rain_box)
  })

}

# ── Run ───────────────────────────────────────────────────────────────────────

shinyApp(ui, server)
