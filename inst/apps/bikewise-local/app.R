library(shiny)


# ── UI ────────────────────────────────────────────────────────────────────────
# Single placeholder — server swaps screens by updating output$page.

ui <- fluidPage(
  uiOutput("page")
)


# ── UI helpers ────────────────────────────────────────────────────────────────
login_ui <- function() {
  fluidRow(
    column(12, align = "center",
      h2("BikeWise", icon("bicycle")),
      strong("Cycle safely. Arrive dry."),
      p("log in or sign up!"),
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      actionButton("login_btn", "sign in")
    )
  )
}

rain_tolerance_ui <- function() {
  fluidRow(
    column(12, align = "center",
      h2("BikeWise", icon("bicycle")),
      p("How much rain can you handle?"),
      fluidRow(actionButton("tol_none",
                            tagList(icon("sun"), " No rain"),
                            width = "200px")),
      br(),
      fluidRow(actionButton("tol_light",
                            tagList(icon("cloud"), " Light rain is fine"),
                            width = "200px")),
      br(),
      fluidRow(actionButton("tol_moderate",
                            tagList(icon("cloud-rain"),
                                    " Moderate rain is fine"),
                            width = "200px")),
      br(),
      fluidRow(actionButton("tol_heavy",
                            tagList(icon("cloud-showers-heavy"),
                                    " I cycle in any weather"),
                            width = "200px"))
    )
  )
}

pick_start_ui <- function(saved_labels) {
  btn_class <- function(key) {
    if (key %in% saved_labels) "btn-success" else "btn-default"
  }
  # Use tagList as container — fluidRow breaks the layout here.
  tagList(
    fluidRow(column(12, align = "center",
      h2("BikeWise", icon("bicycle")))),
    fluidRow(column(12, align = "center",
      splitLayout(
        h3("pick your start point"),
        div(align = "center", actionButton("settings_btn", icon("cog")))
      )
    )),
    br(),
    splitLayout(
      actionButton("from_home",
                   tagList(icon("home"), " Home"),
                   width = "100%", class = btn_class("home")),
      actionButton("from_work",
                   tagList(icon("briefcase"), " Work"),
                   width = "100%", class = btn_class("work"))
    ),
    br(),
    splitLayout(
      actionButton("from_education",
                   tagList(icon("graduation-cap"), " Education"),
                   width = "100%", class = btn_class("education")),
      actionButton("from_friends",
                   tagList(icon("users"), " Friends"),
                   width = "100%", class = btn_class("friends"))
    ),
    br(),
    splitLayout(
      actionButton("from_sports",
                   tagList(icon("running"), " Sports"),
                   width = "100%", class = btn_class("sports")),
      actionButton("from_music",
                   tagList(icon("music"), " Music"),
                   width = "100%", class = btn_class("music"))
    ),
    br(),
    splitLayout(
      actionButton("from_custom1",
                   tagList(icon("pencil"), " Custom 1"),
                   width = "100%", class = btn_class("custom1")),
      actionButton("from_custom2",
                   tagList(icon("pencil"), " Custom 2"),
                   width = "100%", class = btn_class("custom2"))
    )
  )
}

pick_end_ui <- function(saved_labels) {
  btn_class <- function(key) {
    if (key %in% saved_labels) "btn-success" else "btn-default"
  }
  # Use tagList as container — fluidRow breaks the layout here.
  tagList(
    fluidRow(column(12, align = "center",
      h2("BikeWise", icon("bicycle")))),
    fluidRow(column(12, align = "center",
      splitLayout(
        h3("pick your end point"),
        div(align = "center", actionButton("settings_btn", icon("cog")))
      )
    )),
    br(),
    splitLayout(
      actionButton("to_home",
                   tagList(icon("home"), " Home"),
                   width = "100%", class = btn_class("home")),
      actionButton("to_work",
                   tagList(icon("briefcase"), " Work"),
                   width = "100%", class = btn_class("work"))
    ),
    br(),
    splitLayout(
      actionButton("to_education",
                   tagList(icon("graduation-cap"), " Education"),
                   width = "100%", class = btn_class("education")),
      actionButton("to_friends",
                   tagList(icon("users"), " Friends"),
                   width = "100%", class = btn_class("friends"))
    ),
    br(),
    splitLayout(
      actionButton("to_sports",
                   tagList(icon("running"), " Sports"),
                   width = "100%", class = btn_class("sports")),
      actionButton("to_music",
                   tagList(icon("music"), " Music"),
                   width = "100%", class = btn_class("music"))
    ),
    br(),
    splitLayout(
      actionButton("to_custom1",
                   tagList(icon("pencil"), " Custom 1"),
                   width = "100%", class = btn_class("custom1")),
      actionButton("to_custom2",
                   tagList(icon("pencil"), " Custom 2"),
                   width = "100%", class = btn_class("custom2"))
    ),
    br(),
    fluidRow(column(12, align = "center",
      actionButton("back_btn", tagList(icon("arrow-left"), " Back"))
    ))
  )
}

route_ui <- function(route_data) {
  tagList(
    fluidRow(column(12, align = "center",
      h2("BikeWise", icon("bicycle"))
    )),
    br(),

    # Rain summary
    fluidRow(column(12, align = "center",
      uiOutput("rain_advice")
    )),
    br(),
    # Always rendered — server returns invisible(NULL) when no rain data.
    fluidRow(column(12, align = "center",
      plotOutput("rain_plot", height = "220px")
    )),

    # Route stats
    splitLayout(
      div(align = "right", style = "padding-right: 30px;",
          h3(icon("road"),  paste(round(route_data$distance_km, 1), "km"))),
      div(align = "left",  style = "padding-left: 30px;",
          h3(icon("clock"), paste(route_data$duration_min, "min")))
    ),

    # Actions
    br(),
    splitLayout(
      actionButton("new_route_btn", tagList(icon("route"), " New route"),
                   width = "100%", class = "btn-default"),
      actionButton("logout_btn", tagList(icon("sign-out-alt"), " Log out"),
                   width = "100%", class = "btn-danger")
    ),
    div(
      style = "position: fixed; bottom: 20px; width: 100%; text-align: center;",
      p(em("Note: cycling times assume a constant pace and may vary with terrain and wind.")) # nolint: line_length_linter
    )
  )
}

settings_ui <- function(saved_labels, saved_speed) {
  btn_class <- function(key) {
    if (key %in% saved_labels) "btn-success" else "btn-default"
  }
  tagList(
    fluidRow(column(12, align = "center",
      h2("Settings"),
      p("BikeWise", icon("bicycle"))
    )),

    # Locations
    fluidRow(column(12, align = "center",
      h4("Your Locations"),
      p("update your saved spots!")
    )),
    br(),
    splitLayout(
      actionButton("settings_home",
                   tagList(icon("home"), " Home"),
                   width = "100%", class = btn_class("home")),
      actionButton("settings_work",
                   tagList(icon("briefcase"), " Work"),
                   width = "100%", class = btn_class("work"))
    ),
    br(),
    splitLayout(
      actionButton("settings_education",
                   tagList(icon("graduation-cap"), " Education"),
                   width = "100%", class = btn_class("education")),
      actionButton("settings_friends",
                   tagList(icon("users"), " Friends"),
                   width = "100%", class = btn_class("friends"))
    ),
    br(),
    splitLayout(
      actionButton("settings_sports",
                   tagList(icon("running"), " Sports"),
                   width = "100%", class = btn_class("sports")),
      actionButton("settings_music",
                   tagList(icon("music"), " Music"),
                   width = "100%", class = btn_class("music"))
    ),
    br(),
    splitLayout(
      actionButton("settings_custom1",
                   tagList(icon("pencil"), " Custom 1"),
                   width = "100%", class = btn_class("custom1")),
      actionButton("settings_custom2",
                   tagList(icon("pencil"), " Custom 2"),
                   width = "100%", class = btn_class("custom2"))
    ),
    br(),

    # Rain tolerance
    fluidRow(column(12, align = "center",
      h4("Update your rain tolerance"),
      p("How much rain can you handle?")
    )),
    # Buttons stay on settings — no page navigation triggered.
    splitLayout(
      actionButton("settings_tol_none",
                   tagList(icon("sun"), " No rain"),
                   width = "100%"),
      actionButton("settings_tol_light",
                   tagList(icon("cloud"), " Light rain is fine"),
                   width = "100%")
    ),
    br(),
    splitLayout(
      actionButton("settings_tol_moderate",
                   tagList(icon("cloud-rain"), " Moderate rain is fine"),
                   width = "100%"),
      actionButton("settings_tol_heavy",
                   tagList(icon("cloud-showers-heavy"),
                           " I cycle in any weather"),
                   width = "100%")
    ),
    br(),

    # Cycling speed
    fluidRow(column(12, align = "center",
      h4("Your cycling speed"),
      p("How fast do you usually cycle?")
    )),
    fluidRow(column(12, align = "center",
      numericInput("settings_speed_input",
                   label = NULL,
                   value = if (!is.na(saved_speed) &&
                               length(saved_speed) > 0) saved_speed else 15,
                   min = 1, max = 100, step = 1,
                   width = "120px"),
      actionButton("settings_speed_save",
                   tagList(icon("save"), " Save speed"),
                   width = "50%")
    )),
    br(),

    fluidRow(column(12, align = "center",
      actionButton("settings_back_btn", tagList(icon("arrow-left"), " Back"))
    ))
  )
}




# ── Server ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # State
  current_page  <- reactiveVal("login")
  current_user  <- reactiveVal(NULL)
  from_coords   <- reactiveVal(NULL)
  to_coords     <- reactiveVal(NULL)
  previous_page <- reactiveVal(NULL)

  # Holds the location label clicked while a modal is open.
  pending_label <- reactiveVal(NULL)

  # Counters incremented to force reactives to re-run after settings updates.
  tolerance_version <- reactiveVal(0)
  speed_version     <- reactiveVal(0)
  settings_version  <- reactiveVal(0)



  # Page rendering
  output$page <- renderUI({
    settings_version()
    switch(current_page(),
      login          = login_ui(),
      rain_tolerance = rain_tolerance_ui(),
      # get_locations() called fresh each render so saved locations appear green.
      pick_start     = pick_start_ui(
        get_locations(current_user(), example = TRUE)$label
      ),
      pick_end       = pick_end_ui(
        get_locations(current_user(), example = TRUE)$label
      ),
      settings       = settings_ui(
        get_locations(current_user(), example = TRUE)$label,
        cycling_speed(current_user(), example = TRUE)
      ),
      # route_data() returns NULL when bikeroute fails — guard prevents UI crash
      route = {
        rd <- route_data()
        if (is.null(rd)) p("Could not load route.", style = "color: gray;")
        else route_ui(rd)
      },
      h2("Unknown page")
    )
  })


  # Login
  observeEvent(input$login_btn, {
    result <- authenticate_user(input$username, input$password, example = TRUE)
    switch(result,
      created = {
        current_user(input$username)
        current_page("rain_tolerance")
      },
      authenticated = {
        current_user(input$username)
        current_page("pick_start")
      },
      wrong_password = showNotification(
        "Whoops, password or username are incorrect!",
        type = "error"
      )
    )
  })

  # Rain tolerance — onboarding choice navigates to pick_start after selection
  lapply(c("none", "light", "moderate", "heavy"), function(tol) {
    observeEvent(input[[paste0("tol_", tol)]], {
      rain_tolerance(current_user(), tol, example = TRUE)
      current_page("pick_start")
    })
  })


  # Location buttons — from
  lapply(names(preset_titles), function(lbl) {
    observeEvent(input[[paste0("from_", lbl)]], {
      locs <- get_locations(current_user(), example = TRUE)
      if (lbl %in% locs$label) {
        match <- locs[locs$label == lbl, ]
        from_coords(list(lat = match$lat[1], lon = match$lon[1], label = lbl))
        current_page("pick_end")
      } else {
        pending_label(lbl)
        showModal(modalDialog(
          title = paste("Add", preset_titles[[lbl]], "Address"),
          textInput("new_address", "Address",
                    placeholder = "e.g. Roetersstraat, Amsterdam"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_address_btn", "Save & Use",
                         class = "btn-primary")
          ),
          easyClose = TRUE
        ))
      }
    })
  })

  # Location buttons — to
  # guards against routing to the same location the user departed from
  lapply(names(preset_titles), function(lbl) {
    observeEvent(input[[paste0("to_", lbl)]], {
      locs <- get_locations(current_user(), example = TRUE)
      if (lbl %in% locs$label) {
        if (!is.null(from_coords()) && from_coords()$label == lbl) {
          showNotification("Start and end can't be the same location.",
                           type = "warning")
          return()
        }
        match <- locs[locs$label == lbl, ]
        to_coords(list(lat = match$lat[1], lon = match$lon[1], label = lbl))
        current_page("route")
      } else {
        pending_label(lbl)
        showModal(modalDialog(
          title = paste("Add", preset_titles[[lbl]], "Address"),
          textInput("new_address", "Address",
                    placeholder = "e.g. Roetersstraat, Amsterdam"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_address_btn", "Save & Use",
                         class = "btn-primary")
          ),
          easyClose = TRUE
        ))
      }
    })
  })

  # settings location buttons — always show modal to update or add an address
  # no saved/unsaved branch needed: settings is always for overwriting
  lapply(names(preset_titles), function(lbl) {
    observeEvent(input[[paste0("settings_", lbl)]], {
      pending_label(lbl)
      showModal(modalDialog(
        title = paste("Update", preset_titles[[lbl]], "Address"),
        textInput("new_address", "Address",
                  placeholder = "e.g. Roetersstraat, Amsterdam"),
        footer = tagList(modalButton("Cancel"),
                         actionButton("confirm_address_btn", "Save",
                                      class = "btn-primary")),
        easyClose = TRUE
      ))
    })
  })

  # shared modal confirm — current_page() routes the result to the right screen
  observeEvent(input$confirm_address_btn, {

    coords <- tryCatch(
      save_location(current_user(), pending_label(), input$new_address,
                    example = TRUE),
      error = function(e) {
        showNotification("Address not found. Please try a different address.",
                         type = "error", duration = 5)
        NULL
      }
    )

    # keep the modal open if geocoding failed
    if (is.null(coords)) return()

    removeModal()

    if (current_page() == "pick_start") {
      from_coords(list(
        lat = coords$lat, lon = coords$lon, label = pending_label()
      ))
      current_page("pick_end")
    } else if (current_page() == "settings") {
      settings_version(settings_version() + 1)
    } else {
      to_coords(list(
        lat = coords$lat, lon = coords$lon, label = pending_label()
      ))
      current_page("route")
    }
  })

  # clear stale end point so back-navigation starts fresh
  observeEvent(input$back_btn, {
    to_coords(NULL)
    current_page("pick_start")
  })

  # both coords cleared — avoids previous route bleeding into the next one
  observeEvent(input$new_route_btn, {
    from_coords(NULL)
    to_coords(NULL)
    current_page("pick_start")
  })

  # wipe all user state — next login will start a clean session
  observeEvent(input$logout_btn, {
    current_user(NULL)
    from_coords(NULL)
    to_coords(NULL)
    current_page("login")
  })

  # previous_page stored so settings_back_btn can restore the right screen
  observeEvent(input$settings_btn, {
    previous_page(current_page())
    current_page("settings")
  })

  observeEvent(input$settings_back_btn, {
    current_page(previous_page())
  })

  # rejects out-of-range input before persisting; speed_version bump
  # forces user_speed reactive to re-run
  observeEvent(input$settings_speed_save, {
    speed <- input$settings_speed_input
    if (is.na(speed) || speed < 1 || speed > 100) {
      showNotification("Please enter a speed between 1 and 100 km/h.",
                       type = "warning", duration = 4)
      return()
    }
    cycling_speed(current_user(), speed, example = TRUE)
    speed_version(speed_version() + 1)
    showNotification("Cycling speed saved!", type = "message", duration = 2)
  })


  # stays on settings — tolerance_version triggers rain_result to recalculate
  lapply(c("none", "light", "moderate", "heavy"), function(tol) {
    observeEvent(input[[paste0("settings_tol_", tol)]], {
      rain_tolerance(current_user(), tol, example = TRUE)
      tolerance_version(tolerance_version() + 1)
    })
  })


  # ── Reactives ───────────────────────────────────────────────────────────────

  # tolerance_version() establishes a dependency so settings updates
  # invalidate this reactive without requiring a user change
  tolerance <- reactive({
    tolerance_version()
    req(current_user())
    rain_tolerance(current_user(), example = TRUE)
  })

  # same dependency pattern as tolerance — 15 km/h is a typical average
  # cycling pace and used as fallback when the user hasn't set a speed yet
  user_speed <- reactive({
    speed_version()
    req(current_user())
    s <- cycling_speed(current_user(), example = TRUE)
    if (length(s) == 0 || is.na(s)) 15 else s
  })


  # req() silently cancels until the user has picked both endpoints;
  # user_speed() as a dependency re-runs this when speed is updated
  route_data <- reactive({
    req(from_coords(), to_coords())
    tryCatch(
      bikeroute(from_coords()$lat, from_coords()$lon,
                to_coords()$lat, to_coords()$lon,
                speed_kmh = user_speed()),
      error = function(e) {
        showNotification("Could not calculate route. Please try again.",
                         type = "error", duration = 5)
        NULL
      }
    )
  })


  # threshold = tolerance() makes safe_to_go specific to this user's preference
  # — always runs, even for heavy users; rain_advice skips the result for them
  rain_result <- reactive({
    req(route_data(), tolerance())
    tryCatch(
      raintracker(route_data()$timed_coords,
                  start_time = Sys.time(),
                  threshold  = tolerance()),
      error = function(e) {
        showNotification("Could not fetch weather data. Please try again.",
                         type = "error", duration = 5)
        NULL
      }
    )
  })


  # heavy users get a fixed message; NULL rain_result falls back gracefully
  output$rain_advice <- renderUI({
    req(tolerance())

    if (tolerance() == "heavy") {
      return(h3("Built different. Just ride."))
    }

    if (is.null(rain_result())) {
      return(p("Could not load weather data. Please try again.",
               style = "color: gray;"))
    }
    result <- rain_result()

    if (!result$safe_to_go) {
      # no rain-free window found in today's forecast
      h3("No dry window today. Grab a raincoat")

    } else if (as.numeric(difftime(result$suggested_departure,
                                   Sys.time(),
                                   units = "mins")
                          ) <= 5) {
      # departure is now or within 5 minutes
      h3("You're good to go!")

    } else {
      # departure is later — tell user when to leave
      h3(paste("Leave at",
               format(result$suggested_departure, "%H:%M"),
               "for a dry ride."))
    }
  })


  # render rain plot — always renders; blank axes if API failed
  output$rain_plot <- renderPlot({
    req(route_data(), tolerance())
    if (is.null(rain_result())) return(invisible(NULL))
    plot_rain(rain_result()$route_rain_summary, tolerance())
  })


}


# ── Launch ────────────────────────────────────────────────────────────────────

shinyApp(ui, server)
