library(shiny)
library(BikeWise)

# ── UI ────────────────────────────────────────────────────────────────────────
# The UI describes what the user sees in the browser.
# We use a single placeholder (uiOutput) here and let the server decide
# which screen to show based on where the user is in the flow.

ui <- fluidPage(
  uiOutput("page")
)

# ── Server ────────────────────────────────────────────────────────────────────
# The server contains all the logic: what happens when the user clicks a button,
# how data is loaded, and which screen to show next.


# ── UI helpers ────────────────────────────────────────────────────────────────
# login_ui helper to determine the login screen
login_ui <- function() {

  # use fluidrow to organise the UI
  fluidRow(
    column(12, align = "center", # fill full screen and center

    # set up displayed content
    h2("BikeWise", icon("bicycle")), # add title and icon
    strong("Cycle safely. Arrive dry."), # add bold subtitle as paragraph
    p("log in or sign up!"), # paragraph element with user instruction

    # set up user input
    textInput("username", "Username"), # create file for user input
    passwordInput("password", "Password"), # as above, but hiding input

    # create a button allowing the user to sign in
    actionButton("login_btn", "sign in")
  )
)
}

# rain_tolerance_ui to determine the rain preference screen
rain_tolerance_ui <- function() {

  # use fluidrow to organise the UI
  fluidRow(
    column(12, align = "center",
    h2("BikeWise", icon("bicycle")), # add title and icon
    p("How much rain can you handle?"), # message to user as paragraph

    # add stacked buttons for the user to interact with
    fluidRow(actionButton("tol_none",
                          tagList(icon("sun"), " No rain"),
                          width = "200px")),

    fluidRow(actionButton("tol_light",
                          tagList(icon("cloud"), " Light rain is fine"),
                          width = "200px")),

    fluidRow(actionButton("tol_moderate",
                          tagList(icon("cloud-rain"), " Moderate rain is fine"),
                          width = "200px")),

    fluidRow(actionButton("tol_heavy",
                          tagList(icon("cloud-showers-heavy"),
                                  " I cycle in any weather"),
                          width = "200px"))
    )
  )
}

# pick_start_ui to determine where to start your ride
# saved_labels: vector of saved location labels for this user
pick_start_ui <- function(saved_labels) {

  # use taglist as container, as fluidRow messes with Layout
  tagList(
    fluidRow(column(12,
                    align = "center",
                    h2("BikeWise", icon("bicycle")))), # centered title

    # title and settings icon side by side, centered
    fluidRow(column(12, align = "center",
      splitLayout(
        h3("pick your start point"),

        # div as a container to center the icon
        div(align = "center", actionButton("settings_btn", icon("cog")))
      )
    )),
    br(),

    # create location cards with spacing between rows
    splitLayout(
      actionButton("from_home",
                    tagList(icon("home"), " Home"),
                    width = "100%",

                    # change color to green if in saved_labels
                    class = if ("home" %in% saved_labels) {
                      "btn-success"
                    } else {
                      "btn-default"
                    }),

      actionButton("from_work",
                   tagList(icon("briefcase"), " Work"),
                   width = "100%",
                   class = if ("work" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   })
    ),
    br(),
    splitLayout(
      actionButton("from_education",
                   tagList(icon("graduation-cap"), " Education"),
                   width = "100%",
                   class = if ("education" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   }),
      actionButton("from_friends",
                   tagList(icon("users"), " Friends"),
                   width = "100%",
                   class = if ("friends" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   })
    ),
    br(),
    splitLayout(
      actionButton("from_sports",
                   tagList(icon("running"), " Sports"),
                   width = "100%",
                   class = if ("sports" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   }),
      actionButton("from_music",
                   tagList(icon("music"), " Music"),
                   width = "100%",
                   class = if ("music" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   })
    ),
    br(),
    splitLayout(
      actionButton("from_custom1",
                   tagList(icon("pencil"), " Custom 1"),
                   width = "100%",
                   class = if ("custom1" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   }),
      actionButton("from_custom2",
                   tagList(icon("pencil"), " Custom 2"),
                   width = "100%",
                   class = if ("custom2" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   })
    )
  )

}


# pick_end_ui to determine where you end your bike ride
pick_end_ui <- function(saved_labels) {

  # use taglist as container, as fluidRow messes with Layout
  tagList(
    fluidRow(column(12,
                    align = "center",
                    h2("BikeWise", icon("bicycle")))), # centered title

    # title and settings icon side by side, centered
    fluidRow(column(12, align = "center",
      splitLayout(
        h3("pick your end point"),

        # div as a container to center the icon
        div(align = "center", actionButton("settings_btn", icon("cog")))
      )
    )),
    br(),

    # create location cards with spacing between rows
    splitLayout(
      actionButton("to_home",
                    tagList(icon("home"), " Home"),
                    width = "100%",
                    class = if ("home" %in% saved_labels) {
                      "btn-success"
                    } else {
                      "btn-default"
                    }),
      actionButton("to_work",
                   tagList(icon("briefcase"), " Work"),
                   width = "100%",
                   class = if ("work" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   })
    ),
    br(),
    splitLayout(
      actionButton("to_education",
                   tagList(icon("graduation-cap"), " Education"),
                   width = "100%",
                   class = if ("education" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   }),
      actionButton("to_friends",
                   tagList(icon("users"), " Friends"),
                   width = "100%",
                   class = if ("friends" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   })
    ),
    br(),
    splitLayout(
      actionButton("to_sports",
                   tagList(icon("running"), " Sports"),
                   width = "100%",
                   class = if ("sports" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   }),
      actionButton("to_music",
                   tagList(icon("music"), " Music"),
                   width = "100%",
                   class = if ("music" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   })
    ),
    br(),
    splitLayout(
      actionButton("to_custom1",
                   tagList(icon("pencil"), " Custom 1"),
                   width = "100%",
                   class = if ("custom1" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   }),
      actionButton("to_custom2",
                   tagList(icon("pencil"), " Custom 2"),
                   width = "100%",
                   class = if ("custom2" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   })
    ),
    br(),
    fluidRow(column(12,
                    align = "center",
                    actionButton("back_btn",
                                 tagList(icon("arrow-left"), " Back"))
                    ))
  )

}


# route_ui to show the result of the bike route and rain forecast
route_ui <- function(route_data) {

  # setup layout
  tagList(
    fluidRow(column(12, align = "center",
      h2("BikeWise", icon("bicycle"))
    )),
    br(),

    # add rain advice
    fluidRow(column(12, align = "center",
      uiOutput("rain_advice")
    )),
    br(),

    # rain plot — always rendered, server shows blank when no rain data
    fluidRow(column(12, align = "center",
      plotOutput("rain_plot", height = "220px")
    )),

    # show distance and duration side by side with icons
    splitLayout(
      div(align = "right",  style = "padding-right: 30px;",
          h3(icon("road"),  paste(round(route_data$distance_km, 1), "km"))),
      div(align = "left",   style = "padding-left: 30px;",
          h3(icon("clock"), paste(route_data$duration_min, "min")))
    ),

    # new route and logout buttons side by side
    br(),
    splitLayout(
      actionButton("new_route_btn", tagList(icon("route"), " New route"),
                   width = "100%", class = "btn-default"),
      actionButton("logout_btn", tagList(icon("sign-out-alt"), " Log out"),
                   width = "100%", class = "btn-danger")
    ),

    # note pinned to bottom of page
    div(
      style = "position: fixed; bottom: 20px; width: 100%; text-align: center;",
      p(em("Note: cycling times assume a constant pace and may vary with terrain and wind."))
    )


  )
}


# settings_ui so that the user can update their preferences and locations
settings_ui <- function(saved_labels, saved_speed) {
  tagList(
    fluidRow(column(12, align = "center",
    h2("Settings"),
    p("BikeWise", icon("bicycle"))
    )),

    # section 1 shows saved locations and allows to update
    fluidRow(column(12, align = "center",
    h4("Your Locations"),
    p("update your saved spots!")
    )),
    br(),

    # location cards — green if saved, grey if not
    splitLayout(
      actionButton("settings_home",
                   tagList(icon("home"), " Home"),
                   width = "100%",
                   class = if ("home" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   }),
      actionButton("settings_work",
                   tagList(icon("briefcase"), " Work"),
                   width = "100%",
                   class = if ("work" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   })
    ),
    br(),
    splitLayout(
      actionButton("settings_education",
                   tagList(icon("graduation-cap"), " Education"),
                   width = "100%",
                   class = if ("education" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   }),
      actionButton("settings_friends",
                   tagList(icon("users"), " Friends"),
                   width = "100%",
                   class = if ("friends" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   })
    ),
    br(),
    splitLayout(
      actionButton("settings_sports",
                   tagList(icon("running"), " Sports"),
                   width = "100%",
                   class = if ("sports" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   }),
      actionButton("settings_music",
                   tagList(icon("music"), " Music"),
                   width = "100%",
                   class = if ("music" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   })
    ),
    br(),
    splitLayout(
      actionButton("settings_custom1",
                   tagList(icon("pencil"), " Custom 1"),
                   width = "100%",
                   class = if ("custom1" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   }),
      actionButton("settings_custom2",
                   tagList(icon("pencil"), " Custom 2"),
                   width = "100%",
                   class = if ("custom2" %in% saved_labels) {
                     "btn-success"
                   } else {
                     "btn-default"
                   })
    ),
    br(),

    # section 2: let user update their rain tolerance
    fluidRow(column(12, align = "center",
      h4("Update your rain tolerance"),
      p("How much rain can you handle?")
    )),

    # tolerance buttons in 2x2 grid — stay on settings after selecting
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

    # section 3: let user update their cycling speed
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
      p(style = "color: grey; font-size: 0.85em;", "km/h — default is 15"),
      actionButton("settings_speed_save",
                   tagList(icon("save"), " Save speed"),
                   width = "50%")
    )),
    br(),

    # back button — returns to previous page
    fluidRow(column(12, align = "center",
      actionButton("settings_back_btn", tagList(icon("arrow-left"), " Back"))
    ))

  )
}



server <- function(input, output, session) {

  # tracks which screen is currently shown
  current_page <- reactiveVal("login")

  # track and save the username
  current_user <- reactiveVal(NULL)

  # store chosen start and end coordinates
  from_coords <- reactiveVal(NULL)
  to_coords   <- reactiveVal(NULL)

  # remember clicked location button
  pending_label <- reactiveVal(NULL)

  # remember which page the user came from before opening settings
  previous_page     <- reactiveVal(NULL)
  tolerance_version <- reactiveVal(0)
  speed_version     <- reactiveVal(0)



  # reads current_page() and draws matching screen
  output$page <- renderUI({

    # change screen based on current_page()
    switch(current_page(),
           login = login_ui(),
           rain_preference = rain_tolerance_ui(),
           # fetch fresh locations to reflect newly saved ones
           pick_start = pick_start_ui(
             get_locations(current_user(), example = TRUE)$label
           ),
           # same here, ensures green cards show up after saving on pick_start
           pick_end = pick_end_ui(
             get_locations(current_user(), example = TRUE)$label
           ),
           settings = settings_ui(
             get_locations(current_user(), example = TRUE)$label,
             cycling_speed(current_user(), example = TRUE)
           ),
           route = route_ui(route_data()),
           h2("Unknown page")
    )
    })


  # handle login button
  observeEvent(input$login_btn, {

    # store if user is authenticated based on provided credentials
    result <- authenticate_user(input$username,
                                input$password,
                                example = TRUE)

    # act depending on the result of authentication
    switch(result,

           # let new users indicate their rain tolerance
           created = {
             current_user(input$username)
             current_page("rain_preference")
           },

           # switch to new UI; let user choose startpoint
           authenticated = {
             current_user(input$username)
             current_page("pick_start")
           },

           # give notification for wrong password
           wrong_password = showNotification(
             "Whoops, password or username are incorrect!",
             type = "error"
           )
    )

  })

  # change screen after rain tolerance was indicated
  observeEvent(input$tol_none, {
    rain_preference(current_user(), "none", example = TRUE) # save tolerance
    current_page("pick_start")                              # go to next page
  })

  observeEvent(input$tol_light, {
    rain_preference(current_user(), "light", example = TRUE)
    current_page("pick_start")
  })

  observeEvent(input$tol_moderate, {
    rain_preference(current_user(), "moderate", example = TRUE)
    current_page("pick_start")
  })

  observeEvent(input$tol_heavy, {
    rain_preference(current_user(), "heavy", example = TRUE)
    current_page("pick_start")
  })


  # check if "home" is in saved locations
  observeEvent(input$from_home, {

    # if saved, use stored coordinates
    if ("home" %in% get_locations(current_user(), example = TRUE)$label) {
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "home", ]
      from_coords(list(lat = match$lat[1], lon = match$lon[1], label = "home"))
      current_page("pick_end")
    } else {

      # if not saved, show modal to input new address to the clicked label
      pending_label("home")

      # define modal content
      showModal(modalDialog(
        title = "Add Home Address",
        textInput("new_address", "Address",
                  placeholder = "e.g. Roetersstraat, Amsterdam"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_address_btn", "Save & Use",
                       class = "btn-primary")
        ),

        # allow to close modal by clicking outside
        easyClose = TRUE
      ))
    }
  })

  # check if "work" is in saved locations
  observeEvent(input$from_work, {
    if ("work" %in% get_locations(current_user(), example = TRUE)$label) {
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "work", ]
      from_coords(list(lat = match$lat[1], lon = match$lon[1], label = "work"))
      current_page("pick_end")
    } else {
      pending_label("work")
      showModal(modalDialog(
        title = "Add Work Address",
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

  # check if "education" is in saved locations
  observeEvent(input$from_education, {
    if ("education" %in% get_locations(current_user(), example = TRUE)$label) {
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "education", ]
      from_coords(list(
        lat = match$lat[1], lon = match$lon[1], label = "education"
      ))
      current_page("pick_end")
    } else {
      pending_label("education")
      showModal(modalDialog(
        title = "Add Education Address",
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

  # check if "friends" is in saved locations
  observeEvent(input$from_friends, {
    if ("friends" %in% get_locations(current_user(), example = TRUE)$label) {
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "friends", ]
      from_coords(list(
        lat = match$lat[1], lon = match$lon[1], label = "friends"
      ))
      current_page("pick_end")
    } else {
      pending_label("friends")
      showModal(modalDialog(
        title = "Add Friends Address",
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

  # check if "sports" is in saved locations
  observeEvent(input$from_sports, {
    if ("sports" %in% get_locations(current_user(), example = TRUE)$label) {
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "sports", ]
      from_coords(list(
        lat = match$lat[1], lon = match$lon[1], label = "sports"
      ))
      current_page("pick_end")
    } else {
      pending_label("sports")
      showModal(modalDialog(
        title = "Add Sports Address",
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

  # check if "music" is in saved locations
  observeEvent(input$from_music, {
    if ("music" %in% get_locations(current_user(), example = TRUE)$label) {
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "music", ]
      from_coords(list(lat = match$lat[1], lon = match$lon[1], label = "music"))
      current_page("pick_end")
    } else {
      pending_label("music")
      showModal(modalDialog(
        title = "Add Music Address",
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

  # check if "custom1" is in saved locations
  observeEvent(input$from_custom1, {
    if ("custom1" %in% get_locations(current_user(), example = TRUE)$label) {
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "custom1", ]
      from_coords(list(
        lat = match$lat[1], lon = match$lon[1], label = "custom1"
      ))
      current_page("pick_end")
    } else {
      pending_label("custom1")
      showModal(modalDialog(
        title = "Add Custom 1 Address",
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

  # check if "custom2" is in saved locations
  observeEvent(input$from_custom2, {
    if ("custom2" %in% get_locations(current_user(), example = TRUE)$label) {
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "custom2", ]
      from_coords(list(
        lat = match$lat[1], lon = match$lon[1], label = "custom2"
      ))
      current_page("pick_end")
    } else {
      pending_label("custom2")
      showModal(modalDialog(
        title = "Add Custom 2 Address",
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


  # save address from modal — handles both pick_start and pick_end
  observeEvent(input$confirm_address_btn, {

    # call coords, but make sure to give warning if address unavailable
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

    # store coords and navigate depending on which screen opened the modal
    if (current_page() == "pick_start") {
      from_coords(list(
        lat = coords$lat, lon = coords$lon, label = pending_label()
      ))
      current_page("pick_end")
    } else if (current_page() == "settings") {
      # location updated from settings — stay on settings
      current_page("settings")
    } else {
      to_coords(list(
        lat = coords$lat, lon = coords$lon, label = pending_label()
      ))
      current_page("route")
    }
  })

  # go back to pick start from pick end
  observeEvent(input$back_btn, {
    to_coords(NULL)
    current_page("pick_start")
  })

  # reset coords and go back to pick_start for a new route
  observeEvent(input$new_route_btn, {
    from_coords(NULL)
    to_coords(NULL)
    current_page("pick_start")
  })

  # clear session state and return to login
  observeEvent(input$logout_btn, {
    current_user(NULL)
    from_coords(NULL)
    to_coords(NULL)
    current_page("login")
  })

  # to_* location buttons — same logic as from_* but stores to to_coords
  observeEvent(input$to_home, {
    if ("home" %in% get_locations(current_user(), example = TRUE)$label) {
      # prevent routing from and to the same location
      if (!is.null(from_coords()) && from_coords()$label == "home") {
        showNotification(
          "Start and end can't be the same location.",
          type = "warning"
        )
        return()
      }
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "home", ]
      to_coords(list(lat = match$lat[1], lon = match$lon[1], label = "home"))
      current_page("route")
    } else {
      pending_label("home")
      showModal(modalDialog(
        title = "Add Home Address",
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

  observeEvent(input$to_work, {
    if ("work" %in% get_locations(current_user(), example = TRUE)$label) {
      # prevent routing from and to the same location
      if (!is.null(from_coords()) && from_coords()$label == "work") {
        showNotification(
          "Start and end can't be the same location.",
          type = "warning"
        )
        return()
      }
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "work", ]
      to_coords(list(lat = match$lat[1], lon = match$lon[1], label = "work"))
      current_page("route")
    } else {
      pending_label("work")
      showModal(modalDialog(
        title = "Add Work Address",
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

  observeEvent(input$to_education, {
    if ("education" %in% get_locations(current_user(), example = TRUE)$label) {
      # prevent routing from and to the same location
      if (!is.null(from_coords()) && from_coords()$label == "education") {
        showNotification(
          "Start and end can't be the same location.",
          type = "warning"
        )
        return()
      }
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "education", ]
      to_coords(list(
        lat = match$lat[1], lon = match$lon[1], label = "education"
      ))
      current_page("route")
    } else {
      pending_label("education")
      showModal(modalDialog(
        title = "Add Education Address",
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

  observeEvent(input$to_friends, {
    if ("friends" %in% get_locations(current_user(), example = TRUE)$label) {
      # prevent routing from and to the same location
      if (!is.null(from_coords()) && from_coords()$label == "friends") {
        showNotification(
          "Start and end can't be the same location.",
          type = "warning"
        )
        return()
      }
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "friends", ]
      to_coords(list(lat = match$lat[1], lon = match$lon[1], label = "friends"))
      current_page("route")
    } else {
      pending_label("friends")
      showModal(modalDialog(
        title = "Add Friends Address",
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

  observeEvent(input$to_sports, {
    if ("sports" %in% get_locations(current_user(), example = TRUE)$label) {
      # prevent routing from and to the same location
      if (!is.null(from_coords()) && from_coords()$label == "sports") {
        showNotification(
          "Start and end can't be the same location.",
          type = "warning"
        )
        return()
      }
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "sports", ]
      to_coords(list(lat = match$lat[1], lon = match$lon[1], label = "sports"))
      current_page("route")
    } else {
      pending_label("sports")
      showModal(modalDialog(
        title = "Add Sports Address",
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

  observeEvent(input$to_music, {
    if ("music" %in% get_locations(current_user(), example = TRUE)$label) {
      # prevent routing from and to the same location
      if (!is.null(from_coords()) && from_coords()$label == "music") {
        showNotification(
          "Start and end can't be the same location.",
          type = "warning"
        )
        return()
      }
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "music", ]
      to_coords(list(lat = match$lat[1], lon = match$lon[1], label = "music"))
      current_page("route")
    } else {
      pending_label("music")
      showModal(modalDialog(
        title = "Add Music Address",
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

  observeEvent(input$to_custom1, {
    if ("custom1" %in% get_locations(current_user(), example = TRUE)$label) {
      # prevent routing from and to the same location
      if (!is.null(from_coords()) && from_coords()$label == "custom1") {
        showNotification(
          "Start and end can't be the same location.",
          type = "warning"
        )
        return()
      }
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "custom1", ]
      to_coords(list(lat = match$lat[1], lon = match$lon[1], label = "custom1"))
      current_page("route")
    } else {
      pending_label("custom1")
      showModal(modalDialog(
        title = "Add Custom 1 Address",
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

  observeEvent(input$to_custom2, {
    if ("custom2" %in% get_locations(current_user(), example = TRUE)$label) {
      # prevent routing from and to the same location
      if (!is.null(from_coords()) && from_coords()$label == "custom2") {
        showNotification(
          "Start and end can't be the same location.",
          type = "warning"
        )
        return()
      }
      locs <- get_locations(current_user(), example = TRUE)
      match <- locs[locs$label == "custom2", ]
      to_coords(list(lat = match$lat[1], lon = match$lon[1], label = "custom2"))
      current_page("route")
    } else {
      pending_label("custom2")
      showModal(modalDialog(
        title = "Add Custom 2 Address",
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


  # open settings and remember which page we came from
  observeEvent(input$settings_btn, {
    previous_page(current_page())
    current_page("settings")
  })

  # return to previous page
  observeEvent(input$settings_back_btn, {
    current_page(previous_page())
  })

  # settings location buttons — always show modal to add or update address
  observeEvent(input$settings_home, {
    pending_label("home")
    showModal(modalDialog(
      title = "Update Home Address",
      textInput("new_address", "Address",
                placeholder = "e.g. Roetersstraat, Amsterdam"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_address_btn", "Save",
                                    class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$settings_work, {
    pending_label("work")
    showModal(modalDialog(
      title = "Update Work Address",
      textInput("new_address", "Address",
                placeholder = "e.g. Roetersstraat, Amsterdam"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_address_btn", "Save",
                                    class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$settings_education, {
    pending_label("education")
    showModal(modalDialog(
      title = "Update Education Address",
      textInput("new_address", "Address",
                placeholder = "e.g. Roetersstraat, Amsterdam"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_address_btn", "Save",
                                    class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$settings_friends, {
    pending_label("friends")
    showModal(modalDialog(
      title = "Update Friends Address",
      textInput("new_address", "Address",
                placeholder = "e.g. Roetersstraat, Amsterdam"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_address_btn", "Save",
                                    class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$settings_sports, {
    pending_label("sports")
    showModal(modalDialog(
      title = "Update Sports Address",
      textInput("new_address", "Address",
                placeholder = "e.g. Roetersstraat, Amsterdam"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_address_btn", "Save",
                                    class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$settings_music, {
    pending_label("music")
    showModal(modalDialog(
      title = "Update Music Address",
      textInput("new_address", "Address",
                placeholder = "e.g. Roetersstraat, Amsterdam"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_address_btn", "Save",
                                    class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$settings_custom1, {
    pending_label("custom1")
    showModal(modalDialog(
      title = "Update Custom 1 Address",
      textInput("new_address", "Address",
                placeholder = "e.g. Roetersstraat, Amsterdam"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_address_btn", "Save",
                                    class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$settings_custom2, {
    pending_label("custom2")
    showModal(modalDialog(
      title = "Update Custom 2 Address",
      textInput("new_address", "Address",
                placeholder = "e.g. Roetersstraat, Amsterdam"),
      footer = tagList(modalButton("Cancel"),
                       actionButton("confirm_address_btn", "Save",
                                    class = "btn-primary")),
      easyClose = TRUE
    ))
  })

  # save cycling speed — validate input, persist, and invalidate speed cache
  observeEvent(input$settings_speed_save, {
    speed <- input$settings_speed_input
    if (is.na(speed) || speed < 1 || speed > 100) {
      showNotification("Please enter a speed between 1 and 100 km/h.",
                       type = "warning", duration = 4)
      return()
    }
    cycling_speed(current_user(), speed, example = TRUE) # save speed
    speed_version(speed_version() + 1)                  # invalidate speed cache
    showNotification("Cycling speed saved!", type = "message", duration = 2)
  })


  # settings tolerance buttons — update tolerance and stay on settings
  observeEvent(input$settings_tol_none, {
    rain_preference(current_user(), "none", example = TRUE)
    tolerance_version(tolerance_version() + 1) # invalidate tolerance cache
  })

  observeEvent(input$settings_tol_light, {
    rain_preference(current_user(), "light", example = TRUE)
    tolerance_version(tolerance_version() + 1)
  })

  observeEvent(input$settings_tol_moderate, {
    rain_preference(current_user(), "moderate", example = TRUE)
    tolerance_version(tolerance_version() + 1)
  })

  observeEvent(input$settings_tol_heavy, {
    rain_preference(current_user(), "heavy", example = TRUE)
    tolerance_version(tolerance_version() + 1)
  })


  # ── Reactives ───────────────────────────────────────────────────────────────

  # load user's rain tolerance — re-runs when tolerance_version changes
  tolerance <- reactive({
    tolerance_version()
    req(current_user())
    rain_preference(current_user(), example = TRUE)
  })

  # load user's cycling speed — falls back to 15 km/h if not yet set
  user_speed <- reactive({
    speed_version()
    req(current_user())
    s <- cycling_speed(current_user(), example = TRUE)
    if (length(s) == 0 || is.na(s)) 15 else s
  })


  # call bikeroute when both start and end are set
  route_data <- reactive({
    req(from_coords(), to_coords()) # only run if both set
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


  # call raintracker, not for heavy tolerance users
  rain_result <- reactive({
    req(route_data(), tolerance()) # both above need to be loaded first
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


  # build rain advice message based on raintracker result
  output$rain_advice <- renderUI({
    req(tolerance())

    # return and end if heavy tolerance
    if (tolerance() == "heavy") {
      return(h3("Built different. Just ride."))
    }

    # save rain result for user
    req(rain_result())
    result <- rain_result()

    # pick message based on rain result
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


  # render rain plot — plot_rain handles NULL data internally
  output$rain_plot <- renderPlot({
    req(rain_result())
    plot_rain(rain_result()$route_rain_summary, tolerance())
  })


}


# ── Launch ────────────────────────────────────────────────────────────────────
# This line wires the UI and server together and starts the app.

shinyApp(ui, server)
