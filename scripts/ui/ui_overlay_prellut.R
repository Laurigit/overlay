
tabItem(tabName = "tab_overlay_prellut",
        fluidPage(
          fluidRow(
            column(2,
                   fluidRow(uiOutput("overlay_left_col")
                            )),
            column(2, offset = 8,
                   fluidRow(   uiOutput("overlay_right_col"))

            )))
)
