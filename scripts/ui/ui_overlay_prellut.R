
tabItem(tabName = "tab_overlay_prellut",
        fluidPage(
          fluidRow(
            tags$head(tags$style(HTML(".small-box {height: 94px}"))),
                  column(2, uiOutput("overlay_left_col"))
                            ,

                  column(2, offset = 8, uiOutput("overlay_right_col"))

            )
))
