#library(shiny)
#library(shinydashboard)

# Define UI for application that draws a histogram


uusi_peli <- dashboardBody(

  useShinyalert(),
  useShinyjs(),
 # extendShinyjs(text = jscode),

  extendShinyjs(functions = c("hidehead"), text = "shinyjs.hidehead = function(parm){
                                    $('header').css('display', parm);
                                }"),

  tags$head(
    tags$style(
      HTML("
           #myScrollBox{
           overflow-y: scroll;
           overflow-x: hidden;
           height:740px;
           }
           ")
      )
    ,

    tags$style(type = "text/css", "
      .irs-slider {width: 30px; height: 30px; top: 22px;}
    ")


      ),
  tabItems(
  #  source("./scripts/ui/ui_uusi_peli.R",local = TRUE)$value,
  #  source("./scripts/ui/ui_tallenna_peli.R",local = TRUE)$value,
  # source("./scripts/ui/ui_life_counter.R",local = TRUE)$value,

    #PALAUTA MINUT
    #etsi kaikki PALAUTA MINUT ctrl+f
  #   source("./scripts/ui/ui_overlay.R",local = TRUE)$value
    ####
    source("./scripts/ui/ui_overlay_prellut.R",local = TRUE)$value



  ))



#SIDEBAR
sidebar <- dashboardSidebar(
  sidebarMenu(id = "sidebarmenu",
              # menuItem("Uusi peli", tabName = "tab_uusi_peli", icon = icon("gamepad")),
              # menuItem("Tallenna peli", icon = icon("hdd"), tabName = "tab_tallenna_peli"),
              # menuItem("LifeCounter", tabName = "tab_LifeCounter", icon = icon("gamepad")),


              #PALAUTA MINUT
           # menuItem("Overlay", icon = icon("server"), tabName = "tab_overlay"),
                ################

            menuItem("Overlay", icon = icon("server"), tabName = "tab_overlay_prellut"),


              # actionButton("automated_tests", label = h5("Run tests")),
             actionButton("refresh", label = "Update data"),
             #radioButtons("radio_total_mode", label = h5("Total mode"),choices = list("Pois" = FALSE, "Paalla" = TRUE), selected = FALSE,inline=T),

             #radioButtons("radio_debug_mode", label = h5("Debug"),choices = list("Pois" = FALSE, "Paalla" = TRUE), selected = FALSE,inline=T),
               #div(style="display:inline-block;width:90%;text-align: center;",uiOutput("sarjataulukkovalitsin")),
             actionButton("loginbutton", "Login")
              #menuSubItem(icon = NULL,actionButton("luo_peleja","Luo uudet pelit"))
  )


)

#RUNKO
dashboardPage(
  title = "Overlay",

  #dashboardHeader(title = paste0("run_mode = ", GLOBAL_test_mode, " ", textOutput('blow_timer')),
#  dashboardHeader(title = textOutput('blow_timer'),
#                 titleWidth = 450),
  dashboardHeader(title = textOutput('Username')),

  sidebar,
  uusi_peli
)





