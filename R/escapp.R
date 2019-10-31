#' function to launch the lidar app
#'
#' @importFrom magrittr %>%
#'
#' @export
esc_app <- function() {

  ### Language input ###########################################################
  shiny::addResourcePath(
    'images', system.file('resources', 'images', package = 'escApp')
  )
  lang_choices <- c('cat', 'spa', 'eng')
  lang_flags <- c(
    glue::glue("<img class='flag-image' src='images/cat.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/spa.png' width=20px><div class='flag-lang'>%s</div></img>"),
    glue::glue("<img class='flag-image' src='images/eng.png' width=20px><div class='flag-lang'>%s</div></img>")
  )

  ## UI ####
  ui <- shiny::tagList(
    # shinyjs
    shinyjs::useShinyjs(),

    # css
    shiny::tags$head(
      # custom css
      shiny::includeCSS(
        system.file('resources', 'escapp.css', package = 'escApp')
      ),
      # corporative image css
      shiny::includeCSS(
        system.file('resources', 'corp_image.css', package = 'escApp')
      )
    ),

    navbarPageWithInputs(
      # opts
      title = 'Ecosystem Services App',
      id = 'nav',
      collapsible = TRUE,

      # navbar with inputs (helpers.R) accepts an input argument, we use it for the lang
      # selector
      inputs = shinyWidgets::pickerInput(
        'lang', NULL,
        choices = lang_choices,
        selected = 'cat',
        width = '100px',
        choicesOpt = list(
          content = c(
            sprintf(lang_flags[1], lang_choices[1]),
            sprintf(lang_flags[2], lang_choices[2]),
            sprintf(lang_flags[3], lang_choices[3])
          )
        )
      ),

      # navbarPage contents
      shiny::tabPanel(
        title = 'Explore',
        ########################################################### debug ####
        # shiny::absolutePanel(
        #   id = 'debug', class = 'panel panel-default', fixed = TRUE,
        #   draggable = TRUE, width = 640, height = 'auto',
        #   # top = 100, left = 100, rigth = 'auto', bottom = 'auto',
        #   # top = 'auto', left = 'auto', right = 100, bottom = 100,
        #   top = 60, left = 'auto', right = 50, bottom = 'auto',
        #
        #   shiny::textOutput('debug1'),
        #   shiny::textOutput('debug2'),
        #   shiny::textOutput('debug3')
        # ),
        ####################################################### end debug ####

        # we need an UI beacuse we need to translate based on the lang input from the
        # navbar
        shiny::uiOutput('explore_ui')

      ) # end of tabPanel "Explore"
    ) # end of navbarwithinputs
  ) # end of ui (tagList)

  ## SERVER ####
  server <- function(input, output, session) {
    ## debug #####
    # output$debug1 <- shiny::renderPrint({
    #   input$
    # })
    # output$debug2 <- shiny::renderPrint({
    #   input$
    # })
    # output$debug3 <- shiny::renderPrint({
    #   input$
    # })

    ## lang reactive ####
    lang <- shiny::reactive({
      input$lang
    })

    ## explore UI (to use lang) ####
    output$explore_ui <- shiny::renderUI({

      # lang
      lang_declared <- lang()

      # proper UI ####
      shiny::fluidPage(
        shiny::sidebarLayout(

          sidebarPanel = shiny::sidebarPanel(
            width = 4,
            # title
            # shiny::h4(translate_app('sidebar_h4_title', lang_declared)),
            shiny::h4('Settings'),

            # level input
            shiny::selectInput(
              'es_level_sel',
              # translate_app('es_level_sel_label', lang_declared),
              'Select level of detail',
              choices = c('municipality', 'plot'), #%>%
                # magrittr::set_names(translate_app(., lang_declared)),
              selected = 'municipality'
            ),

            # variable input
            shiny::selectInput(
              'es_var_sel',
              # translate_app('es_var_sel_label', lang_declared),
              'Select the service',
              choices = c(
                "mushroom", "runoff", "wood", "cover_natural", "cover_riparian",
                "cover_slope", "sink_c", "water_store", "stock_c", "animal_obs",
                "wikiloc", "turism", "nw2000", "rich_birds", "rich_trees"
              ), #%>%
              # magrittr::set_names(translate_app(., lang_declared)),
              selected = 'wood'
            ),

            # metric input
            shiny::selectInput(
              'es_metric_sel',
              # translate_app('es_var_sel_label', lang_declared),
              'Select the metric',
              choices = c(
                "raw", "ranked"
              ), #%>%
              # magrittr::set_names(translate_app(., lang_declared)),
              selected = 'ranked'
            )
          ), # end of sidebar panel

          mainPanel = shiny::mainPanel(
            width = 8,
            leaflet::leafletOutput('services_map', height = 600) %>%
              shinyWidgets::addSpinner(spin = 'cube', color = '#26a65b')
          ) # end of main panel
        ) # end of layout
      ) # end of fluidPage
    }) # end of explore_ui

    # data res reactive ####
    data_res <- shiny::reactive({

      shiny::validate(
        shiny::need(input$es_level_sel, 'no inputs')
      )

      data_res <- switch(
        input$es_level_sel,
        # 'plot' = plot_services,
        'municipalities' = municipalities_services
      )
      return(data_res)
    })

    # var name reactive ####
    var_name <- shiny::reactive({

      shiny::validate(
        shiny::need(input$es_var_sel, 'no inputs'),
        shiny::need(input$es_metric_sel, 'no inputs')
      )

      metric <- switch(
        input$es_metric_sel,
        'ranked' = '_ranked',
        'raw' = ''
      )

      var_name <- glue::glue("{input$es_var_sel}{metric}")

      return(var_name)
    })

    ## map output ####
    output$services_map <- leaflet::renderLeaflet({

      shiny::validate(
        # shiny::need(data_res(), translate_app('data_res_need', lang()))
        shiny::need(data_res(), 'no data'),
        shiny::need(var_name(), 'no var_name')
      )

      lang_declared <- lang()

      # data
      data_color_map <- data_res()
      

      palette <- leaflet::colorNumeric(
        viridis::plasma(100),
        # raster::values(basal_area_raster),
        raster::values(lidar_raster),
        na.color = 'transparent'
      )

      # poly intermediates
      poly_type <- input$poly_type_sel
      var_column <- glue::glue('mean_{tolower(input$lidar_var_sel)}')
      user_poly <- data_res() %>%
        sf::st_transform('+proj=longlat +datum=WGS84') %>%
        dplyr::select(poly_id, !! rlang::sym(var_column))

      # proper map
      leaflet::leaflet() %>%
        leaflet::setView(1.744, 41.726, zoom = 8) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldShadedRelief,
          group = 'Relief' %>% translate_app(lang_declared),
          options = leaflet::providerTileOptions(
            # zIndex = -1
          )
        ) %>%
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = 'Imaginery' %>% translate_app(lang_declared),
          options = leaflet::providerTileOptions(
            # zIndex = -1
          )
        ) %>%
        leaflet::addMapPane('polys', zIndex = 410) %>%
        leaflet::addMapPane('rasters', zIndex = 420) %>%
        leaflet::addLayersControl(
          baseGroups = c('Relief', 'Imaginery') %>% translate_app(lang_declared),
          overlayGroups = c('lidar', 'poly') %>%
            translate_app(lang_declared) %>%
            purrr::map_chr(~ glue::glue(.x)),
          options = leaflet::layersControlOptions(collapsed = FALSE, autoZIndex = FALSE)
        ) %>%
        leaflet::hideGroup('lidar' %>% translate_app(lang_declared)) %>%
        leaflet::removeImage('raster') %>%
        leaflet::clearGroup('poly' %>%
                              translate_app(lang_declared) %>%
                              purrr::map_chr(~ glue::glue(.x))) %>%
        leaflet::addRasterImage(
          lidar_raster, project = FALSE, colors = palette, opacity = 1,
          group = 'lidar' %>% translate_app(lang_declared), layerId = 'raster'
        ) %>%
        leaflet::addPolygons(
          data = user_poly,
          group = 'poly' %>%
            translate_app(lang_declared) %>%
            purrr::map_chr(~ glue::glue(.x)),
          label = ~poly_id,
          layerId = ~poly_id,
          weight = 1, smoothFactor = 1,
          opacity = 1.0, fill = TRUE,
          color = '#6C7A89FF', fillColor = palette(user_poly[[var_column]]),
          fillOpacity = 0.7,
          highlightOptions = leaflet::highlightOptions(
            color = "#CF000F", weight = 2,
            bringToFront = FALSE
          ),
          options = leaflet::pathOptions(
            pane = 'polys'
          )
        ) %>%
        leaflet::addLegend(
          pal = palette, values = raster::values(lidar_raster),
          title = input$lidar_var_sel %>% translate_app(lang_declared), position = 'bottomright',
          opacity = 1
        ) %>%
        # leaflet.extras plugins
        leaflet.extras::addDrawToolbar(
          targetGroup = 'poly' %>%
            translate_app(lang_declared) %>%
            purrr::map_chr(~ glue::glue(.x)),
          position = 'topleft',
          polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE,
          markerOptions = FALSE, circleMarkerOptions = FALSE,
          polygonOptions = leaflet.extras::drawPolygonOptions(
            shapeOptions = leaflet.extras::drawShapeOptions()
          ),
          editOptions = leaflet.extras::editToolbarOptions(
            edit = TRUE, remove = TRUE
          ),
          singleFeature = TRUE
        )
    }) # end of map output


  } # end of server function

}
