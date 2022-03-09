library(shiny)
library(shinyWidgets)
library(ggplot2)
library(tidync)
library(dplyr)
library(googledrive)

# Bypass authentication for publicly available files
drive_deauth()
drive_user()

# This operation will take some time to download the requisite data set.
if (!file.exists("data")) {
  dir.create("data")
  drive_download("https://drive.google.com/uc?export=download&id=1s0LbIHZdnsOOKXpZKNccJXKQV9yySLzS", path = "./data/MAXT.nc")
  drive_download("https://drive.google.com/uc?export=download&id=1L2d2Rvaos1FnYjLVK1nkF9lft1sSxxI7", path = "./data/MINT.nc")
  drive_download("https://drive.google.com/uc?export=download&id=1OWR7_5kjKMc60dEFR6xC-5459pW6eflE", path = "./data/PREC.nc")
}

source("map-data.R")

#######################################################################################################
#######################################         SERVER         ########################################
#######################################################################################################
server <- function(input, output, session) {
  update <- list(
    timeFilter = reactive({ input$timeFilter }),
    coor = reactive({list(lon = input$lon, lat = input$lat)}),
    basedata = reactive({
      switch (
        input$baseData,
        "ERA-Interim" = {
          maxt.slc <-
            var.maxt %>% hyper_filter(time = between(
              as.Date(time, origin = "1979-01-01"),
              input$timeFilter,
              input$timeFilter
            )) %>% hyper_tibble()
          mint.slc <-
            var.mint %>% hyper_filter(time = between(
              as.Date(time, origin = "1979-01-01"),
              input$timeFilter,
              input$timeFilter
            )) %>% hyper_tibble()
          prec.slc <-
            var.prec %>% hyper_filter(time = between(
              as.Date(time, origin = "1979-01-01"),
              input$timeFilter,
              input$timeFilter
            )) %>% hyper_tibble()
          
          full_join(maxt.slc, mint.slc, by = c("lon", "lat", "time")) %>% full_join(prec.slc, by = c("lon", "lat", "time")) %>%
            mutate(DIFF = MAXT - MINT, lon = lon - 360) %>%
            select(lon, lat, MAXT, MINT, DIFF, PREC)
        },
      )
    }),
    linedata = reactive({
        time <- input$timeFilter
        multiplier <- input$trend
        range <- c(time - (360 * multiplier), time + (360 * multiplier))
        
        maxt.slc <-
          var.maxt %>% hyper_filter(
            lon = lon == input$lon + 360,
            lat = lat == input$lat,
            time = between(as.Date(time, origin =
                                     "1979-01-01"), range[1], range[2])
          ) %>% hyper_tibble()
        
        mint.slc <-
          var.mint %>% hyper_filter(
            lon = lon == input$lon + 360,
            lat = lat == input$lat,
            time = between(as.Date(time, origin =
                                     "1979-01-01"), range[1], range[2])
          ) %>% hyper_tibble()
        
        prec.slc <-
          var.prec %>% hyper_filter(
            lon = lon == input$lon + 360,
            lat = lat == input$lat,
            time = between(as.Date(time, origin =
                                     "1979-01-01"), range[1], range[2])
          ) %>% hyper_tibble()
        
        full_join(maxt.slc, mint.slc, by = c("lon", "lat", "time")) %>% full_join(prec.slc, by = c("lon", "lat", "time")) %>%
          mutate(DIFF = MAXT - MINT, lon = lon - 360) %>%
          select(time, lon, lat, MAXT, MINT, DIFF, PREC)
      })
  )
  
  output$title <- renderText(input$baseData)
  output$coor <- renderText(paste("lon:", input$plot_hover$x[1],
                                  "lat:", input$plot_hover$y[1]))
  
  observeEvent(input$plot_click, {
    x <- round((input$plot_click$x[1] + 136.5) / 0.75) * 0.75 - 136.5
    y <- round((input$plot_click$y[1] - 17.25) / 0.75) * 0.75 + 17.25
    
    updateSliderInput(session, "lon", value = x)
    updateSliderInput(session, "lat", value = y)
  })
  
  #### MAIN TAB
  output$maxt <- renderPlot({
    base <- update$basedata()
    
    ggplot(base) +
      ggtitle("Maximum") +
      geom_raster(aes(x = lon, y = lat, fill = MAXT), interpolate = TRUE) +
      scale_fill_distiller(palette = "Reds") +
      base.map +
      labs(x = NULL, y = NULL) +
      geom_point(
        x = update$coor()$lon,
        y = update$coor()$lat,
        color = "white",
        shape = "cross",
        size = 2
      ) +
      theme_bw()
  })
  output$mint <- renderPlot({
    base <- update$basedata()
    
    ggplot(base) +
      ggtitle("Minimum") +
      geom_raster(aes(x = lon , y = lat, fill = MINT), interpolate = TRUE) +
      base.map +
      labs(x = NULL, y = NULL) +
      geom_point(
        x = update$coor()$lon,
        y = update$coor()$lat,
        color = "white",
        shape = "cross",
        size = 2
      ) +
      theme_bw()
  })
  output$diff <- renderPlot({
    base <- update$basedata()
    
    ggplot(base) +
      ggtitle("Difference") +
      geom_raster(aes(x = lon, y = lat, fill = DIFF), interpolate = TRUE) +
      scale_fill_distiller(palette = "Purples") +
      base.map +
      labs(x = NULL, y = NULL) +
      geom_point(
        x = update$coor()$lon,
        y = update$coor()$lat,
        color = "white",
        shape = "cross",
        size = 2
      ) +
      theme_bw()
  })
  output$prec <- renderPlot({
    base <- update$basedata()
    
    ggplot(base) +
      ggtitle("Precipitation") +
      geom_raster(aes(x = lon, y = lat, fill = PREC), interpolate = TRUE) +
      scale_fill_distiller(palette = "Greens") +
      base.map +
      labs(x = NULL, y = NULL) +
      geom_point(
        x = update$coor()$lon,
        y = update$coor()$lat,
        color = "white",
        shape = "cross",
        size = 2
      ) +
      theme_bw()
  })
  
  output$line1 <- renderPlot({
    base <- update$linedata()
    
    ggplot(base) +
      geom_line(aes(x = as.Date(time, origin = "1979-01-01"), y = MAXT), color = "red") +
      scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month") +
      geom_vline(xintercept = update$timeFilter(),
                 color = "orange") +
      labs(x = NULL, y = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, vjust = .5))
  })
  output$line2 <- renderPlot({
    base <- update$linedata()
    
    ggplot(base) +
      geom_line(aes(x = as.Date(time, origin = "1979-01-01"), y = MINT), color = "blue") +
      scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month") +
      geom_vline(xintercept = update$timeFilter(),
                 color = "orange") +
      labs(x = NULL, y = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, vjust = .5))
  })
  output$line3 <- renderPlot({
    base <- update$linedata()
    
    ggplot(base) +
      geom_line(aes(
        x = as.Date(time, origin = "1979-01-01"),
        y = MAXT - MINT
      ), color = "purple") +
      scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month") +
      geom_vline(xintercept = update$timeFilter(),
                 color = "orange") +
      labs(x = NULL, y = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, vjust = .5))
  })
  output$line4 <- renderPlot({
    base <- update$linedata()
    
    ggplot(base) +
      geom_line(aes(x = as.Date(time, origin = "1979-01-01"), y = PREC), color = "green") +
      scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month") +
      geom_vline(xintercept = update$timeFilter(),
                 color = "orange") +
      labs(x = NULL, y = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, vjust = .5))
  })
  
  #### DATA TAB
  output$basedata <- renderDataTable({
    update$basedata()
  })
}

shinyApp(htmlTemplate("../app/template.html"), server, enableBookmarking = "url")
