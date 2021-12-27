require(shiny)
require(tidyverse)
require(tsibble)
require(tmap)
require(sf)
require(DT)
require(plotly)

set.seed(124)

## basemap
africa = st_read("data/africa.shp", quiet = TRUE)

pemba.sf = africa %>%  
  st_crop(xmin = 38, xmax = 41, ymin = -8, ymax = -2)

## color codes
mycolor2 = c("#040ED8", "#2050FF", "#4196FF", "#6DC1FF", "#86D9FF", "#9CEEFF", "#AFF5FF", "#CEFFFF", "#FFFE47", "#FFEB00", "#FFC400", "#FF9000", "#FF4800", "#FF0000", "#D50000", "#9E0000")



## Sea level data processing
sea.level = read_csv("data/zanzibar_monthly.txt", col_names = T) 

# sea.level = sea.level%>% 
#   separate(col = 1, into = c("date.d", "a"), sep = ";")%>% 
#   separate(col = 3, into = c("level_mm", "a"), sep = ";")  %>% 
#   mutate(date = as.numeric(date.d),
#          date = lubridate::date_decimal(date) %>% lubridate::as_date(),
#          level_mm = as.numeric(level_mm))%>% 
#   select(date.d,date, level_mm)


## Tuna EEZ Data
tunas = read_csv("data/tunas.csv") 

# %>% 
#   mutate(category_name = str_remove_all(string = category_name, pattern = "Tuna"), 
#          date = lubridate::mdy(fishing_date),
#          month = lubridate::month(date, label = TRUE, abbr = TRUE),
#          season = lubridate::month(date), season = if_else(season %in% c(5:9), true = "SE", false = "NE"),
#          weight = na_if(x = weight, y = 0))


flag = tunas %>% distinct(flag_state) %>% pull()
species = tunas %>% distinct(category_name) %>% pull()
gear = tunas %>% distinct(gear) %>% pull()
years = tunas %>% distinct(year) %>% pull()
n  = nrow(tunas)


## ringnet fishery
ringnet = read_csv("data/ringnet_cpue_all.csv") %>% 
  filter(between(lon,38, 42), between(lat, -13, -4))

ring.sf = ringnet %>% drop_na(lon)  %>% st_as_sf(coords = c("lon", "lat"), crs = 4326)
# ringnet %>% glimpse()
fish.groups = ringnet %>% distinct(Groups) %>% pull()
  

## upwelling phenomenon in Tanga
upwelling = read_csv("data/upwelling_events.csv")


## Chumbe Temperature
sst.chumbe = read_csv("data/sst_chumbe.csv")

sst.chumbe.long = sst.chumbe %>% 
  pivot_longer(cols = 2:23, values_to = "sst", names_to = "year") %>% 
  unite(1:2, col = "time", sep = "-", remove = TRUE) %>% 
  mutate(date = lubridate::dmy(time)) %>% 
  arrange(date)

index = which(is.na(sst.chumbe.long$date))

sst.chumbe.long = sst.chumbe.long %>% 
  slice(-index) %>% 
  mutate(doy = lubridate::yday(date),  year = lubridate::year(date)) %>% 
  relocate(sst, .after = year)



## MSP data
## data

channels = st_read("data/channels.shp", quiet = TRUE) 
channel.name = channels %>% distinct(channels) %>% pull()

landing.sites = st_read("data/fish_landing_sites.shp", quiet = TRUE)
iba = st_read("data/iba.shp", quiet = TRUE)
coelacanth = st_read("data/coelacanth.shp", quiet = TRUE)
dolphin = st_read("data/dolphin.shp", quiet = TRUE)
dugong = st_read("data/dugong.shp", quiet = TRUE)
prawn.culture =  st_read("data/prawn_culture.shp", quiet = TRUE)
ports =  st_read("data/ports.shp", quiet = TRUE)
mpa = st_read("data/mpa.shp", quiet = TRUE)
coastal_features =  st_read("data/coastal_features.shp", quiet = TRUE) %>% 
  janitor::clean_names() 

coastal.only = coastal_features %>% 
  filter(class %in% c("Mangrove", "Salt marsh", "Tidal reef", "Salt pan", "Saline bare area", "Sand", "Seagrass", "Swamp") )

tmap_options(check.and.fix = TRUE)





ui = fluidPage(
  titlePanel(title = tags$h1("Marine Visualization Hub of Tanzania")),
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),#shinythemes::shinytheme(theme = "journal"),
  
  
  sidebarLayout(
    sidebarPanel(width = 2,
      tags$h2("Today's Outlook"),
      # tags$br(),
      p("This Data Driven Web Application Tool was developed by the Institute of Marine Sciences in collaboration with the Nelson Mandela African Institution of Science and Technology with financial support from the GMES AFRICA. The tool aim to provide up-to date information of the coastal state. The information as developed is the data driven with an intention to support decision makers and managers of coastal and marine resources along the Territorial and Exclusive Economic Zone waters' of Tanzania"),
      tags$br(),
      tags$img(src = "coat.png", width = "150px", height = "172px"),
      tags$img(src = "udsm.png", width = "171px", height = "185px"),
      tags$img(src = "nmaist.png", width = "150px", height = "132px"),
      tags$img(src = "wiomsa.png", width = "200px", height = "130px"),
      tags$img(src = "dsfa.png", width = "150px", height = "169px"),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$br(),
      tags$p("Designed and Developed by"),
      tags$img(src = "wior.png", width = "150px", height = "169px")
    ),
    mainPanel(width = 10,
              tags$br(),
              tags$br(),      
              
      fluidRow(
        tags$h3("The Sea Level"),
        column(width = 2, 
               sliderInput(inputId = "sl", label = "Prediction Months", min = 0, max = 30, value = 15),
               sliderInput(inputId = "year", label = "Choose a Start Year", value = c(2001, 2018), min = 1980, max = 2020),
               helpText("The slide changes help the user to interact with the system and visualize the changes of the sea level as the machie predict")),
        column(width = 4, plotOutput(outputId = "sstplot")),
        column(width = 4, htmlOutput(outputId = "slTrend"), 
               helpText("Information based on Data From Zanzibar Harbour. A nonparametric test for a monotonic trend based on Kendall's tau statistic, and optionally compute a confidence interval for the slope"))
      ),
      tags$br(),
      tags$br(),
      tags$br(),
      fluidRow(
        tags$h3("Sea Surface Temperature"),
        tags$br(),
        tags$br(),
        column(width = 2, 
               dateRangeInput(inputId = "tsRange", label = "Select Date range", start = "2009-01-01", end = "2018-01-01"),
               sliderInput(inputId = "sstPred", label = "Prediction Months", min = 0, max = 30, value = 15),
               helpText("The Chumbe Island has been monitoring the sea surface temprature since 1997. With this data long term data, we can precisely asses the impact of raising temperature in vital coastal habitats like the coral reefs, seagrasses and mangrove forest.")
        ),
        column(width = 4, plotOutput(outputId = "clplot")),
        column(width = 4, plotOutput(outputId = "decompose"))

      ),
      tags$br(),
      tags$br(),
      tags$br(),
      fluidRow(
        tags$h3("Ringnet Fishery in the Territorial Water"),
        column(width = 2, 
               checkboxGroupInput(inputId = "ringfish", label = "Priority Fish", choices = fish.groups, selected = "Sardines"),
               dateRangeInput(inputId = "ringDate", label = "Fishing Window", start = "2019-01-01", end = lubridate::today()),
        ),
        column(width = 4, tmapOutput(outputId = "ringmap")),
        column(width = 4, DT::DTOutput(outputId = "ringtable"))
      ),
      tags$br(),
      tags$br(),
      tags$br(),
      fluidRow(
        tags$h3("Longline Fishery in the EEZ Water"),
        column(width = 2, 
               selectInput(inputId = "tunas", label = "Choose a Tuna Species", choices = species),
               sliderInput(inputId = "sampleTuna", label = "Select Sample Size", min = 0, max = 2000, value = 40),
               dateRangeInput(inputId = "fishDate", label = "Fishing Window", start = "2010-01-01", end = lubridate::today()),
               checkboxGroupInput(inputId = "season", label = "Choose season", choices = c("SE", "NE"), selected = "NE"),
               checkboxGroupInput(inputId = "gears", label = "Fishing Gear", choices = gear, selected = gear)
               # helpText("By simply pick the tuna species and slide the desired number of sample, you allow the tools to compute and display the total weight recorded at that particular fishing events and at particular location. The catch vary with size that is dispalyed as proportion of a bubble size")
               ),
        column(width = 5, tmapOutput(outputId = "tunamap")),
        column(width = 5, DT::DTOutput(outputId = "tunadata")),
      ),
      tags$br(),
      tags$br(),
      tags$br(),
      fluidRow(
        tags$h3("Upwelling Phenomenon in the Pemba Channel"),
        column(width = 2, checkboxGroupInput(inputId = "upwevents", label = "Pick a month", choices = month.abb, selected = c("Dec", "Jan", "Feb", "Mar"))),
        column(width = 8, plotOutput(outputId = "upwellingplot")),
      ),
      tags$br(),
      tags$br(),
      tags$br(),
      fluidRow(
        tags$h3("Habitats and Economic Features"),
        column(width = 2, selectInput(inputId = "channel", label = "Fishing Events by space", choices = channel.name, selected = NULL),
               checkboxGroupInput(inputId = "season1", label = "Monsoon season", choices = c("NE", "SE"), selected = c("SE"))),
        column(width = 4, tmapOutput(outputId = "msp")),
        column(width = 4, plotlyOutput(outputId = "pie"))
      ),
      tags$br(),
      tags$br(),
      tags$br()
    )
    
    
  )
  
  
 
)

server = function(input, output, session){
  
  my_range <- reactive({
    cbind(input$year[1],input$year[2])
  })
# sea level  
  ## fill the missing gaps and also filter the years of interest
  sea.leve.ts = reactive({
    sea.level %>% 
      select(level_mm) %>% 
      ts(start = c(1984,3), frequency = 12) %>% 
      # forecast::na.interp() %>% 
      imputeTS::na_random()     %>% 
      window(start = input$year[1], end = input$year[2])
    
    
  })
  
  output$sstplot = renderPlot({
    
    ## predict the sea level rise
    sea.leve.ts() %>% 
      autoplot()+
      geom_smooth(color = "red", fill = "red", alpha = 0.3, size = 1.2)+
      forecast::geom_forecast(h = input$sl)+
      labs(subtitle = "Sea level changes in Zanzibar Port",  y = "Sea level (mm)")+
      theme_bw()+
      theme(axis.title.x = element_blank(), axis.text = element_text(size = 12, color = 1), 
            axis.title = element_text(size = 14, color = 1, face = "bold"))
    
  })
  
  ## Quantify annual change (trend) in sea level
  output$slTrend = renderText({
    sea.leve.ts() %>% 
      TSstudio::ts_reshape(type = "long") %>% 
      as_tibble() %$% 
      EnvStats::kendallTrendTest(value ~ year) %>% 
      broom::glance() %>% 
      dplyr::select(1:5) %>% 
      kableExtra::kbl(digits = 3, col.names = c("Tau", "Slope", "Intercept", "Z", "p")) %>% 
      kableExtra::kable_styling() %>% 
      kableExtra::add_header_above(c("","","", "Statistic Values" = 2))
  })
# end of sea level 
  
# begin of  tuna manipulation
  tuna.pick = reactive({
    tunas %>% 
      filter(category_name == input$tunas & 
               season == input$season & 
               gear == input$gears &
               date > input$fishDate[1] & date <= input$fishDate[2]) %>% 
      sample_n(size = input$sampleTuna, replace = TRUE)
    
  })
  
  output$tunamap = renderTmap({
    
    tuna.pick()  %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
      tm_shape(name = input$tunas) + 
      tm_bubbles(size = "weight", style = "kmeans", col = "season")
  })
  
  output$tunadata = DT::renderDT({
    tuna.pick() %>% 
      select(year,season,lon,lat, species = category_name, weight) %>% 
      DT::datatable(rownames = FALSE, options = list(pageLength = 6))
  })
  
# end of tuna manipulation
  
# begin of ringnet data manipulation
  ringData = reactive({
    ringnet %>% 
      filter(Groups %in% input$ringfish & 
               begin_time > input$ringDate[1] & begin_time < input$ringDate[2]) %>% 
      drop_na(lon) %>% 
      drop_na(lat)
    
  })
  
  output$ringmap = renderTmap({
    ringData() %>% 
      st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
      tm_shape(name = input$ringfish) +
      tm_bubbles(size = "uzito", col = "Groups")
  })
  
  output$ringtable = DT::renderDT({
    
    ringData() %>% 
      dplyr::select(lon,lat,landing_site = mwalo_bandari_diko, 
                    fishing_ground = sehemu_aliyovua_locality, gear = aina_ya_mtego, species =aina_ya_samaki, weight = uzito ) %>% 
      DT::datatable(rownames = FALSE, options = list(pageLength = 6))
  })
  
# end of ringnet data manipulation
  
# begin of upweeling manipulation
  
  output$upwellingplot = renderPlot({
    
  upwdata = reactive({
  
  upwelling %>% 
    # mutate(sst.anomaly = wior::inverse_hyperbolic(sst.anomaly)) %>% 
    filter(between(longitude, 39,40) & between(latitude, -5.5,-4.3) & miezi %in% input$upwevents) 
    
    
  })
  
  upwdata() %>% 
    ggplot(aes(x = longitude, y = latitude, z = sst.anomaly))+
    metR::geom_contour_fill(bins = 20)+
    facet_wrap(~miezi, nrow = 2)+
    ggspatial::layer_spatial(data = pemba.sf)+
    coord_sf(xlim = c(39.1,39.8), ylim = c(-5., -4.4))+
    theme_bw() +
    theme(axis.title.x = element_blank(), axis.text = element_text(size = 12, color = 1), 
          axis.title = element_text(size = 14, color = 1, face = "bold"))+
    metR::scale_x_longitude(breaks = c(39.0,39.4,39.8))+
    metR::scale_y_latitude(breaks = c(-5.4,-5.0,-4.6,-4.2))+
    scale_fill_gradientn(colours = mycolor2,  
                         trans = scales::modulus_trans(p = .01), 
                         breaks = scales::breaks_width(.25),
                         guide = guide_colorbar(title = "Indices",
                                                title.position = "top", 
                                                title.theme = element_text(angle = 0),
                                                title.hjust = .5, 
                                                barheight = unit(4, "cm"),
                                                barwidth = unit(.4, "cm")))
  })
# end of upwelling manipulation
  
  ## begin of sea surface temperature
  
  sstReactive = reactive({
    sst.chumbe.long %>%
      filter(date > input$tsRange[1] & date < input$tsRange[2])
    
    
  })
  
  output$clplot = renderPlot({
    
    sstReactive()  %>%
      ggplot(aes(x = doy, y = year, z = sst))+
      metR::geom_contour_fill(bins = 8, na.fill = TRUE)+
      scale_x_continuous(breaks = scales::breaks_width(30), expand = c(0,0), name = "Day of Year")+
      scale_y_reverse(expand = c(0,0), name = "Year")+
      scale_fill_gradientn(colors = mycolor2,
                           trans = scales::modulus_trans(p = .1),
                           breaks = scales::breaks_width(width = 1.2, offset = .4),
                           guide = guide_colorbar(title.position = "right",
                                                  title.hjust = .5,
                                                  title = expression(SST~(degree*C)),
                                                  title.theme = element_text(angle = 90),
                                                  raster = TRUE,
                                                  barheight = unit(20,"lines"),
                                                  barwidth = unit(1.5, "lines")))+
      theme_bw() +
      theme(axis.text = element_text(size = 12, color = 1), 
            axis.title = element_text(size = 14, color = 1, face = "bold"), 
            legend.text = element_text(size = 11, color = 1))
  })
  
  
  output$decompose = renderPlot({
    # establish a year for which to create a ts that is dynamic based on choice
    mwanzo.year = sstReactive() %>% distinct(year) %>% pull()
    
    # sstReactive() %>% 
    #   mutate(month = lubridate::month(date)) %>% 
    #   group_by(month, year, .drop = TRUE) %>% 
    #   summarise(sst = median(sst, na.rm = TRUE)) %>% 
    #   # ungroup() %>% 
    #   arrange(year, month) %$% 
    #   ts(data = sst, start = c(1997,1), frequency = 12) %>% 
    #   imputeTS::na_random() %>% 
    #   decompose() %>% 
    #   autoplot()+
    #   theme_bw()+
    #   theme(axis.title.x = element_blank(), axis.text = element_text(size = 12, color = 1), 
    #         axis.title = element_text(size = 14, color = 1, face = "bold"))
    
    ## decompose ts object inorder to obtain a trend over the period
    decomp = sstReactive() %>%
      mutate(month = lubridate::month(date)) %>%
      group_by(month, year, .drop = TRUE) %>%
      summarise(sst = mean(sst, na.rm = TRUE)) %>%
      arrange(year, month) %$%
      ts(data = sst, start = c(mwanzo.year[1],1), frequency = 12) %>%
      imputeTS::na_random() %>% 
      decompose()
    
    trend.rate = decomp$trend %>% 
      TSstudio::ts_reshape(type = "long") %>% 
      as_tibble() %$% 
      EnvStats::kendallTrendTest(value ~ year) %>% 
      broom::glance() %>% 
      dplyr::select(rate = 2, significant = 5)
    
   decomp$trend %>%
     autoplot()+
     geom_smooth(color = "red", fill = "red", alpha = 0.3, size = 1.2)+
     forecast::geom_forecast(h = input$sstPred)+
     labs(y = expression(SST~(degree*C)), title = paste("The rate of change is ", trend.rate$rate))+
     theme_bw()+
     theme(axis.title.x = element_blank(), axis.text = element_text(size = 12, color = 1), 
           axis.title = element_text(size = 14, color = 1, face = "bold"))
     
  })
  
# end of the sst manipulation
  
  
  ring.channel = reactive({
    ring.sf %>% 
      filter(season == input$season1) %>% 
      st_join(channels %>% 
                filter(channels == input$channel), join = st_within) %>% 
      filter(!is.na(channels)) 
    
    
  })
  
  coords = reactive({
    channels %>% filter(channels == input$channel) %>% 
      st_centroid() %>% st_coordinates() %>% 
      as_tibble()
  })
  
  
  output$msp = renderTmap({
    
    
    tmap_mode(mode = "view")
    
    tm_shape(shp = iba, name = "Important Bird Areas")+
      tm_fill(col = "red", alpha = .1)+
      tm_shape(shp = mpa, name = "Marine Protected Areas")+
      tm_fill(col = "green", alpha = .1)+
      tm_shape(shp = coastal.only %>% dplyr::select(class), name = "Coastal Habitat") +
      tm_fill(col = "class", title = "Habitat Type", legend.show = FALSE) +
      tm_shape(shp = coelacanth %>% dplyr::select(NAME), name = "Coelacanth Sites") +
      tm_symbols(col = "blue", border.col = "darkblue", size = .02, alpha = .1)+
      tm_shape(shp = dolphin %>% dplyr::select(NAME), name = "Dolphin Sites")+
      tm_symbols(size = 0.02, col = "green", border.col = "green", alpha = .2)+
      tm_shape(shp = dugong %>% dplyr::select(Name), name = "Dugong Sites")+
      tm_symbols(size = 0.02, col = "maroon", border.col = "maroon", alpha = .2)+
      tm_shape(shp = ports %>% dplyr::select(Name), name = "Harbours and Ports")+
      tm_symbols(size = 0.02, col = "darkgreen", border.col = "darkgreen", alpha = .2)+
      # tm_shape(shp = landing.sites %>% dplyr::select(Name), name = "Landing Sites") +
      # tm_markers(clustering = TRUE, text ="Name" ,  text.just = "top",  markers.on.top.of.text = FALSE,  group = NA)+
      # tm_shape(shp = prawn.culture, name = "Prawn Culture") +
      # tm_markers(clustering = TRUE, text ="Name" ,  text.just = "top",  markers.on.top.of.text = FALSE,  group = NA)+
      tm_shape(shp = ring.channel(), name = input$channel) +
      tm_markers(clustering = TRUE)+
      tm_view(set.view = c(lon = coords()$X[1], lat = coords()$Y[1], zoom = 9))
  })
  
  output$pie = renderPlotly({
    
  ring.channel() %>% 
    st_drop_geometry()  %>% 
    group_by(Groups, season) %>% 
    summarise(count = n()) %>% 
    ungroup() %>%
    as_tibble() %>% 
    plot_ly(type = "pie", labels = ~Groups, values = ~count, hole = .5, 
            textinfo='label+percent',
            insidetextorientation='radial') %>% 
    layout(title = paste("Species Composition at ", input$channel, "Channel"),  
           showlegend = F,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, 
                        showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, 
                        zeroline = FALSE, showticklabels = FALSE))
    
  })
}
# end of server

shinyApp(ui, server)