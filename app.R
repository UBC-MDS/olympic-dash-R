library(dash)
library(readr)
library(dplyr)
library(tidyr)
library(jsonlite)
library(ggplot2)
library(ggthemes)
library(plotly)
library(dashHtmlComponents)

raw_data <- na.omit(readr::read_csv(here::here('data/raw', 'olympics_data.csv')))
noc_list <- sort(unique(raw_data$noc))

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  dbcContainer(
    dbcRow(
      list(
        htmlDiv(
          htmlH1("Olympics Dashboard")
        ),
        dbcCol(
          list(
            htmlH3("Season"),
            dccRadioItems(
              id = 'season',
              options = list(list(label = 'Summer', value = 'Summer'),
                             list(label = 'Winter', value = 'Winter'),
                             list(label = "All", value = "all")),
              value = 'all',
              labelStyle = list("display" = "block")),
            htmlH3("Medal Type"),
            dccChecklist(
              id = 'medal_type',
              options = list(list(label = 'Gold', value = 'Gold'),
                             list(label = 'Silver', value = 'Silver'),
                             list(label = 'Bronze', value = 'Bronze')),
              value = list('Gold', 'Silver', 'Bronze'),
              labelStyle = list("display" = "block")),
            htmlH3("Year"),
            dccSlider(
              id = 'medals_by_country',
              value = 2000,
              min = 1896,
              max = 2016,
              step = 2,
              marks = list(
                '1896' = '1896', 
                '1936' = '1936', 
                '1976' = '1976', 
                '2016' = '2016'
              ),
              tooltip = list(placement = 'bottom', always_visible = TRUE),
              included = FALSE),
            dccStore(id='filter_data')
          ), width = 2
        ),
        dbcCol(
          list(
            htmlH3("Medals Earned by Country"),
            dccGraph(id='bubble'),
            htmlBr(),
            htmlBr(),
            htmlH3("Athlete Height Distribution"),
            dccGraph(id='plot-area'),
            dccDropdown(
              id='sport-select',
              options = as.list(unique(raw_data$sport)) %>%
                purrr::map(function(col) list(label = col, value = col)), 
              value='Judo')
          ), width = 5
        ),
        dbcCol(
          list(
            htmlH3("Medals Earned by Athlete Age"),
            dccGraph(id='age_hist'),
            dccRangeSlider(
              id='age_slider',
              min=0,
              max=75,
              step=2,
              value=list(0, 80),
              marks = list(
                '0' = '0', 
                '20' = '20', 
                '40' = '40', 
                '60' = '60',
                '80' = '80'
              )),
            htmlH3("Medals Earned Over Time"),
            dccGraph(id = "line_chart"),
            dccDropdown(
              id = "noc-select",
              options = noc_list %>%
                purrr::map(function(col) list(label = col, value = col)),
              value = "CAN"
            )
          ), width = 5
        )
      )
    )
  )
)

app$callback(
  output('filter_data', 'data'),
  list(input('season', 'value'),
       input('medal_type', 'value')),
  function(sel_season, medal_type) {
    temp <- raw_data
    
    if (sel_season != 'all') {
      temp <- temp %>%
        filter(season == sel_season)
    }
    
    if (length(medal_type) > 0) {
      temp <- drop_na(temp) %>%
        filter(medal %in% medal_type)
    } else {
      temp <- drop_na(temp)
    }
    
    temp %>% toJSON()
  }
)

app$callback(
  output('bubble', 'figure'),
  list(input('filter_data', 'data'),
       input('medals_by_country', 'value')),
  function(data, sel_year){
    temp <- fromJSON(data, simplifyDataFrame = TRUE)
    sel_year <- as.integer(sel_year)
    
    temp <- temp %>%
      filter(year == sel_year)
    
    athletes <- raw_data %>%
      filter(year == sel_year)
    
    athletes <- athletes %>%
      group_by(noc) %>%
      summarise(athletes = n_distinct(id))
    
    medals <- temp %>%
      group_by(noc) %>%
      summarise(metal_count = n())
    
    graph_data <- merge(athletes, medals)
    
    graph_data <- graph_data %>%
      mutate(ave_metals = metal_count / athletes)
    
    p <- ggplot(graph_data) +
      geom_point(aes(x = athletes,
                     y = ave_metals,
                     size = metal_count),
                 stat = "identity") +
      scale_color_tableau()
    
    ggplotly(p + aes(text = noc), tooltip = list('noc', 'size'))
  }
)

app$callback(
  output('age_hist', 'figure'),
  list(input('filter_data', 'data'),
       input('age_slider', 'value'),
       input('medals_by_country', 'value')
       ),
  function(data, age_range, sel_year) {
    minage = age_range[1]
    maxage = age_range[2]
    sel_year <- as.integer(sel_year)
    
    temp <- fromJSON(data, simplifyDataFrame = TRUE)

    temp <- temp %>%
      filter(year == sel_year) %>%
      filter(between(age, minage, maxage))
    
    temp$medal <- factor(temp$medal, levels=c("Gold", "Silver","Bronze"))
    
    p <- ggplot(temp) +
      geom_histogram(aes(x=age, fill=medal), bins=30) + 
      scale_fill_manual(values = c('#FFD700', '#C0C0C0', '#CD7F32')) +
      xlab("Age range") + 
      ylab("medals earned")
    ggplotly(p)
  }
)

app$callback(
  output('plot-area', 'figure'),
  list(input('sport-select', 'value'),
       input('filter_data', 'data'),
       input('medals_by_country', 'value')),
  function(sport_sel, data, sel_year) {
    
    sel_year <- as.integer(sel_year)
    
    temp <- fromJSON(data, simplifyDataFrame = TRUE)
    
    temp <- temp %>%
      filter(year == sel_year) %>%
      filter(sport == sport_sel)
    
    p <- ggplot(temp, aes(x = height)) +
      geom_histogram() +
      scale_x_log10() +
      ggthemes::scale_color_tableau()
    ggplotly(p)
  }
)

app$callback(
  output('line_chart', 'figure'),
  list(input('filter_data', 'data'),
       input('noc-select', 'value')),
  function(data, noc1) {
    olympic_data_line_chart <- fromJSON(data, simplifyDataFrame = TRUE) %>%
      subset(noc == noc1)
    
    line_chart <- olympic_data_line_chart %>%
      ggplot(aes(x = year)) +
      geom_line(stat = 'count') +
      labs(
        x = "Year",
        y = "Count of medals"
      )
    ggplotly(line_chart)
    
  }
)

# app$run_server(debug = T, host = '0.0.0.0')
app$run_server(debug = T)
