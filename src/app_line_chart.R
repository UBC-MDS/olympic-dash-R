library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(dplyr)
library(readr)
library(tidyr)
library(jsonlite)
library(ggplot2)
library(ggthemes)
library(plotly)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

raw_data <- na.omit(readr::read_csv(here::here('data/raw', 'olympics_data.csv')))
noc_list <- sort(unique(raw_data$noc))

app$layout(
  dbcContainer(
    list(
      dccRadioItems(
        id = "season",
        options = list(
          list(label = "Summer", value = "summer"),
          list(label = "Winter", value = "winter"),
          list(label = "All", value = "all")
        ),
        value = "all"
      ),
      dccChecklist(
        id = "medal_type",
        options = list(
          list(label = "Gold", value = "Gold"),
          list(label = "Silver", value = "Silver"),
          list(label = "Bronze", value = "Bronze")
        ),
        value = list("Gold", "Silver", "Bronze")
      ),
      dccGraph(id = "line_chart"),
      dccDropdown(
        id = "noc-select",
        options = noc_list %>%
          purrr::map(function(col) list(label = col, value = col)),
        value = "CAN"
      ),
      dccStore(id='filter_data')
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
                y = "Count of medals",
                title = "Medals Earned Over Time"
            )
        ggplotly(line_chart)

    }
)

# app$run_server(host = '0.0.0.0')
app$run_server(debug = T)