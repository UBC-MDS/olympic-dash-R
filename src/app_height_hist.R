library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(purrr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- readr::read_csv(here::here('data', 'olympics_data.csv'))

app$layout(
    dbcContainer(
        list(
            dccGraph(id='plot-area'),
            dccDropdown(
                id='sport-select',
                options = as.list(unique(df$sport)) %>%
                    purrr::map(function(col) list(label = col, value = col)), 
                value='Judo')
        )
    )
)

app$callback(
    output('plot-area', 'figure'),
    list(input('sport-select', 'value')),
    function(sport_sel) {
        df_filtered <- df %>%
            filter(sport == sport_sel) %>%
            filter(year >= 1990)
        p <- ggplot(df_filtered, aes(x = height)) +
            geom_histogram() +
            scale_x_log10() +
            ggthemes::scale_color_tableau()
        ggplotly(p)
    }
)

app$run_server(host = '0.0.0.0')
