library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(purrr)
library(gapminder)
library(dashHtmlComponents)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
  div(
    list(
      dbcRow(dbcCol(div("Gapminder line plot"))),
      dbcRow(
        list(
          dbcCol(
            list(
              dbcRow(
                dccDropdown(
                  id='target-select',
                  options = gapminder %>%
                    select(lifeExp,pop,gdpPercap) %>%
                    colnames() %>%
                    purrr::map(function(col) list(label = col, value = col)), 
                  value='lifeExp')
              ),
              dbcRow(
                dccDropdown(
                  id='continent-select',
                  options = levels(factor(gapminder$continent))%>%
                    purrr::map(function(col) list(label = col, value = col)), 
                  value='Americas')
              ),
              dbcRow(
                dccDropdown(
                  id='country-select',
                  options = levels(factor(gapminder$country))%>%
                    purrr::map(function(col) list(label = col, value = col)), 
                  value='Canada')
              ),
              dbcRow(
                dccDropdown(
                  id='year-select',
                  options = levels(factor(gapminder$year))%>%
                    purrr::map(function(col) list(label = col, value = col)),
                  value='2007')
              )
            )
          ),
          dbcCol(dccGraph(id='plot-line'))
        )
      )
    )
  )
)

app$callback(
  output('plot-line', 'figure'),
  list(input('target-select', 'value'),
       input('continent-select', 'value'),
       input('country-select', 'value'),
       input('year-select', 'value')),
  function(target, continent_x, country_x, year_x) {
    
    # World 
    df = gapminder %>%
      group_by(year) %>% 
      summarise(target_study = mean(!!sym(target))) %>%
      mutate(label = "World")
    
    # Region
    df_continent <- gapminder %>%
      filter(continent == continent_x) %>%
      group_by(year) %>%
      summarise(target_study = mean(!!sym(target))) %>%
      mutate(label = "Continent")
    
    # Country
    df_country <- gapminder %>%
      filter(country == country_x) %>%
      group_by(year) %>%
      summarise(target_study = mean(!!sym(target))) %>%
      mutate(label = "Country")
    
    # Year
    df = rbind(df,df_continent, df_country)
    df = df %>%
      filter(year<=year_x)
    
    p <- ggplot(df, aes(x = year,
                        y = target_study,
                        color = label
                        )) +
      geom_line() +
      ggthemes::scale_color_tableau()
    ggplotly(p)
  }
)




app$run_server(host = '0.0.0.0')
