rm(list = ls())
cat("\014")

library(shiny)
library(ggvis)
library(reshape)
library(tidyr)
library(plyr)

df_le <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', skip=3)
df_fr <- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', skip=3)
df_pop <- read.csv('API_SP.POP.TOTL_DS2_en_csv_v2.csv',skip=3)
df_reg <- read.csv('Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')


df_country_code <- unique( df_le[, c('Country.Name', 'Country.Code')] )
df_country_reg <- merge(x = df_country_code, y = df_reg[, c('Country.Code', 'Region')], by = "Country.Code", all = TRUE)
df_country_reg <- df_country_reg[, c('Country.Name', 'Region')]
rm(list = c('df_country_code'))

# Drop irrelevant columns
drop = c("Country.Code","Indicator.Code")
df_le <- df_le[, !(colnames(df_le) %in% drop)]
df_fr <- df_fr[, !(colnames(df_fr) %in% drop)]
df_pop <- df_pop[, !(colnames(df_pop) %in% drop)]

df_le_fr_pop <- rbind(df_le, df_fr, df_pop)
rm(list = c('df_le', 'df_fr', 'df_pop'))

df_le_fr_pop_melt <- melt(df_le_fr_pop, id=c("Country.Name", "Indicator.Name"))
df_le_fr_pop_melt <- spread(df_le_fr_pop_melt, Indicator.Name, value)
df <- merge(x = df_le_fr_pop_melt, y = df_country_reg, by = "Country.Name", all = TRUE)
rm(list = c('df_country_reg', 'df_reg', 'df_le_fr_pop_melt', 'df_le_fr_pop'))
df <- df[df$Country.Name != 'World', ]
df$Region <- as.character(df$Region)
df <- df[!is.na(df$Region) & df$Region != "", ]


names(df)[names(df) == 'Life expectancy at birth, total (years)'] <- 'le'
names(df)[names(df) == 'Fertility rate, total (births per woman)'] <- 'fr'
names(df)[names(df) == 'Population, total'] <- 'Population'
names(df)[names(df) == 'variable'] <- 'year'

df$year <- as.integer(gsub("[^0-9]","",df$year))

limits <- c()
limits$lemin = min(df$le, na.rm=T)
limits$lemax = max(df$le, na.rm=T)
limits$frmin = min(df$fr, na.rm=T)
limits$frmax = max(df$fr, na.rm=T)

Regions <- unique(df$Region)

ui <- fluidPage(
  headerPanel('HW2 Valentin Vrzheshch'),
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis")
  ),
  sidebarPanel(
    sliderInput(inputId="year", label="Year", min=min(df$year, na.rm=T), 
                max=max(df$year, na.rm=T)-1, value=min(df$year, na.rm=T), step=1, sep='', animate = T),
    checkboxGroupInput("regions", "Choose Regions to display:", choices = Regions, selected = Regions,
                       inline = FALSE, width = NULL, choiceNames = NULL, choiceValues = NULL)
  )
)

server <- function(input, output) {
  sub_df <- reactive({df[df$year == input$year & df$Region %in% input$regions, ]})
  
  output$info <- renderText({
    paste0("year=", input$year, "\nnrow(sub_df)=", nrow(sub_df))
  })
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- df[df$le == x$le & df$fr == x$fr & !is.na(df$le) & !is.na(df$fr), ]
    paste0(row$Country.Name)
  }

  sub_df %>% 
    ggvis(x = ~le, y = ~fr, fill = ~factor(Region), size = ~Population) %>%
    layer_points() %>%
    add_axis("x", title = "Life Expectancy (years per human body)") %>%
    add_axis("y", title = "Fertility Rate (babies per woman)") %>%
    add_legend("fill", title="Region", properties = legend_props(legend = list(y = 150))) %>%
    add_legend("size", title="Population", properties = legend_props(legend = list(y = 50))) %>%
    add_tooltip(all_values, "hover") %>%
    scale_numeric("x", domain = c(limits$lemin, limits$lemax), nice = T) %>%
    scale_numeric("y", domain = c(limits$frmin, limits$frmax), nice = T) %>%
    set_options(duration=0) %>%
    bind_shiny("ggvis", "ggvis_ui")
  
}


shinyApp(ui = ui, server = server)

