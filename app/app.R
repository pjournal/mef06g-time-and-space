#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#library(shiny)
library(tidyr)
library(gt)
library(scales)
library(dplyr)

bs <- readRDS("balance_sheet.rds")
i_s <- readRDS("income_statement.rds")

# Filter balance sheet data

bs$accounts <- bs$accounts %>% gsub("\\s+", "_", .)

bs_df <- bs %>% 
  filter(accounts %in% c("I-Current_assets", 
                         "III-Short-term_liabilities", 
                         "IV-Long-term_liabilities", 
                         "Total_assets", 
                         "V-Own_funds")) %>% 
  pivot_wider(names_from = accounts, values_from = value)

#Prepare income statement data
i_s$accounts <- i_s$accounts %>% gsub("\\s+", "_", .)

net_income <- i_s %>% 
  filter(accounts == "Net_profit_or_loss_for_the_financial_year") %>% 
  pivot_wider(names_from = accounts, values_from = value) %>% 
  pull(Net_profit_or_loss_for_the_financial_year)

# Add net income data to balance sheet data
bs_df <- bs_df %>% mutate(net_income_is = net_income)

# Define financial ratios for each year
bs_df_economy <- bs_df %>% 
  mutate(current_ratio = bs_df$"I-Current_assets" / bs_df$"III-Short-term_liabilities", 
         leverage = bs_df$"III-Short-term_liabilities" / bs_df$"Total_assets",
         return_on_equity = bs_df$"net_income_is" / bs_df$"V-Own_funds",
         return_on_assets = bs_df$"net_income_is" / bs_df$"Total_assets") %>% 
  select(c(sector_code:year),c(current_ratio:return_on_assets), Total_assets)

# Define bs_df_new

bs_df_new <- bs_df %>% 
  mutate(current_ratio = bs_df$"I-Current_assets" / bs_df$"III-Short-term_liabilities", 
         leverage = bs_df$"III-Short-term_liabilities" / bs_df$"Total_assets",
         return_on_equity = bs_df$"net_income_is" / bs_df$"V-Own_funds",
         return_on_assets = bs_df$"net_income_is" / bs_df$"Total_assets") %>% 
  select(c(sector_code:year),c(current_ratio:return_on_assets))

bs_df_new <- bs_df_new %>% 
  pivot_longer(cols = c("current_ratio", "leverage", "return_on_equity", "return_on_assets"), 
               names_to = "Indicator", 
               values_to = "Value")

# Define Financial Fragility Index
bs_df_ffi <- bs_df_new %>% 
  pivot_wider(names_from = "Indicator", values_from = "Value") %>% 
  group_by(year) %>% 
  mutate(liq_rank = percent_rank(current_ratio*-1), 
         lev_rank = percent_rank(leverage), 
         profit_rank = percent_rank(return_on_assets*-1)) %>% 
  mutate(fin_fra_index = (liq_rank + lev_rank + profit_rank)/3) %>% 
  select(-c(current_ratio:return_on_assets))



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Financial Risk Scorecard for Real Sector Companies in Turkey"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("year",
                        "Select Year",
                        choices = unique(bs_df_economy$year))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           gt_output("table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  year_choice <- reactive({
    input$year
  })
    
  heatmap_df <- reactive({bs_df_economy %>% 
    filter(year == year_choice()) %>% 
    select(sector_name, current_ratio, leverage, return_on_assets) %>% 
    mutate(ffi = bs_df_ffi %>% select(year, sector_name, fin_fra_index) %>% filter(year == year_choice()) %>% pull(fin_fra_index)) %>% 
    arrange(desc(ffi))})
  
  heatmap_df_2 <- reactive({test_data <- heatmap_df()
  colnames(test_data) <- c("Sectors", "Liquidity", 
                            "Leverage", "Profitability", 
                            "Financial Fragility Index")
  test_data})
  
  heatmap_df_3 <- reactive({
    test_data_2 <- heatmap_df_2()
    test_data_2[,2:5] <- lapply(test_data_2[,2:5], FUN = function(x) round(x*100, 2))
    test_data_2
  })
    
  gt_tbl <- reactive({heatmap_df_3() %>% 
    gt() %>% 
    data_color(columns = 'Liquidity', 
               colors = col_numeric(palette = c("red", "white", "green"), 
                                    domain = c(min(heatmap_df_3()[,2]), max(heatmap_df_3()[,2])))) %>% 
    data_color(columns = 'Leverage', 
               colors = col_numeric(palette = c("green", "white", "red"), 
                                    domain = c(min(heatmap_df_3()[,3]), max(heatmap_df_3()[,3])))) %>% 
    data_color(columns = 'Profitability', 
               colors = col_numeric(palette = c("red", "white", "green"), 
                                    domain = c(min(heatmap_df_3()[,4]), max(heatmap_df_3()[,4])))) %>% 
    data_color(columns = 'Financial Fragility Index', 
               colors = col_numeric(palette = c("green", "white", "red"), 
                                    domain = c(min(heatmap_df_3()[,5]), max(heatmap_df_3()[,5]))))})
    
    output$table <- render_gt(
      expr = gt_tbl()
        
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
