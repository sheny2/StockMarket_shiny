# STAT 220. Final Project 


library(generics)
library(timetk)
library(tidyquant)
library(tidyverse)
library(plotly) 
library(DescTools)
library(stringr)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shiny)
library(plotly)
library(lubridate)
library(rlang)
library(rsconnect)
library(readr)
library(ggthemes)



# Read in necessary data sets 

SP500_all <- read.csv("SP500_all.csv")

SP500_Individual <- read_csv("all_stocks_5yr.csv") 

SP_clean<-read.csv("SP500_Individual_clean.csv")


# K-Means Panel Prep
standard_fun <- function(x) { (x - mean(x, na.rm=TRUE))/sd(x, na.rm = TRUE)}
SP_clean <- SP_clean %>% drop_na()
SP500_stand <- SP_clean%>%
  select(Stock,Volatility, Return) %>%
  mutate(Volatility = standard_fun(Volatility),Return=standard_fun(Return)) 


# K-means

set.seed(8848)

km_SP500 <- kmeans(SP500_stand[2:3],centers=5, nstart=20)

SP500_all_single<-SP_clean %>%
  mutate(cluster_km5 = str_c("Cluster ",km_SP500$cluster))
SP500_cluster_graph <- ggplot(SP500_all_single,aes(x=Volatility,y=Return,color=cluster_km5,tooltip=Stock)) +
  geom_point()+
  theme_economist() +
  theme(legend.position="right",plot.title = element_text(hjust = 0.5),panel.grid.major = element_line(size = 0, linetype = 'solid',
                                                                                                       colour = "white"))+
  labs(color="Stock Cluster")+ ggtitle("5 Clusters of S&P 500 Stocks based on Past Return and Volatility (2013-2018)")
generate_graph_cluster <- function(stock){
  SP500_cluster_graph + geom_point(aes(x = Volatility,y = Return), data = SP500_all_single %>% filter(Stock == stock), shape = 8, color = "black",size=5.5) 
}




# Get Three Key Market Trend Indicators 


all_DJI <- read.csv("all_DJI.csv")

all_SPY <- read.csv("all_SPY.csv")

all_QQQ <- read.csv("all_QQQ.csv")

# This function is used to plot market trend in each different sector 
industry_trend <-function(industry,start_date,end_date)
{
  industries = pull(distinct(SP500_all%>%filter(date=="2021-02-22")%>%pull(sector)%>%as_tibble()),value)
  index = match(industry,industries)
  
  industries_ETF <- c("XLK","XLY","XLC","XLF","XLV","XLP","XLE","XLI","XLU","XLB")
  ETF <- industries_ETF[index]
  
  tq_get(ETF,from = '2000-01-01',to = '2021-03-1',get = 'stock.prices') %>% select(date,adjusted) %>% drop_na() %>% filter(date >= start_date, date <=end_date) %>%
    ggplot(aes(x=date,y=adjusted)) + geom_point(size=0.1) + geom_line(size = 0.3) + 
    labs(x = "",y="SPDR Sector ETF Price",color="Stock", title = "Sector Performance")+
    theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()
}

# This function is used to analyze market trend in each different sector 

industry_analyze <-function(industry,start_date,end_date)
{ 
  industries = pull(distinct(SP500_all%>%filter(date=="2021-02-22")%>%pull(sector)%>%as_tibble()),value)
  index = match(industry,industries)
  
  industries_ETF <- c("XLK","XLY","XLC","XLF","XLV","XLP","XLE","XLI","XLU","XLB")
  stock <- industries_ETF[index]
  
  ## SPY Info
  price_data_SPY <- tq_get("SPY",from = '2000-01-01',to = '2021-03-1',get = 'stock.prices') %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    drop_na() %>% filter(date >= start_date, date <=end_date) 
  store_spy = cumprod(price_data_SPY[-1] + 1) 
  store_ret <- (store_spy-lag(store_spy))/lag(store_spy) 
  mean_SPY <- lapply(store_ret, mean, na.rm = TRUE)
  annual_SPY <- ((mean_SPY[[1]] + 1)^252) - 1
  sd_SPY <- lapply(store_ret, sd, na.rm = TRUE)[[1]]*252^0.5
  
  ## Sector Info 
  
  price_data_sector <- tq_get(stock,from = '2000-01-01',to = '2021-03-1',get = 'stock.prices') %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    drop_na() %>% filter(date >= start_date, date <=end_date) 
  store_sector = cumprod(price_data_sector[-1] + 1) 
  store_ret_sector <- (store_sector-lag(store_sector))/lag(store_sector) 
  mean_sector <- lapply(store_ret_sector, mean, na.rm = TRUE)
  annual_sector<- ((mean_sector[[1]] + 1)^252) - 1
  sd_sector <- lapply(store_ret_sector, sd, na.rm = TRUE)[[1]]*252^0.5
  
  result<- data.frame("Investment" = c("SPY","Sector"), "Annualized_Return" = c(annual_SPY,annual_sector), "Annualized_Volatility" = c(sd_SPY,sd_sector))
  
  result
}

# This function is used to read data from the Portfolio Simulation Page to Compare user's Portfolio with S&P 500.

portfolio_fun <- function(stocks,weights)
{
  ## Getting S&P 500 data
  price_data_SPY <- tq_get("SPY",from = '2016-03-01',to = '2021-03-1',get = 'stock.prices') %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    drop_na() 
  store_spy = cumprod(price_data_SPY[-1] + 1) 
  
  ## Getting Portfolio Data
  price_data <- tq_get(stocks,from = '2016-03-01',to = '2021-03-1',get = 'stock.prices') %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    pivot_wider(names_from=symbol,values_from = ret) 
  ## Calculating Portfolio metrics and combining with S&P 500 data
  store = cumprod(price_data[-1] + 1 )
  for (i in (1:ncol(store)))
  {
    store[i] =  store[i]*weights[i]
  }
  store <- store %>% rowwise() %>% mutate(Portfolio = sum(c_across(1:ncol(store))))
  store['date'] = price_data['date']
  store["SP_500"] = store_spy
  ## Generating Graph
  store %>% select(date,Portfolio,SP_500) %>%pivot_longer(2:3,names_to = "Investment",values_to = "Return") %>%
    ggplot(aes(x=date,y=Return,color=Investment)) + geom_line() +
    labs(x = "",y="Daily Investment Worth",color="Investment", title = "Your Portfolio against S&P 500 in the Past Five Years")+
    theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()
}

# This function is used to analyze portfolio and S&P.

port_analyze <-function(stocks,weights)
{ 
  ## SPY Info
  price_data_SPY <- tq_get("SPY",from = '2016-03-01',to = '2021-03-1',get = 'stock.prices') %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    drop_na() 
  store_spy = cumprod(price_data_SPY[-1] + 1) 
  store_ret <- (store_spy-lag(store_spy))/lag(store_spy) 
  mean_SPY <- lapply(store_ret, mean, na.rm = TRUE)
  annual_SPY <- ((mean_SPY[[1]] + 1)^252) - 1
  sd_SPY <- lapply(store_ret, sd, na.rm = TRUE)[[1]]*252^0.5
  
  ## Port Info 
  
  price_data <- tq_get(stocks,from = '2016-03-01',to = '2021-03-1',get = 'stock.prices') %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    pivot_wider(names_from=symbol,values_from = ret) 
  
  store = cumprod(price_data[-1] + 1 )
  for (i in (1:ncol(store)))
  {
    store[i] =  store[i]*weights[i]
  }
  
  store_port <- store %>% rowwise() %>% mutate(Portfolio = sum(c_across(1:ncol(store)))) %>% select(Portfolio)
  
  store_port <- (store_port-lag(store_port))/lag(store_port) 
  mean_port <- lapply(store_port, mean, na.rm = TRUE)
  annual_port <- ((mean_port[[1]] + 1)^252) - 1
  sd_port <- lapply(store_port, sd, na.rm = TRUE)[[1]]*252^0.5
  
  result<- data.frame("Investment" = c("SPY","Portfolio"), "Annualized_Return" = c(annual_SPY,annual_port), "Annualized_Volatility" = c(sd_SPY,sd_port))
  
  result
  
}

# This function is used to read in data of 5 selected stocks and conduct simulation of Return & Risk portfolio

optimal_fun <- function(ER,stocks,n_simulation=1000){
  price_data <- tq_get(stocks,from = '2000-01-01',to = '2021-03-1',get = 'stock.prices') %>%
    group_by(symbol) %>%
    tq_transmute(select = adjusted,mutate_fun = periodReturn, period = 'daily', col_rename = 'ret',type = 'log') %>%
    pivot_wider(names_from=symbol,values_from = ret) %>%
    drop_na()
  
  
  # Define and prepare necessary statistics 
  
  mean_ret <- colMeans(price_data[c(2:5,5)])
  cov_mat <- cov(price_data[c(2:5,5)]) * 252
  total_weights <- matrix(nrow = n_simulation, ncol = length(stock_selected))
  
  
  # Key Measurements of stock performance
  
  port_returns <- vector('numeric', length = n_simulation)
  port_risk <- vector('numeric', length = n_simulation)
  sharpe_ratio <- vector('numeric', length = n_simulation)
  
  
  # Go through simulation of performance
  
  for (i in seq_along(port_returns)) 
  {
    weights <- runif(length(stock_selected))
    weights <- weights/sum(weights)
    
    # Storing weight in the total_weights matrix
    total_weights[i,] <- weights
    
    # Portfolio returns
    port_ret <- sum(weights * mean_ret)
    port_ret <- ((port_ret + 1)^252) - 1
    
    # Storing Portfolio Returns values
    port_returns[i] <- port_ret
    
    # Creating and storing portfolio risk
    # Risk is defined as variation
    port_sd <- sqrt(t(weights) %*% (cov_mat  %*% weights))
    port_risk[i] <- port_sd
    
    # sharpe_ratio is defined by return/risk
    sharpe_ratio[i]  <- port_ret/port_sd
  }
  
  #outcome portfolio of five selected stocks 
  portfolio_values <- tibble(Return = port_returns,
                             Risk = port_risk,
                             SharpeRatio = sharpe_ratio,
                             Stock_1 = total_weights[,1],
                             Stock_2 = total_weights[,2],
                             Stock_3 = total_weights[,3],
                             Stock_4 = total_weights[,4],
                             Stock_5 = total_weights[,5]) 
  
  
  # find optimized weights for selected stocks 
  
  weight <- portfolio_values %>%
    filter(Return >= Closest(Return,ER)) %>%
    filter(Risk == min(Risk))%>%
    select(Return,Stock_1,Stock_2,Stock_3,Stock_4,Stock_5)
  
  
  # Prepare to highligh optimized combination among all simulation 
  
  highlight_df <- portfolio_values %>% 
    filter(Return >= Closest(Return,ER))%>%
    filter(Risk == min(Risk))%>%
    select(Return,Risk)
  
  
  # Simulation Graph
  sim_graph <- portfolio_values %>%
    ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
    geom_point() +
    theme_economist()+
    theme(legend.position="right",plot.title = element_text(hjust = 0.5),panel.grid.major = element_line(size = 0, linetype = 'solid',
                                                                                                         colour = "white"))+
    
    #theme( panel.background= element_rect(fill = "lightblue"),
    #       plot.background = element_rect(fill = "lightblue"),
    #      legend.background = element_rect(fill = "lightblue")) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(labels = scales::percent) +
    labs(x = 'Annualized Risk',
         y = 'Annualized Returns',
         title = "Portfolio Optimization") +
    geom_point(aes(x = Risk,y = Return), data = highlight_df, color = 'red', size=1.5) 
  # +geom_text(aes(x = Risk,y = Return,label="Minimum Variation Portfolio"), data=highlight_df,hjust=0, vjust=0)
  
  
  
  # The combination of stocks that are recommended 
  weight <- portfolio_values %>%
    filter(Return == Closest(Return,ER)) %>%
    select(Return,Risk,Stock_1,Stock_2,Stock_3,Stock_4,Stock_5) %>%
    mutate(Stock_1 = str_c(round(Stock_1,3)*100,"%"),
           Stock_2 = str_c(round(Stock_2,3)*100,"%"),
           Stock_3 = str_c(round(Stock_3,3)*100,"%"),
           Stock_4 = str_c(round(Stock_4,3)*100,"%"),
           Stock_5 = str_c(round(Stock_5,3)*100,"%"),
           Return = str_c(round(Return,3)*100,"%"),
           Risk = str_c(round(Risk,3)*100,"%"))
  
  
  cnames <- c("Return", "Risk",stocks[1], stocks[2],stocks[3], stocks[4],stocks[5])
  colnames(weight) <- cnames
  
  
  
  return <- list(sim_graph,weight)
  return
}

# Set up initial Default Values for optimal_fun

stock_selected <- c('JETS', 'FB', 'KO', 'XOM', 'CCL')
optimal_fun(ER = 0.1,stocks = stock_selected)


# Setting Text for Introduction and Other Sections

p1 <- "Hello. This is a tool that aims to give you an edge to better optimize your investment strategy in the stock market. 
  There are several main functionalities of this app."

p2 <- "First, in “Market Trends”, you will be able to simply see the return over the years you specify of the main stock indexes and your stocks of interest. 
It aims to give you a broad overview of how the market, along with individual stocks are doing. "

p3 <- "Second, in “Stock Selection”, you will be asked to enter one stock of interest and a time period of interest. Then, we will generate a set of stocks that have performed similar to your stock within the time interval you specified. K-means algorithm is used based on three dimensions (daily return, daily volatility, and transaction volume). 
This tool enables you to perform comparative analysis on a group of similar stocks. "

p4 <- "Third, in “Select Your Own Portfolio”, you will be allowed to enter your current portfolio or your portfolio of interest through inputing stock symbols and their corresponding weights. Your portfolio’s past performance will be tracked along with the industry and market performance. 
It allows you to back-test your investment strategies in the past."

p5 <- "Fourth, in “Portfolio Optimization”, you will be asked to input five stocks/ETF of interest along with you expected annual return. 
An optimal portfolio weight will be generated based on the stocks’ past performance and covariances. Please be patient since it may take a couple of seconds to complete our optimization function."

p6 <- "Before diving deep into using this program, if you are a beginner in the stock market, here are a few helpful tips. "

l1<- "SPY (S&P 500) and DJI (Dow Jones) are the standard ETF/Index that tracks how well the entire U.S. stock market is doing. Hence, they are often used as a benchmark in the field."

l2 <- "QQQ (Nasdaq) is getting more popular as it in an index that tracks mainly tech related companies. "

l3 <- "Together, SPY, DJI, and QQQ are the three most prominent index in the U.S. stock market "

l4 <- "SEDOL stands for Stock Exchange Daily Official List, which is a list of security identifiers. "

p7 <- "If you want to learn more about how to construct optimal portfolios, make sure to check out the Modern Portfolio Theory, 
which has gained its prominence in the academics of investment theory. "

data_summary_1 <- "Our data source comes from Yahoo finance, a website that provides financial news, data and commentary including stock quotes, press releases, financial reports, and other original contents. Our app mainly uses its records of every single stock/ETF that gets traded on the U.S. stock exchanges from Jan 1, 2000 till the end of Feburary, 2021. 
For each stock, it offers some basic information such as company name, SEDOL code, industrial sector, currency etc. The main variables of interests are the stock's the daily opening, closing, highest and lowest prices, as well as the trading volume everyday."

data_summary_2 <- "Here is a snapshot of what the 500 largest companies on the U.S. stock exchanges (SP500) looks like on a particular day."

data_summary_3 <- "And of course, you have the opportunity to view a specific stock and get some basic ideas about its performance! Please type in stock symbol or abbreviation. If you are not sure about your stock's symbol, feel free to refer to this link that contains the list of all stock abbreviation! The search fucntion can be used to further refine the output by time. For example, if you want to see your stock's price on a particular date, try type in Year-Month-Date, like 2021-01-01."

selection_0 <- "A stock of interest could be entered and a black locator will locate your stock on the graph"

selection_1 <- "Feel free to click on the graph to view specific stock information. The table below will automatically provide you with stocks with the closet characteristics of your selection."

selection_2 <- "Cluster 1 (Red) = High Return and High Volatility."
selection_3 <- "Cluster 2 (Dark Green) = Low Return and Low Volatility."
selection_4 <- "Cluster 3 (Green) = High Return and low Volatility."
selection_5 <- "Cluster 4 (Blue) = Medium Return and Medium Volatility."
selection_6 <- "Cluster 5 (Purple) = Medium/Low Return and low Volatility."

market_dis_instruction_1 <- "This tab mainly consists of two functions:" 
market_dis_instruction_2 <- "When “Overall Market” is selected, a graph demonstrating the sector distribution in terms of count and trading volume within the S&P 500 universe will be generated."
market_dis_instruction_3 <- "When “Individual Sector” is selected, the user could select a specific sector and a specific time frame that she is interested in. 
A graph will be generated comparing the sector ETF’s performance to the general market’s (S&P 500 index) within the specified time frame. 
Annualized return and volatility of the sector ETF and S&P 500 index will also be generated as a reference. "
market_dis_instruction_4 <- "The user could use this tab to learn more about how different sectors play a role 
in S&P 500 and how each individual sector did in the past compared to the general market. "

stock_trend_jargon_1 <- "S&P 500 (SPY): A stock market index that measures the stock performance of 500 large companies 
listed on stock exchanges in the United States. It is one of the most commonly followed equity indices.
"
stock_trend_jargon_2 <- "Dow Jones Industrial Average (DJI): Also known as the Dow, is a stock market index that measures 
the stock performance of 30 large companies listed on stock exchanges in the United States
"
stock_trend_jargon_3 <- "Nasdaq 100 (QQQ): A stock market index made up of 102 equity securities issued by 100 of the largest non-financial companies listed on the Nasdaq stock market, 
which primarily focuses on technology."

stock_trend_jargon_4 <- "Investment worth: How much an one-dollar investment in a specific investment vehicle 
appreciates/depreciates over a period of time. It is used to standardize the return of different investment vehicles. 
"
stock_trend_jargon_5 <- "ETF: An exchange traded fund (ETF) is a basket of securities that trade on an exchange, just like a stock."
stock_trend_1 <- "This tab mainly consists of two functions:"
stock_trend_2 <- "When “Individual Stocks” is selected, the user could input two stocks of interest. 
Then, the user will be given the chance of selecting whether she wants to analyze the stock’s daily investment worth ordaily transaction volume. An option is also given to compare the investment with the S&P to give the user a better sense of whether the stock of interest has outperformed or underperformed the market. 
User could also use the slidebar to select her time frame of interest "
stock_trend_3 <- "When the three indexes are selected, a graph plotting the investment worth of the index will be generated based on the time frame of interest.
A threshold value could also be entered to set targeted values"
stock_trend_4 <- "The user could use this tab to learn about general market trends and compare individual stock’s performance with the general market’s. "

selection_instruction_1 <- "This tab mainly consist of two functions: "
selection_instruction_2<- "The user could input a stock of interest within the S&P 500 universe, a black pointer will be generated on the cluster graph identifying the stock of interest. The user could easily the cluster that the stock belongs to"
selection_instruction_3<-  "The user could also click on any point on the graph to find out what company it represents along with its neighbors. "
selection_instruction_4<- "The user could use this tab to learn about which cluster her stock of interest belongs to and investigate stocks who are in a similar cluster. The details of how the clusters are generated is detailed in the model building tab. "

selection_model_1<- "This model uses the k-means unsupervised algorithm to separate the S&P 500 stocks 
into 5 different clusters based on return and volatility from 2013-2018. "
selection_model_2<- "Return:  A net gain or loss of an investment over a specified time period, expressed as a percentage of the investment’s initial cost. "
selection_model_3<- "Volatility:  A statistical measure of the dispersion of returns for a given security or market index. In most cases, the higher the volatility, the riskier the security."
selection_model_4<- "K-mean clustering: A method of vector quantization, originally from signal processing, that aims to partition n observations into k clusters in which each observation belongs to the cluster with the nearest mean"

portfolio_opt_instruction <- "This tab allows users to input five different stocks with a targeted annualized return. 
Weights will be randomly generated and assigned to each stock and about 1000 different portfolios will be generated. 
Then, the system will automatically pick the portfolio with the minimum risk that achieved the targeted annualized over the past ten years, 
or in those words, a portfolio with the targeted annualized return on the “efficient frontier” will be generated. 
A weight of the five stocks will be displayed both as a graph and a table with the corresponding portfolio risk and return. 
Users could use this tab to construct an optimal portfolio to minimize risk based on past data.
"

portfolio_opt_term <- "Efficient frontier: In modern portfolio theory, the efficient frontier is an 
investment portfolio which occupies the efficient parts of the risk–return spectrum. 
Formally, it is the set of portfolios which satisfy the condition that no other portfolio exists with a higher expected return
but with the same standard deviation of return (volatility)"

portfolio_opt_term_2 <- "Risk: One of the most common methods of determining the risk an investment poses is standard deviation. Standard deviation helps determine market volatility or the spread of asset prices from their average price. When prices move wildly, standard deviation is high, meaning an investment will be risky."

portfolio_process_1 <- "The daily return of each stock input is first calculated based on historical data (2000-01-01 to 2021-03-01). The covariance matrix is also generated and annualized to represent annualized volatility. "
portfolio_process_2 <- "A set of 1000 random weights are generated (e.g. 0.2,0.2,0.2,0.3,0.1). Each weight is used to calculate the portfolio return and the return is subsequently annualized. Each weight is also used to compute the risk of each portfolio through using the covariance matrix. For each weight, the corresponding return and risk are recorded. "
portfolio_process_3 <- "Then, all 1000 sets of returns and risks are plotted on the graph to generate the frontier. The optimal portfolio weights are also calculated based on the result of the 1000 sets of portfolio return and risk. "

intro_1 <- "Are you sometimes befuddled when choosing the best future investment? Are you so tired of not catching up with the fluctuation of the financial market? Are you frustrated by the unpredictableness of your stocks? Don't Worry! Our app will be a tool that aims to give you an edge to better optimize your investment strategy in the stock market. Here is a brief overview of this tool. 
"
intro_2 <- "We recommend to visit our data summary first. It provides a general sense of the data running behind the scenes, which is the backbone of all the functions there are built into this app."

intro_3 <-  "The rest of our main tabs serve to illustrate a more analytical pitcure of stocks, with detailed instructions and informative terminologies provided along with the graphs. The market distribution tab explains how different sectors play a role in S&P 500 and how each individual sector did in the past compared to the general market. The stock trend tab allows users to learn about general market trends and compare individual stock’s performance with the general market’s. The stock selection tab provides insights on individual stocks based on K-means clustering. The understand your portfolio page enables users to back test your own newly acquired investment strategies. Finally, the portfolio optimization page optimizes the user's portfolio to minimize risk based on past data. 
"

intro_4 <-  "Now, please give it a try and enjoy exploring the stock market!"


## Main APP Part

# Shiny ui function

ui <- navbarPage("How to Survive in the U.S. Stock Market", theme = shinytheme("superhero"),
                 
                 #Major Tab 1
                 tabPanel("Introduction of this App",
                          icon = icon("compass"),
                          tags$br(
                            p(intro_1),
                            br(),
                            p(intro_2),
                            br(),
                            p(intro_3),
                            br(),
                            p(intro_4)
                          ),
                          div(img(src='Wall_Pic.png',width="50%"), style="text-align: center;"),
                          
                 ),
                 
                 
                 #Major Tab 2
                 tabPanel("Data Summary",
                          
                          icon = icon("atlas"),
                          
                          fluidPage(
                            tags$br(br(),
                                    p(data_summary_1),
                                    br(),
                                    p(data_summary_2),
                            ),
                            
                            dataTableOutput("Overall_table"),
                            
                            tags$br(br(),
                                    p(data_summary_3),
                            ),
                            
                            tags$a(href="https://eoddata.com/symbols.aspx?AspxAutoDetectCookieSupport=1", "Stock Reference List"),
                            
                            br(),
                            
                            textInput(inputId = "Summary_Stock_Selected",label = "Your Stock of Interest",value = "AAPL"),
                            
                            
                            dataTableOutput("Summary_stock"),
                            
                          )),
                 
                 
                 #New Test Major Tab 
                 tabPanel("Market Distribution",
                          
                          icon = icon("chart-bar"),
                          
                          fluidPage(
                            
                            sidebarPanel( 
                              
                              radioButtons("sector_indicator", "Select Your Interested Aspect",
                                           c("Sector Distribution" = "overall",
                                             "Individual Sector" = "individual"
                                             
                                           )),
                              
                              
                              uiOutput("option_dist_2"),
                              uiOutput("option_dist_3"),
                              uiOutput("option_dist_1")
                              
                              
                            ),
                            
                            mainPanel(tabsetPanel(type = "tabs",
                                                  tabPanel("Instruction",
                                                           br(),
                                                           strong(market_dis_instruction_1),
                                                           tags$ol(
                                                             br(),
                                                             tags$li(market_dis_instruction_2),
                                                             br(),
                                                             tags$li(market_dis_instruction_3),
                                                             br()
                                                             
                                                           ),
                                                           p(market_dis_instruction_4),
                                                           div(img(src='pexels-photo-187041.png',width="60%"), style="text-align: center;"),
                                                           br()
                                                           
                                                           
                                                           
                                                  ),
                                                  tabPanel("Sector Plot", plotOutput("dist_graph"),dataTableOutput("sector_compare")))
                                      
                                      
                            ))
                          
                          
                          
                 ),
                 
                 
                 #Major Tab 3
                 tabPanel("Stock Trends",
                          
                          icon = icon("chart-line"),
                          
                          fluidPage(
                            sidebarLayout(
                              
                              # First Button chooses your interested market indicator
                              sidebarPanel(
                                radioButtons("market_indicator", "Select Your Interested Market Indicator",
                                             c("Individual Stocks (ex. AAPL for Apple Inc.)" = "individual",
                                               "Dow Jones Industrial Average (DJI)" = "DJI",
                                               "SPDR S&P 500 Trust ETF (SPY)" = "SPY",
                                               "Nasdaq Invesco Trust Series (QQQ)" = "QQQ")),
                                br(),
                                
                                # Option reactive to the first 
                                uiOutput("option21"),
                                
                                br(),
                                
                                uiOutput("option22"),
                                
                                br(),
                                
                                uiOutput("third_option"),
                                
                                br(),
                                
                                # Time Frame 
                                sliderInput("Trend_Time",
                                            "Select Your Interested Time Frame",
                                            value = 2021,
                                            min = 2000,
                                            max = 2021, 
                                            animate = T,
                                            sep = ""),
                                
                                br(),
                                
                                # Threshold value for line charts
                                numericInput("target_value",h5("Threshold Value: "),value=0)
                                
                              ),
                              
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Instruction",
                                                     br(),
                                                     
                                                     strong(stock_trend_1),
                                                     
                                                     tags$ol(
                                                       br(),
                                                       tags$li(stock_trend_2),
                                                       br(),
                                                       tags$li(stock_trend_3),
                                                       br()
                                                       
                                                     ),
                                                     p(stock_trend_4),
                                                     br(),
                                                     div(img(src='dow-sp500-nasdaq.png',width="60%"), style="text-align: center;"),
                                                     br()
                                            ),
                                            tabPanel("Key Terms",
                                                     
                                                     br(),
                                                     br(),
                                                     p(stock_trend_jargon_1),
                                                     br(),
                                                     p(stock_trend_jargon_2),
                                                     br(),
                                                     p(stock_trend_jargon_3),
                                                     br(),
                                                     p(stock_trend_jargon_4),
                                                     br(),
                                                     p(stock_trend_jargon_5),
                                                     br()
                                            ),
                                            tabPanel("Market Trend Plot", plotOutput("market_trend_plot")))
                              )
                            )
                          )
                          
                 ),
                 
                 
                 
                 #Major Tab 4
                 tabPanel("Stock Selection",
                          
                          icon = icon("diagnoses"),
                          
                          fluidPage(
                            
                            sidebarPanel(titlePanel("Pick Your Stock of Interest"),
                                         
                                         tags$br(br(),
                                                 p(selection_0),
                                                 
                                         ),
                                         
                                         textInput(inputId = "Stock_Selected",label = "Stock of Interest",value = "AAPL"),
                                         
                                         tags$br(
                                           br(),
                                           p(selection_1),
                                           br()),
                                         
                                         dataTableOutput("info")),
                            
                            mainPanel(tabsetPanel(type="tabs", 
                                                  tabPanel("Instruction",
                                                           br(),
                                                           
                                                           strong(selection_instruction_1),
                                                           
                                                           tags$ol(
                                                             br(),
                                                             tags$li(selection_instruction_2),
                                                             br(),
                                                             tags$li(selection_instruction_3),
                                                             br()
                                                             
                                                           ),
                                                           p(selection_instruction_4),
                                                           br(),
                                                           div(img(src='image-neba-articl.png',width="60%"), style="text-align: center;"),
                                                           br()
                                                           
                                                  ),
                                                  tabPanel("Model Building",
                                                           br(),
                                                           br(),
                                                           p(selection_model_1),
                                                           br(),
                                                           p(selection_model_2),
                                                           br(),
                                                           p(selection_model_3),
                                                           br(),
                                                           p(selection_model_4),
                                                           br()
                                                           
                                                  ),
                                                  tabPanel("S&P 500 Cluster", plotOutput("Cluster",click = "my_click"),
                                                           br(),dataTableOutput("cluster_info"))
                            ))
                            
                          )
                          
                 ),
                 
                 
                 
                 #Major Tab 5
                 tabPanel("Understand Your Portfolio",
                          
                          icon = icon("search-dollar"),
                          fluidPage(
                            
                            
                            sidebarPanel(titlePanel("Choose Your Own Portfolio"),
                                         tags$br(
                                           br(),
                                           p("Please Enter the Stock Symbol Seperated by Commas (e.x AAPL,MSFT)"),
                                         ),
                                         textInput(inputId = "Stocks_Port",label = "Stocks in Portfolio",value = "AAPL,MSFT,TSLA"),
                                         tags$br(
                                           br(),
                                           p("Please Enter the Corresponding Weights in Order Seperated by Commas (e.x 0.6,0.4)"),
                                         ),
                                         textInput(inputId = "Weights",label = "Corresponding Weights",value = "0.5,0.3,0.2"),
                                         dataTableOutput("info_compare")
                                         
                            ),
                            
                            mainPanel(tabsetPanel(type="tabs", 
                                                  tabPanel("Instruction",
                                                           
                                                           br(),
                                                           p("This tab allows users to input different stocks with different corresponding weights. 
                                                           A portfolio will be generated based on the stock and weight inputs. The portfolio will be 
                                                           compared with the S&P 500 from Mar.1 2016 to Mar.1 2021. The annualized return and volatility of the portfolio 
                                                           and the S&P 500 market index will be calculated and displayed in a table. 
                                                          Users could use this tab to back test her investment strategy with the S&P 500 index."),
                                                           br(),
                                                           br(),
                                                           div(img(src='Trading_Graph_Chart.png',width="60%"), style="text-align: center;"),
                                                           br()
                                                           
                                                           
                                                  ),
                                                  tabPanel("Your Chosen Portfolio Against S&P500", plotOutput("Comparison"))),
                                      
                            )
                            
                          )
                          
                 ),
                 
                 
                 #Major Tab 6
                 tabPanel("Portfolio Optimization",
                          
                          icon = icon("coins"),
                          
                          sidebarPanel( verbatimTextOutput("test"),
                                        titlePanel(("Pick Five Favourite Stocks")),
                                        textInput(inputId = "Select_Stock_1", label="Stock 1",value = "JETS"),
                                        textInput(inputId = "Select_Stock_2", label="Stock 2",value = "FB"),
                                        textInput(inputId = "Select_Stock_3", label="Stock 3",value = "KO"),
                                        textInput(inputId = "Select_Stock_4", label="Stock 4",value = "GS"),
                                        textInput(inputId = "Select_Stock_5", label="Stock 5",value = "CCL"),
                                        numericInput(inputId = "ER",label = "Expected_Return",value = 0.1),
                                        br(),
                                        strong("Optimalized Portfolio"),
                                        br(),
                                        dataTableOutput(outputId="portfolioinfo"),
                                        width = 4),
                          
                          mainPanel(
                            tabsetPanel(type="tabs", 
                                        
                                        tabPanel("Instruction",
                                                 br(),
                                                 p(portfolio_opt_instruction),
                                                 br(),
                                                 br(),
                                                 div(img(src='INV_Stock-Portfolio.png',width="60%"), style="text-align: center;"),
                                                 br()
                                        ),
                                        
                                        tabPanel("Key Terms", 
                                                 br(),
                                                 p(portfolio_opt_term),
                                                 br(),
                                                 p(portfolio_opt_term_2)),
                                        
                                        tabPanel("Frontier Construction Process", 
                                                 br(),
                                                 p(portfolio_process_1),
                                                 br(),
                                                 p(portfolio_process_2),
                                                 br(),
                                                 p(portfolio_process_3)),
                                        
                                        tabPanel("Portfolio Optimization",
                                                 
                                                 plotOutput("simulationplot"),
                                                 
                                                 plotOutput("portfoliodistribution"),
                                                 
                                                 textOutput(outputId = "weight")
                                        )
                            )))
                 
                 
)



# Shiny Server Function
server <- function(input, output) {
  
  
  # Data summary Tab 
  
  output$Overall_table <- renderDataTable(SP500_all%>%
                                            filter(date == "2021-02-22")%>%
                                            select(-X) %>%
                                            rename(
                                              Company = company,
                                              Symbol = symbol,
                                              Identifier = identifier,
                                              Sedol=sedol
                                            ), 
                                          options = list(pageLength = 10, lengthChange = FALSE, searching = F))
  
  
  output$Summary_stock <- renderDataTable(tq_get(input$Summary_Stock_Selected,from = '2000-01-01',to = '2021-03-1',get = 'stock.prices') %>%
                                            rename(
                                              Stock_Abbreviation = symbol
                                            ),
                                          options = list(pageLength = 12, lengthChange = FALSE, sDom  = '<"top">flrt<"bottom">ip'))
  
  
  # Output in market_dist
  
  output$option_dist_1 <- renderUI(
    if (input$sector_indicator != "individual")
    {radioButtons("graph_type", "Select Your Preferred Visualization",
                  c("Counts of Each Sector" = "Counts",
                    "Volume of Each Sector" = "Volume"))}
    else{selectInput(inputId = "interested_sector", 
                     label = "Select Your Interested Sector",
                     choices = (distinct(SP500_all%>%filter(date=="2021-02-22")%>%pull(sector)%>%as_tibble()))$value, 
                     selected = "Information Technology")})
  
  
  output$option_dist_2 <- renderUI(
    if (input$sector_indicator == "individual")
    {textInput(inputId = "start_date",label = "Start Date in yyyy-mm-dd",value = "2000-01-01")}
    else{})
  
  output$option_dist_3<- renderUI(
    if (input$sector_indicator == "individual")
    {textInput(inputId = "end_date",label = "End Date in yyyy-mm-dd",value = "2020-01-01")}
    else{})
  
  
  output$dist_graph <- renderPlot(
    if (input$sector_indicator != "individual" & input$graph_type == "Counts")
    { 
      SP500_all%>%filter(date=="2021-02-22")%>% 
        group_by(sector) %>% mutate(count=n()) %>% select(sector,count) %>% distinct() %>%
        ggplot(aes(x = reorder(sector, -count), y = count, fill = sector)) + 
        geom_bar(stat = "identity")+
        theme_economist() +
        labs(x = '', y = 'Counts Among SP500', 
             title = "Distribution of Various Sector Among SP500 Companies") +  
        theme(legend.position="right",plot.title = element_text(hjust = 0.5), 
              legend.text=element_text(size=12),
              axis.text.x = element_text(face = "bold", size = 8, angle=-60)) 
    }
    
    
    else if (input$sector_indicator != "individual" & input$graph_type == "Volume")
    {SP500_all%>%filter(date=="2021-02-22")%>%
        select(sector,volume)%>%
        as_tibble()%>%
        group_by(sector)%>% 
        summarise(total_v = sum(volume))%>%
        ggplot() +
        geom_col(mapping = aes(x = reorder(sector, -total_v),y=total_v/1000000000,fill=sector))+
        theme_economist() +
        labs(x = '', y = 'Total Daily Trading Volume (in billion)', title = "Volumes of Various Sector Among SP500 Companies") +
        scale_fill_hue(name = "Sector")+ 
        theme(legend.position="right",plot.title = element_text(hjust = 0.5),
              legend.text=element_text(size=12),
              axis.text.x = element_text(face = "bold", 
                                         size = 8, angle=-60))}
    else
    {
      industry_trend(input$interested_sector,input$start_date,input$end_date)
    }
    
  )
  
  output$sector_compare <-renderDataTable(
    
    if(input$sector_indicator == "individual")
    {
      industry_analyze(input$interested_sector,input$start_date,input$end_date)
    },
    options = list(pageLength = 5, lengthChange = FALSE,info = FALSE,dom='t',searching=F,sDom  = '<"top">lrt<"bottom">ip')
    
  )
  
  
  
  # Reactive option in market_trend
  
  output$option21 <- renderUI(
    if (input$market_indicator == "individual")
    {textInput(inputId = "Select_Stock_01", label="First Stock of Interest",value = "AAPL")}
    else{})
  
  output$option22 <- renderUI(
    if (input$market_indicator == "individual")
    {textInput(inputId = "Select_Stock_02", label="Second Stock of Interest",value = "TSLA")}
    else{})                
  
  
  output$third_option <- renderUI(
    if (input$market_indicator == "individual")
    {radioButtons("stock_stats", "Select specific stock market information",
                  c("Daily Investment Worth" = "open",
                    "Daily Investment Worth Comparing with S&P 500" = "Compare",
                    "Daily Transaction Volume" = "volume"))}
    else{})
  
  
  # Render Plots in Market Trend Tab
  
  output$market_trend_plot <- renderPlot(
    if (input$market_indicator == "individual" & input$stock_stats == "open")
    {tq_get(c(input$Select_Stock_01,input$Select_Stock_02),from = '2000-01-01',to = '2021-03-1',get = 'stock.prices') %>%
        drop_na()%>%
        mutate(year=year(date))%>%
        filter(year <= input$Trend_Time)%>%
        group_by(symbol)%>%
        mutate(open = open/open[[1]]) %>%
        group_by(year,symbol)%>%
        summarize(mean_stock_stats = mean(open))%>%
        ggplot(aes(x=year,y=mean_stock_stats,color=symbol)) + geom_point() + geom_line(size = 1.2)+
        labs(x="", y="Daily Investment Worth",color="Stock", title = "A One-dollar Investment in Your Specified Stocks")+
        geom_hline(yintercept= input$target_value,color="black",size = 0.5,alpha=0.5) +
        theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()+
        xlim(2000, 2021)}
    
    
    else if (input$market_indicator == "individual" & input$stock_stats == "volume")
    {tq_get(c(input$Select_Stock_01,input$Select_Stock_02),from = '2000-01-01',to = '2021-03-1',get = 'stock.prices') %>%
        drop_na()%>%
        mutate(year=year(date))%>%
        filter(year <= input$Trend_Time)%>%
        group_by(year,symbol)%>%
        summarize(mean_stock_stats = mean(volume))%>%
        ggplot(aes(x=year,y=mean_stock_stats/1000000,color=symbol)) + geom_point() + geom_line(size = 1.2)+
        labs(x="", y="Daily Transcation (in millions)",color="Stock")+
        geom_hline(yintercept= input$target_value,color="black",size = 0.5,alpha=0.5) +
        theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()+ 
        xlim(2000, 2021)}
    
    
    else if (input$market_indicator == "individual" & input$stock_stats == "Compare")
    {tq_get(c(input$Select_Stock_01,input$Select_Stock_02,"SPY"),from = '2000-01-01',to = '2021-03-1',get = 'stock.prices') %>%
        drop_na()%>%
        mutate(year=year(date))%>%
        filter(year <= input$Trend_Time)%>%
        group_by(symbol)%>%
        mutate(open = open/open[[1]]) %>%
        group_by(year,symbol)%>%
        summarize(mean_stock_stats = mean(open))%>%
        ggplot(aes(x=year,y=mean_stock_stats,color=symbol)) + geom_point() + geom_line(size = 1.2)+
        labs(x="", y="Daily Investment Worth",color="Stock", title = "A One-dollar Investment in Your Specified Stocks")+
        geom_hline(yintercept= input$target_value,color="black",size = 0.5,alpha=0.5) +
        theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()+ 
        xlim(2000, 2021)}
    
    
    else if (input$market_indicator == "DJI")
    {all_DJI %>%
        drop_na()%>%
        mutate(year=year(date))%>%
        filter(year <= input$Trend_Time)%>%
        group_by(year)%>%
        summarize(mean_stock_stats=mean(adjusted))%>%
        ggplot(aes(x=year,y=mean_stock_stats)) + geom_point() + geom_line(size = 1.2)+
        labs(x="Year", y="Investment in  DJI")+
        geom_hline(yintercept= input$target_value,color="black",size = 0.5,alpha=0.5) +
        ggtitle("A One-dollar Investment in the DJI Index in 2000")+
        theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()+ 
        xlim(2000, 2021)}
    
    
    else if (input$market_indicator == "SPY")
    {all_SPY %>%
        drop_na()%>%
        mutate(year=year(date))%>%
        filter(year <= input$Trend_Time)%>%
        group_by(year)%>%
        summarize(mean_stock_stats=mean(adjusted))%>%
        ggplot(aes(x=year,y=mean_stock_stats)) + geom_point() + geom_line(size = 1.2)+
        labs(x="", y="Investment in SPY")+
        ggtitle("A One-dollar Investment in the S&P 500 Index in 2000")+
        geom_hline(yintercept= input$target_value,color="black",size = 0.5,alpha=0.5) +
        theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()+ 
        xlim(2000, 2021)}
    
    
    else if (input$market_indicator == "QQQ")
    {all_QQQ %>%
        drop_na()%>%
        mutate(year=year(date))%>%
        filter(year <= input$Trend_Time)%>%
        group_by(year)%>%
        summarize(mean_stock_stats=mean(adjusted))%>%
        ggplot(aes(x=year,y=mean_stock_stats)) + geom_point() + geom_line(size = 1.2)+
        ggtitle("A One-dollar Investment in the NASDAQ 100 Index in 2000")+
        labs(x="", y="Investment in QQQ")+
        geom_hline(yintercept= input$target_value,color="black",size = 0.5,alpha=0.5) +
        theme(legend.position="right",plot.title = element_text(hjust = 0.5)) +  theme_economist()+ 
        xlim(2000, 2021)}
  )
  
  
  # Selection Tab
  
  output$Cluster <- renderPlot({generate_graph_cluster(input$Stock_Selected)})
  
  
  output$info <- renderDataTable(nearPoints(SP500_all_single, input$my_click,threshold = 10) %>%
                                   select(-X) %>%
                                   rename(
                                     Stock_cluster = cluster_km5
                                   ), 
                                 options = list(pageLength = 5, lengthChange = FALSE,info = FALSE,dom='t',searching=F,sDom  = '<"top">lrt<"bottom">ip'))
  
  Cluster <- c(1,2,3,4,5)
  
  Color <- c("Red","Dark Green","Green","Blue","Purple")
  
  Return <- c("High","Low","High","Medium","Low")
  
  Volatility <- c("High","High","Low","Medium","Low")
  
  
  cluster_info <- data.frame(Cluster, Color,Return, Volatility)
  
  
  output$cluster_info <- renderDataTable(cluster_info, 
                                         options = list(lengthChange = FALSE,info = FALSE,dom='t',searching=F,sDom  = '<"top">lrt<"bottom">ip'))
  
  
  # Portfolio Simulation Tab
  
  Stocks_Port = reactive({
    Stocks_Port <-strsplit(input$Stocks_Port, ",")[[1]]
  })
  
  Weights = reactive({
    Weights <-as.numeric(strsplit(input$Weights, ",")[[1]])
  })
  
  output$Comparison <- renderPlot({portfolio_fun(Stocks_Port(),Weights())})
  output$info_compare <-renderDataTable(port_analyze(Stocks_Port(),Weights()),options = list(pageLength = 5, lengthChange = FALSE,info = FALSE,dom='t',searching=F,sDom  = '<"top">lrt<"bottom">ip'))
  
  # Portfolio Optimization Tab 
  
  Select_Stock <- reactive({Select_Stock <- c(input$Select_Stock_1,
                                              input$Select_Stock_2,
                                              input$Select_Stock_3,
                                              input$Select_Stock_4,
                                              input$Select_Stock_5)})
  
  result <- reactive({result <- optimal_fun(input$ER, Select_Stock())})
  
  
  output$simulationplot <- renderPlot({
    result()[[1]]
  })
  
  
  
  output$portfolioinfo <- renderDataTable({
    result()[[2]]
  },options = list(pageLength = 5, lengthChange = FALSE,info = FALSE,dom='t',searching=F,sDom  = '<"top">lrt<"bottom">ip'))
  
  
  
  output$portfoliodistribution <- renderPlot({
    result()[[2]] %>% 
      select(3:7) %>% 
      pivot_longer(1:5,names_to = "Stocks" ,values_to= "Weight") %>% 
      mutate(Weight_num=as.numeric(sub("%", "",Weight)))%>% 
      ggplot(aes(x = fct_reorder(Stocks,Weight_num), y = Weight_num/100, fill = Stocks)) +
      geom_bar(stat = 'identity') +
      theme_economist() +
      labs(x = '', y = 'Weights', title = "Weights of Stocks in the Optimized Portfolio") +
      scale_fill_hue(name = "Stocks Selected")+
      scale_y_continuous(labels = scales::percent)+
      geom_text(aes(label=Weight), vjust=1.5,size=5) +
      theme(legend.position="right",plot.title = element_text(hjust = 0.5),panel.grid.major = element_line(size = 0, linetype = 'solid',
                                                                                                           colour = "white"))
  })
  
}



# Complete app with ui and server components
shinyApp(ui, server)

