
# STAT 220 Final Project
==================

Group Name: Steins Gate

Group Members: Mitchell Wang and Yicheng Shen

Project URL: https://yicheng-shen.shinyapps.io/DS_Final_Project/


==================

Are you sometimes befuddled when choosing your best future investment? Are you tired of not catching up with the fluctuation of the financial market? Are you frustrated by the unpredictableness of your stocks? 

Don't worry if your answer is yes! Because equipped with the data science and business knowledge of our team, this app is going to bring you the most informative overview and helpful advice regarding your problem!  


## Project Description

This is a project about helping beginning investors to maximize their profits in the contemporary stock market. There are several major functions associated with the app that we built. It first offers a comprehensive summary of over 500 stocks during a 20 years time span. Based on stock's daily prices and trading `volume`, we further calculated more variables such as its `volatility`, `return` and so on. This essential information is used to create various schematic and analytical diagrams for users to have a better understanding of the performance of certain stocks and total expected profits from a portfolio of stock selections. 


## Data Sources & Files

  Our raw data source comes from Yahoo finance, a website that provides financial news, data and commentary including stock quotes, press releases, financial reports, and other original contents. In particular, our app uses its comprehensive record of 500 largest companys' stock/ETF (known as SP500) that gets traded on the U.S. stock exchanges from Jan 1, 2000 till the end of February, 2021. 


  For each stock, the raw data offers some basic information such as `company`, `sedol`, `sector`, `currency` etc. The main variables of interest are the stock's daily `open, close, high` and `low` prices, as well as the trading `volume` everyday. The `adjusted` value is an average of the stock's daily prices. In the subsequent analysis, we also calculated the `return`, `volatility` and `risk`(variation) of the stock to account for its long term performance.

  The raw data set, ranging from 500 stocks over 20 years, is massive and substantially larger than any other datasets we have processed. Therefore, the github page is unable to offer a preview. Nevertheless, we have uploaded it as csv format and named it `all_stocks_5yr.csv`. Users can download the raw data version from the folder above.

  In order to have a clear and succinct view of our dataset, we have also created a subsetted version of the SP500 dataset with only three days' record for each stock. Users could open our app and go to the Data Summary tab to have a direct view of the data. The same `SP500_all.csv` file in the folder above is also available for preview. The individual_clean version is a further trimmed data set with only `price` and `volume` information. 
  
  For graphing purporses, we also created three smaller data sets that recorded three popular market indicators on a daily basis. They are `DJI`, `SPY` and `QQQ`, which stand for Dow Jones Industrial Average, SPDR S&P 500 Trust ETF, and Nasdaq Invesco Trust Series. Their daily records are stored in three corresponding csv files above. 
  
  In addition, the `Prepwork.rmd` file is where our data-cleaning, pre-processing and algorithm function codes are stored. We saved our work in this separate file so that our main file does not need to repeat these steps everytime when the app runs.
  
  In terms of our main product, the `app.R` includes the essential codes for running our shiny app. All the pictures used in the app, which are in PNG format, are stored and accessable in the `wwww` folder.


## How to use Our App

  The `app.R` file is where our main code for the shiny app is stored. Using the url at the top of the page can also have access to the published version of our app on shinyapps.io. There are six major tabs embedded in our app. Each has its unique features and it is recommended to go through them by order. Notice that reading huge data sets takes time and some tabs that perform complex graphs and simulations may need slightly more time to load the pictures, so please be patient and we are sorry for the inconvenience. 

  - The first tab, called **Introduction**, is an overview of our app's purposes and functions. Users can also find some basic terminologies and guidances about the stock market. 
  
  - The second tab is **Data summary**, which provides users a picture of what our data set looks like. It is also capable of letting users search for a specific stock and see its `price` and `volume` on any day in the past 20 years.  

  - Starting from the third tab is our analytical work and visualization. The third Tab is **Market Distribution**. It displays the sector distribution in terms of count and trading volume within the S&P 500 comapnys. Users can also select one specific sector and compare the sector ETF’s performance to the general market’s (S&P 500 index) within a specified time frame. Annualized return and volatility of the sector ETF and S&P 500 index will also be generated as a reference.

  - The fourth tab **Market Trend** allows users to grab a general sense of how the market did in the past through featuring the three major stock indexes (Dow Jones Industrial Average, Standard & Poor 500, and Nasdaq 100). You could also manually adjust the time of focus and the tab provides data starting from Jan.1 2000. Users could also choose two stocks of interest and compare their past performances or past transaction volume. An option of comparing with the general market (S&P 500 index) is also provided. Hence, users could use this tab to compare and contrast how individual stocks and stock indexes performed in the past.
  
  - The fifth tab is **Stock Selection**, which allows users to input a stock of interest from the S&P 500 stocks universe. A k-means unsupervised algorithm was performed to separate the S&P 500 stocks into 5 different clusters based on return and volatility from 2013-2018. User's stock of interest will be displayed on the graph, which presents all S&P 500 stocks in different colored clusters. Users could easily find out which cluster the stock belongs. Additionally, through clicking on specific data points on the graph, the user could see which company's stock that data point represents and a table will be generated with the nearby similar stocks. Hence, users could use this tab to find out the general cluster that his/her/their stock of interest belongs. 
  
  - The sixth tab is **Understand Your Portfolio**, which allows users to input different stocks with different corresponding weights. A portfolio will be generated based on the stock and weight inputs. The portfolio will be compared with the S&P 500 from Mar.1 2016 to Mar.1 2021. The annualized return and volatility of the portfolio and the S&P 500 market index will be calculated and displayed in a table. Users could use this tab to back test his/her/their investment strategy with the S&P 500 index. 

  - The last tab is **Portfolio Optimization**, which allows users to input five different stocks with a targeted annualized return. Weights will be randomly generated and assigned to each stock and about 1000 different portfolios will be generated. Then, the system will automatically pick the portfolio with the minimum risk that achieved the targeted annualized over the past ten years. A weight of the five stocks will be displayed both as a graph and a table with the corresponding portfolio risk and return. Users could use this tab to construct an optimal portfolio to minimize risk based on past data.  


## Analytical Tools

  Our app operates mainly based on R languages and the `shiny` package (along with `shinyWidgets` and `shinytheme`), which give the foundation of how this interactive app looks like. A number of other useful R packages such as `ggplot2`, `tidyquant`, and `tidyverse`, are also necessary for us to construct the data set and the app itself. `tidyquant`, as a new package for us, is important for extracting information from the financial market.  
  
  Many of the tools and coding techniques are applications of what we have learned from the STATS 220 course this term. For example, over a dozen graphs embedded in this app are built based upon the useful `ggplot2` and `ggthemes` packages. We successfully implemented graphs of different types, such as line charts, scatterplot, and bar charts. 
  
  Our data summary and tables would not become possible without the `tidyverse`, `stringr` and `lubridate` packages. These packages offer us easy ways to filter, subset and calculate summary statistics from the complex and massive raw data set. The `lubridate` package is helpful in extracting the datetime information from the data set and building graphs that present a time trend. 
  
  Besides the extensive use of packages, we also frequently wrote functions, if/else statements and for loops in this app. There are many repeated calculations and logical connections between user's inputs and what the app generates. Therefore, functions are very critical in saving meaningless repetitions (especially in the last two tabs) and keep our structure of the app clean and efficient. 
  
  Moreover, the statistical knowledge we learned from this course also significantly contributed to our project. The k-means algorithm, is especially helpful for us to classify the SP500 stocks into different categories. We used different traits of the stocks to categorize all stocks, and users can specify a particular stock and observe which cluster of similar stocks it belongs to.   
  
  
 ## Conclusion
 
   Overall, this is an app that incorporates sophisticated data science techiniques as well as buiness metrics, capable of generating personalized investment analysis and suggestions. It is strongly recommended to utilize its interactive functions to adjust the desired visualization as needed. We hope that you will like this app and it can be of some help to your life. Please enjoy it and thank you very much! 

   
  


