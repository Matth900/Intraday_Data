# ===================== INTRADAY SCRAPING - YAHOO FINANCE - JSON OBJECT ===================
## Basic Scraping tool to get some Intraday Data from Interactive Charts JSON source parsed from Yahoo Finance

Intraday_yf<-function(ticker,s_date,e_date,freq) {
  
  library(jsonlite) # Need to Load JSON Parser Library
  
  # =========== STEP 1: Dates =========================
  ## Converting Dates input by the user in seconds since EPOCH = January 1, 1970 - Dates have to be in the format YYYY-mm-dd hh:mm:ss
  
  temp_s <- as.POSIXct(s_date) # POSIXct stores the time as seconds since epoch. 
  
  p1<-as.integer(temp_s)
  
  temp_e<-as.POSIXct(e_date)
  
  p2<-as.integer(temp_e)
  
  # =========== STEP 2: Download JSON ================
 
  url<-"https://finance-yql.media.yahoo.com/v7/finance/chart/" %+% ticker %+% "?period2=" %+% p2 %+% "&period1=" %+% p1 %+% "&interval=15m&indicators=quote&includeTimestamps=true&includePrePost=false&events=div|split|earn&corsDomain=finance.yahoo.com"
    
  raw_data<-fromJSON(url)
  
  # Cleaning the PARSED JSON which is returned by the package JSON Lite as a Nested List
  temp_data <-raw_data$chart$result$indicators$quote
  
  names(temp_data)<-"Main"
  
  clean_data<-temp_data$Main
  
  # Getting Vectors representing OHLC + V Object and binding them in a single DataFrame to be returned to the user
  high<-clean_data$high
  names(high)<-"High"
  high<-high$High
  
  open<-clean_data$open
  names(open)<-"Open"
  open<-open$Open
  
  low<-clean_data$low
  names(low)<-"Low"
  low<-low$Low
  
  close<-clean_data$close
  names(close)<-"Close"
  close<-close$Close
  
  volume<-clean_data$volume
  names(volume)<-"Volume"
  volume<-volume$Volume
  
  output<-data.frame("High"=high, "Open"=open, "Low"=low, "Close" = close, "Volume"= volume)
  
  # Getting the TimeStamps - The JSON gives them as seconds since EPOCH. Convert them using As.POSIXct
  
  dates<-raw_data$chart$result$timestamp
  names(dates)<-"Dates"
  dates<-dates$Dates
  dates<-as.POSIXct(dates, origin ="1970-01-01")
  
  rownames(output)<-dates
  
  output
}


Intra_returns<-function(stock_frame) {
  
  # The assumption here  is that the user returns an OHLCV Intraday Frame 
  ## See starting routine on how to get this
  
  stock_frame$Log_Returns<-c(0,diff(log(stock_frame$Close)))
  
}



rolling_intra_sd<-function(stockframe) {
  
  
  # REQUIRED INPUT: stockrame must be an OHLCV DataFrame or xts/ts/zoo object (See first routing)
  
  ########################### INSERT CODE #################################
  ## Note that we must have a column named ret_sq...ADJUST FOR THAT
  
  ## Step 1- Calculate Squared  Returns
  
  log_ret<-Intra_returns(stockframe)
  
  stockframe$ret<-log_ret
  stockframe$ret_sq<-log_ret^2
  
  ## Step 2- Calculate Rolling Variance and Rolling Standard Deviation (zero mean assumption, equal weight)
  rolling_var<-c()
  
  for (i in seq(1,nrow(stockframe),26)) { 
    
    for (j  in seq(1,26)) {
        
        if (j==1) {
          
          new_item<-c(stockframe[i+1,"ret_sq"])
          
        } else {
          
          #new_item<-(stockframe[i+j,"ret_sq"]+stockframe[i+j-1,"ret_sq"])/(j-1) }
          
          new_item<-(stockframe[i+j,"ret_sq"]+(rolling_var[length(rolling_var)]*(j-1)))/(j) 
          
        }
        
        rolling_var<-c(rolling_var,new_item)
    
    }
  
}

stockframe$var <- c(0,rolling_var[1:length(rolling_var)-1])
stockframe$sd <- c(0,sqrt(rolling_var[1:length(rolling_var)-1]))


par(mfrow=c(3,3))
# Standard Deviation Plot
for (i in seq(0,nrow(stockframe)-26,26)) {
  
  plot(stockframe[(i+1):(i+26),"sd"],type="l", col="blue",lwd=2)
  
  # The Par NEW Option is useful to plot SEPARATELY the two series on the same chart
  par(new=TRUE)
  
  plot(stockframe[(i+1):(i+26),"Volume"],type="l",axes = F, xlab = NA, ylab = NA,col="red",lwd=2) # We need to remove the axis so that we can plot it later on the right
  
  # For better understanding of Double Axis Plotting, see:
  ## http://www.carlislerainey.com/2012/12/28/how-to-add-an-extra-vertical-axis-to-r-plots/
  
  axis(side=4)}

par(mfrow=c(1,1))

stockframe

}

volumes_multiple<-function(stocks,date_s,date_e) {
  
  vols_list<-list()
  
  require(dplyr)
  
  for (stock in stocks) {
    
    temp<- Intraday_yf(stock,date_s,date_e)
    
    tso<-tso_yf(stock)
    
    temp<-subset(temp,Volume!=0)
    
    temp<-mutate(temp,turnover=(Volume/tso)*100)
    
    vols_list<-c(vols_list,label=list(temp$turnover)) 
    
  }
  
  df_vols<-as.data.frame(vols_list)
  
  colnames(df_vols)<-stocks
  
  df_vols
  
  
}


# Funtion to analyse / predict Intraday Volume - (15 Minutes - 25/26 Ticks per Day)
## Factor Analysis + AR(1) or SETAR

volume_analysis<-function(df) {
  
  # The input for this analysis must be an OHLCV DataFrame as downloaded through the Intraday Function
  
  # We use the library tseries as well as QuandL to download TSO (Total Shares Outstanding) which we need to get the Intraday Turnover
  #library(tseries)
  #library(Quandl)
  
  # In order to manage better the DataFrame we will use two packages
  #require(dplyr)
  require(data.table)

  
  clean_vol<-subset(df,Volume!=0)
  
  # STEP1 Add a further column to the dataframe, starting from the rownames to indicate Hours and Minutes
  
  times<-rownames(clean_vol)
  
  # Convert the Times to POSIXlt
  
  pos_times <- as.POSIXlt(times,format="%Y-%m-%d %H:%M")
  
  clean_times<-format(pos_times, "%H:%M")
  
  clean_vol$Times<-clean_times
  
  table<-data.table(clean_vol)
  
  average_volume<-table[,mean(Volume),by=Times]
  
  average_volume

  # Get Average Volumes
  
  # Get TSO (Total Shares Outstanding)
  ## 1st Choice - csv or JSON from QuandL
  ## 2nd Chocice - Quantmod?
  ## 3rd Choice - Google Finane Double (NYSE-NASDAQ) webscraping
  
}

# Create a Turnover DataTable/Frame for multiple stocks/tickers

turnover_frame<-function(stocks,date_s,date_e) {

turn_list<- list()

require(dplyr)
  
  for (stock in stocks) {
    
    temp<- Intraday_yf(stock,date_s,date_e) 
    
    volumes<-volume_analysis(temp)
    
    # At this point we make use of the the Package DPLYR, which further extends methods of data tables and data frames
    
    tso<-tso_yf(stock)
    
    volumes<-mutate(volumes,turnover=(V1/tso)*100)
  
    turn_list<-c(turn_list,label=list(volumes$turnover)) 
    
      
    }
    

df_turnover<-as.data.frame(turn_list)

colnames(df_turnover)<-stocks

df_turnover

}
# Plot volumes for multiple tickers
plot_volumes<-function(stocks,date_s,date_e) {

  # Setting some global options for  charting
  columns_plot<-floor(length(stocks)/3)+1
  
  par(mfrow=c(columns_plot,3))
  
  par(mar=c(2,2,2,2))
  
  for (stock in stocks) {

    temp<- Intraday_yf(stock,date_s,date_e) 
    
    volumes<-volume_analysis(temp)
    
    # At this point we load the Package DPLYR, which further extends methods of data tables and data frames
    
    require(dplyr)
    
    tso<-tso_yf(stock)
    
    volumes<-mutate(volumes,turnover=(V1/tso)*100)
    
    with(volumes,plot(turnover,type="l",main=stock %+% " 15 min-day Vol"))}

}


# Forecasting Volume for a multiple stocks

volume_forecasting<-function(stocks,date_s,date_e){
  
  # INPUT - a list of tickers of different stocks (possibly liquid one..)
  
  # FIRST STEP - Use Routine on Turnover Frame to get a Table with the Turnover for each  stock
  ## Frequency Used all the Module = 15 Minuts - 26 Time Slots (Pre/Post Trading Excluded)
  
  turnover_data<-volumes_multiple(stocks,date_s,date_e)
  
  # SECOND STEP - Use the 2008 (Bialkowski,Darolles) Factor Model for Intraday Forecasting
  ## The specific version of the fator model is carried out by Bai,J.  (2003)
  ### For further details see Master R for Quantitative Finance (Pag. 61)
  
  # The Model is that of Principal Components + AR(1) for the Residuals (orin alternative SETAR)
  
  ## Estimating the Seasonal Component 
  
  ### Define the Sample Period (All Dayss - The Last One (Out-Of Sample Testing))
  n<-nrow(turnover_data)-26
  
  m<-ncol(turnover_data)
  
  sample<-as.matrix(turnover_data[1:n,])
  
  
  
  
}




# Some Helper Functions to paste the  URL

'%+%'<-function(str1,str2) {
  
  paste(str1,str2, sep="")
  
}

# Retrieve Total Shares Outstanding from Quandl (ZACK PREMIUM DATABASE - FREE TRIAL)
tso_quandl<-function(ticker) {
  
  # Downloading TSO through the QuandL R Library and the Zacks-A Fundamentals
  ## Please Note: This Database is in free-trial and will expire in a week
  
  # Note we select only the last vaue (first row, second column)
  tso<-Quandl("ZFA/" %+% ticker %+% "_COMM_SHARES_OUT_Q", authcode="YA4nLmWHJ6937eqEeMN7")[1,2]
  
  tso<-tso*1000000
  
  tso
  
}

# Retrieve Total Shares Outstanding (LAST) from  Yahoo Finance through HTML RegEx Scraping
tso_yf<-function(ticker) {
  
  # To read the HTML Page as text, we're going to use the HTTR Page
  
  library(httr)
  
  library(XML)
  
  url<-"https://www.google.com/finance?q=NASDAQ%3A" %+% ticker %+% "&fstype=ii&ei=mYqWVan4GcPKUeHigfAB"
  
  request<-GET(url)
  
  page<-content(request)
  
  # Scrape the HTML using XPath

  key<-"//td[@class='r bld']"
  
  tso<-xpathApply(page,key,xmlValue)
  
  tso<-tso[146][[1]] # In google Finance teh Shares Outstanding are the 146 element in the all the fin statements tables
  
  # This returns a list of ONE ELEMENT. Either NUll (check for NYSE) or the number we want
  
  if (is.null(tso) == FALSE) {
    
    as.numeric(gsub(',', '', tso)) *1000000 
    
    } else {
      
    url<-"https://www.google.com/finance?q=NYSE%3A" %+% ticker %+% "&fstype=ii&ei=mYqWVan4GcPKUeHigfAB"
      
    request<-GET(url)
      
    page<-content(request)
      
    # Scrape the HTML using XPath
      
    key<-"//td[@class='r bld']"
      
    tso<-xpathApply(page,key,xmlValue)
      
    tso<-tso[146][[1]]
      
    as.numeric(gsub(',', '', tso)) *1000000 
      
    }  
 
  }    
