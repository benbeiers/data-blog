---
title: Stock Sundays Introduction
subtitle: A foray into algorithmic trading
summary: Each week, I post large-cap stocks poised for a strong week using a technicals-based algorithm.
authors:
- admin
tags: ["Stock Sundays"]
categories: []
date: "2019-11-14T00:00:00Z"
lastMod: "2019-11-14T00:00:00Z"
featured: false
draft: false

# Featured image
# To use, add an image named `featured.jpg/png` to your page's folder. 
image:
  caption: ""
  focal_point: ""

# Projects (optional).
#   Associate this post with one or more of your projects.
#   Simply enter your project's folder or file name without extension.
#   E.g. `projects = ["internal-project"]` references 
#   `content/project/deep-learning/index.md`.
#   Otherwise, set `projects = []`.
projects: ["Stock Sundays"]
---
## What to Expect

In a blog series, I will post weekly stocks chosen by an R program that analyzes S&P 500 stocks for short term technical signals. Every Sunday, I will post my algorithm's favorite "stock of the week" with a writeup in addition to its preferred stock from each of the other ten GICS sectors. Since equity movement is highly correlated with equity movement within its broader sector, I will measure my algorithm's performance in comparison sector indices. Every post after the first will also recap the previous trading week and how my program's stocks fared relative to their sector and the market. The S&P 500 was chosen as the primary benchmark because this algorithm only considers stocks within it.

## R Program Methodology

My R program uses Moving Average ratios weighted by a stock's Relative Strength Index to find large-cap stocks with strong momentum at optimal buying times. 

This program makes three assumptions common to technical analysis. The first is that the market is near-efficient and has already priced in the data underlying price variation. The second assumption is that prices typically move in trends. The third is that history repeats itself, so pattern analysis is valuable. To strengthen these assumptions, I decided to focus on S&P 500 stocks. This is because they are traded at high volumes, and because they must already have very strong fundamentals.

With these three assumptions in mind, my investment goal is simple: find stocks with strong momentum, at optimal buying times. I focus on the price trends of stocks to achieve this, instead of focusing on what causes the stock to move. 

## R Program

Now, to the actual program itself. I used several libraries to construct this program, the most vital being QuantMod. QuantMod allowed me to easily pull pricing data from Yahoo Finance. It also had some useful graphical tools that helped with visualization.

First, I scraped S&P data from Wikipedia, focusing on the Symbols and GICS Sectors. I used these columns to build a map of S&P data with Symbols mapped to Sectors: 

```r
sp500_wiki <- read_html(
  "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies")
symbols_table <- sp500_wiki %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()
industry_table <- symbols_table[[1]]$'GICS Sector'
symbol_table <- symbols_table[[1]]$Symbol
SPXstocks <- Map(c, symbol_table, industry_table)

```

Next, I wrote several functions which I would integrate into the final algorithm. I wrote two sorts: sectorSort and priceSort. These would allow me to filter to relevant stocks and decrease the runtime of my final algorithm by letting it consider fewer stocks.

On to the functions to be used within the actual algorithm. I wrote movingAverage to take a symbol, get the pricing data of it, and calculate its Simple Moving Average over x days. Here's this function:

```r
movingAverage <- function(symbol, days = 50) {
  stockPrices <- getSymbols(symbol, auto.assign = FALSE)
  closingPrices <- tail(stockPrices[,4], y)
  return((sum(closingPrices[,1]))/days)
}

```

Then, I wrote relativeStrengthIndex to calculate the RSI of a stock from a given date. This function was a bit more complex. It involved figuring out how often prices increased or decreased, and by how much:

```r
relativeStrengthIndex <- function(stock, days = 14, r = Sys.Date()) {
  stockPrices <- getSymbols(stock, auto.assign = FALSE, to = r)
  closingPrices <- tail(stockPrices[,4], days)
  gains = 0
  losses = 0
  gainCounter = 0
  lossCounter = 0
  for (i in 2:days) {
    if (closingPrices[[i]] >= closingPrices[[i-1]]) {
      gainCounter = gainCounter + 1
      gains = gains + (closingPrices[[i]] - closingPrices[[i-1]])
    }
    else{
      lossCounter = lossCounter + 1
      losses = losses - (closingPrices[[i]] - closingPrices[[i-1]])
    }
  }
  avgGains = gains / gainCounter
  avgLosses = losses / lossCounter
  relStrength <- (100 - 100 / (1 + avgGains / avgLosses ))
  return(relStrength)
}

```

I incorporated these two functions into a final algorithm, stockPicker, that weighted ratios of different length moving averages by RSI. The weights were fine-tuned using training data from 2017-2019, since 2020 has been such a unique year for markets. This means that my algorithm could be most well-suited for a more normal trading environment that closely mimics 2017-2019, but I believe that its weighting and methodology will still apply well to late 2020 and early 2021.

## Future Goals for Stock Sundays

I hope to turn towards options trading and rigorously determine the best options strategies for a given stock play. I have written a couple of functions to help in this process. 

There is no easy way to figure out the nearest options expiration using QuantMod, so I wrote a function findExpiry that takes a stock and a number of weeks desired to expiry:

```r
findExpiry <- function(stock, weeks, z = Sys.Date()) {
  dates <- getOptionChain(stock, Exp = NULL)
  n <- names(dates)
  yMod <- z + (7*weeks)
  
  returnVal <- list()
  j <- 1
  for (i in c(1:length(n))) {
    f <- n[[i]]
    p <- as.Date(f, format = "%b.%d.%Y")
    if (p >= yMod) {
      returnVal[[j]] <- n[[i]] 
      j <- j+1
    }
  }
  as.Date(returnVal[[1]], format = "%b.%d.%Y")
}

```

I also wrote a function, optionIdeas, that finds near at-the-money options for a stock given a strategy of "calls" or "puts", weeks to expiry, and a list of stocks. It returns all available strike prices within 5% of a stock's current quote at the nearest expiry, with the chosen strategy:

```r
optionIdeas <- function(strategy = "calls", weeks, z = SPXstocks) {
  if (strategy == "calls") {
    stock <- stockPicker(z)
    d <- findExpiry(stock, weeks)
    options <- getOptionChain(stock, d)
    stockOptions <- options$calls
  } else {
    stock <- stockPicker(z, dir = "down")
    d <- findExpiry(stock, weeks)
    options <- getOptionChain(stock, d)
    stockOptions <- options$puts
  }
  cp <- getQuote(stock)
  cp <- cp$Last
  cpHigh <- cp + cp * .05
  cpLow <- cp - cp * .05
  stockOptions <- filter(stockOptions, stockOptions$Strike > cpLow)
  stockOptions <- filter(stockOptions, stockOptions$Strike < cpHigh)
  return(stockOptions)
}
```