---
title: Stock Sundays Introduction
subtitle: A foray into algorithmic trading
summary: Each week, I post large-cap stocks poised for a strong week using a technicals-based algorithm
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

In a blog series, I will post weekly stocks picked by a novel R program that analyzes S&P 500 stocks for short term technical signals. Every Sunday, I will post my algorithm's favorite "stock of the week" in addition to its preferred stock from each of the other ten GICS sectors. Since equity movement is highly correlated with equity movement within its broader sector, I will compare my algorithm's performance to sector indices. Every post after the first will also recap the previous trading week, and how my program's stock picks fared relative to their sector.

## R Program Methodology

My R program uses Moving Average ratios weighted by a stock's Relative Strength Index to find large-cap stocks with strong momentum at optimal buying times. 

This program makes three assumptions common to technical analysis. The first is that the market is near-efficient and has already priced in the data underlying price variation. The second assumption is that prices typically move in trends. The third is that history repeats itself, so pattern analysis is valuable. To strengthen these assumptions, I decided to focus on S&P 500 stocks. This is because they are traded at high volumes, and because they must already have very strong fundamentals.

With these three assumptions in mind, my investment goal is simple: find stocks with strong momentum, at optimal buying times. I focus on the price trends of stocks to achieve this, instead of focusing on what causes the stock to move. 

## R Program

Now, to the actual program itself. I used several libraries to construct this program, the most vital being QuantMod. QuantMod allowed me to easily pull pricing data from Yahoo Finance. It also had some useful graphical tools that helped with visualization.

First, I scraped S&P data from Wikipedia, focusing on the Symbols and GICS Sectors. I used these columns to build a map of S&P data with Symbols mapped to Sectors. This looked like this: 
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