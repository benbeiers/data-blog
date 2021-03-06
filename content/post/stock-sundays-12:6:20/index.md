---
title: Stock Sundays - 12/06/2020
subtitle: A foray into algorithmic trading
summary: Each week, I post large-cap stocks poised for a strong week using a technicals-based algorithm.
authors: 
- admin
tags: ["Stock Sundays"]
categories: []
date: "2019-12-06T00:00:00Z"
lastMod: "2019-12-06T00:00:00Z"
featured: false
draft: false

# Featured image
# To use, add an image named `featured.jpg/png` to your page's folder. 
image:
  caption: "Pick of the week: Gap Inc"
  focal_point: ""

# Projects (optional).
#   Associate this post with one or more of your projects.
#   Simply enter your project's folder or file name without extension.
#   E.g. `projects = ["internal-project"]` references 
#   `content/project/deep-learning/index.md`.
#   Otherwise, set `projects = []`.
projects: ["Stock Sundays"]
---

I've written an R program that uses Moving Average ratios weighted by Relative Strength Index to find large-cap stocks with strong momentum at optimal buying times. In a weekly blog series, I will give my program's "pick of the week" in addition to its favorite stock from every other GICS sector. Every post will update how my stocks have performed relative to indices, a strong indicator of success.

## Recap

Week three saw profits, though this algorithm was outperformed by the S&P index by over one percentage point. Gap, my algorithm's pick of the week, had a poor week, falling  over a percent. Seven of the eleven picks were profitable on the week, the strongest being WMB with a weekly gain of over four percent. Here's how each individual stock performed:

![png](./11.29.2020IndividualStockReturns.png)

To track this algorithm's overall performance, I started with $11,000 split evenly between the eleven GICS sectors. Each week, the value of the portfolio will be calculated and redistributed evenly between the eleven new stocks. After week three, here is how my algorithm's portolio stands as compared to the S&P 500:

![png](./11.29.2020weekThreeResults.png)

The portfolio generated by this algorithm found itself up .77% this week and made $86.23, as compared to SPX's 1.77% gain. Overall, my portfolio is up 3.25% while the S&P 500 is up 1.8%.

## Pick of the week: Gap Inc (GPS)

My algorithm's pick of the week is the same as last week: Gap Inc, a Consumer Discretionary stock that is a well-known clothing retailer. Since bottoming in late-March at just over $5, this stock has been on an absolute tear and quintupled from its lows. Gap pulled back after its earnings call last week which suggested its stock price had overgrown its financials. Optimism around Gap stems from growth in its Old Navy and Athleta lines. While Gap disappointed as this algorithm's pick of the week in week three, this algorithm has faith that last week was a temporary setback and that Gap will continue its uptrend.

![png](./Discretionary12.06.2020.png)

## Information Technology: Salesforce, Inc (CRM)

![png](./Tech12.06.2020.png)

## Health Care: Mettler-Toledo International Inc (MTD)

![png](./Health12.06.2020.png)

## Communication Services: Charter Communications Inc (CHTR)

![png](./Comms12.06.2020.png)

## Financials: Assurant, Inc (AIZ)

![png](./Financials12.06.2020.png)

## Industrials: Quanta Services Inc (PWR)

![png](./Industrials12.06.2020.png)

## Consumer Staples: Costco Wholesale Corporation (COST)

![png](./Staples12.06.2020.png)

## Utilities: American Water Works Company Inc (AWK)

![png](./Utilities12.06.2020.png)

## Real Estate: Ventas, Inc (VTR)

![png](./Estate12.06.2020.png)

## Materials: Celanese Corporation (CE)

![png](./Materials12.06.2020.png)

## Energy: Cabot Oil & Gas Corporation (COG)

![png](./Energy12.06.2020.png)

Disclaimer: We are not registered as a securities broker-dealer or an investment adviser either with the U.S. Securities and Exchange Commission (the “SEC”) or with any state securities regulatory authority. We are neither licensed nor qualified to provide investment advice. Do not base any investment decision upon any material found on this website.