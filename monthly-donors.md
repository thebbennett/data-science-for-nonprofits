---
title: "Monthly Donor Analysis"
output: html_document
---



## Monthly Donors: A Journey 

This project seeks to uncover the patterns underlying the monthly giving program in order to suggest new strategies to drive more members. 


First, we are going to read in the data and tidy up the set, only keeping the columns we want to use

```r
setwd("~/Documents/Monthly-Donor-Analysis/")

mydata <- read.csv("monthly-donors.csv", stringsAsFactors=FALSE)
drops <- c(1:2,4:12,14:36,39,41:74,76,78, 80:279)
lesscol <- mydata[, -(drops)]
colnames(lesscol)[2] <- "date"

library(readr)
lesscol$amount <- parse_number(lesscol$amount)


library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(tidyr)
split_time <- lesscol %>% separate(date, c('day', 'time'), sep=" +")
```

```
## Warning: Expected 2 pieces. Additional pieces discarded in 6556 rows [1, 2,
## 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].
```

```r
time_col <- 3
df <- split_time[,-(time_col)]

df$day <- as.Date(df$day, format = "%m/%d/%Y")
```


## Total Giving Since December 2016
By running a simple sum over the amount column, we discover that this monthly giving program has amassed over $\ 70,000 since its inception in December 2016.


```r
sum(df$amount)
```

```
## [1] 71856.35
```



## Donations per Month
Below we plot monthly giving over time up till June 2018. We can see that there is practically a constant rate of growth in the monthly giving program. If we continue on this path, how much are we going to grow?


```r
bymonth <- rowsum(df$amount, format(df$day, "%b-%Y"))
bymonth <- as.data.frame(as.table(bymonth))
bymonth <- bymonth[,-2]

names(bymonth) <- c("month", "amount")

library(zoo) #this is a little more forgiving:
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
bymonth$month <- as.yearmon((bymonth$month), "%b-%Y")
bymonth <- bymonth[order(bymonth[,1]),]


plot(bymonth, type="l", lwd = 4, col = "grey")
fit <- lm(bymonth$amount~bymonth$month)
abline(fit$coefficients, lwd = 2,lty = 2, col = "red")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)
## Finding the growth rate of the monthly giving program
Let's fit a linear model to this line plot.






```r
summary(fit)
```

```
## 
## Call:
## lm(formula = bymonth$amount ~ bymonth$month)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -1130.40  -240.86   -57.64   305.32   907.64 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -1.180e+07  5.373e+05  -21.97 8.05e-13 ***
## bymonth$month  5.853e+03  2.663e+02   21.98 8.00e-13 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 448.2 on 15 degrees of freedom
## Multiple R-squared:  0.9699,	Adjusted R-squared:  0.9679 
## F-statistic: 483.1 on 1 and 15 DF,  p-value: 8.004e-13
```

## How much are we earning now?

Below I find out currently how much we earn per month:


```r
subset(bymonth, bymonth=="Apr 2018")$amount
```

```
## [1] 6998.47
```

As of April 2018, the monthly giving program earns rounf $\ 7,000 per month.


## Projecting the monthly giving program to January 2019
By using the slope found above, we estimate that the monthly giving program will grow to $\ 17,207/month!


```r
x <- 2019
by_2019 <- 5.853e+03*x -1.180e+07
by_2019
```

```
## [1] 17207
```

## A bar plot, because it looks nicer



```r
library(ggplot2)
ggplot(bymonth, aes(x=month, y = amount))+
        geom_bar(stat = "identity", fill="#FF9999") 
```

```
## Don't know how to automatically pick scale for object of type yearmon. Defaulting to continuous.
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)


## Let's determine our unique monthly donors
Below we clean up the "name" column of our donor data and the aggregate the data frame so we have a row for each unique donor. We also rename some columns for clarity.


```r
# figure out the number of unique monthly donors, growth over time
df$signup_full_name <- tolower(df$signup_full_name)

bydonor <- aggregate(amount ~ signup_full_name, data=df, FUN=sum)
names(bydonor) <- c("full", 'amount')
```

We continue our analysis below. For each unique donor, I takefind their first donation and their most recent donation. I then combine those two dataframes together to create a full picture of their giving history (and call the dataframe "donor_history").

I also rename the columns since things got a little muddle up. I then join this dataframe up with our old pal "by donor" up above so we have a complete record for each unique donor!




```r
library(dplyr)
recent <- df %>% 
        group_by(signup_full_name) %>%
        slice(which.max(day))
oldest <- df %>%
        group_by(signup_full_name) %>%
        slice(which.min(day))
donor_history <- cbind(as.data.frame(recent), as.data.frame(oldest))

donor_history<- donor_history[,-(11:16)]

names(donor_history) <- c("amount", "recent_date", "state", "city", "zip", "first", "last", "full", "amount", "first_date")
donor_history <- donor_history[,-9]

library(plyr)
```

```
## -------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```r
donor_history <- join(donor_history, bydonor, by = c("full"))
names(donor_history)[10] <- c("total")
```


## Summary Statistics by Donor 
Below we run a basic summary analyzing the amount our donors give. The average donation amount is $\ 11.13, but is that truly representative of our donor base? We immediately see an outlier at $\ 100/mo as our max. That doesn't seem right!


```r
summary(donor_history$amount)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    5.00    5.00   11.13   10.00  100.00
```

Let's plot our monthly donor data as a histogram so we can see the distrubtion of how much our donors are giving.



```r
hist(donor_history$amount,
     main="Distribtuion of Monthly Donors",
     xlab="Amount",
     col="blue",
     breaks=20)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

As we can see, a lot of donors fall around the $\ 1- $\ 5/month range, and almost all of them fall below $\ 20/month!


As we hinted at above, the mean of our data set probably doesn't paint the full picture. Let's find the mode.

```r
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

Mode(donor_history$amount)
```

```
## [1] 5
```

The most common amount our donors give is $\ 5/month. This seems more reasonable.

## By the numbers

What percentage of our donors give exactly $\1 per month. Surprisingly, it's 3.8%. If we're mailing them a $\ 7 book and a pin, this seems like a big expense for the organization.



```r
sum(donor_history$amount == 1)/nrow(donor_history) * 100
```

```
## [1] 3.875969
```


Carrying on, we look at how many of our donors are giivng exactly $\ 5/month. It's a whopping 46%! Almost half our donors are giving $\ 5/month. Now we're starting to see where we can improve this program. 

```r
sum(donor_history$amount == 5)/nrow(donor_history) *100
```

```
## [1] 46.29014
```

## How many donors join each month?
Below I work the data to produce a bar plot of the number of donors that join our monthly giving program each month. As we can see, there's no clear linear pattern, but we can start to deduce some conclusions. For example, April had a low amount of new donors join -- this is when we changed our name. There was a huge spike in new donors around December, which matches well with our understanding of last minute winter giving. It also looks like the program started off strong in January 2017 when we ran our first (and only) monthly donor campaign. Maybe we should do more of those?


```r
include <- c(1,8,9)
monthly_join <- donor_history[,include]


include <- c(2,8,10)
monthly_last <- donor_history[,include]

#first and last donations 
flow <- join(monthly_join, monthly_last, by=c("full"))
flow <- transform(flow, duration = total/amount )


library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:plyr':
## 
##     here
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
flow$recent_date <- ymd(flow$recent_date)
april <- subset(flow, recent_date >= ymd("2018-03-24") & recent_date <= ymd("2018-04-24"))

lapsed <- flow[!(flow$full %in% april$full),]

detach("package:plyr", unload=TRUE) 
```

```
## Warning: 'plyr' namespace cannot be unloaded:
##   namespace 'plyr' is imported by 'ggplot2' so cannot be unloaded
```

```r
#how many people join each month
join_each_month <- flow %>% group_by(month=floor_date(first_date, "month")) %>%
        summarize(total.count = n())
        
library(scales)
```

```
## 
## Attaching package: 'scales'
```

```
## The following object is masked from 'package:readr':
## 
##     col_factor
```

```r
ggplot(join_each_month, aes(x=month, y = total.count))+
        geom_bar(stat = "identity") +
        theme_bw() +
        scale_x_date(date_breaks = "2 months", labels = date_format("%m-%Y"))
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png)

## How many people leave us each month?
Like the above work, I plot the number of donors that leave each month. Since the axes are not the same for these two plots, I overlay them below.


```r
leave_each_month<- lapsed %>% group_by(month=floor_date(recent_date, "month")) %>%
        summarize(total.count = n())

ggplot(leave_each_month, aes(x=month, y = total.count))+
        geom_bar(stat = "identity") +
        theme_bw() +
        scale_x_date(date_breaks = "2 months",labels = date_format("%m-%Y"))
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png)


## Combine Leaving and Joining Plots 


```r
p <- ggplot(NULL, aes(month, total.count)) +
        geom_bar(stat="identity", aes(fill="join_each_month"), data = join_each_month, alpha = 0.5) + 
        geom_bar(stat="identity", aes(fill="leave_each_month"), data = leave_each_month, alpha = 0.5)

p
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png)
                 
Let's dive into this. Our donor attrition rates seem fairly constant with a few spikes. However, one may be able to make a case that our attrition rates have been slowly increasing since December 2017. 

## How did our December fundraising appeals go?
I came on after our fundraising campaign in December, but just in time to play with the data. Below I subset the data for the month of December and clean it up.

Below is a bar plot of how much money was raised each day. Each vertical line represents a date where we sent out a fundraising appeal email to our list. It's clear that the last email sent preformed the best. What made that email special? What kind of language did the writer use? 


```r
december <- subset(flow, first_date >= ymd("2017-12-01") & first_date <= ymd("2018-01-01"))

byday <- december %>% group_by(day=floor_date(first_date, "day")) %>%
        summarize(total.count = n())


as.numeric(as.Date("2017-12-15"))
```

```
## [1] 17515
```

```r
as.numeric(as.Date("2017-12-21"))
```

```
## [1] 17521
```

```r
as.numeric(as.Date("2017-12-18"))
```

```
## [1] 17518
```

```r
ggplot(byday, aes(x=day, y = total.count))+
        geom_bar(stat = "identity") +
        theme_bw() +
         geom_vline(xintercept=17515)+
        geom_vline(xintercept=17521)+
        geom_vline(xintercept=17518)+
        scale_x_date(date_breaks = "3 day",labels = date_format("%d-%m"))
```

![plot of chunk unnamed-chunk-18](figure/unnamed-chunk-18-1.png)



## Active Donors 
Before we had been using our entire donor base. Below, I focus only on active donors and ignore everyone who's cancelled their membership with us.

When we run the same summary statistics, we see that they don't budge much. The median is still $\ 5 and the average hovers around $\ 11. Phew. Our analysis above wasn't for naught. 


```r
active <- subset(flow, recent_date >= ymd("2018-03-24") & recent_date <= ymd("2018-04-24"))

summary(active$amount)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##    1.00    5.00    5.00   11.18   10.00  100.00
```


## Upgrading Donors 
Above we discovered that practically half our donor base give $\5 or less. That's a lot of room for improvement. I've come across several big name organizations (Greenpeace, 350.org) that implement a minimum monthly gift amount. Below I toy with some numbers.

First, what if we upgraded all our donors giving less than 5 dollars a month to 5 dollars a month. How much extra revenue would that bring us?


```r
less_than_five <- subset(active, active$amount<5)

nrow(less_than_five)*5*12 - sum(less_than_five$amount)*12
```

```
## [1] 2178
```

It looks like that would amass us an extra $\ 2.1k a year at the *most*. Not a lot, but it's not nothing.

However, what if we encouraged all our $\ 5\month donors to double their impact by doubling their gift? Remember, that's a whopping 45% of our donor base. 



```r
exactly_five <- subset(active, active$amount == 5)

nrow(exactly_five)*10*12 - sum(exactly_five$amount)*12
```

```
## [1] 20820
```

Wow! That would add an additional $\ 20k a year to our revenue stream! Now we're cooking.

Now, what if we set the minimum monthly donation to be $\ 10. That might deter some new donors, but it isn't an unheard of practice in the field. Street canvasers for large nonprofits are told to only to accept donations of $\ 15  month or more. 


```r
less_than_ten <- subset(active, active$amount<10)

nrow(less_than_ten)*10*12 - sum(less_than_ten$amount)*12
```

```
## [1] 26977.44
```

Upgrading our less than $\ 10/month donors leads to almost $\ 27,000 of additional revenue a year. It sounds like we need to run an upgrade campaign. Upgrade campaigns can be messy (logistically, technolgically), but I'm sure we can find a way.


##Pushing people to give between $15-$30 a day

Continuing on with our feeling that we should run an upgrade campaign, let's play around with some numbers. If just 1% of our membership gave $\ 15/month and another 1% of our membership gave $\ 30/month, we'd amass an extra $\ 41k/year. While toying around the 5 and 10 dollar amounts is reasonable, it isn't too far fetched to poke those extra special donors to give 15 or even 30 dollars a month. After all, $\ 30/month is just a dollar a day. 


```r
number_donors <- nrow(active)
percent_15 <- 0.1
percent_30 <- 0.1

cash <- (percent_15*15*number_donors + percent_30*30*number_donors) * 12
cash
```

```
## [1] 41310
```

## Playing around with numbers, again

Below I present a hypothetical distribution of monthly donors. Say 60we implement a minimum of $\ 10/month. Given that practically half our donors give below this amount, I estimate that a good 60% will give exactly $\ 10/month.

Given my experience with monthly giving programs, I strongly suggest implementing tiered giving. That is, have set amoutns people can give with clear benefits for each.

Below I give some rough estimates for the percentage of donors that will opt for each tier. 



```r
low <- 0.6 * 10
mid <- 0.35 * 15
high <- 0.04 * 30
very_high <- 0.01 * 83.33


new_monthly_donors <- 30 + (15* 12) + 200

total_revenue <- (new_monthly_donors*low+new_monthly_donors*mid+new_monthly_donors*high+new_monthly_donors*very_high)
total_revenue
```

```
## [1] 5446.153
```

```r
acq <- 5000

total_revenue <- acq * ( low + mid + high + very_high)
```

If we implement the tiered program, we could be earning * $\ 5,446/month * *additionally* on top of the $\ 7,000 per month we already earn. That's without an upgrade campaign, which could add another $\ 2,000


## Conclusion and Recommendations 

There are a lot of things this monthly giving program is doing well. We've amassed a pool of almost 800 donors totalling roughy $\ 7,000 a month. But, there are a lot of small tweaks that can be implemented for big results. Let's review some of the biggest findings of this report:

### *Upggrade Donors*
This is least rewarding of the strategies I suggest. I don't recommend spending too much time here. Howver, a quick couple of email blasts to our membership might result in a couple of upgrades. It won't produce much extra revenue, but it is always a good strategy to ask your donors to upgrade every now and then.


### *Create a Minimum*
This will be *vital* to the success of this program. Without a minimum, we are losing money for donors who give less that $\ 3/month. We know a minimum works because big name nonprofits institute a minimum. It might take some experimenting to find the threshold that captures the most donors while still pushing people to give at their highest capacity. 

A $\ 5 minimum will be good enough, but a $\ 10/month minimum can produce even stronger results. 


### *Implement Tiers*
Since this organization was about to run a big aquisition campaign to bring in thousands of donors, it became clear to me that the most rewarding strategy moving forward would be to capture people at their highest giving level.

In my experience, giving tiers push people to give at their highest capacity. By creating solid tiers with explicit perks -- or clear indicators of value-- you encourage people to give more. This, combined with a minimum, will bring great results tot he fundraising program.

OVerall, I estimate that we could gain $\ 5,446/month * *additionally* on top of the $\ 7,000 per month we already earn. The upgrade campaign would add another $\ 2,000/month.

## Overall, the new revenue for this organization would *double*, totalling  $\ 14,446!


