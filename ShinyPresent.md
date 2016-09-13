<style>
.small-code pre code {
  font-size: 10px;
}
</style>

Medial Subset and Counter Balanced Median
========================================================
author: David A Vaillette
date: 9/12/2016
autosize: true
A way to present central tendencies of granular data in a smooth way.
<p><p align="right">
<font size=3 >
Thanks to <a href="http://www.retrosheet.org/"> Retrosheet</a> and <a href="http://www.seanlahman.com/baseball-archive/statistics/"> the Lahman Database</a> for data used with this presentation.
</font>

 
Definitions
========================================================

<font size=5>
<b>Rank Ordered Set: </b> A rank ordered set is a set of numbers sorted from either largest to smallest or smallest to largest.  Each element of the set receives a unique integer value starting with one and continuing sequentially.

<b>Medial Subset: </b> A subset of numbers from a set that retains the rank ordered identifier from the original set.  The AVERAGE of the retained rank ordered numbers will be exactly the same as the AVERAGE of the rank ordering from the original set.  A median from a set with an odd number of values is a medial subset of size 1.  A subset containing the minimum and maximum values from a set is a medial subset.

<b>Counter Balanced Median: </b> The mean of a medial subset with the following characteristics:
- it contains the median
- it contains the at most 1 other value (but probably multiple examples of that value)
- it is the largest possible subset that that fulfills the above 3 requirements
</font>
The Function
========================================================
<font size=4>Scoring is the set of runs scored by Major League Baseball Teams from 1970-2013*

```r
counter.balanced.median<- function(x){
  len<-length(x)
  x<-sort(x)
  x<-data.frame(x)
  x$row<-row.names(x)
  med<-median(x$x)
#test if the calculated median is part of the set, and if not
#return the calculated median.
  if(identical(as.numeric(x[x$x==med,2]),numeric(0))) return(med)
#find the range of the median
  Min<-min(as.numeric(x[x$x==med,2]))
  Max<-max(as.numeric(x[x$x==med,2]))
#Where is the center of the medial range?
  if(abs(Min-(len+1)/2)>abs(Max-(len+1)/2)){
#What is the number next to the median?
    temp1<-x[Max+1,1]
#How far does that number go?
    temp2<-max(as.numeric(x[x$x==temp1,2]))
#What is the counterbalence to the mean?
    temp3<-len+1-Min
#Which is smaller?
    cb2<-min(c(temp2,temp3))
#What is the counter balence to that number(cb2)?
    cb1<-len+1-cb2
  }else{
    temp1<-x[Min-1,1]
    temp2<-min(as.numeric(x[x$x==temp1,2]))
    temp3<-len+1-Max
    cb1<-max(c(temp2,temp3))
    cb2<-len+1-cb1}
  cb<-mean(x[cb1:cb2,1])
  cb}
Scoring<-read.csv("TotalScoring.csv")
counter.balanced.median(Scoring$Own)
```

```
[1] 3.966992
```
</font>
The Counter Balanced Median At Work
========================================================
<font size=4>As we saw on the previous page, the Counter Balenced Median for the years 1970-2013 in Major League Baseball is 3.97.  The medial set<br> of numbers that we took a mean of, therefore, consisted of 3s and 4s and mainly of 4s.  If we took a look at the median of the same set it will<br> be 4.


```r
median(Scoring$Own)
```

```
[1] 4
```
For any set of consecutive integers (such as baseball scores) the median can be computed from the counter balanced median by rounding unless the median is not itself an integer.  Such is the case when we have an even number of observations and the 2 "middle terms"" do not match.  In this case we compute the median from a medial subset of size 2, so we are already used to doing this.  Go to <a href="https://dvdoc555.shinyapps.io/Shiny/" target="_blank"> my shiny application</a> and choose 1987 for the year and TEX for the team. 

```r
median(Scoring[Scoring$Team=="TEX"&Scoring$Year==1987,3])
```

```
[1] 4.5
```
The median and the Counter Balanced Median match in this case. Try adding terms to the data for "Own" and see what happens to the Counter balanced median and the mean.
As you can see from the TEX 1987 page of my website adding any integer value greater than 5 increases the size of the Counter Balanced Median to 4.52 and any integer value less than 4 to 4.48.  (These results can be found in the "Results with Added Data" section) Adding other data will affect the mean and the Counter Balanced Median.  Play with it.
</font>

Final Discussion
===
<font size=5>
  I feel that due it it's smoothness compared to the median, the counter balanced median is a better indicator of central tendency for applications like sports scores, on-line movie reviews, any data with integer or otherwise limited orderable data values.<p>
  My feeling are not important however.  What is important is if the value is useful in prediction.  In this acccount you will find a file called Teams.csv and a file that shows how I got from TotalScoring.csv (Scoring) to Teams.csv.  It is the data displayed on the website. I will not put the following code into R, because it will make this page take far to long to run, but I ran the code:<br>

set.seed(406)<br>
fitrf1<-train(WP~Own+Opp+Own.CBM+Opp.CBM+SDOwn+SDOpp,data=Teams,importance=T)
<p>
I was interested in seeing the importance of the variable values R would come up with.  I did find the results surprising.  When I ran varImp(fitrf1) my 2nd most important variable was Opp.CBM (with a score of 69)</font>
