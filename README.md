# Coronavirus(COVID-19) Real-time Dynamics
- An interactive R shiny app to track real-time dynamic information of COVID-19.
&emsp; https://coronavirus-real-time-dynamics.shinyapps.io/COVID-19-Real-Time-Dynamics/

### Author
Yue Lyu, Yujia Lu, Naijia Wang, Yuqing Yang 
***
### Motivation
Coronavirus has spread to more than 190 countries and become a global pandemic. The aim of this App is to provide worldwide real-time information about new coronavirus outbreak, including World Trend showing virus spreading; Detailed Situation with deaths, recovered and active cases; Daily Report that allowed country comparisons; Typical Country Data that people may concern about; Breaking News reported about "coronavirus" in four countries; More Information where you could find more about the virus and U.S. testing.  
***

### User Guide
- World Trend (home page):  


&emsp; Our home page is the real-time world situation map showing the total cases confirmed, deaths and recovered. Users can choose countries that they are interested in to see the further details of that country.  

![image](https://github.com/yuelyuu/COVID-19/blob/master/Dashboard.png)

- Detailed Situation:  


&emsp; The second page contains a table that shows ten countries by default each page and can be sort by order of any column or filter by typing the name of the country or setting range of each numeric statistics.  

- Daily Report:  


&emsp; It is the line chart of the cumulative confirmed and daily new confirmed, death or recovered cases of different countries. Users can choose the countries they are interested in by selecting the country in the top left bar called “Select Countries”. They can select multiple countries in the bar for comparison.  

- Typical Country Data:  


&emsp; In this page, users can get the map showing the total cases and total death of a country by selecting one among China, US and Canada. They can choose to see the total cases or total deaths. They can also see the statistical data individual number of a province/state interactive with the map.  

- Breaking News:  


&emsp; We include latest news with title, resource(e.g. CNN), post time(local time) and description in the next page. The news is also updated real-time with the News API. The description part gives an abstract of each news. Users can find out the full article of the news they are interested by clicking the title. News of four countries including US, UK, Canada and Italy are available now.

- More Information:  


&emsp; For the last page, there are three sub-pages. The first one is [General Q&A], provided by the CDC. The second one, [US Testing], is daily testing result showing both in map and histogram for each state across the US. The last part is [About]. We include our motivation, github and contact information for future reference.
***

### Source
- Worldometers: https://www.worldometers.info/coronavirus/#countries
- Novel Coronavirus (COVID-19): https://github.com/CSSEGISandData/COVID-19
- World and China Map Data: https://github.com/deldersveld/topojson
- Australia and Canada Map Data: https://exploratory.io/map
- News API: https://newsapi.org/s/us-news-api
- CDC Frequently Q&A: https://www.cdc.gov/coronavirus/2019-ncov/faq.html#basics
***

### Contact us
If you want to report any issue, please contact us: covidapphelp@gmail.com.
