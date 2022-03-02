
install.package
install.packages("maptools")
install.packages("rworldmap")
install.packages("treemap")
install.packages("wordcloud")
install.packages("SnowballC")

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(dplyr) # data manituplation
library(e1071) # svm model
library(DT)
library(maptools)
library(rworldmap)
library(treemap)
library(wordcloud)
library(SnowballC)

cacao <- read_csv("C:/Users/USHA/Downloads/flavors_of_cacao.csv")
datatable(cacao)

# Group by country
colnames(cacao) <- c("company", "bean.bar.orgin", "ref","date" , "percent", "location","rating", "beantype" ,"origin")

commap <- group_by(cacao, location)
commap1 <- summarise(commap,  count=n())
map1 <- joinCountryData2Map(commap1, joinCode="NAME", nameJoinColumn="location")

mapCountryData(map1, nameColumnToPlot="count", mapTitle="Chocolate Company Distribution" , colourPalette = "negpos8")

#avarage rating by location
loca <- group_by(cacao, location)
good <- summarise(loca,  count=n(),
                  rate1= mean(rating))
good1<- arrange(good, desc(rate1))

#scattor plot
ggplot(good1,aes(x=reorder(location,rate1), y=rate1)) +geom_point(aes(size=count, colour=factor(rate1)), alpha=1/2) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1) , legend.position="none") +
  labs(x="Country", "Chocolate Rating", "Chocolate Rating vs Country")

#company count
keys1 <- group_by(cacao, location,company )
keys2 <- dplyr::summarise(keys1,  count=n())
keys2 <- arrange(keys2, desc(count))
keys3  <-filter(keys2, count >8)
# company treemap
treemap(keys3, index=c("location","company"), vSize="count", type="index", 
        palette="Accent", title="Top Chocolate Company", fontsize.title=6)