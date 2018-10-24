library(plyr)
library(arules)
library(reshape2)
library(stringi)
library(dplyr)
library(arulesViz)
library(shiny)
library(grid)

df_groceries <- read.csv("groceries.csv")
View(df_groceries)

df_sorted <- df_groceries[order(df_groceries$Member_number),]
df_sorted$Member_number <- as.numeric(df_sorted$Member_number)
class(df_sorted$Member_number)
df_sorted$itemDescription <- as.factor(df_sorted$itemDescription)
str(df_sorted$itemDescription)

if(sessionInfo()['basePkgs']=="dplyr" | sessionInfo()['otherPkgs']=="dplyr"){
  detach(package:dplyr, unload=TRUE)
}

df_itemList <- ddply(df_groceries, c("Member_number","Date"), function(df1)paste(df1$itemDescription,collapse = ","))
df_itemList$Member_number <- NULL
df_itemList$Date <- NULL
colnames(df_itemList) <- c("itemList")
write.csv(df_itemList,"ItemList.csv", quote = FALSE, row.names = TRUE)

txn = read.transactions(file="ItemList.csv", rm.duplicates= FALSE, format="basket",sep=",",cols = 1);
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)
basket_rules <- apriori(txn,parameter = list(minlen=2,sup = 0.001, conf = 0.01, target="rules"))

if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:sentiment, unload=TRUE)
  detach(package:tm, unload=TRUE)
}
inspect(basket_rules) #1186 rules get created

df_basket <- as(basket_rules,"data.frame")
df_basket$confidence <- df_basket$confidence * 100
df_basket <- transform(df_basket, rules = colsplit(rules, pattern = "=>", names = c("lhs","rhs")))

# Remove curly brackets around rules
df_basket$rules$lhs <- gsub("[[:punct:]]", "", df_basket$rules$lhs)
df_basket$rules$rhs <- gsub("[[:punct:]]", "", df_basket$rules$rhs)
df_basket$rules$lhs <- as.character(df_basket$rules$lhs)
df_basket$rules$rhs <- as.character(df_basket$rules$rhs)

df_basket$rules %>%
  filter(stri_detect_fixed(lhs, "yogurt")) %>%
  select(rhs)

plot(basket_rules,interactive = TRUE)
itemFrequencyPlot(txn, topN = 5)

ui <- fluidPage(
        titlePanel("MarketBasketAnalysis"),
  
        sidebarLayout(position = "left",
                sidebarPanel("sidebar panel",
                             checkboxInput("do1", "Scatter plot", value = T),
                             checkboxInput("do2", "Grouped Plot", value = F),
                             checkboxInput("do3", "Graph Plot", value = F),
                             checkboxInput("do4", "Matrix Plot", value = F)
                ),
                mainPanel("main panel",
                          fluidRow(
                            splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph1"), plotOutput("plotgraph2"))
      ),
      fluidRow(
        splitLayout(cellWidths = c("50%", "50%"), plotOutput("plotgraph3"), plotOutput("plotgraph4"))
      )
    )
  )
)
server<-function(input, output) 
{
  set.seed(8000)
  pt1 <- reactive({
    input$do1
    if (input$do1){
      return(plot(basket_rules[1:10],title="plotgraph1"))
    } else {
      return(NULL)
    }
  })
  pt2 <- reactive({
    input$do2
    if (input$do2){
      return(plot(basket_rules, method = "grouped", control = list(k = 5),title="plotgraph2"))
    } else {
      return(NULL)
    }
  })
  pt3 <- reactive({
    input$do3
    if (input$do3){
      return(plot(basket_rules[1:10,], method="graph", control=list(type="items"),title="plotgraph3"))
    } else {
      return(NULL)
    }
  })
  pt4 <- reactive({
    input$do4
    if (input$do4){
      return(plot(basket_rules[1:10,],method="matrix"))
    } else {
      return(NULL)
    }
  })
  output$plotgraph1 = renderPlot({pt1()})
  output$plotgraph2 = renderPlot({pt2()})
  
  output$plotgraph3 = renderPlot({pt3()})
  output$plotgraph4 = renderPlot({pt4()})
}
shinyApp(ui, server)