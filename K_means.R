library(ggplot2)
library(arules)
library(arulesViz)
library(plyr)
library(factoextra)
library(cluster)
library(shiny)

data_raw <- read.csv(file="GroceriesInitial.csv",header=TRUE,sep=",")

product_names <- levels(unlist(data_raw[,4:35])) # Identify all unique products

# Identify the products asked inside the product_names list
citrus <- which(product_names == "citrus fruit")
tropical <- which(product_names == "tropical fruit")  
milk <- which(product_names == "whole milk") 
other_vegetables <- which(product_names == "other vegetables") 
rolls_buns <- which(product_names == "rolls/buns")  
chocolate <- which(product_names == "chocolate")  
water <- which(product_names == "bottled water") 
yogurt <- which(product_names == "yogurt")  
sausage <- which(product_names == "sausage") 
root_vegetables <- which(product_names == "root vegetables")  
pastry <- which(product_names == "pastry")  
soda <- which(product_names == "soda") 
cream <- which(product_names == "cream") 

# Define asked products vector
product_vector <- c(citrus,tropical, milk, other_vegetables, rolls_buns, chocolate, water, yogurt, sausage, root_vegetables, pastry, soda, cream)
product_names <- product_names[product_vector]  #Delete other variable 

# Create the binary table without headers or extra information
products <- as.data.frame(t(apply(data_raw[,4:35],1,  function(x)(product_names) %in% as.character(unlist(x))))) 
class(products)
# %in% returns a vector as long as product_names (left) with TRUE or FALSE 
# depending on the existance of the product in the specified transaction linnames(products) <- product_names
# Create the initial binary table without the qualitative basket values
data_binary <- cbind(data_raw[,1:3],products) # Join the products table with the extra transaction information
# Discretizing the continouous variables 
data_discrete <- data_binary
cut_points <- quantile(data_discrete$basket_value, probs = c(0,0.33, 0.66,1), na.rm = TRUE,names = FALSE)

data_discrete$basket_value_bin <- cut(data_discrete$basket_value,breaks = cut_points,labels=c("Low","Medium","High"),include.lowest = TRUE)
table(data_discrete$basket_value_bin)
#Below, an inline function to make the binary format again, now that we have discretized any continuous variables
binarize <-function(data_columns,extra_columns=NULL){
  
  column_names <- levels(unlist(data_columns))
  cat(column_names)
  blank <- which(column_names == "") # Remove blank columns
  if (length(blank) !=0)
    column_names <- column_names[-c(blank)]
  
  binary_result <- as.data.frame(t(apply(data_columns,1,  function(x) column_names %in% as.character(unlist(x)))))
  names(binary_result) <- column_names
  if (is.null(extra_columns)==FALSE)  #Not Necessary
    binary_result<- cbind(extra_columns,binary_result)
  return(binary_result)
}

# Converting basket_value_bin to binary format instead of qualitative char data (low, medium, high)
data_discrete <- binarize(as.data.frame(data_discrete$basket_value_bin),data_discrete) 
data_discrete <- data_discrete[,-c(which(colnames(data_discrete)=="basket_value_bin"))] # Remove the non-binary column
data_kmeans <- data_discrete[,2:3]
#K_Means Clustering Algorithm 
data_kmeans$Cluster <- kmeans(scale(data_kmeans), 5)$cluster
means <- ddply(data_kmeans, c("Cluster"), colwise(mean))

data_discrete$Cluster <- as.character(data_kmeans$Cluster)
table(data_kmeans$Cluster)

#Cluster Analysis
data_discrete <- binarize(as.data.frame(data_discrete$Cluster),data_discrete) 
data_discrete <- data_discrete[,-c(which(colnames(data_discrete)=="Cluster"))]
df <- data_discrete$basket_value
k2 <- kmeans(df, centers = 5, nstart = 25)
p4 <- fviz_cluster(k2,data = data_kmeans, geom = "point", repel = TRUE,ellipse.type = "norm") + ggtitle("k = 5")
View(means)
plot(p4)

standard_deviation <- ddply(data_kmeans, c("Cluster"), colwise(sd)) # Get standard deviation
plot(standard_deviation)


ui <- fluidPage(
  titlePanel("MarketBasketAnalysis"),
  
  sidebarLayout(position = "left",
                sidebarPanel("Options for Analysis",
                             checkboxInput("do1", "Plot", value = T),
                             checkboxInput("do2", "Boxplot", value = F),
                             checkboxInput("do3", "BasketIndex", value = F),
                             checkboxInput("do4", "SmoothScatter", value = F)
                ),
                mainPanel("Cluster Analysis",
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
      return(plot(means[,2:3]))
    } else {
      return(NULL)
    }
  })
  pt2 <- reactive({
    input$do2
    if (input$do2){
      return(boxplot(means[,2:3]))
    } else {
      return(NULL)
    }
  })
  pt3 <- reactive({
    input$do3
    if (input$do3){
      return(plot.default(means$basket_value))
    } else {
      return(NULL)
    }
  })
  pt4 <- reactive({
    input$do4
    if (input$do4){
      return(smoothScatter(means[,2:3]))
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
