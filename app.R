#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#Movie Analysis

# required libraries 
library(shiny)
library(shinyjs)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(readr)
library(reshape)
library(ggplot2)


# read the csv file
mydata = read.csv("clean_final_dataset.csv",header = TRUE)


# unique state names
countries = unique(as.character(mydata$country)) %>% sort
countries = c("ALL",countries)

independent_varaibles = c("Language","UserFeedback","FBLikes","Duration","GrossAmount","Geners")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("MOVIES Analysis"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectInput("country", "Select Country:", choices = countries,multiple = FALSE),
        selectInput("criteria", "Select feature:", choices = sort(independent_varaibles),multiple = FALSE),
        uiOutput("radiosO")
        
     
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("Plot_1", plotOutput("plot1"),plotOutput("plot2")),
                    tabPanel("Plot_2", plotOutput("plot3"))
                    
        )
        
      )
   )
)



    
# Define server logic required to draw a histogram
server <- function(input, output,session) {
   
  # Drop down  for counties
  output$radiosO<- renderUI({
    
    # If missing input, return to avoid error later in function
    if(input$criteria == "UserFeedback")
      # create the radio button
      radioButtons("radiosI", "User Feedback:", c("Votes" = "votes","Reviews" = "reviews"),inline = TRUE)
    
  })
  
    
   output$plot1 <- renderPlot({
     
     
     # create a varaible data, to subset the country data
     if(input$country == "ALL")
       data = mydata
     else
       data = mydata[mydata$country == input$country,]
     
     # read the criteria 
     criterias = input$criteria
     
     
     # function for language
     language <- function(x)
     {
       print("in language")
       # read only language data
       datal = data[,c(8,10)]
      
       
      # conver the moving rating to categorical
      datal$imdb_score = cut(datal$imdb_score,breaks = c(seq(1,10)))
      datal$imdb_score =  mapvalues(datal$imdb_score,from = c("(1,2]","(2,3]","(3,4]","(4,5]","(5,6]","(6,7]","(7,8]","(8,9]","(9,10]"),to = 
                                       c(seq(1:9)))
      datal$imdb_score = as.factor(datal$imdb_score)
       
       
       # plot for differnt languages
       ggplot(datal,aes(x = language,col = imdb_score))+geom_bar(aes(fill = imdb_score),position = "dodge")+
         theme(axis.text.x = element_text(angle = 90,hjust = 1),legend.position = "bottom")+
         labs(title = paste("Movie rating w.r.t languanges in ",input$country))
       
     }
     
     # function for user feedback
     userfeedback <- function(x)
     {
       print("in criticreviews")
       
       votes <- function(x1)
       {
         # select the uservote data
         vote = data[,c(6,10)]
         vote$num_voted_users = cut(vote$num_voted_users,c(1,250000,500000,750000,max(vote$num_voted_users)),labels = 1:4)
         vote$num_voted_users = as.factor(vote$num_voted_users)
      
         
         # users voted
         ggplot(vote,aes(imdb_score))+geom_histogram(aes(fill = num_voted_users),binwidth = 1)+
           labs(title = paste("Movie Rating w.r.t user voting feedback - ",input$country),x = "Movie Rating")+
           scale_fill_discrete(name = "No of users voted",breaks = c("1","2","3","4"),labels = c("1-250k","251k-500k","501k-750k","751k+"))+
           theme(legend.position = "bottom")
         
       }
        
       reviews <- function(x1)
       {
         
         # select the uservote data
         review = data[,c(7,10)]
         review$num_user_for_reviews = cut(review$num_user_for_reviews,c(1,1250,2500,3750,max(review$num_user_for_reviews)),labels = 1:4)
         review$num_user_for_reviews = as.factor(review$num_user_for_reviews)
         
         
         # User Reviewed
         ggplot(review,aes(imdb_score))+geom_histogram(aes(fill = num_user_for_reviews),binwidth = 1)+
           labs(title = paste("Movie Rating w.r.t User review feedback - ",input$country),x = "Movie Rating")+
           scale_fill_discrete(name = "No of User Reviews",breaks = c("1","2","3","4"),labels = c("1-1250 ","1251-2500","2501-3750","3751+"))+
          theme(legend.position = "bottom")  
       }
       
       if(is.null(input$radiosI ))
         return()
       else
         switch(input$radiosI,votes = votes(x1),reviews = reviews(x1))
     }
     
     # function for facebook likes
     facebooklikes <- function(x)
     {
       print("in facebooklikes")
       
       
         # facebook movie likes data
         fm = data[,c(11,10)]
         fm$movie_facebook_likes = cut(fm$movie_facebook_likes,c(-1,1,50000,100000,400000),labels = 0:3)
         fm$movie_facebook_likes = as.factor(fm$movie_facebook_likes)
         
         #plot for facebook likes for movies
         ggplot(fm,aes(imdb_score))+geom_histogram(aes(fill = movie_facebook_likes),binwidth = 1)+
           labs(title = paste("Movie Rating w.r.t Facbook likes of movie - ",input$country),x = "Movie Rating")+
           facet_wrap(~movie_facebook_likes,ncol =2,scales = "fixed")+
           scale_fill_discrete(name = "No of movie Facebook Likes",breaks = c("0","1","2","3"),labels = c("NO LIKES","1-50k","51k-100k","101k+"))+
           theme(legend.position = "bottom")
         
     }
     
     # function for duration
     duration <- function(x)
     {
       print("in duration")
       # duration data
       dur = data[,c(3,9,10)]
       dur$duration = cut(dur$duration,c(1,60,120,180,240),labels = 1:4)
       dur$duration = as.factor(dur$duration)
       
       # plot of duration
       ggplot(dur,aes(imdb_score))+geom_histogram(aes(fill = duration),binwidth = 1)+
         labs(title = paste("Movie success on duration of the movie - ",input$country),x = "Movie Rating")+
         scale_fill_discrete(name = "Movie Duration",breaks = c("1","2","3","4"),labels = c("< 1hour ","1-2hours","2-3hours","> 3 hours"))+
         theme(legend.position = "bottom") 
       
       
      # ggplot(dur,aes(x = duration, y = imdb_score,col = country))+geom_point()+#geom_smooth(method = "lm")+
       #  labs(title = paste("Movie success on duration of the movie - ",input$country),x = "Duration(Mins)", y ="Rating")+
        # scale_color_brewer(palette = "Paired")+
        # theme(legend.position = "null")
       
       
     }
     
     # function for grossamount
     grossamount <- function(x)
     {
       print("in grossamount")
       # Movie Budget data
       gr = data[,c(4,9,10)]
       #str(gr)
       
       # plot the distribution of movie budget on movie ratings
       ggplot(gr,aes(x = gross,y = imdb_score))+geom_point()+
         labs(title = paste("Movie success on the movie budget - ",input$country),x = "Movie Budget",y = "Rating")+geom_smooth(method = "lm")
       
     }
     
     # geners function
     geners <- function(x)
     {
       print("in Geners")
       # geners data
       gen = data[,c(12:30)]
       # add up all geners
       col_total = data.frame(colSums(gen)) 
       col_total$Geners = rownames(col_total)
       rownames(col_total) = NULL
       colnames(col_total) = c("Count","Geners")
       
       # plot the geners
       ggplot(col_total,aes(x= Geners,y = Count,fill = Geners))+geom_bar(stat = "identity",color = "black")+
         labs(title = paste("Geners - ",input$country))+
         geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5)+scale_fill_grey()+
         #scale_fill_brewer(palette="Blues")+
         theme(axis.text.x = element_text(angle = 90,hjust = 1),legend.position = "NULL")
       
       
     }
     
     # switch statement to select the criteria type i.e age,gender or profession function 
     switch(criterias,Language = language(data),UserFeedback = userfeedback(data),FBLikes = facebooklikes(data),
            Duration = duration(data),GrossAmount = grossamount(data),Geners = geners(data))
   })# end of render plot1
   
     #output$plot2 <- renderPlot({
         
       
     #}) # end of render plot2
     
     output$plot3 <- renderPlot({
       
       if(input$country != "ALL")
       {  
          
         # function for language
         language <- function(x)
         {
           # read only language data
           datal = mydata[,c(8,10)]
      
           
           # conver the moving rating to categorical
           datal$imdb_score = cut(datal$imdb_score,breaks = c(seq(1,10)))
           datal$imdb_score =  mapvalues(datal$imdb_score,from = c("(1,2]","(2,3]","(3,4]","(4,5]","(5,6]","(6,7]","(7,8]","(8,9]","(9,10]"),to = 
                                           c(seq(1:9)))
           datal$imdb_score = as.factor(datal$imdb_score)
           
           
           # plot for differnt languages
           ggplot(datal,aes(x = language,col = imdb_score))+geom_bar(aes(fill = imdb_score),position = "dodge")+
             theme(axis.text.x = element_text(angle = 90,hjust = 1),legend.position = "bottom")+
             labs(title = "Movie rating w.r.t languanges across countries")
          }
         
         # function for userfeedback reviews
         userfeedback <- function(x)
         {
           print("in criticreviews")
           
           votes <- function(x1)
           {
             # select the uservote data
             vote = mydata[,c(6,10)]
             vote$num_voted_users = cut(vote$num_voted_users,c(1,250000,500000,750000,max(vote$num_voted_users)),labels = 1:4)
             vote$num_voted_users = as.factor(vote$num_voted_users)
             
             
             # users voted
             ggplot(vote,aes(imdb_score))+geom_histogram(aes(fill = num_voted_users),binwidth = 1)+
               labs(title = "Movie Rating w.r.t user voting feedback across countries",x = "Movie Rating")+
               scale_fill_discrete(name = "No of users voted",breaks = c("1","2","3","4"),labels = c("1-250k","251k-500k","501k-750k","751k+"))+
               theme(legend.position = "bottom")
             
           }
           
           reviews <- function(x1)
           {
             
             # select the uservote data
             review = mydata[,c(7,10)]
             review$num_user_for_reviews = cut(review$num_user_for_reviews,c(1,1250,2500,3750,max(review$num_user_for_reviews)),labels = 1:4)
             review$num_user_for_reviews = as.factor(review$num_user_for_reviews)
             
             
             # User Reviewed
             ggplot(review,aes(imdb_score))+geom_histogram(aes(fill = num_user_for_reviews),binwidth = 1)+
               labs(title = "Movie Rating w.r.t User review feedback across countries",x = "Movie Rating")+
               scale_fill_discrete(name = "No of User Reviews",breaks = c("1","2","3","4"),labels = c("1-1250 ","1251-2500","2501-3750","3751+"))+
               theme(legend.position = "bottom")  
           }
           
           if(is.null(input$radiosI ))
             return()
           else
             switch(input$radiosI,votes = votes(x1),reviews = reviews(x1))
         }
         
         # function for facebook likes
         facebooklikes <- function(x)
         {
           print("in facebooklikes")
         
           # facebook movie likes data
           fm = mydata[,c(11,10)]
           fm$movie_facebook_likes = cut(mydata$movie_facebook_likes,c(-1,1,50000,100000,max(mydata$movie_facebook_likes)),labels = 0:3)
           fm$movie_facebook_likes = as.factor(fm$movie_facebook_likes)
           
           #plot for facebook likes for movies
           ggplot(fm,aes(imdb_score))+geom_histogram(aes(fill = movie_facebook_likes),binwidth = 1)+
             labs(title = "Movie Rating w.r.t Facbook likes of movie across countries",x = "Movie Rating")+
             facet_wrap(~movie_facebook_likes,ncol =2,scales = "fixed")+
             scale_fill_discrete(name = "No of movie Facebook Likes",breaks = c("0","1","2","3"),labels = c("NO LIKES","1-50k","51k-100k","101k+"))+
             theme(legend.position = "bottom")
           
           
         }
         
         # function for duration
         duration <- function(x)
         {  
           # duration data
           dur = mydata[,c(3,9,10)]
           dur$duration = cut(dur$duration,c(1,60,120,180,240),labels = 1:4)
           dur$duration = as.factor(dur$duration)
           
           # plot of duration
           ggplot(dur,aes(imdb_score))+geom_histogram(aes(fill = duration),binwidth = 1)+
             labs(title = "Movie success on duration of the movie across countries",x = "Movie Rating")+
             scale_fill_discrete(name = "Movie Duration",breaks = c("1","2","3","4"),labels = c("< 1hour ","1-2hours","2-3hours","> 3 hours"))+
             theme(legend.position = "bottom") 
           
           
          
         }
         
         # function for grossamount
         grossamount <- function(x)
         {
           print("in grossamount")
            
           # Movie Budget data
           gr = mydata[,c(4,9,10)]
           #str(gr)
           
           # plot the distribution of movie budget on movie ratings
           ggplot(gr,aes(x = gross,y = imdb_score))+geom_point()+
             labs(title = "Movie success on the movie budget across countries",x = "Movie Budget",y = "Rating")+geom_smooth(method = "lm")
           
         }
         # geners function
         geners <- function(x)
         {
           print("in Geners")
           # geners data
           gen = mydata[,c(12:30)]
           # add up all geners
           col_total = data.frame(colSums(gen)) 
           col_total$Geners = rownames(col_total)
           rownames(col_total) = NULL
           colnames(col_total) = c("Count","Geners")
           
           # plot the geners
           ggplot(col_total,aes(x= Geners,y = Count,fill = Geners))+geom_bar(stat = "identity",color = "black")+
             labs(title = paste("Geners Distribution"))+
             geom_text(aes(label=Count), vjust=1.6, color="white", size=3.5)+scale_fill_grey()+
             #scale_fill_brewer(palette="Blues")+
             theme(axis.text.x = element_text(angle = 90,hjust = 1),legend.position = "NULL")
           
           
         }
         
         # switch statement to select the criteria type i.e age,gender or profession function 
         switch(input$criteria,Language = language(data),UserFeedback = userfeedback(data),FBLikes = facebooklikes(data),
                Duration = duration(data),GrossAmount = grossamount(data),Geners = geners(data)) 
       } # end of if
       
   
     }) # end of render plot3
   
} # endof server function

# Run the application 
shinyApp(ui = ui, server = server)

