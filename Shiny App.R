# Downloading libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
# install.packages ('shinydashboard')
# library(rsconnect)
# library(datasets)

### SECTION #1: Loading databases and tables ###

# Loading Datamart
# data_mart = load("db.RData")

# Loading tables created from the data mart
# Note: The creation of these tables can be find in the db_tables file

t0 = load(file = 'user_data.Rdata')
t0 = data.frame(user_data)

t1 = load(file = 'age_AVG_LA.RData')
t1 = data.frame(age_AVG_LA)
t1.1 = data.frame(age_AVG_LA)

t2 = load(file = 'age_AVG_FO.RData')
t2 = data.frame(age_AVG_FO)

t3 = load(file = 'country_LAProfit_ranks.RData')
t3 = data.frame(country_LAProfit_ranks)
t3 = t3[order(t3$Overall_Profit, decreasing = TRUE),]

t4 = load(file = 'country_FOProfit_ranks.RData')
t4 = data.frame(country_FOProfit_ranks)

t5 = load(file = 'country_Profit_ranks.RData')
t5 = data.frame(country_Profit_ranks)

t6 = load(file = 'country_Profit.RData')
t6 = data.frame(country_Profit)

t7 = load(file = 'age_Profit.RData')
t7 = data.frame(age_Profit)
t7$age_cat = ifelse(t7$AGE < 18,"Underage",ifelse(18<=t7$AGE & t7$AGE<=25,"18-25",ifelse(26<=t7$AGE & t7$AGE<=35,"26-35",ifelse(36<=t7$AGE & t7$AGE<=45,"36-45",">45"))))
t7$age_cat = as.factor(t7$age_cat)
f7 = factor(t7$age_cat, levels = c("Underage","18-25","26-35","36-45",">45"))
t7$age_cat = f7

t8 = load(file = 'age_LA_stakes.RData')
t8 = data.frame(age_LA_stakes)

t9 = load(file = 'age_FO_stakes.RData')
t9 = data.frame(age_FO_stakes)

t10 = load(file = 'CountryRanksLA.RData')
t10 = data.frame(CountryRanksLA)

t11 = load(file = 'CountryRanksFO.RData')
t11 = data.frame(CountryRanksFO)

t12 = load(file = 'ClientAmount.Rdata')
t12 = data.frame(ClientAmount)
t12 = t12[t12$Clients_Amount > 100,]

t13 = load(file = 'active_FO.Rdata')
t13 = data.frame(active_FO)

t15 = load(file = 'active_LA_FO.Rdata')
t15 = data.frame(active_LA_FO)

t16 = load(file = 'stakes_LA_FO.Rdata')
t16 = data.frame(stakes_LA_FO)

t17 = load(file = 'clients_Regdate.Rdata')
t17 = data.frame(clients_Regdate)

t18 = load(file = 'lastActive.Rdata')
t18 = data.frame(lastActive)

t19 = load(file = 'client_per_country.Rdata')
t19 = data.frame(client_per_country)

t20 = load(file = 'client_per_gender.Rdata')
t20 = data.frame(client_per_gender)

t21 = load(file = 'top_players_ts.Rdata')
t21 = data.frame(top_players_ts)

t22 = load(file = 'client_per_language.Rdata')
t22 = data.frame(client_per_language)

### SECTION #2: Creating dashboard on Shiny ###

# Uploading to the server

rsconnect::setAccountInfo(name='jorgepolanco2',
                          token='4FE65EAAFD5A02FE85311F819539B849',
                          secret='IYNOPan/4Ubm8lsSescuROrMX/nY/5vt24Ghyu2S')

# Creating a title for the dashboard 
header <- dashboardHeader(title = "Dashboard")

# Defining the sidebar/slides where we want to put our charts
# Here we are defining some subsections with their respective dashboards
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = "Highlights",tabName = "d0",icon = icon("th")),
        
        menuItem(text = "User Searcher",tabName = "d13",icon = icon("dashboard")),
        
        menuItem('1. Clients Report',icon = icon("bar-chart-o"),
                 menuSubItem(text = "a) Registration of Clients",tabName = "d9", icon = icon("document")),
                 menuSubItem(text = "b) Last Month Activated Clients",tabName = "d10",icon = icon("document")),
                 menuSubItem(text = "c) Clients per Country",tabName = "d6",icon = icon("document")),
                 menuSubItem(text = "d) Number of Bets per Country",tabName = "d7",icon = icon("document")),
                 menuSubItem(text = "e) Amount of Bets per Country",tabName = "d8",icon = icon("document"))),
        
        menuItem('2. Demographics Report',icon = icon("bar-chart-o"),
                 menuSubItem(text = "a) % of Clients per Gender",tabName = "d3",icon = icon("document")),
                 menuSubItem(text = "b) The most common languages",tabName = "d5",icon = icon("document"))),
        
        menuItem('3. Profitability Report',icon = icon("bar-chart-o"),
                 menuSubItem(text = "a) Avg Profitability per Country",tabName = "d1",icon = icon("document")),
                 menuSubItem(text = "b) Avg Profitability per Age Group",tabName = "d2",icon = icon("document")),
                 menuSubItem(text = "c) Top 20 of Total Stakes",tabName = "d4",icon = icon("document")))))

# Once created the tabs, here we are selecting the elements that will be withing each tab
# Each section will have a title, text box and a graphic
body <- dashboardBody(
    tabItems(
        tabItem(tabName = "d0",
                # Slide of Highligths
                fluidRow(
                    box(width = 12,title = "Key Takeaways","In the following charts, you will find a basic description of our datamart visualizing the main metrics."),
                    column(width = 6,infoBox(width = 22,title = "Insight #1",subtitle = "9% of players present a betting performance above mean.",color = 'yellow')),
                    column(width = 6,infoBox(width = 22,title = "Insight #2",subtitle = "There are a lot of players with a high recency.",color = 'red')),
                    column(width = 6,infoBox(width = 22,title = "Insight #3",subtitle = "92% of the client base is male. There is an important opportunity to reach a 'new' segment.",color = 'yellow')),
                    column(width = 6,infoBox(width = 22,title = "Insight #4",subtitle = "62% of our clients speak german, but there are other important languages we should consider.",color = 'green')),
                    # valueBox(width = 12, value = "40,498",subtitle = "Unique Users", icon = icon("lightbulb-o")),
                    # valueBox(width = 12, value = 80,subtitle = "Countries/Islands", icon = icon("lightbulb-o")),
                    # valueBox(width = 12, value = "58%",subtitle = "of the clients come from Germany", icon = icon("lightbulb-o")),
                )),
        
        tabItem(tabName = "d1", h1("Avg Profitability per Country"),
                fluidRow(
                    box(width = 12,title = "Analysis","Some countries outstand over the rest; however, most of them present these numbers due to the small client base that they have. Basically, the house never loses."),
                    box(width = 12,heigth = 8,  plotOutput("plot1")))),
        
        tabItem(tabName = "d2", h1("Avg Profitability per Age Group"),
                fluidRow(
                    box(width = 12,title = "Analysis","Only >45 Segment presents an important difference among its percentils."),
                    box(width = 12,heigth = 8,  plotOutput("plot2")))),
        
        
        tabItem(tabName = "d3", h1("% of Clients per Gender"),
                fluidRow(
                    box(width = 12,title = "Analysis","There is an important opportunity to reach to the female segment, which for some reason currently is under-represented in our client base."),
                    box(width = 12,heigth = 8,  plotOutput("plot12")))),
        
        tabItem(tabName = "d4", h1("Best Players - Total Stakes"),
                fluidRow(
                    box(width = 12,title = "Analysis","We should keep closer our most important players, those which are overperforming vs the mean."),
                    box(width = 12,heigth = 8,  plotOutput("plot13")))),
        
        tabItem(tabName = "d5", h1("The most common languages"),
                fluidRow(
                    box(width = 12,title = "Analysis","The following chart helps us to understand which languages we should be able to have in our platforms or even around client attention."),
                    box(width = 12,heigth = 8,  plotOutput("plot14")))),
        
        tabItem(tabName = "d6", h1("Clients Amount per Country"),
                fluidRow(
                    box(width = 12,title = "Analysis","We have selected countries with more than a hundred customers. 
                            In this chart on the distribution of customer numbers, 
                            we can find that German customers account for the vast majority. 
                            In addition, compared to other countries, 
                            Turkey, Poland, Greece, France also has a large number of customers."),
                    box(width = 12,heigth = 8,  plotOutput("plot11")))),
        
        
        tabItem(tabName = "d7", h1("Clients Amount of Different Bets Counts"),
                fluidRow(
                    box(width = 12,title = "Analysis","In this chart, we divide the data into 
                                                   10 groups according to the number of gamblings 
                                                   of fixed odds and the number of gambling of live actions. 
                                                   As can be seen from the figure, the most people gambling more 
                                                   than 100 times and less than 10 times. In addition, there are 
                                                   far more fixed odds than live actions."),
                    box(width = 12,heigth = 8,  plotOutput("plot7")))),
        
        
        tabItem(tabName = "d8", h1("Clients Amount of Different Stakes Amount"),
                fluidRow(
                    box(width = 12,title = "Analysis","In this chart, we divided the data into 10 groups 
                                            according to the gambling amount of fixed odds 
                                            and the gambling amount of live actions. 
                                            We found that the largest proportion of the gambling amount 
                                            was less than 50 euros, while the largest proportion was 
                                            more than 500 euros."),
                    box(width = 12,heigth = 8,  plotOutput("plot8")))),
        
        
        tabItem(tabName = "d9", h1("Clients Amount of Different Registration Date"),
                fluidRow(
                    box(width = 12,title = "Analysis","In this chart, we counted the number of people 
                                                signing up on different dates to see the growth trend.
                                                We found that the overall trend was up, but it fluctuated 
                                                a lot, especially in late February."),
                    box(width = 12,heigth = 8,  plotOutput("plot9")))),
        
        
        tabItem(tabName = "d10", h1("Clients Amount of Last Active Month"),
                fluidRow(
                    box(width = 12,title = "Analysis","In this chart, we counted the number of users 
                                                in each month according to the last active date of users 
                                                in fixed odds and live actions. We found that the number 
                                                of active users decreased every month, but there were still 
                                                many active users in the 
                                                last month of the statistical period."),
                    box(width = 12,heigth = 8,  plotOutput("plot10")))),
        
        tabItem(tabName = "d12", h1("adzda"),
                fluidRow(
                    box(width = 12,title = "Analysis","adazdaz"),
                    box(width = 12,heigth = 8,  plotOutput("plot15")))),
        
        tabItem(tabName = "d11", h1("adadz"),
                fluidRow(
                    box(width = 12,title = "Analysis","dadzd"),
                    box(width = 12,heigth = 8,  plotOutput("plot16")))),
        
        tabItem(tabName = "d13", h1("Searcher of TOP 4000 Users"),
                fluidRow(
                    box(width = 12, textInput("text","Searcher of TOP 4000 Users: ")),
                    box(width = 12, dataTableOutput("table"))))
        
    ))

# The dashboard style will be define here
ui <- fluidPage(
    titlePanel("Marketing Performance Report"),
    dashboardPage(skin = "blue", header,sidebar,body))

# We are creating the graphs that will be in within the tabs 
# In order to format my graphs, we will use tidyr
server <- function(input, output) {
    #output$plot1 <- renderPlot({plot(t3$Countryname, main = input$title, cex = input$size)})
    #output$table1 <- renderTable({t3})
    # output$plot1 <- renderPlot({barplot(t3$Overall_Profit)})
    output$plot1 <- renderPlot({
        ggplot(t3, aes(x= reorder(Countryname, Overall_Profit),y= Overall_Profit))  +
            stat_summary(fun.y = sum, geom = "bar",colour="black",fill="black")+
            geom_bar(stat="identity") +
            labs(title="Avg Overall Profit per Country", y ="Avg Profit", x ="Country/Island" ) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_text(hjust=-0.55, size = 3, label = round(t3$Overall_Profit))+
            coord_flip() })
    
    output$plot2 <- renderPlot({
        ggplot(t7, aes(x= age_cat,y= Overall_Profitability,fill=age_cat))  +
            geom_boxplot() +
            labs(title="Avg Profitability per Age Group", y ="Profitability", x ="Age Group" ) +
            theme(legend.position="none")})
    
    
    output$plot6 <- renderPlot({
        ggplot(t12, aes(x= reorder(Countryname, Clients_Amount),y= Clients_Amount))  +
            stat_summary(fun.y = sum, geom = "bar",colour="steelblue",fill="steelblue")+
            labs(title="Clients Amount per Country", y ="Clients Amount", x ="Country/Island" ) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_text(hjust=-0.55, size = 3, label = round(t12$Clients_Amount))+
            coord_flip() })
    
    
    
    output$plot7 <- renderPlot({
        ggplot(t15, aes(x= reorder(active, value),y= value,fill=variable))  +
            stat_summary(fun.y = sum, geom = "bar",position=position_dodge())+
            labs(title="Clients Amount of Different Bets Counts", y ="Clients Amount", x ="Bet Counts Level" ) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_text(hjust=-0.55, size = 3, label = round(t15$value))+
            scale_fill_brewer(palette="Paired")+
            coord_flip()})
    
    
    
    output$plot8 <- renderPlot({
        ggplot(t16, aes(x= reorder(stakes, value),y= value,fill=variable))  +
            stat_summary(fun.y = sum, geom = "bar",position=position_dodge())+
            labs(title="Clients Amount of Different Stakes Amount", y ="Clients Amount", x ="Stakes Level" ) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_text(hjust=-0.55, size = 3, label = round(t16$value))+
            scale_fill_brewer(palette="Paired")+
            coord_flip()})
    
    
    output$plot9 <- renderPlot({
        ggplot(data=t17, aes(x=RegistrationDate, y=Clients_Amount)) +
            geom_line(color="steelblue")+
            geom_point()+
            labs(title="Clients Amount of Different Registration Date", y ="Clients Amount", x ="Registration Date" )+
            geom_text(hjust=-0.55, size = 3, label = round(t17$Clients_Amount))
    })
    
    
    output$plot10 <- renderPlot({
        ggplot(data=t18, aes(x=month, y=value,group=variable)) +
            geom_line(aes(color=variable))+
            geom_point(aes(color=variable))+
            labs(title="Clients Amount of Last Active Month", y ="Clients Amount", x ="Last Active Date" )+
            geom_text(hjust=-0.55, size = 3, label = round(t18$value))
    })
    
    output$plot11 <- renderPlot({
        ggplot(data=t19, aes(x= reorder(client_per_country, percentage),y= percentage))  +
            stat_summary(fun.y = sum, geom = "bar",colour="black",fill="black")+
            geom_bar(stat="identity") +
            labs(title="% of clients per Country", y ="%", x ="Country" ) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_text(hjust=-0.55, size = 3, label = round(t19$percentage))+
            coord_flip() })
    
    output$plot12 <- renderPlot({
        ggplot(t20, aes(x="Gender", y=percentage, fill=client_per_gender))+
            geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
    })
    
    output$plot13 <- renderPlot({
        ggplot(data=t21, aes(x= reorder(USERID, TotalStakes),y= TotalStakes))  +
            stat_summary(fun.y = sum, geom = "bar",colour="black",fill="black")+
            geom_bar(stat="identity") +
            labs(title="Top Players", y ="Total Stakes", x ="ID" ) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_text(hjust=-0.55, size = 3, label = round(t21$TotalStakes))+
            coord_flip() })
    
    output$plot14 <- renderPlot({
        ggplot(t22, aes(x="Language", y=percentage, fill=client_per_language))+
            geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
    })
    
    output$plot15 <- renderPlot({
        ggplot(data=t1.1, aes(x= reorder(profitabilityindex),y= Countryname))  +
            stat_summary(fun.y = sum, geom = "bar",colour="black",fill="black")+
            geom_bar(stat="identity") +
            labs(title="Top Players", y ="Total Stakes", x ="ID" ) +
            theme_classic() +
            theme(plot.title = element_text(hjust = 0.5))+
            geom_text(hjust=-0.55, size = 3, label = round(t21$TotalStakes))+
            coord_flip() })
    
    output$plot16 <- renderPlot({
        ggplot(t1.1, aes(x="Age", y=profitabilityindex, fill=profitabilityindex))+
            geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0)
    })
    
    output$table <- renderTable({t0})
    
}

# Here we are calling to our app
shinyApp(ui, server)

# rsconnect::deployApp("C:/Users/jpolancoroque/Desktop/R_Project")
# shinyapps::deployApp("C:/Users/jpolancoroque/Desktop/R_Project")
