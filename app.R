#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


##Load data
load('/home/velazqam/Documents/Projects/Germline-Promoters/BigDATA.RData')

germ=read.table("/home/velazqam/Documents/Projects/Germline-Promoters/All-GermlinExpressedGenes-PATC.2.txt",header=TRUE, sep="\t")
#My bad
germ=germ[-4515,]
rownames(germ)=as.character(germ[,2])

genes=read.table("/home/velazqam/Documents/Projects/Germline-Promoters/Genes-PATC-TSS.txt",header=TRUE,sep='\t')
rownames(genes)=as.character(genes[,2])

##Get info and calculate scores from bigdata
####Consumes a lot of memory!
##rownames(BigdataP)=as.character(Biginfo[,2]) 
##rownames(BigdataN)=as.character(Biginfo[,2])
rownames(Biginfo)=as.character(Biginfo[,2]) 
Biginfo[,6]=1:nrow(Biginfo)

##Produce previous plots
BP2=apply(BigdataP[Biginfo[rownames(germ),6],],2,mean)
BN2=apply(BigdataN[Biginfo[rownames(germ),6],],2,mean)

GBP2=apply(BigdataP,2,mean)
GBN2=apply(BigdataN,2,mean)



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Transcriptional Start Sites (TSSs) around germline genes"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("x",
                        "Window size (100bp):",
                        min = 0,
                        max = 100,
                        value = 5),
            sliderInput("y",
                        "Y axis (in 1/1000 setps):",
                        min = 0,
                        max = 100,
                        value = 5)),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("ALLPlot"),
            plotOutput("GermPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$ALLPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x = input$x*100
        y = input$y/1000
        
        plot(GBP2[c((10001-x):10001,10002:(10001+x))],type='l',ylim=c(0,y), ylab="Normalized frequency of ATGs", main='ATGs around the 1st ATG of all genes',xlab='bp around 1st ATG',xaxt='n')
        lines(GBN2,col='red')
        labi=c(paste("-",x,sep=""),"0",paste("-",x,sep=""))
        axis(1, at=c(0,x,x+x),labels=labi, col.axis="black", las=2)
    })
    
    output$GermPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x = input$x*100
        y = input$y/1000
        
        plot(BP2[c((10001-x):10001,10002:(10001+x))],type='l',ylim=c(0,y), ylab="Normalized frequency of ATGs", main='ATGs around the 1st ATG of germline-expressed genes',xlab='bp around 1st ATG',xaxt='n')
        lines(BN2,col='red')
        labi=c(paste("-",x,sep=""),"0",paste("-",x,sep=""))
        axis(1, at=c(0,x,x+x),labels=labi, col.axis="black", las=2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)