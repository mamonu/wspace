library(wordspace)
library(MASS)
library(shiny)




Triples <- subset(DSM_VerbNounTriples_BNC, mode == "written")

VObj <- dsm(target=Triples$noun, feature=Triples$verb, score=Triples$f,raw.freq=TRUE, sort=TRUE)


VObj <- subset(VObj, nnzero >= 3, nnzero >= 3, recursive=TRUE)

dim(VObj)


VObj <- dsm.score(VObj, score="simple-ll", transform="log", normalize=TRUE)

VObj300 <- dsm.projection(VObj, method="rsvd", n=150, oversampling=4)




server <- function(input, output) {

  output$info <- renderText({
    
    paste0(' cosine pair distance:  ', pair.distances(paste0(str_replace_all(input$text,"([[:punct:]])|\\s+","+")), 
                   paste0(str_replace_all(input$text2,"([[:punct:]])|\\s+","+")), 
                   VObj300, method="cosine", convert=FALSE))
    
  })
  
    output$plot1 <- renderPlot({
    nn1 <- nearest.neighbours(VObj300, paste0(str_replace_all(input$text,"([[:punct:]])|\\s+","+")), n=15)
    nn.terms <- c(paste0(str_replace_all(input$text,"([[:punct:]])|\\s+","+")), names(nn1))
    # nn = distances labelled with the neighbour terms
    nn.dist <- dist.matrix(VObj300, terms=nn.terms, method="cosine")
    plot(nn.dist,expand=.1, cex=1.2)
  })

    output$plot2 <- renderPlot({
      nn2 <- nearest.neighbours(VObj300, paste0(str_replace_all(input$text2,"([[:punct:]])|\\s+","+")), n=15)
      nn.terms2 <- c(paste0(str_replace_all(input$text2,"([[:punct:]])|\\s+","+")), names(nn2))
      # nn = distances labelled with the neighbour terms
      nn2.dist <- dist.matrix(VObj300, terms=nn.terms2, method="cosine")
      plot(nn2.dist,expand=.1, cex=1.2)
    })
  
  
  }






# a standard R package that includes two MDS implementations
 
#mds <- isoMDS(nn.dist, p=2)
#plot(mds$points, pch=20, col="red")
#text(mds$points, labels=nn.terms, pos=3)

