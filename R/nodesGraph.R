#NB: topGO fa require('Rgraphviz') || stop('package Rgraphviz is required')

nodesGraph<- function(sampleGOdata,result,ntop, wantedNodes, outDir, w,h,r) {
  
  scores<-sort(score(result))
  firstSigNodes<-sum(names(scores[1:ntop]) %in% wantedNodes)
  indexes<-which (!(names(scores[1:ntop]) %in% wantedNodes))
  scores[indexes]<-1
  p<-NULL
  tryCatch( {
    require("Rgraphviz")
    filename<-paste("nodesGraph.tiff",sep="")
    filePath<-file.path(outDir,filename)
    tiff(filePath, width=w, height=h,res=r)
    p<-showSigOfNodes(sampleGOdata, scores, firstSigNodes=firstSigNodes, useInfo = 'all',
                      wantedNodes=wantedNodes, putWN = TRUE,plotFunction = GOplot, sigForAll = FALSE,
    )
    show(p)
    dev.off()
    },
    error = function(err){
      print('install Rgraphviz package to plot nodes graphs...')
      print('...install "graphviz" in your computer before')
    })#chiudo tryCatch
  return(p)
}

