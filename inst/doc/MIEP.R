## ----setup, echo=FALSE, results="hide"----------------------------------------
knitr::opts_chunk$set(tidy = FALSE,
                      cache = FALSE,
                      dev = "png",
                      message = FALSE, error = FALSE, warning = TRUE)

## ----quickStart, eval=FALSE---------------------------------------------------
#  # full start
#  MIEP(filePath1=".../projectName/pipeline",filePath2=".../gmtFiles")
#  # paths are passed as parameters
#  # fast start
#  MIEP()
#  # it follows the uploading of a text file providing required file paths

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 1: Extract of an *in silico* data set in the format expected by DESeq2 for the *"raw_genes_counts.tsv"* file'----
knitr::include_graphics('images/rawGeneCounts.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 2: Corresponding *"coldata.csv"* file. This is in comma-separated-value format, i.e. values in a row are separated by comma instead of tab '----
knitr::include_graphics('images/coldata.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 3: *Shiny* apps to change DESeq2 default settings'----
knitr::include_graphics('images/settings.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 4: Specifity of patients overwhelms the effect of treatment'----
knitr::include_graphics('images/PCA_before.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 5: Comparison of shrinkage methods'----
knitr::include_graphics('images/shrinkageMethodsComparison.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 6: Default thresholds for the selection of differentially expressed features are shown in Volcano plot'----
knitr::include_graphics('./images/volcanoPlotWithBoundaries.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 7: microRNAs can be saved by changing default settings'----
knitr::include_graphics('./images/microRNAs.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 8: PCA performed on Z-score-transformed data separates samples based on treatment. This facilitates the study of the effects of treatment on patients.'----
knitr::include_graphics('images/PCA_after.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap="Figure 9: Hierarchical clustering performed on gene counts gathers data based on individual patients, confirming patients' specifities as a dominant trait. Custom palette specifically designed to highlight differences in gene expression."----
knitr::include_graphics('./images/heatmap_before.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 10: Hierarchical clustering performed on Z-score-transformed data highlights two main clusters in which treated samples are separated from controls. Custom palette specifically designed to highlight differences in gene expression.'----
knitr::include_graphics('images/heatmap_after.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap="Figure 11: Genes' *importance*, i.e. their weights when composing first principal component."----
knitr::include_graphics('images/PC1_50topContributors.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 12: *Shiny* app to select the subset of genes to consider as *significant* with respect to the *gene universe*. User can choose among differentially expressed features, or on the basis of PCA results, or submit custom lists to the algorithm that will calculate the enrichment of gene ontology terms.'----
knitr::include_graphics('images/geneOntology.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 13: An example of ranking of GO terms.'----
knitr::include_graphics('images/rankingOfGOterms.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 14: *Shiny* app to select what classes to classify for.'----
knitr::include_graphics('images/shinyForRandomForests.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap="Figure 15: Features' importance represents their usefulness to separated treated samples from controls (classification by condition)."----
knitr::include_graphics('images/varImportance.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 16: Heat map of the features showing the highest *variables importance* in classification.'----
knitr::include_graphics('images/heatmapDEonly_customColors.png')

## ----out.width='75%', echo=FALSE, fig.align='center', fig.cap='Figure 17: Fold changes of genes in Figure 16 are reported graphically.'----
knitr::include_graphics('images/heatmapFC_customColors.png')

