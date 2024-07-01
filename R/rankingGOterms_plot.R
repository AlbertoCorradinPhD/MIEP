rankingGOterms_plot<-function(data,resName) {
  
  p <- ggplot(data,
              aes(x = .data$Term, y = .data$'Genes in the subset of interest', 
                  size = .data$'Genes in the subset of interest', 
                  fill = -log10(.data$padj))) +
    expand_limits(y = 1) +
    geom_point(shape = 21) +
    scale_size(range = c(5,15)) +
    scale_fill_continuous(low = 'royalblue', high = 'red4') +
    xlab('') + #ylab('p-value (Benjamini-Hochberg correction) in logarithmic scale') +
    ylab('Genes in the subset of interest referring to the GO term') +
    labs(
      title = paste('Top 10 GO terms based on',resName, "test"),
      subtitle = 'GOBP terms showing p-value (Benjamini-Hochberg correction)< 0.05 ',
      #caption = 'Cut-off lines drawn at equivalents of padj=0.05, padj=0.01, padj=0.001'
      ) +
    #geom_hline(yintercept = c(-log10(0.05), -log10(0.01), -log10(0.001)),
    #           linetype = c("dotted", "longdash", "solid"),
    #           colour = c("black", "black", "black"),
    #           linewidth = c(0.5, 1, 1.5)) +
    theme_bw(base_size = 10) +
    theme(
      legend.position = 'right',
      legend.background = element_rect(),
      plot.title = element_text(angle = 0, size = 18, face = 'bold', vjust = 1),
      plot.subtitle = element_text(angle = 0, size = 14, face = 'bold.italic', vjust = 1),
      plot.caption = element_text(angle = 0, size = 10, face = 'bold.italic', vjust = 1),
      axis.text.x = element_text(angle = 0, size = 12, face = 'bold', hjust = 1.10),
      axis.text.y = element_text(angle = 0, size = 12, face = 'bold', vjust = 0.5),
      axis.title = element_text(size = 12, face = 'bold'),
      axis.title.x = element_text(size = 12, face = 'bold'),
      axis.title.y = element_text(size = 12, face = 'bold'),
      axis.line = element_line(colour = 'black'),
      #Legend
      legend.key = element_blank(), # removes the border
      legend.key.size = unit(1, "cm"), # Sets overall area/size of the legend
      legend.text = element_text(size = 16, face = "bold"), # Text size
      title = element_text(size = 16, face = "bold")) +
    #ylim(10,70)+
    coord_flip()
  
}