
###########
# Methods #
###########

combine_data <- function(data, info_to_extract){
    extracted_data = c()
    for(i in 1:length(data)){
        extracted_data=cbind(extracted_data, data[[i]][,info_to_extract])  
    }
    return(extracted_data)
}

plot_time_line <- function(x, y, xlab, ylab, col, plot_name, data_names){
    ylim = c(min(y), max(y))

    svg(filename= paste('results/', plot_name, '.svg', sep = ''))
    plot(1, type="n", xlab = xlab, ylab = ylab, xlim = c(1,length(years)), 
        ylim = ylim, xaxt="n")
    axis(1, at = 1:dim(y)[1], labels=x, las = 2)
    for(i in 1:dim(y)[2]) lines(1:dim(y)[1], y[,i], col = col[i])
    legend("top", col = col, lty = 1, legend = data_names, cex = 0.8)

    dev.off()
}

########
# Data #
########
data = list()
data$bioperl = read.table('data/bioperl.txt',sep="\t", h = T, row.names = 1)
data$biopython = read.table('data/biopython.txt',sep="\t", h = T, row.names = 1)
data$biojava = read.table('data/biojava.txt',sep="\t", h = T, row.names = 1)
data$bioruby = read.table('data/bioruby.txt',sep="\t", h = T, row.names = 1)
data$biophp = read.table('data/biophp.txt',sep="\t", h = T, row.names = 1)
data$biojs = read.table('data/biojs.txt',sep="\t", h = T, row.names = 1)
data$bioconductor = read.table('data/bioconductor.txt',sep="\t", h = T, 
    row.names = 1)

colors = c("#A87929",
"#C94FCA",
"#4F85A8",
"#569340",
"#C04F81",
"#CD4E3C",
"#816DC3")

years = row.names(data$bioperl)
#years = years[2:length(years)]

google_scholar_results = combine_data(data, 'google_scholar_results')
plot_time_line(years, google_scholar_results, 'Years', 
    'Number of Google Scholar results', colors, 'google_scholar_results',
    names(data))

article_citations_google_scholar = combine_data(data, 'article_citations_google_scholar')
plot_time_line(years, article_citations_google_scholar, 'Years', 
    'Number of citation of the article on Google Scholar', colors,
    'article_citations_google_scholar', names(data))
