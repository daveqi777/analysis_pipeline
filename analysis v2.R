##############################################################################################
# README 
# pipe_analysis function will run the % population analysis and output HTML and PDF outputs. 
# User needs to define the following function parameters
# csv_file: the input csv file
# dependent: the response variable
# xvariables: the explanatory variables
# latlong: lat and long
# cutoff: % of population. defaulted to be 10%

# HOWTO
# Put the two files analysis v2.R and Doc.Rmd in your working directory
# Run the function. 
# The two output files should be created ending with time stamp. 
##############################################################################################

pipe_analysis <- function(
    csv_file = 'samp_20180417.csv'
    ,xvariables = c("Ind1", "Ind2", "Ind3", "Ind4", "Ind5", "Ind6")
    ,dependent = c("Dependent")
    ,hierarchy = c("Hierarchy1", "Hierarchy2", "Hierarchy3", "Hierarchy4")
    ,latlong = c("Lat", "Long")
    ,cutoff = 0.1
){
  csv_file = csv_file
  x = xvariables
  y = dependent
  h = hierarchy
  ll = latlong
  
  libs <- list(
    'data.table'
    ,'Hmisc'
    ,'psych'
    ,'knitr'
    ,'rmarkdown'
    ,'dplyr'
    ,'leaflet'
    ,'DT'
  )
  
  libcheck <- function(x){
    if (require(x, character.only=T)==T) {cat(paste("pakcage",x,"has been loaded"),"\n")
    } else {
      message("Process needs to install dependent packages\n")
      install.packages(x, quiet=T)
      suppressMessages(suppressWarnings(require(x, character.only=T)))
    } 
  }
  
  sapply(libs, libcheck)
  
  data = fread(csv_file)
  if (!all(x %in% names(data))) {stop("User input variable names not found in file")}
  if (!all(y %in% names(data))) {stop("User input dependent variable name not found in file")}
  if (!all(h %in% names(data))) {stop("User input hierarchy variable name not found in file")}
  if (!all(ll %in% names(data))) {stop("User input lat or Long variable names not found in file")}

  data$Rank = rank(data[[y]])
  data$Qtl = 1 - ((data$Rank-1)/nrow(data))
  data$Group = do.call(paste0, data[, h, with=F])
  
  # apply cut-off
  data = data[Qtl<=cutoff]
  datapoints = copy(data)
  datapoints = datapoints[order(datapoints[[y]], decreasing=TRUE), ]
  datapoints = select(datapoints, -c(Rank, Qtl, Group))
  
  # variable stats by group
  mat = data[, c("Group", x), with=F]
  map = unique(data[, c("Group", h), with=F])
  stat = describeBy(mat, group=mat$Group, digits=2)
  
  # variable vs hierarchy
  for (i in x){
    t = data[[i]]
    with(data, boxplot(t~Group
                       ,las=2
                       ,cex.axis=0.7
                       ,at=rank(tapply(t, Group, median))
                       ,main=paste(i, 'vs Hierarchy')
    ))
  }
  
  # Map the points
  dots = data[, ll, with=F]
  
  leaf_plot <- dots %>% 
    leaflet() %>% 
    addTiles('') %>%
    addMarkers(clusterOptions = markerClusterOptions()) %>% 
    addProviderTiles(providers$OpenStreetMap)
  
  # Render doc
  render('Doc.Rmd', c('html_document', 'pdf_document'))
  # tryCatch(render('Doc.Rmd', c('html_document', 'pdf_document'))
  #          ,error=function(e) message('Please check the warning message for file convertion requirements\n'))
  
  if (file.exists("Doc.html")) {
  timepoint = format(Sys.time(), "%Y-%m-%d__%H-%M-%S")
  newfile = paste0(file.path(getwd()), "/Analysis_output_", timepoint, ".html")
  file.rename("Doc.html", newfile)
  }
  if (file.exists("Doc.pdf")) {
    timepoint = format(Sys.time(), "%Y-%m-%d__%H-%M-%S")
    newfile = paste0(file.path(getwd()), "/Analysis_output_", timepoint, ".pdf")
    file.rename("Doc.pdf", newfile)
  }
}
