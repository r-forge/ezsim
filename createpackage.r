library(roxygen)

# setwd('C:\\Users\\julian-new\\Dropbox\\R project\\ezsim')
package.skeleton('ezsim',
code_files=c('ezsim.R'),
namespace=TRUE,force=TRUE)
 
roxygenize('ezsim', roxygen.dir='ezsim', 
copy.package=FALSE, unlink.target=FALSE)
 