#' Statistical Software Popularity
#'
#' Some queries on Google Scholar on statistcal software,
#' based on a blog post on librestats.com by Robert A. Muenchen.
#'
#' @docType data
#' @return a Gscholar object 
#' @references 
#' \url{http://librestats.com/2012/04/12/statistical-software-popularity-on-google-scholar/}
#' @export
statsoft_popularity <- function() gscholar(
  map.lst=list(
    BDMP="BDMP",
    JMP="JMP+AND+%22SAS+Institute%22",
    Minitab="Minitab",
    SPSS="SPSS",
    SAS="%22SAS+Institute%22+-JMP",
    Stata="Statacorp",
    Statistica="%22Statsoft+Statistica%22",
    Systat="Systat",
    R="%22the+R+software%22+OR+%22the+R+project%22+OR+%22r-project.org%22+OR+hmisc+OR+ggplot2+OR+RTextTools",
    SPlus="%22s-plus%22%2Btibco+OR+%22s-plus%22%2B%22insightful%22"
  ) 
)
