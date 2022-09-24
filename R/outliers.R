#' Title
#'
#' @param x A data frame
#' @param column
#' @param col_name
#'
#' @import tidyverse
#' @import stats
#'
#' @return
#' @export
#'
#' @examples outlier_r(df, "col", col)

outlier_r <- function(x,column,col_name) {
  Q <- quantile(x[[column]], probs = c(.25,.75), na.rm = FALSE)
  iqr <- IQR(x[[column]])

  up <- Q[2]+1.5*iqr
  low <- Q[1]-1.5*iqr

  outliers_removed <- x %>%
    filter(col_name > low & col_name < up)

  return(outliers_removed)
}

