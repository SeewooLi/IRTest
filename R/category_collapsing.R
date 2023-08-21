#' A recommendation for the category collapsing of items based on item parameters
#'
#' @description
#' In a polytomous item, one or more score categories may not have the highest probability among the categories in an acceptable \eqn{\theta} range.
#' In this case, the category could be regarded as a redundant one in a psychometric point of view and can be collapsed into another score category.
#' This function returns a recommendation for a recategorization scheme based on item parameters.
#'
#' @param item.matrix A matrix of item parameters.
#' @param range A range of \eqn{\theta} to be evaluated.
#' @param increment A width of the grid scheme.
#'
#' @return A list of recommended recategorization for each item.
#'
#' @author Seewoo Li \email{cu@@yonsei.ac.kr}
#'
#' @export
#'
cat_clps <- function(item.matrix, range = c(-4,4), increment=0.005){
  clpsd_rslt <- list()
  item_names <- row.names(item.matrix)
  for(i in 1:nrow(item.matrix)){
    par <- item.matrix[i,]
    n_cat <- sum(!is.na(par))
    cats <- (1:n_cat)
    theta_scale <- seq(range[1],range[2],increment)
    p_matrix <- P_P(theta_scale, par[1], par[-1])
    which_cat_is_max <- apply(p_matrix, 1, which.max)
    to_be_colpsd <- setdiff(cats, unique(which_cat_is_max))
    modes <- apply(as.matrix(p_matrix[,to_be_colpsd]), 2, which.max)
    after <- which_cat_is_max[modes]
    cats[cats%in%to_be_colpsd] <- after
    cats <- list(reorder_vec(cats))
    names(cats) <- item_names[i]
    clpsd_rslt <- append(clpsd_rslt, cats)
  }
  return(clpsd_rslt)
}

#' Recategorization of data using a new categorization scheme
#'
#' @description
#' With a recategorization scheme as an input, this function implements recategorization for the input data.
#'
#' @param data An item response matrix
#' @param new_cat A list of a new categorization scheme
#'
#' @return Recategorized data
#'
recategorize <- function(data, new_cat){
  data <- reorder_mat(data)
  for(i in 1:ncol(data)){
    new_cat_item <- new_cat[[i]]
    for(j in 1:length(new_cat_item)){
      data[data[,i]==(j-1),i] <- new_cat_item[j]
    }
  }
  return(data)
}
