#' Find the densest subset of a matrix
#'
#' Given a matrix with unknown values, find a densest subset of matrix.
#'
#' This is a stepwise filtering algorithm that deletes the least represented columns and/or rows
#' to get a matrix subset with the least amount of unknown values until an optimum is reached.
#'
#' In a lot of cases, an optimum would be a single vector. For this reasons, the algorithm
#' is limited both by the number of steps and by a desired density.
#'
#' @param x a matrix with unknown values
#' @param empty the unknown value if different from NA; such as `0` for a numeric matrix or `N`,
#' `empty` or `unknown` for character matrix
#' @param steps the number of steps
#' @param density a desired density
#'
#' @return a list containing the densest submatrix, vectors of deleted columns and a vector of
#' densities  at each step of the algorithm
#' @export
densest_subset = function(x, empty=NA, steps=100, density=1){
    if(!is.na(empty))
        mat = empty_to_na(x, empty)

    mat = !is.na(mat)
    mode(mat) = "numeric"

    rowsums = IndexedVector$new(rowSums(mat))
    colsums = IndexedVector$new(colSums(mat))

    rowidxdel = c()
    colidxdel = c()
    densities = c()

    for(i in seq_len(steps)){
        rowmin = rowsums$min()
        colmin = colsums$min()

        # This is very ugly, but I have no clue how to refactor this
        if(rowmin <= colmin){
            # rows are smaller
            indices = rowsums$which_min(rowmin)
            matindices = rowsums$indices[indices]

            rowidxdel = c(rowidxdel, matindices)
            if(!is.null(colidxdel)){
                del_colsums = colSums(mat[matindices, -colidxdel, drop=FALSE])
                } else {
                del_colsums = colSums(mat[matindices, , drop=FALSE])
                }

            rowsums$subset(-indices)
            colsums$substract(del_colsums)
            } else {
            # columns are smaller
            indices = colsums$which_min(colmin)
            matindices = colsums$indices[indices]

            colidxdel = c(colidxdel, matindices)
            if(!is.null(rowidxdel)){
                del_rowsums = rowSums(mat[-rowidxdel, matindices, drop=FALSE])
                } else {
                del_rowsums = rowSums(mat[, matindices, drop=FALSE])
                }
            colsums$subset(-indices)
            rowsums$substract(del_rowsums)
            }
     

        if(rowsums$sum() != colsums$sum() )
            stop("Rowsums and colsums are not equal. This shouldn't happen.")

        if(rowsums$len() == 0 || colsums$len() == 0)
            break

        current_density = rowsums$sum() / (rowsums$len()*colsums$len())
        densities = c(densities, current_density)

        if(current_density >= density)
            break
        }


    list(
        "density" = densities,
        "delted_rows" = rowidxdel,
        "deleted_columns" = colidxdel,
        "representation_matrix" = mat,
        "result" = x[-rowidxdel, -colidxdel]
        )
    }
