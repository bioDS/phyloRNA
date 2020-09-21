#' Functions for manipulation with the expression data
#'
#' A group of functions, often lifted and modified from the Seurat package for manipulation with the 10X scRNAseq data.
#'
#' The Seurat package is a great tool for manipulation with the 10X scRNAseq expression data. However, it has two major issues. The first one is that it assumes that the zero expression is true zero. While this is reasonable assumption with a high coverage, small coverage scRNAseq can suffer for drop out due to the nature of a small amount of starting product and certain randomness coming from used methodology. This means that the measured zero level of expression is more accurately described as a missing data. Unfortunatelly, the sparse matrice implementation used by Seurat can not allow this change of context.
#'
#' The second issue is the huge amount of dependencies that the Seurat brings. Due to the limited scope in which Seurat functionality is used and given that the utilized functionality had to be already rewritten due to the above reasons, it seems more convenient to just lift up remaining Seurat functionality.
#'
#' @name expr
NULL

#' @describeIn expr Read 10X data
#'
#' @param dir a directory with barcodes, features and sparse matrix
#' @param gene_column **optional** the position of column with gene/feature names
#' @param unique_features **optional** gene/feature names will be made unique to prevent possible
#' name conflict
#' @param strip_suffix **optional** the `-1` suffix which is common for 10X barcodes
#' @return sparse matrix
#'
#' @export
expr_read10x = function(dir, gene_column=2, unique_features=TRUE, strip_suffix=FALSE){
    barcodes = file.path(dir, "barcodes.tsv.gz")
    features = file.path(dir, "features.tsv.gz")
    matrix_mtx = file.path(dir, "matrix.mtx.gz")

    barcodes = readLines(barcodes)
    if(strip_suffix)
        barcodes = sub("-1$", "", barcodes)

    features = get_features(features, gene_column=2, make_unique=unique_features)
        rownames(x = data) = make.unique(names = features[, gene_column])

    data = Matrix::readMM(matrix_mtx)
    rownames(data) = features[,gene_column]

    if(ncol(features) > 2){
        data = split_data(data, features[,3])
        } else {
        data = list(data)
        }

    return(data)
    }


#' @describeIn expr Read 10X data in the `.h5` format.
#'
#' @param input an input data in the `.h5` format
#' @param use_names **optional** use gene names instead of gene IDs
#' @param unique_features **optional** gene/feature names will be made unique to prevent possible
#' name conflict
#' @return a list of sparse matrices
#'
#' @export
expr_read10xh5 = function(input, use_names=TRUE, unique_features=TRUE){
    h5 = hdf5r::H5File$new(input, mode="r")
    genomes = names(h5)

    output = list()
    if(!h5$attr_exists("PYTABLES_FORMAT_VERSION")){
        # cellranger version 3
        if(use_names){
            feature_slot = "features/name" 
            } else {
            feature_slot = "features/id"
            }
        } else {
        if(use_names){
            feature_slot = "gene_names"
            } else {
            feature_slot = "genes"
            }
        }
    for(genome in genomes){
        counts = h5[[paste0(genome, "/data")]]
        indices = h5[[paste0(genome, "/indices")]]
        indptr = h5[[paste0(genome, "/indptr")]]
        shp = h5[[paste0(genome, "/shape")]]
        features = h5[[paste0(genome, "/", feature_slot)]]
        barcodes = h5[[paste0(genome, "/barcodes")]]

        sparse_matrix = Matrix::sparseMatrix( 
            i = indices[] + 1,
            p = indptr[],
            x = as.numeric(counts[]),
            dims = shp[],
            giveCsparse = FALSE
            )
        if(unique_features){
            features = make.unique(features[])
            }
        rownames(sparse_matrix) = features
        colnames(sparse_matrix) = barcodes[]

        sparse_matrix = methods::as(sparse_matrix, Class = "dgCMatrix")
        # Split v3 multimodal
        if(h5$exists(name = paste0(genome, "/features"))){
            types = h5[[paste0(genome, "/features/feature_type")]][]
            types_unique = unique(types)
            if(length(types_unique) > 1){
                message(
                    "Genome ", genome, " has multiple modalities,",
                    " returning a list of matrices for this genome")
                sparse_matrix = lapply(types_unique, function(x) sparse_matrix[which(types == x), ])
                names(sparse_matrix) = types_unique
                }
            }
        output[[genome]] = sparse_matrix
        }

    h5$close_all()
    if(length(output) == 1){
        return(output[[genome]])
        } else {
        return(output)
        }
    }


split_data = function(data, type){
    types = factor(type)
    levels = levels(types)

    if(length(levels) > 1){
        message("10x data contains more than one type and is being returned as a list containing matrices of each type.")
        }

    expr_name = "Gene Expression"
    if(expr_name %in% levels){ # Return the Gene Expression data first
        levels = c(expr_name, levels[-which(levels == expr_name)])
        }

    result = lapply(levels, function(x) data[types == x, , drop=FALSE])
    names(result) = levels

    return(result)
    }


get_features = function(file, gene_column=2, make_unique=TRUE){
    features = utils::read.table(file, sep="\t", header=FALSE, stringsAsFactors=FALSE)

    na_features = is.na(features[gene_column])
    replacement_column = if(gene_column == 2) 1 else 2

    if(gene_column >= ncol(features)){
        stop(paste0(
            "Gene column was set to:", gene_column,
            " but the data matrix has only ", ncol(features), " columns."
            ))
        }

    if(any(na_features)){
        warning("Some feature names are NA. Replacing with the names from the opposite column")
        features[na_features, gene_column] = features[na_features, replacement_column]
        }

    if(make_unique)
        features[, gene_column] = make.unique(features[, gene_column])

    return(features)
    }



#' @describeIn expr Log-normalize data. Feature counts for each cell are divided by the total count
#' for that cell multiplied by a scale factor. This is then natural log transformed using log1p.
#'
#' @param data an expression matrix 
#' @param scale_factor **optional** a scaling factor
#' @return log-normalized matrix
#'
#' @export
expr_normalize = function(data, scale_factor=10000){
    totals = rowSums(data, na.rm=TRUE)
    x = log1p( (data/totals) * scale_factor)
    data
    }


#' @describeIn expr Scale and center genes/features
#'
#' @param data an expression matrix
#' @return rescaled and centered data
#'
#' @export
expr_scale = function(data){
    t(scale(t(data)))
    }


#' @describeIn expr Transform a sparse matrix into dense matrix where zeros are respresented
#' as `NA`.
#'
#' @param data a sparse matrix
#' @return a dense matrix with `NA` instead of zeros
#'
#' @export
expr_zero_to_na = function(data){
    data[data == 0] = NA
    as.matrix(data)
    }
