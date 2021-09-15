#' Convert object or its items into a particular type
#'
#' Convert an object, such as `vector` or `matrix` or items of an object for `list` and `data.frame`
#' into a particular type, .

convert = function(x, type){
    UseMethod("convert", x)
    }


convert.default = function(x, type){
    if (typeof(x) == type) x else as(x, type)
    }


convert.list = function(x, type){
    x[] = lapply(x, function(y){convert(y, type)})
    x
    }


convert.data.frame = function(x, type){
    x[] = lapply(x, function(y){convert(y, type)})
    x
    }


convert.matrix = function(x, type){
    mode(x) = type
    x
    }
