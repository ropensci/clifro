#' @include cfData.R cfStation.R
 
# cfDataList --------------------------------------------------------------

#' @importFrom methods setClass
setClass("cfDataList", contains = "list")

# Methods -----------------------------------------------------------------

#' @importFrom methods setClass
setMethod("show",
          signature(object = "cfDataList"),
          function (object)
          {
            n_row = sapply(object@.Data, nrow)
            data = sapply(object, slot, "dt_name")
            type = sapply(object, slot, "dt_type")
            type[is.na(type)] = ""
            start_list = lapply(object, function(x)
              min(as(x, "data.frame")[, 2]))
            start = sapply(start_list, format, "(%Y-%m-%d %k:00)")
            end_list = lapply(object, function(x)
              max(as(x, "data.frame")[, 2]))
            end = sapply(end_list, format, "(%Y-%m-%d %k:00)")
            cat("List containing clifro data frames:\n")
            print(data.frame(data = tidy_names(data, max_len = 15),
                             type = type,
                             start = start,
                             end = end,
                             rows = n_row,
                             row.names = paste0("df ", seq_along(n_row), ")")))
          }
)

#' @importFrom methods setMethod
setMethod("[",
          signature(x = "cfDataList"),
          function (x, i, j){
            if (!missing(j))
              warning("column subscripts ignored")
            x@.Data[[i]]
          }
)

#' @importFrom methods setMethod
setMethod("[[",
          signature(x = "cfDataList"),
          function (x, i, j){
            if (!missing(j))
              warning("column subscripts ignored")
            x@.Data[[i]]
          }
)