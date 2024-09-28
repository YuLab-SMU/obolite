##' parse obo file
##'
##'
##' @title read-obo
##' @param file obo file
##' @return a list of obo information, including term, relation, alias and synonym
##' @importFrom yulab.utils rbindlist
##' @export
##' @author Guangchuang Yu \url{https://yulab-smu.top}
read.obo <- function(file) {
    x = readLines(file)

    start = grep("^\\[Term\\]", x)
    end <- c(start[-1] -1, length(x))

    res <- lapply(seq_along(start), function(i) {
        extract_obo_item(x[start[i]:end[i]])
    })

    j <- vapply(res, is.null, logical(1))
    message(paste0(sum(j), '/', length(j)), " obsolete terms found.")
    res <- res[!j]

    info <- lapply(res, function(x) x$do) |> 
            rbindlist() |> 
        as.data.frame()
    alias <- lapply(res, function(x) x$alias) |>
            rbindlist()
    synonym <- lapply(res, function(x) x$synonym) |>
            rbindlist()
    rel <- lapply(res, function(x) x$relation) |>
            rbindlist()

    return(list(info = info, rel = rel, alias = alias, synonym = synonym))
}


extract_obo_item <- function(item) {
    i <- grep('^\\[Typedef\\]', item)
    if (length(i) > 0) {
        item <- item[-(i[1]:length(item))]
    }
    ## is_obsolete: true
    useless <- get_obo_info(item, '^is_obsolete:')
    if (!is.na(useless)) return(NULL)

    id <- get_obo_info(item, "^id:")
    level <- get_obo_info(item, "^namespace:")
    name <- get_obo_info(item, "^name:")
    alt_id <- get_obo_info(item, "^alt_id:")
    synonym <- get_obo_info(item, "^synonym:")
    #synonym[,2] <- sub('\\"\\s*EXACT\\s\\[\\]', "", synonym[, 2]) 
    def <- get_obo_info(item, "^def:")
    def <- sub('\\"', "", def)
    def <- sub('\\".*', "", def)

    isa <- get_obo_info(item, '^is_a:')
    isa <- sub("\\s*!.*", "", isa)
    res <- list(do=c(id=id, name=name, def=def, level = level),
        alias = data.frame(id = id, alias = alt_id),
        synonym = data.frame(id = id, synonym = synonym),
        relationship = data.frame(id=id, parent=isa))
}

get_obo_info <- function(item, pattern) {
    i <- grep(pattern, item)
    if (length(i) == 0) return(NA)

    sub("\\s*", "",
            sub(pattern, "", item[i])
    )
}