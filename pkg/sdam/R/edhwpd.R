
## 
## FUNCTION edhwpd() to organize EDH dataset province and dates by similarity
## (CC BY-SA 4.0) Antonio Rivero Ostoic, multiplex@post.com 
##
## version 0.0.5 (02-06-2023)
##
## PARAMETERS
##
## vars     (vector, variables or attributes to be chosen from x)
## province (choosen EDH province abbreviation)
##
## OPTIONAL PARAMETERS
##
## x        (list, typically fragments of EDH dataset or database API)
## dates    (vector with TAQ and TPQ)
## clean    (clean NAs and re-encode? logical and optional)
##


edhwpd <-
function (x = "EDH", vars, dates, province, clean, ...) 
{
    if (is.null(x) == TRUE) 
        stop("'x' is NULL")
    ifelse(missing(dates) == TRUE, dates <- c("not_before", "not_after"), 
        NA)
    ifelse(missing(vars) == TRUE && isTRUE(x == "EDH") == FALSE, 
        vars <- colnames(x)[which(!(colnames(x) %in% c(dates, 
            "id")))], NA)
    if (missing(province) == TRUE && is.list(x[[1]]) == TRUE) {
        province <- x[[1]]$province_label
    }
    else if (missing(province) == TRUE && isTRUE(length(attr(x, 
        "class")) == 2) == TRUE) {
        province <- attr(x, "class")[2]
    }
    else {
        province <- NULL
    }
    if (isTRUE(x == "EDH") == TRUE) {
        message("\"x\" is for dataset \"EDH\".")
        if (!(exists("EDH"))) {
            utils::data("EDH", package = "sdam", envir = environment())
            EDH <- get("EDH", envir = environment())
        }
        else {
            NA
        }
        x <- EDH
        class(x) <- NULL
        comment(x) <- NULL
        xdf <- suppressMessages(edhw(x = x, vars = c(dates, vars, 
            "province_label"), as = "df"))
        prv <- unique(edhw(x = xdf, vars = c(dates, vars), province = province, 
            as = "df"))
    }
    else {
        prv <- edhw(x = x, vars = c(dates, vars), as = "df")
    }
    if (isTRUE(nrow(prv) == 0) == TRUE) 
        return(NULL)
    ifelse(missing(clean) == FALSE && isTRUE(clean == TRUE) == 
        TRUE, prv <- cln(prv, ...), NA)
    smat <- simil(prv, vars = vars)
    mat <- multiplex::rm.isol(multiplex::dichot(smat, c = max(smat)))
    cmps <- multiplex::comps(mat)
    lbs0 <- unlist(cmps$com)
    ddf <- vector("list", length = length(cmps$com))
    for (i in seq_len(length(ddf))) {
        ddf[[i]] <- edhw(x = prv, id = cmps$com[[i]])
    }
    rm(i)
    if (isTRUE(length(which(!(dimnames(smat)[[1]] %in% lbs0))) > 
        0) == TRUE) {
        if (isTRUE(length(which(!(dimnames(smat)[[1]] %in% lbs0))) == 
            1) == TRUE) {
            ddf1 <- edhw(x = prv, id = dimnames(smat)[[1]][which(dimnames(smat)[[1]] %in% 
                lbs0 == FALSE)])
            mat1 <- smat1 <- NULL
            lbs1 <- dimnames(smat)[[1]][which(!(dimnames(smat)[[1]] %in% 
                lbs0))]
        }
        else {
            smat1 <- smat[which(!(dimnames(smat)[[1]] %in% lbs0)), 
                which(!(dimnames(smat)[[1]] %in% lbs0))]
            mat1 <- multiplex::rm.isol(multiplex::dichot(smat1, 
                c = max(smat1)))
            cmps1 <- multiplex::comps(mat1)
            lbs1 <- unlist(cmps1$com)
            ddf1 <- vector("list", length = length(cmps1$com))
            for (i in seq_len(length(ddf1))) {
                ddf1[[i]] <- edhw(x = prv, id = cmps1$com[[i]])
            }
            rm(i)
        }
        lddf <- list(ddf, ddf1)
    }
    else {
        lddf <- ddf
        lbs1 <- vector()
    }
    if (isTRUE(length(vars) > 2) == TRUE && isTRUE(all.equal(mat1, 
        smat1) == FALSE) == TRUE) {
        lbsx <- c(lbs0, lbs1)
        ddfx <- list()
        for (k in seq(3, length(vars))) {
            smatx <- smat[which(!(dimnames(smat)[[1]] %in% lbsx)), 
                which(!(dimnames(smat)[[1]] %in% lbsx))]
            matx <- multiplex::rm.isol(multiplex::dichot(smatx, 
                c = max(smatx)))
            cmpsx <- multiplex::comps(matx)
            lbsx <- append(lbsx, unlist(cmpsx$com))
            ddftmp <- vector("list", length = length(cmpsx$com))
            for (i in seq_len(length(ddftmp))) {
                ddftmp[[i]] <- edhw(x = prv, id = cmpsx$com[[i]])
            }
            rm(i)
            ddfx[[length(ddfx) + 1L]] <- ddftmp
        }
        rm(k)
        ifelse(isTRUE(length(cmpsx$isol) == 0) == TRUE, NA, ddfx[[length(ddfx) + 
            1L]] <- edhw(x = prv, id = cmpsx$isol))
        res <- c(lddf, ddfx)
    }
    else {
        ifelse(isTRUE(nrow(prv[which(prv$id %in% c(lbs0, lbs1) == 
            FALSE), ]) == 0) == TRUE, NA, lddf[[length(lddf) + 
            1L]] <- prv[which(prv$id %in% c(lbs0, lbs1) == FALSE), 
            ])
        res <- lddf
    }
    class(res) <- noquote(c("EDH", province, nrow(prv)))
    res
}
