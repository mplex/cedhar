
## 
## FUNCTION prex() to compute probability of existence ot time events
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.0.1 (18-11-2020)
##
## PARAMETERS
## x     (list or data frame object from EDH database)
## bins  (bin periods)
##


prex <-
function (x, bins = NULL) 
{
    ifelse(is.null(bins) == TRUE, bins <- list(Arch = rev(seq(from = -500, 
        to = -700)), Class = rev(seq(from = -325, to = -500)), 
        Hell = rev(seq(from = -325, to = 0)), Rom = rev(seq(from = 0, 
            to = 650)), Byz = rev(seq(from = 650, to = 1200))), 
        NA)
    ifelse(is.data.frame(x) == TRUE, xdf <- x, xdf <- edhw(x = x, 
        vars = c("not_before", "not_after"), as = "df"))
    breaks <- vector()
    for (k in seq_len(length(bins))) {
        breaks <- append(breaks, min(bins[[k]]))
    }
    rm(k)
    breaks <- append(breaks, max(bins[[length(bins)]]))
    taq <- as.numeric(as.vector(unlist(xdf$not_before)))
    tpq <- as.numeric(as.vector(unlist(xdf$not_after)))
    dur <- vector("list", length = nrow(xdf))
    names(dur) <- unlist(xdf$id)
    for (i in seq_len(nrow(xdf))) {
        if (is.na(tpq[i]) == TRUE || is.na(taq[i]) == TRUE) {
            dur[[i]] <- 1
        }
        else {
            dur[[i]] <- tpq[i] - taq[i]
        }
    }
    rm(i)
    pertaq <- vector("list", length = length(bins))
    pertpq <- vector("list", length = length(bins))
    names(pertaq) <- names(pertpq) <- names(bins)
    for (k in seq_len(length(bins))) {
        ifelse(isTRUE(length(which(taq %in% bins[[k]])) > 0) == 
            TRUE, pertaq[[k]] <- which(taq %in% bins[[k]]), NA)
        ifelse(isTRUE(length(which(tpq %in% bins[[k]])) > 0) == 
            TRUE, pertpq[[k]] <- which(tpq %in% bins[[k]]), NA)
    }
    rm(k)
    prxa <- vector("list", length = length(bins))
    names(prxa) <- names(bins)
    for (k in seq_len(length(bins))) {
        if (is.null(pertaq[[k]]) == FALSE) {
            for (i in seq_len(length(pertaq[[k]]))) {
                if (isTRUE(dur[pertaq[[k]][i]] < breaks[k + 1] - 
                  taq[pertaq[[k]][i]]) == TRUE) {
                  tmpr <- append(prxa[[k]], 1)
                }
                else {
                  tmpr <- append(prxa[[k]], (breaks[k + 1] - 
                    taq[pertaq[[k]][i]])/as.numeric(dur[[pertaq[[k]][i]]]))
                }
                prxa[[k]] <- tmpr
            }
            rm(i)
        }
        else {
            NA
        }
    }
    rm(k)
    prxp <- vector("list", length = length(bins))
    names(prxp) <- names(bins)
    for (k in seq_len(length(bins))) {
        if (is.null(pertpq[[k]]) == FALSE) {
            for (i in seq_len(length(pertpq[[k]]))) {
                if (isTRUE(dur[pertpq[[k]][i]] < tpq[pertpq[[k]][i]] - 
                  breaks[k]) == TRUE) {
                  tmpr <- append(prxp[[k]], 1)
                }
                else {
                  tmpr <- append(prxp[[k]], (tpq[pertpq[[k]][i]] - 
                    breaks[k])/as.numeric(dur[pertpq[[k]][i]]))
                }
                prxp[[k]] <- tmpr
            }
            rm(i)
        }
        else {
            NA
        }
    }
    rm(k)
    prxs <- mapply(sum, prxa, prxp, SIMPLIFY = FALSE)
    return(prxs)
}
