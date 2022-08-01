# fix 'province'
# na.rm default FALSE
# c/ cln117()
# Error in as.table.default(cln(xp))
# vars with id and limit in df
# clean people asdataframematrix
# 'clean' arg c/ cln114()
# accept EDH again
# missing(id) error
# 'id' bug x=NULL
# 'vars' works with 'province'
# correct df to list Don't try clean arg
# NAs in province
# bug when x NULL: flglv=TRUE
# bug NULL class df x; na.rm TRUE default
# bugs as df valids=1
# new arg 'ldf' list of data frames
# flglv
# df -> df flgv=TRUE
# edhsd$Arm AGAIN
# people vars/id list dfs: ids df BUG
# vars: ids df BUG
# id not in EDH
# HD in id
# id as list
# province
# 'id'
# 'ids' for json->list->df
# JUMP 133 -> 138
# Error: $ operator is invalid for atomic vectors
# open to 'province_label'-like options as gender ('df' default)
# vars to extract variables from df, id alone where?
# more fix ids even with NAs
# fix ids
# correct NA's for EDH PART II (valids <- seq_len(length(edhl)) ruined all)
# correct NA's for EDH
# all/none vars
# na.rm and 'people'
# handle exceptions in 'wide'
# handle NULLs and empty vars
# for 'id' ID->id
# fix limit > length(x)
# remove people in lists when not specified
# combine select with no-people vars
# multiple select in lists
# resolve na.rm df and list
# NAs 100 111 381 402 485
# fix warnings NA
# fix bug merge by HDid
# 'select' with 'wide' format
# fix 'wide' format
# optimize ids when people
# EDH 0.3.0

edhw <- function(x="EDH", vars, as=c("df","list"), type=c("long","wide","narrow"), 
                    split, select, addID, limit, id, na.rm, ldf, province, gender, rp, clean,...) { #

# for EDH
if(is.null(x)==TRUE) stop("\'x\' is NULL")

## na.rm default FALSE
ifelse(missing(na.rm)==FALSE && isTRUE(na.rm==TRUE)==TRUE, na.rm<-TRUE, na.rm<-FALSE)


# default: list object input
flgdf <- FALSE
if(isTRUE(x=="EDH")==TRUE) {
#if(is.null(x)==TRUE) {
# when get.edh() produces a NULL output
#warning("\"x\" is NULL and dataset \"EDH\" is taken if available.")
warning("\"x\" is for dataset \"EDH\".")
if(missing(province)==FALSE) warning("\'province\' works with \'x\' as data frame")
# moved/changed
flglv <- TRUE
#
if(!(exists("EDH"))) {
  utils::data("EDH", package="sdam", envir=environment())
  EDH <- get("EDH", envir=environment()) 
  # remove metadata: citation
  #  EDH$cite <- NULL
} else { NA } #stop("\"x\" is NULL and \"EDH\" dataset not available.")
x <- EDH
class(x) <- NULL
comment(x) <- NULL
#
} else if(isTRUE(is.data.frame(x)==TRUE)==TRUE || isTRUE(is.data.frame(x[[1]])==TRUE)==TRUE) {
	flgdf <- TRUE
#
	if(isTRUE(is.data.frame(x[[1]])==TRUE)==TRUE) {
	warning("\"x\" is list of data frames.")
	#data.frame(do.call(rbind, x), stringsAsFactors=FALSE)
	#options(stringsAsFactors=FALSE)
	x <- data.frame(lapply(do.call("rbind.data.frame", x), as.character), stringsAsFactors=FALSE)
	#x <- do.call("rbind.data.frame", x)
	rownames(x) <- NULL
	#warning("\"x\" is list of data frames, only first component is taken.")
	#x <- x[[1]]
	} else { 
	ifelse(isTRUE(is.list(x)==TRUE)==TRUE, x<-as.data.frame(x), NA)
	}
# coerce NA's
#ifelse((match.arg(as)=="df") %% (class(x)%in%c("tbl_df","tbl")==FALSE), x[x=="list()"]<-NA, NA)
#
	# check 'id' 'limit'
if(match.arg(as)=="df") {	
	if(missing(id)==FALSE) {	
		if(missing(vars)==FALSE) {
		ifelse(all(vars%in%colnames(x))==TRUE,
		return(x[which(x$id%in%id),vars]), NA
		)
		} else { 
		return(x[which(x$id%in%id),])
		}
	} else if(missing(limit)==FALSE) { 
	NA
	}
} else if(match.arg(as)=="list") {
	if(missing(id)==FALSE) {	
		if(missing(vars)==FALSE) {
		ifelse(all(vars%in%colnames(x))==TRUE,
		return(as.list(x[which(x$id%in%id),vars])), NA
		)
		} else { 
		return(as.list(x[which(x$id%in%id),]))
		}
	} else if(missing(limit)==FALSE) { 
		if(missing(vars)==FALSE) {
		ifelse(all(vars%in%colnames(x))==TRUE,
		xll <- as.list(x[seq_len(limit),which(colnames(x)%in%c("id",vars))]), NA
		)
		} else { 
		xll <- as.list(x[seq_len(limit),]) 
		}
	return(xll)
	}
}
	#
#
	# pl (province_label)
	if(missing(province)==FALSE) {
	# roman provinces
	#data("rp")
	if(missing(rp)==TRUE) {
#	if(!(exists("rp"))) {
	  utils::data("rp", package="sdam", envir=environment())
	  rp <- get("rp", envir=environment()) 
	} else { 
	  NA
	}
	#
	if(isTRUE(province%in%names(rp))==FALSE) { 
	if(isTRUE(province%in%rp)==FALSE) stop("\"province\" not found. Use \"rp\" argument with province names.")
	  province <- names(rp)[which(rp%in%province)]
	} else { NA }
	#
	xp <- x[`$`(x , province_label)==unlist(rp[which(names(rp)==province)], use.names=FALSE), ]
	xp <- xp[which(!(is.na(xp$province_label))), ]
#	xp <- x[`$`(x , province)==unlist(rp[which(names(rp)==province)], use.names=FALSE), ]
	#return(x[`$`(x , province)==unlist(rp[which(names(rp)==province)], use.names=FALSE), ])
	#} else { xp<-x }
	ifelse(missing(vars)==TRUE, NA, 
	xp <- xp[,-which(!(colnames(xp)%in%c(vars,"id")))]
	)
	} else { 
		xp <- x
	}
	#
	ifelse(isTRUE(na.rm==FALSE)==TRUE, NA, 
#	ifelse(missing(na.rm)==FALSE && isTRUE(na.rm==FALSE)==TRUE, NA, 
	xp<-xp[which(lapply(strsplit(rownames(xp),"[.]"), function(x) {"NA"%in%x})==FALSE),] 
	)
#		NA, xp<-xp[stats::complete.cases(xp), ])
	# gender
	if(missing(gender)==FALSE) {
	xp <- xp[-which(is.na(`$`(xp, gender))), ]
	# clean?
	ifelse(missing(clean)==FALSE && isTRUE(clean==TRUE)==TRUE, return(cln117(xp[-which(`$`(xp, gender)!=gender), ])), 
		return(xp[-which(`$`(xp, gender)!=gender), ])
		)
	} else {
	# return
	if(missing(province)==FALSE || missing(gender)==FALSE) {
if(match.arg(as)=="list") {
		xpl <- apply(xp, 1, function(x) { lapply(as.list(x),"as.vector") })
		names(xpl) <- NULL
	# clean?
	ifelse(missing(clean)==FALSE && isTRUE(clean==TRUE)==TRUE, return(cln117(xpl)), 
		return(xpl) 
		)
} else { 
	# two options clean?
	if(missing(clean)==FALSE && isTRUE(clean==TRUE)==TRUE) {
#	ifelse(missing(clean)==FALSE && isTRUE(clean==TRUE)==TRUE, return(as.data.frame.matrix(as.table(cln117(xp)))), 
#	ifelse(missing(clean)==FALSE && isTRUE(clean==TRUE)==TRUE, return(cln117(xp)), 
	xpc <- cln117(xp)
	if(isTRUE(class(xpc)=="noquote")==TRUE) {
		xpcdf <- as.data.frame.matrix(as.table(xpc))
		return(xpcdf)
		} else {
		return(xpc)
		}
	} else {
		return(xp) 
		}
}
	} else { NA }
#	ifelse(missing(province)==FALSE || missing(gender)==FALSE, return(xp), NA)
	}
	#

#
} else if(isTRUE(is.list(x)==TRUE)==TRUE) {
#	class(x) <- NULL
if(missing(province)==FALSE) warning("\'province\' works with \'x\' as data frame")
	## edhsd is a list of data frames
	if(is.list(x[[1]])==TRUE) {
	ifelse(is.list(x[[1]][[1]])==FALSE, flglv<-TRUE, flglv<-FALSE)
	if(missing(ldf)==FALSE && isTRUE(ldf==TRUE)==TRUE) {
	flglv<-FALSE
	x <- x[[1]]
	warning("For list of data frames only first component is taken.")
	} else { NA }
	} else {
#	NA
	flglv<-FALSE
	}
} else {
	# for "" vars?
	ifelse(isTRUE(is.character(x)==TRUE)==TRUE, x<-eval(parse(text=x)), NA)
}
##


## addID TRUE by default
if(missing(addID)==FALSE && isTRUE(addID==FALSE)==TRUE) {
	if((isTRUE(na.rm==TRUE)==TRUE) | (match.arg(as)=="df")) {
#	if((missing(na.rm)==FALSE && isTRUE(na.rm==TRUE)==TRUE) | (match.arg(as)=="df")) {
	warning("\'addID\' is set to TRUE for \'na.rm\' and data frame output.")
	addID <- TRUE
	} else {
	addID <- FALSE
#	ifelse(isTRUE("id"%in%vars)==TRUE, addID<-TRUE, addID<-FALSE)
	}
} else if(missing(addID)==FALSE && isTRUE(addID==TRUE)==TRUE) {
	addID <-TRUE
} else {
	ifelse(match.arg(as)=="df", addID<-TRUE, addID<-FALSE)
}
#ifelse(missing(addID)==FALSE && isTRUE(addID==TRUE)==FALSE, addID<-FALSE, addID<-TRUE)




## CHECK VARS
#if(missing(vars)==TRUE && (missing(id)==TRUE && (missing(limit)==TRUE))) {
if(missing(vars)==TRUE) {
#	print("LLEGA?")
	flgv <- FALSE
	ifelse(match.arg(as)=="df", flgp<-TRUE, flgp<-FALSE)
	if(match.arg(as)=="list") {
	if(missing(id)==TRUE && missing(limit)==TRUE) {
#	ifelse(isTRUE(flgdf==TRUE)==TRUE, return(as.list(x)), return(x) )
	#####
	if(isTRUE(flgdf==TRUE)==TRUE) {
	edhl <- list() 
	for(k in seq_len(dim(x)[1])) {
	edhll <- vector("list", ncol(x))
	attr(edhll, "names") <- colnames(x)
	for(i in seq_len(ncol(x))) {
	ifelse(isTRUE(length(x[[which(attr(x,"names")==colnames(x)[i])]][[k]])==0)==TRUE,
	edhll[i] <- NA,
	edhll[i] <- x[[which(attr(x,"names")==colnames(x)[i])]][[k]]
	)
	};rm(i)
	edhl[[k]] <- edhll
	};rm(k)
	return(edhl)
	} else {
	return(x)
	}
	#####
	} else {
	  if(missing(id)==FALSE) {
	# support HD
	if(is.character(id)==TRUE && is.na(as.numeric(gsub("HD","",id)))==TRUE) {
	stop("Invalid \"id\".")
	} else {
	ifelse(is.character(id)==TRUE, id<-as.numeric(gsub("HD","",id)), NA)
	}
	ifelse(isTRUE(flgdf==TRUE)==TRUE, return(as.list(x[id])), NA )
#	ifelse(isTRUE(flgdf==TRUE)==TRUE, return(as.list(x[id])), return(x[id]) )
	  } else if(missing(id)==TRUE) {
	NA
	  }
	}
#	stop("Data frame to list transformation not supported."), return(x) )
	} else if(match.arg(as)=="df") {
	# choose id's ONLY CHARACTER YET
	if(missing(id)==FALSE && isTRUE(is.numeric(id)==TRUE)==TRUE) {
#	if(isTRUE(is.numeric(id)==TRUE)==TRUE) {
	  if(isTRUE(length(id)==1)==TRUE) {
		id <- paste0("HD",paste(rep(0,6-nchar(id)),collapse=""),id,sep="")
	  } else {
	tmp <- vector()
	for(i in seq_len(length(id))) {
	tmp <- append(tmp,paste0("HD",paste(rep(0,6-nchar(id[i])),collapse=""),id[i],sep=""))
	}; rm(i)
		id <- tmp
#	stop("1 at the time")
	  }
	}
	ifelse(missing(id)==FALSE && isTRUE(flgdf==TRUE)==TRUE, x<-x[which(x$id%in%id),], NA)
	##
	ifelse(isTRUE(flgdf==TRUE)==TRUE || (missing(ldf)==FALSE && isTRUE(ldf==TRUE)==TRUE), return(x), vars<-unique(names(unlist(x))) )
#	ifelse(isTRUE(flgdf==FALSE)==TRUE, vars<-unique(names(unlist(x))), return(x) )
	}
} else { 
	flgv <- TRUE
	if(is.character(vars)==FALSE) {
	if(isTRUE(isTRUE(is.list(vars)==TRUE)==TRUE) || isTRUE(is.vector(vars)==FALSE)==TRUE) stop("\'vars\' should be a vector or character.")
	}
	# remove 'ids' from variables if found
	ifelse(isTRUE("id"%in%vars)==TRUE, vars<-vars[which(!(vars=="id"))], NA)
#
ifelse(isTRUE("people"%in%vars)==TRUE || any(grepl("people.",vars,fixed=TRUE))==TRUE, 
	flgp<-TRUE, flgp<-FALSE)
#
}
##

## vars or not vars
if(isTRUE(flgv==FALSE)==TRUE) {
if(isTRUE(flgdf==TRUE)==TRUE) {
	xvars <- colnames(x)
} else {
## EDH also needs to be unlisted
	ifelse(is.null(names(x))==TRUE ||  isTRUE(unique(names(x))=="")==TRUE, 
	       xvars<-unique(names(unlist(x))), xvars<-unique(names(x)))
	## later on there will be x$id
#	xvars<-xvars[which(!(xvars=="ID"))] 
}
} else {
	xvars <- vars
}
#ifelse(isTRUE(flgdf==TRUE)==TRUE, xvars <- colnames(x), 
#			          xvars <- unique(names(unlist(x))) )


# list of list?
ifelse(isTRUE(typeof(x[[1]])=="list")==FALSE && isTRUE(flgdf==FALSE)==TRUE, x<-list(x), NA)

##
if(isTRUE(flgv==TRUE)==TRUE) {
if(isTRUE(vars!="people")==TRUE && all(vars %in% xvars)==FALSE) { 
warning(paste("Variable(s)",vars[which(!(vars %in% xvars))],"is/are not present in \"x\" and may be disregarded.",sep=" "))
vars <- vars[which(vars %in% xvars)]
# e.g. people vars but not available
#} else if(isTRUE(vars %in% xvars)==FALSE && isTRUE("people" %in% vars)==FALSE) {
## OJO ESTE...
#} else if((all(vars %in% xvars)==FALSE || isTRUE(vars %in% xvars)==FALSE ) && 
 #         isTRUE(length(unlist(strsplit(xvars,split="people."))) > length(xvars))==FALSE) {
#} else if(isTRUE(vars %in% xvars)==FALSE && isTRUE(length(unlist(strsplit(xvars,split="people."))) > length(xvars))==FALSE) {
} else if(all(vars %in% xvars)==FALSE  && isTRUE(length(unlist(strsplit(xvars,split="people."))) > length(xvars))==FALSE) {
#} else if(isTRUE(vars %in% xvars)==FALSE) {
	warning(paste("Variable(s)",vars[which(!(vars %in% xvars))],"is/are not present in input data.",sep=" "))
} else if(all(vars %in% xvars)==FALSE) {
if(isTRUE(length(vars[which(!(vars %in% xvars))][vars[which(!(vars %in% xvars))]!="people"])>0)==TRUE) {
warning(paste("Variable(s)",paste(vars[which(!(vars %in% xvars))][vars[which(!(vars %in% xvars))]!="people"],collapse=", "),"is(are) not present in \"x\" and might been disregarded.",sep=" "))
}
## take all variables in 'x'
} else {  NA  }
}
## end CHECK VARS



# for limit in df list
if(isTRUE(flgdf==FALSE)==TRUE) {
# choose id's
if(missing(id)==FALSE) { 
	if(is.character(id)==TRUE && is.na(as.numeric(gsub("HD","",id)))==TRUE) {
	stop("Invalid \"id\".")
	} else {
	ifelse(is.character(id)==TRUE, id<-as.numeric(gsub("HD","",id)), NA)
	}
	# remove NULLs in id
	xn <- unique(x)
#	xn <- x
	xn[sapply(lapply(xn, function(x) { x$id }), is.null)] <- NULL
	#
#	if(is.null(xn[which(as.vector(unlist(lapply(xn, `[`, "id")))==sprintf("HD%06d", as.numeric(i)))][[1]]$id)==TRUE) return(NULL)
	#
#	edhlm <- vector("list", length=length(id))
	edhlm <- list()
	for(i in id) {
	if(isTRUE(length(which(as.vector(unlist(lapply(xn, `[`, "id")))==sprintf("HD%06d", as.numeric(i))))>0)==TRUE) {
	if((xn[which(as.vector(unlist(lapply(xn, `[`, "id")))==sprintf("HD%06d", as.numeric(i)))][[1]]$id==sprintf("HD%06d", as.numeric(i)))==FALSE) {
#		edhlm[i] <- xn[i]
		edhlm[length(edhlm)+1L] <- xn[i]
	} else {
#		edhlm[i] <- xn[as.numeric(which(unlist(lapply(xn, `[`, "id"), use.names=FALSE)==sprintf("HD%06d", as.numeric(i))))]
		edhlm[length(edhlm)+1L] <- xn[as.numeric(which(unlist(lapply(xn, `[`, "id"))==sprintf("HD%06d", as.numeric(i))))][1] #if duplicates
	}
	} else {
	ifelse(isTRUE(length(id)==1L)==TRUE, return(NULL), NA)
	}
	};rm(i)
##
} else if(missing(id)==TRUE) {
# limit only if 'id' is not present
  if(missing(limit)==TRUE || (missing(limit)==FALSE && isTRUE(length(x)<limit)==TRUE)) { 
		# 'list main' with no id nor limits
		edhlm <- x
  } else if(missing(limit)==FALSE) { 
	if(isTRUE(length(limit)==1L)==TRUE) {
		edhlm<-x[seq_len(limit)]
	} else {
		edhlm<-x[limit]
#		ifelse(isTRUE(length(x)>max(limit))==TRUE, edhlm<-x[min(limit):length(x)], edhlm<-x[limit])
	}
#		ifelse(isTRUE(length(limit)==1L)==TRUE, edhlm<-x[seq_len(limit)], edhlm<-x[limit])
  }
}
#
}



# treat DATA FRAME INPUTS
#} else if(isTRUE(flgdf==TRUE)==TRUE) {
if(isTRUE(flgdf==TRUE)==TRUE) {
#	ifelse(missing(limit)==FALSE, warning("When \"x\" is a data frame, argument \"limit\" is not available."), NA)
	if(match.arg(as)=="df") {
	# why this?
#	if(isTRUE(flgv==TRUE)==TRUE) {
#	if(missing(vars)==FALSE) {
#	x <- x[which(colnames(x)%in%vars)]
#	} else { NA }
	###### CORREGIR ESTE !!!!! 25-03-2021
	if(missing(id)==FALSE) { 
#	x <- x[which(x$id%in%id),]
	if(any(is.na(do.call(c, lapply(x$id, (function(x) { if (is.null(x) | length(x) == 0) {NA} else { x }  })))))==TRUE) {
	tmpx <- x[!(sapply(x$id, is.null)),]
	pck <- which(unlist(lapply(tmpx$id, function(x) { (as.numeric(paste(strsplit(x,"")[[1]][3:8],collapse=""))) }))%in%id)
	return(tmpx[pck, ])
	} else {
	pck <- which(unlist(lapply(x$id, function(x) { (as.numeric(paste(strsplit(x,"")[[1]][3:8],collapse=""))) }))%in%id)
	return(x[pck, which(colnames(x)%in%c("id",vars))])
	}
#	 
	} else if(missing(limit)==FALSE) {
		if(isTRUE(flgv==TRUE)==TRUE) {
		ifelse(isTRUE(length(limit)==1L)==TRUE, return(head(x[which(colnames(x)%in%c("id",vars))],limit)),
		       return(x[limit,which(colnames(x)%in%c("id",vars))]) )
		} else {
		ifelse(isTRUE(length(limit)==1L)==TRUE, return(head(x,limit)), return(x[limit,]) )
		}
	} #else {
	ifelse(isTRUE(flgv==TRUE)==TRUE, return(x[which(colnames(x)%in%c("id",vars))]), return(x) )
	#}
	# AS LIST FROM DATA FRAME INPUT
	} else if(match.arg(as)=="list") {
#	print("NO LLEGA")
	edhl <- list() 
	for(k in seq_len(dim(x)[1])) {
	edhll <- vector("list", length(vars))
	attr(edhll, "names") <- vars
	for(i in seq_len(length(vars))) {
	ifelse(isTRUE(length(x[[which(attr(x,"names")==vars[i])]][[k]])==0)==TRUE,
	edhll[i] <- NA,
	edhll[i] <- x[[which(attr(x,"names")==vars[i])]][[k]]
	)
	};rm(i)
	edhl[[k]] <- edhll
	};rm(k)
	rm(edhll)
	}
	#
} #else { NA }
#}
# END treat DATA FRAME INPUTS

#   if(isTRUE(flgdf==TRUE)==FALSE) {
#   #edhlm <- x
#   # choose variables
#   # should also checK if 'VARS' is vector
#   if(missing(vars)==FALSE && isTRUE(is.vector(vars)==TRUE)==TRUE) { 
#   # ch vars
#   edhl <- lapply(edhlm, `[`, vars) 
#   #
#   ifelse(missing(na.rm)==FALSE && isTRUE(na.rm==FALSE)==TRUE, 
#   valids <- seq_len(length(edhl)),
#   valids <- which(as.vector(unlist(lapply(edhl, function(x) { all(is.na(as.vector(unlist(x)))) })))==FALSE)
#   )
#   #
#   if(is.null(unlist(edhl))==FALSE) {
#   # <NA>s replace with 'vars'
#   	if(isTRUE(length(unique(names(unlist(edhl))))!=length(vars))==FALSE || 
#   	any(is.na(unique(unlist(lapply(edhl, "names")))))==TRUE ) {
#   	for(k in seq_len(length(edhl))) {
#   	ifelse(any(is.na(names(edhl[[k]])))==FALSE, NA, 
#   	names(edhl[[k]])[which(is.na(names(edhl[[k]])))] <- vars[which(!(vars %in% names(edhl[[k]])))]
#   	)
#   	};rm(k)
#   	} else { NA }
#   # NULL -> <NA>
#   edhl <- lapply(edhl, function(x) {
#       x[sapply(x, is.null)] <- NA
#       return(x)
#   #    return(x)
#   })
#   #
#   } else {
#   #NA  
#   return(NULL)
#   }
#   ##
#   } else if(missing(vars)==TRUE) {
#   	edhl <- edhlm
#   } else {
#   	stop("Argument \'vars\' should be a vector.")
#   }
#   #
#   } else { NA
#   #edhl <- edhlm
#   }



#flgp <- FALSE
if(isTRUE(flgdf==FALSE)==TRUE) {
#edhlm <- x
# choose variables
# should also checK if 'VARS' is vector
if(isTRUE(flgv==TRUE)==TRUE && isTRUE(is.vector(vars)==TRUE)==TRUE) { 
#if(missing(vars)==FALSE && isTRUE(is.vector(vars)==TRUE)==TRUE) { 
# ch vars
edhl <- lapply(edhlm, `[`, vars) 
# names components
edhl <- lapply(edhl, setNames, vars)

# no vars?
if(is.null(unlist(edhl))==TRUE) {
	warning("\"vars\" not in \"x\".")
	return(NULL)
# or vars
} else {
# na remove
ifelse(isTRUE(na.rm==TRUE)==TRUE, 
valids <- which(as.vector(unlist(lapply(edhl, function(x) { all(is.na(as.vector(unlist(x)))) })))==FALSE),
valids <- seq_len(length(edhl))
)
#ifelse(missing(na.rm)==FALSE && isTRUE(na.rm==FALSE)==TRUE, 
#valids <- seq_len(length(edhl)),
#valids <- which(as.vector(unlist(lapply(edhl, function(x) { all(is.na(as.vector(unlist(x)))) })))==FALSE)
#)

if(isTRUE(na.rm==TRUE)==TRUE) {
edhrm <- lapply(lapply(lapply(edhl, names), is.na), all)
if(isTRUE(length(which(unlist(edhrm)==TRUE))>0)==TRUE) {
edhlm <- edhlm[-which(unlist(edhrm)==TRUE)]
edhl <- edhl[-which(unlist(edhrm)==TRUE)]
} else { NA }
# already above
#valids <- which(as.vector(unlist(lapply(edhl, function(x) { all(is.na(as.vector(unlist(x)))) })))==FALSE)
#
} else {
#
#if(is.null(unlist(edhl))==FALSE) {
#   # <NA>s replace with 'vars'
#   	if(isTRUE(length(unique(names(unlist(edhl))))!=length(vars))==FALSE || 
#   	any(is.na(unique(unlist(lapply(edhl, "names")))))==TRUE) {
#   	for(k in seq_len(length(edhl))) {
#   	ifelse(any(is.na(names(edhl[[k]])))==FALSE, NA, 
#   	names(edhl[[k]])[which(is.na(names(edhl[[k]])))] <- vars[which(!(vars %in% names(edhl[[k]])))]
#   	)
#   	};rm(k)
#   	} else { NA }
# NULL -> <NA>
ifelse(any(lapply(edhl, function(z) { length(z[sapply(z, is.null)]) } )>0)==TRUE, flgn<-TRUE, flgn<-FALSE)
#ifelse(lapply(edhl, function(z) { length(z[sapply(z, is.null)]) } )[[1]]>0, flgn<-TRUE, flgn<-FALSE)
edhl <- lapply(edhl, function(x) {
    x[sapply(x, is.null)] <- NA
    return(x)
    })
#
}
#
}

##
} else if(isTRUE(flgv==FALSE)==TRUE) {
#} else if(missing(vars)==TRUE) {
	edhl <- edhlm
	valids <- which(as.vector(unlist(lapply(edhl, function(x) { all(is.na(as.vector(unlist(x)))) })))==FALSE)
} else {
	stop("Argument \'vars\' should be a vector.")
}
#
} else { NA
#edhl <- edhlm
}

## IDS
ids <- lapply(edhlm, function(x) { x$id })


## AS DATA FRAME OUTPUT
if(match.arg(as)=="df") {
#if(match.arg(as)=="df" && (is.na(unlist(edhl))==FALSE)) {
## split df?
ifelse(missing(split)==FALSE && isTRUE(split==TRUE)==TRUE, split<-TRUE, split<-FALSE)
#
### PEOPLE for vars with PEOPLE
if(isTRUE(flgp==TRUE)==TRUE) {
#	ifelse(isTRUE(na.rm==TRUE)==TRUE, 
#	warning("Argument \"na.rm\" is deactivated for variable \"people\"."), NA )
    # with 'x' argument
#	    pnames <- lapply(edhl, "names")[[1]]
	    pnames <- lapply(edhl, "names")
#    if(isTRUE(flgx==TRUE)==TRUE) {
	if(all(is.na(edhl))==FALSE) {
#	    pnames <- lapply(edhl, "names")
	# ONLY PEOPLE OR COMBINED ?
	if(isTRUE(vars=="people")==TRUE) {
#	    edhlp <- edhl[which(!(is.na((pnames))))]
	    edhlp <- edhl[which(!(is.na(unlist(pnames))))]
	      if(isTRUE(length(edhlp)>0)==TRUE) {
	        plbs <- unique(attr(unlist(edhlp),"names"))
	        plbs <- sort(unique(unlist(strsplit(plbs,split="people.")))) 
		plbs <- plbs[which(plbs!="people")]
	        ifelse(isTRUE(addID==TRUE)==TRUE, plbs[1]<-"id", plbs<-plbs[2:length(plbs)])
#		}
	      } else { return(NULL) }
	} else {
	edhlp <- lapply(edhlm, `[`, "people")
	edhlp <- lapply(edhlp, setNames, "people")
	# NULL -> <NA>
	edhlp <- lapply(edhlp, function(x) {
	    x[sapply(x, is.null)] <- NA
	    return(x)
	})

#	if(is.null(unlist(edhlp))==FALSE) {
	## check consistency of 'edhlp'
	# nvars no-people vars
	npvars <- vars[!(vars%in%"people")]
#	npvars <- vars[(vars%in%xvars)]
	ifelse(isTRUE(flgp==TRUE)==TRUE && isTRUE(vars=="people")==FALSE, vars<-c("people",npvars), vars<-npvars)
	ifelse(isTRUE(flgv==FALSE)==TRUE, npvars<-npvars[which(grepl("people.",npvars,fixed=TRUE)==FALSE)], NA)
#	ifelse(isTRUE(flgp==TRUE)==TRUE, vars<-c("people",npvars), vars<-npvars)
#	if(isTRUE(length(unique(names(unlist(edhlp))))!=length(npvars))==FALSE || 
#	any(is.na(unique(unlist(lapply(edhlp, "names")))))==TRUE ) {
#	for(k in seq_len(length(edhlp))) {
#	ifelse(any(is.na(names(edhlp[[k]])))==FALSE, NA, 
#	names(edhlp[[k]])[which(is.na(names(edhlp[[k]])))] <- npvars[which(!(npvars %in% names(edhlp[[k]])))]
#	)
#	};rm(k)
#	} else { NA }
	## check consistency of 'edhlp'
#	} else { NA }
#
	if(isTRUE(flgv==TRUE)==TRUE) {
	edhlq <- lapply(edhlm, `[`, sort(vars[which(vars!="people")]))
	edhlq <- lapply(edhlq, setNames, sort(vars[which(vars!="people")]))
	} else {
	edhlq <- lapply(edhlm, `[`, npvars)
	edhlq <- lapply(edhlq, setNames, npvars)
	}
	# NULL -> <NA>
	edhlq <- lapply(edhlq, function(x) {
	    x[sapply(x, is.null)] <- NA
	    return(x)
	})
	## check consistency of 'edhlq'
	if(is.null(unlist(edhlq))==FALSE) {
	# <NA>s replace with 'no-people-vars'
#	npvars <- vars[(vars%in%xvars)]
	if(isTRUE(length(unique(names(unlist(edhlq))))!=length(npvars))==FALSE || 
	any(is.na(unique(unlist(lapply(edhlq, "names")))))==TRUE ) {
	for(k in seq_len(length(edhlq))) {
	ifelse(any(is.na(names(edhlq[[k]])))==FALSE, NA, 
	names(edhlq[[k]])[which(is.na(names(edhlq[[k]])))] <- npvars[which(!(npvars %in% names(edhlq[[k]])))]
	)
	};rm(k)
	} else { NA }
	}
	## end check consistency of 'edhlq'
#	
	      if(isTRUE(length(edhlp)>0)==TRUE && is.null(unlist(edhlp))==FALSE) {
#	      if(isTRUE(length(edhlp)>0)==TRUE) {
	        plbs <- unique(attr(unlist(edhlp),"names"))
	        plbs <- sort(unique(unlist(strsplit(plbs,split="people.")))) 
	        ifelse(isTRUE(addID==TRUE)==TRUE, plbs[1]<-"id", plbs<-plbs[2:length(plbs)])
	      } else { 
	      plbs <- vector()
	      #NA 
	      }
#
	      if(isTRUE(length(edhlq)>0)==TRUE && is.null(unlist(edhlq))==FALSE) {
#	      if(isTRUE(length(edhlq)>0)==TRUE) {
		qlbs <- sort(unique(unlist(lapply(edhlq, "names"))), na.last=TRUE)
		ifelse(isTRUE(addID==TRUE)==TRUE, qlbs<-append("id",qlbs), NA)
	      } else { 
	      qlbs <- vector()
	      #NA 
	      }
#
	}
	# end ONLY PEOPLE OR COMBINED
	## when 'edhl' is NULL
	} else {
		return(as.data.frame(edhl))
	}
##

## IDS
#ids <- lapply(edhl, function(x) { x$id })
#    ids <- vector()
#    for(i in seq_len(length(edhl))) {
#	ids <- append(ids, edhlm[[i]]$id)
#    }; rm(i)
    ids <- ids[which(is.na(pnames)==FALSE)]
##

## CREATE DATA FRAME STRUCTURE
#
if(match.arg(type)=="long") {
#
# OPTION 1: PEOPLES IN ROWS
#### optimize PEOPLE (not working)
if(isTRUE(length(edhlp)>0)==TRUE && is.null(unlist(edhlp))==FALSE) {
#
xdfp <- data.frame(matrix(ncol=length(plbs), nrow=0))
colnames(xdfp) <- plbs
#
for(k in seq_len(length(edhlp))[valids]) {
if(is.null(unlist(edhlp[[k]]))==FALSE && is.na(unlist(edhlp[[k]]))==FALSE) {
#if(is.na(unlist(edhlp[[k]]))==FALSE) {
#if(is.null(unlist(edhlp[[k]]))==FALSE) {
tmpdf <- data.frame(matrix(ncol=length(plbs), nrow=0))
colnames(tmpdf) <- plbs
  for(i in seq_len(length(edhlp[[k]]$people))) {
	edhlp[[k]]$people[[i]] <- edhlp[[k]]$people[[i]][order(names(edhlp[[k]]$people[[i]]))]
	edhlp[[k]]$people[[i]][lengths(edhlp[[k]]$people[[i]]) == 0L] <- NA
	tmpdf[i, which((plbs%in%attr(edhlp[[k]]$people[[i]],"names")))] <- as.vector(unlist(edhlp[[k]]$people[[i]]))
  }; rm(i)
	ifelse(isTRUE(addID==TRUE)==TRUE, tmpdf[,1]<-ids[k], NA)
#
	xdfp <- rbind(xdfp, tmpdf)
#
} else { NA }
}; rm(k)
#xdfp
#
# select
if(missing(select)==FALSE) {
	xdfp <- subset(xdfp, select=c("id",select[which(select%in%plbs)]))
#	xdfp <- subset(xdfp, select=c("id",select))
} else { NA }
#
} else {
	ifelse(isTRUE(vars=="people")==TRUE, return(NULL), NA)
}
#

##
if(isTRUE(vars=="people")==TRUE) {
	###
	if(isTRUE(na.rm==TRUE)==TRUE) {
		if(isTRUE(nrow(xdfp)!=0)==TRUE && isTRUE(addID==TRUE)==TRUE) {
		#	ifelse(isTRUE(addID==TRUE)==FALSE, xdfp<-xdfp[which(apply(xdfp, 1, function(x){all(is.na(x))})==FALSE),], 
			xdfp <- xdfp[which(apply(xdfp[2:ncol(xdfp)], 1, function(x){all(is.na(x))})==FALSE),] 
		#)
		} else { return(NULL) }
	} else { NA }
	###
	if(isTRUE(split==TRUE)==TRUE) {
		return(split(xdfp[,-1], xdfp$id))
	} else {
	return(xdfp)
	}
# people and more vars
} else {
#
# for NO-PEOPLE
if(isTRUE(length(edhlq)>0)==TRUE) {
	if(isTRUE(length(valids)==1)==TRUE) {
	edhlq <- as.list(unlist(edhlq[valids]))
	xdfq <- data.frame(matrix(unlist(edhlq), ncol=length(edhlq), byrow=TRUE))
	    colnames(xdfq) <- names(edhlq) 
	} else {
	edhlq <- edhlq[valids]
	# no need to deal with colnames
	xdfq <- as.data.frame(do.call(rbind,lapply(edhlq, `length<-`, max(lengths(edhlq)))), stringsAsFactors=TRUE)
	}
#
#if(isTRUE(na.rm==TRUE)==TRUE) {
#	rmids <- which(rowSums(is.na(xdfq))!=ncol(xdfq))
#	xdfq <- xdfq[rmids,]
#	if(isTRUE(addID==TRUE)==TRUE) {
#	xdfq$id<-unlist(ids[rmids])
#	xdfq <- rev(xdfq)
#	} else { NA }
#} else {
	# add id
	if(isTRUE(addID==TRUE)==TRUE) {
		  xdfq$id <- ids[valids]
	ifelse(isTRUE(flgv==TRUE)==TRUE, xdfq <- rev(xdfq), NA)
#		  xdfq <- rev(xdfq)
		} else {  NA  }
##
#}
#
} else {
	ifelse(isTRUE("people"%in%vars==FALSE)==TRUE, return(NULL), NA)
}
# END for NO-PEOPLE
#
if(isTRUE(length(edhlq)>0)==TRUE && (isTRUE(length(edhlp)>0)==TRUE && is.null(unlist(edhlp))==FALSE)) {
#
	tmpxdfp <- xdfp
	tmpxdfp$id <- sub("[[:alpha:]]+","",xdfp$id)
	tmpxdfq <- xdfq
	tmpxdfq$id <- sub("[[:alpha:]]+","",xdfq$id)
	# THIS IS A PROBLEM ids not correct
	xdfpq <- merge(tmpxdfp, tmpxdfq, all=TRUE)
#	xdfpq <- merge(xdfp, xdfq, all.x=TRUE)
	xdfpq$id <- paste("HD",xdfpq$id, sep="")
	# NA to <NA>
#	xdfpq[is.na(xdfpq)] <- NA
#
###
	###
	if(isTRUE(na.rm==TRUE)==TRUE) {
		if(isTRUE(nrow(xdfpq)!=0)==TRUE && isTRUE(addID==TRUE)==TRUE) {
		#	ifelse(isTRUE(addID==TRUE)==FALSE, xdfpq<-xdfpq[which(apply(xdfpq, 1, function(x){all(is.na(x))})==FALSE),], 
			xdfpq <- xdfpq[which(apply(xdfpq[2:ncol(xdfpq)], 1, function(x){all(is.na(x))})==FALSE),] 
		#)
		} else { return(NULL) }
	} else { NA }
	###
	###
	if(isTRUE(na.rm==TRUE)==TRUE) {
		if(isTRUE(nrow(xdfq)!=0)==TRUE && isTRUE(addID==TRUE)==TRUE) {
		#	ifelse(isTRUE(addID==TRUE)==FALSE, xdfq<-xdfq[which(apply(xdfq, 1, function(x){all(is.na(x))})==FALSE),], 
			xdfq <- xdfq[which(apply(xdfq[2:ncol(xdfq)], 1, function(x){all(is.na(x))})==FALSE),] 
		#)
		} else { return(NULL) }
	} else { NA }
	###
###
#
	if(isTRUE(split==TRUE)==TRUE) {
		return(split(xdfpq[,-1], xdfpq$id))
	} else {
		return(xdfpq)
	}
#
} else {
	if(isTRUE(length(plbs)>0)==TRUE) {
		if(isTRUE(split==TRUE)==TRUE) {
			return(split(xdfp[,-1], xdfp$id))
		} else {
		return(xdfp)
		}
	} else if(isTRUE(length(qlbs)>0)==TRUE) {
#if(isTRUE(na.rm==TRUE)==TRUE) {
#	ifelse(isTRUE(addID==TRUE)==TRUE,
#	xdfq<-xdfq[which(!(is.na(xdfq[,2:length(xdfq)]))),], 
#	xdfq<-xdfq[which(!(is.na(xdfq))),] )
#} else { NA }
		if(isTRUE(split==TRUE)==TRUE) {
			return(split(xdfq[,-1], xdfq$id))
		} else {
		return(xdfq)
		}
	} else {
		return(NULL)
	}
}
#
}
#
} else if(match.arg(type)=="wide") {
ifelse(isTRUE(flgp==FALSE)==TRUE, NA, edhlp <- edhlp[valids])
ifelse(isTRUE(vars=="people")==TRUE, NA, edhlq <- edhlq[valids])
ifelse(isTRUE(addID==FALSE)==TRUE, NA, ids <- ids[valids])

# OPTION 2: PEOPLES IN COLUMNS
#### OPTIMIZE wide FORMAT  ####
if(isTRUE(length(edhlp)>0)==TRUE && is.null(unlist(edhlp))==FALSE) {

#
pp <- max(as.numeric(unlist(edhlm)[which(attr(unlist(edhlm),"names")=="people.person_id")]))
plbss <- vector()
for(i in seq_len(pp)) {
	plbss <- append(plbss, paste(plbs[2:length(plbs)],i,sep=""))
};rm(i)
ifelse(isTRUE(addID==TRUE)==TRUE, plbss<-append("id",plbss), NA)
#

#### OPTIMIZE
options(stringsAsFactors = FALSE)
xdfpp <- data.frame(matrix(ncol=length(plbss), nrow=0))
#colnames(xdfpp) <- plbss
##
for(k in seq_len(length(edhlp))) {
#
if(isTRUE(is.na(edhlp[[k]]$people)==TRUE)==TRUE) {
	NA 
} else {
#
tmpdf <- data.frame(matrix(ncol=length(plbs), nrow=0))
colnames(tmpdf) <- plbs
  for(i in seq_len(length(edhlp[[k]]$people))) {
#	if(is.na(edhlp[[k]]$people)==FALSE) {
	edhlp[[k]]$people[[i]] <- edhlp[[k]]$people[[i]][order(names(edhlp[[k]]$people[[i]]))]
	edhlp[[k]]$people[[i]][lengths(edhlp[[k]]$people[[i]]) == 0L] <- NA
#	tmpdf[i, which((plbss%in%attr(edhlp[[k]]$people[[i]],"names")))] <- as.vector(unlist(edhlp[[k]]$people[[i]]))
	tmpdf[i, which((plbs%in%attr(edhlp[[k]]$people[[i]],"names")))] <- as.vector(unlist(edhlp[[k]]$people[[i]]))
#	} else { NA }
  }; rm(i)
#
if(isTRUE(nrow(tmpdf)>1L)==TRUE) {
vecp <- vector()
for(i in seq_len(nrow(tmpdf))) {
	vecp <- append(vecp, unlist(tmpdf[i,2:ncol(tmpdf)], use.names=FALSE))
};rm(i)
ifelse(isTRUE(dim(tmpdf)[1]==pp)==TRUE, vecpp<-c(as.vector(unlist(ids))[k], vecp),
	vecpp<-c(as.vector(unlist(ids))[k], vecp, rep(NA,(length(plbss)-length(vecp)-1L))) )
xdfpp <- (rbind(xdfpp,vecpp))
#xdfpp <- suppressWarnings(rbind(xdfpp,vecpp))
} else if(isTRUE(nrow(tmpdf)==1L)==TRUE) {
	if(isTRUE(dim(xdfpp)[1]==0)==TRUE) {
	xdfpp <- rbind(as.vector(unlist(xdfpp)), c(as.vector(unlist(ids))[k],as.vector(unlist(tmpdf[2:ncol(tmpdf)])),rep(NA,(length(plbss)-ncol(tmpdf)))))
#	colnames(xdfpp) <- plbss
	} else {
	xdfpp <- (rbind(xdfpp, c(as.vector(unlist(ids))[k],as.vector(unlist(tmpdf[2:ncol(tmpdf)])),rep(NA,(length(plbss)-ncol(tmpdf))))))
	}
}
#
} 
#
}; rm(k)
#
	xdfpp <- as.data.frame(xdfpp)
	colnames(xdfpp) <- plbss
	rownames(xdfpp) <- NULL
	## select
	if(missing(select)==FALSE) {
		colnames(xdfpp) <- c("id",sub("[[:digit:]]+$","",plbss[2:length(plbss)]))
		xdfpp <- xdfpp[,c(1,which(colnames(xdfpp)%in%select))]
	#	xdfpp <- subset(xdfpp, select=c("id",select[which(select%in%sub("[[:digit:]]+$","",plbss[2:length(plbss)]))]))
	} else { NA }
#
} else {
	ifelse(isTRUE(vars=="people")==TRUE, return(NULL), NA)
}
###
if(isTRUE(vars=="people")==TRUE) {
	if(isTRUE(split==TRUE)==TRUE) {
		return(split(xdfpp[,-1], xdfpp$id))
	} else {
		return(xdfpp)
	}
#} else if(isTRUE(vars!="people")==TRUE) {
} else {
#
#if(isTRUE(length(edhlq)>0)==TRUE && (isTRUE(length(edhlp)>0)==TRUE && is.null(unlist(edhlp))==FALSE)) {
if(isTRUE(length(edhlq)>0)==TRUE) {
#
	  xdfq <- data.frame(matrix(ncol=length(as.vector(stats::na.omit(qlbs))), nrow=length(edhlq))) 
	  colnames(xdfq) <- as.vector(stats::na.omit(qlbs))
#	  xdfq <- data.frame(matrix(ncol=length(qlbs), nrow=length(edhlq))) #
#	  colnames(xdfq) <- qlbs
#
	  for(i in seq_len(length(edhlq))) {
		edhlq[[i]] <- edhlq[[i]][order(names(edhlq[[i]]))]
		edhlq[[i]][lengths(edhlq[[i]]) == 0L] <- NA
		xdfq[i,2:ncol(xdfq)] <- as.vector(unlist(edhlq[[i]]))
#		xdfq[i, ] <- as.vector(unlist(edhlq[[i]]))
#		xdfq[i, which((qlbs%in%attr(edhlq[[i]],"names")))] <- as.vector(unlist(edhlq[[i]]))
	  }; rm(i)
#
		ifelse(isTRUE(addID==TRUE)==TRUE, xdfq[,1]<-as.vector(unlist(ids)), NA)
}
#
if(isTRUE(length(plbs)>0)==TRUE && isTRUE(length(qlbs)>0)==TRUE) {
#
	tmpxdfpp <- xdfpp
	tmpxdfpp$id <- sub("[[:alpha:]]+","",xdfpp$id)
	tmpxdfq <- xdfq
	tmpxdfq$id <- sub("[[:alpha:]]+","",xdfq$id)
#
	xdfpq <- merge(tmpxdfpp, tmpxdfq, all=TRUE)
	xdfpq$id <- paste("HD",xdfpq$id, sep="")
#
#	xdfpq <- merge(xdfpp, xdfq, all.x=TRUE)
	#
	if(isTRUE(split==TRUE)==TRUE) {
		return(split(xdfpq[,-1], xdfpq$id))
	} else {
		return(xdfpq)
	}
} else if(isTRUE(length(plbs)>0)==TRUE) {
	if(isTRUE(split==TRUE)==TRUE) {
		return(split(xdfpp[,-1], xdfpp$id))
	} else {
	return(xdfpp)
	}
} else if(isTRUE(length(qlbs)>0)==TRUE) {
	if(isTRUE(split==TRUE)==TRUE) {
		return(split(xdfq[,-1], xdfq$id))
	} else {
	return(xdfq)
	}
} else { print("w") }
#
#
}
###
} else if(match.arg(type)=="narrow") {
	print("Argument \'type\' *narrow* is not yet implemented.")
}
#
}
##### REVISAdo
# NO people
if(isTRUE(flgp==FALSE)==TRUE) {
### IDS
#  functional programming = good
#ids <- lapply(edhlm, function(x) { x$id })
#
#   edhlq <- lapply(edhlm, `[`, sort(c("id",vars[which(vars!="people")])))
   edhlq <- lapply(edhlm, `[`, sort(vars[which(vars!="people")]))
   edhlq <- lapply(edhlq, setNames, sort(vars[which(vars!="people")]))
   # check if df is 1 column
#    ifelse(any(lapply(edhlq, function(z) { length(z[sapply(z, is.null)]) } )>0)==TRUE, flgn<-TRUE, flgn<-FALSE)
   ifelse(lapply(edhlq, function(z) { length(z[sapply(z, is.null)]) } )[[1]]>0, flgn<-TRUE, flgn<-FALSE)
# NULL -> <NA>
	if(isTRUE(flgn==FALSE)==TRUE) {
	edhlq <- lapply(edhlq, function(x) {
	    x[sapply(x, is.null)] <- NA
	    return(x)
	})
	} else { NA }
#   edhlq <- lapply(edhlq, function(x) x[!is.na(x)])
	# list names when <NA>
	for(k in seq_len(length(edhlq))) {
	ifelse(any(is.na(names(edhlq[[k]])))==FALSE, NA, 
	names(edhlq[[k]])[which(is.na(names(edhlq[[k]])))] <- vars[which(!(vars %in% names(edhlq[[k]])))]
	)
	};rm(k)
#
if(isTRUE(length(edhlq)>0)==TRUE) {
	if(isTRUE(flgn==TRUE)==TRUE) {
	if(isTRUE(length(edhlq)==1)==TRUE && isTRUE(length(edhlq[[1]])==2)==TRUE) {
	# HERE WAS A PROBLEM!!!! when???
	tmpq <- lapply(edhlq, function(z) { (z[!(sapply(z, is.null))]) } )
	xdfq <- data.frame(matrix(unlist(tmpq), ncol=max(length(tmpq)), dimnames=list(NULL,names(tmpq[[1]]))))
	} else {
	# NO NEED FOR COLUMN NAMES (BUT FOR edhsd$Arm!!!)
	xdfq <- as.data.frame(do.call(rbind,lapply(edhlq, `length<-`, max(lengths(edhlq)))))
	}
	} else {
#	xdfq <- do.call(rbind, lapply(edhlq, as.data.frame))
	#### PROBLEM ###### valids=1
#	ifelse(missing(ldf)==FALSE && isTRUE(ldf==TRUE)==TRUE, 
	ifelse(isTRUE(flglv==TRUE)==FALSE, 
	xdfq <- data.frame(matrix(unlist(edhlq), ncol=max(lengths(edhlq)), byrow=FALSE)), #cors
	xdfq <- data.frame(matrix(unlist(edhlq), ncol=max(lengths(edhlq)), byrow=TRUE)) ) #iud
#	xdfq <- data.frame(matrix(unlist(edhlq), ncol=max(lengths(edhlq)), byrow=FALSE))
#	xdfq <- data.frame(matrix(unlist(edhlq), ncol=max(lengths(edhlq)), byrow=TRUE))
	qlbs <- sort(unique(unlist(lapply(edhlq, "names"))), na.last=TRUE)
	    colnames(xdfq) <- as.vector(stats::na.omit(qlbs))
	}
#
#	if(any(which(rowSums(is.na(xdfq))!=ncol(xdfq))==TRUE) && 
#	if(isTRUE(na.rm==TRUE)==TRUE) {
#	rmids <- which(rowSums(is.na(xdfq))!=ncol(xdfq))
#	xdfq <- xdfq[rmids,]
#	if(isTRUE(addID==TRUE)==TRUE) {
#	xdfq$id<-unlist(ids[rmids])
#	xdfq <- rev(xdfq)
#	} else { NA }
##	xdfq <- stats::na.omit(xdfq)
#	} else {
#
	if(isTRUE(addID==TRUE)==TRUE) {
		# otherwise unlist produces different lengths
		  ids[sapply(ids, is.null)] <- NA
		  xdfq$id <- as.vector(unlist(ids))
		  ifelse(isTRUE(flgv==TRUE)==TRUE, xdfq <- rev(xdfq), NA)
#		  xdfq <- rev(xdfq)
		} else {
		NA
		}
#
#	}
#
} else { 
return(NULL)
}
#
	if(isTRUE(split==TRUE)==TRUE) {
		return(split(xdfq[,-1], xdfq$id))
	} else {
		return(xdfq)
	}

}

### LIST
} else if(match.arg(as)=="list") {
##	OPTIMIZE
#ifelse(isTRUE(na.rm==TRUE)==TRUE, edhl1<-edhl[valids], edhl1<-edhl)
#	edhl1 <- edhl
#	for(n in seq_len(length(edhl))) {
#		ifelse(any(is.na(attr(edhl[[n]],"names")))==FALSE, NA, 
#		attr(edhl1[[n]],"names")[which(is.na(attr(edhl[[n]],"names")))] <- vars[which(is.na(attr(edhl[[n]],"names")))]
#		)
#	}; rm(n)
##
#	edhlp <- lapply(edhlm, `[`, "people")
if(isTRUE(flgp==TRUE)==TRUE && isTRUE(flgv==TRUE)==TRUE) {
#if(isTRUE(flgp==TRUE)==TRUE) {
#
	if(missing(select)==FALSE) {
#	warning("Option \"select\" not yet implemented for \"list\" type.")
	edhlp <- lapply(edhlm, `[`, "people")
#	edhlp <- lapply(edhl, `[`, "people")
#	edhlp <- lapply(edhl1, `[`, "people")
#	edhlp <- lapply(edhl1, function(x) { `$`(x,people) })
#
	if(isTRUE(na.rm==TRUE)==TRUE) {
	ids <- ids[which(as.vector(unlist(lapply(edhlp, function(x) all(is.na(x)))))==FALSE)]
	edhlp0 <- Filter(function(x) !all(is.na(x)), edhlp)
	} else { 
		edhlp0 <- edhlp
	}
#
	edhl1 <- vector("list", length=length(edhlp0))
	for(k in seq_len(length(edhlp0))) {
	tmpsl <- lapply(edhlp0[[k]]$people,`[`, select)
	# <NA>s replace with 'select'
	for(j in seq_len(length(tmpsl))) {
		ifelse(any(is.na(names(tmpsl[[j]])))==FALSE, NA, 
		names(tmpsl[[j]])[which(is.na(names(tmpsl[[j]])))] <- select[which(!(select %in% names(tmpsl[[j]])))] )
	};rm(j)
	# NULL -> <NA>
	tmpsl <- lapply(tmpsl, function(x) {
	    x[sapply(x, is.null)] <- NA
	    return(x)
	})
	edhl1[[k]] <- tmpsl
	};rm(k)
#
	} else {
	edhl1 <- lapply(edhlm, `[`, "people")
	}
##
	if(isTRUE(vars=="people")==TRUE) {
	#ifelse(isTRUE(na.rm==TRUE)==TRUE, edhl2 <- Filter(function(x) !all(is.na(x)), edhl1), edhl2 <- edhl1 )
	edhl2 <- Map(c, people=edhl1)
#
} else if(isTRUE("people"%in%vars)==TRUE) { 
	# no-people vars
	edhlq <- lapply(edhlm, `[`, sort(vars[which(vars!="people")]))
	# NULL -> <NA>
	#edhlq <- lapply(edhlq, function(x) {
	#    x[sapply(x, is.null)] <- NA
	#    return(x)
	#})
	ifelse(isTRUE(na.rm==TRUE)==TRUE, edhlq<-Filter(function(x) !all(is.na(x)), edhlq), NA)
	edhl2 <- suppressWarnings(Map(c, people=edhl1, edhlq))
} else {
	edhl2 <- lapply(edhlm, `[`, sort(vars[which(vars!="people")]))
	ifelse(isTRUE(na.rm==TRUE)==TRUE, edhl2<-Filter(function(x) !all(is.na(x)), edhl2), NA)
#	edhl2 <- edhlq
	#edhl2 <- Map(c, edhl1, edhlq)
}
#
}
##

# clean?
if(missing(clean)==FALSE && isTRUE(clean==TRUE)==TRUE) {
	ifelse(isTRUE(flgp==FALSE)==TRUE, 
#	NA, edhl2 <- cln117(edhl2)
	edhl <- cln117(edhl), edhl2 <- cln117(edhl2)
	)
}

##
if(isTRUE(addID==TRUE)==TRUE) {
	ifelse(isTRUE(flgp==FALSE)==TRUE, 
	return(Map(c, id=ids, edhl)), 
	return(Map(c, id=ids, edhl2))
	)
#	ifelse(isTRUE(na.rm==TRUE)==TRUE, 
#	return(Map(c, id=ids[valids], edhl2)), return(Map(c, id=ids, edhl2)) )
#	edhl2 <- Map(c, id=ids, Map(c, people=edhl1, edhlq))
#	return(Map(c, id=ids, people=edhl1, edhlq))
} else {
	if(isTRUE(flgp==FALSE)==TRUE) {
	ifelse(isTRUE(length(edhl)==1)==TRUE, return(edhl[[1]]), return(edhl))
	} else {
	ifelse(isTRUE(length(edhl2)==1)==TRUE, return(edhl2[[1]]), return(edhl2))
	}
#	ifelse(isTRUE(flgp==FALSE)==TRUE, 
#	return(edhl), 
#	return(edhl2)
#	)
##	edhl2 <- Map(c, people=edhl1, edhlq)
##	return(Map(c, people=edhl1, edhlq))
}
#ifelse(isTRUE(addID==TRUE)==TRUE, return(Map(c, id=ids, people=edhl1, edhlq)), return(Map(c, people=edhl1, edhlq))) 



#	if(isTRUE(na.rm==TRUE)==TRUE) {
#	edhl0 <- list()
#	for(n in seq_len(length(edhl))) {
#	  if(isTRUE(flgp==TRUE)==TRUE) {
#	    if(isTRUE(addID==TRUE)==TRUE) {
#	        ifelse(is.null(edhl[[n]]$people)==FALSE, edhl0[[length(edhl0)+1L]]<-c(edhlm[[n]]$id,edhl[[n]]$people), NA)
#	    } else {
#	        ifelse(is.null(edhl[[n]]$people)==FALSE, edhl0[[length(edhl0)+1L]]<-edhl[[n]]$people, NA)
#	    }
#	  } else {
#	    ifelse(any(is.na(attr(edhl[[n]],"names")))==FALSE, edhl0[[length(edhl0)+1L]]<-edhl[[n]], NA)
#	#edhlm[[k]]$id
#	  }
#	}; rm(n)
#	return(edhl0)
#	# na.rm=F
#	} else {
#	edhl1 <- edhl
#	for(n in seq_len(length(edhl))) {
#	ifelse(any(is.na(attr(edhl[[n]],"names")))==FALSE, NA, 
#	attr(edhl1[[n]],"names")[which(is.na(attr(edhl[[n]],"names")))] <- vars[which(is.na(attr(edhl[[n]],"names")))]
#	)
#	}; rm(n)
#	return(edhl1)
#	}
## PLACE FOR FUTURE output FORMATS?
} else { NA }
##

}
