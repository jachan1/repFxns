#' @title KM Curves with ggplot
#'
#'
#' generic function to create a ggplot kaplan meier curve
#' @param s survival object
#' @param CI include confidence interval (default TRUE)
#' @param plot.cens plot marks to indicate a censored subject (default TRUE)
#' @param surv.col colors to use for survival curves. Should match number of groups.
#' @param cens.col color of censor marks
#' @param cens.shape shape of censor marks
#' @param lty.est line type for survival curve
#' @param lty.ci line type for confidence interval
#' @param xlab label for x axis (default 'Time')
#' @param ylab label for y axis (default '')
#' @param main title for plot (default '')
#' @param cumProb plot cumulative probability instead of survival (default FALSE)
#' @param yTicks number of ticks on the y axis
#' @param addCounts add number still at risk above the x axis. This is an attempt to replicate SAS behavior. It does not always give good results (default FALSE)
#' @param bw print in black and white (default FALSE)
#' @param legend_title title to use for the legend
#' @param legend_pos where to position the legend
#' @keywords survival ggplot
#' @export
#' @examples
#' require(survival)
#' sv1 <- with(ovarian, Surv(futime, fustat))
#' 
#' ## fit intercept only survival
#' sf1 <- survfit(sv1 ~ 1, ovarian)
#' ## basic km curve
#' ggsurv(sf1)
#' ## cumulative probability instead of survival
#' ggsurv(sf1, cumProb=T)
#' ## add in survival counts
#' ggsurv(sf1, cumProb=T, addCounts=T)
#' 
#' ## fit model with two strata
#' sf2 <- survfit(sv1 ~ resid.ds, ovarian)
#' ## basic km curve
#' ggsurv(sf2, CI=F)
#' ## add in confidence intervals
#' ggsurv(sf2, CI=T)
#' ## cumulative probability instead of survival
#' ggsurv(sf2, cumProb=T)
#' ## add in survival counts
#' ggsurv(sf2, cumProb=T, addCounts=T)

ggsurv <- function(s, ...){
  UseMethod("ggsurv")
}

#' @rdname ggsurv_m
#' @title ggsurv multi level helper
#' @export

ggsurv_m <- function(s, strata, yAxisScale, legend_title, legend_pos, starter, 
                     CI, plot.cens, surv.col, cens.col, lty.est, lty.ci, 
                     cens.shape, xlab, ylab, main, cumProb, yTicks, dataLabels, 
                     addCounts, count_size, bw, strata_names, cens.col.match, 
                     cens.seq, cens.cnt = 6, ...) {
  nticks <- seq(0,1, length.out=yTicks)
  nlabs <- paste0(100*nticks, "%")
  yAxisScale <- scale_y_continuous(limits=c(ifelse(addCounts, -0.125 - 0.065*(strata-1), -0.025),1.1), breaks=nticks, labels = nlabs)
 
  ## given default informative y axis label
  if(ylab == ""){
    ylab = ifelse(cumProb, "Cumulative Probability", "Survival")
  }
  
  gr.name <- if(is.na(legend_title)) {
    ifelse("survfit" %in% class(s), unlist(strsplit(names(s$strata), '='))[1], NA)
  } else {
    gr.name = legend_title
  }
  
  gr.df <- starter(s, legend_title)
  
  ## helper functions
  makecnts <- function(x){
    x$count_lab[nrow(x)] <- x$atRisk[nrow(x)]
    x$count_lab_x[nrow(x)] <- as.numeric(as.character(x$time_max[nrow(x)]))
    x$count_lab_y <- -0.08 - 0.065*(as.numeric(x$group) - 1)
    x
  }
  
  makezeros <- function(x) {
    x$lablab <- NA
    x$lablaby <- NA
    x$lablab[1] <- paste0(as.numeric(x$group[1]), ":")
    x$lablaby[1] <- -0.08 - 0.065*(as.numeric(x$group[1]) - 1)
    
    x$count_lab[1] <- x$atRisk[1]
    x$count_lab_x[1] <- 0
    x
  }
  time_range <- range(unlist(lapply(gr.df, "[[", "time")))
  if(missing(cens.seq)) cens.seq <- seq(time_range[1], time_range[2], length.out = cens.cnt)
  dat <- bind_rows(gr.df) %>% mutate(group=factor(group)) %>%
    mutate(time_grp = cut(time, cens.seq, include.lowest = T), 
           time_max = factor(as.numeric(time_grp), levels=1:length(levels(time_grp)), labels=cens.seq[-1]),
           count_lab=NA, count_lab_x=NA, count_lab_y=NA) %>% 
    group_by(group, time_grp) %>% arrange(time) %>% do(makecnts(.)) %>% 
    group_by(group) %>% do(makezeros(.)) %>% ungroup()
  ## time_grps helpful for adding counts
  time_grps <- with(dat, seq(min(time), max(time), length.out = 6))
  ## dat needs to have addcounts info added before ggplot first called
  if(addCounts) {
    dat$group <- factor(sprintf("%g: %s", as.numeric(dat$group), dat$group))
    tmp <- dat %>%
      filter(time > 0) %>% 
      with(table(group, time_grp)) %>%
      as.data.frame() %>% 
      filter(Freq == 0) 
    allrows <- NULL
    if(nrow(tmp) > 0){
      levels(tmp[['time_grp']]) <- time_grps[-1]
      for(i in 1:nrow(tmp)){
        time_cut <- as.numeric(as.character(tmp[['time_grp']][i]))
        allrows <- bind_rows(allrows,
                             dat %>% 
                               filter(group == as.character(tmp[['group']][i])) %>%
                               filter(time == max(ifelse(time <= time_cut, time, -Inf))) %>% 
                               mutate(time = time_cut, count_lab = atRisk, count_lab_x = time_cut))
      }
    }
    dat <- bind_rows(allrows, dat) %>% 
      arrange(group, time) %>% 
      mutate(count_lab = ifelse(is.na(lablab), paste(count_lab), paste(lablab, count_lab)))
  }
  
  ## initial plot created
  pl <- dat %>%
    filter(is.na(lablab)) %>%
    ggplot(aes(x = time, y = surv, group = group)) +
    xlab(xlab) + ylab(ylab) + ggtitle(main) +
    yAxisScale +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  if(bw){
    pl <- pl + geom_step(aes(lty = group))
  } else {
    pl <- pl + geom_step(aes(col = group, lty = group))
  }
  
  ## counts added
  if(addCounts) {
    pl <- pl + geom_text(data=dat[!is.na(dat$count_lab_x), ], aes(x= count_lab_x, label=count_lab, y=count_lab_y), size=count_size) +
      geom_hline(yintercept=0, color="#CCCCCC", linetype="dotted")
  }
  
  ## colors defined
  pl <- if(surv.col[1] != 'gg.def'){
    pl + scale_colour_manual(name = gr.name, 
                        values = surv.col)
  } else {
    pl + scale_colour_discrete(name = gr.name)
  }
  
  pl <- if(length(lty.est) == 1){
    pl + scale_linetype_manual(name = gr.name,
                               values = rep(lty.est, strata))
  } else {pl + scale_linetype_manual(name = gr.name, 
                                     values = lty.est)}
  
  ## add in confidence intervals
  if(CI){
    col <- if(length(surv.col) == 1){
      scale_fill_discrete(name = gr.name)
    } else{
      scale_fill_manual(name = gr.name, 
                        values = surv.col)
    }
    pl <- pl + geom_rect(aes(xmin = time, xmax = timeMax, ymin = low, ymax = up, fill = group),
                         alpha = 0.1) + col
  }
  
  ## plot censor points
  if(plot.cens == T & any(dat$cens > 0)){
    if(cens.col.match) {
      pl <- pl + geom_point(data = dat %>% filter(cens > 0), aes(y = surv, col=group), 
                            shape = cens.shape, size=1.75)
    } else {
      pl <- pl + geom_point(data = dat %>% filter(cens > 0), aes(y = surv), shape = cens.shape,
                            col = cens.col)
    }
  } else if(plot.cens == T & all(dat$cens == 0)) {
    stop ('There are no censored observations')
  }
  
  lPos <- if(is.na(legend_pos)){
    if(cumProb) {
      c(0,1)
    } else {
      c(1,1)
    }
  } else {
    legend_pos
  }
  
  pl + theme(legend.justification=lPos, legend.position=lPos)
}


#' @rdname ggsurv_s
#' @title ggsurv single level helper
#' @export
ggsurv_s <- function(s, yAxisScale, CI, plot.cens, surv.col, cens.col, 
                     lty.est, lty.ci, cens.shape, xlab, ylab, main, 
                     cumProb, yTicks, dataLabels, addCounts, count_size,
                     bw, legend_title, legend_pos, strata_names, cens.col.match,
                     cens.seq, cens.cnt){
  nticks <- if(addCounts){
    c(-0.08, seq(0,1, length.out=yTicks))
  } else seq(0,1, length.out=yTicks)
  nlabs <- if(addCounts) {
    c("N", paste0(100*(nticks[-1]), "%"))
  } else paste0(100*nticks, "%")
  yAxisScale <- scale_y_continuous(limits=c(ifelse(addCounts, -0.1, -0.025),1.1), breaks=nticks, labels = nlabs)
  
  ## given default informative y axis label
  if(ylab == ""){
    ylab = ifelse(cumProb, "Cumulative Probability", "Survival")
  }
  ## set up survival data in ggplot form
  if (cumProb) {
    survCol <- 1 - c(1, s$surv)
    lowCol <- 1 - c(1, s$lower)
    highCol <- 1 - c(1, s$upper)
  } else {
    survCol <- c(1, s$surv)
    lowCol <- c(1, s$lower)
    highCol <- c(1, s$upper)
  }
  dat <- data.frame(time = c(0, s$time),
                    surv = survCol,
                    up = highCol,
                    low = lowCol,
                    groupFull = "All",
                    cens = c(0, s$n.censor),
                    atRisk = c(s$n.risk[1], s$n.risk))
  dat$timeMax <- c(dat$time[-1], dat$time[length(dat$time)])
  dat.cens <- subset(dat, cens != 0)
  
  ## if col not specified use black 
  col <- ifelse(surv.col == 'gg.def', 'black', surv.col)
  
  ## extra rows need to be created in order for survival numbers to be added
  
  time_range <- range(dat$time)
  if(missing(cens.seq)) cens.seq <- seq(time_range[1], time_range[2], length.out = cens.cnt)
  
  count_cuts <- dat %>% mutate(time_grp = cut(time, cens.seq)) %>% 
    group_by(time_grp) %>% summarise(time=max(time)) %>% right_join(tibble(time_grp=levels(.$time_grp)), by="time_grp")
  
  dat$count_lab <- dat$atRisk
  dat$count_lab[!dat$time %in% c(min(dat$time), count_cuts$time)] <- NA
  dat <- dat %>% 
    left_join(tibble(time = c(min(dat$time), count_cuts$time), 
                         lab_time = cens.seq), by="time")
  
  ## initial ggplot object created
  min_time <- min(dat$time)
  pl <- ggplot(dat, aes(x = time, y = surv)) +
    xlab(xlab) + ylab(ylab) + ggtitle(main) +
    yAxisScale +
    geom_step(col = col, lty = lty.est) +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank())
  
  ## add counts below graph
  if(addCounts) {
    pl <- pl + geom_text(aes(x=lab_time, label=count_lab, y=-0.08), size=count_size) +
      geom_hline(yintercept=min_time, color="#BBBBBB", linetype="dashed")
  }
  
  ## add rectangles for error
  pl <- if(CI) {
    pl + geom_rect(aes(xmin = time, xmax = timeMax, ymin = low, ymax = up),
                   fill=col, alpha = 0.1)
  } else (pl)
  
  ## plot censor data as crosses
  pl <- if(plot.cens == T & length(dat.cens) > 0){
    if(cens.col.match) {
      pl + geom_point(data = dat.cens, aes(y = surv, col=group),
                      shape = cens.shape, size=1.75)
    } else {
      pl + geom_point(data = dat.cens, aes(y = surv), shape = cens.shape,
                      col = cens.col)
    }
    
  } else if (plot.cens == T & length(dat.cens) == 0){
    stop ('There are no censored observations')
  } else(pl)
  pl
  
}


#' @rdname ggsurv.survfit
#' @title ggsurv survfit s3 object
#' @export

ggsurv.survfit <- function(s, CI = T, plot.cens = T, surv.col = 'gg.def',
                           cens.col = 'red', lty.est = 1, lty.ci = 2,
                           cens.shape = 3, xlab = 'Time',
                           ylab = '', main = '', cumProb = F, yTicks=5, 
                           dataLabels="", addCounts=F, count_size=3, bw=F,
                           legend_title=NA, legend_pos=NA, cens.col.match=F,
                           cens.cnt = 6, cens.seq, ...){
  
  ## confirm validity of parameters
  strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)
  if(!"survfit" %in% class(s)) stop("First parameter needs to be a survfit object")
  ## need a separate construction for single strata and multi-strata

  # Extract info from survfit object 
  ggsurv_m_starter <- function(s, legend_title) {
 
    strata <- ifelse(is.null(s$strata) ==T, 1, length(s$strata))
    
    groups <- factor(unlist(strsplit(names
                                     (s$strata), '='))[seq(2, 2*strata, by = 2)])
    #         groups_labs <- sapply(levels(groups), expression)
    #         names(groups_labs) <- levels(groups)
    
    gr.df <- vector('list', strata)
    ind <- vector('list', strata)
    n.ind <- c(0,s$strata); n.ind <- cumsum(n.ind)
    
    ## need to build data separately for strata
    for(i in 1:strata){
      ind[[i]] <- (n.ind[i]+1):n.ind[i+1]
      if (cumProb) {
        survCol <- 1 - c(1, s$surv[ ind[[i]] ])
        lowCol <- 1 - c(1, s$lower[ ind[[i]] ])
        highCol <- 1 - c(1, s$upper[ ind[[i]] ])
      } else {
        survCol <- c(1, s$surv[ ind[[i]] ])
        lowCol <- c(1, s$lower[ ind[[i]] ])
        highCol <- c(1, s$upper[ ind[[i]] ])
      }
      tmp <-tibble(
        time = c(0, s$time[ ind[[i]] ]),
        surv = survCol,
        up = highCol,
        low = lowCol,
        cens = c(0, s$n.censor[ ind[[i]] ]),
        group = rep(groups[i], s$strata[i] + 1),
        groupFull = rep(names(s$strata)[i], s$strata[i] + 1),
        atRisk = c(s$n.risk[ind[[i]]][1], s$n.risk[ ind[[i]] ]))
      tmp$timeMax <- c(tmp$time[-1], tmp$time[length(tmp$time)])
      gr.df[[i]] <- tmp
    }
    return(gr.df)
  }
  defaults <- as.list(formals())
  givens <- as.list(match.call()[-1])
  used_def <- defaults[!names(defaults) %in% c("...", names(givens))]
  .args <- c(givens, defaults[!names(defaults) %in% c(names(givens), "...")])
  
  ## call either single or multi strata function
  if(strata == 1) {
    call1 <- call("ggsurv_s")
    for(i in 1:length(.args)) call1[[names(.args)[i]]] <- as.name(names(.args)[i])
    call1$strata = as.name("strata")
    eval(call1)
  } else {
    call1 <- call("ggsurv_m")
    for(i in 1:length(.args)) call1[[names(.args)[i]]] <- as.name(names(.args)[i])
    call1$starter = as.name("ggsurv_m_starter")
    call1$strata = as.name("strata")
    eval(call1)
  }
}

#' @rdname ggsurv.survfit.cox
#' @title ggsurv s3 survfit cox object
#' @export
#' 
ggsurv.survfit.cox <- function(s, CI = T, plot.cens = T, surv.col = 'gg.def',
                               cens.col = 'red', lty.est = 1, lty.ci = 2,
                               cens.shape = 3, xlab = 'Time',
                               ylab = '', main = '', cumProb = F, yTicks=5,
                               dataLabels="", addCounts=F, bw=F, cens.col.match=F,
                               cens.cnt = 6, cens.seq,
                               legend_title, legend_pos){
  
  ## confirm validity of parameters
  strata <- ifelse(is.null(ncol(s$surv)) ==T, 1, ncol(s$surv))
  stopifnot(length(surv.col) == 1 | length(surv.col) == strata)
  stopifnot(length(lty.est) == 1 | length(lty.est) == strata)

  # if(class(s) != "survfit") stop("First parameter needs to be a survfit object")
  ## need a separate construction for single strata and multi-strata

  # extract info from cox survfit object
  ggsurv_m_starter <- function(s, legend_title, strata){
    strata <- ifelse(is.null(ncol(s$surv)) ==T, 1, ncol(s$surv))
    gr.df <- vector('list', strata)
    
    snm <- sub("^.*?=", "", names(s$strata))
    ## need to build data separately for strata
    for(i in 1:strata){
      
      if (cumProb) {
        survCol <- 1 - c(1, s$surv[,i])
        lowCol <- 1 - c(1, s$lower[,i])
        highCol <- 1 - c(1, s$upper[,i])
      } else {
        survCol <- c(1, s$surv[,i])
        lowCol <- c(1, s$lower[,i])
        highCol <- c(1, s$upper[,i])
      }
      tmp <- tibble(time=c(0, s$time), surv=survCol, 
                        up=highCol, low=lowCol,
                        cens=c(0, s$n.censor), group=snm[i],
                        atRisk=c(s$n.risk[1], s$n.risk))
      tmp$timeMax <- c(tmp$time[-1], tmp$time[length(tmp$time)])
      gr.df[[i]] <- tmp
    }
    return(gr.df)
  }
  
  defaults <- as.list(formals())
  givens <- as.list(match.call()[-1])
  used_def <- defaults[!names(defaults) %in% c("...", names(givens))]
  .args <- c(givens, defaults[!names(defaults) %in% names(givens)])
  ## call either single or multi strata function
  if(strata == 1) {
    call1 <- match.call(expand.dots=T)
    call1[[1L]] <- as.name("ggsurv_s")
    for(i in 1:length(used_def)) call1[[names(used_def)[i]]] <- as.name(names(used_def)[i])
    eval(call1)
  } else {
    call1 <- match.call(expand.dots=T)
    call1[[1L]] <- as.name("ggsurv_m")
    call1$starter = as.name("ggsurv_m_starter")
    call1$strata = as.name("strata")
    for(i in 1:length(used_def)) call1[[names(used_def)[i]]] <- as.name(names(used_def)[i])
    eval(call1)
  }

}
