#' @title Table Maker
#'
#'
#' generic function to create a summary table of given variables.
#' @param tab_in data.frame with variables var, varnm, and type. optional variables group, target, test_interval, fisher
#' @param ds dataset with variables referenced in tab_in
#' @param grp name of variable to be used for grouping
#' @param pp rounding digits for proportions
#' @param mp rounding digits for mean/sd
#' @param test boolean to request test of group differences
#' @param denom boolean to request display of denominator in proportions
#' @param header choose header for summary columns. "both" - "Percent (n) or Mean (SD)", "msd" - "Mean (SD)", np - "Percent (n)"
#' @keywords table summary print
#' @export
#' @examples
#' 
#' datas <- data.frame(grp=rep(c("A", "B", "C"), rep(100, 3)), 
#' var1 = rnorm(300, 0,1) + rep(1:3, rep(100, 3)), 
#' var2 = sample(c("Yes", "No"), 300, replace=T), 
#' var3 = sample(c("X", "Y", "Z"), 300, replace=T))
#' tab_desc <- bind_rows(data.frame(var="var1", varnm="Variable 1", type="c"),
#'                       data.frame(var="var2", varnm="Variable 2", type="b", target="Yes"),
#'                       data.frame(var="var3", varnm="Variable 3", type="m"))
#' tab1 <- tab_desc %>% group_by(ordr=1:n()) %>% do(tab1_fxn(., datas, grp="grp", test=T)) %>% ungroup
#' tab1 %>% select(-ordr) %>% TIRC(rnames="Characteristic", rgroup_col="group")
#' grp_tirc(tab1 %>% select(-ordr), grp="grp", p="p")
#' grp_tirc(tab1 %>% select(-ordr, -N), grp="grp", p="p")
#' 

tab1_fxn <- function(tab_in, ds, grp, pp=1, mp=1, test=F, denom=F, header="both"){
  # print(tab_in)
  # if(tab_in$var=="ddx") browser()
  ## tab_in should include columns "varnm", "var", "type", *optional* "group"
  ## if type == "b" then target should be available
  # if(tab_in$var == "white") browser()
  if(!"group" %in% names(tab_in)) tab_in$group = ""
  if(!"test_interval" %in% names(tab_in)) tab_in$test_interval = F
  if(!"fisher" %in% names(tab_in)) tab_in$fisher = F
  if(!missing(grp)){
    ds_out <- ds %>% group_by_(grp) %>% do(tab1_fxn_hpr(.,tab_in, pp=pp, mp=mp, denom=denom, header=header))
    if(test){
      p <- tryCatch(test_grp(ds, grp, tab_in), error=function(e) NA)
      ds_out <- ds_out %>% ungroup %>% mutate(p=p)
      if(tab_in$type=="m") ds_out$p[ds_out$Characteristic != ds_out$Characteristic[1]] <- NA
    }
    ds_out
  } else {
    ds %>% do(tab1_fxn_hpr(., tab_in, pp=pp, mp=mp, denom=denom, header=header))
  }
}

#' @rdname tab1_fxn_hpr
#' @title tab1 helper function
#' @export

tab1_fxn_hpr <- function(ds, tab_in, pp, mp, denom=F, header="both"){
  ## header can be both, msd, or np
  # if(tab_in$var=="ddx") browser()
  var_values <- ds[[as.character(tab_in$var)]]
  if(class(var_values) == "factor") {
    targets <- levels(var_values)
    var_values <- as.character(var_values)
  } else {
    var_values <- ifelse(var_values == "", NA, var_values)
    targets <- unique(var_values[!is.na(var_values)])
  }
  n_avail <- sum(!is.na(var_values))
  pct_fxn <- if(denom){
    function(target=tab_in$target) sprintf("%1.*f%% (%g/%g)", pp, 100*sum(var_values == target, na.rm=T)/sum(!is.na(var_values)), sum(var_values == target, na.rm=T), sum(!is.na(var_values)))
  } else {
    function(target=tab_in$target) sprintf("%1.*f%% (%g)", pp, 100*sum(var_values == target, na.rm=T)/sum(!is.na(var_values)), sum(var_values == target, na.rm=T))
  }
  msd_fxn <- function(vv=var_values) sprintf("%1.*f (%1.*f)", mp, mean(vv, na.rm=T), mp, sd(vv, na.rm=T))
  value = if(tab_in$type == "c"){
    msd_fxn()
  } else if(tab_in$type == "b"){
    pct_fxn()
  }
  
  summary_head <- ifelse(header=="msd", "Mean (SD)", ifelse(header=="np", "Percent (n)", "Percent (n) or Mean (SD)"))
  tab_out <- if(tab_in$type == "m"){
    values <- sapply(targets, pct_fxn)
    data_frame(group=tab_in$varnm, Characteristic=targets, N=n_avail, summary_col=values)
  } else {
    data_frame(group=tab_in$group, Characteristic=tab_in$varnm, N=n_avail, summary_col=value)
  } 
  tab_out %>% setNames(c("group", "Characteristic", "N", summary_head))
}


#' @rdname test_grp
#' @title do tests of group differences
#' @export

test_grp <- function(ds, grp, tab_in){
  ## linear model for continuous variables and a chisq test otherwise
  fm1 <- formula(sprintf("%s ~ %s", tab_in$var, grp))
  if(tab_in$type == "c"){
    if(length(unique(ds[[grp]])) == 2){
      vt <- var.test(fm1, ds %>% filter(pair_grp %in% c("Sleep Dist, No Consti", "No Sleep Dist, Consti")))
      var.equal = vt$p.value >= 0.05
      tt <- t.test(fm1, ds %>% filter(pair_grp %in% c("Sleep Dist, No Consti", "No Sleep Dist, Consti")), var.equal=var.equal)
      tt$p.value
    } else if(tab_in$test_interval) {
      print(sprintf("Test for assuming linear groups in order %s", paste(levels(factor(ds[[grp]])), collapse=", ")))
      fm1 <- formula(sprintf("%s ~ as.numeric(factor(%s))", tab_in$var, grp))
      lm1 <- lm(fm1, data=na.omit(ds[c(tab_in$var, grp)]))
      slm1 <- summary(lm1)
      anova(lm1)[1, "Pr(>F)"]
    } else {
      fm1 <- formula(sprintf("%s ~ factor(%s)", tab_in$var, grp))
      lm1 <- lm(fm1, data=na.omit(ds[c(tab_in$var, grp)]))
      slm1 <- summary(lm1)
      anova(lm1)[1, "Pr(>F)"]
    }
  } else {
    if(tab_in$fisher){
      fisher.test(ds[[tab_in$var]], ds[[grp]])$p.value
    } else {
      chisq.test(ds[[tab_in$var]], ds[[grp]])$p.value
    }
  }
}


#' @rdname grp_tirc
#' @title print out grouped table
#' @export

grp_tirc<- function(x, rgroup_col="group", grp="study_grp", rnames="Characteristic", p=F){
  if(p == F) p <- as.character()
  if(!rgroup_col %in% names(x)) x[[rgroup_col]]=""
  cols <- setdiff(names(x), c(grp, p))
  rrdrs <- paste(x[[rgroup_col]], x[[rnames]], sep="_1_")
  x$rorder <- factor(rrdrs, unique(rrdrs))
  # print(cols)
  unique_cols <- c(rgroup_col, rnames)
  wide <- Reduce(function(x, y) merge(x, y, by=c(unique_cols, "rorder"), all=T), lapply(unique(x[[grp]]), function(y) x[x[[grp]]==y, c(cols, "rorder")]))
  grps <- as.character(unique(x[[grp]]))
  ngrps <- rep(length(cols)-2, length(grps))
  pround <- function(x) ifelse(x < 0.001, "&lt;&nbsp;0.001", sprintf("%1.3f", x))
  if(length(p) > 0) {
    wide <- merge(wide, unique(x[c(rgroup_col, rnames, p)]), by=c(rgroup_col, rnames), all.x=T)
    grps <- c(grps, "")
    ngrps <- c(ngrps, 1)
    wide[[p]] <- pround(wide[[p]])
  }
  
  wide <- wide[order(wide[["rorder"]]), -1*which(names(wide)=="rorder")]
  names(wide) <- c(unique_cols, rep(setdiff(cols, unique_cols), length(unique(x[[grp]]))), p)

  TIRC(wide, rnames=rnames, rgroup_col=rgroup_col, cgroup=grps, n.cgroup=ngrps)
}