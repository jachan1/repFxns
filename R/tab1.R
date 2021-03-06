#' @title Table Maker
#'
#'
#' generic function to create a summary table of given variables.
#' @param tab_in data.frame with variables var, varnm, and type. optional variables group, target, test_interval, fisher, mwu
#' @param ds dataset with variables referenced in tab_in
#' @param grp name of variable to be used for grouping
#' @param pp rounding digits for proportions [used if rnd not in tab_in]
#' @param mp rounding digits for mean/sd [used if rnd not in tab_in]
#' @param long_cr boolean to request more details in parenthesis
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

tab1_fxn <- function(tab_in, ds, grp, pp=0, mp=1, test=F, denom=F, header="both", long_cr=F, plusmn=F){
  # print(tab_in)
  # if(tab_in$var=="ddx") browser()
  ## tab_in should include columns "varnm", "var", "type", *optional* "group" "target" "rnd"
  ## if type == "b" then target should be available
  # if(tab_in$var == "white") browser()
  # if(tab_in$var == "elescrlr") browser()
  if(!"group" %in% names(tab_in)) tab_in$group = ""
  if(!"rnd" %in% names(tab_in)) tab_in$rnd = ifelse(tab_in$type %in% c("m", "b"), pp, mp)
  if(!"test_interval" %in% names(tab_in)) tab_in$test_interval = F
  if(!"fisher" %in% names(tab_in)) tab_in$fisher = F
  if(!"mwu" %in% names(tab_in)) tab_in$mwu = F
  var_values <- ds[[as.character(tab_in$var)]]
  targets <- if(class(var_values) == "factor") {
    levels(var_values)
  } else {
    unique(var_values[!is.na(var_values)])
  }
  
  if(!missing(grp)){
    ds_out <- ds %>% group_by(!! sym(grp)) %>% 
      do(tab1_fxn_hpr(.,tab_in, denom=denom, header=header, long_cr=long_cr, targets=targets, plusmn=plusmn))
    if(test){
      p <- tryCatch(test_grp(ds, grp, tab_in), error=function(e) NA)
      ds_out <- ds_out %>% ungroup %>% mutate(p=p)
      if(tab_in$type=="m") ds_out$p[ds_out$Characteristic != ds_out$Characteristic[1]] <- NA
    }
    ds_out
  } else {
    ds %>% 
      do(tab1_fxn_hpr(., tab_in, denom=denom, header=header, long_cr=long_cr, targets=targets, plusmn=plusmn)) %>% 
      mutate(across(c(Characteristic), as.character))
  }
}

#' @rdname tab1_fxn_hpr
#' @title tab1 helper function
#' @export

tab1_fxn_hpr <- function(ds, tab_in, denom=F, header="both", long_cr=F, targets=c(), plusmn=F){
  ## header can be both, msd, or np
  # if(tab_in$var=="ddx") browser()

  rnd <- tab_in$rnd[1]
  var_values <- ds[[as.character(tab_in$var)]]
  var_values <- if(class(var_values) == "factor") {
    as.character(var_values)
  } else {
    ifelse(as.character(var_values) == "", NA, var_values)
  }
  n_avail <- sum(!is.na(var_values))
  pct_fxn <- if(denom){
    function(target=tab_in$target) sprintf("%1.*f%% (%g/%g)", rnd, 100*sum(var_values == target, na.rm=T)/sum(!is.na(var_values)), sum(var_values == target, na.rm=T), sum(!is.na(var_values)))
  } else {
    function(target=tab_in$target) sprintf("%1.*f%% (%g)", rnd, 100*sum(var_values == target, na.rm=T)/sum(!is.na(var_values)), sum(var_values == target, na.rm=T))
  }
  msd_str <- ifelse(plusmn, "%1.*f &plusmn; %1.*f", "%1.*f (%1.*f)")
  msd_fxn <- function(vv=var_values) sprintf(msd_str, 
                                             rnd, mean(vv, na.rm=T), 
                                             rnd, sd(vv, na.rm=T))
  msd_long_fxn <- function(vv=var_values) {
    nvv <- sum(!is.na(vv))
    mnvv <- mean(vv, na.rm=T)
    sdvv <- sd(vv, na.rm=T)
    sprintf("%1.*f (%1.*f, 95%% CI: %1.*f, %1.*f, range: %1.*f, %1.*f)", 
            rnd, mnvv, 
            rnd, sdvv,
            rnd, mnvv - qt(0.975, nvv-1)*sdvv/sqrt(nvv),
            rnd, mnvv + qt(0.975, nvv-1)*sdvv/sqrt(nvv),
            rnd, min(vv, na.rm=T),
            rnd, max(vv, na.rm=T))
  }
  value = if(tab_in$type == "c"){
    if(long_cr) {
      msd_long_fxn()
    } else {
      msd_fxn()
    }
  } else if(tab_in$type == "b"){
    pct_fxn()
  }
  msd_head = ifelse(plusmn, "Mean &plusmn; SD", "Mean (SD)")
  summary_head <- ifelse(header=="msd", msd_head, ifelse(header=="np", "Percent (n)", sprintf("Percent (n) or %s", msd_head)))
  tab_out <- if(tab_in$type == "m"){
    values <- sapply(targets, pct_fxn)
    tibble(group=tab_in$varnm, Characteristic=targets, N=n_avail, summary_col=values)
  } else {
    tibble(group=tab_in$group, Characteristic=tab_in$varnm, N=n_avail, summary_col=value)
  } 
  tab_out %>% setNames(c("group", "Characteristic", "N", summary_head))
}


#' @rdname test_grp
#' @title do tests of group differences
#' @export

test_grp <- function(ds, grp, tab_in){
  ## linear model for continuous variables and a chisq test otherwise
  fm1 <- formula(sprintf("`%s` ~ `%s`", tab_in$var, grp))
  if(tab_in$type == "c"){
    if(length(unique(ds[[grp]])) == 2){
      if(tab_in$mwu) {
        wt <- wilcox.test(fm1, ds)
        wt$p.value
      } else {
        vt <- var.test(fm1, ds)
        var.equal = vt$p.value >= 0.05
        tt <- t.test(fm1, ds, var.equal=var.equal)
        tt$p.value 
      }
    } else if(tab_in$test_interval) {
      print(sprintf("Test for assuming linear groups in order %s", paste(levels(factor(ds[[grp]])), collapse=", ")))
      fm1 <- formula(sprintf("`%s` ~ as.numeric(factor(`%s`))", tab_in$var, grp))
      lm1 <- lm(fm1, data=na.omit(ds[c(tab_in$var, grp)]))
      slm1 <- summary(lm1)
      anova(lm1)[1, "Pr(>F)"]
    } else {
      fm1 <- formula(sprintf("`%s` ~ factor(`%s`)", tab_in$var, grp))
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

grp_tirc<- function(x, rgroup_col="group", 
                    grp="study_grp", rnames="Characteristic", p=F, summ_col="Percent (n) or Mean (SD)", 
                    grp_ordr, ...){

  if(!rgroup_col %in% names(x)) x[[rgroup_col]]=""
  if(!"ordr" %in% names(x)) x$ordr <- 1

  miss_str <- "Missing"
  while(miss_str %in% names(x)) {
    miss_str <- paste0("~", miss_str)
  }
  
  x_pc <- x %>% 
    ungroup() %>% 
    mutate_at(summ_col, function(x) ifelse(x=="NaN% (0)", NA, x)) %>% 
    mutate(rorder = factor(paste(sprintf("%05d", ordr), !! sym(rgroup_col), !! sym(rnames), sep="_")))
  
  if(any(is.na(x_pc[[grp]]))) x_pc[[grp]] <- ifelse(is.na(x_pc[[grp]]), miss_str, as.character(x_pc[[grp]]))
  
  if(missing(grp_ordr)) {
    if(is.factor(x_pc[[grp]])) {
      grp_ordr <- levels(x_pc[[grp]])
    } else {
      grp_ordr = unique(x_pc[[grp]])
    }
  }
    
  grpd_cols <- apply(expand.grid(c("N", summ_col), grp_ordr, stringsAsFactors = F), 1, paste, collapse="_")
  
  pround <- function(p1) ifelse(p1 < 0.001, "&lt;0.001", sprintf("%1.3f", p1))
  wide_ds <- x_pc %>% 
    select(-ordr) %>% 
    pivot_wider(names_from=c(!!sym(grp)), values_from=c(N, !!sym(summ_col))) %>%
    arrange(rorder) %>% 
    select(-rorder)
                  
  
  if(p != F){
    wide_ds <- wide_ds %>% 
      mutate_at(p, pround)
    p_head = ""
  } else {
    p_head <- p <- as.character()
  }
  
  setNames(wide_ds[c(rgroup_col, rnames, grpd_cols, p)],
           c(rgroup_col, rnames, rep(c("N", summ_col), length(grp_ordr)), p)) %>% 
    TIRC(rnames=rnames, rgroup_col=rgroup_col, cgroup=c(grp_ordr, p_head), n.cgroup=c(rep(2, length(grp_ordr)), length(p)), ...)
}
