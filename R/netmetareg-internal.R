nmr_df <- function(x,
                   covar = x$.netmeta$covar,
                   covar.name = x$.netmeta$covar.name) {
  
  # Link abbreviated treatment names with original treatment names
  #
  treat_key <- data.frame(treat_long = x$.netmeta$trts,
                          treat = x$.netmeta$trts.abbr)
  treat_key2 <- treat_key
  #
  # Not intuitive: common with consistency only incorporates covariates from
  # reference containing comparisons, so all non-ref contain the observed
  # covariate levels of the reference
  #
  treat_key2$treat <- ifelse(treat_key2$treat == x$.netmeta$reference.group,
                             "nonref", treat_key2$treat)
  treat_key <- unique(rbind(treat_key2, treat_key))
  #
  res_b_df <- data.frame(treat = "", est = x$b, se = x$se,
                         lower = x$ci.lb, upper = x$ci.ub,
                         z = x$zval, pval = x$pval)
  #
  # Remove backticks which were created in the manual model matrix
  #
  rownames(res_b_df) <- gsub("^`|`$","",rownames(res_b_df))
  #
  res_b_df$treat <- rnam <- rownames(res_b_df)
  #
  sel_TE <- !grepl(":", rnam)
  res_b_TE <- res_b_df[sel_TE, , drop = FALSE]
  rnam_d <- paste0("d[", rnam[sel_TE], "]")
  #
  if (is.null(covar)) {
    rownames(res_b_TE) <- rnam_d
    return(res_b_TE)
  }
  #
  rnam_beta <- paste0("beta[", rnam[!sel_TE], "]")
  #
  res_b_TE$type <- "d"
  res_b_TE$covar <- NA
  res_b_TE$cov_lvl <- NA
  res_b_TE$cov_ref <- NA
  res_b_TE$treat_long <- NA # retain information on original treatment name
  #
  # Get estimates covariate cov_lvls for each treatment
  #
  res_b_interactions <- res_b_df[!sel_TE, , drop = FALSE]
  #
  # Split 'rowname' using strsplit
  #
  split_names <- do.call(rbind, strsplit(rownames(res_b_interactions), "[:]"))
  #
  res_b_interactions$type <- "beta"
  res_b_interactions$treat <- split_names[ , 1]
  res_b_interactions$covar <- covar.name#split_names[, 2]
  #
  # Add original treatment names
  #
  res_b_interactions <-
    merge(res_b_interactions, treat_key, by = "treat", all.x = TRUE)
  #
  is_num <- is.numeric(covar)
  #
  if (!is_num) {
    # Get observed covariate levels for each treatment
    #
    dat <- x$.netmeta$x$data[, c("treat1","treat2", covar.name)]
    #
    # Convert to long format
    #
    unique_lvls <- rbind(setNames(dat[, c("treat1",covar.name)],
                                  c("treat_long", "lvl")),
                         setNames(dat[, c("treat2", covar.name)],
                                  c("treat_long", "lvl")))
    #
    unique_lvls <- unique(unique_lvls)
    # Add treatment abbreviations
    unique_lvls <- merge(unique_lvls,
                         data.frame(treat_long = x$.netmeta$trts,
                                    treat = x$.netmeta$trts.abbr),
                         by = "treat_long", all.x = TRUE)
    #
    if (x$.netmeta$assumption == "common") {
      unique_lvls$treat <-
        ifelse(unique_lvls$treat == x$.netmeta$reference.group,
               "nonref",
               unique_lvls$treat)
    }
    #
    # Capture estimated factor levels
    #
    res_b_interactions$lvl <- gsub(covar.name, "", split_names[, 2])
    #
    # Get the non-included covariate levels aka covariate references
    #
    ref_lvls <-
      merge(unique_lvls, res_b_interactions,
            by = c("treat_long", "lvl", "treat"),
            all.x = TRUE)
    #
    ref_lvls <- ref_lvls[is.na(ref_lvls$est), c("treat_long","lvl", "treat")]
    colnames(ref_lvls)[colnames(ref_lvls) == "lvl"] <- "cov_ref"
    #
    # Should be 1 to many. If there is an issue then it is likely in rma.mv()
    #
    res_b_interactions <-
      merge(res_b_interactions, ref_lvls,
            by = c("treat_long", "treat"), all.x = TRUE)
    colnames(res_b_interactions)[colnames(res_b_interactions) == "lvl"] <-
      "cov_lvl"
    #
    # Remove variable treat_long which we just used for internal merging
    #
    res_b_interactions <-
      res_b_interactions[, names(res_b_interactions) != "treat_long"]
  }
  #
  res <- rbind(res_b_TE[, names(res_b_interactions)], res_b_interactions)
  #
  rownames(res) <- c(rnam_d, rnam_beta)
  #
  attr(res, "reference.group") <- x$.netmeta$reference.group
  attr(res, "covar.name") <- covar.name
  #
  res <- res[, c("type", "treat",
                 if (!is_num) "cov_lvl", if (!is_num) "cov_ref",
                 "est", "se", "lower", "upper", "z", "pval")]
  #
  res
}
