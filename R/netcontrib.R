#' Contribution matrix in network meta-analysis
#' 
#' @description
#' This function generates the contribution of direct comparisons to
#' every network treatment comparison. The output is a matrix where
#' rows represent network treatment effects and columns represent the
#' contribution of direct treatment effects.
#' 
#' @aliases netcontrib print.netcontrib
#' 
#' @param x An object of class \code{netmeta} or \code{netcontrib}.
#' @param method A character string indicating which method is to
#'   calculate the contribution matrix. Either \code{"randomwalk"},
#'   \code{"shortestpath"}, \code{"cccp"}, or \code{"pseudoinverse"},
#'   can be abbreviated.
#' @param hatmatrix.F1000 A logical indicating whether hat matrix
#'   given in F1000 article should be used for \code{method =
#'   "shortestpath"}.
#' @param common A logical indicating whether a contribution matrix
#'   should be printed for the common effects network meta-analysis.
#' @param random A logical indicating whether a contribution matrix
#'   should be printed for the random effects network meta-analysis.
#' @param nchar.trts A numeric defining the minimum number of
#'   characters used to create unique treatment names (see Details).
#' @param digits Minimal number of significant digits, see
#'   \code{print.default}.
#' @param legend A logical indicating whether a legend should be
#'   printed.
#' @param pathContribution A logical indicating whether the contribution matrix
#'   of individual paths should be printed. (Currently supported only for the
#'   \{"shortextpath"} method)
#' @param warn.deprecated A logical indicating whether warnings should
#'   be printed if deprecated arguments are used.
#' @param verbose A logical indicating whether progress information
#'   should be printed.
#' @param \dots Additional arguments.
#' 
#' @details
#' In network meta-analysis (NMA), it is important to assess the
#' influence of limitations or other characteristics of individual
#' studies on the estimates obtained from the network. To this end,
#' the contribution matrix shows how much each direct treatment effect
#' contributes to each treatment effect estimate from network
#' meta-analysis.
#' 
#' We use ideas from graph theory to derive the proportion that is
#' contributed by each direct treatment effect using R package
#' \bold{igraph}. We start
#' with the 'projection' matrix in a two-step network meta-analysis model,
#' called the H matrix, which is analogous to the hat matrix in a
#' linear regression model. H entries are translated to proportion
#' contributions based on the observation that the rows of H can be
#' interpreted as flow networks. A stream is defined as the
#' composition of a path and its associated flow (Papakonstantinou et
#' al., 2018).
#'
#' To account for multi-arm trials, we use the H matrix from a
#' two-step (aggregate) version of the graph theoretical NMA model
#' (Davies et al., 2022). This H matrix can be obtained from
#' \code{\link{hatmatrix}} with argument \code{method = "davies"}.
#' 
#' Four methods are implemented to estimate the proportion
#' contributions:
#' 
#' (1) If argument \code{method = "randomwalk"}, an analytical
#' random-walk (RW) approach is used (Davies et al., 2022). Here, the
#' "full" version of the aggregate H matrix (\code{\link{hatmatrix}}
#' with arguments \code{method = "davies"} and \code{type = "full"})
#' is used to define RW transition matrices. For each pair of
#' treatments (ij) in the network, the elements in the corresponding
#' row of H-full define a transition matrix from node i to node j. We
#' use the \bold{igraph} package to find every (directed) path from
#' node i to node j. The flow through each path is then equal to the
#' probability that a walker takes that path. This is simply the
#' product of the transition probabilities associated with each edge
#' along the path.
#' 
#' (2) If argument \code{method = "shortestpath"}, an iterative
#' algorithm is used (Papakonstantinou et al., 2018). Broadly
#' speaking, each iteration of the algorithm consists of the following
#' steps: (i) A path in the evidence flow network is selected. (ii)
#' The minimum flow through the edges making up the path is
#' identified. This is assigned as the flow associated with the
#' path. (iii) The flow of the path is subtracted from the values of
#' flow in the edges that make up that path. This means that the edge
#' corresponding to the minimum flow in that path is removed from the
#' graph. (iv) A new path is then selected from the remaining
#' graph. The process repeats until all the evidence flow in the edges
#' has been assigned to a path.
#' 
#' In the original F1000 paper (Papakonstantinou et al., 2018), the
#' hat matrix used did not account for correlations due to multi-arm
#' trials. For reproducibility the result of this version can be
#' obtained by specifying \code{hatmatrix.F1000 = TRUE} for
#' \code{method = "shortestpath"}. For other purposes, this method is
#' not recommended.
#'
#' (3) If argument \code{method = "cccp"}, contributions are estimated
#' using \code{\link[cccp]{l1}} from R package \bold{cccp} which must be
#' available (Rücker et al., 2024).
#'
#' (4) If argument \code{method = "pseudoinverse"}, contributions are
#' derived from an L2 solution based on a Moore-Penrose pseudoinverse
#' (Rücker et al., 2024).
#' 
#' Once the streams have been identified by any method, the proportion
#' contribution of each direct comparison is equal to the sum over the
#' flow of evidence in each path containing that edge divided by the
#' number of edges that make up that path.
#'
#' By default, treatment names are not abbreviated in
#' printouts. However, in order to get more concise printouts,
#' argument \code{nchar.trts} can be used to define the minimum number
#' of characters for abbreviated treatment names (see
#' \code{\link{abbreviate}}, argument \code{minlength}). R function
#' \code{\link{treats}} is utilised internally to create abbreviated
#' treatment names.
#'
#' Calculation of network contributions can be compute-intensive for
#' the random-walk approach in large networks. Crude information on
#' the computation progress is printed if argument \code{verbose} is
#' \code{TRUE}. In addition, computation times are printed if R
#' package \bold{tictoc} is installed.
#' 
#' @return
#' An object of class \code{netcontrib} with corresponding
#' \code{print} function. The object is a list containing the
#' following components:
#' \item{common}{Numeric matrix of percentage contributions of direct
#'   comparisons for each network comparison for the common effects
#'   model.}
#' \item{random}{Numeric matrix of percentage contributions of direct
#'   comparisons for each network comparison for the random effects
#'   model.}
#' \item{common.pcm}{Data frame containing path contributions for each
#'   comparison for the common effect model.}
#' \item{random.pcm}{Data frame containing path contributions for each
#'   comparison for the random effects model.}
#' \item{x}{As defined above.}
#' \item{tictoc.common}{Computation times under common effects model
#'   (if R package \bold{tictoc} is installed).}
#' \item{tictoc.random}{Computation times under random effects model
#'   (if R package \bold{tictoc} is installed).}
#' with the contribution matrices for common and random NMA. Each
#' matrix has the percentage contributions of each direct comparison
#' as columns for each network comparison, direct or indirect as rows.
#' 
#' @author Theodoros Papakonstantinou \email{dev@@tpapak.com}, Annabel
#'   Davies \email{annabel.davies@@manchester.ac.uk}
#' 
#' @seealso \code{\link{netmeta}}, \code{\link[metadat]{dat.woods2010}}
#' 
#' @references
#' Davies AL, Papakonstantinou T, Nikolakopoulou A, Rücker G, Galla T
#' (2022):
#' Network meta-analysis and random walks.
#' \emph{Statistics in Medicine},
#' \bold{41}, 2091--2114
#' 
#' Papakonstantinou T, Nikolakopoulou A, Rücker G, Chaimani A, Schwarzer G,
#' Egger M, Salanti G (2018):
#' Estimating the contribution of studies in network meta-analysis:
#' paths, flows and streams.
#' \emph{F1000Research}
#' 
#' Rücker G, Papakonstantinou T, Nikolakopoulou A, Schwarzer G, Galla T,
#' Davies AL (2024):
#' Shortest path or random walks? A framework for path weights in network
#' meta-analysis.
#' \emph{Statistics in Medicine}.
#' 
#' @keywords contribution
#' 
#' @examples
#' \donttest{
#' # Use the Woods dataset
#' #
#' pw1 <- pairwise(treatment, event = r, n = N,
#'   studlab = author, data = dat.woods2010, sm = "OR")
#' 
#' net1 <- netmeta(pw1)
#' 
#' # Shortest path approach (default)
#' netcontrib(net1)
#' }
#'
#' \dontrun{ 
#' # Random walk approach
#' netcontrib(net1, method = "r")
#' }
#' 
#' @rdname netcontrib
#' @export netcontrib

netcontrib <- function(x,
                       method = "shortestpath",
                       hatmatrix.F1000 = FALSE,
                       common = x$common,
                       random = x$random,
                       nchar.trts = x$nchar.trts,
                       studyContribution = FALSE,
                       pathContribution = FALSE,
                       warn.deprecated = gs("warn.deprecated"),
                       verbose = FALSE,
                       ...) {
  
  ##
  ##
  ## (1) Check for netmeta object and upgrade object
  ##
  ##
  chkclass(x, "netmeta")
  x <- updateversion(x)
    
  
  ##
  ##
  ## (2) Check other arguments
  ##
  ##
  method <-
    setchar(method, c("randomwalk", "shortestpath", "cccp", "pseudoinverse"))
  chklogical(hatmatrix.F1000)
  if (method == "randomwalk" & hatmatrix.F1000) {
    warning("Argument 'hatmatrix.F1000' ignored for random walk method.",
            call. = FALSE)
    hatmatrix.F1000 <- FALSE
  }
  ##
  if (method == "cccp")
    is_installed_package("cccp")
  
  chklogical(studyContribution)
  chklogical(pathContribution)
  chknumeric(nchar.trts, min = 1, length = 1)
  chklogical(verbose)
  ##
  ## Check for deprecated arguments in '...'
  ##
  args  <- list(...)
  chklogical(warn.deprecated)
  ##
  missing.common <- missing(common)
  common <- deprecated(common, missing.common, args, "comb.fixed",
                       warn.deprecated)
  common <- deprecated(common, missing.common, args, "fixed",
                       warn.deprecated)
  chklogical(common)
  ##
  random <- deprecated(random, missing(random), args, "comb.random",
                       warn.deprecated)
  chklogical(random)
  
  
  ##
  ##
  ## (3) Create netcontrib object
  ##
  ##
  x$common <- common
  x$random <- random
  ##
  cm.f <- contribution.matrix(x, method, "common", hatmatrix.F1000, verbose, studyContribution, pathContribution)
  cm.r <- contribution.matrix(x, method, "random", hatmatrix.F1000, verbose, studyContribution, pathContribution)
  ##
  res <- list(common = cm.f$weights,
              random = cm.r$weights,
              common.scm = cm.f$studyContribution.Matrix,
              random.scm = cm.r$studyContribution.Matrix,
              common.pcm = cm.f$pathContribution.Matrix,
              random.pcm = cm.r$pathContribution.Matrix,
              method = method,
              hatmatrix.F1000 = hatmatrix.F1000,
              nchar.trts = nchar.trts,
              x = x,
              version = packageDescription("netmeta")$Version
              )
  ##
  if (!is.null(cm.f$tictoc))
    res$tictoc.common <- cm.f$tictoc
  if (!is.null(cm.r$tictoc))
    res$tictoc.random <- cm.r$tictoc
  ##
  ## Backward compatibility
  ##
  res$fixed <- res$common
  if (!is.null(res$tictoc.common))
    res$tictoc.fixed <- res$tictoc.common
  ##
  class(res) <- "netcontrib"
  ##
  res
}


#' @rdname netcontrib
#' 
#' @method print netcontrib
#' 
#' @export

print.netcontrib <- function(x,
                             common = x$x$common,
                             random = x$x$random,
                             digits = 4,
                             nchar.trts = x$nchar.trts,
                             legend = gs("legend"),
                             warn.deprecated = gs("warn.deprecated"),
                             ...) {
  
  ##
  ##
  ## (1) Check for netcontrib object and upgrade object
  ##
  ##
  chkclass(x, "netcontrib")
  chksuitable(x, "Network contributions",
              classes = c("netmeta.crossnma", "netmeta.multinma"))
  x <- updateversion(x)
  
  
  ##
  ##
  ## (2) Check other arguments
  ##
  ##
  chknumeric(nchar.trts, length = 1)
  chklogical(legend)
  ##
  ## Check for deprecated arguments in '...'
  ##
  args  <- list(...)
  chklogical(warn.deprecated)
  ##
  missing.common <- missing(common)
  common <- deprecated(common, missing.common, args, "comb.fixed",
                       warn.deprecated)
  common <- deprecated(common, missing.common, args, "fixed",
                       warn.deprecated)
  chklogical(common)
  ##
  random <- deprecated(random, missing(random), args, "comb.random",
                       warn.deprecated)
  chklogical(random)
  
  
  ##
  ##
  ## (3) Print results for contribution matrix
  ##
  ##
  matitle(x$x)
  ##
  cat("Contribution matrix (",
      if (is.null(x$method) | x$method == "shortestpath")
        "Papakonstantinou et al., 2018, F1000Research"
      else if (x$method == "randomwalk")
        "Davies et al., 2022, Stat Med"
      else if (x$method == "cccp")
        paste0("Ruecker et al., 2023, ",
               "L1 solution based on R package cccp")
      else if (x$method == "pseudoinverse")
        paste0("Ruecker et al., 2023, ",
               "L2 solution based on Moore-Penrose pseudoinverse")
      else
        "unknown method",
      ")",
      sep = "")
  ##
  if ((is.null(x$method) | x$method == "shortestpath") & x$hatmatrix.F1000)
    cat(",\nhat matrix does not take correlation of",
        "multi-arm studies into account")
  ##
  cat("\n\n")
  
  
  ##
  trts <- x$x$trts
  ##
  if (common) {
    rownames(x$common) <- comps(x$common, trts, x$x$sep.trts, nchar.trts)
    colnames(x$common) <- comps(x$common, trts, x$x$sep.trts, nchar.trts,
                                row = FALSE)
    ##
    cat("Common effects model:\n\n")
    prmatrix(round(x$common, digits))
    if (random)
      cat("\n")
  }
  if (random) {
    rownames(x$random) <- comps(x$random, trts, x$x$sep.trts, nchar.trts)
    colnames(x$random) <- comps(x$random, trts, x$x$sep.trts, nchar.trts,
                                row = FALSE)
    ##
    cat("Random effects model:\n\n")
    prmatrix(round(x$random, digits))
  }
  ##
  ## Add legend with abbreviated treatment labels
  ##
  legendabbr(trts, treats(trts, nchar.trts), legend & (common | random))
  ##
  invisible(NULL)
}
