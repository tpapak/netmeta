
library(corpcor)
library(meta)
library(netmeta)


#----------------------------------------------
# Binary, fixed effects
#----------------------------------------------
data1 <- read.csv("tests/NMA_data_binary_FE.csv")

pairwise1 <- meta::pairwise(treat = T,
                            event = R,
                            n = N,
                            studlab = Study,
                            data = data1,
                            sm = "OR")

netmeta1 <- netmeta::netmeta(TE = TE,
                             seTE = seTE,
                             treat1 = treat1,
                             treat2 = treat2,
                             studlab = studlab,
                             common = TRUE,
                             data = pairwise1,
                             sm = "OR")

contributions1 <- netmeta::netcontrib(x = netmeta1,
                                      method = "shortestpath",
                                      random = FALSE)

#The current version of CINeMA uses the old hat matrix
# contributions1 <- netmeta::netcontrib(x = netmeta1,
#                                       method = "shortestpath",
#                                       random = FALSE,
#                                       hatmatrix.F1000 = TRUE)


#The B matrix for 3-arm studies described in Rucker, Schwarzer
B_matrix_3_arms <- matrix(c(1, -1, 0,
                            1, 0, -1,
                            0, 1, -1),
                          byrow = TRUE,
                          nrow = 3)

#Unadjusted weights
weights <- 1 / pairwise1$seTE^2
names(weights) <- paste0(pairwise1$studlab, "_", pairwise1$treat1, "_", pairwise1$treat2)

#Variance matrix
variance_matrix <- diag(pairwise1$seTE^2)
rownames(variance_matrix) <- names(weights)
colnames(variance_matrix) <- names(weights)

#Variance matrix for StudyABC1
variance_matrix_ABC1 <- variance_matrix[3:5, 3:5]
#Inverse Laplacian for StudyABC1
pseudo_inverse_laplacian_ABC1 <- -t(B_matrix_3_arms) %*% B_matrix_3_arms %*% variance_matrix_ABC1 %*% t(B_matrix_3_arms) %*% B_matrix_3_arms / (2 * 3^2)
#Laplacian for StudyABC1
laplacian_ABC1 <- corpcor::pseudoinverse(pseudo_inverse_laplacian_ABC1)

#Variance matrix for StudyABC2
variance_matrix_ABC2 <- variance_matrix[6:8, 6:8]
#Inverse Laplacian for StudyABC2
pseudo_inverse_laplacian_ABC2 <- -t(B_matrix_3_arms) %*% B_matrix_3_arms %*% variance_matrix_ABC2 %*% t(B_matrix_3_arms) %*% B_matrix_3_arms / (2 * 3^2)
#Laplacian for StudyABC2
laplacian_ABC2 <- corpcor::pseudoinverse(pseudo_inverse_laplacian_ABC2)

#Updated weights taking multi-arm studies into account
updated_weights <- weights
updated_weights[3] <- laplacian_ABC1[1, 2]
updated_weights[4] <- laplacian_ABC1[1, 3]
updated_weights[5] <- laplacian_ABC1[2, 3]
updated_weights[6] <- laplacian_ABC2[1, 2]
updated_weights[7] <- laplacian_ABC2[1, 3]
updated_weights[8] <- laplacian_ABC2[2, 3]

#Contributions to the AB comparison (top row of the contribution matrix)
study_contributions_to_AB <- vector(length = length(unique(data1$Study)))
names(study_contributions_to_AB) <- unique(data1$Study)

#Contributions from direct AB comparison
sum_AB_weights <- sum(updated_weights[c(1:3, 6)])
study_contributions_to_AB["StudyAB1"] <- contributions1$common["A:B", "A:B"] * updated_weights["StudyAB1_A_B"] / sum_AB_weights
study_contributions_to_AB["StudyAB2"] <- contributions1$common["A:B", "A:B"] * updated_weights["StudyAB2_A_B"] / sum_AB_weights
study_contributions_to_AB["StudyABC1"] <- contributions1$common["A:B", "A:B"] * updated_weights["StudyABC1_A_B"] / sum_AB_weights
study_contributions_to_AB["StudyABC2"] <- contributions1$common["A:B", "A:B"] * updated_weights["StudyABC2_A_B"] / sum_AB_weights

#Contributions from direct AC comparison
sum_AC_weights <- sum(updated_weights[c(4, 7, 9, 10)])
study_contributions_to_AB["StudyABC1"] <- study_contributions_to_AB["StudyABC1"] + contributions1$common["A:B", "A:C"] * updated_weights["StudyABC1_A_C"] / sum_AC_weights
study_contributions_to_AB["StudyABC2"] <- study_contributions_to_AB["StudyABC2"] + contributions1$common["A:B", "A:C"] * updated_weights["StudyABC2_A_C"] / sum_AC_weights
study_contributions_to_AB["StudyAC1"] <- contributions1$common["A:B", "A:C"] * updated_weights["StudyAC1_A_C"] / sum_AC_weights
study_contributions_to_AB["StudyAC2"] <- contributions1$common["A:B", "A:C"] * updated_weights["StudyAC2_A_C"] / sum_AC_weights

#Contributions from direct AD comparison
sum_AD_weights <- sum(updated_weights[c(11, 12)])
study_contributions_to_AB["StudyAD1"] <- contributions1$common["A:B", "A:D"] * updated_weights["StudyAD1_A_D"] / sum_AD_weights
study_contributions_to_AB["StudyAD2"] <- contributions1$common["A:B", "A:D"] * updated_weights["StudyAD2_A_D"] / sum_AD_weights

#Contributions from direct BC comparison
sum_BC_weights <- sum(updated_weights[c(5, 8, 13, 14)])
study_contributions_to_AB["StudyABC1"] <- study_contributions_to_AB["StudyABC1"] + contributions1$common["A:B", "B:C"] * updated_weights["StudyABC1_B_C"] / sum_BC_weights
study_contributions_to_AB["StudyABC2"] <- study_contributions_to_AB["StudyABC2"] +contributions1$common["A:B", "B:C"] * updated_weights["StudyABC2_B_C"] / sum_BC_weights
study_contributions_to_AB["StudyBC1"] <- contributions1$common["A:B", "B:C"] * updated_weights["StudyBC1_B_C"] / sum_BC_weights
study_contributions_to_AB["StudyBC2"] <- contributions1$common["A:B", "B:C"] * updated_weights["StudyBC2_B_C"] / sum_BC_weights

#Final contributions to AB
study_contributions_to_AB



