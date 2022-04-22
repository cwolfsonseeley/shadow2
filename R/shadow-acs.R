combine_estimates <- function(bg, moe_bg, tract, moe_tract) {
    # this implementation is arbitrary. it just weights by the inverse of the
    # moe's
    result <- (bg/moe_bg + tract/moe_tract) / (1/moe_bg + 1/moe_tract)
    result[is.na(bg)] <- tract[is.na(bg)]
    result
}

# function that, given numerator and denominator along with associated moes,
# calculates the moe of the derived estimate
ratio_moe <- function(numerator, numerator_moe, denominator, denominator_moe) {
    ratio <- numerator / denominator
    top <- sqrt((numerator_moe ^ 2) + ((ratio ^ 2) * (denominator_moe ^ 2)))
    top / denominator
}

# given all bg and tract estimates and moe's for numerator and denominator,
# calculates a combined estimated ratio
combined_ratio <- function(num_bg, num_moe_bg, denom_bg, denom_moe_bg,
                           num_tract, num_moe_tract, denom_tract, denom_moe_tract) {
    bg <- num_bg / denom_bg
    moe_bg <- ratio_moe(num_bg, num_moe_bg, denom_bg, denom_moe_bg)
    tract <- num_tract / denom_tract
    moe_tract <- ratio_moe(num_tract, num_moe_tract, denom_tract, denom_moe_tract)
    combine_estimates(bg, moe_bg, tract, moe_tract)
}

#' @import dplyr
#' @export
shadow_acs <- function() {
    high_income <- 411160
    high_income_moe <- 1.645 * 8038
    moe_scaler <- 1.88
    income_trunc <- 250000

    qry <- getcdw::parameterize_template(
        system.file(
            "sql",
            "acs-qry-template.sql",
            package = "shadow2")
    )

    tract_demo <- getcdw::get_cdw(qry("tract"))
    bg_demo <- getcdw::get_cdw(qry("bg"))

    tract_demo <- dplyr::select(tract_demo, entity_id,
                                hh:occupied_before_1980_moe)
    tract_demo <- dplyr::rename_at(tract_demo,
                                   .vars = dplyr::vars(-entity_id),
                                   .funs = dplyr::funs(paste0(., "_tract")))
    bg_demo <- dplyr::select(bg_demo, entity_id,
                             hh:occupied_before_1980_moe)
    bg_demo <- dplyr::rename_at(bg_demo, .vars = dplyr::vars(-entity_id),
                         .funs = dplyr::funs(paste0(., "_bg")))
    demo <- dplyr::full_join(tract_demo, bg_demo, by = "entity_id")

    demo %>%
        # adjust median income numbers to (try to) deal with the 250k cutoff
        # adjust both point estimate and moe
        mutate(median_income_moe_bg =
                   ifelse(median_income_bg > income_trunc,
                          high_income_moe * moe_scaler,
                          median_income_moe_bg)) %>%
        mutate(median_income_moe_tract =
                   ifelse(median_income_tract > income_trunc,
                          high_income_moe,
                          median_income_moe_tract)) %>%
        mutate(median_income_bg =
                   ifelse(median_income_bg > income_trunc,
                          high_income,
                          median_income_bg)) %>%
        mutate(median_income_tract =
                   ifelse(median_income_tract > income_trunc,
                          high_income,
                          median_income_tract)) %>%
        # combined_ratio combines estimates from block-group and tract levels,
        # and calculates a percentage
        mutate(investor_pct =
                   combined_ratio(
                       investor_hh_bg, investor_hh_moe_bg,
                       hh_bg, hh_moe_bg,
                       investor_hh_tract, investor_hh_moe_tract,
                       hh_tract, hh_moe_tract)) %>%
        mutate(median_income =
                   combine_estimates(
                       median_income_bg, median_income_moe_bg,
                       median_income_tract, median_income_moe_tract)) %>%
        mutate(under_18_pct =
                   combined_ratio(
                       under_18_bg, under_18_moe_bg,
                       total_pop_bg, total_pop_moe_bg,
                       under_18_tract, under_18_moe_tract,
                       total_pop_tract, total_pop_moe_tract)) %>%
        mutate(over_65_pct =
                   combined_ratio(
                       over_65_bg, over_65_moe_bg,
                       total_pop_bg, total_pop_moe_bg,
                       over_65_tract, over_65_moe_tract,
                       total_pop_tract, total_pop_moe_tract)) %>%
        mutate(college_educated_pct =
                   combined_ratio(
                       college_educated_bg, college_educated_moe_bg,
                       pop25_plus_bg, pop25_plus_moe_bg,
                       college_educated_tract, college_educated_moe_tract,
                       pop25_plus_tract, pop25_plus_moe_tract)) %>%
        mutate(private_hs_pct =
                   combined_ratio(
                       private_hs_bg, private_hs_moe_bg,
                       pop_hs_bg, pop_hs_moe_bg,
                       private_hs_tract, private_hs_moe_tract,
                       pop_hs_tract, pop_hs_moe_tract)) %>%
        mutate(homeowner_pct =
                   combined_ratio(
                       tenure_type_owner_bg, tenure_type_owner_moe_bg,
                       tenure_type_hh_bg, tenure_type_hh_moe_bg,
                       tenure_type_owner_tract, tenure_type_owner_moe_tract,
                       tenure_type_hh_tract, tenure_type_hh_moe_tract)) %>%
        mutate(after2000_pct =
                   combined_ratio(
                       occupied_after_2000_bg, occupied_after_2000_moe_bg,
                       owner_occupied_bg, owner_occupied_moe_bg,
                       occupied_after_2000_tract, occupied_after_2000_moe_tract,
                       owner_occupied_tract, owner_occupied_moe_tract)) %>%
        mutate(before1980_pct =
                   combined_ratio(
                       occupied_before_1980_bg, occupied_before_1980_moe_bg,
                       owner_occupied_bg, owner_occupied_moe_bg,
                       occupied_before_1980_tract, occupied_before_1980_moe_tract,
                       owner_occupied_tract, owner_occupied_moe_tract)) %>%
        select(entity_id,
               median_income,
               investor_pct,
               under_18_pct,
               over_65_pct,
               college_educated_pct,
               private_hs_pct,
               homeowner_pct,
               after2000_pct,
               before1980_pct)
}
