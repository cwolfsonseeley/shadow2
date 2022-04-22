#' @export
shadow_ca_campaign <- function() {
    getcdw::get_cdw("
    select
        ca.entity_id,
        ca.rcpt_date,
        ca.amount,
        cvr.filer_naml,
        cvr.filer_namf,
        cvr.cand_naml,
        cvr.cand_namf,
        cvr.bal_name,
        cvr.bal_num,
        cvr.bal_juris,
        cvr.office_cd,
        cvr.offic_dscr,
        cvr.juris_cd,
        cvr.juris_dscr,
        cvr.dist_no,
        cvr.off_s_h_cd,
        cvr.sup_opp_cd
    from rdata.ca_campaign ca
        left join rdata.ca_campaign_cvr cvr
            on ca.filing_id = cvr.filing_id
            and ca.amend_id = cvr.amend_id
")
}

shadow_ca_campaign_old <- function() {
    files <- grep(
        "ca_campaign.*\\.csv$",
        list.files(config("shadowdir"), full.names = TRUE),
        value = TRUE
    )

    read_ca <- function(filename) {
        readr::read_csv(
            filename,
            col_types = readr::cols_only(
                entity_id = readr::col_integer(),
                rcpt_date = readr::col_date(format = ""),
                amount = readr::col_double(),
                ctrib_dscr = readr::col_character(),
                filer_naml = readr::col_character(),
                cand_naml = readr::col_character(),
                cand_namf = readr::col_character(),
                bal_name = readr::col_character(),
                bal_num = readr::col_character(),
                bal_juris = readr::col_character(),
                office_cd = readr::col_character(),
                offic_dscr = readr::col_character(),
                juris_cd = readr::col_character(),
                juris_dscr = readr::col_character(),
                dist_no = readr::col_character(),
                off_s_h_cd = readr::col_character(),
                sup_opp_cd = readr::col_character(),
                total_contributions = readr::col_double()
            )
        )
    }

    all_ca <- lapply(files, read_ca)
    dplyr::bind_rows(all_ca)
}
