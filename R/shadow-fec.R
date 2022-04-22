#' @export
shadow_fec <- function() {
getcdw::get_cdw("
    select
    entity_id,
    fec.cmte_id,
    fec.image_num,
    fec.transaction_tp,
    fec.transaction_dt,
    fec.transaction_amt,
    fec.sub_id,
    fec_committees.cmte_nm,
    fec_committees.cmte_dsgn,
    fec_committees.cmte_tp,
    fec_committees.cmte_pty_affiliation,
    fec_committees.org_tp,
    fec_committees.connected_org_nm,
    fec_committees.cand_id,
    fec_cmte_party.party_desc as party,
    fec_cmte_category.cmte_code,
    fec_cmte_category.catname as category,
    fec_cmte_category.catorder,
    fec_cmte_category.industry,
    fec_cmte_category.sector,
    case
    when fec_cmte_party.party_desc is null
    then fec_cmte_category.catname
    else fec_cmte_party.party_desc
    end as cause
    from
    rdata.fec
    left join rdata.fec_committees
    on fec.fec_cycle = fec_committees.fec_cycle
    and fec.cmte_id = fec_committees.cmte_id
    left join rdata.fec_cmte_party
    on fec_committees.cmte_pty_affiliation = fec_cmte_party.party_code
    left join rdata.fec_cmte_category
    on fec.fec_cycle = fec_cmte_category.fec_cycle
    and fec.cmte_id = fec_cmte_category.cmte_id
")
}


