#' @export
shadow_sec <- function() {
    getcdw::get_cdw(
"
select
    dict.cik,
    dict.entity_id,
    hdr.ticker_neat as ticker,
    hdr.filing_date as report_date,
    hdr.issuer_name as company_name,
    hdr.is_director as director,
    hdr.is_officer as officer,
    hdr.is_ten_percenter as ten_percenter,
    max(case when nd.direct_indirect = 'D' then nd.post_transaction_shares else 0 end) as direct_shares,
    max(case when nd.direct_indirect = 'I' then nd.post_transaction_shares else 0 end) as indirect_shares,
    max(case when nd.direct_indirect = 'D' then nd.post_transaction_shares else 0 end) +
      max(case when nd.direct_indirect = 'I' then nd.post_transaction_shares else 0 end) as total_shares,
    max(price_share) as price,
    max(price_share) *
      (max(case when nd.direct_indirect = 'I' then nd.post_transaction_shares else 0 end) +
       max(case when nd.direct_indirect = 'D' then nd.post_transaction_shares else 0 end)) as total_value
from       rdata.sec_cik_dict dict
inner join rdata.sec_hdr hdr
        on dict.cik = hdr.cik
left  join rdata.sec_nonderiv nd
        on hdr.accession = nd.accession
group by dict.cik,
         dict.entity_id,
         hdr.accession,
         hdr.ticker_neat,
         hdr.filing_date,
         hdr.issuer_name,
         hdr.is_director,
         hdr.is_officer,
         hdr.is_ten_percenter
")
}
