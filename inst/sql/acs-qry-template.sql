with geography as (
  select
    addr.entity_id,
    geo.bg_geo_id,
    geo.tract_geo_id,
    row_number() over (
      partition by entity_id
      order by addr.primary_home_addr_ind desc,
               addr.addr_type_code desc,
               addr.xsequence desc) as rn
  from
    cdw.d_bio_address_mv addr
    inner join rdata.pd_address_shapes geo
      on addr.latitude = geo.latitude
      and addr.longitude = geo.longitude
      and not regexp_like(addr.street1, 'p\.?o\.? box', 'i')
  where
      addr.addr_type_code in ('6', 'H')
      and addr.addr_status_code in ('A', 'K')
      and addr.entity_id in (
          select entity_id from cdw.d_entity_mv
          where
              person_or_org = 'P'
              and record_status_code = 'A'
      )
)

select
  geography.entity_id,
  acs.*
from geography
inner join rdata.acs_pd_wealth_indicators_mv acs
  on geography.##geo##_geo_id = acs.geo_id
  and acs_version = '2012-2016'
  and geography.rn = 1
