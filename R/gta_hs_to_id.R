gta_hs_to_id=function(hs.codes=NULL,
                      new.treatment.values=NULL,
                      treatment.unit=NULL, 
                      prior.treatment.values=NULL,
                      is.tariff.line.official=NULL,
                      inception.date=NA,
                      removal.date=NA,
                      intervention.id=NULL,
                      db.connection='pool'){
  
  param.check = list(hs.codes=hs.codes, 
                     new.treatment.values=new.treatment.values, 
                     treatment.unit=treatment.unit, 
                     prior.treatment.values = prior.treatment.values,
                     is.tariff.line.official=is.tariff.line.official,
                     inception.date=inception.date,
                     removal.date=removal.date,
                     intervention.id=intervention.id)
  
  if(length(intervention.id)!=1) stop('The parameter intervention.id must be of length 1. Please ensure also that this is an intervention id and not state.act.id.')
  if(any(sapply(param.check, is.null)==T)) stop(paste0('The following vector provided is NULL: ', names(param.check[sapply(param.check, is.null)==T])))
  len = sapply(param.check, function(x) length(x)) 
  if(! all(len == 1 | len ==max(len))) stop('The provided vectors are of unsuitable length to form a dataframe.')
  
  unit.list=data.frame(level.unit.id=c(1:19),
                       level.unit=c("percent", "total budget (USD)", "firm-specific budget (USD)", "USD/MT", "USD/kg", "USD/tonne", "USD/unit", "count", "USD/litre", "USD/pc", "USD/squaremetre", "USD/pair", "USD/ldt", "USD/lb", "USD/gallon", "USD/hl", "USD/LAL", "USD/tyre", "USD/stick"),
                       stringsAsFactors = F)
  
  if(!all(unique(treatment.unit) %in% unit.list$level.unit)) stop(paste0('The following treatment unit is not interpretable: ',unique(treatment.unit)[!(unique(treatment.unit) %in% unit.list$level.unit)]))
  if(!all(unique(nchar(hs.codes))==6)) stop('All hs codes must be given as a character and must be 6 letters long.')
  if(!all(is.numeric(unique((c(new.treatment.values, prior.treatment.values)))))) stop('All provided values must be numeric.')
  if(length(unique(is.tariff.line.official))!=1 | !unique(is.tariff.line.official) %in% c(0,1)) stop('The parameter is.tariff.line.official must be a unique value and can only take the values 0 and 1.')
  
  
  #make sure the inception and removal dates are interpretable values 
  
  tryCatch({date=format(as.Date(x=inception.date),"%Y-%m-%d")},
           error=function(e){
             stop("Please specify a valid unique inception.date ('yyyy-mm-dd'). Default is NA.")
           })
  
  
  tryCatch({date=format(as.Date(x=removal.date),"%Y-%m-%d")},
           error=function(e){
               stop("Please specify a valid unique removal.date ('yyyy-mm-dd'). Default is NA.")
          })
  
  inception.date=as.Date(inception.date)
  removal.date=as.Date(removal.date)  
  

  treatment.unit=plyr::mapvalues(treatment.unit, unit.list$level.unit, unit.list$level.unit.id)
  up.tar <<- data.frame(intervention.id=intervention.id,
                      tariff.line.code=hs.codes,
                      tariff.line.code.4=substr(hs.codes,1,4),
                      prior.level=prior.treatment.values,
                      new.level=new.treatment.values,
                      tariff.peak=0,
                      is.positively.affected=0,
                      is.tariff.line.official=is.tariff.line.official,
                      inception.date=inception.date,
                      removal.date=removal.date,
                      unit=treatment.unit
                      )
  
  gta_sql_create_table(write.df = 'up.tar',
                       append.existing = F,
                       contains.data = T)
  
  insert.query=paste('INSERT INTO gta_affected_tariff_line (intervention_id,tariff_line_code,tariff_line_code_4,prior_level,new_level,tariff_peak,is_positively_affected,is_tariff_line_official,inception_date,removal_date,unit)',
                     'SELECT * FROM gta_up_tar;')
  gta_sql_get_value(insert.query)
  gta_sql_get_value('DROP TABLE gta_up_tar;')
  
}