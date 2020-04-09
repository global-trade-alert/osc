osc_pull_display_info=function(is.freelancer = NULL, user.id = NULL){
  if(is.null(is.freelancer) | length(is.freelancer)!= 1 | !is.logical(is.freelancer) | is.na(is.freelancer)) stop('is.freelancer must be false if you are an editor, or true if you are a freelancer, no other value permitted')
  
  # is.freelancer = T
  # library(gtalibrary)
  # library(gtasql)
  # library(pool)
  # gta_setwd()
  # gta_sql_pool_open(table.prefix = 'osc_',
  #                   db.title="ricardo",
  #                   db.host = gta_pwd("ricardomain")$host,
  #                   db.name = gta_pwd("ricardomain")$name,
  #                   db.user = gta_pwd("ricardomain")$user,
  #                   db.password = gta_pwd("ricardomain")$password)
  
 
  if(is.freelancer == T){
    # attach only those urls in the bt_hint_url which are suggested by bastiat OR accepted by editor on the other end
    pull.display = paste0("
                          SELECT hints_flancer.hint_id, bt_jurisdiction_list.jurisdiction_name, GROUP_CONCAT(DISTINCT(bt_url_log.url) SEPARATOR ' ; ') AS url,
                          bt_url_type_list.url_type_name, osc_file_log.file_path, GROUP_CONCAT(DISTINCT(osc_hint_comment_log.comment)  ORDER BY osc_hint_comment_log.time_stamp DESC SEPARATOR ' ; ') AS comment 
                          FROM 
                            (SELECT DISTINCT(bt_hint_processing.hint_id) FROM bt_hint_processing
                            JOIN bt_hint_log ON bt_hint_log.hint_id = bt_hint_processing.hint_id
                            JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id
                            WHERE bt_hint_processing.user_id = ",user.id," AND bt_hint_state_list.hint_state_name = 'OSC - freelancer desk') hints_flancer
                            JOIN bt_hint_jurisdiction ON hints_flancer.hint_id = bt_hint_jurisdiction.hint_id
                            JOIN bt_jurisdiction_list ON bt_hint_jurisdiction.jurisdiction_id = bt_jurisdiction_list.jurisdiction_id
                            JOIN bt_hint_url ON hints_flancer.hint_id = bt_hint_url.hint_id AND bt_hint_url.url_accepted != 0
                            JOIN bt_url_log ON bt_url_log.url_id = bt_hint_url.url_id
                            JOIN bt_url_type_list ON bt_hint_url.url_type_id = bt_url_type_list.url_type_id
                            LEFT JOIN osc_hint_file ON osc_hint_file.hint_id = hints_flancer.hint_id
                            LEFT JOIN osc_file_log ON osc_hint_file.file_id = osc_file_log.file_id
                            LEFT JOIN osc_hint_comment_log ON osc_hint_comment_log.hint_id = hints_flancer.hint_id
                            GROUP BY hints_flancer.hint_id, bt_hint_url.url_type_id;")
  } else {
    #attach only those urls which are non-dormant, i.e. those hints @osc editor desk & search_id non null & was_accepted null (pending decision) or 1
    pull.display = paste0("SELECT hints_editor.hint_id, bt_jurisdiction_list.jurisdiction_name, GROUP_CONCAT(DISTINCT(bt_url_log.url) SEPARATOR ' ; ') AS url,
                            bt_url_type_list.url_type_name, osc_file_log.file_path, GROUP_CONCAT(DISTINCT(osc_hint_comment_log.comment)  ORDER BY osc_hint_comment_log.time_stamp DESC SEPARATOR ' ; ') AS comment 
                            FROM 
                            (SELECT DISTINCT(bt_hint_processing.hint_id) FROM bt_hint_processing
                            JOIN bt_hint_log ON bt_hint_log.hint_id = bt_hint_processing.hint_id
                            JOIN bt_hint_state_list ON bt_hint_log.hint_state_id = bt_hint_state_list.hint_state_id
                            WHERE bt_hint_processing.user_id = ",user.id," AND bt_hint_state_list.hint_state_name = 'OSC - editor desk') hints_editor
                            JOIN bt_hint_jurisdiction ON hints_editor.hint_id = bt_hint_jurisdiction.hint_id
                            JOIN bt_jurisdiction_list ON bt_hint_jurisdiction.jurisdiction_id = bt_jurisdiction_list.jurisdiction_id
                            JOIN bt_hint_url ON hints_editor.hint_id = bt_hint_url.hint_id AND bt_hint_url.url_accepted != 0 AND bt_hint_url.classification_id IS NOT NULL
                            JOIN bt_url_log ON bt_url_log.url_id = bt_hint_url.url_id
                            JOIN bt_url_type_list ON bt_hint_url.url_type_id = bt_url_type_list.url_type_id
                            LEFT JOIN osc_hint_file ON osc_hint_file.hint_id = hints_editor.hint_id
                            LEFT JOIN osc_file_log ON osc_hint_file.file_id = osc_file_log.file_id
                            LEFT JOIN osc_hint_comment_log ON osc_hint_comment_log.hint_id = hints_editor.hint_id
                            GROUP BY hints_editor.hint_id, bt_hint_url.url_type_id;")  
  }
  
  col.names = c('hint.id','jurisdiction.name','official','news','consultancy','other','file.path','comment')
  
  display = gta_sql_get_value(pull.display)
  if(nrow(display)>0){
    display = tidyr::spread(display, url.type.name, url)
  
    if(any(!col.names %in% names(display))){
      display[,col.names[!col.names %in% names(display)]] = NA
    }
  display = subset(display, select=col.names)
    
  }
  
  # gta_sql_pool_close()
  return(display)
}

