workspace <- 'C:/Users/gonpa/OneDrive/Documents/ChatGPT/wFDc_RQ'
.libPaths(c(file.path(workspace,'r-library'),.libPaths()))
setwd(file.path(workspace,'shiny-concordance'))
e<-new.env();sys.source('app.R',envir=e)
ref<-'C:/Users/gonpa/OneDrive - Universidad Hemisferios/2_Aims/3_Manuscripts/1p_wFDc_RQ/Db/Anlys/Outputs/Tables'
records<-list()
for(dataset in c('Guayas','Paute','AndeanLakes')){
 d<-read.csv(file.path(ref,paste0('Step4_',dataset,'_Residuals.csv')))
 f<-tempfile(fileext='.csv');write.csv(d,f,row.names=FALSE)
 group<-switch(dataset,Guayas=NULL,Paute='Site',AndeanLakes='Lake')
 expected<-e$compare_metrics(d,'wFDc','RQ','R',model=if(is.null(group))'quadratic' else 'gam',group=group)
 shiny::testServer(e$server,{
  session$setInputs(upload=data.frame(name='indices.csv',size=file.info(f)$size,type='text/csv',datapath=f),
    separator=',',predictor='RQ',response='wFDc',grouped=!is.null(group),group=group,
    control=TRUE,richness='R',model=if(is.null(group))'quadratic' else 'gam',k=5,rationale=TRUE)
  session$flushReact()
  session$setInputs(calculate=1);session$flushReact()
  if(!is.null(state$error))stop(state$error)
  stopifnot(!is.null(state$result),state$result$n==nrow(d),
   isTRUE(all.equal(state$result$agreement,expected$agreement)),
   max(abs(state$result$observations$response_residual-expected$observations$response_residual))<1e-10,
   all(file.exists(file.path(state$export$directory,state$export$files))))
  ss<-state$export$summary
  stopifnot(!any(grepl('Spearman P-value',ss$Statistic,fixed=TRUE)),
            is.null(state$result$spearman$p.value),is.null(state$result$residual_spearman$p.value),
            abs(unname(state$result$spearman$estimate)-cor(d$wFDc,d$RQ,method='spearman'))<1e-12)
  sm<-summary(expected$continuous_model)
  if(is.null(group)){
    fs<-sm$fstatistic;mp<-unname(stats::pf(fs[1],fs[2],fs[3],lower.tail=FALSE))
    stopifnot(ss$Value[ss$Statistic=='Overall model P-value (F-test)']==mp)
  }else stopifnot(ss$Value[ss$Statistic=='Predictor smooth P-value']==sm$s.table['s(x)','p-value'])
  bundle<-tempfile(fileext='.tar.gz');old<-setwd(state$export$directory)
  utils::tar(bundle,files=list.files('.'),compression='gzip',tar='internal');setwd(old)
  stopifnot('OPEN_REPORT.html' %in% utils::untar(bundle,list=TRUE))
  session$setInputs(control=FALSE);session$flushReact()
  stopifnot(is.null(state$result),is.null(state$export))
  session$setInputs(calculate=2);session$flushReact()
  stopifnot(is.null(state$error),!is.null(state$result),is.null(state$result$residual_spearman))
 })
 records[[dataset]]<-data.frame(dataset,n=nrow(d),status='PASS')
}
# Check that app functions are exactly the checked release functions.
for(nm in c('compare_metrics.R','export_comparison.R'))
 stopifnot(identical(readBin(file.path('core',nm),'raw',n=1e7),
                     readBin(file.path(workspace,'release/fdconcordance/R',nm),'raw',n=1e7)))
out<-file.path(workspace,'outputs','shiny_concordance_checks');dir.create(out,showWarnings=FALSE)
write.csv(do.call(rbind,records),file.path(out,'server_checks.csv'),row.names=FALSE)
print(do.call(rbind,records));cat('Server results, optional richness, reset, archive and source identity checks passed.\n')
