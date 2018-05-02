#UTILITY FUNCTION: used for descriptives and bivariates

#provides bivariate summary
bivariateSummary<-function(varname,db, correctName=FALSE) {
  var2keep<-c("converted",varname,"quoteId")
  mydb<-db[,var2keep,with=FALSE]
  setnames(mydb,varname,"varX")
  mydb<-mutate(mydb, converted01=ifelse(converted=="N",0,1))
  mydb <- as(mydb,"data.frame")
  out<- group_by(mydb,varX) %>% summarise(num=n(),conversions=sum(converted01)) %>% mutate(ratio=conversions/num)
  if (correctName) names(out)[1]<-varname
  #out<-as(out,"data.frame")
  return(out)
}


#function to plot two parallel graphs
double_axis_graph <- function(graf1,graf2){
  require(gtable)
  graf1 <- graf1
  graf2 <- graf2
  gtable1 <- ggplot_gtable(ggplot_build(graf1))
  gtable2 <- ggplot_gtable(ggplot_build(graf2))
  par <- c(subset(gtable1[['layout']], name=='panel', select=t:r))
  graf <- gtable_add_grob(gtable1, gtable2[['grobs']][[which(gtable2[['layout']][['name']]=='panel')]],
                          par['t'],par['l'],par['b'],par['r'])
  ia <- which(gtable2[['layout']][['name']]=='axis-l')
  ga <- gtable2[['grobs']][[ia]]
  ax <- ga[['children']][[2]]
  ax[['widths']] <- rev(ax[['widths']])
  ax[['grobs']] <- rev(ax[['grobs']])
  
  ax[['grobs']][[1]][['x']] <- ax[['grobs']][[1]][['x']] - unit(1,'npc') + unit(0.15,'cm')
  
  graf <- gtable_add_cols(graf, gtable2[['widths']][gtable2[['layout']][ia, ][['l']]], length(graf[['widths']])-1)
  
  graf <- gtable_add_grob(graf, ax, par['t'], length(graf[['widths']])-1, par['b'])
  
  return(graf)
  
}

#creating a function to perform bivariate plots
doGraphs<-function(db, varX, textsize=10, rotationAngle=0) {
  require(scales) #to add percentages
  #summarize
  mySummary<-bivariateSummary(varname = varX,db = db)
  
  #base histogram
  basePlot<-ggplot(mySummary, aes(x = varX, y = num)) + geom_bar(stat = "identity", colour="steelblue", fill="steelblue")+ theme(axis.text.x = element_text(angle = 45, hjust = 1),text = element_text(size=textsize))
  basePlot <- basePlot + theme_bw() + theme(legend.position="top") +  theme(axis.text.x = element_text(angle = rotationAngle, hjust = 1))
  basePlot <- basePlot + labs(title=paste(varX,"vs","conversion rate"),x=varX,y="frequency")
  #conversion
  
  convPlot<-ggplot(mySummary, aes(x = varX, y = ratio)) + geom_line(group=varX,size=1.3) 
  convPlot <- convPlot + geom_point(size=3.8, shape=21, fill="white")
  convPlot <- convPlot + theme_bw() + theme(panel.grid=element_blank()) + theme(panel.background = element_rect(fill = NA))
  convPlot <- convPlot + scale_y_continuous(labels = scales::percent)
  #convPlot<-basePlot+geom_abline(intercept = meanConv, slope = 0,lty=5,col=myColour1)
  
  myDoublePlot<-double_axis_graph(graf1=basePlot,graf2 = convPlot)
  
  return(myDoublePlot)
}



prepare_db_xgboost<-function(df,x_vars, y_var, offset_var, weight_var, na_code) {
  require(Matrix)
  require(xgboost)
  #force df as data frame
  df<-as.data.frame(df)
  previous_na_action <- options('na.action')
  options(na.action='na.pass')
  
  #definisco le variabili da prendere
  supplementaryVars<-character()
  if (!missing(offset_var)) supplementaryVars<-c(supplementaryVars,offset_var)
  if (!missing(weight_var)) supplementaryVars<-c(supplementaryVars,weight_var)
  
  vars2Keep<-c(x_vars,y_var,supplementaryVars)
  
  df<-df[,vars2Keep]
  
  # Matrici sparse
  sparse_all<-sparse.model.matrix(object = ~.-1., data = df)
  options(na.action=previous_na_action$na.action)
  
  # only predictors cols
  predictors_cols<-setdiff(colnames(sparse_all),c(y_var,supplementaryVars))
  
  #creo la xgbmatrix, eventualmente ponderata / na
  
  if (!missing(na_code)) {
    if (missing(weight_var)) {
      db_xgb_out <- xgb.DMatrix(data =sparse_all[,predictors_cols], label=sparse_all[,y_var], missing = na_code )
    } else {
      db_xgb_out <- xgb.DMatrix(data =sparse_all[,predictors_cols], label=sparse_all[,y_var],weight = sparse_all[,weight_var], missing = na_code )
    }
  } else {
    if (missing(weight_var)) {
      db_xgb_out <- xgb.DMatrix(data =sparse_all[,predictors_cols], label=sparse_all[,y_var])
    } else {
      db_xgb_out <- xgb.DMatrix(data =sparse_all[,predictors_cols], label=sparse_all[,y_var],weight = sparse_all[,weight_var])
    }
  }
  
  # adding possible offset
  
  if(!missing(offset_var)) {
    setinfo(db_xgb_out,"base_margin",sparse_all[,offset_var])
  }
  
  return(db_xgb_out)
}

