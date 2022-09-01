library(data.table)
library(ggplot2)
library(ggthemes)
library(grid)
library(gridExtra)
library(scales)
library(GGally)
library(MASS)
library(gtable)
library(plyr)
library(car)
library(moments)
library(extrafont)


point <- format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)
point_format = function(){function (x) {
  if (length(x) == 0) 
    return(character())
  x <- round(x,2)
  point(x)
}
}
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="lavender", ...)
}
## put correlations & 95% CIs on the upper panels,
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y,use="complete.obs")
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  prefix <- "r = "
  rc <- cor.test(x,y)
  p <- rc$p.value
  mystars <- ifelse(p < .05, "* ", " ")
  
  txt <- paste(prefix, txt, mystars, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = 1.5)
}


analise.tema <-  function () { 
  return (theme ( axis.line         = element_blank()
                  , axis.text.x       = element_text ( colour = "black", size   = 8, margin = unit ( 1, "mm")) 
                  , axis.text.y       = element_text ( colour = "black", size   = 14,  margin = unit ( 1, "mm"))
                  , axis.ticks.x      = element_blank()
                  , axis.ticks.y      = element_blank() 
                  , axis.title.x      = element_blank() 
                  , axis.title.y      = element_blank() 
                  , axis.ticks.length = unit         ( 1, "mm")
                  , panel.background  = element_rect ( fill  = "bisque", color = NA)
                  , strip.background  = element_rect ( fill  = "bisque", color = NA)
                  , plot.background   = element_rect ( fill  = "white", color = NA)
                  , plot.margin       = unit         ( c (1,1,1,1), "mm" ) 
  )) }

analise.tema2 <-  function () { 
  return (theme ( axis.line         = element_blank()
                  , axis.text.x       = element_text ( colour = "black", size   = 8, margin = unit ( 1, "mm")) 
                  , axis.text.y       = element_text ( colour = "black", size   = 14,  margin = unit ( 1, "mm"))
                  , axis.ticks.x      = element_blank()
                  , axis.ticks.y      = element_blank() 
                  , axis.title.x      = element_blank() 
                  , axis.title.y      = element_blank() 
                  , axis.ticks.length = unit         ( 1, "mm")
                  , panel.background  = element_rect ( fill  = "bisque", color = NA)
                  , strip.background  = element_rect ( fill  = "bisque", color = NA)
                  , plot.background   = element_rect ( fill  = "#F0F0F0", color = NA)
                  , plot.margin       = unit         ( c (1,1,1,1), "mm" ) 
  )) }


x.vs.y       <- function (data, x.title, y.title) {
  ggplot(data, aes(x=x,y=y)) + 
    geom_point() +
    xlab(x.title) +
    ylab(y.title)+
    geom_smooth() +
    theme_bw() +
    theme (text       = element_text ( face='plain', size=11 ),
           axis.title = element_text ( face='plain', size=11 ),
           plot.title = element_text ( face='plain', size=11 ))
}
largura      <- function (p) {
  if ( class(p)[1] == "gtable" )  
    return ( convertUnit ( gtable_width(p) ,"mm", valueOnly=TRUE ))
  else
    return (convertUnit( widthDetails(p) ,"mm", valueOnly=TRUE ))
}
razaoL       <- function (lista, item) return(largura(lista[[item]])/(sum(sapply(X=lista,FUN=largura))))
proporcoesL  <- function (lista) return(sapply(X=seq(1:length(lista)),FUN=razaoL,lista=lista))
altura       <- function(p) {
  if ( class(p)[1] == "gtable" )  
    return ( convertUnit ( gtable_height(p) ,"mm", valueOnly=TRUE ))
  else
    return ( convertUnit ( heightDetails(p) ,"mm", valueOnly=TRUE ))
}
razaoA       <- function (lista, item) return(altura(lista[[item]])/(sum(sapply(X=lista,FUN=altura))))
proporcoesA  <- function (lista) return(sapply(X=seq(1:length(lista)),FUN=razaoA,lista=lista))
signif.code  <- function(x) { 
  sapply(x,function(x) if ( !is.nan(x) ) 
    if      ( x <= 0.001 ) "***"
    else if ( x <= 0.01  ) "** "
    else if ( x <= 0.05  ) "*  "
    else if ( x <= 0.1   ) ".  "
    else                   "   " )
} 
tema         <- function(show) {
  theme.default ( gpar.corefill = gpar ( fill = "white", col = "#135DA2" ),
                  gpar.rowfill  = gpar ( fill = "white", col = "#135DA2" ),
                  gpar.colfill  = gpar ( fill = "white", col = "#135DA2" ),
                  gpar.coltext  = gpar ( fontsize = 11 ),             
                  gpar.rowtext  = gpar ( fontsize = 11  , fontface="bold" ),
                  gpar.coretext = gpar ( fontsize = 11 ),
                  core.just     = "center",
                  show.rownames = show,
                  equal.height  = TRUE,
                  padding.v = unit(6, "mm")
  )   
}
lm.eqn       <- function(m) {  
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}
lm.eqn.df     <- function(df) {
  m = lm(y ~ x, df)
  lm.eqn (m)  
}
formatPercentual <- function (x,d) {
  return ( paste(format(x*100,
                        nsmall=d,
                        digits=d, 
                        decimal.mark=","),'%', sep = "") )     
}  
formatQuantidade   <- function ( x, n, ns=n ) { 
  return ( format(x, 
                  digits=1, 
                  decimal.mark=",",
                  big.mark=".",
                  big.interval=3,
                  nsmall=ns) )
}
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
recode <- function(var, old, new) {
  x <- replace(var, var == old, new)
  if(is.factor(x)) factor(x) else x
}
dw_critical  <- function (reg, n) {    
  dw.table <<-read.table("~/dwtable.txt", header = TRUE, dec=".")
  x=as.numeric(dw.table [ dw.table$K >= reg & dw.table$T >= n, ][1,c(3,4)])  
}
fancy.dist <- function (  df
                          , colunaAlvo
                          , tituloAlvo  = colunaAlvo
                          , df.sombra = 0
                          , numeroRef = c(0)
                          , estiloRef = "P"
                          , Piso  = round ( min (df[!(is.na(df[,colunaAlvo])),colunaAlvo] ) - 1 )
                          , Teto  = round ( max (df[!(is.na(df[,colunaAlvo])),colunaAlvo] ) + 1 )
                          , Passo = round ( ( round ( max (df[!(is.na(df[,colunaAlvo])),colunaAlvo] ) + 1 ) 
                                              - round ( min (df[!(is.na(df[,colunaAlvo])),colunaAlvo] ) - 1 ) 
                          ) / 10 )
                          , corte.X             = Teto + Passo
                          , corte.Y             = 1.1
                          , numeroClasses      = 0
                          , incluiStatEcdf     = T
                          , incluiStatBin      = F
                          , incluiPareto       = F
                          , incluiQuartis      = T
                          , incluiNormalidade  = F
                          , incluiResumo       = T
                          , incluiMean         = F
                          , quartil1           = .25, quartil1Nome = "1º Quartil"
                          , quartil2           = .50, quartil2Nome = "Mediana"
                          , quartil3           = .75, quartil3Nome = "3º Quartil"
                          , outputScreen       = T
                          , outputFile         = T
                          , limiares           = NULL
                          , limite.y           = 1
                          , labels             = round ( seq ( Piso, Teto + Passo, Passo ), 3)
                          , lang = "pt-br"
) {
  
  if ( lang == "es") {
    quartil1Nome = "1º Cuartil"
    quartil3Nome = "3º Cuartil"
    tituloAlvo = gsub("[/]","y", tituloAlvo)
  }
  else
  {
    tituloAlvo = gsub("[/]","e", tituloAlvo)
  }

  
  setTimeLimit        ( cpu = 9999999999999999999, elapsed = 9999999999999999999, transient = FALSE)
  setSessionTimeLimit ( cpu = 9999999999999999999, elapsed = 99999999999999999999999999999)
  
  globalColunaAlvo <<- data.frame(df[!(is.na(df[,colunaAlvo])),colunaAlvo])
  names(globalColunaAlvo) = c(colunaAlvo)
  globalTituloAlvo <<- tituloAlvo
  
  if ( incluiMean ) {
    tt = t.test  ( globalColunaAlvo )
    df.mean <<-
      data.frame (
        Media.Inferior = round ( tt$conf.int[1] , 3)
        , Media.Estimada = round ( tt$estimate    , 3)
        , Media.Superior = round ( tt$conf.int[2] , 3)
        , Media.p.value  = round ( tt$p.value     , 3)
      )
  }
  xP0 <<- hist ( as.numeric ( globalColunaAlvo[,1] )
               , right  = FALSE
               , breaks = seq ( Piso, Teto+Passo, Passo )
               , plot   = FALSE
  )
  
  dados.resumo.distribuicao <<- 
    data.frame ( xP0$breaks[1:length(xP0$counts)]
                 , xP0$counts
                 , formatPercentual ( xP0$counts         / length ( globalColunaAlvo[,1] ), 2 )
                 , formatPercentual ( cumsum(xP0$counts) / length ( globalColunaAlvo[,1] ), 2 )
                 , xP0$breaks[1:length(xP0$counts)]  ) 
  
  names(dados.resumo.distribuicao) <<- c ( "Intervalo"
                                           , "Casos"
                                           , "Frequência (%)"
                                           , "Frequência (%)\nCumulativa"
                                           , "Limiar"
  )
  if (lang == "es") 
    names(dados.resumo.distribuicao) <<- c ( "Rango"
                                             , "Casos"
                                             , "Frecuencia (%)"
                                             , "Frecuencia (%)\nAcumulativa"
                                             , "Limiar"
                                             
    )
    
  require ( nortest )
  if (incluiNormalidade) {
    
    W=shapiro.test     ( globalColunaAlvo [,1] )
    if ( nrow ( globalColunaAlvo ) > 7 )
      A=ad.test          ( globalColunaAlvo [,1] )
    else {
      A=list ( statistic=c(t=NA)
               , parameters=c(df=NA)
               , p.value=1
               , method="Anderson-Darling normality test"
               , data.name= ""
               , alternative=c("") )
      class(A)<-"htest"      
    }  
    D=lillie.test      ( globalColunaAlvo [,1] )
    
    dados.teste.normalidade <<- 
      rbind (  data.frame ( method = strtrim(W$method,nchar(W$method)-15), statistic = round(W$statistic,3), p.value = paste ( round ( W$p.value, 3 ), signif.code ( W$p.value ) ) )
               ,  data.frame ( method = strtrim(A$method,nchar(A$method)-15), statistic = round(A$statistic,3), p.value = paste ( round ( A$p.value, 3 ), signif.code ( A$p.value ) ) )
               ,  data.frame ( method = strtrim(D$method,nchar(D$method)-15), statistic = round(D$statistic,3), p.value = paste ( round ( D$p.value, 3 ), signif.code ( D$p.value ) ) )
      )
    
    names(dados.teste.normalidade) = c ( "Método"
                                         , "Estatística"
                                         , "valor-p"
    )
    if (lang == "es") 
      names(dados.teste.normalidade) = c ( "Método"
                                           , "Estadística"
                                           , "valor-p"
      )
    
  }
  
  if ( numeroClasses != 0 ) {
    
    dados.resumo.tail <<- tail ( dados.resumo.distribuicao
                                 , nrow ( dados.resumo.distribuicao ) - (numeroClasses-1) 
    )
    
    dados.resumo.tail <<- 
      data.frame ( min ( dados.resumo.tail [,1] ) 
                   , sum ( dados.resumo.tail [,2] )
                   , formatPercentual ( sum ( dados.resumo.tail [,2] ) 
                                        / length ( globalColunaAlvo[,1] ), 2 ) 
                   , formatPercentual ( 1, 2 )
                   , min ( dados.resumo.tail[,5]) )
    
    names ( dados.resumo.tail ) <<- names ( dados.resumo.distribuicao )
    
    dados.resumo.head <<- head ( dados.resumo.distribuicao, (numeroClasses-1) )
    
    names ( dados.resumo.head ) <<- names ( dados.resumo.distribuicao )
    
    rm ( dados.resumo.distribuicao, envir=.GlobalEnv )
    rm ( dados.resumo.distribuicao )
    
    dados.resumo.distribuicao <<- 
      rbind ( dados.resumo.head 
              , dados.resumo.tail  
      )
    
    rm ( dados.resumo.head, envir=.GlobalEnv )
    rm ( dados.resumo.tail, envir=.GlobalEnv )
    
    dados.resumo.distribuicao [  numeroClasses, 1] <<- 
      paste ( "≥"   , as.numeric ( dados.resumo.distribuicao[  numeroClasses, 1 ] ) ) 
    
    dados.resumo.distribuicao [ -numeroClasses, 1] <<- 
      paste ( "≥"   , as.numeric ( dados.resumo.distribuicao[ -numeroClasses, 1 ] )
              , "e <" , as.numeric ( dados.resumo.distribuicao[ -numeroClasses, 1 ] ) + Passo
      ) 
    
    
  } 
  else {
    dados.resumo.distribuicao [,1] <<- 
      paste ( "≥"   , round(dados.resumo.distribuicao[,1],3)
              , "e <" , round(dados.resumo.distribuicao[,1],3) + Passo
      ) 
  }

  tema = 
    ttheme_default ( 
      core = list ( fg_params = list ( fontsize = 11, col  = "dodgerblue4" )
                    , bg_params = list ( fill = c ("#0072B2","white"), col  = "gray50", alpha=0.1 )
                    , show.vlines = FALSE
      )
      , colhead  = list ( fg_params = list ( fontsize = 11, fontface = "bold", col = "dodgerblue4" )
                          , bg_params = list ( fill = "#0072B2", col  = "gray50", alpha=0.3 )
                          , show.vlines = FALSE
      )
    ) 
  
  tabela.resumo.distribuicao <<- 
    tableGrob ( dados.resumo.distribuicao[dados.resumo.distribuicao$Casos!=0,-5]
                , rows = NULL
                , theme = tema
    ) 
  
  if ( incluiMean ) {
    mean.plot <<- 
      ggplot ( data = df.mean
               , aes ( ymin = Media.Inferior
                       , ymax = Media.Superior
                       , y    = Media.Estimada
                       , x    = corte.Y - .1 ) ) +       
      geom_errorbar( colour = "#135DA2" ) +
      geom_point ( size = 4, colour="orangered4" ) +
      geom_text  ( aes ( y=Media.Inferior, label = round(Media.Inferior,2))                                  , size=4, vjust   = +0.5, hjust  =  +1.2, colour = "#135DA2" )+
      geom_text  ( aes ( y=Media.Superior, label = paste(round(Media.Superior,2),signif.code(Media.p.value))), size=4, vjust   = +0.5, hjust  =  -0.2, nudge_y = 1, colour = "#135DA2" )
  }
  else
  {
    box.plot <<-
      ggplot ( globalColunaAlvo, aes(x=corte.Y - .1, y=globalColunaAlvo[,1])) +
      geom_boxplot       ( outlier.colour = "#135DA2"
                           , outlier.size   = 2 
                           , fill           = "#DC8E05"
                           , colour         = "#135DA2"
                           , alpha          = 0.50
      ) +
      geom_violin(alpha=.1)
  }
  if ( incluiMean )
    top.plot <<- mean.plot
  else 
    top.plot <<- box.plot 

  if ( lang == "es")
    xlab1 <<- "Frecuencia porcentual (Casos por rango)"
  else
    xlab1 <<- "Frequência Percentual (Casos por Faixa)"
  
  if ( lang == "es")
    subtitle <<- "Estadística descriptiva"
  else
    subtitle <<- "Estatísticas Descritivas"
  
  top.plot <<-  top.plot +  
    theme_bw() +    xlab ( xlab1 ) +
    
    ggtitle ( label    = globalTituloAlvo
              , subtitle = subtitle) +
    coord_flip         ( ylim = c ( Piso, corte.X )) +
    scale_y_continuous ( limits = c ( Piso, Teto + Passo ), breaks = seq ( Piso, Teto + Passo, Passo ) ) +
    scale_x_continuous ( labels = percent_format() ) +  
    theme ( rect=element_rect(colour="white")
            , axis.title.x    = element_text ( colour = "white" )
            , axis.text.y     = element_text ( colour = "white" )
            , axis.line.y     = element_blank()
            , axis.ticks.y    = element_blank()
            , axis.title.y    = element_text ( colour     = "white"
                                          , size       = 0.000000000000001
                                          , lineheight = 0.000000000000001
            )
            , axis.text.x     = element_text ( colour     = "white"
                                            , size       = 0.000000000000001
                                            , lineheight = 0.000000000000001
            )
            , axis.line.x     = element_blank()
            , axis.ticks.x    = element_blank()
            , legend.spacing = unit ( 0, "mm" )
            , legend.title =  element_text ( colour     = NA
                                             , size       = 0.000000000000001
                                             , lineheight = 0.000000000000001
            )
            , panel.grid.major.y = element_blank()
            , panel.grid.major.x = element_blank()
            , panel.grid.minor.y = element_blank()
            , panel.grid.minor.x = element_blank()
            , panel.spacing  = unit ( 0, "mm" )
            , panel.border=element_rect(colour=NA)
            , plot.margin   = unit ( c ( 0, 2, 0, 2 ), "mm"  )
            , panel.background   = element_rect ( fill  = "#ECECEC", color = "#CFCFCF")
            , plot.title = theme_fivethirtyeight()$plot.title
    )
  
  quadro.resumo.distribuicao <<- 
    ggplot ( globalColunaAlvo, aes_string ( x = colunaAlvo ) )  
  
  if ( df.sombra != 0 ) {
    quadro.resumo.distribuicao <<-
      quadro.resumo.distribuicao +
      geom_histogram  ( data = df.sombra
                        , aes_string ( x = colunaAlvo,
                                       y = "..count../sum(..count..)")
                        , breaks = seq ( Piso, Teto + Passo, Passo )
                        , closed ="left"
                        , fill     = "#135DA2"
                        , colour   = "#135DA2"
                        , alpha    = 1 
      )
  }
  
  quadro.resumo.distribuicao <<- quadro.resumo.distribuicao +
    geom_histogram  ( breaks = seq ( Piso, Teto + Passo, Passo )
                      , closed ="left"
                      , fill     = "#dd8e04"
                      , colour   = "#dd8e04"
                      , alpha    = 0.40
                      , aes ( y = ..count../sum(..count..)) ) +
    scale_x_continuous (  limits = c ( Piso, Teto + Passo )
                          ,  breaks = seq ( Piso, Teto + Passo, Passo )
                          , labels = labels
    ) + 
    coord_cartesian ( xlim = c ( Piso, corte.X )
                      , ylim = c ( 0   , corte.Y ) ) +
    scale_y_continuous ( labels = percent_format()
                         , limits = c (0 , limite.y + .05)
                         , breaks = seq ( 0, limite.y, .1 ) ) +
    
    ylab               ( "Frequência Percentual (Casos por Faixa)" ) +
    theme_bw() +
    theme ( legend.spacing     = unit ( 0, "mm" )
            , axis.title.y    = element_text (angle = 90, colour = "dodgerblue4")
            , axis.title.x    = element_text (colour = NA)
            , axis.text.x     = element_text (angle = 90, colour = "dodgerblue4")
            , axis.text.y     = element_text (colour = "dodgerblue4")
            , axis.line.x     = element_blank()
            , axis.line.y     = element_blank()
            , axis.ticks.x    = element_blank()          
            , axis.ticks.y    = element_blank()
            , panel.spacing       = unit ( 0, "mm" ) 
            , panel.grid.major.y = element_line(colour="#CFCFCF")
            , panel.grid.major.x = element_blank()
            , panel.grid.minor.y = element_blank()
            , panel.grid.minor.x = element_blank()
            , plot.margin        = unit ( c ( 0, 2, 0, 2 ), "mm"  ) 
            , panel.background   = element_rect ( fill  = "#ECECEC", color = "#CFCFCF")
            , panel.border=element_rect(colour=NA)
    ) 
  
  if ( incluiPareto ) {
    n.p50    = median ( globalColunaAlvo[,1] )
    n.p80    = quantile (globalColunaAlvo[,1],.8)
    n.p20    = quantile (globalColunaAlvo[,1],.2)
    n.min    = min ( globalColunaAlvo[,1] )
    n.max    = max ( globalColunaAlvo[,1] )
    
    v.A = sum ( globalColunaAlvo [ globalColunaAlvo[,1] >  n.p80 , 1] )
    v.B = sum ( globalColunaAlvo [ globalColunaAlvo[,1] >  n.p50 
                                   & globalColunaAlvo[,1] <= n.p80 , 1] )
    v.C = sum ( globalColunaAlvo [ globalColunaAlvo[,1] <= n.p50 , 1] )
    v   = sum (globalColunaAlvo[,1])
    
    df.pareto <<- data.frame ( x = c("A(20%)", "B(30%)", "C(50%)")
                               , y = c( v.A / v, v.B / v, v.C / v)
                               , z = c (n.p80, n.p50, "-")
    )
    quadro.resumo.distribuicao <<-
      quadro.resumo.distribuicao +
      geom_errorbarh ( aes ( y     = corte.Y - .09
                           , xmin  = Piso
                           , xmax  = n.p50
      )
      , size = .25
      , colour = "#135DA2"
      )  +
      geom_segment ( aes ( y     = corte.Y - .09, yend=corte.Y - .09
                           , x  = n.p50
                           , xend  = n.p80
      )
      , size = .25
      , colour = "#135DA2"
      )  +
      geom_errorbarh ( aes ( y     = corte.Y - .09
                           , xmin  = n.p80
                           , xmax  = Teto
      )
      , size = .25
      , colour = "#135DA2"
      )  +
      geom_text   ( aes ( x = ( n.min + n.p50 ) / 2, y = corte.Y - .1, label  = "C" )     
                    , size   = 4 
                    , hjust  = NA
                    , vjust = -1
                    , colour = "#135DA2"
      ) +
      geom_text   ( aes ( x = ( n.p50 + n.p80 ) / 2, y = corte.Y - .1, label  = "B" )   
                    , size   = 4 
                    , hjust  = NA
                    , vjust = -1
                    , colour = "#135DA2"
      ) +
      geom_text   ( aes ( x = ( n.p80 + corte.X ) / 2, y = corte.Y - .1 , label = "A" )     
                    , size   = 4 
                    , hjust = NA
                    , vjust = -1
                    , colour = "#135DA2"
      ) +
      geom_segment ( aes ( y = 0.95, yend = 1.05
                           , x = n.p80, xend  = n.p80
      )
      , colour = "#135DA2"
      , alpha  = .9
      ) +
      geom_segment ( aes ( y = 0.95, yend = 1.05
                           , x = n.p50, xend  = n.p50
      )
      , colour = "#135DA2"
      , alpha  = .9
      )  +
      geom_segment    ( aes ( x = x, y = y, xend = xend, yend = yend ), data.frame ( x = n.p80, y = 0.0, xend = n.p80, yend = 1 )
                        , linetype = "twodash", colour = "#135DA2", size = .5 )+  
      geom_segment    ( aes ( x = x, y = y, xend = xend, yend = yend ), data.frame ( x = n.p50, y = 0.0, xend = n.p50, yend = 1 )
                        , linetype = "twodash", colour = "#135DA2", size = .5 )   
    
    
    quadro.normal <<- 
      ggplot      ( df.pareto
                    , aes ( x    = 1
                            , y    = y 
                            , fill = x) 
      ) +
      geom_bar    ( stat= "identity"#, position = "fill"
                    , width = 1 
                    ,  fill  = head ( c ( "#0072B2"
                                          , "#DC8E05"
                                          , "#BBC4D0"), 3)
      )  + 
      geom_text   ( aes_string ( x     = "1.25"
                                 , label = names(df.pareto)[1]
                                 , y     = "y/2 + c(0,cumsum(y)[-length(y)])"
                                 , hjust = 0
      )
      , size = 4
      ) +
      scale_x_continuous (  limits = c ( Piso, Teto + Passo )
                            ,  breaks = seq ( Piso, Teto + Passo, Passo )
                            , labels = labels
      )
      coord_polar ( theta = "y") +
      theme ( axis.line =         element_blank() 
              , axis.text.x  = element_blank() 
              , axis.text.y  = element_blank() 
              , axis.ticks.x = element_blank() 
              , axis.ticks.y = element_blank() 
              , axis.title.x = element_blank() 
              , axis.title.y = element_blank() 
              , panel.spacing      = unit ( 1, "mm" )
              , panel.background  = element_rect ( fill  = NA, color = NA )
              , plot.background   = element_rect ( fill  = NA, color = NA )
              , plot.margin       = unit ( c ( 0, 0 , -5 , -5 ), "mm"  ) 
      )
    
    df.pareto$y <<- formatPercentual(df.pareto$y,2)
    names(df.pareto) <<- c("Os % dos\ncasos", "Que correspondem\na % dos valores", "Corte")
    if ( lang == "es" )
      names(df.pareto) <<- c("Los % de\nlos casos", "Que corresponden\al % de valores", "Corte")
    
    tabela.teste.normalidade <<-
      tableGrob ( df.pareto
                  , rows = NULL
                  , theme = tema      
      ) 
    
  } 
  else
  {
    if ( incluiQuartis ) {
      quadro.resumo.distribuicao <<-
        quadro.resumo.distribuicao +
        geom_segment    ( aes ( x = x, y = y, xend = xend, yend = yend ), data.frame ( x = Piso, y = quartil3, xend = quantile ( globalColunaAlvo[,1], quartil3 ), yend = quartil3 )
                          , linetype = "twodash", colour = "#135DA2", size = .5 ) +
        geom_segment    ( aes ( x = x, y = y, xend = xend, yend = yend ), data.frame ( x = quantile ( globalColunaAlvo[,1], quartil3 ) , y    = 0.0, xend = quantile ( globalColunaAlvo[,1], quartil3 ), yend = quartil3 )
                          , linetype = "twodash", colour = "#135DA2", size = .5 ) +  
        geom_segment    ( aes ( x = x, y = y, xend = xend, yend = yend ), data.frame ( x = Piso, y = quartil1, xend = quantile ( globalColunaAlvo[,1], quartil1 ), yend = quartil1 )
                          , linetype = "twodash", colour = "#135DA2", size = .5 ) +
        geom_segment    ( aes ( x = x, y = y, xend = xend, yend = yend ), data.frame ( x = quantile ( globalColunaAlvo[,1], quartil1 ), y = 0.0, xend = quantile ( globalColunaAlvo[,1], quartil1 ), yend = quartil1 )
                          , linetype = "twodash", colour = "#135DA2", size = .5 ) + 
        geom_segment    ( aes ( x = x, y = y, xend = xend, yend = yend ), data.frame ( x = Piso, y = quartil2, xend = quantile ( globalColunaAlvo[,1], quartil2 ), yend = quartil2 )
                          , linetype = "solid", colour   = "#135DA2", size = .5 ) +
        geom_segment    ( aes ( x = x, y = y, xend = xend, yend = yend ), data.frame ( x = quantile ( globalColunaAlvo[,1], quartil2 ), y = 0.0, xend = quantile ( globalColunaAlvo[,1], quartil2 ), yend = quartil2 )
                          , linetype = "solid", colour   = "#135DA2", size = .5 )  
    }    
  }
  if ( ! is.null( limiares  ) ) {
    
    df.limiares <<- data.frame()
    for ( i in 2:length ( limiares ) ) {
      df.limiares <<- 
        rbind ( df.limiares 
                , data.frame ( x     = limiares [ i - 1 ]
                               , xend  = limiares [ i ]
                               , label = names ( limiares ) [ i ] )
        )
    }
    quadro.resumo.distribuicao <<- quadro.resumo.distribuicao + 
      geom_text   ( data = df.limiares
                    , aes ( x = ( x + xend ) / 2, y = limite.y - 0.1, label  = label )     
                    , size   = 4 
                    , hjust  = NA
                    , colour = "#135DA2"
      ) +
      geom_segment ( data = df.limiares
                     , aes ( y     = limite.y 
                             , yend  = limite.y
                             , xend  = xend
                             , x     = x
                     )
                     , arrow  = arrow(length=unit(0.3,"cm"), ends = "both")
                     , colour = "#135DA2"
                     , size  = .2
                     , alpha  = .9
      )  
    
    names(df.limiares) <<- c("De", "Até", "Faixa")
    df.limiares$De  = point(df.limiares$De)
    df.limiares$Até = point(df.limiares$Até)
    
    
    tabela.resumo.distribuicao <<- 
      tableGrob ( df.limiares
                  , rows = NULL
                  , theme = tema
      ) 
    
  }
  
  if ( incluiStatBin )
    quadro.resumo.distribuicao <<- 
    quadro.resumo.distribuicao +
    stat_bin ( breaks = seq ( Piso, Teto + Passo, Passo )
               , closed = "left"
               , geom="text"
               , aes ( label = ..count.., y = ..count../sum(..count..)  )
               , vjust=-1.5
               , color = "#135DA2"
    ) 
  
  if ( incluiStatEcdf )
    quadro.resumo.distribuicao <<- 
    quadro.resumo.distribuicao +
    stat_ecdf ( geom   = "area"
                , fill   = "dodgerblue4"
                , colour = NA
                , alpha  = .2
    )   
  
  if ( numeroRef != c(0) ) {
    
    for ( i in ( 1:length(as.vector(numeroRef) ) ) ) {
      
      if ( estiloRef == "P" )
        quadro.resumo.distribuicao <<- 
        quadro.resumo.distribuicao +
        geom_point ( data = data.frame ( x = as.vector(numeroRef)[i]
                                         , y = ecdf ( globalColunaAlvo[,1]) (as.vector(numeroRef)[i])
        )
        , mapping=aes(x=x, y=y)
        , shape = 10
        , size  = 4
        ) 
      else
        quadro.resumo.distribuicao <<- 
        quadro.resumo.distribuicao +
        geom_segment ( data = data.frame ( x    = as.vector(numeroRef)[i]
                                         , xend = as.vector(numeroRef)[i]
                                         , y    = 0
                                         , yend = ecdf ( globalColunaAlvo[,1]) (as.vector(numeroRef)[i])
        )
        , mapping=aes(x=x, y=y, xend=xend, yend=yend)
        , arrow = arrow(length = unit(0.03, "npc"))
        ) 
      
        
      quadro.resumo.distribuicao <<- 
          quadro.resumo.distribuicao +
          
        geom_text  ( data = data.frame ( x = as.vector(numeroRef)[i]
                                         , y = ecdf(globalColunaAlvo[,1])(as.vector(numeroRef)[i])
                                         , i = i 
        )
        , mapping = aes ( x=x, y=y, label =  paste ( "  [", round(x,digits=2), ", ", round(y*100,digits=2), "%]", sep="" ) ) #i/10+.2
        , hjust = 0
        , vjust = 0
        , size  = 4
        )                     
    }
  }  
  
  if ( incluiNormalidade ) {
    tabela.teste.normalidade <<-
      tableGrob ( dados.teste.normalidade
                  , rows = NULL
                  , theme = tema      
      ) 
    quadro.normal <<- 
      ggplot ( df, aes_string ( x = colunaAlvo ) ) +
      stat_function   ( geom = "area"
                        , fun    = function (x, mean, sd, n) { 
                          dnorm ( x    = x
                                  , mean = mean
                                  , sd   = sd
                          ) 
                        }
                        , args   = list   ( mean = mean   ( globalColunaAlvo[,1] )
                                            , sd   = sd     ( globalColunaAlvo[,1] )
                                            , n    = length ( globalColunaAlvo[,1] )
                        )
                        , fill  = NA
                        , colour = "black"
                        , alpha = .20
      ) +
      stat_density    ( alpha = .20
                        , fill =  "#dd8e04" ) +
      scale_x_continuous ( limits = c ( Piso, Teto )
                           , breaks = seq ( Piso, Teto, Passo )
      ) +
      theme_bw() +
      theme   ( legend.spacing    = unit ( 0, "mm" )
                , axis.line         = element_line (color = NA)  
                , axis.title.x      = element_text (color = NA, size = 0.000000000000001)  
                , axis.text         = element_text ( margin = unit ( 0, "mm") )
                , axis.text.x       = element_text (color = NA, size = 0.000000000000001)  
                , axis.ticks.x      = element_line (color = NA)
                , axis.title.y      = element_text (color = NA, size = 0.000000000000001)  
                , axis.text.y       = element_text (color = NA, size = 0.000000000000001)  
                , axis.ticks.y      = element_line (color = NA)
                , axis.ticks.length = unit ( 0, "mm" )
                , panel.border      = element_rect (color = NA)
                , panel.spacing     = unit ( 0, "mm" ) 
                , panel.grid        = element_line (color = NA)
                , panel.grid.major  = element_line (color = NA)
                , panel.grid.minor  = element_line (color = NA) 
                , panel.background  = element_rect (fill  = NA)
                , plot.margin       = unit ( c ( 0, -2.8, -3, -5.2 ), "mm"  )
                , plot.background   = element_rect (fill  = NA)
      ) 
  }
  
  if ( ( incluiNormalidade == F ) &
       ( incluiPareto      == F ) )
  {
    tabela.teste.normalidade <<- tableGrob("")
    quadro.normal <<- tableGrob("")
  }
  
  Resumo = summary(globalColunaAlvo[,1])
  
  Resumo [2] = quantile(globalColunaAlvo[,1],quartil1)
  Resumo [3] = quantile(globalColunaAlvo[,1],quartil2)
  Resumo [5] = quantile(globalColunaAlvo[,1],quartil3)
  
  dados.resumo <<- data.frame ( rbind ( c ( Resumo[1]
                                            , Resumo[2]
                                            , Resumo[3]
                                            , Resumo[5]
                                            , Resumo[6]
                                            , mean     ( globalColunaAlvo [,1] )
                                            , sd       ( globalColunaAlvo [,1] )
                                            , length   ( globalColunaAlvo [,1] )
                                            , kurtosis ( globalColunaAlvo [,1] ) 
                                            , skewness ( globalColunaAlvo [,1] )
  ) ) ) 
  
  names(dados.resumo) <<-  c ( "Min"
                               , quartil1Nome
                               , quartil2Nome
                               , quartil3Nome
                               , "Máx"
                               , "Média"
                               , "D.Padrão"
                               , "n"
                               , "Kurtosis"
                               , "Skewness")
  
  if ( lang == "es")
    names(dados.resumo) <<-  c ( "Min"
                                 , quartil1Nome
                                 , quartil2Nome
                                 , quartil3Nome
                                 , "Max"
                                 , "Media"
                                 , "D.Estándar"
                                 , "n"
                                 , "Kurtosis"
                                 , "Skewness")
  
  
  tabela.resumo <<- 
    tableGrob ( round ( dados.resumo
                        , digits=2 
    )
    , rows  = NULL
    , theme = tema    )
  altura.topo                 = 
    ( altura ( tabela.resumo            ) / 0.12 ) 
  
  altura.base                 =
    max ( altura ( tabela.resumo.distribuicao ), ( altura ( tabela.teste.normalidade ) / 0.30 ) )
  
  altura.total                =
    altura.topo + altura.base 
  
  largura.total               =
    largura ( tabela.resumo.distribuicao ) +
    largura ( tabela.teste.normalidade   ) 
  
  proporcao.topo              =
    ( altura.topo / altura.total ) 
  
  proporcao.base              =
    ( altura.base / altura.total ) 
  
  proporcao.esquerda          =
    largura ( tabela.resumo.distribuicao ) / largura.total
  
  proporcao.direita           =  
    1 - proporcao.esquerda
  setTimeLimit        ( cpu = 9999999999999999999, elapsed = 9999999999999999999, transient = FALSE)
  setSessionTimeLimit ( cpu = 9999999999999999999, elapsed = 99999999999999999999999999999)
  
  if ( incluiResumo ) {
    quadro.final <<- 
      arrangeGrob (  arrangeGrob ( top.plot  
                                    , quadro.resumo.distribuicao
                                    , tabela.resumo
                                    , heights = c ( 0.30, 0.58, 0.12 )
                    )
                    , arrangeGrob ( tabela.resumo.distribuicao
                                    , arrangeGrob ( quadro.normal
                                                    , tabela.teste.normalidade
                                                    , heights = c ( 0.70 , 0.30 )
                                                    , ncol    = 1
                                                    , nrow    = 2
                                    )
                                    , widths = c ( proporcao.esquerda, proporcao.direita )
                                    , ncol = 2
                                    , nrow = 1
                    )
                    , ncol = 1
                    , nrow = 2
                    , heights = c ( proporcao.topo
                                  , proporcao.base
                    )
      )
    
  }
  else {
    altura.total <<- 150
    quadro.final <<- 
      arrangeGrob (  top.plot  
                    , quadro.resumo.distribuicao
                    , tabela.resumo
                    , heights = c (  0.26, 0.62, 0.12 )
                    , ncol = 1
                    , nrow = 3
      )
  }
  if ( outputFile ) {
    png ( filename = paste ( "~/", tituloAlvo, " - Dstribuição.png", sep = "")
          , width  =  210 # max(210, largura.total + 5)
          , height =  290 # altura.total + 5 #148
          , units  = "mm"
          , res    = 92
    )
  
    grid.arrange ( quadro.final )
    dev.off()
  }
  
  if ( outputScreen )
    grid.arrange ( quadro.final )  
  
  # rm ( "tabela.resumo"              , envir=.GlobalEnv)
  # rm ( "tabela.resumo.distribuicao" , envir=.GlobalEnv)
  # rm ( "tabela.teste.normalidade"   , envir=.GlobalEnv)
  # rm ( "tabela.titulo"              , envir=.GlobalEnv)
  # rm ( "quadro.final"               , envir=.GlobalEnv)
  # rm ( "quadro.normal"              , envir=.GlobalEnv)
  # rm ( "quadro.resumo.distribuicao" , envir=.GlobalEnv)
  # rm ( "globalColunaAlvo"           , envir=.GlobalEnv)
  # rm ( "globalTituloAlvo"           , envir=.GlobalEnv)
  # rm ( "dados.resumo.distribuicao"  , envir=.GlobalEnv)
  # rm ( "dados.resumo"               , envir=.GlobalEnv)
  # if ( exists( "dados.teste.normalidade" ) )
  #   rm ( "dados.teste.normalidade"  , envir=.GlobalEnv)
  # rm ( "box.plot"                   , envir=.GlobalEnv)
  
  return (c( largura.total, altura.total ))
  
}
Correlacao   <- function ( x
                           , y
                           , tituloX
                           , tituloY 
) {
  
  globalTituloX <<- tituloX  
  globalTituloY <<- tituloY 
  
  dfTemp <<- df[,c(x,y)]
  names(dfTemp) = c(tituloX,tituloY)
  
  quadro.titulo <<- 
    textGrob ( paste("Correlação:", globalTituloX, "x", globalTituloY )  
               , gp = gpar ( fontsize=12, cex=1 ) )
  
  quadro.correlacao <<- 
    plotmatrix ( dfTemp ) + 
    geom_smooth ( method="lm" ) +
    theme_bw() +
    theme   ( legend.spacing =       unit ( 0, "mm" )
              , axis.title.x      = element_text ( color = NA, size = 0.000000000000001)  
              , axis.ticks.x      = element_line ( color = NA )          
              , axis.text.x       = element_text ( color = NA, size = 0.000000000000001)  
              , axis.title.y      = element_text ( color = NA, size = 0.000000000000001)  
              , axis.ticks.y      = element_line ( color = NA )
              , axis.text.y       = element_text ( color = NA, size = 0.000000000000001)  
              , axis.line         = element_line ( color = NA)
              , strip.background  = element_rect ( color = NA, fill = NA)
              , panel.background  = element_rect ( color = NA, fill = NA)
    ) 
  
  quadro.teste <<- 
    tableGrob ( paste(strwrap(cor.test.interpretation ( dfTemp[,1], dfTemp[,2], 0.05 , globalTituloX, globalTituloY ),
                              width=60),sep="\n")
                , gp = gpar ( fontsize=11, cex=1 ) 
    ) 
  
  altura.total <<- 
    altura (quadro.titulo ) +
    altura (quadro.teste  ) +
    100
  
  proporcao.topo <<- altura ( quadro.titulo ) / altura.total
  proporcao.base <<- altura ( quadro.teste  ) / altura.total
  
  quadro.final <<- arrangeGrob ( quadro.titulo
                                 , quadro.correlacao
                                 , quadro.teste
                                 , ncol = 1
                                 , heights= c ( proporcao.topo
                                                , 1 - ( proporcao.topo + proporcao.base )
                                                , proporcao.base  
                                 ) 
  )
  
  png ( filename = paste ( "~/Correlacao_"
                           , tituloX
                           , "_x_"
                           , tituloY
                           , ".png"
                           , sep = ""
  )
  , width  = (297-20)/2   
  , height = altura.total + 10 
  , units  = "mm"
  , res    = 92
  )
  
  grid.arrange ( quadro.final )
  
  dev.off()    
  
  grid.arrange (quadro.final)
  
  rm ( dfTemp            , envir=.GlobalEnv )
  rm ( quadro.final      , envir=.GlobalEnv)
  rm ( quadro.titulo     , envir=.GlobalEnv ) 
  rm ( quadro.correlacao , envir=.GlobalEnv )
  rm ( quadro.teste      , envir=.GlobalEnv )
  rm ( altura.total      , envir=.GlobalEnv )
  rm ( proporcao.topo    , envir=.GlobalEnv )
  rm ( proporcao.base    , envir=.GlobalEnv )
  rm ( globalTituloX     , envir=.GlobalEnv )  
  rm ( globalTituloY     , envir=.GlobalEnv )
  rm ( tabela.teste.normalidade, envir=.GlobalEnv )
} 
fancy.pizza         <- function ( colunaAgregado
                                  , tituloAgregado
                                  , colunaChave = "RO"
                                  , legendas    = c()
                                  , box         = F
                                  , grafico     = T
                                  , df  ) {
  totalCasos <<- nrow (df)
  
  dt <- data.table ( df )
  
  setkeyv (dt, colunaAgregado) 
  
  controle=data.frame(df[,colunaChave])
  
  dfSumario <<- data.frame ( dt [, list ( Casos      = length (get ( colunaChave ))                ,
                                          Percentual = length (get ( colunaChave )) / totalCasos ) ,
                                 by = list ( get ( colunaAgregado ) ) ] )
  if ( exists ("legendas") )
    if ( !is.null(legendas)) {
      dfSumario$get <<- legendas
    }
  
  names(dfSumario)[1]=colunaAgregado
  
  dfGrafico <<- data.frame ( dfSumario [ dfSumario$Percentual > 0.01, ]
                             , stringsAsFactors = FALSE
  )
  dfGrafico[[1]] = as.character ( dfGrafico[[1]] )
  
  if ( nrow ( dfSumario [ dfSumario$Percentual <= 0.01, ] ) != 0 ) {
    dfGrafico <<- rbind ( dfGrafico
                          ,  c ( "Outros (≤1%)"
                                 ,     sum ( dfSumario [ dfSumario$Percentual <= 0.01, "Casos" ] ) 
                                 , 1 - sum ( dfSumario [ dfSumario$Percentual > 0.01, "Percentual" ] )        
                          ) 
    )
  }
  
  dfGrafico[[1]] = as.factor  (dfGrafico[[1]])
  dfGrafico[[2]] = as.numeric (dfGrafico[[2]])
  dfGrafico[[3]] = as.numeric (dfGrafico[[3]])
  
  
  dfSumario[[1]] = as.factor (dfSumario[[1]])
  
  
  dfSumario$PercentualFormatado = formatPercentual(dfSumario$Percentual,2)
  dfGrafico$PercentualFormatado = formatPercentual(dfGrafico$Percentual,2)
  
  
  if (nrow(dfSumario) <= 5) {
    p1 <<-
      ggplot      ( dfSumario
                    , aes_string ( x    = 1
                                   , y    = "Percentual") 
      ) +
      geom_bar    ( stat= "identity"
                    , width = 1 
                    , fill  = head ( c ( "#008577"
                                         , "#9690C6"
                                         , "#F78D1E"
                                         , "#776851"
                                         , "#ED1C24" ), nrow(dfSumario))
                    # factor (dfSumario[[1]])
                    #  , alpha = 0.5
                    , color = NA
      )  + 
      #      scale_fill_manual ( values=c("#999999", "#E69F00", "#C3C3C3")) +
      geom_text   ( aes_string ( x     = "1.25"
                                 , label = names(dfSumario)[1]
                                 , y     = "Percentual/2 + c(0,cumsum(Percentual)[-length(Percentual)])"
                                 , hjust = 0
      )
      , size = 5 
      ) +
      coord_polar ( theta = "y") +
      theme ( axis.line =         element_blank() #line(color="gray50")
              , axis.text.x =       element_blank() #text(colour = NA)# ,size = 0.0000000000001, lineheight = 0.0000000000001, vjust = 1)
              , axis.text.y =       element_blank() #text(colour = NA)#,size = 0.0000000000001, lineheight = 0.0000000000001, vjust = 1)
              , axis.ticks.x =      element_blank() #line(colour = NA)#,size = 0.0000000000001)
              , axis.ticks.y =      element_blank() #line(colour = NA)#,size = 0.0000000000001)
              , axis.title.x =      element_blank() #text(colour = NA)#,size = 0.0000000000001, vjust = 1)
              , axis.title.y =      element_blank() #text(colour = NA)#,size = 0.0000000000001, vjust = 1)
              , axis.ticks.length = unit ( 1, "mm")
              , axis.ticks.margin = unit ( 1, "mm")
              , panel.spacing      = unit ( 1, "mm" )
              , panel.background  = element_rect ( fill  = "white"
                                                   , color = NA
              )
              , plot.background   = element_rect  ( fill  = "white"
                                                    , color = "gray50"
              )
              , plot.margin   = unit ( c ( 0, 0 , -5 , -5 ), "mm"  ) 
      )
  }
  else {
    p1 <<-
      ggplot      ( dfGrafico
                    , aes_string ( x    = colunaAgregado 
                                   , y    = "Percentual"
                                   , fill = colunaAgregado) 
      ) +
      geom_bar ( data=dfGrafico[dfGrafico[[1]] != "Outros (≤1%)",]
                 , stat  = "identity"
                 , fill = "burlywood4"
                 , alpha = .5
      )  +
      guides(fill=FALSE) +
      geom_text   (aes ( label = paste  ( " "
                                          , format ( Casos
                                                     , digits = nchar(Casos)
                                                     , big.mark ="."
                                                     , decimal.mark =",")
                                          , " ("
                                          , PercentualFormatado
                                          , ")" 
                                          , sep =""
      ) 
      #, y     = max (Percentual)
      , vjust = NA
      , hjust = 0
      # , family = "mono"
      , fontface   = 2
      )
      , size = 4 
      , angle=0
      )  +
      ylim ( 0, max(dfGrafico$Percentual)+0.1 ) +
      coord_flip() +
      theme_fivethirtyeight()
    theme ( axis.line   =       element_blank()
            , axis.text.x =       element_blank() 
            , axis.text.y =       element_text ( colour = "black"
                                                 , size   = 12
                                                 # , family = "mono" 
            )
            , axis.ticks.x =      element_blank()
            , axis.ticks.y =      element_blank() 
            , axis.title.x =      element_blank() 
            , axis.title.y =      element_blank() 
            , axis.ticks.length = unit ( 1, "mm")
            , axis.ticks.margin = unit ( 1, "mm")
            , panel.background  = element_rect ( fill  = "bisque"
                                                 , color = NA
            )
            , panel.spacing     = unit (  c(4,4,4,4), "mm" )
            , plot.background   = element_rect  ( fill  = "white"
                                                  , color = NA
                                                  
            )
            , plot.margin       = unit ( c(1,1,1,1), "mm" ) 
    )
    
    if ( nrow ( dfGrafico[dfGrafico[[1]] == "Outros (≤1%)",] ) != 0 ) {
      p1 <<- p1 +
        geom_bar ( data=dfGrafico[dfGrafico[[1]] == "Outros (≤1%)",]
                   , stat  = "identity"
                   , fill = "dodgerblue4"
                   , alpha = .5
        )  
    }  
  }
  
  if (nrow(dfSumario) <= 5)
    dfQuadro = dfSumario 
  else {
    dfQuadro = dfSumario[dfSumario$Percentual<=0.01,] 
    dfQuadro = dfQuadro[order(-dfQuadro$Percentual),]
  }
  
  p2 <<- textGrob ("") 
  p4 <<- textGrob ("")
  p5 <<- textGrob ("")
  
  tema.default =
    ttheme_default ( 
      core = list ( fg_params = list ( col = "dodgerblue4" ), fontsize = 9
                    , bg_params = list ( fill = c ("white","grey90") )
                    , show.vlines = FALSE
      )
      , row  = list ( fg_params = list ( col = "dodgerblue4" )
                      , bg_params = list ( fontsize = 9, fontface = "bold", fill = c ("white","grey90") )
                      , show.vlines = FALSE
      )
      , col  = list ( fg_params = list ( col = "dodgerblue4" )
                      , bg_params = list ( fontsize = 9, fontface = "bold", fill = c ("white","grey90") )
                      , show.vlines = FALSE
      )
    ) 
  
  
  if ( nrow(dfQuadro) != 0 ) {
    dfQuadro$Percentual <- formatPercentual(dfQuadro$Percentual,2) 
    p2 <<- tableGrob ( dfQuadro [1:min(15,nrow(dfQuadro)),-4]
                       , cols=names(dfQuadro[,-4]), rows=NULL
                       , theme = tema.default )
  } 
  if ( nrow(dfQuadro) > 15 ) {
    p4 <<- tableGrob (  dfQuadro [16:30,-4]
                        , cols=names(dfQuadro[,-4]), rows = NULL
                        , theme =   tema.default )
    
  }
  if ( nrow(dfQuadro) > 30 ) {
    p5 <<- tableGrob (  dfQuadro [31:45,-4]
                        , cols=names(dfQuadro[,-4]), rows = NULL
                        , theme =   tema.default )
  } 
  
  p3 <<- ggplot ( df, aes_string(x=1, y=colunaAgregado)) + 
    geom_boxplot ( outlier.colour = "blue"
                   , outlier.size = 3
                   , fill = "gray50"
                   , alpha = 0.10
    )  +
    theme_bw     ()    +
    theme ( axis.title.x = element_text ( colour     = NA
                                          , size       = 0.000000000000001
                                          , lineheight = 0.000000000000001
    )
    , axis.text.x  = element_text ( colour     = NA
                                    , size       = 0.000000000000001
                                    , lineheight = 0.000000000000001
    )
    #  , axis.text.y   = element_blank() 
    #, axis.text.x   = element_blank()
    , axis.ticks.x  = element_line ( colour = NA      )          
    , axis.ticks.y  = element_line ( colour = "White" )
    # axis.title.x  = element_blank()
    , axis.title.y  = element_blank()   
    , legend.spacing = unit ( 0, "mm" )
    , panel.spacing = unit ( 0, "mm" ) 
    , plot.margin   = unit ( c ( 0, 2, -3, 2 ), "mm"  ) 
    ) 
  
  if ( nrow ( dfQuadro ) == 0 ) 
    pQuadro <<- textGrob("")
  else 
    if ( nrow ( dfQuadro ) < 16 )
      pQuadro <<- p2
  else 
    if ( nrow ( dfQuadro ) < 31 )
      pQuadro <<- arrangeGrob(p2,p4, ncol=2)
  else 
    pQuadro <<- arrangeGrob(p2,p4,p5, ncol=3)
  
  
  if (!box) {
    if (!grafico) {
      grid.arrange ( textGrob   ( tituloAgregado,
                                  gp=gpar(fontsize=12) )
                     , pQuadro
                     , ncol    = 1
                     , heights = c(0.10, 0.90) ) 
      
      
    } 
    else {
      grid.arrange ( textGrob   ( tituloAgregado
                                  , gp = gpar ( fontsize = 12 ) )
                     , p1
                     , pQuadro
                     , ncol    = 1
                     , heights = c ( 0.05, 0.50, 0.45 ) 
      )
    }
  }
  else {
    grid.arrange ( textGrob ( tituloAgregado
                              , gp = gpar(fontsize=12) 
    )
    , arrangeGrob ( arrangeGrob ( p1
                                  , pQuadro
    )
    , p3
    , ncol=2
    , widths = c ( 0.80 , 0.20 )
    )
    , ncol    = 1
    , heights = c(0.10,0.90) 
    )
  }
  # rm(dfSumario,envir=.GlobalEnv)
  # rm(dfQuadro,envir=.GlobalEnv)
  # rm(dfGrafico,envir=.GlobalEnv)  
  # rm(p1,envir=.GlobalEnv)
  # rm(p2,envir=.GlobalEnv)
  # rm(p3,envir=.GlobalEnv)
  # rm(p4,envir=.GlobalEnv)
  # rm(p5,envir=.GlobalEnv)
  # rm(pQuadro,envir=.GlobalEnv)
  # rm(totalCasos,envir=.GlobalEnv)
  
}
fancy.anova         <- function (l
                                 , Grafico=T
                                 , Compl
                                 , lp.par=NULL
                                 , titulo=NULL
                                 , lang = "pt-br") {
  tema.anova <<- ttheme_default ( 
    core = list ( fg_params = list ( fontsize = 11, col  = "dodgerblue4" )
                  , bg_params = list ( fill = c ("#0072B2","white"), col  = "gray50", alpha=0.1 )
                  , show.vlines = FALSE
    )
    , colhead  = list ( fg_params = list ( fontsize = 11, fontface = "bold", col = "dodgerblue4", parse = TRUE )
                        , bg_params = list ( fill = "#0072B2", col  = "gray50", alpha=0.3 )
                        , show.vlines = FALSE
    )
  ) 
  
  s      <<- summary (l)  
  df.out <<- data.frame ( l$model
                          , predict ( l, interval = 'prediction' )[,-1]                                      
  )

  if ( is.null(lp.par))
  {
    theFormula <<- 
      paste ( "scale (`"
            , names(l$model)[1]
            , "`)"
            , sep = ""
    )
    for ( i in (2:length ( names (l$model) ) ) ) {
      if ( i == 2 ) {
        incluiGrafico = T
        theFormula <<- 
          paste ( theFormula 
                , " ~"
                , sep = "" )
      }
      else {
        incluiGrafico = F
        theFormula <<-
          paste ( theFormula 
                , " +"
                , sep = ""
        )
      } 
      if ( typeof ( l$model [, i ] ) != "character" 
           & !is.factor ( l$model [, i ] ) )
        theFormula <<- 
          paste ( theFormula
                , "scale (`"
                , names(l$model)[i]
                , "`)"
                , sep = ""
                )
      else
        theFormula <<- 
          paste ( theFormula
                , names(l$model)[i]
          )
    }
    if (is.na ( coef(l)["(Intercept)"] ) ) {
      theFormula <<-
        paste (theFormula, "- 1" )
    }  
    lp <<- lm ( theFormula, data=df.out )
  } else {
    lp <<- lp.par
  }
  
  if ( length ( names ( l$model ) ) > 2 ) {
    incluiGrafico = F
  } else {  
    incluiGrafico = T
  }
  
  if ( incluiGrafico & !Grafico ) incluiGrafico = F
  
  incluiIntercepto = T
  if (is.na ( coef(l)["(Intercept)"] ) ) {
    incluiIntercepto = F
  }
  
  confint_l <<- confint(l, level=0.95)
  df.out$Id <<- dimnames(l$qr[[1]])[[1]]
  df.out$fit                   <<- fitted.values  (l)                                          ## PRE_1
  df.out$resid                 <<- resid          (l)                                          ## RES_1
  df.out$ER                   <<- df.out$resid / df.out$fit                              ## MER
  df.out$MER                   <<- abs (df.out$ER)                              ## MER
  df.out$PRED25                <<- ifelse (df.out$MER <= .25, 1, 0)                              ## PRED25
  df.out$rstudent              <<- rstudent       (l)                                          ## SDR_1
  df.out$rstandard             <<- rstandard      (l)                                          ## SRE_1
  df.out$fit.standard          <<- ( df.out$fit - mean (df.out$fit) )/ sd (df.out$fit)         ## ZPR_1              
  df.out$cooks.distance        <<- cooks.distance (l)                                          ## COO_1
  df.out$dffits                <<- dffits         (l)                                          ## SDF_1
  df.out$hatvalue              <<- hatvalues      (l)                                    
  df.out$rstandard.wo.deletion <<- df.out$resid / s$sigma                                     ## ZRE_1
  df.out$rstudent.wo.deletion  <<- df.out$resid / ( s$sigma * sqrt ( 1 - df.out$hatvalue ) ) ## SRE_1   
  df.out$ZPR_2                 <<- df.out$fit.standard * df.out$fit.standard                    ## ZPR_2
  df.out$ZRE_2                 <<- df.out$rstandard.wo.deletion * df.out$rstandard.wo.deletion  ## ZRE_2
  
  pred_lwr_l <<- lm ( paste ("lwr ~", names(l$model)[2]), data = df.out)
  pred_upr_l <<- lm ( paste ("upr ~", names(l$model)[2]), data = df.out)
  
  P.upr <<- pred_upr_l$coefficients[[1]]
  P.lwr <<- pred_lwr_l$coefficients[[1]]
  
  if ( incluiIntercepto ) {
    P.coef        <<- l$coefficients [[2]]
    C.lwr         <<- confint_l[2,1] 
    C.upr         <<- confint_l[2,2]
    Formula.espec <<- "y ~ x"
    Formula       <<- paste ( '#', names(l$model)[1], ' ='
                              , format ( l$coefficients[[1]],  digits=3, nsmall=1), " + "
                              , format ( l$coefficients[[2]],  digits=3, nsmall=1)
                              , 'x #', names(l$model)[2], ','
                              , 'R2=', format(s$r.squared, digits=3), sep=' ' )
    
  } else {
    P.coef        <<- l$coefficients [[1]]  
    C.lwr         <<- confint_l[1,1] 
    C.upr         <<- confint_l[1,2]
    Formula.espec <<- "y ~ x - 1"
    Formula       <<- paste ( '#', names(l$model)[1], ' ='
                              , format ( l$coefficients[1],  digits=3, nsmall=1)
                              , 'x #', names(l$model)[2],  ','
                              , 'R2=', format(s$r.squared, digits=3), sep=' ' )
  }
  
  Intervalo = "Intervalo"
  if ( lang == "es" )
    Intervalo = "Intervalo"
  
  Confianca = "Confiança"
  if ( lang == "es" )
    Confianca = "Confianza"
  
  Predicao = "Predição"  
  if ( lang == "es" )
    Predicao = "Predicción"
  
  
  pG <<- ggplot ( df.out 
                  , aes_string ( x=names(l$model)[2]
                                 , y=names(l$model)[1] ) ) + theme_fivethirtyeight()+
    scale_fill_manual ( Intervalo
                        , values = c('burlywood4', 'dodgerblue4') ) +
    geom_point  () +
    geom_smooth ( method = 'lm'
                  , formula = Formula.espec
                  , aes ( fill = paste ( '\n', Confianca, ':\n ', names(l$model)[1], ' = '
                                         , format ( C.lwr
                                                    , digits = 5
                                                    , nsmall = 1 ), ' | '
                                         , format ( C.upr
                                                    , digits = 5
                                                    , nsmall = 1 ), ' * '
                                         , names(l$model)[2], '\n', sep='' ) )
                  , alpha = 0.25 ) +
    geom_ribbon ( aes ( ymin  = lwr
                        , ymax  = upr
                        , fill  = paste ( '\n', Predicao, ':\n', names(l$model)[1], ' ≈ '
                                          , format ( P.lwr
                                                     , digits = 5
                                                     , nsmall = 0 ), ' | ' 
                                          , format ( P.upr
                                                     , digits = 5
                                                     , nsmall = 0 ),' + '
                                          , format ( P.coef
                                                     , digits = 5
                                                     , nsmall = 1 ), ' * '
                                          , names(l$model)[2], '\n', sep='')
    )
    , alpha = 0.25 )
  #   +
  #     geom_text ( aes(x=xR2,yR2,label = Formula)
  #                 , size    = 5  
  #                 , data.frame ( xR2     = (max(l$model[2])-min(l$model[2]))/2+min(l$model[2]) 
  #                              , yR2     = 0
  #                              , Formula = Formula ))   
  
  #
  ## Preparaçaão de Dados
  #
  
  # T-testes Individuais
  
  coeficientesL <<- 
    data.frame ( sprintf ( "%.5f" , s$coefficients[,1])
               , sprintf ( "%.5f" , s$coefficients[,2])
               , sprintf ( "%.5f" , coef(lp)          )
               , sprintf ( "%.5f" , s$coefficients[,3])
               , sprintf ( "%.5f" , s$coefficients[,4])
               , signif.code ( as.numeric ( s$coefficients[,4] ) ) 
               , row.names = row.names(s$coefficients)
  )
  coeficientesL2 <<-     data.frame ( s$coefficients[,1]
                                    ,  s$coefficients[,2]
                                    , coef(lp)          
                                    , s$coefficients[,3]
                                    , s$coefficients[,4]
                                    , signif.code ( as.numeric ( s$coefficients[,4] ) ) 
                                    , row.names = row.names(s$coefficients)
  )
  coeficientesL2$Variavel = row.names(coeficientesL2)
  
  names     (coeficientesL2) = c("Estimativa", 
                                "Erro Padrão", 
                                "Beta", 
                                "Estat. T", 
                                "valor-p",
                                "a",
                                "Variável")
  
  coeficientesL$Variavel = row.names(coeficientesL)
  
  names     (coeficientesL) = c("Estimativa", 
                           "Erro Padrão", 
                           "Beta", 
                           "Estat. T", 
                           "valor-p",
                           "a",
                           "Variável")
  colnames  (coeficientesL) = 
    c ( expression(paste(bold("Estimativa ("), beta[i], bold(")")))
        , expression(bold("Erro Padrão"))
        , "Beta"
        , expression(bold("Estat. T"))
        , expression(bold("valor-p" ))
        , expression(alpha)
        , expression(bold("Variável"))
    )
  
  
  # Intervalo de confiança
  
  confiancaL <<- data.frame ( confint(l, level=0.95)
                              , stringsAsFactors = FALSE  
  ) 
  
  confiancaL2 <<- confiancaL
    
  confiancaL <<- data.frame ( sprintf ( "%.5f", confiancaL[,1] )
                              , sprintf ( "%.5f", confiancaL[,2] )
                              , row.names = row.names ( confiancaL ) 
                              , stringsAsFactors = FALSE  
  )
  
  names    (confiancaL) = c( "Inferior", "Superior" )   
  colnames (confiancaL) = 
    c ( expression(bold(Inferior))
      , expression(bold(Superior))
    )
  
  
  # F-teste 
  
  nVI   = length ( l$coefficients )
  DfR   = sum ( anova(l)[1:nrow(anova(l))-1  , 1] )
  DfE   =       anova(l)[  nrow(anova(l))    , 1]
  N     = DfR + DfE
  
  SSR   = sum ( anova(l)[1:nrow(anova(l))-1 , 2] )
  SSE   =       anova(l)[  nrow(anova(l))   , 2]
  SST   = sum ( anova(l)[1:nrow(anova(l))   , 2] )
  
  MSR   = SSR / DfR
  MSE   =       anova(l)[nrow(anova(l))     , 3]
  
  FStat = MSR / MSE 
  valor.p = pf ( FStat, DfR, DfE, lower=FALSE)
  
  anovaL <<- data.frame(t(data.frame( 
    c ( DfR 
        , point ( SSR )
        , point ( MSR ) 
        , point ( FStat ) 
        , paste  ( format ( valor.p, digits=3, nsmall=3 ), signif.code ( valor.p ) ) 
    ),
    c ( DfE 
        , point ( SSE )
        , point ( MSE )
        , " "                      
        , " "                                 
    ),
    c ( N   
        , point ( SST )
        , " "                      
        , " "                         
        , " "   
    )
  )))
  
  names     (anovaL) = c("Df", "Soma.de.Quadrados", "Quadrado.Medio","F", "valor.p")
  colnames  (anovaL) = 
    c ( expression(bold("Df"))
      , expression(bold("Soma de Quadrados"))
      , expression(bold("Quadrado Médio"))
      , expression(bolditalic(F)[0])
      , expression(bold("valor-p"))
      )
  row.names (anovaL) = c("Modelo", "Erro", "Total")
  
  # Medidas de associação
  
  l$model$MER    = abs ( resid ( l ) ) / fitted.values ( l ) 
  l$model$PRED25 = ifelse ( l$model$MER <= .25, 1, 0)
  
  associacaoL <<- 
    data.frame ( R           = format ( ( ( s$r.squared ^ 0.5 ) * sign ( s$coefficients[2] ) ), digits = 3, nsmall = 3 ) 
                 , R2          = format (     s$r.squared                                       , digits = 3, nsmall = 3 ) 
                 , R2.Ajustado = format (     s$adj.r.squared                                   , digits = 3, nsmall = 3 ) 
                 , Erro.Padrao = format (     s$sigma                                           , digits = 3, nsmall = 3 ) 
                 , PRED25      = paste ( round ( ( sum ( l$model$PRED25 ) * 100 ) / nrow ( l$model ), 1 ), "%" )
                 , MMER        = paste ( round ( ( sum ( l$model$MER    ) * 100 ) / nrow ( l$model ), 1 ), "%" )
    )  
  if ( !missing(Compl)  ) {
    for ( i in (1:nrow(Compl)))
      associacaoL <<- rbind ( associacaoL,
                              data.frame ( R            = ""
                                           , R2           = ""
                                           , R2.Ajustado  = ""
                                           , Erro.Padrao  = Compl[i,]$T
                                           , PRED25       = paste ( round ( Compl[i,]$P, 1 ), "%" )
                                           , MMER         = paste ( round ( Compl[i,]$M  , 1 ), "%" )
                              ) 
      )
  }
  
  names     (associacaoL) = 
    c("R", "R2", "R2 Ajustado", "Erro Padrão da Regressão", "PRED(25)", "MMER")
  colnames  (associacaoL) = 
    c ( expression(bold(R  ))
      , expression(bold(R^2))
      , expression(bold(paste(R^2, " Ajustado"))) 
      , expression(paste(bold("Erro Padrão da Regressão ("), italic(s), bold(")")))
      , expression(bold("PRED(25)"))
      , expression(bold("MMER"))
      )
  
  tabela.teste.F <<- 
    tableGrob (
      anovaL
      , rows  = NULL
      , theme = tema.anova 
    )
  
  tabela.medidas.associacao <<- 
    tableGrob (
      associacaoL
      , rows=NULL, theme=tema.anova )
  
  #
  ##
  #
  
  tabela.teste.t.col1 <<- tableGrob (
    coeficientesL[,c(7,1:2)]
    , rows=NULL, theme=tema.anova   )
  
  tabela.teste.t.col3 <<- tableGrob (
    coeficientesL[,c(3)]
    , cols = c("beta")
    , rows=NULL, theme=tema.anova 
  )
  
  tabela.teste.t.col4 <<- tableGrob ( coeficientesL[,c(4,5,6)]
                                      , rows=NULL, theme=tema.anova 
  )
  tabela.teste.t.col5 <<- tableGrob ( confiancaL[,c(1:2)]
                                      , rows=NULL, theme=tema.anova 
  )
  
  tabela.teste.t.cab2 <<- textGrob("Coeficientes não\nPadronizados" , gp=gpar(fontsize=11 ))
  tabela.teste.t.cab3 <<- textGrob("Coeficientes\nPadronizados"     , gp=gpar(fontsize=11 ))  
  tabela.teste.t.cab4 <<- textGrob("Testes de\nSignificância"       , gp=gpar(fontsize=11 ))
  tabela.teste.t.cab5 <<- textGrob("Intervalos de\nConfiança em 95%", gp=gpar(fontsize=11 )) 
  
  nada <<- textGrob(" ")
  
  tabela.teste.t.linha1 <<- arrangeGrob ( tabela.teste.t.cab2
                                          , tabela.teste.t.cab3
                                          , tabela.teste.t.cab4
                                          , tabela.teste.t.cab5
                                          , ncol   = 4
                                          , widths = proporcoesL ( list ( tabela.teste.t.col1
                                                                          , tabela.teste.t.col3
                                                                          , tabela.teste.t.col4 
                                                                          , tabela.teste.t.col5
                                          ) )
  )
  tabela.teste.t.linha2 <<- arrangeGrob ( tabela.teste.t.col1
                                          , tabela.teste.t.col3
                                          , tabela.teste.t.col4
                                          , tabela.teste.t.col5  
                                          , ncol   = 4
                                          , widths = proporcoesL ( list ( tabela.teste.t.col1
                                                                          , tabela.teste.t.col3
                                                                          , tabela.teste.t.col4
                                                                          , tabela.teste.t.col5
                                          ) )
  )        
  tabela.teste.t.linha3 <<- textGrob ( 
    expression(" "^"a"*"Significância: 0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 ")
    , gp=gpar(fontsize=11 )
  )
  
  gTitulo <<- textGrob   ( sub("~","=", substr(l$call[2],1,nchar(l$call[2]))) )  
  
  altura.titulo <<- 
    altura (titulo)
  
  altura.testes.t <<-   
    (  altura (tabela.teste.t.cab2) 
       + altura (tabela.teste.t.col1)
       + altura (tabela.teste.t.linha3) 
    )  
  
  altura.teste.F <<- 
    altura (tabela.teste.F) + 10 
  
  largura.teste.F <<-
    largura (tabela.teste.F)
  
  altura.medidas.associacao <<-
    altura (tabela.medidas.associacao) + 10 
  
  largura.medidas.associacao <<-
    largura (tabela.medidas.associacao)
  
  altura.total <<-
    ( altura.titulo
      + altura.testes.t
      + altura.medidas.associacao 
      + altura.teste.F   
    )
  
  largura.total <<-
    sum ( sapply ( X = list ( tabela.teste.t.col1
                              , tabela.teste.t.col3
                              , tabela.teste.t.col4
                              , tabela.teste.t.col5
    )
    , FUN = largura
    ) )
  
  proporcao.titulo   <<-
    ( altura.titulo 
      / altura.total
    )
  proporcao.testes.t <<-
    ( altura.testes.t
      / altura.total
    )
  
  proporcao.teste.F <<-
    ( altura.teste.F 
      / altura.total
    )
  
  proporcao.medidas.associacao <<-
    ( altura.medidas.associacao  
      / altura.total
    )
  
  quadro.final <<- 
    arrangeGrob ( gTitulo
                  , arrangeGrob ( tabela.teste.t.linha1
                                  , tabela.teste.t.linha2
                                  , tabela.teste.t.linha3
                                  , nrow    = 3
                                  , ncol    = 1
                                  , heights = proporcoesA ( list ( tabela.teste.t.cab2 
                                                                   , tabela.teste.t.col1
                                                                   , tabela.teste.t.linha3 ) )
                  )
                  , arrangeGrob ( tabela.teste.F
                                  , nada
                                  , nrow    = 1
                                  , ncol    = 2
                                  , widths = c ( ( largura.teste.F / largura.total )
                                                 , 1 - ( largura.teste.F / largura.total )
                                  )
                  )
                  , arrangeGrob ( tabela.medidas.associacao
                                  , nada
                                  , nrow    = 1
                                  , ncol    = 2
                                  , widths = c ( ( largura.medidas.associacao / largura.total )
                                                 , 1 - ( largura.medidas.associacao / largura.total )
                                  )
                  )
                  , nrow    = 4
                  , ncol    = 1
                  , heights = c ( proporcao.titulo
                                  , proporcao.testes.t           
                                  , proporcao.teste.F            
                                  , proporcao.medidas.associacao 
                  ) 
    )
  
  if ( incluiGrafico ) altura.total = altura.total + 120 
  png ( filename = paste ( "~/"
                           , titulo
                           , " - ANOVA - "
                           , sub("~","=", substr(l$call[2],1,nchar(l$call[2]))) 
                           , ".png"
                           , sep=""
  )
  , width  = largura.total
  , height = altura.total + 10 
  , units  = "mm"
  , res    = 92
  )
  
  if (incluiGrafico) {
    grid.arrange ( pG, quadro.final, heights = c( .5, .5 ) )    
  } else {
    grid.arrange ( quadro.final )
  }
  
  dev.off()
  if (incluiGrafico){
    png ( filename = paste ( "~/"
                             , titulo
                             , " - ANOVA - "
                             , sub("~","=", substr(l$call[2],1,nchar(l$call[2]))) 
                             , "_Grafico.png"
                             , sep=""
    )
    , width  = 297
    , height = 210 
    , units  = "mm"
    , res    = 92
    )
    pG = pG +theme ( legend.position   = "bottom" ) 
    
    print(pG)
    dev.off()
  }
  write.table ( cbind ( coeficientesL2, confiancaL2 ) 
                , quote        = F
                , row.names    = F
                , sep          = ";"
                , dec          = ","
                , fileEncoding = "UTF-8"
                , paste ( "~/"
                          , titulo
                          , " - ANOVA - "
                          , sub("~","=", substr(l$call[2],1,nchar(l$call[2]))) 
                          , "_dados.csv"
                          , sep=""
                )  )
  
  rm ( "tabela.teste.t.col1", envir = .GlobalEnv )
  rm ( "tabela.teste.t.col3", envir = .GlobalEnv )
  rm ( "tabela.teste.t.col4", envir = .GlobalEnv ) 
  rm ( "tabela.teste.t.col5", envir = .GlobalEnv )
  rm ( "nada", envir = .GlobalEnv )
  rm ( "tabela.teste.t.cab2", envir = .GlobalEnv )
  rm ( "tabela.teste.t.cab3", envir = .GlobalEnv )
  rm ( "tabela.teste.t.cab4", envir = .GlobalEnv )
  rm ( "tabela.teste.t.cab5", envir = .GlobalEnv )
  rm ( "tabela.teste.t.linha1", envir = .GlobalEnv )
  rm ( "tabela.teste.t.linha2", envir = .GlobalEnv )
  rm ( "tabela.teste.t.linha3", envir = .GlobalEnv )
  rm ( "tabela.medidas.associacao", envir = .GlobalEnv )
  rm ( "tabela.teste.F", envir = .GlobalEnv )
  rm ( "quadro.final"  , envir= .GlobalEnv)
  rm ( "coeficientesL", envir = .GlobalEnv)
  rm ( "confiancaL", envir = .GlobalEnv)
  rm ( "anovaL", envir = .GlobalEnv)
  rm ( "associacaoL", envir = .GlobalEnv)
  rm ( "altura.testes.t", envir = .GlobalEnv) 
  rm ( "altura.teste.F", envir = .GlobalEnv)
  rm ( "largura.teste.F", envir = .GlobalEnv)
  rm ( "altura.medidas.associacao", envir = .GlobalEnv)
  rm ( "largura.medidas.associacao", envir = .GlobalEnv)
  rm ( "altura.total", envir = .GlobalEnv)
  rm ( "largura.total", envir = .GlobalEnv)
  rm ( "proporcao.testes.t", envir = .GlobalEnv)
  rm ( "proporcao.teste.F", envir = .GlobalEnv)
  rm ( "proporcao.medidas.associacao", envir = .GlobalEnv)
  
  fancy.normalidade ( l, df.out, titulo = titulo)
  fancy.rstudent    ( df.out, chave="Id", titulo = titulo )
  fancy.influencia  ( df.out, chave="Id", l , titulo = titulo)
  
  
  return (paste ( "~/"
                  , titulo
                  , " - ANOVA - "
                  , sub("~","=", substr(l$call[2],1,nchar(l$call[2]))) 
                  , ".png"
                  , sep=""
  ))
}
fancy.colinearidade <- function (l) {
  
  s = summary (l)
  v = vif(l)
  
  #
  ## Estatísticas de Multicolinearidade
  #
  #http://www.statisticalhorizons.com/multicollinearity
  
  tema.default =
    ttheme_default ( 
      core = list ( fg_params = list ( col = "dodgerblue4" ), fontsize = 9
                    , bg_params = list ( fill = c ("white","grey90") )
                    , show.vlines = FALSE
      )
      , row  = list ( fg_params = list ( col = "dodgerblue4" )
                      , bg_params = list ( fontsize = 9, fontface = "bold", fill = c ("white","grey90") )
                      , show.vlines = FALSE
      )
      , col  = list ( fg_params = list ( col = "dodgerblue4" )
                      , bg_params = list ( fontsize = 9, fontface = "bold", fill = c ("white","grey90") )
                      , show.vlines = FALSE
      )
    ) 
  
  if ( is.vector (v) ) {
    dados.estatisticas.colinearidade <<- 
      format ( round ( data.frame ( row.names = row.names(v)
                                    , data.frame ( c ( 0, ( 1 / vif ( l ) ) ) )                    
                                    , data.frame ( c ( 0,       vif ( l )   ) ) 
      )
      , 3 ), nsmall = 3 )
    names(dados.estatisticas.colinearidade) = c ( "Tolerância", "VIF" )
    tabela.estatisticas.colinearidade.col1 <<- 
      tableGrob ( cbind ( row.names ( dados.estatisticas.colinearidade )
                          , dados.estatisticas.colinearidade[,c(1:2)]
      )  
      , cols = c ( "Variável", "Tolerância", "VIF" )
      , theme = tema.default 
      )
    
  } else {
    dados.estatisticas.colinearidade <<- 
      format ( round ( data.frame ( v ), 3 ), nsmall = 3 )
    names(dados.estatisticas.colinearidade) = c ( "GVIF", "DF", "GVIF Ajustado" )
    tabela.estatisticas.colinearidade.col1 <<- 
      tableGrob ( dados.estatisticas.colinearidade 
                  , theme = tema.default
      )
    
  } 
  
  tabela.estatisticas.colinearidade.cab1 <<- 
    textGrob ( "\nEstatísticas de Colinearidade"
               , gp = gpar ( fontsize = 11 )
    ) 
  
  tabela.estatisticas.colinearidade.linha1 <<- 
    tabela.estatisticas.colinearidade.cab1
  
  tabela.estatisticas.colinearidade.linha2 <<- 
    tabela.estatisticas.colinearidade.col1
  
  #
  ##
  #
  
  quadro.tolerancia <<- 
    ggplot ( data.frame ( x = c ( 0, 0.1, 0.2, 1 ), y = c (1,1,1,1) ), aes(x=x,y=y)) + 
    geom_rect ( aes ( xmin  = 0
                      , xmax  = 0.1
                      , ymin  = 0
                      , ymax  = 0.5
    )
    , alpha = 0.1
    , fill  = "red"
    ) +
    ggtitle ("Interpretação da Tolerância")+
    geom_rect ( aes ( xmin  = 0.1
                      , xmax  = 0.2
                      , ymin  = 0
                      , ymax  = 0.5
    )
    , alpha = 0.1
    , fill  = "grey"
    ) +
    geom_rect ( aes ( xmin  = 0.2
                      , xmax  = 1
                      , ymin  = 0
                      , ymax  = 0.5
    )
    , alpha = 0.1
    , fill  = "blue"
    ) +
    geom_text ( data = data.frame (label="0"), aes ( label = label, x = 0   , y = -0.15 ), size=4 ) +
    geom_text ( data = data.frame (label="0,10"), aes ( label = label, x = 0.10, y =  0.65 ), size=4 ) +
    geom_text ( data = data.frame (label="0,20"), aes ( label = label, x = 0.20, y = -0.15 ), size=4 ) +
    geom_text ( data = data.frame (label="1"), aes ( label = label, x = 1   , y =  0.65 ), size=4 ) +
    theme_bw()+
    theme   ( legend.spacing     = unit ( 0, "mm" )
              , rect              = element_rect (color = NA)
              , line              = element_line (color = NA)
              , axis.title.x      = element_text (color = NA, size = 0.000000000000001)  
              , axis.text.x       = element_text (color = NA, size = 0.000000000000001)  
              , axis.ticks.x      = element_line (color = NA)
              , axis.title.y      = element_text (color = NA, size = 0.000000000000001)  
              , axis.text.y       = element_text (color = NA, size = 0.000000000000001)  
              , axis.ticks.y      = element_line (color = NA)
              , panel.border      = element_rect (color = NA)
              , panel.grid        = element_line (color = NA)
              , panel.grid.major        = element_line (color = NA)
              , panel.grid.minor        = element_line (color = NA)
              , panel.grid.major.x        = element_line (color = NA)
              , panel.grid.minor.y        = element_line (color = NA)
              , panel.grid.major.x        = element_line (color = NA)
              , panel.grid.minor.y        = element_line (color = NA)
              , panel.spacing        = unit ( 2, "mm" ) 
              , plot.margin       = unit ( c ( 2, 2, 2, 2 ), "mm"  ) 
              , text              = element_text ( family="Helvetica", size=9, face="plain")
    ) 
  
  #
  ## Diagnóstico de Multicolinearidade
  #
  
  x0 = rep ( 1, nrow ( l$model ) )
  Z  =  x0 / sqrt ( sum ( x0^2 ) )
  for ( i in 2:length ( l$coefficients ) ) {
    Z = cbind ( Z, l$model[,i] / sqrt ( sum ( l$model[,i] ^ 2 ) ) )
  }  
  ZTZ = crossprod(Z,Z)
  ev  = eigen(ZTZ)$values
  v   = eigen(ZTZ)$vectors
  ci  = sqrt ( max(ev) / ev ) 
  
  # http://books.google.com.br/books?id=Be_XlY13r-EC&pg=PA175&lpg=PA175&dq=how+to+calculate+condition+index+from+eigenvalues&source=bl&ots=lMtLUnjawG&sig=_ZUhY7F7bvbCcwsBjbLmYbka-x0&hl=pt-BR&sa=X&ei=f6o4U9HBDMeV0QGN1YDwCA&ved=0CIUBEOgBMAk#v=onepage&q=how%20to%20calculate%20condition%20index%20from%20eigenvalues&f=false
  
  a <- numeric ( length ( l$coefficients )  ) 
  p = matrix ( numeric ( length ( l$coefficients ) * length ( l$coefficients ) ) 
               , nrow = length ( l$coefficients )
               , ncol = length ( l$coefficients )
  )           
  for ( k in 1:length ( l$coefficients ) ) { 
    a[k] = sum (v[k,]^2/ev) 
    for (j in 1:length ( l$coefficients ) ) {
      p[j,k]<-(v[k,j]^2/ev[j])/a[k]
    }
  }
  
  dados.diagnostico.colinearidade <<- 
    format ( round ( data.frame ( ev, ci, p ), 3), nsmall=3)
  
  names(dados.diagnostico.colinearidade) = c ( "Eigenvalue", "Condition.Index", names(l$coefficients) )
  
  tabela.diagnostico.colinearidade.col1 <<- 
    tableGrob ( dados.diagnostico.colinearidade[,c(1:2)]  
                , cols = c ( "Eigenvalue", "Condition Index" )
                , theme = tema.default
    )
  
  tabela.diagnostico.colinearidade.col2 <<- 
    tableGrob ( dados.diagnostico.colinearidade[,-c(1:2)]
                , cols = names(l$coefficients) 
                , theme = tema.default
    )
  
  tabela.diagnostico.colinearidade.cab1 <<- 
    textGrob ( "\nDiagnósticos de\nColinearidade"
               , gp = gpar ( fontsize = 11 )
    ) 
  
  tabela.diagnostico.colinearidade.cab2 <<- 
    textGrob ( "\nProporções\nda Variação"
               , gp = gpar ( fontsize = 11 )
    ) 
  
  tabela.diagnostico.colinearidade.linha1 <<- 
    arrangeGrob ( tabela.diagnostico.colinearidade.cab1
                  , tabela.diagnostico.colinearidade.cab2
                  , ncol = 2
                  , nrow = 1
                  , widths = proporcoesL ( list ( tabela.diagnostico.colinearidade.col1
                                                  , tabela.diagnostico.colinearidade.col2 
                  ) 
                  )
    )
  
  tabela.diagnostico.colinearidade.linha2 <<- 
    arrangeGrob ( tabela.diagnostico.colinearidade.col1
                  , tabela.diagnostico.colinearidade.col2
                  , ncol = 2
                  , nrow = 1
                  , widths = proporcoesL ( list ( tabela.diagnostico.colinearidade.col1
                                                  , tabela.diagnostico.colinearidade.col2 
                  ) 
                  )
    )  
  
  tabela.diagnostico.colinearidade <<-
    arrangeGrob ( tabela.diagnostico.colinearidade.linha1
                  , tabela.diagnostico.colinearidade.linha2
                  , ncol = 1
                  , nrow = 2
                  , heights = proporcoesA ( list ( tabela.diagnostico.colinearidade.cab1
                                                   , tabela.diagnostico.colinearidade.col1 
                  ) 
                  )
    )       
  
  altura.tabela.estatisticas.colinearidade <<-   
    ( altura (tabela.estatisticas.colinearidade.cab1) 
      + altura (tabela.estatisticas.colinearidade.col1)
    )  
  altura.tabela.diagnostico.colinearidade <<-   
    ( altura (tabela.diagnostico.colinearidade.cab1) 
      + altura (tabela.diagnostico.colinearidade.col1)
    )    
  largura.tabela.estatisticas.colinearidade <<-   
    ( largura (tabela.estatisticas.colinearidade.col1)
    ) 
  largura.tabela.diagnostico.colinearidade <<-   
    ( largura (tabela.diagnostico.colinearidade.col1) 
      + largura (tabela.diagnostico.colinearidade.col2)
    )  
  
  altura.total <<-
    ( altura.tabela.estatisticas.colinearidade
      + altura.tabela.diagnostico.colinearidade
    )
  largura.total <<-
    ( largura.tabela.diagnostico.colinearidade + 0.5)  
  
  tabela.estatisticas.colinearidade <<-
    arrangeGrob ( 
      arrangeGrob ( tabela.estatisticas.colinearidade.linha1
                    , tabela.estatisticas.colinearidade.linha2
                    , ncol = 1
                    , nrow = 2
                    , heights = proporcoesA ( list ( tabela.estatisticas.colinearidade.cab1
                                                     , tabela.estatisticas.colinearidade.col1
                    )
                    )
      )
      , quadro.tolerancia 
      , ncol = 2
      , nrow = 1 
      , widths = c ( round (largura.tabela.estatisticas.colinearidade / largura.total,1)
                     , 1 - round(largura.tabela.estatisticas.colinearidade / largura.total,1)
      )
    )
  
  
  quadro.final <<- 
    arrangeGrob ( tabela.estatisticas.colinearidade
                  , tabela.diagnostico.colinearidade  
                  , heights = c ( altura.tabela.estatisticas.colinearidade / altura.total
                                  , 1 - altura.tabela.estatisticas.colinearidade / altura.total
                  )
    )
  
  grid.arrange (quadro.final)
  
  png ( filename = "~/Colinearidade.png"
        , width  = largura.total
        , height = altura.total
        , units = "mm"
        , res=92
  )
  grid.arrange ( quadro.final )
  dev.off()
}
fancy.residuos   <- function (df, titulo="") {
  
  df.resid <<-  data.frame(resid=df[,c("resid")], x=seq ( 1, nrow(df), 1 ))
  
  resid.grafico <<- 
    ggplot ( df.resid, aes ( x=df.resid[,2], y=df.resid[,1] ) ) +
    ggtitle( "Resíduos") +
    geom_hline         ( aes ( yintercept = 0 ) ) +
    geom_text          ( data = data.frame ( label= "Subestimado: Estimado < Realizado" )
                         , aes ( label = label
                                 , x = max ( df.resid[,2] )
                                 , y = min ( df.resid[,1] ) )
                         , size  = 5
                         , hjust = 1
                         , vjust = -0.2
                         , color = '#0072B2'
    ) +
    geom_text          ( data = data.frame ( label= "Superestimado: Estimado > Realizado\n" )
                         , aes ( label = label
                                 , x = max ( df.resid[,2] )
                                 , y = max ( df.resid[,1] ) )
                         , size  = 5
                         , hjust = 1
                         , vjust = 1.5
                         , color = "#E69F00"
    ) +
    geom_point         ( data=df.resid[df.resid$resid>0,]
                         , aes ( x=df.resid[df.resid$resid>0,2]
                                 , y=df.resid[df.resid$resid>0,1] )
                         , shape=24
                         , size = 2
                         , color = '#0072B2'
    )+
    geom_point         ( data=df.resid[df.resid$resid<=0,]
                         , aes ( x=df.resid[df.resid$resid<=0,2]
                                 , y=df.resid[df.resid$resid<=0,1] )
                         , shape=25
                         , size = 2 
                         , color = "#E69F00"
    )+
    geom_path          (  color = "gray50", size =.25 ) +
    scale_x_continuous(breaks=seq(1,max(df.resid$x),1)
                       , labels = point_format()) +
    scale_y_continuous( labels = point_format() ) +
    theme_fivethirtyeight()
  
  png ( filename = paste("~/", titulo, "_Residuos.png", sep="") 
        , width  = 210
        , height = 210
        , units  = "mm"
        , res    = 92
  )
  
  grid.arrange ( resid.grafico )
  
  dev.off()
}
fancy.rstudent   <- function (df, chave, titulo="") {
  
  titulo = gsub ( "[/]","e", titulo )
  
  df.rstudent <<- df [ !is.na ( df$rstudent ), c ( "rstudent", chave ) ]
  df.rstudent$N <<- seq ( 1, nrow(df.rstudent), 1 )
  
  globalChave <<- chave
  
  rstudent.grafico <<- 
    ggplot ( df.rstudent, aes ( x = df.rstudent$N
                                , y = df.rstudent$rstudent 
    ) ) +
    ggtitle( "Studentizados") +
    geom_point         ( size  = 2 ) +
    geom_path          ( size  = 0.25
                         , color = "gray50"
    ) +
    ylab ("Resíduos Studentizados")+
    xlab ("Casos")+
    scale_y_continuous ( limits = c ( round ( min ( -3, -1 * ( max ( abs ( df.rstudent$rstudent ) ) + 1 ) ) )
                                      , round ( max (  3,      ( max ( abs ( df.rstudent$rstudent ) ) + 1 ) ) ) 
    )
    , breaks = seq ( round ( min ( -3, -1 * ( max ( abs ( df.rstudent$rstudent ) ) + 1 ) ) )
                     , round ( max (  3,      ( max ( abs ( df.rstudent$rstudent ) ) + 1 ) ) )
                     , 1
    )
    ) +   
    geom_rect         ( data = data.frame ( xmin = -Inf
                                            , xmax = +Inf
                                            , ymin = -2
                                            , ymax = -Inf
    )
    , aes ( xmin = xmin
            , xmax = xmax
            , ymin = ymin
            , ymax = ymax
    )                
    , alpha = 0.1
    , fill = "#DC8E05"
    , inherit.aes = FALSE
    ) +  
    geom_rect         ( data = data.frame ( xmin = -Inf
                                            , xmax = +Inf
                                            , ymin = +2
                                            , ymax = +Inf
    )
    , aes ( xmin = xmin
            , xmax = xmax
            , ymin = ymin
            , ymax = ymax
    )                
    , alpha = 0.1
    , fill = "#DC8E05"
    , inherit.aes = FALSE
    ) +      
    geom_hline         ( aes ( yintercept =  2 ), linetype="dashed", size=0.25, color="#DC8E05" ) +
    geom_hline         ( aes ( yintercept =  0 ) ) +
    geom_hline         ( aes ( yintercept = -2 ), linetype="dashed", size=0.25, color="#DC8E05" ) + 
    scale_x_continuous(breaks=seq(1,max(df.rstudent$N),2)
                       , labels = point_format()) +
    theme_fivethirtyeight() 
  
  if ( nrow (df.rstudent[ abs ( df.rstudent$rstudent ) > 2, ]) > 0 ) {
    rstudent.tabela <<- 
      tableGrob ( data.frame ( c1 =         df.rstudent[ abs ( df.rstudent$rstudent ) > 2, globalChave ]
                               , c2 = round ( df.rstudent[ abs ( df.rstudent$rstudent ) > 2, "rstudent"  ], 3)
      )
      , cols = c ( globalChave                           
                   , "Resíduos\nStudentizados"
      )
      , rows=NULL, theme=tema.anova
      )
  } else {
    rstudent.tabela <<- 
      textGrob ("")
  }   
  rstudent.final <<-
    arrangeGrob ( rstudent.grafico
                  , rstudent.tabela
                  , ncol = 2
                  , nrow = 1
                  , widths = c ( 65, 35 ) 
    )  
  
  png ( filename = paste ( "~/", titulo, " - Valores Extremos (student).png", sep="" )
        , width  = 210   
        , height = 105
        , units  = "mm"
        , res    = 92
  )
  
  grid.arrange ( rstudent.final )
  
  dev.off()
}
fancy.rstandard   <- function (df, chave, titulo="") {
  
  titulo = gsub ( "[/]","e", titulo )
  
  df.rstandard <<- df [ !is.na ( df$rstandard ), c ( "rstandard", chave ) ]
  df.rstandard$N <<- seq ( 1, nrow(df.rstandard), 1 )
  
  globalChave <<- chave
  
  rstandard.grafico <<- 
    ggplot ( df.rstandard, aes ( x = df.rstandard$N
                                 , y = df.rstandard$rstandard 
    ) ) +
    geom_point         ( size  = 3 ) +
    geom_path          ( size  = 0.25
                         , color = "gray50"
    ) +
    ylab ("Resíduos Padronizados")+
    xlab ("Casos")+
    scale_y_continuous ( limits = c ( round ( min ( -4, -1 * ( max ( abs ( df.rstandard$rstandard ) ) + 1 ) ) )
                                      , round ( max (  4,      ( max ( abs ( df.rstandard$rstandard ) ) + 1 ) ) ) 
    )
    , breaks = seq ( round ( min ( -4, -1 * ( max ( abs ( df.rstandard$rstandard ) ) + 1 ) ) )
                     , round ( max (  4,      ( max ( abs ( df.rstandard$rstandard ) ) + 1 ) ) )
                     , 1
    )
    ) +   
    #     geom_text          ( data = data.frame ( y    = df.rstandard [ abs ( df.rstandard$rstandard ) > 3, "rstandard" ]  
    #                                            , x    = df.rstandard [ abs ( df.rstandard$rstandard ) > 3, "N"        ]
    #                                            )
    #                        , aes ( x = x
    #                              , y = y 
    #                              )
    #                         , label = paste ( " (", df.rstandard[abs(df.rstandard$rstandard)>3,globalChave],")"
    #                                         )
    #                         , size  = 4
    #                         , hjust = 0
    #                         , vjust = 0
  #                         , color ="red"
  #     ) +    
  geom_rect         ( data = data.frame ( xmin = -Inf
                                          , xmax = +Inf
                                          , ymin = -3
                                          , ymax = -Inf
  )
  , aes ( xmin = xmin
          , xmax = xmax
          , ymin = ymin
          , ymax = ymax
  )                
  , alpha = 0.1
  , fill  = "#DC8E05"
  , inherit.aes = FALSE
  ) +  
    geom_rect         ( data = data.frame ( xmin = -Inf
                                            , xmax = +Inf
                                            , ymin = +3
                                            , ymax = +Inf
    )
    , aes ( xmin = xmin
            , xmax = xmax
            , ymin = ymin
            , ymax = ymax
    )                
    , alpha = 0.1
    , fill  = "#DC8E05"
    , inherit.aes = FALSE
    ) +  
    geom_hline         ( aes ( yintercept =  3 ), linetype="dashed", size=0.5, color="#DC8E05" ) +
    geom_hline         ( aes ( yintercept =  0 ) ) +
    geom_hline         ( aes ( yintercept = -3 ), linetype="dashed", size=0.5, color="#DC8E05" ) + 
    theme_fivethirtyeight() 
  
  if ( nrow (df.rstandard[ abs ( df.rstandard$rstandard ) > 3, ]) > 0 ) {
    rstandard.tabela <<- 
      tableGrob ( data.frame ( c1 =         df.rstandard[ abs ( df.rstandard$rstandard ) > 2, globalChave ]
                               , c2 = round ( df.rstandard[ abs ( df.rstandard$rstandard ) > 2, "rstandard"  ], 3)
      )
      , cols = c ( globalChave                           
                   , "Resíduos\nPadronizados"
      )
      , rows=NULL, theme=tema.anova
      )
  } else {
    rstandard.tabela <<- 
      textGrob ("")
  }   
  rstandard.final <<-
    arrangeGrob ( rstandard.grafico
                  , rstandard.tabela
                  , ncol = 2
                  , nrow = 1
                  , widths = c ( 65, 35 ) 
    )  
  
  png ( filename = paste ( "~/", titulo, " - Valores Extremos (standard).png", sep="" ) 
        , width  = 210   
        , height = 105
        , units  = "mm"
        , res    = 92
  )
  
  grid.arrange ( rstandard.final )
  
  dev.off()
}
fancy.influencia <- function (df, chave="Id", l, titulo="") {
  
  titulo = gsub ( "[/]","e", titulo )
  
  globalChave <<- chave  
  
  #
  ## COOK's
  #
  
  df.cooks   <<- df [ !is.na ( df$cooks.distance ), c( "cooks.distance", chave ) ]
  df.cooks$N <<- seq ( 1, nrow ( df.cooks ), 1 )
  
  cutoff <<- 4 / ( ( nrow ( df ) - length ( l$coefficients ) - 2 ) ) 
  cutoff <<- qf ( 0.5, length ( l$coefficients ), nrow (df) - length ( l$coefficients ) -1 )
  cutoff <<- 4 / nrow ( df )
  
  cooks.distance.grafico <<- ggplot ( df.cooks, aes ( x=df.cooks$N, y=df.cooks$cooks.distance ) ) +
    ggtitle( "Distância de Cook") +
    scale_y_continuous ( limits = c   ( 0, ceiling ( max ( df.cooks$cooks.distance ) )     ) 
                         , breaks = seq ( 0, ceiling ( max ( df.cooks$cooks.distance ) ), 0.2 )
    ) +
    scale_x_continuous(breaks=seq(1,max(df.cooks$N),2)
                       , labels = point_format()) +
    geom_path          ( size   = 0.25
                         , color ="gray50"
    ) +
    geom_point         ( size = 2 ) +
    geom_text          ( data = data.frame ( label= paste ( round ( cutoff, 4 ), "\n" ) )
                         , aes ( label = label
                                 , x = max(df.cooks$N)
                                 , y = cutoff )
                         , size  = 5
                         , hjust = 1
                         , vjust = 0
                         , color = "red"
    ) +
    geom_hline         ( aes ( yintercept = cutoff )
                         , linetype = "dashed"
                         , size     = 0.5
                         , color    = "blue"
    ) +
    geom_hline         ( aes ( yintercept = 1 )
                         , linetype = "dotted"
                         , size     = 0.25
                         , color    = "blue"
    ) +
    geom_rect         ( data = data.frame ( xmin = -Inf
                                            , xmax = +Inf
                                            , ymin = cutoff
                                            , ymax = 1
    )
    ,  aes ( xmin = xmin
             , xmax = xmax
             , ymin = ymin
             , ymax = ymax
    )                
    , alpha = 0.1
    , fill  = "#DC8E05"
    , inherit.aes = FALSE
    ) +    
    geom_rect         ( data = data.frame ( xmin = -Inf
                                            , xmax = +Inf
                                            , ymin = 1
                                            , ymax = +Inf
    )
    ,  aes ( xmin = xmin
             , xmax = xmax
             , ymin = ymin
             , ymax = ymax
    )                
    , alpha = 0.2
    , fill  = "#DC8E05"
    , inherit.aes = FALSE
    ) +    
    theme_fivethirtyeight()
  
  if ( nrow (df.cooks [ df.cooks$cooks.distance > cutoff , ] ) > 0 ) {
    cooks.distance.tabela <<- 
      tableGrob ( data.frame ( c1 = df.cooks [ abs ( df.cooks$cooks.distance ) > cutoff, globalChave ]
                               , c2 = round ( df.cooks [ abs ( df.cooks$cooks.distance ) > cutoff, "cooks.distance"  ], 3)
      )
      , cols = c ( globalChave                           
                   , "Distância\nde Cook"
      )
      , rows=NULL, theme=tema.anova
      )
  } else {
    cooks.distance.tabela <<- 
      textGrob ("")
  }
  
  #
  ## DFFITS
  #
  
  df.dffits   <<- df [ !is.na ( df$dffits ), c ( "dffits", chave ) ]
  df.dffits$N <<- seq ( 1, nrow ( df.dffits ), 1 )
  
  cutoff.dffits <<- 2 * sqrt ( ( length ( l$coefficients ) )
                               / ( nrow ( df ) ) 
  ) 
  
  dffits.grafico <<- 
    ggplot ( df.dffits, aes ( x=df.dffits$N, y=df.dffits$dffits ) ) +
    ggtitle ( "DFFITS" ) +
    scale_x_continuous ( breaks=seq(1,max(df.dffits$N),2)
                       , labels = point_format()) +
    scale_y_continuous ( limits = c   ( round ( min ( -1, min ( df.dffits$dffits ) - 1 ) )  
                                        , round ( max (  1, max ( df.dffits$dffits ) + 1 ) ) )
                         , breaks = seq ( round ( min ( -1, min ( df.dffits$dffits ) - 1 ) )  
                                          , round ( max (  1, max ( df.dffits$dffits ) + 1 ) )
                                          , .5 
                         ) 
    ) +   
    geom_path          ( size  = 0.25
                         , color ="gray50"
    ) +
    geom_point         ( size = 2 ) +
    geom_text         ( data = data.frame ( label= paste ( "\n",round ( cutoff.dffits, 4 ) ) )
                        , aes ( label = label
                                , x     = max ( df.dffits$N )
                                , y     = round ( max (  1, max ( df.dffits$dffits )  + 1 ) ) )
                        , size     = 5
                        , hjust    = 1
                        , vjust    = 1
                        , color    ="red"
    ) +
    geom_hline        ( aes ( yintercept = 1 )
                        , linetype = "dotted"
                        , size     = 0.25
                        , color    = "blue"
    ) + 
    geom_hline        ( aes ( yintercept = cutoff.dffits )
                        , linetype = "dashed"
                        , size     = 0.25
                        , color    = "blue"
    ) + 
    geom_hline        ( aes ( yintercept = 0 ) 
    ) +
    geom_hline        ( aes ( yintercept = -cutoff.dffits )
                        , linetype = "dashed"
                        , size     = 0.25
                        , color    = "blue"
    ) +
    geom_hline        ( aes ( yintercept = -1 )
                        , linetype = "dotted"
                        , size     = 0.25
                        , color    = "blue"
    ) + 
    geom_rect         ( data = data.frame ( xmin = -Inf
                                            , xmax = +Inf
                                            , ymin = +1
                                            , ymax = +Inf
    )
    ,  aes ( xmin = xmin
             , xmax = xmax
             , ymin = ymin
             , ymax = ymax
    )                
    , alpha = 0.2
    , fill  = "#DC8E05"
    , inherit.aes = FALSE
    ) +
    geom_rect         ( data = data.frame ( xmin = -Inf
                                            , xmax = +Inf
                                            , ymin = -cutoff.dffits
                                            , ymax = -1
    )
    ,  aes ( xmin = xmin
             , xmax = xmax
             , ymin = ymin
             , ymax = ymax
    )                
    , alpha = 0.1
    , fill  = "#DC8E05"
    , inherit.aes = FALSE
    ) +
    geom_rect         ( data = data.frame ( xmin = -Inf
                                            , xmax = +Inf
                                            , ymin = +cutoff.dffits
                                            , ymax = +1
    )
    ,  aes ( xmin = xmin              
             , xmax = xmax
             , ymin = ymin
             , ymax = ymax
    )              
    , alpha = 0.1
    , fill  = "#DC8E05"
    , inherit.aes = FALSE
    ) +
    geom_rect         ( data = data.frame ( xmin = -Inf
                                            , xmax = +Inf
                                            , ymin = -1
                                            , ymax = -Inf
    )
    ,  aes ( xmin = xmin
             , xmax = xmax
             , ymin = ymin
             , ymax = ymax
    )                
    , alpha = 0.2
    , fill  = "#DC8E05"
    , inherit.aes = FALSE
    ) +
    theme_fivethirtyeight()
  
  if ( nrow ( df.dffits [ abs ( df.dffits$dffits ) > cutoff.dffits, ] ) > 0 )  {
    
    influentes <<- df.dffits [ abs ( df.dffits$dffits ) > cutoff.dffits, c(globalChave, "dffits") ]
    
    dffits.tabela <<- 
      tableGrob ( data.frame ( c1 =         influentes [, globalChave ]
                             , c2 = round ( influentes [, "dffits"    ], 3 )
      )
      , cols = c ( globalChave                           
                   , "DFFITS"
      )
      , rows=NULL, theme=tema.anova)
    write.table ( influentes 
                  , quote        = F
                  , row.names    = F
                  , sep          = ";"
                  , dec          = ","
                  , fileEncoding = "UTF-8"
                  , paste( "~/", titulo, "- Valores Influentes (DIFFS_Cooks).csv", sep="" ) )
    
  } else {
    dffits.tabela <<- 
      textGrob ("")
  }
  
  png ( filename = paste( "~/", titulo, "- Valores Influentes (DIFFS_Cooks).png", sep="" )
        , width  = 210   
        , height = 210
        , units  = "mm"
        , res    = 92
  )  
  
  grid.arrange ( 
    arrangeGrob ( dffits.grafico
                  , dffits.tabela
                  , ncol = 2
                  , nrow = 1
                  , widths = c ( 70, 30 ) 
    )
    , arrangeGrob ( cooks.distance.grafico
                    , cooks.distance.tabela
                    , ncol = 2
                    , nrow = 1
                    , widths = c ( 70, 30 ) 
    )
    , ncol = 1
    , nrow = 2
    , heights=c(50,50))
  
  dev.off()
  
}
fancy.autocorrelacao <- function (l,valorChave, nomeChave) {
  
  require(lmtest)
  
  dw <<- dwtest(l)
  
  dados.dw <<-
    data.frame ( paste ( format ( dw$statistic, digits = 3, nsmall = 3  )
                         , signif.code ( dw$p.value ) 
    )
    ) 
  
  names     (dados.dw) = c("Durbin-Watson")
  row.names (dados.dw) = c("Valor")
  
  tabela.dw <<- tableGrob (
    dados.dw,
    cols =  c ( "Durbin-Watson"
    )
    , show.rownames = FALSE
    , gpar.corefill = gpar ( fill = "white" , col = "black", show.vlines = FALSE )
    , gpar.rowfill  = gpar ( fill = "gray90", col = "black", show.vlines = FALSE )
    , gpar.colfill  = gpar ( fill = "gray90", col = "black", show.vlines = FALSE )
    , gpar.coltext  = gpar ( fontsize=11  )
    , gpar.rowtext  = gpar ( fontsize=11, fontface="bold" )
    , gpar.coretext = gpar ( fontsize=11  )
    , core.just     = "center" )
  
  altura.tabela.dw <<-
    altura  ( tabela.dw ) 
  
  largura.tabela.dw <<-
    largura ( tabela.dw ) 
  
  #
  ##
  #  
  
  d <<- dw_critical ( l$rank-1
                      , nrow ( l$model )
  )  
  if ( !is.na ( d[1] ) ) {
    
    altura.quadro.dw <<- 40
    
    quadro.dw <<- 
      ggplot ( data.frame(x=seq(0,4),y=rep(1.2,5)), aes ( x = x ) ) + 
      geom_text ( data = data.frame (label="0"                             ), aes ( label = label, x = 0             , y = -0.07 ), size=4, vjust=0 ) +
      geom_text ( data = data.frame (label=paste( "d","[l] =" , d[1]   )   ), aes ( label = label, x = d[1]   - 0.02 , y = 0     ), size=4, vjust=0, hjust=1 ) +
      geom_text ( data = data.frame (label=paste( "d","[u] =" , d[2]   )   ), aes ( label = label, x = d[2]   - 0.02 , y = -0.17 ), size=4, vjust=0, hjust=1) +
      geom_text ( data = data.frame (label="2"                             ), aes ( label = label, x = 2             , y = 0     ), size=4, vjust=0) +
      geom_text ( data = data.frame (label=paste("4 - d[u] =" , 4-d[2] )   ), aes ( label = label, x = 4-d[2] + 0.02 , y = -0.17 ), size=4, vjust=0, hjust=0 ) +
      geom_text ( data = data.frame (label=paste("4 - d[l] =" , 4-d[1] )   ), aes ( label = label, x = 4-d[1] + 0.02 , y = 0     ), size=4, vjust=0, hjust=0 ) +
      geom_text ( data = data.frame (label="4"                             ), aes ( label = label, x = 4             , y = -0.07 ), size=4, vjust=0 ) +
      geom_text ( data = data.frame (label="Autocorrelação Positiva"       ), aes ( label = label, x = d[1]/2        , y =  0.70 ), size=4, vjust=0 ) +
      geom_text ( data = data.frame (label="Ausência de Autocorrelação"    ), aes ( label = label, x = 2             , y =  1.00 ), size=4, vjust=0 ) +
      geom_text ( data = data.frame (label="Autocorrelação Negativa"       ), aes ( label = label, x = 4-d[1]+d[1]/2 , y =  0.70 ), size=4, vjust=0 ) +
      geom_segment    ( aes ( x    = d[1] + (d[2]-d[1])/2 
                              , y    = 0.6
                              , xend = d[1]-0.2
                              , yend = 0.9
      )
      , arrow = arrow(length = unit(0.2, "cm"))
      , size  = 0.25
      ) +
      geom_segment    ( aes ( x    = (4-d[2])+((4-d[1])-(4-d[2]))/2  
                              , y    = 0.6
                              , xend = (4-d[1])+0.2
                              , yend = 0.9
      )  
      , size  = 0.25
      , arrow = arrow(length = unit(0.2, "cm"))
      ) +
      geom_text       ( data = data.frame (label="Não Conclusivo"), aes ( label = label, x = d[1]     - (0.02+0.2), y =  0.9 + 0.02), size=4, vjust = 0, hjust=1 ) +
      geom_text       ( data = data.frame (label="Não Conclusivo"), aes ( label = label, x = (4-d[1]) + (0.02+0.2), y =  0.9 + 0.02), size=4, vjust = 0, hjust=0 ) +
      geom_segment    ( aes ( x = as.numeric(dw$statistic) 
                              , y    = 0.6
                              , xend = as.numeric(dw$statistic)  
                              , yend = 0.3
      )
      , arrow = arrow(length = unit(0.2, "cm"))
      , color = "red"
      , size  = 1
      ) +
      geom_text       ( data = data.frame ( label=format ( as.numeric (dw$statistic)
                                                           , digits = 3
                                                           , nsmall = 3  
      )
      )
      , aes ( label = label
              , x = as.numeric(dw$statistic) + 0.02
              , y =  0.5 )
      , size  = 5
      , vjust = 0
      , hjust = 0 
      ) +
      geom_segment    ( aes ( x    = d[1] 
                              , y    = 0.5
                              , xend = d[1]
                              , yend = 0
      )
      , linetype = "dashed"
      , size     = 0.25
      ) +
      geom_segment    ( aes ( x    = d[2]
                              , y    = 0.6
                              , xend = d[2]
                              , yend = -0.17
      )
      , linetype = "dashed"
      , size      = 0.25
      ) +
      geom_segment    ( aes ( x    = 4-d[2]
                              , y    = 0.6
                              , xend = 4-d[2] 
                              , yend = -0.17
      )
      , linetype = "dashed"
      , size     = 0.25
      ) +
      geom_segment    ( aes ( x    = 4-d[1]
                              , y    = 0.5
                              , xend = 4-d[1]
                              , yend = 0
      )
      , linetype = "dashed"
      , size     = 0.25
      ) +  
      geom_segment    ( aes ( x    = 2-0.04
                              , y    = 0
                              , xend = 2-0.04
                              , yend = -0.30
      )
      , size    = 0.25  
      ) +
      geom_segment    ( aes ( x    = 2-0.04
                              , y    = -0.30
                              , xend = 0
                              , yend = -0.30
      )
      , arrow = arrow(length = unit(0.2, "cm"))
      , size  = 0.25
      ) +  
      geom_segment    ( aes ( x    = 2+0.04
                              , y    = 0
                              , xend = 2+0.04
                              , yend = -0.30
      )
      , size  = 0.25  
      ) +
      geom_segment    ( aes ( x    = 2+0.04
                              , y    = -0.30
                              , xend = 4
                              , yend = -0.30
      )
      , arrow = arrow(length = unit(0.2, "cm"))
      , size  = 0.25
      ) +
      geom_rect ( aes ( xmin  = 0
                        , xmax  = d[1]
                        , ymin  = 0.2
                        , ymax  = 0.5
      )
      , alpha = 0.1
      , fill  = "red"
      )  +
      geom_rect ( aes ( xmin  = d[1]
                        , xmax  = d[2]
                        , ymin  = 0.2
                        , ymax  = 0.6
      )
      , alpha = 0.1
      , fill  = "gray50"
      ) +
      geom_rect ( aes ( xmin  = d[2]
                        , xmax  = 4-d[2]
                        , ymin  = 0.2
                        , ymax  = 0.7
      )
      , alpha = 0.1
      , fill  = "blue"
      ) +
      geom_rect ( aes ( xmin  = 4-d[2]
                        , xmax  = 4-d[1]
                        , ymin  = 0.2
                        , ymax  = 0.6
      )
      , alpha = 0.1
      , fill  = "grey50"
      ) +
      geom_rect ( aes ( xmin  = 4-d[1]
                        , xmax  = 4
                        , ymin  = 0.2
                        , ymax  = 0.5
      )
      , alpha = 0.1
      , fill  = "red"
      ) +
      theme_bw() +
      theme   ( legend.spacing     = unit ( 0, "mm" )
                , axis.title.x      = element_text (color = NA, size = 0.000000000000001)  
                , axis.text.x       = element_text (color = NA, size = 0.000000000000001)  
                , axis.ticks.x      = element_line (color = NA)
                , axis.title.y      = element_text (color = NA, size = 0.000000000000001)  
                , axis.text.y       = element_text (color = NA, size = 0.000000000000001)  
                , axis.ticks.y      = element_line (color = NA)
                , panel.border      = element_rect (color = NA)
                , panel.spacing      = unit ( 0, "mm" ) 
                , panel.grid        = element_line (color = NA)
                , panel.grid.major  = element_line (color = NA)
                , panel.grid.minor  = element_line (color = NA) 
                , plot.margin       = unit ( c ( 0, 0, -5.2, -5.2 ), "mm"  ) 
                , text              = element_text ( family="Helvetica", size=11, face="plain")
      ) 
  } else {
    
    altura.quadro.dw <<- 0
    
    quadro.dw <<-tableGrob(" ")
    
  }
  
  #
  ##
  #
  
  df.resid       <<- 
    data.frame ( x.col = as.Date ( valorChave )
                 , resid = resid (l)
    )
  df.resid   <<- df.resid [ order(df.resid[,1]), ]
  globalXlab <<- nomeChave
  
  grafico.residuos <<- 
    ggplot ( df.resid
             , aes ( x=x.col 
                     , y=resid ) ) +
    ylab               ( "Resíduos" ) +
    xlab               ( globalXlab ) +
    geom_hline         ( aes ( yintercept = 0 ) ) +
    geom_text          ( data = data.frame ( label= "Subestimado: Estimado < Realizado" )
                         , aes ( label = label
                                 , x     = max(df.resid[,"x.col"])
                                 , y     = 20 )
                         , size  = 5
                         , hjust = 1
                         , vjust = 0
                         , color = "blue"
    ) +
    geom_text          ( data = data.frame ( label= "Superestimado: Estimado > Realizado\n" )
                         , aes ( label = label
                                 , x     = max(df.resid[,"x.col"])   
                                 , y     = -20 )
                         , size  = 5
                         , hjust = 1
                         , vjust = 1
                         , color = "red3"
    ) +
    geom_point         ( data=df.resid[df.resid$resid>0,]
                         , aes ( x=df.resid[df.resid$resid>0,1]
                                 , y=df.resid[df.resid$resid>0,2] )
                         , shape=24
                         , fill  = "blue"
                         , color = "blue"
    )+
    geom_point         ( data=df.resid[df.resid$resid<=0,]
                         , aes ( x=df.resid[df.resid$resid<=0,1]
                                 , y=df.resid[df.resid$resid<=0,2] )
                         , shape=25
                         , fill  = "red3"
                         , color = "red3"
    )+
    geom_smooth() +
    theme_bw()
  
  altura.grafico.residuos <<- 60
  
  #
  ##
  #
  
  assunto <<- 
    editGrob ( textGrob ( "Avaliação da Independência dos Resíduos"), vp=viewport(angle=90))
  
  titulo.teste          <<- textGrob ( "Teste Durbin-Watson" )  
  titulo.grafico        <<- textGrob ( "Gráfico para Análise" )
  
  altura.titulo.teste   <<- altura (titulo.teste)
  altura.titulo.grafico <<- altura (titulo.grafico)
  
  altura.total <<-
    ( altura.titulo.teste
      + altura.quadro.dw
      + altura.titulo.grafico
      + altura.grafico.residuos
    )
  
  largura.total <<- 210
  
  quadro.final <<-
    arrangeGrob ( 
      assunto
      , arrangeGrob ( 
        titulo.teste
        , arrangeGrob ( tabela.dw
                        , quadro.dw
                        , ncol   = 2 
                        , nrow   = 1
                        , widths =  c ( largura.tabela.dw/(largura.total*.90)
                                        , 1 - (largura.tabela.dw/(largura.total*.90))
                        )
        )
        , titulo.grafico
        , grafico.residuos
        , ncol = 1
        , nrow = 4 
        , heights = c ( altura.titulo.teste     / altura.total
                        , altura.quadro.dw        / altura.total
                        , altura.titulo.grafico   / altura.total
                        , altura.grafico.residuos / altura.total
        ) 
      )
      , ncol = 2
      , widths = c(0.05,0.95)
    )
  
  png ( filename = paste ( "~/autocorrelação"
                           , sub("~","=", substr(l$call[2],1,nchar(l$call[2]))) 
                           , ".png"
                           , sep=""
  )
  , width  = 210   
  , height = 210  
  , units  = "mm"
  , res    = 92
  )
  
  grid.arrange (quadro.final)
  
  dev.off()
  
  rm ( "grafico.residuos"         , envir = .GlobalEnv )
  rm ( "df.resid"                , envir = .GlobalEnv )
  rm ( "d"                       , envir = .GlobalEnv ) 
  rm ( "dw.table"                , envir = .GlobalEnv )
  rm ( "dw"                      , envir = .GlobalEnv )
  rm ( "quadro.dw"               , envir = .GlobalEnv )
  rm ( "altura.total"            , envir = .GlobalEnv )
  rm ( "altura.tabela.dw"        , envir = .GlobalEnv )
  rm ( "altura.grafico.residuos" , envir = .GlobalEnv )
  rm ( "tabela.dw"               , envir = .GlobalEnv )
  rm ( "quadro.final"            , envir = .GlobalEnv )
  rm ( "largura.tabela.dw"       , envir = .GlobalEnv )
  rm ( "largura.total"           , envir = .GlobalEnv )
  rm ( "globalXlab"              , envir = .GlobalEnv )
  rm ( "altura.quadro.dw"        , envir = .GlobalEnv )
  rm ( "dados.dw"                , envir = .GlobalEnv )
  
}
fancy.heterocedasticidade <- function (l, df.out, df, i, titulo="") {
  
  titulo = gsub ( "[/]","e", titulo )
  
  lista = names(l$coefficients)
  rm_df_0 <<- data.frame ( x = df.out$fit
                          , y = df.out$rstudent.wo.deletion
  )
  rm_pt_0 <<- 
    x.vs.y     ( rm_df_0
                 , "Valores Estimados"
                 , ""
    ) +
    geom_hline ( aes ( yintercept = 0 )
                 , linetype = "solid"
                 , size     = 0.5
                 , color    = "black"
    )
  
  for (j in 1:length(lista))
  { 
    print(j)
    print(lista[j])
    x <<- df [, lista [ j ] ]
    print(x)
    names ( x ) <<- c ( "x" )
    assign ( paste ("rm_df_",i,sep="")
             , data.frame ( x = x, y = df$rstudent.wo.deletion )
             , .GlobalEnv 
    )
    assign ( paste ("rm_pt_",j,sep="")
             , x.vs.y     ( get ( paste("rm_df_", j, sep="" )
                                  , envir = .GlobalEnv
             )
             , lista[j]
             , ""
             ) +
               geom_hline ( aes ( yintercept = 0 )
                            , linetype = "solid"
                            , size     = 0.5
                            , color    = "black"
               )
             , .GlobalEnv 
    ) 
  }
  
  quadro.residuos <<-
    arrangeGrob ( rm_pt_0
                  , rm_pt_1
#                  , rm_pt_2,rm_pt_3,rm_pt_4,rm_pt_5,rm_pt_6
                  , main= textGrob ( paste ( "Resíduos studentizados (sem exclusão) x \n"
                                             , "Valores estimados e Variáveis Independentes"
                  )
                  , gp = gpar( fontsize=11)
                  )
    )
  
  l.teste = lm (ZRE_2 ~ ZPR_2, data=df )
  lp <<- lm (scale(ZRE_2) ~ scale(ZPR_2), data=df )
  nome.anova   <<- 
    fancy.anova(l.teste, Grafico = F)
  
  require ( png )
  
  png.anova <<- 
    readPNG ( nome.anova )
  
  assunto <<- 
    editGrob ( textGrob ( "Avaliação da Homocedasticidade dos Resíduos"), vp=viewport(angle=90))
  titulo.Pesaran  <<- textGrob ( "Teste Pesaran–Pesaran" )  
  titulo.residuos <<- textGrob ( "Gráfico para Análise" )
  
  quadro.Pesaran <<-
    rasterGrob( png.anova, interpolate=TRUE)
  
  quadro.final <<-
    arrangeGrob ( 
      assunto
      , arrangeGrob ( titulo.Pesaran
                      , quadro.Pesaran 
                      , titulo.residuos
                      , quadro.residuos
                      , ncol    = 1
                      , heights = c(5,45,5,45)
      )
      , ncol = 2
      , widths = c(0.05,0.95)
    )
  png ( filename = paste ( "~/", titulo, " - Avaliação da Homocedasticidade"
                           , sub("~","=", substr(l$call[2],1,nchar(l$call[2]))) 
                           , ".png"
                           , sep=""
  )
  , width  = 210
  , height = 210
  , units = "mm"
  , res=92
  )
  
  grid.arrange ( quadro.final )
  
  dev.off()
  
  rm ( list = ls (pattern="rm_df_") )
  rm ( list = ls (pattern="rm_pt_") )
}
fancy.normalidade <- function (l, df, titulo="") {
  
  titulo = gsub ( "[/]","e", titulo )
  
  fancy.dist  ( df=df, "resid", paste ( titulo, " - Resíduos"
                                        , sep=""
  ), incluiNormalidade=T)
  

}
analise.residuos <- function (data, l, chave) {

  fancy.normalidade ( df = data, l = l)
  fancy.rstudent    ( df = data, chave = chave )
  fancy.influencia  ( df = data, chave = chave, l = l )
  fancy.residuos    ( df = data )

  p1 <<- arrangeGrob ( 
    quadro.normal + theme_fivethirtyeight() + scale_y_continuous ( percent_format() )
    , tabela.teste.normalidade 
    , ncol = 2
    , widths = c(.5,.5 )
  )
  p2 <<- arrangeGrob (
    rstudent.grafico
    , dffits.grafico 
    , cooks.distance.grafico
    , ncol=3
  )
  grid.draw ( 
    grobTree ( 
      rectGrob ( gp=gpar ( fill = "#F0F0F0"
                           , lwd  = 0 ) 
      )
      , arrangeGrob ( 
        resid.grafico 
        , p1
        , p2
      )
    ) 
  )  
}
