require(shiny)
require(ggplot2)
require(DT)
require(data.table)
require(stringr)
require(dplyr)

dat <- readRDS("dat.rds")
dat$tender_tenderers_identifier_legalName <- readLines("tender_tenderers_identifier_legalName.txt", encoding="UTF-8")
dat$awards_suppliers_identifier_legalName <- readLines("awards_suppliers_identifier_legalName.txt", encoding="UTF-8")
dat$awards_unsuccessfulTenderer_legalName <- readLines("awards_unsuccessfulTenderer_legalName.txt", encoding="UTF-8")
dat <- dat %>% mutate_if(is.factor, as.character)

compRank          <- readRDS("compRank.rds")
compRank$廠商名稱 <- readLines("getAwards_name.txt", encoding="UTF-8")
compRank          <- compRank %>% mutate_if(is.factor, as.character)

failReason          <- readRDS("failReason.rds")
failReason$廠商名稱 <- readLines("failReason.txt", encoding="UTF-8")

datlenCheck <- function(bclass, sclass, location, amountRange){
  if(sclass != "不選擇"){
    datlen <- length(dat$url[which(grepl(location, dat$SimpleLocation) & dat$tender_classification_description==sclass & dat$tender_procurementAmountRange==amountRange)] %>% unique)
    }else{
    datlen <- length(dat$url[which(grepl(location, dat$SimpleLocation) & dat$tcd.simp==bclass & dat$tender_procurementAmountRange==amountRange)] %>% unique)
    }
  return(datlen)
}

getsClass <- function(bclass, sclass1, sclass2, sclass3){
  if(bclass == "工程類"){
    smallClassification <- sclass1
  }else if(bclass == "勞務類"){
    smallClassification <- sclass2
  }else if(bclass == "財物類"){
    smallClassification <- sclass3
  }else{
    smallClassification <- "error"
  }
  return(smallClassification)
}


shinyServer(function(input, output, session) {

  wrtfun2 <- reactive({
    if (!is.null(input$var1))
      setwd("/opt/shiny-server/samples/sample-apps/test")
    sink("outfile.txt")
    cat(input$var1)
    sink()
  })
  
  ## body of index
  output$body <- renderText({
    paste0("<h2 style='font-family:微軟正黑體;'>此平台包含多項樞紐分析...</h2>",
           "<span style='font-family:微軟正黑體;'>1. 決標金額分布<br />2. 決標金額分布圖<br />3. 採購金額級距<br />4. 預算高於決標倍數<br />5. 投標廠商數分布狀況<br />6. 完工時間分布<br />7. 實際標案連結</span>")
  })
  
  ## Pivot
  if(TRUE){
    
    output$title <- renderText({
      smallClassification <- getsClass(input$classification, input$smallclassification_1, input$smallclassification_2, input$smallclassification_3)
      if(smallClassification == "不選擇")
        smallClassification <- input$classification
      
      paste0("<h3>", input$location, "  ", smallClassification, "：『", input$amountRange, "』</h3>")
    })
    
    ##
    output$datLen <- renderText({
      smallClassification <- getsClass(input$classification, input$smallclassification_1, input$smallclassification_2, input$smallclassification_3)
      
      if(smallClassification != "不選擇"){
        element <- paste0("<span class='num'>", 
                          length(dat$url[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)] %>% unique) %>% round(., 0) %>% format(., nsmall = 0, big.mark=","), 
                          "</span>筆資料")
      }else{
        element <- paste0("<span class='num'>", 
                          length(dat$url[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)] %>% unique) %>% round(., 0) %>% format(., nsmall = 0, big.mark=","), 
                          "</span> 筆資料")
      }
      paste0('<div class=block>', 
             "<span><font size='5'>資料筆數</font></span><HR><br>　",
             element, 
             '</div>')
    })
    
    output$sdRatio <- renderText({
      smallClassification <- getsClass(input$classification, input$smallclassification_1, input$smallclassification_2, input$smallclassification_3)
      
      if(smallClassification != "不選擇"){
        s_element <- sum(dat$tender_numberOfBids[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)], na.rm = T)
        d_element <- length(dat$url[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)] %>% unique)
      }else{
        s_element <- sum(dat$tender_numberOfBids[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)], na.rm = T)
        d_element <- length(dat$url[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)] %>% unique)
        
        
        element <- paste0("<span class='num'>", 
                          length(dat$url[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)] %>% unique) %>% round(., 0) %>% format(., nsmall = 0, big.mark=","), 
                          "</span> 筆資料")
      }
      paste0('<div class=block>', 
             "<span><font size='5'>供給需求比率</font></span><HR><br>",
             "　<span class='num'>", 
             (s_element / d_element * 100) %>% round(., 2), 
             "</span> %（供需比）",
             " = ",
             s_element %>% round(., 0) %>% format(., nsmall = 0, big.mark=","), 
             "（供給數）",
             " / ", 
             d_element %>% round(., 0) %>% format(., nsmall = 0, big.mark=","), 
             "（需求數）",
             '</div>')
    })
    
    ## 決標金額
    output$summary <- renderText({
      smallClassification <- getsClass(input$classification, input$smallclassification_1, input$smallclassification_2, input$smallclassification_3)
      
      if(smallClassification != "不選擇"){
        #summary(dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)] %>% as.numeric)
        element_mean   <- mean(dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)] %>% as.numeric, na.rm = T) %>% round(., 0)
        element_median <- median(dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)] %>% as.numeric, na.rm = T) %>% round(., 0)
      }else{
        #summary(dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)] %>% as.numeric)
        element_mean   <- mean(dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)] %>% as.numeric, na.rm = T) %>% round(., 0)
        element_median <- median(dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)] %>% as.numeric, na.rm = T) %>% round(., 0)
        }
      shiny::validate(
        need(datlenCheck(input$classification, smallClassification, input$location, input$amountRange) != 0, "查無資料")
      )
      element_mean   <- element_mean %>% round(., 0) %>% format(., nsmall = 0, big.mark=",")
      element_median <- element_median %>% round(., 0) %>% format(., nsmall = 0, big.mark=",")
      
      paste0('<div class=block>', 
             "<span><font size='5'>決標金額</font></span><HR><br>",
             "　<span class='num'>", 
             element_mean, 
             "</span> （平均數）", 
             "<br>　<span class='num'>", 
             element_median,
             "</span> （中位數）", 
             '</div>')
      
    })
    
    output$distPlotheader <- renderText({
      paste0('<div class=plotheader>', 
             "<span><font size='5'>決標金額分布</font></span><HR><br>",
             '</div>')
    })
    
    output$distPlot <- renderPlot({
      smallClassification <- getsClass(input$classification, input$smallclassification_1, input$smallclassification_2, input$smallclassification_3)
      
      if(smallClassification != "不選擇"){
        datlen <- length(dat$url[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)] %>% unique)
        value <- dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)]
        element <- hist(value[!value %in% boxplot.stats(value)$out], breaks = 100, 
                        main = paste0(input$location, smallClassification, "\nValue Amount"),
                        xlab = "Value amount", col = "lightblue")
      }else{
        datlen <- length(dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)])
        value <- dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)]
        element <- hist(value[!value %in% boxplot.stats(value)$out], breaks = 100,
                        main = "",#paste0(input$location, "\nValue Amount"),
                        xlab = "決標金額", col = "lightblue")
      }
      shiny::validate(
        need(datlenCheck(input$classification, smallClassification, input$location, input$amountRange) != 0, "查無資料")
      )
      element
    })
    
    output$valuePercentage <- renderText({
      smallClassification <- getsClass(input$classification, input$smallclassification_1, input$smallclassification_2, input$smallclassification_3)
      
      if(smallClassification != "不選擇"){
        datlen  <- length(dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)])
        value   <- dat$ValuePercntage[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)]
        element <- mean(value[!value %in% boxplot.stats(value)$out], na.rm = T)  %>% round(., 3)
      }else{
        datlen  <- length(dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)])
        value   <- dat$ValuePercntage[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)]
        element <- mean(value[!value %in% boxplot.stats(value)$out], na.rm = T)  %>% round(., 3)
      }
      shiny::validate(
        need(datlenCheck(input$classification, smallClassification, input$location, input$amountRange) != 0, "查無資料")
      )
      paste0('<div class=block>',
             "<span><font size='5'>平均預算適切度</font></span><HR><br>",
             "　<span class='num'>", 
             (1/element * 100) %>% round(., 2), 
             "</span> %（決標金額 / 預算金額）", 
             '</div>')
    })
    
    output$numberOfBidsPlotheader <- renderText({
      paste0('<div class=plotheader>', 
             "<span><font size='5'>投標廠商數分布</font></span><HR><br>",
             '</div>')
    })
    output$numberOfBidsPlot <- renderPlot({
      smallClassification <- getsClass(input$classification, input$smallclassification_1, input$smallclassification_2, input$smallclassification_3)
      
      if(smallClassification != "不選擇"){
        value <- dat$tender_numberOfBids[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)]
        value <- table(value) %>% as.data.frame(., stringsAsFactors=F)
        value <- value[order(-value$Freq), ]
        
        names(value)[1] <- "投標廠商數"
        ##ggplot2 bar chart
        value <- eval(parse(text=
                              paste0("within(value, ", names(value)[1], 
                                     " <- factor(", names(value)[1], 
                                     ", levels=rev(value[,1])))")
        ))
        element <- eval(parse(text=paste0(
          "ggplot(data=value, aes(x=", names(value)[1], ", y=Freq)) +
          geom_bar(stat='identity', fill = '#FF6666') + xlab('", names(value)[1], "') + theme_classic() + 
          theme(axis.text = element_text(size=14)) +
          geom_text(aes(x=", names(value)[1], ", y=Freq, label=Freq, 
          hjust=1), 
          position = position_dodge(width=1)) + 
          coord_flip()" # + ggtitle('Distribution of Tenderers')+ theme(plot.title = element_text(size=18))
        )))
      }else{
        value <- dat$tender_numberOfBids[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)]
        value <- table(value) %>% as.data.frame(., stringsAsFactors=F)
        value <- value[order(-value$Freq), ]
        
        names(value)[1] <- "投標廠商數"
        ##ggplot2 bar chart
        value <- eval(parse(text=
                              paste0("within(value, ", names(value)[1], 
                                     " <- factor(", names(value)[1], 
                                     ", levels=rev(value[,1])))")
        ))
        element <- eval(parse(text=paste0(
          "ggplot(data=value, aes(x=", names(value)[1], ", y=Freq)) +
          geom_bar(stat='identity', fill = '#FF6666') + xlab('", names(value)[1], "') + theme_classic() + 
          theme(axis.text = element_text(size=14)) +
          geom_text(aes(x=", names(value)[1], ", y=Freq, label=Freq, 
          hjust=1), 
          position = position_dodge(width=1)) + 
          coord_flip()" # + ggtitle('Distribution of Tenderers')+ theme(plot.title = element_text(size=18))
        )))
      }
      shiny::validate(
        need(datlenCheck(input$classification, smallClassification, input$location, input$amountRange) != 0, "查無資料")
      )
      element
      })
    
    output$contractPeriodSummary <- renderText({
      smallClassification <- getsClass(input$classification, input$smallclassification_1, input$smallclassification_2, input$smallclassification_3)
      
      if(smallClassification != "不選擇"){
        value <- as.Date(dat$awards_suppliers_contractPeriod_endDate[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)] %>% substr(., 1, 10)) - 
          as.Date(dat$awards_suppliers_contractPeriod_startDate[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)] %>% substr(., 1, 10))
      }else{
        value <- as.Date(dat$awards_suppliers_contractPeriod_endDate[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)] %>% substr(., 1, 10)) - 
          as.Date(dat$awards_suppliers_contractPeriod_startDate[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)] %>% substr(., 1, 10))
      }
      shiny::validate(
        need(datlenCheck(input$classification, smallClassification, input$location, input$amountRange) != 0, "查無資料")
      )
      paste0('<div class=block>', 
             "<span><font size='5'>完成合約天數</font></span><HR><br>",
             "　<span class='num'>", 
             mean(value %>% as.numeric, na.rm = T) %>% round(., 0) %>% format(., nsmall = 0, big.mark=","), 
             "</span> 天（平均數）", 
             "<br>　<span class='num'>", 
             median(value %>% as.numeric, na.rm = T) %>% round(., 0) %>% format(., nsmall = 0, big.mark=","),
             "</span> 天（中位數）", 
             '</div>')
    })
    
    output$viewheader <- renderText({
      paste0('<div class=plotheader>', 
             "<span><font size='5'>標案列表</font></span><HR><br>",
             '</div>')
    })
    
    output$view <- renderDataTable({
      smallClassification <- getsClass(input$classification, input$smallclassification_1, input$smallclassification_2, input$smallclassification_3)
      
      if(smallClassification != "不選擇"){
        cases <- dat$url[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)]
        
        element <- datatable(cbind(paste0('<a href="', cases, '" target="_blank">', 
                                          dat$name[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)], '</a>'), 
                                   dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tender_classification_description==smallClassification & dat$tender_procurementAmountRange==input$amountRange)] %>% as.numeric()), escape = FALSE, 
                             colnames = c("標案名稱", "決標金額"), options = list(bSort=FALSE)
        )
      }else{
        cases <- dat$url[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)]
        
        element <- datatable(cbind(paste0('<a href="', cases, '" target="_blank">', 
                                          dat$name[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)], '</a>'), 
                                   dat$awardsValue[which(grepl(input$location, dat$SimpleLocation) & dat$tcd.simp==input$classification & dat$tender_procurementAmountRange==input$amountRange)] %>% as.numeric() %>% round(., 0) %>% format(., nsmall = 0, big.mark=",")), escape = FALSE, 
                             colnames = c("標案名稱", "決標金額"), options = list(bSort=FALSE)
        )
      }
      shiny::validate(
        need(datlenCheck(input$classification, smallClassification, input$location, input$amountRange) != 0, "查無資料")
      )
      element
    })
    
    
  }
  
  ## Company information
  if(TRUE){
    output$cmpTitle <- renderText({
      paste0("<h3>投標相關公司</h3>
             <span>　　點擊表格中公司名稱，下面即顯示該公司投標相關資訊。</span>
             <br><br><br>
             ")
    })
    
    output$rankheader <- renderText({
      paste0('<div class=plotheader>', 
             "<span><font size='5'>標案相關公司得標率</font></span><HR><br>",
             '</div>')
    })
    # selection = 'single' 
    # can only select one row
    output$ranking <- renderDataTable(compRank, selection = 'single')
    
    # if any one of the row is selected, it's information will be printed
    output$cmpName <- renderText({
      s <- input$ranking_rows_selected
      if (length(s)) paste0("<div class=block><h3>", compRank$廠商名稱[input$ranking_rows_selected], "  ", "</h3></div>")
    })
    
    output$classBarPlotheader <- renderText({
      s <- input$ranking_rows_selected
      if (length(s)) paste0('<div class=plotheader>', 
                            "<span><font size='5'>標案類別分布</font></span><HR><br>",
                            '</div>')
    })
    
    output$classBarPlot <- renderPlot({
      s <- input$ranking_rows_selected
      if(length(s)){
        value <- dat$tcd.simp[which(grepl(paste0(gsub("\\", "", compRank$廠商名稱[input$ranking_rows_selected], fixed=T), "@"), dat$awards_suppliers_identifier_legalName %>% paste0("@")))]
        value <- value %>% str_split(., "@") %>% unlist
        value <- table(value) %>% as.data.frame(., stringsAsFactors=F)
        value <- value[order(-value$Freq), ]

        names(value)[1] <- "標案類別"
        ##ggplot bar chart
        value <- eval(parse(text=
                              paste0("within(value, ", names(value)[1], 
                                     " <- factor(", names(value)[1], 
                                     ", levels=rev(value[,1])))")
                            ))
        
        eval(parse(text=paste0(
          "ggplot(data=value, aes(x=", names(value)[1], ", y=Freq)) +
        geom_bar(stat='identity', fill = '#FF6666') + xlab('", names(value)[1], "') + theme_classic() + 
        theme(axis.text = element_text(size=14)) +
        geom_text(aes(x=", names(value)[1], ", y=Freq, label=Freq, 
        hjust=1), 
        position = position_dodge(width=1)) + 
        coord_flip()" # + ggtitle('Distribution of Tenderers')+ theme(plot.title = element_text(size=18))
          )))
      }
    })
    
    output$amountRangeBarPlotheader <- renderText({
      s <- input$ranking_rows_selected
      if (length(s)) paste0('<div class=plotheader>', 
                            "<span><font size='5'>標案金額級距分布</font></span><HR><br>",
                            '</div>')
    })
    
    output$amountRangeBarPlot <- renderPlot({
      s <- input$ranking_rows_selected
      if(length(s)){
        value <- dat$tender_procurementAmountRange[which(grepl(paste0(gsub("\\", "", compRank$廠商名稱[input$ranking_rows_selected], fixed=T), "@"), dat$awards_suppliers_identifier_legalName %>% paste0("@")))]
        value <- table(value) %>% as.data.frame(., stringsAsFactors=F)
        value <- value[order(-value$Freq), ]
        
        names(value)[1] <- "標案類別"
        ##ggplot bar chart
        value <- eval(parse(text=
                              paste0("within(value, ", names(value)[1], 
                                     " <- factor(", names(value)[1], 
                                     ", levels=rev(value[,1])))")
                            ))
        
        eval(parse(text=paste0(
          "ggplot(data=value, aes(x=", names(value)[1], ", y=Freq)) +
        geom_bar(stat='identity', fill = '#FF6666') + xlab('", names(value)[1], "') + theme_classic() + 
        theme(axis.text = element_text(size=14)) +
        geom_text(aes(x=", names(value)[1], ", y=Freq, label=Freq, 
        hjust=1), 
        position = position_dodge(width=1)) + 
        coord_flip()" # + ggtitle('Distribution of Tenderers')+ theme(plot.title = element_text(size=18))
          )))
      }
    })
    
    output$avgPPheader <- renderText({
      s <- input$ranking_rows_selected
      if (length(s)) paste0('<div class=plotheader>', 
                            "<span><font size='5'>平均決標金額與時間</font></span><HR><br>",
                            '</div>')
    })
    
    output$avgPP <- renderDataTable({
      s <- input$ranking_rows_selected
      if(length(s)){
        setDT(dat)
        price  <- dat[which(grepl(paste0(gsub("\\", "", compRank$廠商名稱[input$ranking_rows_selected], fixed=T), "@"), dat$awards_suppliers_identifier_legalName %>% paste0("@"))),
                      mean(awardsValue, na.rm=T), by = .(tender_procurementAmountRange)]
        period <- dat[which(grepl(paste0(gsub("\\", "", compRank$廠商名稱[input$ranking_rows_selected], fixed=T), "@"), dat$awards_suppliers_identifier_legalName %>% paste0("@"))),
                      mean((as.Date(awards_suppliers_contractPeriod_endDate %>% substr(., 1, 10)) - 
                              as.Date(awards_suppliers_contractPeriod_startDate %>% substr(., 1, 10))
                      ), na.rm=T), by = .(tender_procurementAmountRange)]
        setDF(dat)
        names(price)  <- c("x", "y")
        names(period) <- c("x", "y")
        tmp <- merge(x = price, y = period, by = "x", all.x = TRUE)
        names(tmp) <- c("金額級距", "平均決標金額（案件總金額）", "平均完成時間（天）")
        tmp$'平均決標金額（案件總金額）'       <- tmp$'平均決標金額（案件總金額）' %>% round(., 0) %>% format(., nsmall = 0, big.mark=",")
        tmp$'平均完成時間（天）' <- tmp$'平均完成時間（天）' %>% round(., 0) %>% format(., nsmall = 0, big.mark=",")
        datatable(tmp, options = list(bSort=FALSE))
      }
    })
    
    output$failReasonheader <- renderText({
      s <- input$ranking_rows_selected
      if (length(s)) paste0('<div class=plotheader>', 
                            "<span><font size='5'>未得標原因</font></span><HR><br>",
                            '</div>')
    })
    
    output$failReason <-  renderDataTable({
      s <- input$ranking_rows_selected
      if(length(s)){
        tmp <- failReason[grepl(gsub("\\", "", compRank$廠商名稱[input$ranking_rows_selected], fixed=T), failReason$廠商名稱),]
        tmp <- tmp[,c("失敗原因", "筆數")]
        rownames(tmp) <- NULL
        datatable(tmp, options = list(bSort=FALSE))
      }
    })     
    
    output$CompViewheader <- renderText({
      s <- input$ranking_rows_selected
      if (length(s)) paste0('<div class=plotheader>', 
                            "<span><font size='5'>相關標案</font></span><HR><br>",
                            '</div>')
    })
    
    output$CompView <- renderDataTable({
      s <- input$ranking_rows_selected
      if(length(s)){
        cases <- dat$url[which(grepl(paste0(gsub("\\", "", compRank$廠商名稱[input$ranking_rows_selected], fixed=T), "@"), dat$awards_suppliers_identifier_legalName %>% paste0("@")))]
        
        datatable(cbind(paste0('<a href="', cases, '" target="_blank">', 
                               dat$name[which(grepl(paste0(gsub("\\", "", compRank$廠商名稱[input$ranking_rows_selected], fixed=T), "@"), 
                                                    dat$awards_suppliers_identifier_legalName %>% paste0("@")))], '</a>'), 
                        dat$awardsValue[which(grepl(paste0(gsub("\\", "", compRank$廠商名稱[input$ranking_rows_selected], fixed=T), "@"), 
                                                    dat$awards_suppliers_identifier_legalName %>% paste0("@")))] %>% as.numeric() %>% round(., 0) %>% format(., nsmall = 0, big.mark=",")), escape = FALSE, 
                  colnames = c("標案名稱", "決標金額"), options = list(bSort=FALSE)
        )
      }
    })
  }
  
  ##hide
  #observe({
  #  #hide(selector = "li a[data-value=tab1]")
  #})
})