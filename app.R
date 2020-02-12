library(shiny)
library(jsonlite)
library(ggplot2)
library(reshape2)
library(ggthemes)
library(DT)
library(jiebaR)
library(wordcloud2)

ui <- fluidPage(

    titlePanel("肺炎疫情shiny"),
    
    sidebarLayout(
      sidebarPanel(
         helpText('累计情况')
      ),
      mainPanel(
          dataTableOutput('add')
      )
    ),
    sidebarLayout(
        sidebarPanel(
            helpText('趋势图'),
            checkboxGroupInput('var',
                               label = '变量',
                               choices = c("confirm","suspect","dead","heal"),
                               selected = c("confirm","suspect","dead","heal"))
        ),
        mainPanel(
            plotOutput(outputId = 'addplot')
        )
    ),
    sidebarLayout(
        sidebarPanel(
            helpText('关键词'),
            numericInput('n',label = '数量显示',value = 20,min = 5,max = 35,step = 5)
        ),
        mainPanel(
            plotOutput(outputId = 'count')
        )
    ),
    sidebarLayout(
        sidebarPanel(
            helpText('词云'),
            sliderInput('midu',label = '密度',min = 1,max = 20,value = 3,step = 1,animate = TRUE)
        ),
        mainPanel(
            wordcloud2Output(outputId = 'word')
        )
    )
    
)


server <- function(input, output,session) {
  
    output$add <- renderDataTable({
      url <- 'https://view.inews.qq.com/g2/getOnsInfo?name=disease_h5'
      pre <- fromJSON(url)
      count <- fromJSON(pre$data)
      add <- count$chinaDayList
      add <- add[-c(5,6)]
      add <- datatable(add)
    })
    output$addplot <- renderPlot({
      url <- 'https://view.inews.qq.com/g2/getOnsInfo?name=disease_h5'
      pre <- fromJSON(url)
      count <- fromJSON(pre$data)
      add <- count$chinaDayAddList
      #date转化为日期格式，其他转化为数字
      add$date <- as.Date(add$date,'%m.%d')
      add$confirm <- as.numeric(add$confirm)
      add$suspect <- as.numeric(add$suspect)
      add$dead <- as.numeric(add$dead)
      add$heal <- as.numeric(add$heal)
        data <- melt(add,id='date',measure = input$var)
        ggplot(data) +
            aes(x = date, y = value, group=variable,colour = variable) +
            geom_line(size = 1L) +
            scale_color_hue() +
            labs(x = " ", y = " ", title = "肺炎疫情趋势图", color = " ") +
            theme_hc()
    })
    
    datacount <- reactive({
        url <- 'https://view.inews.qq.com/g2/getOnsInfo?name=wuwei_ww_time_line'
        pre <- fromJSON(url)
        data <- fromJSON(pre$data)
        #删除数字英文字母
        data$desc <- gsub('[<U+0-9A-Z>]','',data$desc)
        #导入停词库
        wk <- worker(stop_word = 'data/stop.txt')
        #切词
        seg <- segment(data$desc,wk)
        #词频
        count <- freq(seg) 
    })
    count <- isolate(datacount())
    #关键词
    output$count <- renderPlot({
        kw <- count[order(-count$freq),][1:input$n,]
        ggplot(kw) +
            aes(x = reorder(char,freq), weight = freq) +
            geom_bar(fill = "#0c4c8a") +
            labs(x = "keywords", y = "count", title = "肺炎疫情关键词", caption = " ") +
            coord_flip() +
            theme_minimal()
    })
    #词云
    output$word <- renderWordcloud2({
        wordcloud2(count,minSize = input$midu)
    })
}

shinyApp(ui = ui, server = server)
