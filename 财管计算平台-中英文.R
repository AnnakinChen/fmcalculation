library(shiny)
library(shinydashboard)
library(readr)
library(shinyjs)

source('财务管理价值观念.R')

ui = dashboardPage(
  dashboardHeader(title = 'FM Calculation'),
  dashboardSidebar(
    sidebarMenu(
      selectInput('lan','Language',choices = c('中文','English')),
      menuItem(text = '常用系数 (FA coef)',tabName = 'tab1'),
      menuItem(text = '分位数表 (Quantile)',tabName = 'tab2'),
      menuItem(text = '二叉树 (Binomial Model)',tabName = 'tab3')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'tab1',
        fluidRow(
          box(
            # title = '利率+年限',
            div(id = 'box1',strong('利率+年限'),style = "margin-bottom:10px;font-size:17px;"),
            width = 2,
            textInput('t1','利率'),
            textInput('t2','年限'),
            actionButton('a1','提交')
          ),
          box(
            width = 10,
            box(
              title = 'FVIF',
              width = 3,
              verbatimTextOutput('text1')
            ),
            box(
              title = 'PVIF',
              width = 3,
              verbatimTextOutput('text2')
            ),
            box(
              title = 'FVIFA',
              width = 3,
              verbatimTextOutput('text3')
            ),
            box(
              title = 'PVIFA',
              width = 3,
              verbatimTextOutput('text4')
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            fileInput('file1','请上传文件'),
            textAreaInput('t3','请输入R code',height = '300px',placeholder = '上传文件命名为data'),
            actionButton('a2','运行')
          ),
          box(
            width = 6,
            verbatimTextOutput('text5')
          )
        )
      ),
      tabItem(
        tabName = 'tab2',
        fluidRow(
          box(
            # title = '参数',
            div(id = 'box2',strong('参数'),style = "margin-bottom:10px;font-size:17px;"),
            width = 2,
            textInput('t4','概率'),
            textInput('t5','自由度1'),
            textInput('t6','自由度2'),
            actionButton('a3','提交')
          ),
          column(
            width = 10,
            box(
              # title = '标准正态分布',
              div(id = 'box3',strong('标准正态分布'),style = "margin-bottom:10px;font-size:17px;"),
              width = 3,
              verbatimTextOutput('text6')
            ),
            box(
              # title = 'T分布',
              div(id = 'box4',strong('T分布'),style = "margin-bottom:10px;font-size:17px;"),
              width = 3,
              verbatimTextOutput('text7')
            ),
            box(
              # title = '卡方分布',
              div(id = 'box5',strong('卡方分布'),style = "margin-bottom:10px;font-size:17px;"),
              width = 3,
              verbatimTextOutput('text8')
            ),
            box(
              # title = 'F分布',
              div(id = 'box6',strong('F分布'),style = "margin-bottom:10px;font-size:17px;"),
              width = 3,
              verbatimTextOutput('text9')
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            fileInput('file2','请上传文件'),
            textAreaInput('t7','请输入R code',height = '300px',placeholder = '上传文件命名为data'),
            actionButton('a4','运行')
          ),
          box(
            width = 6,
            verbatimTextOutput('text10')
          )
        )
      ),
      tabItem(
        tabName = 'tab3',
        fluidRow(
          box(
            width = 5,
            fluidRow(
              column(
                width = 6,
                selectInput('c1','期权类型',choices = c('call','put')),
                textInput('t8',label = '初始价',value = 40),
                textInput('t9',label = '行权价',value = 40),
                textInput('t10',label = '折现率',value = 0.04)
              ),
              column(
                width = 6,
                textInput('t11',label = '波动率',value = 0.3),
                textInput('t12',label = '周期数',value = 2),
                textInput('t13',label = '到期时间',value = 0.5),
              )
            ),
            fluidRow(
              column(
                width = 12,
                actionButton('a5','运行'),
                align = 'center'
              )
            )
            
            # selectInput('c1','期权类型',choices = c('call','put')),
            # textInput('t8',label = '初始价',value = 40),
            # textInput('t9',label = '行权价',value = 40),
            
            
          ),
          box(
            width = 7,
            verbatimTextOutput('text11')
          )
        )
      )
    )
  ),
  tags$head(
    tags$script(
      HTML('
        $(document).on("shiny:inputchanged", function(event) {
          if (event.name === "lan") {
            var language = event.value;
            if (language === "中文") {
              $("#t1-label").text("利率");
              $("#t2-label").text("年限");
              $("#a1").text("提交");
              $("#box1").html("<b>利率+年限</b>");
              $("#file1-label").text("请上传文件");
              $("#t3-label").text("请输入R code");
              $("#t3").text("上传文件命名为data");
              $("#a2").text("运行");
              $("#box2").html("<b>参数</b>");
              $("#box3").html("<b>标准正态分布</b>");
              $("#box4").html("<b>T分布</b>");
              $("#box5").html("<b>卡方分布</b>");
              $("#box6").html("<b>F分布</b>");
              $("#t4-label").text("概率");
              $("#t5-label").text("自由度1");
              $("#t6-label").text("自由度2");
              $("#a3").text("提交");
              $("#file2-label").text("请上传文件");
              $("#t7").text("上传文件命名为data");
              $("#a4").text("运行");
              $("#c1-label").text("期权类型");
              $("#t8-label").text("初始价");
              $("#t9-label").text("行权价");
              $("#t10-label").text("折现率");
              $("#t11-label").text("波动率");
              $("#t12-label").text("周期数");
              $("#t13-label").text("到期时间");
              $("#a5").text("运行");
            } else {
              $("#t1-label").text("Interest");
              $("#t2-label").text("Years");
              $("#a1").text("Submit");
              $("#box1").html("<b>Interest+Years</b>");
              $("#file1-label").text("Upload a file");
              $("#t3-label").text("Please input R code");
              $("#t3").text("The Uploaded file would be named as data");
              $("#a2").text("Run");
              $("#box2").html("<b>Parameters</b>");
              $("#box3").html("<b>Gaussian Dist</b>");
              $("#box4").html("<b>T Dist</b>");
              $("#box5").html("<b>Chi-squared Dist</b>");
              $("#box6").html("<b>F Dist</b>");
              $("#t4-label").text("Prob");
              $("#t5-label").text("Df1");
              $("#t6-label").text("Df2");
              $("#a3").text("Submit");
              $("#file2-label").text("Upload a file");
              $("#t7").text("The Uploaded file would be named as data");
              $("#a4").text("Run");
              $("#c1-label").text("option type");
              $("#t8-label").text("S0");
              $("#t9-label").text("K");
              $("#t10-label").text("r");
              $("#t11-label").text("sigma");
              $("#t12-label").text("N");
              $("#t13-label").text("T");
              $("#a5").text("Run");
            }
          }
        });
      ')
    )
  )
)
server=function(input,output,session){
  
  observeEvent(
    input$a1,
    {
      req(input$t1,input$t2)
      i=as.numeric(input$t1)
      n=as.numeric(input$t2)
      output$text1=renderPrint({
        cat('FVIF = ',FVIF(i,n))
      })
      output$text2=renderPrint({
        cat('PVIF = ',PVIF(i,n))
      })
      output$text3=renderPrint({
        cat('FVIFA = ',FVIFA(i,n))
      })
      output$text4=renderPrint({
        cat('PVIFA = ',PVIFA(i,n))
      })
    }
  )
  file1=reactive({
    req(input$file1)
    read_csv(input$file1$datapath)
  })
  observeEvent(
    input$a2,
    {
      if(input$t1 == '' & input$t2 == ''){
        i = 0.1
        n = 3
      }
      else{
        i=as.numeric(input$t1)
        n=as.numeric(input$t2)
      }
      FVIF = FVIF(i,n)
      PVIF = PVIF(i,n)
      FVIFA = FVIFA(i,n)
      PVIFA = PVIFA(i,n)
      if(is.null(input$file1)){
        data=iris
      }
      else{
        data=as.data.frame(file1())
      }
      code=input$t3
      result=try(eval(parse(text = code)),silent=T)
      output$text5=renderPrint({
        if(inherits(result,'try-error')){
          paste('错误：',result)
        }
        else{
          result
        }
      })
    }
  )
  file2=reactive({
    req(input$file2)
    read_csv(input$file2$datapath)
  })
  
  observeEvent(
    input$a3,
    {
      req(input$t4)
      p = as.numeric(input$t4)
      output$text6 = renderPrint({
        qnorm(p)
      })
      if(input$t5 != ''){
        df1 = as.numeric(input$t5)
        output$text7 = renderPrint({
          qt(p,df = df1)
        })
        output$text8 = renderPrint({
          qchisq(p,df = df1)
        })
      }
      if(input$t5 != '' & input$t6 != ''){
        df1 = as.numeric(input$t5)
        df2 = as.numeric(input$t6)
        output$text7 = renderPrint({
          qt(p,df = df1)
        })
        output$text8 = renderPrint({
          qchisq(p,df = df1)
        })
        output$text9 = renderPrint({
          qf(p,df1 = df1,df2 = df2)
        })
      }
    }
  )
  observeEvent(
    input$a4,
    {
      code=input$t7
      if(is.null(input$file2)){
        data1=iris
      }
      else{
        data1=as.data.frame(file2())
      }
      if(input$t4 == '' & input$t5 == '' & input$t6 == ''){
        p = 0.95
        df1 = 1
        df2 = 2
      }
      else{
        p = as.numeric(input$t4)
        df1 = as.numeric(input$t5)
        df2 = as.numeric(input$t6)
      }
      qnorm = qnorm(p)
      qt = qt(p,df1)
      qchisq = qchisq(p,df1)
      qf = qf(p,df1,df2)
      result = try(eval(parse(text = code)),silent = T)
      output$text10 = renderPrint({
        if(inherits(result,'try-error')){
          paste('错误：',result)
        }
        else{
          result
        }
      })
    }
  )
  observeEvent(
    input$a5,
    {
      type = input$c1
      s0 = as.numeric(input$t8)
      k = as.numeric(input$t9)
      r = as.numeric(input$t10)
      sigma = as.numeric(input$t11)
      N = as.numeric(input$t12)
      maturity = as.numeric(input$t13)
      if(input$lan=="中文"){
        output$text11 = renderPrint({
          cat('初始时刻期权定价为：',option0(s0,k,r,sigma,N,maturity,type))
        })
      }
      else{
        output$text11 = renderPrint({
          cat('The date-0 option price is:',option0(s0,k,r,sigma,N,maturity,type))
        })
      }
    }
  )
  
  
}

shinyApp(ui,server)








