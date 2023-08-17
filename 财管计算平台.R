library(shiny)
library(shinydashboard)
library(readr)

source('财务管理价值观念.R')


ui=dashboardPage(
  title = '财务管理计算',
  dashboardHeader(title='财管计算平台'),
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = '常用系数',tabName = 'tab1'),
      menuItem(text = '分位数表',tabName = 'tab2')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'tab1',
        fluidRow(
          box(
            title = '利率+年限',
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
            fileInput('file1','Upload a file'),
            textAreaInput('t3','请输入R code',height = '300px',placeholder = '上传文件解析后命名为data'),
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
            title = '参数',
            width = 2,
            textInput('t4','分位点'),
            textInput('t5','自由度1'),
            textInput('t6','自由度2'),
            actionButton('a3','Submit')
          ),
          box(
            width = 10,
            box(
              title = '标准正态分布',
              width = 3,
              verbatimTextOutput('text6')
            ),
            box(
              title = 'T分布',
              width = 3,
              verbatimTextOutput('text7')
            ),
            box(
              title = '卡方分布',
              width = 3,
              verbatimTextOutput('text8')
            ),
            box(
              title = 'F分布',
              width = 3,
              verbatimTextOutput('text9')
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            fileInput('file2','Upload a file'),
            textAreaInput('t7','请输入R code',height = '300px',placeholder = '上传文件解析后命名为data1'),
            actionButton('a4','运行')
          ),
          box(
            width = 6,
            verbatimTextOutput('text10')
          )
        )
      )
    )
  )
)

server=function(input,output){
  
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
  
}


shinyApp(ui,server)








