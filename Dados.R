#colocar as manipulacoes das tabelas 

datasetServer <- function(id, data, data2) {
  moduleServer(id, function(input, output, session) 
  observe(data(),{

    # se nao tiver colocado as planilhas retorna nulo
    
    reactive(data()[[input$faxina]])
    if(is.null(data))
        return(NULL)

    reactive(data2()[[input$disponibilidade]])
    if(is.null(data2))
        return(NULL)

    # leitura do banco de dados de faxinas
    faxinas = read_csv2(data$datapath,locale = locale(encoding = "latin1"))
    #
    # o intervalo de datas posso deixar
    faxinas$Data<-as.POSIXct(faxinas$Data, "UTC", format="%d/%m/%Y")
    faxinas<-faxinas%>%filter(Data>=input$selecionarperiodo & Data<input$selecionarperiodo2)

    # faxinas$`Dia da Semana`<- weekdays(faxinas$Data)

    #planilha de faxina 2018-2019
    # faxinas  <- read_xlsx(data$datapath, sheet=1, col_names = TRUE, skip = 1)
    #precisa fazer essa conversao
    # faxinas <- faxinas %>% mutate(Data = as.numeric(Data)) %>%
    #     mutate(Data = excel_numeric_to_date(Data))

    #planilha de faxina 2019-2020
    #nao precisa fazer conversao de data
    # faxinas  <- read_xlsx(data$datapath,
    #                       sheet=2, col_names = TRUE, skip = 1)

    faxinas$`Dia da Semana`<- weekdays(faxinas$Data)
    semana <- c("segunda", "terça", "quarta", "quinta", "sexta", "sábado", "domingo")
    faxinas$`Dia da Semana` <- factor(faxinas$`Dia da Semana`, order = TRUE, levels = semana)

    # leitura do banco de dados de disponibilidade
    disponibilidade <- read_csv2(data2$datapath,locale = locale(encoding = "latin1"))
    disponibilidade$Data<-as.POSIXct(disponibilidade$Data, "UTC", format="%d/%m/%Y")
    disponibilidade<-disponibilidade%>%filter(Data>=input$selecionarperiodo & Data<input$selecionarperiodo2)
    }))
  }