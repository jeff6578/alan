YHml01<- read_html("https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0AH10&stname=%25E6%25B0%25B8%25E5%2592%258C&datepicker=2018-01")
YHml01data<- html_nodes(YHml01,"td:nth-child(22) , td:nth-child(18) , td:nth-child(14) , td:nth-child(9) , td:nth-child(8) , td:nth-child(1) , th:nth-child(22) , th:nth-child(18) , th:nth-child(14) , th:nth-child(8) , .first_tr+ .second_tr th:nth-child(1)")

head(YHml01data)
YHml01datatx <- html_text(YHml01data)
YHml01datatx
