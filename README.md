# BCB.Data

Algumas funções para explorar a API do Banco Central com dados de conglomerados e instituições financeiras, bem como os dados do Painel de Operações de Crédito (SCR).

1.  IF.Data

    -   get_if_data.R

        -   get_instituicoes_financeiras(): retorna o cadastro das instituições financeiras, sua atividade, início de atividades e outras classificações do BCB.

        -   get_relatorios(): baixa os relatórios do [IF.Data](https://www3.bcb.gov.br/ifdata).

2.  [SCR.data](https://dadosabertos.bcb.gov.br/dataset/scr_data)

    -   get_scr_data.R

        -   get_scr_data: retorna os saldos de operações de crédito (saldo total, saldo inadimplente, ativo problemático etc.) com quebras para estado, tipo de instituição bancária (Tcb), indexador, pessoa, modalidade, atividade (CNAE).

Uma documentação melhor está sendo construída :).
