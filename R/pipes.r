# PARSE DE PIPES -----------------------------------------------------------------------------------

#' Parse Um Unico Pipe
#' 
#' Interpreta a definicao de um unico pipe, gerando as closures definidas
#' 
#' Ambos `env` e `enclos` funcionam tal qual como em `eval`, permitindo que `env` corresponda a uma
#' lista nomeada ou data.frame, e `enclos` seja um ambiente onde funcoes auxiliares podem ser
#' definidas
#' 
#' `raw_pipe` deve ser uma lista nomeada de dois argumentos: `"on"` e `"transforms`. `"on"` e
#' simplesmente um vetor de no maximo dois elementos indicando os nomes das variaveis nas quais
#' a(s) transformacao(oes) opera(m). `"transforms"` deve ser uma lista de especificacoes das
#' transformacoes. Cada transformacao e por sua vez uma lista de ao menos um elemento nomeado,
#' `"fun"`, uma string indicando o nome da geradora de closure a ser chamada. Alem de `"fun"` outros
#' elementos podem existir indicando parametrizacao da geradora de closure. Veja `Exemplos` para 
#' uma ilustracao de pipe bruto.
#' 
#' Note que as funcoes nas definicoes de transformacao devem sempre ser geradoras de closures e
#' necessariamente precisam incluir o arumento variatico `...`. Isto e necessario porque, numa etapa
#' posterior, estas geradoras serao interpretadas para entao produzir o pipe parsed no qual
#' `transforms` sao, de fato, funcoes a serem chamadas no dado.
#' 
#' @param raw_pipe uma lista definindo pipe singular, isto e, com elemento `"on"` e `"transforms"`
#' @param env ambiente onde os pipes brutos serao avaliados
#' @param enclos ambiente de encerramento para avaliacao das closures
#' 
#' @examples 
#' 
#' # geradora de uma closure de unico argumento `x` que chama filtra este argumento com base em
#' # outros parametros especificados
#' gen_closure_filter <- function(by, value, ...) function(x) x[x[[by]] == value, ]
#' 
#' # geradora de uma closure de unico argumento `x` que chama `summary` neste argumento
#' gen_closure_summary <- function(...) function(x) summary(x)
#' 
#' raw_pipe <- list(
#'     on = "mtcars",
#'     transforms = list(
#'         list(
#'             fun = "gen_closure_filter",
#'             by = "cyl", value = 6
#'         ),
#'         list(
#'             fun = "gen_closure_summary"
#'         )
#'     )
#' )
#' 
#' parsed_pipe <- parse_single_pipe(raw_pipe)
#' 
#' @return lista `raw_pipe` com elemento `"transforms"` avaliado para as closures definidas

parse_single_pipe <- function(raw_pipe, env = parent.frame(), enclos = parent.frame()) {
    args <- lapply(raw_pipe$on, str2lang)
    names(args) <- c("x", "y")[seq_along(args)]

    l_t <- raw_pipe$transforms

    cc <- c(l_t[[1]], args)
    cc[[1]] <- str2lang(cc[[1]])
    raw_pipe$transforms[[1]] <- eval(as.call(cc), env, enclos)

    if (length(l_t) >= 2) {
        x <- do.call(raw_pipe$transforms[[1]], args, envir = env)
        for (i in seq_along(l_t)[-1]) {
            cc <- c(l_t[[i]], list(x = x))
            cc[[1]] <- str2lang(cc[[1]])
            raw_pipe$transforms[[i]] <- eval(as.call(cc), env, enclos)
            x <- do.call(raw_pipe$transforms[[i]], list(x = x))
        }
    }

    return(raw_pipe)
}

#' Parse Lista De Pipes
#' 
#' Wrapper simples para loop de `parse_single_pipe` em multiplos pipes
#' 
#' @param raw_pipes lista definindo diversos pipes
#' 
#' @return lista `raw_pipes` com elementos `"transforms"` de cada pipe avaliados

parse_pipes <- function(raw_pipes) {
    lapply(raw_pipes, parse_single_pipe)
}

# EVAL DE PIPES ------------------------------------------------------------------------------------

#' Avalia Um Unico Pipe
#' 
#' Aplica as closures em um `pipe` já parsed ao dado definido em `"on"`
#' 
#' Ambos `env` e `enclos` funcionam tal qual como em `eval`, permitindo que `env` corresponda a uma
#' lista nomeada ou data.frame, e `enclos` seja um ambiente onde funcoes auxiliares podem ser
#' definidas
#' 
#' @param pipe um pipe parsed por `parse_single_pipe`
#' @param env ambiente onde o pipe sera avaliado
#' @param enclos ambiente de encerramento para avaliacao das closures
#' 
#' @return resultado da aplicacao das closures em `transforms` ao(s) dado(s) em `"on"`

eval_single_pipe <- function(pipe, env = parent.frame(), enclos = parent.frame()) {

    args <- lapply(pipe$on, str2lang)
    names(args) <- c("x", "y")[seq_along(args)]

    l_t <- pipe$transforms

    cc <- c(list(l_t[[1]]), args)
    x <- eval(as.call(cc), env, enclos)
    l_t[[1]] <- NULL

    if (length(l_t) >= 1) {
        for (f in l_t) {
            cc <- list(f, x)
            x <- eval(as.call(cc), env, enclos)
        }
    }

    return(x)
}

#' Avalia Lista De Pipes
#' 
#' Wrapper simples para loop de `eval_single_pipe` em multiplos pipes
#' 
#' @param pipes lista de pipes ja parsed por `parse_pipes`
#' 
#' @return data.table unico combinando os resultados da aplicacao de todos os pipes em `pipes`

eval_pipes <- function(pipes) {
    evals <- lapply(pipes, eval_single_pipe)
    Reduce(function(x, y) merge(x, y, by.x = names(x)[1], by.y = names(y)[1]), evals)
}
