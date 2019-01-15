
# Object Definition
rm(teste_afis)
setRefClass("afis", fields=list(name="factor",
                            age="data.frame",
                            char_teste = "character"),
                            methods = list(
                                          set_char_teste = function(object) {
                                          object$char_teste <-"FIXO DE DENTRO"
                                          })
                          )



teste_afis <-  new("afis")


teste_afis$set_char_teste(teste_afis)

teste_afis
