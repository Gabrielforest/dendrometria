# Função que criei para facilitar a correção das alturas - Lista 2

# Função para correção das medições de alturas ----------------------------

altura_arvore <- 
  function( lh_metros, l1_metros, l2_metros, l1_l2_graus = FALSE, largura_prancheta = 0.1,
            metodo = c("prancheta", "suunto", "haga", "blume_leiss"),
            declividade_graus = 0, declividade_porcentagem = 0, escala_metros = 0 ) {
    
    if( metodo == "prancheta" ) {
      
      # casos positivos l1 e l2
      if( (l1_metros > 0 & l2_metros > 0) & declividade_graus < 4 & declividade_porcentagem < 7 ) {
        res <- ( lh_metros * ( l1_metros - l2_metros ) ) / largura_prancheta
      }
      else if( l1_metros > 0 & l2_metros > 0 & declividade_graus > 4 | declividade_porcentagem > 7 ) {
        if( declividade_graus > 4 & l1_l2_graus == TRUE ) {
          res <- (( lh_metros * ( cos ( declividade_graus * pi / 180 ) ) ) * 
                    ( tan ( abs(l2_metros) * pi / 180 ) - ( tan ( abs(l1_metros) * pi / 180) ) ))
        } 
        else if( declividade_graus > 4 & l1_l2_graus == FALSE ) {
          res <- ( ( lh_metros * ( abs(l2_metros) - abs(l1_metros) ) / largura_prancheta ) *
                     cos( declividade_graus * pi / 180 ) )
        }
        else if( declividade_porcentagem > 7 ) {
          res <- ( ( lh_metros * ( abs(l2_metros) - abs(l1_metros) ) ) / largura_prancheta ) * 
            ( cos ( atan( declividade_porcentagem/100 ) ) )
        }
      }
      
      # casos negativos l1 ou l2
      else if ( (l1_metros < 0 | l2_metros < 0) & declividade_graus < 4 & declividade_porcentagem < 7 ) {
        res <- ( lh_metros * ( abs(l1_metros) + abs(l2_metros) ) ) / largura_prancheta
      }
      else if( (l1_metros > 0 | l2_metros > 0) & declividade_graus > 4 | declividade_porcentagem > 7 ) {
        if( declividade_graus > 4 & l1_l2_graus == TRUE ) {
          res <- ( lh_metros * ( cos ( declividade_graus * pi / 180 ) ) ) * 
            ( tan ( abs(l2_metros) * pi / 180 ) + 
                ( tan ( abs(l1_metros) * pi / 180) ) )
        }
        else if( declividade_graus > 4 & l1_l2_graus == FALSE ) {
          res <- ( ( lh_metros * ( abs(l1_metros) + abs(l2_metros) ) / largura_prancheta ) *
                     cos( declividade_graus * pi / 180 ) )
        }
        else if( declividade_porcentagem > 7 ) {
          res <- ( ( lh_metros * ( abs(l2_metros) + abs(l1_metros) ) ) / largura_prancheta ) * 
            ( cos ( atan( declividade_porcentagem/100 ) ) )
        }
      }
    }
    
    else if( any( metodo %in% c( "suunto", "blume_leiss", "haga" ) ) ) {
      
      # casos positivos l1 e l2
      if( (l1_metros > 0 & l2_metros > 0) & declividade_graus < 4 & declividade_porcentagem < 7 ) {
        res <- lh_metros * ( abs(l1_metros) + abs(l2_metros) )
      }
      else if( (l1_metros > 0 & l2_metros > 0) & declividade_graus > 4 | declividade_porcentagem > 7 ) {
        if( declividade_graus > 4 & l1_l2_graus == TRUE ) {
          res <- ( lh_metros * ( cos ( declividade_graus * pi / 180 ) ) ) * 
            ( tan ( abs(l2_metros) * pi / 180 ) - 
                ( tan ( abs(l1_metros) * pi / 180) ) )
        } 
        else if( declividade_graus > 4 & l1_l2_graus == FALSE ) {
          res <- ( ( lh_metros * ( abs(l2_metros) - abs(l1_metros) ) / escala_metros ) *
                     cos( declividade_graus * pi / 180 ) )
        }
        else if( declividade_porcentagem > 7 ) {
          res <- ( ( lh_metros * ( abs(l2_metros) - abs(l1_metros) ) ) / escala_metros ) * 
            ( cos ( atan( declividade_porcentagem/100 ) ) )
        }
      }
      
      # casos negativos l1 ou l2
      else if( (l1_metros < 0 | l2_metros < 0) & declividade_graus < 4 & declividade_porcentagem < 7 ) {
        res <- lh_metros * ( abs(l2_metros) + abs(l1_metros) )
      } 
      else if( (l1_metros < 0 | l2_metros < 0) & declividade_graus > 4 | declividade_porcentagem > 7 ) {
        if( declividade_graus > 4 & l1_l2_graus == TRUE ) {
          res <- ( lh_metros * ( cos ( declividade_graus * pi / 180 ) ) ) * 
            ( tan ( abs(l1_metros) * pi / 180 ) + 
                ( tan ( abs(l2_metros) * pi / 180) ) )
        } 
        else if( declividade_graus > 4 & l1_l2_graus == FALSE ) {
          res <- ((lh_metros * (abs(l1_metros) + abs(l2_metros))/
                     escala_metros) * cos( declividade_graus * pi / 180 ))
        } 
        else if( declividade_porcentagem > 7 ) {
          res <- ( ( lh_metros * ( abs(l2_metros) + abs(l1_metros) ) ) / escala_metros ) * 
            ( cos ( atan( declividade_porcentagem/100 ) ) )
        }
      }
    }
    abs(res)
  }


# Exemplos de como usar a função e sua equivalente ------------------------


# exemplo suunto leitura em metros
altura_arvore( lh_metros = 28, 
               l1_metros = 3.50, 
               l2_metros = 24.30, 
               metodo = "suunto", 
               declividade_porcentagem = 15,
               escala_metros = 20 )
# equivalente a: 
((28*(24.30-3.5))/20)*(cos(atan(0.15)))

# exemplo prancheta
altura_arvore( lh_metros = 30, 
               l1_metros = -0.015, 
               l2_metros = 0.08, 
               metodo = "prancheta", 
               largura_prancheta = 0.1 )
# equivalente a: 
(30*(0.015+0.08))/0.1

# exemplo suunto, leitura em graus
altura_arvore( lh_metros = 31, 
               l1_metros = -11.60, 
               l2_metros = 35.50,
               l1_l2_graus = TRUE,
               metodo = "suunto", 
               declividade_graus = 8 )
# equivalente a: 
(31*(cos(8*pi/180)))*(tan(11.60*pi/180)+(tan(35.50*pi/180)))

# exemplo suunto, leitura em porcentagem deve ser transformada (/100)
altura_arvore( lh_metros = 29, 
               l1_metros = 0.08, 
               l2_metros = 0.9,
               metodo = "suunto" )
# equivalente a: 
29*(0.08+0.9)

# exemplo blume_leiss
altura_arvore( lh_metros = 29, 
               l1_metros = -51.96, 
               l2_metros = 0.25, 
               metodo = "blume_leiss", 
               declividade_graus = 56.65,
               escala_metros = 30 )
# equivalente a: 
((29*(51.96+0.25))/30)*cos(56.65*pi/180)