module Examples where

import LSystems (LSystem(LSystem))

cross, triangle, arrowHead, peanoGosper       :: LSystem
dragon, snowflake, tree, bush, canopy, galaxy :: LSystem

cross = LSystem 90 "M-M-M-M" [ ('M', "M-M+M+MM-M-M+M")
                             , ('+', "+")
                             , ('-', "-")
                             ]

triangle = LSystem 90 "-M" [ ('M', "M+M-M-M+M")
                           , ('+', "+")
                           , ('-', "-")
                           ]

arrowHead = LSystem 60 "N" [ ('M', "N+M+N")
                           , ('N', "M-N-M")
                           , ('+', "+")
                           , ('-', "-")
                           ]

peanoGosper = LSystem 60 "M" [ ('M', "M+N++N-M--MM-N+")
                             , ('N', "-M+NN++N+M--M-N")
                             , ('+', "+")
                             , ('-', "-")
                             ]

dragon = LSystem 45 "MX" [ ('M', "A")
                         , ('X', "+MX--MY+")
                         , ('Y', "-MX++MY-")
                         , ('A', "A")
                         , ('+', "+")
                         , ('-', "-")
                         ]

snowflake = LSystem 60 "M--M--M" [ ('M', "M+M--M+M")
                                 , ('+', "+")
                                 , ('-', "-")
                                 ]

tree = LSystem 45 "M" [ ('M', "N[-M][+M][NM]")
                      , ('N', "NN")
                      , ('[', "[")
                      , (']', "]")
                      , ('+', "+")
                      , ('-', "-")
                      ]

bush = LSystem 22.5 "X" [ ('X', "M-[[X]+X]+M[+MX]-X")
                        , ('M', "MM")
                        , ('[', "[")
                        , (']', "]")
                        , ('+', "+")
                        , ('-', "-")
                        ]

canopy = LSystem 30.0 "M" [ ('M', "M[+MM][-MM]M[-M][+M]M")
                          , ('[', "[")
                          , (']', "]")
                          , ('+', "+")
                          , ('-', "-")
                          ]

galaxy = LSystem 36.0 "[M]++[M]++[M]++[M]++[M]" [ ('M', "+M--M---M")
                                                , ('[', "[")
                                                , (']', "]")
                                                , ('+', "+")
                                                , ('-', "-")
                                                ]
