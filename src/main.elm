module Simple.Main exposing (main)

{-| A simple Arborist app modeling a conversation flow.
-}

import Arborist
import Arborist.Settings as Settings
import Arborist.Tree as Tree
import Browser
import Html exposing (Html, a, button, div, h1, h2, h3, input, label, map, node, p, text)
import Html.Attributes exposing (class, href, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Encode as Encode
import Styles as Styles


{-| The Node data type held in each of the tree's nodes.
-}
type alias Node =
    { name : String
    , comment : String
    }


setName : String -> Node -> Node
setName val item =
    { item | name = val }


setComment : String -> Node -> Node
setComment val item =
    { item | comment = val }


{-| Program model.
-}
type alias Model =
    { arborist : Arborist.Model Node

    -- Keep track of a to-be-inserted node
    , newNode : Node
    }


{-| The starting tree.
-}
tree : Tree.Tree Node
tree =
    Tree.Node { comment = "", name = "My awesome European trip" }
        [ Tree.Node { comment = "25 Sept - Wed 27", name = "Amsterdam" }

            [ Tree.Node { comment = "A must!", name = "Canal boat trip" } []
            ]
        , Tree.Node { comment = "27 Sept - 28 Sept", name = "Brussels" }
            [ Tree.Node { comment = "Yes", name = "Shall we see Manneken Pis?" } []
            ]
        , Tree.Node { comment = "28 Sept - 30 Sept", name = "Paris" }
            [ Tree.Node { comment = "", name = "Climb a tower" } []
            ]
        ]


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    ( { arborist =
            Arborist.initWith
                [ Settings.centerOffset 0 -150
                , Settings.nodeHeight 45
                , Settings.level 100
                , Settings.nodeWidth 160
                , Settings.sturdyMode False
                , Settings.canvasWidth 1200
                , Settings.canvasHeight 600
                , Settings.defaultNode { name = "abc", comment = "def" }
                , Settings.showPlaceholderLeaves True
                ]
                tree
      , newNode = { name = "", comment = "" }
      }
    , Cmd.none
    )


{-| Program message
-}
type Msg
    = ArboristMsg Arborist.Msg
    | EditNewNodeName String
    | EditNewNodeComment String
    | SetActive Node
    | DeleteActive



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ArboristMsg arboristMsg ->
            ( { model | arborist = Arborist.update arboristMsg model.arborist }
            , Cmd.none
            )

        SetActive newNode ->
            ( { model | arborist = Arborist.setActiveNodeWithChildren newNode (Just []) model.arborist }
            , Cmd.none
            )

        DeleteActive ->
            ( { model | arborist = Arborist.deleteActiveNode model.arborist }
            , Cmd.none
            )

        EditNewNodeName val ->
            ( { model
                | newNode = setName val model.newNode
              }
            , Cmd.none
            )

        EditNewNodeComment val ->
            ( { model
                | newNode = setComment val model.newNode
              }
            , Cmd.none
            )



-- View


view : Model -> Html Msg
view model =
    div [] <|
        [ node "style" [] [ text Styles.raw ]
        ]
            ++ [ -- For pop-up coordinates to work, include view in a container
                 div
                    [ style "margin" "auto"
                    , style "position" "absolute"
                    , style "top" "0px"
                    , style "left" "0px"
                    ]
                 <|
                    [ Arborist.view nodeView
                        (Styles.box
                            |> List.map (\( property, value ) -> style property value)
                        )
                        model.arborist
                        |> Html.map ArboristMsg
                    ]
                        ++ (Arborist.activeNode model.arborist
                                |> Maybe.map
                                    (\( item, { position } ) ->
                                        let
                                            ( x, y ) =
                                                position
                                        in
                                        [ div
                                            (List.map (\( property, value ) -> style property value) Styles.popup
                                                ++ [ style "left" <| String.fromFloat x ++ "px"
                                                   , style "top" <| String.fromFloat y ++ "px"
                                                   ]
                                            )
                                            (case item of
                                                Just justItem ->
                                                    [ label []
                                                        [ text "Name"
                                                        , input
                                                            [ value justItem.name
                                                            , onInput (\val -> SetActive { justItem | name = val })
                                                            ]
                                                            []
                                                        ]
                                                    , label []
                                                        [ text "Comment"
                                                        , input
                                                            [ value justItem.comment
                                                            , onInput (\val -> SetActive { justItem | comment = val })
                                                            ]
                                                            []
                                                        ]
                                                    , button
                                                        ((Styles.button
                                                            |> List.map (\( property, value ) -> style property value)
                                                         )
                                                            ++ [ onClick DeleteActive
                                                               ]
                                                        )
                                                        [ text "Delete" ]
                                                    ]

                                                Nothing ->
                                                    [ label []
                                                        [ text "Name", input [ value model.newNode.name, onInput EditNewNodeName ] [] ]
                                                    , label []
                                                        [ text "Comment", input [ value model.newNode.comment, onInput EditNewNodeComment ] [] ]
                                                    , button
                                                        ((Styles.button
                                                            |> List.map (\( property, value ) -> style property value)
                                                         )
                                                            ++ [ type_ "submit"
                                                               , onClick (SetActive model.newNode)
                                                               ]
                                                        )
                                                        [ text "Add node" ]
                                                    ]
                                            )
                                        ]
                                    )
                                |> Maybe.withDefault []
                           )
               ]


{-| Describe how a node should render inside the tree's layout.
-}
nodeView : Arborist.NodeView Node
nodeView context maybeNode =
    maybeNode
        |> Maybe.map
            (\node ->
                div
                    (Styles.nodeContainer
                        ++ [ ( "background-color"
                             , case context.state of
                                Arborist.Active ->
                                    Styles.green

                                Arborist.Hovered ->
                                    Styles.lightBlue

                                Arborist.DropTarget ->
                                    Styles.orange

                                Arborist.Normal ->
                                    Styles.blue
                             )
                           , ( "color", "white" )
                           ]
                        |> List.map (\( property, value ) -> style property value)
                    )
                    [ div [] <|
                        (if node.comment /= "" then
                            [ Styles.bubble node.comment
                            ]

                         else
                            []
                        )
                            ++ [ Styles.bodyText node.name
                               ]
                    ]
            )
        |> Maybe.withDefault
            (div
                (Styles.nodeContainer
                    ++ (case context.state of
                            Arborist.Active ->
                                [ ( "background-color", Styles.green )
                                , ( "color", "white" )
                                , ( "border", "0" )
                                ]

                            Arborist.DropTarget ->
                                [ ( "background-color", Styles.orange )
                                , ( "border", "0" )
                                , ( "color", "white" )
                                ]

                            _ ->
                                [ ( "background-color", "transparent" )
                                , ( "border", "1px dashed #CECECE" )
                                , ( "color", "#898989" )
                                ]
                       )
                    |> List.map (\( property, value ) -> style property value)
                )
                [ Styles.bodyText "New child"
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Arborist.subscriptions model.arborist |> Sub.map ArboristMsg


{-| Entry point
-}
main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
