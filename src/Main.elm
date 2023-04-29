module Main exposing (..)

--import Time
--import AppUrl exposing (AppUrl, QueryParameters)

import Browser
import Browser.Events as Events
import Ease
import Element as El
    exposing
        ( Attribute
        , Element
        , alignRight
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , layout
        , none
        , padding
        , paragraph
        , px
        , rgb255
        , rgba
        , row
        , spacing
        , text
        , textColumn
        , width
        )
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Fifo exposing (Fifo)
import Html.Attributes as Attr
import Return exposing (Return)



-- Types --


type alias Model =
    { page : Route
    , verses : Verses
    , animationState : VerseAnimation
    }


type alias BibleVerse =
    { verse : String, ref : String }


type alias Verses =
    { current : BibleVerse, others : Fifo BibleVerse }


type Msg
    = Goto Route
    | Tick Float


type Route
    = Home
    | Blog Int
    | About
    | Donate
    | NotFound



-- Verse Animation Functions --


type VerseAnimation
    = FadeInVerse Float Float
    | HoldOnVerse Float Float
    | FadeOutVerse Float Float
    | UpdateVerse


fadeDuration : Float
fadeDuration =
    1000


holdDuration : Float
holdDuration =
    10000


animInit : VerseAnimation
animInit =
    FadeInVerse fadeDuration 0


updateVerseAnimation : Float -> VerseAnimation -> VerseAnimation
updateVerseAnimation currentFrame anim =
    case anim of
        FadeInVerse dur current ->
            if current < dur then
                FadeInVerse dur (current + currentFrame)

            else
                HoldOnVerse holdDuration 0

        HoldOnVerse dur current ->
            if current < dur then
                HoldOnVerse dur (current + currentFrame)

            else
                FadeOutVerse fadeDuration 0

        FadeOutVerse dur current ->
            if current < dur then
                FadeOutVerse dur (current + currentFrame)

            else
                UpdateVerse

        UpdateVerse ->
            animInit


nextVerse : Verses -> Verses
nextVerse vs =
    let
        ( mv, vsTemp ) =
            Fifo.remove vs.others
    in
    case mv of
        Just v ->
            { current = v
            , others = Fifo.insert vs.current vsTemp
            }

        Nothing ->
            vs


verseAnimationToAlpha : VerseAnimation -> Attribute Msg
verseAnimationToAlpha va =
    case va of
        FadeInVerse duration current ->
            El.alpha <| Ease.inOutSine <| current / duration

        HoldOnVerse _ _ ->
            El.alpha 1.0

        FadeOutVerse duration current ->
            El.alpha <| Ease.reverse Ease.inOutSine <| current / duration

        UpdateVerse ->
            El.alpha 0.01



-- Main Function --


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subs
        }



-- Initial State --


init : () -> Return Msg Model
init _ =
    Return.singleton
        { page = About
        , verses =
            Verses
                (Maybe.withDefault { verse = "", ref = "" } <|
                    List.head bibleVerseList
                )
            <|
                Fifo.fromList (Maybe.withDefault [] <| List.tail bibleVerseList)
        , animationState = animInit
        }



-- Update Function --


update : Msg -> Model -> Return Msg Model
update msg model =
    case msg of
        Goto page ->
            Return.singleton { model | page = page }

        Tick timeDelta ->
            let
                newAnimationState =
                    updateVerseAnimation timeDelta
                        model.animationState
            in
            Return.singleton
                { model
                    | animationState = newAnimationState
                    , verses =
                        case newAnimationState of
                            UpdateVerse ->
                                nextVerse model.verses

                            _ ->
                                model.verses
                }



-- Subscriptions --


subs : Model -> Sub Msg
subs _ =
    Sub.batch
        [ Events.onAnimationFrameDelta Tick
        ]



-- View Function --


gap : number
gap =
    20


view : Model -> Browser.Document Msg
view model =
    { title = "Tiwale"
    , body =
        [ layout
            [ Font.color pal.black
            , Font.family [ Font.serif ]
            , Bg.gradient
                { angle = (7 / 8) * pi
                , steps =
                    [ pal.sun
                    , pal.sky
                    , pal.up
                    ]
                }
            , padding <| 2 * gap // 3
            ]
          <|
            column
                [ spacing gap
                , height fill
                , centerX
                ]
                [ row [ width fill, spacing <| gap // 2 ]
                    [ row [ width fill, spacing gap ]
                        [ column
                            [ Font.color pal.black
                            , Font.shadow
                                { offset = ( 1, 1 )
                                , blur = 25.5
                                , color = pal.yellow
                                }
                            , spacing 4
                            ]
                            [ El.image
                                [ width <| px 200
                                ]
                                { src = "assets/tiwale.png"
                                , description = "Tiwale: Helping Malawians Shine"
                                }
                            ]
                        , longestVerseView model.animationState model.verses.current
                        ]
                    ]
                , El.wrappedRow
                    [ spacing gap
                    , Border.widthEach { edges | top = 2, bottom = 2 }
                    , Border.color <| rgba 0 0 0 0.1
                    , Border.dashed
                    , El.paddingEach
                        { edges
                            | top = gap
                            , bottom = round <| gap * 1.25
                        }
                    , width fill
                    ]
                    [ pageButton "About us" About model.page
                    , pageButton "What we do" Home model.page
                    , pageButton "Donations" Donate model.page
                    ]
                , case model.page of
                    Home ->
                        homeView model

                    Blog _ ->
                        el [] <| text "Blog"

                    About ->
                        aboutView model

                    Donate ->
                        donateView model

                    NotFound ->
                        el [] <| text "404: Page Not Found!"
                ]
        ]
    }


bibleVerseList : List BibleVerse
bibleVerseList =
    [ { verse = "In the same way, let your light shine before others, so that they may see your good works and give glory to your Father who is in heaven."
      , ref = "Matthew 5:16 ESV"
      }
    , { verse = "Little children, let us not love in word or talk but in deed and in truth."
      , ref = "1 John 3:18 ESV"
      }
    , { verse = "Whatever you do, work heartily as for the Lord and not for men, knowing that from the Lord you will receive the inheritance as your reward. You are serving the Lord Christ."
      , ref = "Colossians 3:23-24"
      }
    , { verse = "I do it all for the sake of the gospel, that I may share with them in its blessings."
      , ref = "1 Corinthians 9:23"
      }
    , { verse = "I have been crucified with Christ. It is no longer I who live, but Christ who lives in me. And the life I now live in the flesh I live by faith in the Son of God, who loved me and gave himself for me."
      , ref = "Galatians 2:20"
      }
    ]


emptyVerse : BibleVerse
emptyVerse =
    { verse = "", ref = "" }


longestVerse : BibleVerse
longestVerse =
    bibleVerseList
        |> List.foldl checkLength emptyVerse


checkLength : BibleVerse -> BibleVerse -> BibleVerse
checkLength v1 v2 =
    let
        lengthV1 =
            String.length v1.verse + String.length v1.ref

        lengthV2 =
            String.length v2.verse + String.length v2.ref
    in
    if lengthV1 > lengthV2 then
        v1

    else
        v2


viewVerse : VerseAnimation -> BibleVerse -> Element Msg
viewVerse anim b =
    paragraph
        (List.concat
            [ noSelect True
            , [ Font.size 16
              , Font.italic
              , Font.justify
              , Font.color pal.md
              , centerY
              , width <| El.maximum 600 fill
              , verseAnimationToAlpha anim
              ]
            ]
        )
        [ text "\""
        , text b.verse
        , text "\""
        , el
            [ alignRight
            , Font.color pal.up
            ]
          <|
            text <|
                "("
                    ++ b.ref
                    ++ ")"
        ]


longestVerseView : VerseAnimation -> BibleVerse -> Element Msg
longestVerseView anim b =
    paragraph
        (List.concat
            [ noSelect True
            , [ Font.italic
              , Font.justify
              , Font.color <| El.rgba 0 0 0 0
              , centerY
              , width <| El.maximum 600 fill
              , El.inFront <| viewVerse anim b
              ]
            ]
        )
        [ text "\""
        , text longestVerse.verse
        , text "\""
        , el
            [ alignRight
            , Font.color <| El.rgba 0 0 0 0
            ]
          <|
            text <|
                "("
                    ++ longestVerse.ref
                    ++ ")"
        ]



-- Other Stuff --


emailElsie : Element Msg
emailElsie =
    El.link
        [ Font.color pal.link
        , Font.shadow
            { offset = ( 1, 1 )
            , blur = 1.2
            , color = rgba 0 0.2 0.4 0.25
            }
        , Border.widthEach { edges | bottom = 2 }
        , Border.dashed
        ]
        { url = "mailto:elsiestanton26@gmail.com"
        , label = text "Elsie"
        }


pageButton : String -> Route -> Route -> Element Msg
pageButton label target current =
    if current == target then
        el
            [ Font.color pal.black
            , Font.shadow
                { offset = ( 1, 1 )
                , blur = 1.5
                , color = rgba 0 0 0 0.25
                }
            , Border.widthEach { edges | bottom = 2 }
            , Border.dashed
            ]
        <|
            text label

    else
        Input.button
            [ Font.color pal.link
            , Font.shadow
                { offset = ( 1, 1 )
                , blur = 1.2
                , color = rgba 0 0.2 0.4 0.25
                }
            , Border.widthEach { edges | bottom = 2 }
            , Border.dashed
            ]
            { onPress = Just <| Goto target
            , label = text label
            }


body : List (Element Msg) -> Element Msg
body =
    textColumn
        [ width fill
        , height fill
        , centerX
        , spacing gap
        , El.paddingEach { edges | bottom = gap * 3 }
        , width <| El.maximum 800 fill
        ]


par : List (Element Msg) -> Element Msg
par ts =
    paragraph
        [ Font.justify
        , spacing 10
        ]
    <|
        List.intersperse (El.text " ") ts


highlighted : Element Msg
highlighted =
    el
        [ Bg.color <| El.rgba 1 0 0 0.5
        , El.paddingEach { edges | bottom = 5 }
        ]
    <|
        text "A"


homeView : Model -> Element Msg
homeView model =
    body
        [ par
            [ text "We assist students by paying their school fees, as well as providing uniforms, shoes, exercise books, backpacks, writing materials, and scientific calculators."
            , text "Because most of the students live in remote areas with little or no access to electricity, we also provide solar powered torches/flashlights to give students the ability to do their homework after dark."
            ]
        , par
            [ text "This is just our initial program at Tiwale."
            , text <| "We are currently helping " ++ String.fromInt numStudents ++ " students."
            , text "There are many more who need help!"
            , text "In the future, we also hope to expand our help to communities with other needs, such as improving access to clean water."
            ]
        , par
            [ text "Please consider"
            , pageButton "donating" Donate model.page
            , text "to help Malawians shine!"
            ]
        ]


numStudents : Int
numStudents =
    14


aboutView : Model -> Element Msg
aboutView _ =
    body
        [ textColumn
            [ width fill
            , spacing gap
            ]
            [ el
                [ El.paddingEach { edges | left = gap }
                , El.alignRight
                ]
              <|
                El.image
                    [ width <| px 300
                    ]
                    { src = "assets/ken-elsie.png"
                    , description = "Ken and Elsie"
                    }
            , par
                [ text "In Malawi, school is free for primary school, but secondary students are expected to pay tuition every quarter."
                , text "Many students struggle with this financial requirement and some are unable to attend school."
                ]
            , par
                [ text "At Tiwale, we believe that everyone deserves the chance to reach their full potential, no matter where they come from."
                , text "Our mission is to help Malawians by providing access to education and resources that allow for brighter futures."
                ]
            , el [ width fill ] <|
                el [ centerX, Font.bold ] <|
                    text "Ken & Elsie Stanton"
            , par
                [ text "Tiwale was founded by Ken and Elsie Stanton in 2022."
                ]
            ]
        ]


donateView : Model -> Element Msg
donateView _ =
    body
        [ par
            [ text "Any donation amount can help our students reach their goals."
            , text "Each student requires an average of $35 per month to cover school expenses."
            , text "It doesn't take much to greatly improve someone's life!"
            ]
        , par
            [ text "At least 80% of donations will go directly to project related expenses, school supplies, and fees for students."
            , text "Our plan is to keep 20% with a cap of $800 per team member per month to sustain work on our projects."
            ]
        , par
            [ text "We should be able to do a lot while keeping our team lean, and working with school faculty members, and community leaders who already have a vested interest in seeing there young people succeed."
            ]
        , par
            [ text "Please contact"
            , emailElsie
            , text "for more information."
            ]
        ]


pal =
    { link = rgb255 0x00 0x55 0x99 -- #005599
    , md = rgb255 0x11 0x77 0x99 -- #3399bb
    , up = rgb255 0x44 0xAA 0xCC -- #44aacc
    , fg = rgb255 0x66 0xDD 0xFF -- #66ddff
    , sky = rgb255 0xCC 0xEE 0xFF -- #cceeff
    , white = rgb255 0xFF 0xFF 0xFF -- #ffffff
    , black = rgb255 0x11 0x22 0x33 -- #112233
    , sun = rgb255 0xFF 0xEE 0xCC -- #ffeecc
    , yellow = rgb255 0xFF 0xDD 0x66 -- #ffdd66
    }


edges :
    { top : Int
    , bottom : Int
    , left : Int
    , right : Int
    }
edges =
    { top = 0
    , bottom = 0
    , left = 0
    , right = 0
    }


corners :
    { topLeft : Int
    , topRight : Int
    , bottomLeft : Int
    , bottomRight : Int
    }
corners =
    { topLeft = 0
    , topRight = 0
    , bottomLeft = 0
    , bottomRight = 0
    }


noSelect : Bool -> List (Attribute Msg)
noSelect b =
    let
        none =
            if b then
                "none"

            else
                "auto"
    in
    [ style "-webkit-user-select" none
    , style "-khtml-user-select" none
    , style "-webkit-touch-callouT" none
    , style "-moz-user-select" none
    , style "-o-user-select" none
    , style "user-select" none
    ]


style : String -> String -> Attribute Msg
style s t =
    El.htmlAttribute <| Attr.style s t
