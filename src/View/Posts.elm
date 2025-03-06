module View.Posts exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes exposing (href)
import Html.Events
import Model exposing (Msg(..))
import Model.Post exposing (Post)
import Model.PostsConfig exposing (Change(..), PostsConfig, SortBy(..), filterPosts, sortFromString, sortOptions, sortToCompareFn, sortToString)

import Time
import Util.Time
import Model.PostsConfig as PostsConfig
import Model.PostsConfig as Config


{-| Show posts as a HTML [table](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table)

Relevant local functions:

  - Util.Time.formatDate
  - Util.Time.formatTime
  - Util.Time.formatDuration (once implemented)
  - Util.Time.durationBetween (once implemented)

Relevant library functions:

  - [Html.table](https://package.elm-lang.org/packages/elm/html/latest/Html#table)
  - [Html.tr](https://package.elm-lang.org/packages/elm/html/latest/Html#tr)
  - [Html.th](https://package.elm-lang.org/packages/elm/html/latest/Html#th)
  - [Html.td](https://package.elm-lang.org/packages/elm/html/latest/Html#td)

-}
postTable : PostsConfig -> Time.Posix -> List Post -> Html Msg
postTable config currentTime posts =
    let
        filteredPosts =
            Config.filterPosts config posts
    in
    Html.table []
        [ Html.thead []
            [ Html.tr []
                [ Html.th [] [ Html.text "Score" ]
                , Html.th [] [ Html.text "Title" ]
                , Html.th [] [ Html.text "Type" ]
                , Html.th [] [ Html.text "Posted Date" ]
                , Html.th [] [ Html.text "Link" ]
                ]
            ]
        , Html.tbody []
            (List.map (postRow currentTime) filteredPosts)
        ]


postRow : Time.Posix -> Post -> Html Msg
postRow currentTime post =
    let
        formattedTime =
            Util.Time.formatTime Time.utc post.time

        relativeDuration =
            case Util.Time.durationBetween post.time currentTime of
                Just duration ->
                    " (" ++ Util.Time.formatDuration duration ++ ")"

                Nothing ->
                    ""
    in
    Html.tr []
        [ Html.td [ Html.Attributes.class "post-score" ] [ Html.text (String.fromInt post.score) ]
        , Html.td [ Html.Attributes.class "post-title" ] [ Html.text post.title ]
        , Html.td [ Html.Attributes.class "post-type" ] [ Html.text post.type_ ]
        , Html.td [ Html.Attributes.class "post-time" ] [ Html.text (formattedTime ++ relativeDuration) ]
        , Html.td [ Html.Attributes.class "post-url" ] [ maybeLink post.url ]
        ]


maybeLink : Maybe String -> Html Msg
maybeLink maybeUrl =
    case maybeUrl of
        Just url ->
            Html.a [ Html.Attributes.href url ] [ Html.text "Link" ]

        Nothing ->
            Html.text "No link"



    -- div [] []
    --Debug.todo "postTable"


{-| Show the configuration options for the posts table

Relevant functions:

  - [Html.select](https://package.elm-lang.org/packages/elm/html/latest/Html#select)
  - [Html.option](https://package.elm-lang.org/packages/elm/html/latest/Html#option)
  - [Html.input](https://package.elm-lang.org/packages/elm/html/latest/Html#input)
  - [Html.Attributes.type\_](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#type_)
  - [Html.Attributes.checked](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#checked)
  - [Html.Attributes.selected](https://package.elm-lang.org/packages/elm/html/latest/Html-Attributes#selected)
  - [Html.Events.onCheck](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onCheck)
  - [Html.Events.onInput](https://package.elm-lang.org/packages/elm/html/latest/Html-Events#onInput)

-}
postsConfigView : PostsConfig -> Html Msg
postsConfigView config =
    Html.div []
        [ Html.div []
            [ Html.label [ Html.Attributes.for "select-posts-per-page" ] [ Html.text "Posts per page: " ]
            , Html.select
                [ Html.Attributes.id "select-posts-per-page"
                , Html.Events.onInput (ConfigChanged << PostsConfig.ChangePostsToShow << Maybe.withDefault 10 << String.toInt)
                ]
                (List.map (postsToShowOption config.postsToShow) [ 10, 25, 50 ])
            ]
        , Html.div []
            [ Html.label [ Html.Attributes.for "select-sort-by" ] [ Html.text "Sort by: " ]
            , Html.select
                [ Html.Attributes.id "select-sort-by"
                , Html.Events.onInput
                    (\input ->
                        case PostsConfig.sortFromString input of
                            Just sortBy ->
                                ConfigChanged (PostsConfig.ChangeSortBy sortBy)

                            Nothing ->
                                ConfigChanged (PostsConfig.ChangeSortBy PostsConfig.None)
                    )
                ]
                (List.map (sortByOption config.sortBy) PostsConfig.sortOptions)
            ]
        , Html.div []
            [ Html.label [ Html.Attributes.for "checkbox-show-job-posts" ]
                [ Html.text "Show job posts" ]
            , Html.input
                [ Html.Attributes.id "checkbox-show-job-posts"
                , Html.Attributes.type_ "checkbox"
                , Html.Attributes.checked config.showJobs
                , Html.Events.onCheck (ConfigChanged << PostsConfig.ChangeShowJobs)
                ]
                []
            ]
        , Html.div []
            [ Html.label [ Html.Attributes.for "checkbox-show-text-only-posts" ]
                [ Html.text "Show text-only posts" ]
            , Html.input
                [ Html.Attributes.id "checkbox-show-text-only-posts"
                , Html.Attributes.type_ "checkbox"
                , Html.Attributes.checked config.showTextOnly
                , Html.Events.onCheck (ConfigChanged << PostsConfig.ChangeShowTextOnly)
                ]
                []
            ]
        ]


postsToShowOption : Int -> Int -> Html msg
postsToShowOption selected value =
    Html.option
        [ Html.Attributes.value (String.fromInt value)
        , Html.Attributes.selected (value == selected)
        ]
        [ Html.text (String.fromInt value) ]

sortByOption : PostsConfig.SortBy -> PostsConfig.SortBy -> Html msg
sortByOption selected value =
    Html.option
        [ Html.Attributes.value (PostsConfig.sortToString value)
        , Html.Attributes.selected (value == selected)
        ]
        [ Html.text (PostsConfig.sortToString value) ]

     --div [] []
    --Debug.todo "postsConfigView"
