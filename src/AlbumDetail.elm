module AlbumDetail where

import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json exposing ((:=))
import Task
import Dict


-- MODEL

type alias Album =
  { artist: String
  , title: String
  , releaseDate: String
  , score: Int
  , artwork : Maybe String
  , itunesUrl: Maybe String
  , spotify: Maybe (Dict.Dict String String)
  }

type alias AlbumResult =
  { album: Maybe Album
  , isLoading: Bool
  }

init : (AlbumResult, Effects Action)
init =
  ({ album = Nothing, isLoading = True }, getRandomAlbum)


-- EFFECTS

getRandomAlbum : Effects Action
getRandomAlbum =
  Http.get decodeUrl randomUrl
    |> Task.toMaybe
    |> Task.map NewAlbum
    |> Effects.task


randomUrl : String
randomUrl =
  --Http.url "https://yslt-api.herokuapp.com/album.json" []
  Http.url "http://localhost:9000/album.json" []

spotifyDecoder : Json.Decoder (Dict.Dict String String)
spotifyDecoder = Json.dict Json.string
--

decodeUrl : Json.Decoder Album
decodeUrl =
  Json.object7 Album
    ("artist" := Json.string)
    ("title" := Json.string)
    ("releaseDate" := Json.string)
    ("score" := Json.int)
    (Json.maybe ("artwork" := Json.string))
    (Json.maybe ("itunesUrl" := Json.string))
    (Json.maybe ("spotify" := spotifyDecoder))


-- UPDATE

type Action
    = RequestMore
    | NewAlbum (Maybe Album)


update : Action -> AlbumResult -> (AlbumResult, Effects Action)
update action model =
  case action of
    RequestMore ->
      ({ model | isLoading = True }, getRandomAlbum)

    NewAlbum maybeAlbum ->
      ({ album = maybeAlbum, isLoading = False }, Effects.none)


-- VIEW

view : Signal.Address Action -> AlbumResult -> Html
view address model =
  section [ class "yslt" ] [
    headerView,
    albumFetcherView model,
    footerView address
  ]

albumFetcherView: AlbumResult -> Html
albumFetcherView model =
  case model.album of
    Just album -> albumDetailView album model.isLoading
    Nothing -> if model.isLoading then
      nothingView "Loading…"
    else
      nothingView "Failed to load album"


nothingView: String -> Html
nothingView msg =
  div [ class "album" ] [
    div [ class "album__media" ] [],
    div [ class "album__body" ] [
      div [ class "album__loading" ] [ text msg ]
    ]
  ]

albumDetailView: Album -> Bool -> Html
albumDetailView album isLoading =
  div [ classList [
      ("album", True),
      ("is-loading", isLoading)
    ]
  ] [
    div [ class "album__media" ] [
      artworkView album
    ],
    div [ class "album__body" ] [
      div [ class "album__details" ] [
        h2 [ class "album__title" ] [ text album.title ],
        span [ class "album__rating", title "Metascore" ] [ text (toString album.score) ]
      ],
      div [ class "album__meta" ] [
        span [ class "album__artist" ] [ text album.artist ]
      ]
      --ul [ class "album__links list-unstyled" ]
      --  [ li [] [itunesLinkView album.itunesUrl] ]
    ]
  ]

artworkView : Album -> Html
artworkView model =
  case model.artwork of
    Just url -> img [ src url, alt model.title  ] []
    Nothing -> span [] []

--itunesLinkView: Maybe String -> Html
--itunesLinkView maybeLink =
--  case maybeLink of
--    Just url -> a [ href url, class "album__link" ] [ text "iTunes" ]
--    Nothing -> span [ class "album__link is-disabled" ] [ text "iTunes" ]

headerView: Html
headerView =
  header [ class "header", attribute "role" "banner" ] [
    h1 [ class "logo" ] [ text "You should listen to" ]
  ]

footerView: Signal.Address Action -> Html
footerView address =
  footer [ class "footer" ] [
    button [ class "btn", onClick address RequestMore ] [ text "Try Another" ],
    div [ class "footer__credits" ] [
      p [] [ text "Top rated Metacritic albums this year" ],
      p [] [
        span [] [ text "A thing from" ],
        text " ",
        a [ href "https://twitter.com/davidrapson" ] [ text "David Rapson" ]
      ],
      p [] [
        a [ href "https://github.com/davidrapson/yslt" ] [ text "Source" ]
      ]
    ]
  ]
