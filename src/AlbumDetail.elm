module AlbumDetail where

import String
import Dict
import Task
import Http
import Json.Decode as Json exposing ((:=))
import Effects exposing (Effects, Never)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- MODEL

type alias Album =
  { artist: String
  , title: String
  , releaseDate: String
  , score: Int
  , spotify: Maybe Spotify
  }

type alias Spotify =
  { id: String
  , uri: String
  , player_url: String
  , images: List Image
  }

type alias Image =
  { url: String
  , width: Int
  , height: Int
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
  Http.url "https://yslt-api-wkfnhwzkah.now.sh/album.json" []


-- DECODERS

decodeSpotify : Json.Decoder Spotify
decodeSpotify =
  Json.object4 Spotify
    ("id" := Json.string)
    ("uri" := Json.string)
    (Json.at ["external_urls", "spotify"] Json.string)
    ("images" := (
      Json.list <|
        Json.object3 Image
          ("url" := Json.string)
          ("width" := Json.int)
          ("height" := Json.int)
    ))

decodeUrl : Json.Decoder Album
decodeUrl =
  Json.object5 Album
    ("artist" := Json.string)
    ("title" := Json.string)
    ("releaseDate" := Json.string)
    ("score" := Json.int)
    (Json.maybe ("spotify" := decodeSpotify))


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


-- HELPERS

srcset : (List Image) -> String
srcset images =
  List.map (\x -> x.url ++ " " ++ (toString x.width) ++ "w") images
    |> String.join ", "


-- VIEW

{-
  Would be nice if there was Html.nothing
  Unless I'm missing something and this could
  be more carefully handled with Maybe
-}
htmlNothing : Html
htmlNothing = text ""

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
      (
        case album.spotify of
          Just spotify -> artworkView spotify.images album.title
          _ -> htmlNothing
      )
    ],
    div [ class "album__body" ] [
      div [ class "album__details" ] [
        h2 [ class "album__title" ] [ text album.title ],
        span [ class "album__rating", title "Metascore" ] [ text (toString album.score) ]
      ],
      div [ class "album__meta" ] [
        span [ class "album__artist" ] [ text album.artist ]
      ],
      ul [ class "album__links list-unstyled" ]
        [ li [] [spotifyLinkView album.spotify] ]
    ]
  ]

artworkView : (List Image) -> String -> Html
artworkView images altText =
  case (List.head images) of
    Just mainImg -> (
      img [
        src mainImg.url,
        attribute "srcset" (srcset images),
        attribute "sizes" "100vw",
        alt altText
      ] []
    )
    _ -> htmlNothing

spotifyLinkView: Maybe Spotify -> Html
spotifyLinkView maybeSpotify =
  case maybeSpotify of
    Just spotify -> a [ href spotify.player_url, class "album__link" ] [ text "Spotify" ]
    _ -> htmlNothing

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
        a [ href "https://github.com/davidrapson/yslt-elm" ] [ text "Source" ]
      ]
    ]
  ]
