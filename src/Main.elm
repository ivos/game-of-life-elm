import Html exposing (..)
import Color exposing (..)
import Graphics.Collage as Collage
import Random as Random
import Time exposing (..)
import String as String

-- model

size = 100
scale = 3

type alias Model = List (Int, Int, Bool)

-- update

updateCell : (Int, Int, Bool) -> (Int, Int, Bool)
updateCell (x, y, bool) = (x, y, not bool)

update : Time -> Model -> Model
update time model = List.map updateCell model

-- view

convertCoordinates : Int -> Int -> (Float, Float)
convertCoordinates absoluteX absoluteY =
  let x = toFloat (absoluteX * scale) - (toFloat (size * scale) / 2)
      y = (toFloat (size * scale) / 2) - (toFloat (absoluteY * scale))
  in (x, y)

viewCell : (Int, Int, Bool) -> Collage.Form
viewCell (x, y, bool) =
  let cellColor = if bool then black else white
      cellShape = Collage.square scale
        |> Collage.filled cellColor
        |> Collage.move (convertCoordinates x y)
  in cellShape

view : Model -> Html
view model =
    let cellsOn = List.filter (\(x, y, bool) -> bool) model
        formList = List.map viewCell cellsOn
        offsetScale = scale * (size + 1)
        element = Collage.collage offsetScale offsetScale formList
    in div [] [ fromElement element ]

-- initial model

randomModelGenerator : Int -> Random.Generator (List Bool)
randomModelGenerator size = Random.list (size * size) Random.bool

seedModel : Int -> Model
seedModel size =
  let (bools, seed) = Random.generate (randomModelGenerator size) (Random.initialSeed 444555)
      xs = List.concat (List.repeat size [1..size])
      ys = List.concat (List.map (List.repeat size) [1..size])
      create3List = \a b c -> (a, b, c)
      model = List.map3 create3List xs ys bools
  in model

-- runtime

loop : Signal Model
loop = Signal.foldp update (seedModel size) (every (500 * millisecond))

main : Signal Html
main = Signal.map view loop
