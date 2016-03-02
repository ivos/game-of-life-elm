import Html exposing (..)
import Color exposing (..)
import Graphics.Collage as Collage
import Random as Random
import Time exposing (..)
import String as String

-- model

size = 100
scale = 3

type alias Model = List (List Bool)

-- update

updateCell : Bool -> Bool
updateCell value = not value

updateRow : (List Bool) -> (List Bool)
updateRow row = List.map updateCell row

update : Time -> Model -> Model
update time model = List.map updateRow model

-- view

convertCoordinates : Int -> Int -> (Float, Float)
convertCoordinates absoluteX absoluteY =
  let x = toFloat (absoluteX * scale) - (toFloat (size * scale) / 2)
      y = (toFloat (size * scale) / 2) - (toFloat (absoluteY * scale))
  in (x, y)

cell : (Int, Int, Bool) -> Collage.Form
cell (x, y, bool) =
  let cellColor = if bool then black else white
      cellShape = Collage.square scale
        |> Collage.filled cellColor
        |> Collage.move (convertCoordinates x y)
  in cellShape

row : (Int, List Bool) -> List Collage.Form
row (y, bools) =
  let ys = List.repeat size y
      xs = [1..size]
      create3List = \a b c -> (a, b, c)
      cellBools = List.map3 create3List xs ys bools
      cellBoolsOn = List.filter (\(x, y, bool) -> bool) cellBools
  in List.map cell cellBoolsOn

rows : List (List Bool) -> Html
rows lists =
  let ys = [1..size]
      rowLists = List.map2 (,) ys lists
      mappedLists = List.concatMap row rowLists
      offsetScale = scale * (size + 1)
  in fromElement (Collage.collage offsetScale offsetScale mappedLists)

view : Model -> Html
view model = div [] [ rows model ]

-- initial model

randomModelGenerator : Int -> Random.Generator (List (List Bool))
randomModelGenerator size = Random.list size (Random.list size Random.bool)

seedModel : Int -> Model
seedModel size =
  let (model, seed) = Random.generate (randomModelGenerator size) (Random.initialSeed 444555)
  in model

-- runtime

loop : Signal Model
loop = Signal.foldp update (seedModel size) (every (500 * millisecond))

main : Signal Html
main = Signal.map view loop
