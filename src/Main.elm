import Html exposing (..)
import Color exposing (..)
import Graphics.Collage as Collage
import Random as Random
import Time exposing (..)
import String as String

-- model

size = 20
scale = 5

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
  let x = (toFloat absoluteX * toFloat scale) - (toFloat size / 2 * toFloat scale)
      y = (toFloat size / 2 * toFloat scale) - (toFloat absoluteY * toFloat scale)
  in (x, y)

cell : (Int, Int, Bool) -> Collage.Form
cell (x, y, bool) =
  let cellColor = if bool then black else white
      cellShape = Collage.square 1
        |> Collage.filled cellColor
        |> Collage.move (convertCoordinates x y)
        |> Collage.scale scale
  in cellShape

row : (Int, List Bool) -> List Collage.Form
row (y, bools) =
  let ys = List.repeat size y
      xs = [1..size]
      create3List = \a b c -> (a, b, c)
      cellBools = List.map3 create3List xs ys bools
  in List.map cell cellBools

rows : List (List Bool) -> Html
rows lists =
  let ys = [1..size]
      rowLists = List.map2 (,) ys lists
      mappedLists = List.concatMap row rowLists
  in fromElement (Collage.collage (scale * (size + 1)) (scale * (size + 1)) mappedLists)

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
loop = Signal.foldp update (seedModel size) (every second)

main : Signal Html
main = Signal.map view loop

