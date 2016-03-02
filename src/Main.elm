import Html exposing (..)
import Color exposing (..)
import Graphics.Collage as Collage
import Random as Random
import Time exposing (..)
import String as String

-- model

size = 40
scale = 3

type alias Model = List (Int, Int, Bool)

-- update

offsets = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

wrap : Int -> Int -> Int
wrap size value = if value < 1 then size + value else if value > size then value - size else value

neighbors : Model -> (Int, Int) -> Int
neighbors cellsOn (x, y) =
  let neighborCoordinates = List.map (\(offsetX, offsetY) -> (wrap size (x + offsetX), wrap size (y + offsetY))) offsets
      filter = \(cellX, cellY, _) -> (List.member (cellX, cellY) neighborCoordinates)
      neighborCellsOn = List.filter filter cellsOn
  in List.length neighborCellsOn

updateCell : Model -> (Int, Int, Bool) -> (Int, Int, Bool)
updateCell cellsOn (x, y, bool) =
  let count = neighbors cellsOn (x, y)
      updated = (count == 3) || (bool && (count == 2))
  in (x, y, updated)

update : Time -> Model -> Model
update time model =
  let cellsOn = List.filter (\(cellX, cellY, bool) -> bool) model
  in List.map (updateCell cellsOn) model

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
