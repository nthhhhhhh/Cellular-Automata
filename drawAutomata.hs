module DrawAutomata (getAllCoordinates, seedGeneration) where

import           CellularAutomata
import           Codec.Picture               (PixelRGBA8 (..))
import           Control.Monad
import           Data.ByteString.Lazy.Char8
import           Data.Sequence               as S
import           Graphics.Rasterific
import           Graphics.Rasterific.Texture




black :: PixelRGBA8
black = PixelRGBA8 0 0 0 255


seedGeneration :: Int -> Generation
seedGeneration width
    | odd width = error "Must have even paddings on left and right."
    | otherwise = S.replicate (width `div` 2) 0
                    >< S.singleton 1
                    >< S.replicate (width `div` 2) 0


getCACoordsFromGen :: Generation -> Int -> Int -> Seq (Int, Int)
getCACoordsFromGen gen column width =
  S.zip (S.fromList $ S.elemIndicesL 1 gen) (S.replicate width column)


getAllCoordinates :: Generation -> Int -> Int -> Int -> Int -> Seq (Int, Int) -> Seq (Int, Int)
getAllCoordinates seedGen width column maxHeight rule coordinates
    | column >= maxHeight = coordinates
    | otherwise        = getAllCoordinates
                          (newGen rule seedGen)
                          width
                          (succ column)
                          maxHeight
                          rule
                          (getCACoordsFromGen seedGen column width >< coordinates)


caWidth :: Int
caWidth = 240


caHeight :: Int
caHeight = 120


caRule :: Int
caRule = 11


path :: String
path = "/Users/joseph/Desktop/example/rule" ++ show caRule ++ ".pdf"


drawingCoordinates :: Int -> Seq (Int, Int)
drawingCoordinates rule = getAllCoordinates
                      (seedGeneration caWidth)
                      caWidth
                      0
                      caHeight
                      rule
                      S.empty


pdfRender :: Int -> ByteString
pdfRender rule =
  renderDrawingAtDpiToPDF caWidth caHeight 96 $
    withTexture (uniformTexture black) $ forM_(drawingCoordinates rule)(\x ->
      fill $ rectangle (V2 (fromIntegral $ fst x) (fromIntegral $ snd x)) 1 1)


pdfRenderMany = forM_ [1..254] (\rule -> do
  let render = pdfRender rule
  let pathToPdf = "~/Desktop/example/" ++ show rule ++ ".pdf"
  Prelude.writeFile pathToPdf $ unpack render
                               )

main :: IO ()
main =
  Prelude.writeFile path $ unpack $ pdfRender caRule


