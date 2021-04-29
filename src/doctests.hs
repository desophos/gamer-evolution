import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Util.hs", "src/Evolution.hs", "src/Gamer.hs"]
