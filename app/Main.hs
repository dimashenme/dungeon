import Dungeon.Game (runApplication)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= runApplication
