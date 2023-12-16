import Graphics.UI.GLUT
import Data.IORef

data Ball = Ball { position :: (GLfloat, GLfloat), velocity :: (GLfloat, GLfloat), radius :: GLfloat }

ball1 :: IORef Ball
ball1 = unsafePerformIO $ newIORef $ Ball (100, 100) (1, 1) 10

ball2 :: IORef Ball
ball2 = unsafePerformIO $ newIORef $ Ball (200, 200) (-1, -1) 10

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Ball Collision"
  displayCallback $= display
  idleCallback $= Just update
  mainLoop

display :: DisplayCallback
display = do
  clear [ColorBuffer]
  b1 <- get ball1
  b2 <- get ball2
  renderBall b1
  renderBall b2
  flush

renderBall :: Ball -> IO ()
renderBall (Ball (x, y) _ r) = preservingMatrix $ do
  translate $ Vector3 x y 0
  renderPrimitive Polygon $ mapM_ (\(x, y) -> vertex $ Vertex2 x y) (circlePoints r 10)

circlePoints :: GLfloat -> Int -> [(GLfloat, GLfloat)]
circlePoints r n = [ (r * cos (2 * pi * i / fromIntegral n), r * sin (2 * pi * i / fromIntegral n)) | i <- [0..n] ]

update :: IdleCallback
update = do
  modifyIORef ball1 (moveBall)
  modifyIORef ball2 (moveBall)
  checkCollision ball1 ball2
  postRedisplay Nothing

moveBall :: Ball -> Ball
moveBall (Ball (x, y) (vx, vy) r) = Ball (x + vx, y + vy) (vx, vy) r

-- Check and Handle Collision
checkCollision :: IORef Ball -> IORef Ball -> IO ()
checkCollision ballRef1 ballRef2 = do
  ball1 <- get ballRef1
  ball2 <- get ballRef2
  let Ball (x1, y1) _ r1 = ball1
      Ball (x2, y2) _ r2 = ball2
      dx = x1 - x2
      dy = y1 - y2
      dist = sqrt(dx*dx + dy*dy)
  when (dist < r1 + r2) $ do
    -- Adjust velocities for a simple elastic collision
    let Ball _ (vx1, vy1) _ = ball1
        Ball _ (vx2, vy2) _ = ball2
        newVx1 = vx2
        newVy1 = vy2
        newVx2 = vx1
        newVy2 = vy1
    writeIORef ballRef1 $ Ball (x1, y1) (newVx1, newVy1) r1
    writeIORef ballRef2 $ Ball (x2, y2) (newVx2, newVy2) r2
