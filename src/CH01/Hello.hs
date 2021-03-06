module CH01.Hello (hello) where

import Graphics.UI.GLUT

display :: DisplayCallback
display = do
  -- clear all pixels
  clear [ColorBuffer]

  -- draw white polygon (rectangle) with corners at
  -- (0.25, 0.25, 0.0) and (0.75, 0.75, 0.0)
  color (Color3 1.0 1.0 (1.0 :: GLfloat))
  -- resolve overloading, not needed in "real" programs
  let vertex3f = vertex :: Vertex3 GLfloat -> IO ()
  renderPrimitive Polygon $
    mapM_
      vertex3f
      [ Vertex3 0.25 0.25 0.0,
        Vertex3 0.75 0.25 0.0,
        Vertex3 0.75 0.75 0.0,
        Vertex3 0.25 0.75 0.0
      ]

  -- don't wait!
  -- start processing buffered OpenGL routines
  flush

myInit :: IO ()
myInit = do
  -- select clearing color
  clearColor $= Color4 0 0 0 0

  -- initialize viewing values
  matrixMode $= Projection
  loadIdentity
  ortho 0 1 0 1 (-1) 1

hello :: IO ()
hello = do
  _ <- getArgsAndInitialize
  initialDisplayMode $= [SingleBuffered, RGBMode]
  initialWindowSize $= Size 250 250
  initialWindowPosition $= Position 100 100
  _ <- createWindow "hello"
  myInit
  displayCallback $= display
  mainLoop