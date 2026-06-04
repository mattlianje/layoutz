{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Layoutz
import Data.List (intercalate)
import Text.Printf (printf)

-- | 3D vector
data V3 = V3 !Double !Double !Double

vzero, vup :: V3
vzero = V3 0 0 0
vup   = V3 0 1 0

vadd, vsub, vcross :: V3 -> V3 -> V3
vadd (V3 ax ay az) (V3 bx by bz) = V3 (ax + bx) (ay + by) (az + bz)
vsub (V3 ax ay az) (V3 bx by bz) = V3 (ax - bx) (ay - by) (az - bz)
vcross (V3 ax ay az) (V3 bx by bz) =
  V3 (ay * bz - az * by) (az * bx - ax * bz) (ax * by - ay * bx)

vscale :: V3 -> Double -> V3
vscale (V3 x y z) s = V3 (x * s) (y * s) (z * s)

vdot :: V3 -> V3 -> Double
vdot (V3 ax ay az) (V3 bx by bz) = ax * bx + ay * by + az * bz

vlen :: V3 -> Double
vlen v = sqrt (vdot v v)

vneg :: V3 -> V3
vneg (V3 x y z) = V3 (-x) (-y) (-z)

vnorm :: V3 -> V3
vnorm v =
  let l = vlen v
  in if l < 1e-10 then vzero else vscale v (1.0 / l)

-- | One screen cell with character + 24-bit color
data Pixel = Pixel !Char !Int !Int !Int

-- | Render a list of pixels into a single ANSI string of width @w@
renderFrameBuffer :: [Pixel] -> Int -> String
renderFrameBuffer pixels w = intercalate "\n" (rowsOf pixels)
  where
    rowsOf [] = []
    rowsOf ps =
      let (rowPixels, rest) = splitAt w ps
      in renderRow rowPixels : rowsOf rest
    renderRow ps = concatMap pixelToAnsi ps ++ "\ESC[0m"
    pixelToAnsi (Pixel ch r g b) =
      "\ESC[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m" ++ [ch]

data RayState = RayState
  { rsTheta       :: !Double
  , rsPhi         :: !Double
  , rsDist        :: !Double
  , rsMorph       :: !Double
  , rsMorphTarget :: !Double
  , rsAutoRotate  :: !Bool
  , rsTick        :: !Int
  }

initialState :: RayState
initialState = RayState
  { rsTheta = 0.0
  , rsPhi = 0.35
  , rsDist = 3.8
  , rsMorph = 0.0
  , rsMorphTarget = 0.0
  , rsAutoRotate = True
  , rsTick = 0
  }

data RayMsg
  = RTick
  | RotL | RotR | RotU | RotD
  | ZoomIn | ZoomOut
  | ToggleAuto
  | NextShape
  deriving (Show, Eq)

-- Render config
fbW, fbH :: Int
fbW = 60
fbH = 28

maxSteps :: Int
maxSteps = 50

maxDist, eps, morphSpeed :: Double
maxDist     = 20.0
eps         = 0.005
morphSpeed  = 0.06

ramp :: String
ramp = " .'`^\",:;Il!i><~+_-?][}{1)(|/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$"

shapeNames :: [String]
shapeNames = ["sphere", "torus", "cube"]

clampD :: Double -> Double -> Double -> Double
clampD lo hi x = max lo (min hi x)

mixD :: Double -> Double -> Double -> Double
mixD a b t = a * (1.0 - t) + b * t

smoothstep :: Double -> Double -> Double -> Double
smoothstep lo hi x =
  let t = clampD 0.0 1.0 ((x - lo) / (hi - lo))
  in t * t * (3.0 - 2.0 * t)

-- Signed-distance functions
sdSphere :: V3 -> Double -> Double
sdSphere p r = vlen p - r

sdTorus :: V3 -> Double -> Double -> Double
sdTorus (V3 x y z) bigR r =
  let qx = sqrt (x * x + z * z) - bigR
  in sqrt (qx * qx + y * y) - r

sdRoundBox :: V3 -> V3 -> Double -> Double
sdRoundBox (V3 px py pz) (V3 bx by bz) r =
  let qx = abs px - bx
      qy = abs py - by
      qz = abs pz - bz
      outer = vlen (V3 (max qx 0) (max qy 0) (max qz 0))
      inner = min (max qx (max qy qz)) 0.0
  in outer + inner - r

scene :: V3 -> Double -> Double
scene p morph =
  let t      = let m = morph - fromIntegral (3 * floor (morph / 3.0) :: Int)
               in if m < 0 then m + 3 else m
      sph    = sdSphere p 1.05
      tor    = sdTorus p 0.9 0.38
      cub    = sdRoundBox p (V3 0.72 0.72 0.72) 0.12
  in if t < 1.0      then mixD sph tor (smoothstep 0 1 t)
     else if t < 2.0 then mixD tor cub (smoothstep 0 1 (t - 1))
     else                 mixD cub sph (smoothstep 0 1 (t - 2))

calcNormal :: V3 -> Double -> V3
calcNormal (V3 x y z) morph =
  let e = 0.001
  in vnorm $ V3
       (scene (V3 (x + e) y z) morph - scene (V3 (x - e) y z) morph)
       (scene (V3 x (y + e) z) morph - scene (V3 x (y - e) z) morph)
       (scene (V3 x y (z + e)) morph - scene (V3 x y (z - e)) morph)

march :: V3 -> V3 -> Double -> Double
march ro rd morph = go 0.0 0
  where
    go !t !i
      | i >= maxSteps || t >= maxDist = -1.0
      | otherwise =
          let d = scene (vadd ro (vscale rd t)) morph
          in if d < eps then t else go (t + d) (i + 1)

lightDir :: V3
lightDir = vnorm (V3 0.8 1.0 (-0.6))

bgPixel :: V3 -> Pixel
bgPixel (V3 _ y _) =
  let vy = y * 0.5 + 0.5
      bg = max 4 (floor (18.0 - (1.0 - vy) * 8.0) :: Int)
  in Pixel ' ' bg bg (bg + 6)

shade :: V3 -> V3 -> Double -> Pixel
shade ro rd morph =
  let t = march ro rd morph
  in if t < 0
       then bgPixel rd
       else
         let hit  = vadd ro (vscale rd t)
             n    = calcNormal hit morph
             diff = max (vdot n lightDir) 0.0
             refl = vsub (vscale n (2.0 * vdot n lightDir)) lightDir
             spec = (max (vdot refl (vneg rd)) 0.0) ** 32.0 * 0.6
             ao   = 1.0 - clampD 0.0 0.4 (scene (vadd hit (vscale n 0.1)) morph * 5.0)
             lum  = clampD 0.0 1.0 ((0.08 + diff * 0.85 + spec) * ao)
             rampLen = length ramp
             idx = floor (clampD 0 (fromIntegral rampLen - 1) (lum * fromIntegral (rampLen - 1))) :: Int
             ch  = ramp !! idx
             V3 nx ny nz = n
             nx' = nx * 0.5 + 0.5
             ny' = ny * 0.5 + 0.5
             nz' = nz * 0.5 + 0.5
             rC  = floor (clampD 0 255 ((nx' * 0.55 + lum * 0.45) * 235 + 20)) :: Int
             gC  = floor (clampD 0 255 ((ny' * 0.45 + lum * 0.55) * 215 + 15)) :: Int
             bC  = floor (clampD 0 255 ((nz' * 0.50 + lum * 0.50 + 0.05) * 200 + 30)) :: Int
         in Pixel ch rC gC bC

renderFrame :: RayState -> L
renderFrame s =
  let theta = rsTheta s
      phi   = rsPhi s
      dist  = rsDist s
      morph = rsMorph s
      ro    = V3 (dist * sin theta * cos phi)
                 (dist * sin phi)
                 (dist * cos theta * cos phi)
      fwd   = vnorm (vneg ro)
      right = vnorm (vcross fwd vup)
      up    = vcross right fwd
      aspect = fromIntegral fbW / fromIntegral fbH * 0.48
      pixels = [ shade ro rd morph
               | py <- [0 .. fbH - 1]
               , let v = 0.5 - fromIntegral py / fromIntegral fbH
               , px <- [0 .. fbW - 1]
               , let u = (fromIntegral px / fromIntegral fbW - 0.5) * aspect
               , let rd = vnorm (vadd (vadd fwd (vscale right u)) (vscale up v))
               ]
  in text (renderFrameBuffer pixels fbW)

-- | Update state in response to a message
updateRay :: RayMsg -> RayState -> RayState
updateRay msg s = case msg of
  RTick ->
    let newTheta = if rsAutoRotate s then rsTheta s + 0.035 else rsTheta s
        diff     = rsMorphTarget s - rsMorph s
        newMorph = if abs diff < 0.01
                     then rsMorphTarget s
                     else rsMorph s + diff * morphSpeed
    in s { rsTheta = newTheta, rsMorph = newMorph, rsTick = rsTick s + 1 }
  RotL  -> s { rsTheta = rsTheta s - 0.15, rsAutoRotate = False }
  RotR  -> s { rsTheta = rsTheta s + 0.15, rsAutoRotate = False }
  RotU  -> s { rsPhi   = min (rsPhi s + 0.1)  1.3 }
  RotD  -> s { rsPhi   = max (rsPhi s - 0.1) (-1.3) }
  ZoomIn  -> s { rsDist = max (rsDist s - 0.25) 2.0 }
  ZoomOut -> s { rsDist = min (rsDist s + 0.25) 8.0 }
  ToggleAuto -> s { rsAutoRotate = not (rsAutoRotate s) }
  NextShape  ->
    let next = (round (rsMorphTarget s) + 1) `mod` 3 :: Int
    in s { rsMorphTarget = fromIntegral next }

-- | Build the visible UI for one frame
viewRay :: RayState -> L
viewRay s =
  let fb        = renderFrame s
      targetIdx = (round (rsMorphTarget s) `mod` 3 + 3) `mod` 3
      settled   = abs (rsMorph s - rsMorphTarget s) < 0.02
      shapeName =
        if settled
          then shapeNames !! targetIdx
          else
            let fromIdx = (round (rsMorph s) `mod` 3 + 3) `mod` 3
            in (shapeNames !! fromIdx) ++ " -> " ++ (shapeNames !! targetIdx)

      sparkData =
        [ 8.0 + 4.0 * sin (fromIntegral (rsTick s + i) * 0.4)
        | i <- [0 .. 29 :: Int]
        ]

      twoPi = 2 * pi :: Double

      cameraStats = withColor ColorBrightBlue $ kv
        [ ("th",   printf "%.2f" (rsTheta s - twoPi * fromIntegral (floor (rsTheta s / twoPi) :: Int)))
        , ("ph",   printf "%.2f" (rsPhi s))
        , ("zoom", printf "%.1f" (rsDist s))
        ]

      rotateLine = tightRow
        [ withColor ColorBrightBlack (text "rotate: ")
        , if rsAutoRotate s
            then withStyle StyleBold (withColor ColorBrightGreen (text "auto"))
            else withColor ColorBrightYellow (text "manual")
        ]

      controls = withStyle StyleDim $ layout
        [ withColor ColorBrightYellow (text "<-^v>  orbit")
        , withColor ColorBrightYellow (text "+/-    zoom")
        , withColor ColorBrightYellow (text "m      shape")
        , withColor ColorBrightYellow (text "a      auto")
        ]

      camera = layout
        [ cameraStats
        , br
        , rotateLine
        , br
        , withColor ColorBrightYellow (text "~12 fps")
        , withColor ColorBrightCyan (plotSparkline sparkData)
        , br
        , withColor ColorBrightMagenta (kv [("shape", shapeName)])
        , br
        , withColor ColorBrightCyan  (spinner "render" (rsTick s `div` 2) SpinnerDots)
        , withColor ColorBrightGreen (spinner "scene"  (rsTick s `div` 3) SpinnerClock)
        , withColor ColorBrightYellow(spinner "light"  (rsTick s `div` 2) SpinnerBounce)
        , br
        , controls
        ]

      cameraBox = withColor ColorBrightMagenta $ setBorder BorderRound $ box "Camera" [camera]

      title = withStyle StyleBold (withColor ColorBrightCyan (text "Ray Marcher"))
  in row [layout [title, fb], cameraBox]

rayMarcher :: LayoutzApp RayState RayMsg
rayMarcher = LayoutzApp
  { appInit = (initialState, CmdNone)
  , appUpdate = \msg state -> (updateRay msg state, CmdNone)
  , appSubscriptions = \_ -> subBatch
      [ subEveryMs 80 RTick
      , subKeyPress $ \case
          KeyLeft      -> Just RotL
          KeyRight     -> Just RotR
          KeyUp        -> Just RotU
          KeyDown      -> Just RotD
          KeyChar '+'  -> Just ZoomIn
          KeyChar '='  -> Just ZoomIn
          KeyChar '-'  -> Just ZoomOut
          KeyChar 'a'  -> Just ToggleAuto
          KeyChar 'A'  -> Just ToggleAuto
          KeyChar 'm'  -> Just NextShape
          KeyChar 'M'  -> Just NextShape
          _            -> Nothing
      ]
  , appView = viewRay
  }

main :: IO ()
main = runApp rayMarcher
