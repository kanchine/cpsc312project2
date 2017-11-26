-- Define Pixel Data Type
-- Defines a data type Color for the RGB value of a pixel
-- Color can be either RGB or NoColor
data Color = RGB Int Int Int | NoColor deriving Show

red :: Color -> Int
red (RGB r _ _) = r

green :: Color -> Int
green (RGB _ g _) = g

blue :: Color -> Int
blue (RGB _ _ b) = b

-- Defines a data type of position of a pixel with the first value being the x coordinate an second value being the y coordinate in the picture
data Pos = Pos Int Int deriving Show

x :: Pos -> Int
x (Pos x _) = x
y (Pos _ y) = y

-- Defines a pixel data type which contains color and position
data Pixel = Pixel Color Pos deriving Show

color :: Pixel -> Color
color (Pixel c _) = c
pos :: Pixel -> Pos
pos (Pixel _ p) = p