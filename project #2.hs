import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import Codec.Picture

import Debug.Trace as DT

-- Define Pixel2 Data Type
-- Defines a data type Color for the RGB value of a Pixel2
-- Color can be either RGB or NoColor
data Color = RGB Double Double Double | NoColor deriving Show

red :: Color -> Double
red (RGB r _ _) = r

green :: Color -> Double
green (RGB _ g _) = g

blue :: Color -> Double
blue (RGB _ _ b) = b

-- Defines a data type of position of a Pixel2 with the first value being the x coordinate an second value being the y coordinate in the picture
data Pos = Pos Int Int | NoPos deriving Show

x :: Pos -> Int
x (Pos x _) = x
y (Pos _ y) = y

-- Defines a Pixel2 data type which contains color and position
data Pixel2 = Pixel2 Color Pos deriving Show

color :: Pixel2 -> Color
color (Pixel2 c _) = c
pos :: Pixel2 -> Pos
pos (Pixel2 _ p) = p

-- Main function, produce compressed image
-- Input: filepath of image and value of k
-- Output compressed image
--imageQuantization :: FilePath -> Int -> IO ([[Pixel2]])
imageQuantization filePath k = do
    pixelList <- imageVectorization filePath
    let (w, g) = fit k pixelList
    let res = quantize w g
    --DT.trace (show w) (pure ()) --for debugging purpose
    return res


-- Read Image, convert image to list of Pixel2
imageVectorization :: FilePath -> IO [Pixel2]
imageVectorization filePath = do
  img <- readImage filePath
  case img of
    Left err -> return []
    Right i -> return (convertImage2Pixels (convertRGB8 i))

-- Helper function to create pixels, extracting the RGB value and position from each pixel
convertImage2Pixels :: Image PixelRGB8 -> [Pixel2]
convertImage2Pixels image@(Image w h _) = 
    [Pixel2 (getColorFromPosition image (x, y)) (Pos x y) | (x,y) <- (generatePixelPos w h)]

-- Helper funciton to obtain RGB value from each pixel
getColorFromPosition ::Image PixelRGB8 -> (Int, Int) -> Color
getColorFromPosition img (x, y) = getColor (pixelAt img x y)
    where 
        getColor:: PixelRGB8 -> Color
        getColor (PixelRGB8 r g b) = (RGB (fromIntegral r) (fromIntegral g) (fromIntegral b))

-- Generate all x, y position pairs of an image based on its width and height
generatePixelPos :: Int -> Int -> [(Int,Int)]
generatePixelPos width height = [(a,b) | a <- [0..width-1], b <- [0..height-1]]


-- Generates a list of k Pixel2s by randomly selecting Pixel2s from list of Pixel2s x
initialize_k_means :: Int -> [Pixel2] -> IO [Pixel2]
initialize_k_means k [] = return [] 
initialize_k_means 0 x = return [] 
initialize_k_means k x = 
    do 
        index <- randomRIO (0, (length x) - 1)
        rest <- initialize_k_means (k-1) (removeNth index x)  
        return ((x !! index) : rest) 

-- Test:
-- let a = Pixel2 (RGB 0 0 0) (Pos 0 0)
-- let b = Pixel2 (RGB 95 37 122) (Pos 12 4)
-- let c = Pixel2 (RGB 91 165 158) (Pos 3 2)
-- let d = Pixel2 (RGB 152 221 39) (Pos 14 10)
-- let e = Pixel2 (RGB 221 39 85) (Pos 1 2)
-- let f = Pixel2 (RGB 14 14 56) (Pos 4 6)
-- let x = [a, b, c, d, e, f]
-- initialize_k_means 0 x
-- Try the following multiple times and see that different lists are randomly generated each time:
-- initialize_k_means 1 x
-- initialize_k_means 6 x 


-- Helper to remove the nth element from a list
removeNth :: Int -> [a] -> [a]
removeNth n [] = []
removeNth n (x:xs) | n == 0 = xs | otherwise = x : removeNth (n-1) xs 

-- Test:
-- removeNth 1 "abc"
-- removeNth 2 [1,2,3]


-- Computes the euclidean distance between two Pixel2s
euclidean_distance_p2p p1 p2 = sqrt ((r1 - r2)^2 + (g1 - g2)^2 + (b1 - b2)^2)
    where
        r1 = red (color p1)
        r2 = red (color p2)
        g1 = green (color p1)
        g2 = green (color p2)
        b1 = blue (color p1)
        b2 = blue (color p2)

-- Computes the euclidean distance of a poin to a list of points and returns a list of euclidean distance
euclidean_distance_p2l p [] = []
euclidean_distance_p2l p (x:xs) = (euclidean_distance_p2p p x) : euclidean_distance_p2l p xs

-- Finds the index of the minimum value in the list
minIndex xs = head $ filter ((== minimum xs) . (xs !!)) [0..]
-- Test: minIndex [1,2,3], minIndex [3,1,1]

-- Finds the index of the minimum euclidean distance bwtween a point and a list of points
euclidean_distance_min_index p l = minIndex (euclidean_distance_p2l p l)

-- Append p to the ith element in l
insert_lol p i l = insert p i l 0
    where 
        insert p i (x:xs) n | i == n = (p:x):xs
                            | otherwise = x:(insert p i xs (n+1))
-- Test: insert_lol 1 1 [[1,2],[3,4]]

-- Creates a list of list of clusters such that each element the list has the shortest distance to the mean
-- res must be initialized to a list of empty list of size k (use replicate k [])
cluster k_means [] res = res
cluster k_means (x:xs) res = cluster k_means xs new_res
    where
        i = euclidean_distance_min_index x k_means
        new_res = insert_lol x i res
-- Test:    	
-- let a = Pixel2 (RGB 1 1 1) (Pos 0 0)
-- let b = Pixel2 (RGB 2 2 2) (Pos 0 0)
-- let c = Pixel2 (RGB 1 1 1) (Pos 0 0)
-- let d = Pixel2 (RGB 2 2 2) (Pos 0 0)
-- let k_means = [a,b]
-- let res = [[],[]]
-- cluster k_means [a,b,c,d] res

-- check if the number of elements in each cluster changes
compare_cluster [] [] = True
compare_cluster (x1:xs1) (x2:xs2) | x1 == length x2 = compare_cluster xs1 xs2
                                  | otherwise = False

-- Computes the number of elements in each cluster
cluster_size [] = []
cluster_size (x:xs) = (length x):(cluster_size xs)

-- Returns a Pixel2 that represent the sum of color values of two Pixel2s
sum_Pixel2 p1 p2 = Pixel2 (RGB ((red (color p1)) + (red (color p2))) ((green (color p1)) + (green (color p2))) ((blue (color p1)) + (blue (color p2)))) NoPos
-- Test
-- let a = Pixel2 (RGB 1 1 1) (Pos 0 0)
-- let b = Pixel2 (RGB 2 2 2) (Pos 0 0)
-- sum_Pixel2 a b

-- Returns a Pixel2 that represent the sum of color value in the list of Pixel2s
get_sum l = foldl sum_Pixel2 (Pixel2 (RGB 0 0 0) NoPos) l
-- Test:
-- let a = Pixel2 (RGB 1 1 1) (Pos 0 0)
-- let b = Pixel2 (RGB 2 2 2) (Pos 0 0)
-- let c = Pixel2 (RGB 3 3 3) (Pos 0 0) 
-- get_sum [a,b,c]

-- Returns a Pixel2 that represent the average color value in the list of Pixel2s
get_mean l = Pixel2 (RGB (r / n) (g / n) (b / n)) NoPos
    where
        p = get_sum l
        n = fromIntegral (length l)
        r = red (color p)
        g = green (color p)
        b = blue (color p)
-- Test:
-- let a = Pixel2 (RGB 1 1 1) (Pos 0 0)
-- let b = Pixel2 (RGB 2 2 2) (Pos 0 0)
-- let c = Pixel2 (RGB 3 3 3) (Pos 0 0) 
-- get_mean [a,b,c]

-- computes the mean of each cluster in a list of clusters
update_means [] = []
update_means (x:xs) = (get_mean x) : (update_means xs) 

-- let a = Pixel2 (RGB 1 1 1) (Pos 0 0)
-- let b = Pixel2 (RGB 2 2 2) (Pos 0 0)
-- let c = Pixel2 (RGB 3 3 3) (Pos 0 0) 
-- let d = Pixel2 (RGB 4 4 4) (Pos 0 0) 
-- update_means [[a,b],[c,d]]


-- Returns a tuple of the cluster and the means
-- 1. take k (number of clusters) as input
-- 2. take a list of Pixel2s as input
-- 3. initialize a list 1s of size k 
-- 4. initialize a list of starting means randomly using initialize_k_means k x
-- 5. cluster the list of Pixel2s with the initial means (or newly computed mean from the clustered list)
-- 6. compute the means of the clustered list
-- 7. cluster with the new mean
-- 8. if the new cluster changed repeat 5
-- 9. else terminate
fit :: Int -> [Pixel2] -> ([[Pixel2]], [Pixel2])
fit k x = fit_helper k x initial_means initial_y
    where
        initial_means = unsafePerformIO (initialize_k_means k x) 
        initial_y = replicate k 1
        fit_helper k x init_means initial_y | compare_cluster old_y cluster_old_means = (cluster_old_means, new_means)
                                            | otherwise = fit_helper k x new_means new_y
            where
                old_means = init_means
                old_y = initial_y
                res = replicate k []
                cluster_old_means = cluster old_means x res
                new_means = update_means cluster_old_means
                new_y = cluster_size cluster_old_means

-- given one cluster and the mean of that cluster, recreate the cluster with a list of Pixel2s that starts with the mean and the remaining being Pixel2s without the color, only the position
quantize_single [] _ = []
quantize_single (c:xc) m = (Pixel2 NoColor (Pos (x (pos c)) (y (pos c)))):(quantize_single xc m)

-- convert from Double to Int
toInt :: Double -> Int
toInt = truncate

-- quantize list of clustered Pixel2s
quantize [] [] = [] 
quantize (c:xc) (m:xm) = (m_mod:(quantize_single c m)):(quantize xc xm)
    where
        rgb = color m
        r = toInt (red rgb)
        g = toInt (green rgb)
        b = toInt (blue rgb)
        m_mod = Pixel2 (Color r g b) NoPos




