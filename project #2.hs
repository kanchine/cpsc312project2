import System.Random
-- Define Pixel Data Type
-- Defines a data type Color for the RGB value of a pixel
-- Color can be either RGB or NoColor
data Color = RGB Double Double Double | NoColor deriving Show

red :: Color -> Double
red (RGB r _ _) = r

green :: Color -> Double
green (RGB _ g _) = g

blue :: Color -> Double
blue (RGB _ _ b) = b

-- Defines a data type of position of a pixel with the first value being the x coordinate an second value being the y coordinate in the picture
data Pos = Pos Int Int | NoPos deriving Show

x :: Pos -> Int
x (Pos x _) = x
y (Pos _ y) = y

-- Defines a pixel data type which contains color and position
data Pixel = Pixel Color Pos deriving Show

color :: Pixel -> Color
color (Pixel c _) = c
pos :: Pixel -> Pos
pos (Pixel _ p) = p

-- Generates a list of k Pixels by randomly selecting pixels from list of pixels x
initialize_k_means k x = [Pixel (RGB 0 0 0) (Pos 0 0)]

-- Computes the euclidean distance between two pixels
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
-- let a = Pixel (RGB 1 1 1) (Pos 0 0)
-- let b = Pixel (RGB 2 2 2) (Pos 0 0)
-- let c = Pixel (RGB 1 1 1) (Pos 0 0)
-- let d = Pixel (RGB 2 2 2) (Pos 0 0)
-- let k_means = [a,b]
-- let res = [[],[]]
-- cluster k_means [a,b,c,d] res

-- Returns a pixel that represent the sum of color values of two pixels
sum_pixel p1 p2 = Pixel (RGB ((red (color p1)) + (red (color p2))) ((green (color p1)) + (green (color p2))) ((blue (color p1)) + (blue (color p2)))) NoPos
-- Test
-- let a = Pixel (RGB 1 1 1) (Pos 0 0)
-- let b = Pixel (RGB 2 2 2) (Pos 0 0)
-- sum_pixel a b

-- Returns a pixel that represent the sum of color value in the list of pixels
get_sum l = foldl sum_pixel (Pixel (RGB 0 0 0) NoPos) l
-- Test:
-- let a = Pixel (RGB 1 1 1) (Pos 0 0)
-- let b = Pixel (RGB 2 2 2) (Pos 0 0)
-- let c = Pixel (RGB 3 3 3) (Pos 0 0) 
-- get_sum [a,b,c]

-- Returns a pixel that represent the average color value in the list of pixels
get_mean l = Pixel (RGB (r / n) (g / n) (b / n)) NoPos
    where
        p = get_sum l
        n = fromIntegral (length l)
        r = red (color p)
        g = green (color p)
        b = blue (color p)
-- Test:
-- let a = Pixel (RGB 1 1 1) (Pos 0 0)
-- let b = Pixel (RGB 2 2 2) (Pos 0 0)
-- let c = Pixel (RGB 3 3 3) (Pos 0 0) 
-- get_mean [a,b,c]

-- computes the mean of each cluster in a list of clusters
update_means [] = []
update_means (x:xs) = (get_mean x) : (update_means xs) 

-- let a = Pixel (RGB 1 1 1) (Pos 0 0)
-- let b = Pixel (RGB 2 2 2) (Pos 0 0)
-- let c = Pixel (RGB 3 3 3) (Pos 0 0) 
-- let d = Pixel (RGB 4 4 4) (Pos 0 0) 
-- update_means [[a,b],[c,d]]

-- Initil
-- Takes a list of pixels and the initial k means and assign each pixel to the cluster with the shortest euclidean distance
-- if the number of elements in each cluster changed, compute the mean and recursively call the fit function on the new k means
-- and the x until termination condiction is met.
-- fit k_means x cond | cond return


-- quantize list of clustered pixels
quantize = 0


