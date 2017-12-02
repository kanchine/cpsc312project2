# cpsc312project2
Support image types: jpg|png|tiff|bmp  

main fn:  
imageQuantization :: FilePath -> Int -> IO ()  
imageQuantization filePath k  

Inputs:  
:load "project #2.hs"  
imageQuantization "input/rabbit.tiff" 2  
imageQuantization "input/dog.png" 3  
imageQuantization "input/dolphin.bmp" 4  
imageQuantization "input/cat.jpg" 5  
-- gif is not supported, gives nice error msg  
imageQuantization "input/pig.gif" 3  
