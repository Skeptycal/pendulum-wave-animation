-- Shahar Elisha ackc233

module MyAnimation where

import Animation

-- PENDULUM WAVE

-- 1275 = n(n+1)/2 for n=50, which is equivalent to sum [50,49..0]

-- a are decremental/incremental numbers 50 to 0, or 0 to 50, depending on whether the function is for deceleration or acceleration

calculateTimeDeceleration :: Time -> Double -> Time
calculateTimeDeceleration time a = (time+((a+1)/1275.0))

calculateTimeAcceleration :: Time -> Double -> Time
calculateTimeAcceleration time a = (time+((a-1)/1275.0))

-- the following are four recursive methods, splitting an entire cycle into four parts: starting from the center (x = 250), it will decelerate
-- as it moves to the left (x = 200). It'll then accelerate back to maximum as it moves back right to the center (x = 250). Then it decelerate
-- again to the right (x = 300), before accelerating back left towards the center.

decelerationLeft :: Double -> Double -> Time -> Point -> [(Time, Point)] -> [(Time, Point)]
decelerationLeft _ _ _ (200, _) _ = []
decelerationLeft i a time (x,y) xs =  ((time*(2/i)), (x,y)):(decelerationLeft i (a+1) (calculateTimeDeceleration time a) ((x-1), y) xs)

accelerationRight :: Double -> Double -> Time -> Point -> [(Time, Point)] -> [(Time, Point)]
accelerationRight _ _ _ (250, _) _ = []
accelerationRight i a time (x,y) xs =  ((time*(2/i)), (x,y)):(accelerationRight i (a-1) (calculateTimeAcceleration time a) ((x+1), y) xs)

decelerationRight :: Double -> Double -> Time -> Point -> [(Time, Point)] -> [(Time, Point)]
decelerationRight _ _ _ (300, _) _ = []
decelerationRight i a time (x,y) xs =  ((time*(2/i)), (x,y)):(decelerationRight i (a+1) (calculateTimeDeceleration time a) ((x+1), y) xs)

accelerationLeft :: Double -> Double -> Time -> Point -> [(Time, Point)] -> [(Time, Point)]
accelerationLeft _ _ _ (250, _) _ = []
accelerationLeft i a time (x,y) xs =  ((time*(2/i)), (x,y)):(accelerationLeft i (a-1) (calculateTimeAcceleration time a) ((x-1), y) xs)

-- concatenates the four generated lists, which lists the times and positions for one complete cycle
completeCycle :: Double -> [(Time, Point)]
completeCycle i = (decelerationLeft i 0 0 (250,((20*i))) []) ++ (accelerationRight i 50 (1.0) (200,((20*i))) []) 
				  ++ (decelerationRight i 0 (2.0) (250,((20*i))) []) ++ (accelerationLeft i 50 (3.0) (300,((20*i))) [])

picture :: Animation
-- Final implementation with acceleration in order to create a pendulum wave with smooth waves
-- Background consists of a black rectangle, and three gradient circles to help the colors of the pendulum circles pop
picture = (withPaint (always black) (rect (always 600) (always 600)))
		  `plus`
		  (combine [(translate 
		  	(always ((250-(50*i)), (350+(50*i)))) (withPaint 
		  		(always (rgb 0 (g/255) (b/255))) 
		  		(circle (always (400-(i*100)))))) 
		  | (i, g, b) <- (zip3 [1..3] [2, 2, 1] [14, 11, 8])])
		  `plus`
		  (translate (always (400, -150)) 
		  	(rotate (always 45)
		  		(combine [(translate 
		  			(repeatSmooth (250,((20*i))) (completeCycle i)) (withPaint 
		  				-- changes from blue gradient to red gradient (i/37 because 35/35 would've been black)
		  				(cycleSmooth 10 [(hsl 200 1 (1 - (i/37))), (hsl 0.95 0.8 (1 - (i/37)))])
		  				(circle (always 10)))) 
		  		| i <- [1..35]])))

-- piece of magic
test :: IO ()
test = writeFile "Pendulum.svg" (svg 800 600 picture)