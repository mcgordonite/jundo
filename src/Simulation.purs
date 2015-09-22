module Simulation (
	RotationDirection(..),
	SimulationState(..),
	initialSimulationState,
	timestep,
	toggleDirection
	) where

import Prelude
import Data.Time (Milliseconds(..), Seconds(..), toSeconds)
import Math.Radians

newtype RadiansPerSecond = RadiansPerSecond Number
data RotationDirection = Clockwise | Anticlockwise

type SimulationState = {direction :: RotationDirection, angle :: Radians}

initialSimulationState :: SimulationState
initialSimulationState = {direction: Anticlockwise, angle: Radians 0.0}

angularSpeed :: RadiansPerSecond
angularSpeed = RadiansPerSecond 1.0

angleFromVelocity :: RadiansPerSecond -> Seconds -> Radians
angleFromVelocity (RadiansPerSecond v) (Seconds t) = Radians (v * t)

timestep :: Milliseconds -> SimulationState -> SimulationState
timestep step {direction: direction, angle: angle} = {direction: direction, angle: angle + angleChange}
	where
	angleChange :: Radians
	angleChange = Radians directionMultiplier * angleFromVelocity angularSpeed (toSeconds step)
	directionMultiplier :: Number
	directionMultiplier = case direction of
		Anticlockwise -> 1.0
		Clockwise -> -1.0

toggleDirection :: SimulationState -> SimulationState
toggleDirection {direction: direction, angle: a} = {direction: newDirection, angle: a}
	where
	newDirection = case direction of
		Clockwise -> Anticlockwise
		Anticlockwise -> Clockwise
