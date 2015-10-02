-- | Pure functions and types representing the (pretty simple) state of the simulation
module Jundo.Simulation (
  RotationDirection(..),
  SimulationState(),
  initialSimulationState,
  timestep,
  toggleDirection
  ) where

import Prelude
import Data.Time (Milliseconds(..), Seconds(..), toSeconds)
import Math.Radians

newtype RadiansPerSecond = RadiansPerSecond Number
data RotationDirection = Clockwise | Anticlockwise

-- | Type representing the state of the simulation: what angle the cube is at and in what direction it is rotating 
newtype SimulationState = SimulationState {direction :: RotationDirection, angle :: Radians}

initialSimulationState :: SimulationState
initialSimulationState = SimulationState {direction: Anticlockwise, angle: Radians 0.0}

angularSpeed :: RadiansPerSecond
angularSpeed = RadiansPerSecond 1.0

-- | Angle change is velocity multiplied by time
angleFromVelocity :: RadiansPerSecond -> Seconds -> Radians
angleFromVelocity (RadiansPerSecond v) (Seconds t) = Radians (v * t)

-- | Update the simulation state to reflect a change in simulation time
timestep :: Milliseconds -> SimulationState -> SimulationState
timestep step (SimulationState {direction: direction, angle: angle}) = SimulationState {direction: direction, angle: angle + angleChange}
  where
  angleChange :: Radians
  angleChange = Radians directionMultiplier * angleFromVelocity angularSpeed (toSeconds step)
  directionMultiplier :: Number
  directionMultiplier = case direction of
    Anticlockwise -> 1.0
    Clockwise -> -1.0

-- | Make the cube spin the other way! Excitement.
toggleDirection :: SimulationState -> SimulationState
toggleDirection (SimulationState {direction: direction, angle: a}) = SimulationState {direction: newDirection, angle: a}
  where
  newDirection = case direction of
    Clockwise -> Anticlockwise
    Anticlockwise -> Clockwise
