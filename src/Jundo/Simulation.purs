-- | Pure functions and types representing the (pretty simple) state of the simulation
module Jundo.Simulation (
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
type CubeState = {direction :: RotationDirection, angle :: Radians}
type CameraState = {yaw :: Radians, pitch :: Radians}
type SimulationState = {cube :: CubeState, camera :: CameraState}

mapCamera :: (CameraState -> CameraState) -> SimulationState -> SimulationState
mapCamera f st = {cube: st.cube, camera: f st.camera}

mapCube :: (CubeState -> CubeState) -> SimulationState -> SimulationState
mapCube f st = {cube: f st.cube, camera: st.camera}

initialSimulationState :: SimulationState
initialSimulationState = {
  cube: {direction: Anticlockwise, angle: Radians 0.0},
  camera: {pitch: Radians 0.0, yaw: Radians 0.0}
  }

angularSpeed :: RadiansPerSecond
angularSpeed = RadiansPerSecond 1.0

-- | Angle change is velocity multiplied by time
angleFromVelocity :: RadiansPerSecond -> Seconds -> Radians
angleFromVelocity (RadiansPerSecond v) (Seconds t) = Radians (v * t)

-- | Update the simulation state to reflect a change in simulation time
timestep :: Milliseconds -> SimulationState -> SimulationState
timestep step = mapCube (\cs -> {direction: cs.direction, angle: cs.angle + angleChange cs.direction})
  where
  angleChange :: RotationDirection -> Radians
  angleChange direction = Radians (directionMultiplier direction) * angleFromVelocity angularSpeed (toSeconds step)
  directionMultiplier :: RotationDirection -> Number
  directionMultiplier direction = case direction of
    Anticlockwise -> 1.0
    Clockwise -> -1.0

-- | Make the cube spin the other way! Excitement.
toggleDirection :: SimulationState -> SimulationState
toggleDirection = mapCube (\cs -> {direction: newDirection cs.direction, angle: cs.angle})
  where
  newDirection d = case d of
    Clockwise -> Anticlockwise
    Anticlockwise -> Clockwise

data CameraAngleChange = CameraAngleChange Number Number
