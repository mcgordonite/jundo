-- | Pure functions and types representing the (pretty simple) state of the simulation
module Jundo.Simulation (
  RotationDirection(..),
  SimulationState(..),
  CameraState(..),
  CubeState(..),
  MouseMove(..),
  initialSimulationState,
  applyMouseMove,
  timestep,
  toggleDirection
  ) where

import Prelude
import Data.Time (Milliseconds(..), Seconds(..), toSeconds)
import Data.Vector3
import Math

type RadiansPerSecond = Number

data RotationDirection = Clockwise | Anticlockwise
type CubeState = {direction :: RotationDirection, angle :: Radians, position :: Vec3 Number}
type CameraState = {yaw :: Radians, pitch :: Radians}
type SimulationState = {cube :: CubeState, camera :: CameraState}

mapCameraState :: (CameraState -> CameraState) -> SimulationState -> SimulationState
mapCameraState f st = {cube: st.cube, camera: f st.camera}

mapCubeState :: (CubeState -> CubeState) -> SimulationState -> SimulationState
mapCubeState f st = {cube: f st.cube, camera: st.camera}

initialSimulationState :: SimulationState
initialSimulationState = {
  cube: {direction: Anticlockwise, angle: 0.0, position: vec3 0.0 0.0 (-6.0)},
  camera: {pitch: 0.0, yaw: 0.0}
  }

angularSpeed :: RadiansPerSecond
angularSpeed = 1.0

-- | Angle change is velocity multiplied by time
angleFromVelocity :: RadiansPerSecond -> Seconds -> Radians
angleFromVelocity v (Seconds t) = v * t

-- | Update the simulation state to reflect a change in simulation time
timestep :: Milliseconds -> SimulationState -> SimulationState
timestep step = mapCubeState (\cs -> {direction: cs.direction, position: cs.position, angle: cs.angle + angleChange cs.direction})
  where
  angleChange :: RotationDirection -> Radians
  angleChange direction = directionMultiplier direction * angleFromVelocity angularSpeed (toSeconds step)
  directionMultiplier :: RotationDirection -> Number
  directionMultiplier direction = case direction of
    Anticlockwise -> 1.0
    Clockwise -> -1.0

-- | Make the cube spin the other way! Excitement.
toggleDirection :: SimulationState -> SimulationState
toggleDirection = mapCubeState (\cs -> {direction: newDirection cs.direction, angle: cs.angle, position: cs.position})
  where
  newDirection d = case d of
    Clockwise -> Anticlockwise
    Anticlockwise -> Clockwise

-- | Type representing a change in mouse position (x y)
data MouseMove = MouseMove Number Number

pitchSensitivity :: Radians
pitchSensitivity = 0.01

yawSensitivity :: Radians
yawSensitivity = 0.01

-- | Apply a change in mouse position to the simulation state
applyMouseMove :: MouseMove -> SimulationState -> SimulationState
applyMouseMove (MouseMove dx dy) = mapCameraState \cs -> {
  pitch: ensurePitch $ cs.pitch + dy * pitchSensitivity,
  yaw: cs.yaw + dx * yawSensitivity
  }
  where
  -- Pitch must be between -0.5 pi and 0.5 pi
  ensurePitch :: Radians -> Radians
  ensurePitch p | p < -0.5 * pi = -0.5 * pi
  ensurePitch p | p > 0.5 * pi = 0.5 * pi
  ensurePitch p = p
