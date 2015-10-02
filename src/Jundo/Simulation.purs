-- | Pure functions and types representing the (pretty simple) state of the simulation
module Jundo.Simulation (
  RotationDirection(..),
  SimulationState(..),
  CameraState(..),
  CubeState(..),
  KeyboardState(..),
  MouseMove(..),
  initialSimulationState,
  applyMouseMove,
  timestep,
  toggleDirection
  ) where

import Prelude
import Jundo.Vectors
import Data.Time (Milliseconds(..), Seconds(..), toSeconds)
import Data.Vector
import Data.Vector3
import Math

type RadiansPerSecond = Number
type MetersPerSecond = Number

data RotationDirection = Clockwise | Anticlockwise
type CubeState = {direction :: RotationDirection, angle :: Radians, position :: Vec3 Number}
type CameraState = {yaw :: Radians, pitch :: Radians, position :: Vec3 Number}
type SimulationState = {cube :: CubeState, camera :: CameraState}

mapCameraState :: (CameraState -> CameraState) -> SimulationState -> SimulationState
mapCameraState f st = {cube: st.cube, camera: f st.camera}

mapCubeState :: (CubeState -> CubeState) -> SimulationState -> SimulationState
mapCubeState f st = {cube: f st.cube, camera: st.camera}

initialSimulationState :: SimulationState
initialSimulationState = {
  cube: {direction: Anticlockwise, angle: 0.0, position: vec3 0.0 0.0 (-6.0)},
  camera: {pitch: 0.0, yaw: 0.0, position: vec3 0.0 0.0 0.0}
  }

-- | Type for tracking which keys are currently depressed
type KeyboardState = {w :: Boolean, a :: Boolean, s :: Boolean, d :: Boolean}

-- | Rotation speed of the cube
angularSpeed :: RadiansPerSecond
angularSpeed = 1.0

-- | Angle change is velocity multiplied by time
angleFromVelocity :: RadiansPerSecond -> Seconds -> Radians
angleFromVelocity v (Seconds t) = v * t

-- | Camera movement rate
movementRate :: MetersPerSecond
movementRate = 0.1

-- | Convert the keyboard state into a unit vector in the direction of movement if the camera was pointing in the -z direction
cameraUnitVelocity :: KeyboardState -> Vec3 Number
cameraUnitVelocity {w: true, a: false, d: false, s: false} = scale (-1.0) k3
cameraUnitVelocity {w: false, a: false, d: false, s: true} = k3
cameraUnitVelocity {w: false, a: true, d: false, s: false} = scale (-1.0) i3
cameraUnitVelocity {w: false, a: false, d: true, s: false} = i3
cameraUnitVelocity {w: true, a: true, d: false, s: false} = vec3 (-sqrt1_2) 0.0 (-sqrt1_2)
cameraUnitVelocity {w: true, a: false, d: true, s: false} = vec3 sqrt1_2 0.0 (-sqrt1_2)
cameraUnitVelocity {w: false, a: true, d: false, s: true} = vec3 (-sqrt1_2) 0.0 sqrt1_2
cameraUnitVelocity {w: false, a: false, d: true, s: true} = vec3 sqrt1_2 0.0 sqrt1_2
cameraUnitVelocity _ = vec3 0.0 0.0 0.0

-- | Update the simulation state to reflect a change in simulation time, applying camera and cube movement
timestep :: Milliseconds -> KeyboardState -> SimulationState -> SimulationState
timestep step ks {cube: cube, camera: camera} = {
  cube: {direction: cube.direction, position: cube.position, angle: cube.angle + angleChange cube.direction},
  camera: {pitch: camera.pitch, yaw: camera.yaw, position: vAdd camera.position positionChange}
  }
  where
  stepSeconds = toSeconds step
  angleChange direction = directionMultiplier direction * angleFromVelocity angularSpeed stepSeconds
  directionMultiplier direction = case direction of
    Anticlockwise -> 1.0
    Clockwise -> -1.0
  positionChange = scale movementRate $ rotateVec3 j3 camera.yaw (cameraUnitVelocity ks)

-- | Make the cube spin the other way! Excitement.
toggleDirection :: SimulationState -> SimulationState
toggleDirection = mapCubeState (\cs -> {direction: newDirection cs.direction, angle: cs.angle, position: cs.position})
  where
  newDirection d = case d of
    Clockwise -> Anticlockwise
    Anticlockwise -> Clockwise

-- | Type representing a change in mouse position (x y)
data MouseMove = MouseMove Number Number

-- | Radians of pitch change per unit mouse movement
pitchSensitivity :: Radians
pitchSensitivity = 0.01

-- | Radians of yaw change per unit mouse movement
yawSensitivity :: Radians
yawSensitivity = 0.01

-- | Apply a change in mouse position to the simulation state
applyMouseMove :: MouseMove -> SimulationState -> SimulationState
applyMouseMove (MouseMove dx dy) = mapCameraState \cs -> {
  pitch: ensurePitch $ cs.pitch + dy * pitchSensitivity,
  yaw: cs.yaw + dx * yawSensitivity,
  position: cs.position
  }
  where
  -- Pitch must be between -0.5 pi and 0.5 pi
  ensurePitch :: Radians -> Radians
  ensurePitch p | p < -0.5 * pi = -0.5 * pi
  ensurePitch p | p > 0.5 * pi = 0.5 * pi
  ensurePitch p = p
