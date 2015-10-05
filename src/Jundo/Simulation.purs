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

instance showRotationDirection :: Show RotationDirection where
  show Clockwise = "Clockwise"
  show Anticlockwise = "Anticlockwise"

newtype CubeState = CubeState {direction :: RotationDirection, angle :: Radians, position :: Vec3 Number}

instance showCubeState :: Show CubeState where
  show (CubeState s) = "CubeState {direction: " ++ show s.direction ++ ", angle: " ++ show s.angle ++ ", position: " ++ show s.position ++ "}"

-- | State of the camera. Yaw is anticlockwise rotation in radians about the positive y axis, pitch is anticlockwise rotation in radians about the 
-- | positive x axis after it has been transformed for the yaw. At zero pitch and zero yaw, the camera points in the negative z direction.
newtype CameraState = CameraState {yaw :: Radians, pitch :: Radians, position :: Vec3 Number}

instance showCameraState :: Show CameraState where
  show (CameraState s) = " CameraState {yaw: " ++ show s.yaw ++ ", pitch: " ++ show s.pitch ++ ", position: " ++ show s.position ++ "}"

newtype SimulationState = SimulationState {cube :: CubeState, camera :: CameraState}

instance showSimulationState :: Show SimulationState where
  show (SimulationState s) = "SimulationState {camera: " ++ show s.camera ++ ", cube: " ++ show s.cube ++ "}"

mapCameraState :: (CameraState -> CameraState) -> SimulationState -> SimulationState
mapCameraState f (SimulationState s) = SimulationState {cube: s.cube, camera: f s.camera}

mapCubeState :: (CubeState -> CubeState) -> SimulationState -> SimulationState
mapCubeState f (SimulationState s) = SimulationState {cube: f s.cube, camera: s.camera}

initialSimulationState :: SimulationState
initialSimulationState = SimulationState {
  cube: CubeState {direction: Anticlockwise, angle: 0.0, position: vec3 0.0 0.0 (-6.0)},
  camera: CameraState {pitch: 0.0, yaw: 0.0, position: vec3 0.0 0.0 0.0}
  }

-- | Type for tracking which keys are currently depressed
newtype KeyboardState = KeyboardState {w :: Boolean, a :: Boolean, s :: Boolean, d :: Boolean}

instance showKeyboardState :: Show KeyboardState where
  show (KeyboardState s) = "KeyboardState {w: " ++ show s.w ++ ", a: " ++ show s.a ++ ", s: " ++ show s.s ++ ", d: " ++ show s.d ++ "}"

-- | Rotation speed of the cube
angularSpeed :: RadiansPerSecond
angularSpeed = 1.0

-- | The rate equation!
applyRate :: Number -> Seconds -> Number
applyRate v (Seconds t) = v * t

-- | Camera movement rate
movementRate :: MetersPerSecond
movementRate = 0.1

-- | Convert the keyboard state into a unit vector in the direction of movement if the camera was pointing in the -z direction
cameraUnitVelocity :: KeyboardState -> Vec3 Number
cameraUnitVelocity (KeyboardState {w: true, a: false, d: false, s: false}) = vec3 0.0 0.0 (-1.0)
cameraUnitVelocity (KeyboardState {w: false, a: false, d: false, s: true}) = k3
cameraUnitVelocity (KeyboardState {w: false, a: true, d: false, s: false}) = vec3 (-1.0) 0.0 0.0
cameraUnitVelocity (KeyboardState {w: false, a: false, d: true, s: false}) = i3
cameraUnitVelocity (KeyboardState {w: true, a: true, d: false, s: false}) = vec3 (-sqrt1_2) 0.0 (-sqrt1_2)
cameraUnitVelocity (KeyboardState {w: true, a: false, d: true, s: false}) = vec3 sqrt1_2 0.0 (-sqrt1_2)
cameraUnitVelocity (KeyboardState {w: false, a: true, d: false, s: true}) = vec3 (-sqrt1_2) 0.0 sqrt1_2
cameraUnitVelocity (KeyboardState {w: false, a: false, d: true, s: true}) = vec3 sqrt1_2 0.0 sqrt1_2
cameraUnitVelocity (KeyboardState {w: true, a: true, d: true, s: false}) = vec3 0.0 0.0 (-1.0)
cameraUnitVelocity (KeyboardState {w: false, a: true, d: true, s: true}) = k3
cameraUnitVelocity (KeyboardState {w: true, a: true, d: false, s: true}) = vec3 (-1.0) 0.0 0.0
cameraUnitVelocity (KeyboardState {w: true, a: false, d: true, s: true}) = i3
cameraUnitVelocity _ = vec3 0.0 0.0 0.0

-- | Update the simulation state to reflect a change in simulation time, applying camera and cube movement
timestep :: Milliseconds -> KeyboardState -> SimulationState -> SimulationState
timestep step ks simulationState = mapCubeState updateCube $ mapCameraState updateCamera simulationState
  where
  stepSeconds :: Seconds
  stepSeconds = toSeconds step
  updateCube :: CubeState -> CubeState
  updateCube (CubeState cs) = CubeState {direction: cs.direction, position: cs.position, angle: cs.angle + angleChange cs.direction}
    where
    angleChange direction = directionMultiplier * applyRate angularSpeed stepSeconds
    directionMultiplier = case cs.direction of
      Anticlockwise -> 1.0
      Clockwise -> -1.0
  updateCamera :: CameraState -> CameraState
  updateCamera (CameraState cs) = CameraState {pitch: cs.pitch, yaw: cs.yaw, position: vAdd cs.position positionChange}
    where
    positionChange = scale (applyRate movementRate stepSeconds) $ rotateVec3 j3 cs.yaw (cameraUnitVelocity ks)

-- | Make the cube spin the other way! Excitement.
toggleDirection :: SimulationState -> SimulationState
toggleDirection = mapCubeState (\(CubeState s) -> CubeState {direction: newDirection s.direction, angle: s.angle, position: s.position})
  where
  newDirection d = case d of
    Clockwise -> Anticlockwise
    Anticlockwise -> Clockwise

-- | Type representing a change in mouse position (x y)
data MouseMove = MouseMove Number Number

instance showMouseMove :: Show MouseMove where
  show (MouseMove dx dy) = "MouseMove (" ++ show dx ++ ", " ++ show dy ++ ")"

-- | Radians of pitch change per unit mouse movement
pitchSensitivity :: Radians
pitchSensitivity = 0.01

-- | Radians of yaw change per unit mouse movement
yawSensitivity :: Radians
yawSensitivity = 0.01

-- | Apply a change in mouse position to the simulation state
applyMouseMove :: MouseMove -> SimulationState -> SimulationState
applyMouseMove (MouseMove dx dy) = mapCameraState \(CameraState cs) -> CameraState {
  pitch: ensurePitch $ cs.pitch + dy * pitchSensitivity,
  yaw: cs.yaw - dx * yawSensitivity,
  position: cs.position
  }
  where
  -- Pitch must be between -0.5 pi and 0.5 pi
  ensurePitch :: Radians -> Radians
  ensurePitch p | p < -0.5 * pi = -0.5 * pi
  ensurePitch p | p > 0.5 * pi = 0.5 * pi
  ensurePitch p = p
