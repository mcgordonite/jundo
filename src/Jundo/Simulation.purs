-- | Pure functions and types representing the state of the simulation
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
import Jundo.Units
import Jundo.Vectors
import Data.Time (Milliseconds(..), Seconds(..), toSeconds)
import Data.Vector
import Data.Vector3
import Math as Math

data RotationDirection = Clockwise | Anticlockwise

instance showRotationDirection :: Show RotationDirection where
  show Clockwise = "Clockwise"
  show Anticlockwise = "Anticlockwise"

-- | State of the cube. Direction is clockwise or anticlockwise rotation about the positive y axis. Angle is the angle
-- | of anticlockwise rotation about the positive y axis.
newtype CubeState = CubeState {direction :: RotationDirection, angle :: Radians, position :: Vec3 Metres}

instance showCubeState :: Show CubeState where
  show (CubeState s) = "CubeState {direction: " ++ show s.direction ++ ", angle: " ++ show s.angle ++ ", position: " ++ show s.position ++ "}"

-- | State of the camera. Yaw is anticlockwise rotation in radians about the positive y axis, pitch is anticlockwise rotation in radians about the 
-- | positive x axis after it has been transformed for the yaw. At zero pitch and zero yaw, the camera points in the negative z direction.
newtype CameraState = CameraState {yaw :: Radians, pitch :: Radians, position :: Vec3 Metres}

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
  cube: CubeState {direction: Anticlockwise, angle: zero, position: vec3 zero (Metres 1.5) (Metres (-6.0))},
  camera: CameraState {pitch: Radians 0.25, yaw: zero, position: vec3 zero zero zero}
  }

-- | Type for tracking which keys are currently depressed
newtype KeyboardState = KeyboardState {w :: Boolean, a :: Boolean, s :: Boolean, d :: Boolean}

instance showKeyboardState :: Show KeyboardState where
  show (KeyboardState s) = "KeyboardState {w: " ++ show s.w ++ ", a: " ++ show s.a ++ ", s: " ++ show s.s ++ ", d: " ++ show s.d ++ "}"

-- | Rotation speed of the cube
angularSpeed :: RadiansPerSecond
angularSpeed = RadiansPerSecond 0.5

-- | Camera movement rate
movementRate :: MetresPerSecond
movementRate = MetresPerSecond 1.0

-- | Convert the keyboard state into a unit vector in the direction of movement if the camera was pointing in the -z direction
cameraUnitVelocity :: KeyboardState -> Vec3 Number
cameraUnitVelocity (KeyboardState {w: true, a: false, d: false, s: false}) = vec3 0.0 0.0 (-1.0)
cameraUnitVelocity (KeyboardState {w: false, a: false, d: false, s: true}) = k3
cameraUnitVelocity (KeyboardState {w: false, a: true, d: false, s: false}) = vec3 (-1.0) 0.0 0.0
cameraUnitVelocity (KeyboardState {w: false, a: false, d: true, s: false}) = i3
cameraUnitVelocity (KeyboardState {w: true, a: true, d: false, s: false}) = vec3 (-Math.sqrt1_2) 0.0 (-Math.sqrt1_2)
cameraUnitVelocity (KeyboardState {w: true, a: false, d: true, s: false}) = vec3 Math.sqrt1_2 0.0 (-Math.sqrt1_2)
cameraUnitVelocity (KeyboardState {w: false, a: true, d: false, s: true}) = vec3 (-Math.sqrt1_2) 0.0 Math.sqrt1_2
cameraUnitVelocity (KeyboardState {w: false, a: false, d: true, s: true}) = vec3 Math.sqrt1_2 0.0 Math.sqrt1_2
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
    angleChange :: RotationDirection -> Radians
    angleChange direction = scaleMeasure directionMultiplier $ mulMeasure angularSpeed stepSeconds
    directionMultiplier :: Number
    directionMultiplier = case cs.direction of
      Anticlockwise -> 1.0
      Clockwise -> -1.0
  updateCamera :: CameraState -> CameraState
  updateCamera (CameraState cs) = CameraState {pitch: cs.pitch, yaw: cs.yaw, position: vAdd cs.position positionChange}
    where
    positionChange :: Vec3 Metres
    positionChange = vScaleMeasure (mulMeasure movementRate stepSeconds) $ rotateVec3 j3 cs.yaw (cameraUnitVelocity ks)

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
pitchSensitivity = Radians 0.01

-- | Radians of yaw change per unit mouse movement
yawSensitivity :: Radians
yawSensitivity = Radians 0.01

-- | Apply a change in mouse position to the simulation state
applyMouseMove :: MouseMove -> SimulationState -> SimulationState
applyMouseMove (MouseMove dx dy) = mapCameraState \(CameraState cs) -> CameraState {
  pitch: ensurePitch $ cs.pitch + scaleMeasure dy pitchSensitivity,
  yaw: cs.yaw - scaleMeasure dx yawSensitivity,
  position: cs.position
  }
  where
  -- Pitch must be between -0.5 pi and 0.5 pi
  ensurePitch :: Radians -> Radians
  ensurePitch (Radians p) | p < -0.5 * Math.pi = Radians (-0.5 * Math.pi)
  ensurePitch (Radians p) | p > 0.5 * Math.pi = Radians (0.5 * Math.pi)
  ensurePitch p = p
