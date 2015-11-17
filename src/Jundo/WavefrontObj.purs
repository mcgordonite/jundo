module Jundo.WavefrontObj where

import Prelude
import Global (isFinite, isNaN, readFloat)
import Data.Array ((..), index, foldM, length, replicate, snoc)
import Data.Either
import Data.Generic (Proxy(), anyProxy)
import Data.Int (fromString)
import Data.Map (Map(), empty, insert, lookup)
import Data.Maybe
import Data.String (joinWith)
import Data.String.Regex (Regex(), regex, match, noFlags, split)
import Data.Tuple (Tuple(..), fst)
import Data.Vector (fromArray, Vec())
import Data.Vector2
import Data.Vector3
import Data.TypeNat

-- | Representation of a parsed Wavefront obj file. The components can be passed to the WebGL bufferData method.
data Mesh = Mesh {elements :: Array Int, vertices :: Array Number, normals :: Array Number, uvs :: Array Number}

instance showMesh :: Show Mesh where
  show (Mesh mesh) = "Mesh {elements: " ++ show mesh.elements ++ ", vertices: " ++ show mesh.vertices ++ ", normals: " ++ show mesh.normals ++ ", uvs: " ++ show mesh.uvs ++ "}"

instance eqMesh :: Eq Mesh where
  eq (Mesh a) (Mesh b) = a.elements == b.elements && a.vertices == b.vertices && a.normals == b.normals && a.uvs == b.uvs

-- Type to track the state while parsing an OBJ file. The elements Map tracks normal, vertex and texture
-- coordinate combinations to a position in the Mesh elements array.
type ParseState = Tuple Mesh {
  elements :: Map String Int,
  vertices :: Array (Vec3 Number),
  normals :: Array (Vec3 Number),
  uvs :: Array (Vec2 Number)
  }

emptyMesh :: Mesh
emptyMesh = Mesh {elements: [], vertices: [], normals: [], uvs: []}

initialState :: ParseState
initialState = Tuple emptyMesh {elements: empty :: Map String Int, vertices: [], normals: [], uvs: []}

-- Return an error if the input is Nothing
validateJust :: forall a. String -> Maybe a -> Either String a
validateJust message input = case input of
  Just x -> Right x
  _ -> Left message

-- Match the regex against the input string, returning an array of the given number of matched capturing
-- groups or nothing if any of the groups was absent
applyRegexForArray :: Regex -> Int -> String -> Maybe (Array String)
applyRegexForArray regex n input = do
  maybesArray <- match regex input
  foldM (\a i -> maybeIndex maybesArray i >>= pure <<< snoc a) [] (1 .. n)

-- Get the value at the given index from an array of Maybe a, binding with id to un-nest the Maybes
maybeIndex :: forall a. Array (Maybe a) -> Int -> Maybe a
maybeIndex array i = index array i >>= id

-- Read a finite non-NaN float from the given string
parseFloat :: String -> Maybe Number
parseFloat input = if isNaN number || not (isFinite number)
  then Nothing
  else Just number
  where
    number :: Number
    number = readFloat input

-- Parse an array of strings into an array of a or nothing if a component could not be parsed
parseStringArray :: forall a. (String -> Maybe a) -> Array String -> Maybe (Array a)
parseStringArray parseItem = foldM (\a s -> parseItem s >>= pure <<< snoc a) []

-- Regex to match and process lines containing mesh data
dataLineRegex :: Regex
dataLineRegex = regex "^\\s*(v|vn|vt|f)\\s+(\\S.*)$" noFlags

-- Check that the given line is a data line and extract it's type and content
parseDataLine :: String -> Maybe (Tuple String String)
parseDataLine line = do
  array <- match dataLineRegex line
  Tuple <$> (maybeIndex array 1) <*> (maybeIndex array 2)

-- Parse a String of vector components (eg "14.2 -12 4.3") into a purescript vector
parseVector :: forall s. (Sized s) => String -> Maybe (Vec s Number)
parseVector input = do
  componentStrings <- applyRegexForArray (regex regexSource noFlags) size input
  components <- parseStringArray parseFloat componentStrings
  return $ fromArray components
  where
    size :: Int
    size = sized (anyProxy :: Proxy s)
    regexSource :: String
    regexSource = "^\\s*" ++ joinWith "\\s+" (replicate size "(-?\\d+(?:\\.\\d+)?)") ++ "\\s*$"

parseVertexData :: String -> ParseState -> Either String ParseState
parseVertexData input (Tuple mesh state) = do
  vector <- validateJust ("Failed to parse vertex vector from \"" ++ input ++ "\"") (parseVector input)
  return $ Tuple mesh {
    vertices: snoc state.vertices vector,
    elements: state.elements,
    normals: state.normals,
    uvs: state.uvs
    }

parseNormalData :: String -> ParseState -> Either String ParseState
parseNormalData input (Tuple mesh state) = do
  vector <- validateJust ("Failed to parse normal vector from \"" ++ input ++ "\"") (parseVector input)
  return $ Tuple mesh {
    vertices: state.vertices,
    elements: state.elements,
    normals: snoc state.normals vector,
    uvs: state.uvs
    }

parseUVData :: String -> ParseState -> Either String ParseState
parseUVData input (Tuple mesh state) = do
  vector <- validateJust ("Failed to parse uv coordinates from \"" ++ input ++ "\"") (parseVector input)
  return $ Tuple mesh {
    vertices: state.vertices,
    elements: state.elements,
    normals: state.normals,
    uvs: snoc state.uvs vector
    }

-- Regex to extract face data from the last part of a line in an OBJ file; eg: "1/5/3 2/4/14 46/1/32" from
-- the face line "f 1/5/3 2/4/14 46/1/32"
faceRegex :: Regex
faceRegex = regex "^\\s*(\\d+)/(\\d+)/(\\d+)\\s+(\\d+)/(\\d+)/(\\d+)\\s+(\\d+)/(\\d+)/(\\d+)\\s*$" noFlags

parseFaceData :: String -> ParseState -> Either String ParseState
parseFaceData input (Tuple mesh state) = do
  validateJust ("Failed to parse face data from \"" ++ input ++ "\"") do
    indexStrings <- applyRegexForArray faceRegex 9 input

    -- Read an array of indices of components in the state arrays
    indices <- parseStringArray fromString indexStrings

    -- Each face contains indices for three vertices
    foldM (parseFace indices) (Tuple mesh state) (0 .. 2)

  where
    elementsKey :: Vec3 Number -> Vec3 Number -> Vec2 Number -> String
    elementsKey vertex normal uv = show (get3X vertex) ++ " " ++ show (get3Y vertex) ++ " " ++ show (get3Z vertex)
      ++ " " ++ show (get3X normal) ++ " " ++ show (get3Y normal) ++ " " ++ show (get3Z normal)
      ++ " " ++ show (get2X uv) ++ " " ++ show (get2Y uv)
    parseFace :: Array Int -> ParseState -> Int -> Maybe ParseState
    parseFace indices (Tuple (Mesh mesh) state) n = do
      -- OBJ face indices are 1 based
      vertex <- index indices (3 * n) >>= (\i -> index state.vertices (i - 1))
      uv <- index indices (1 + 3 * n) >>= (\i -> index state.uvs (i - 1))
      normal <- index indices (2 + 3 * n) >>= (\i -> index state.normals (i - 1))
      key <- pure (elementsKey vertex normal uv)
      case lookup key state.elements of
        Just elementIndex -> do
          newMesh <- pure $ Mesh {
            elements: snoc mesh.elements elementIndex,
            vertices: mesh.vertices,
            uvs: mesh.uvs,
            normals: mesh.normals
            }
          return $ Tuple newMesh state
        Nothing -> do
          -- All mesh arrays should have the same length, choose the vertices array here to get the element array index
          elementIndex <- pure (length mesh.vertices / 3)
          newMesh <- pure $ Mesh {
            elements: snoc mesh.elements elementIndex,
            vertices: snoc (snoc (snoc mesh.vertices (get3X vertex)) (get3Y vertex)) (get3Z vertex),
            uvs: snoc (snoc mesh.uvs (get2X uv)) (get2Y uv),
            normals: snoc (snoc (snoc mesh.normals (get3X normal)) (get3Y normal)) (get3Z normal)
            }
          newState <- pure {
            elements: insert key elementIndex state.elements,
            vertices: state.vertices,
            uvs: state.uvs,
            normals: state.normals
            }
          return $ Tuple newMesh newState

parseLine :: ParseState -> String -> Either String ParseState
parseLine parseState line = case parseDataLine line of
  Nothing -> Right parseState
  Just (Tuple lineType lineData) -> case lineType of
    "v" -> parseVertexData lineData parseState
    "vn" -> parseNormalData lineData parseState
    "vt" -> parseUVData lineData parseState
    -- Otherwise the line must contain face data
    _ -> parseFaceData lineData parseState

-- Regex to match all new line character sequences
-- TODO: Include the \x85, \u2028 and \u2029 characters
newLine :: Regex
newLine = regex "\r\n|[\n\v\f\r]" noFlags

-- | Parse an obj file into a Mesh or an error String if the file is not valid
parseObj :: String -> Either String Mesh
parseObj input = map fst $ foldM parseLine initialState (split newLine input)
