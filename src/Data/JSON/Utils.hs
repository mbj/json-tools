module Data.JSON.Utils
  ( CategoryOptions
  , JSON.CustomJSON (..)
  , JsonOptions
  , invalidInput
  , jsonOptions
  , parseJSONDoubleWithConversion
  , parseJSONEnum
  , parseJSONFixed
  , parseJSONFixedPrecisionNatural
  , parseJSONIntBounded
  , parseJSONTextBoundedLength
  , parseJSONTextMaxLength
  , renameOptions
  , toJsonNumber
  )
where

import Data.Conversions
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Proxy (Proxy(..))
import Data.Scientific (Scientific)
import Data.Typeable (Typeable, typeOf)
import Data.Word (Word16)
import GHC.Float (Double, RealFloat)
import GHC.Num (Integer, fromInteger)
import MPrelude
import Prelude (error)

import qualified Data.Aeson       as JSON
import qualified Data.Aeson.Types as JSON
import qualified Data.List        as List
import qualified Data.Map.Strict  as Map
import qualified Data.Ratio       as Ratio
import qualified Data.Scientific  as Scientific
import qualified Data.Text        as Text
import qualified Deriving.Aeson   as JSON
import qualified GHC.Real         as Real

type DefaultOptionsList mod =
  '[ JSON.ConstructorTagModifier JSON.CamelToSnake
   , JSON.FieldLabelModifier mod
   , JSON.SumUntaggedValue
   , JSON.RejectUnknownFields
   , JSON.OmitNothingFields
   ]

type JsonOptions a     = JSON.CustomJSON (DefaultOptionsList JSON.CamelToSnake) a
type CamelToPeriod     = JSON.CamelTo "."
type CategoryOptions a = JSON.CustomJSON (DefaultOptionsList CamelToPeriod) a

jsonOptions :: JSON.Options
jsonOptions = JSON.defaultOptions
  { JSON.constructorTagModifier = JSON.camelTo2 '_'
  , JSON.fieldLabelModifier     = JSON.camelTo2 '_'
  , JSON.sumEncoding            = JSON.UntaggedValue
  , JSON.rejectUnknownFields    = True
  }

renameOptions :: Map String String -> JSON.Options
renameOptions renames =
  jsonOptions { JSON.fieldLabelModifier = rename }
  where
    rename :: String -> String
    rename input
      = fromMaybe (JSON.camelTo2 '_' input)
      $ Map.lookup input renames

parseJSONFixed
  :: forall a b c . (Eq a, Show a)
  => Text
  -> (String -> (a -> JSON.Parser b) -> c -> JSON.Parser b)
  -> (b -> a)
  -> [b]
  -> c
  -> JSON.Parser b
parseJSONFixed name withValue toKey list =
  withValue (convertText name) $ \key ->
    maybe (unexpected key) pure $ List.lookup key options
  where
    options :: [(a, b)]
    options = (\item -> (toKey item, item)) <$> list

    unexpected :: a -> JSON.Parser b
    unexpected value
      = fail
      . convert
      $ "Unexpected " <> convert name <> ": " <> show value

parseJSONEnum
  :: forall a b c . (Bounded b, Enum b, Eq a, Show a)
  => Text
  -> (String -> (a -> JSON.Parser b) -> c -> JSON.Parser b)
  -> (b -> a)
  -> c
  -> JSON.Parser b
parseJSONEnum name withValue map = parseJSONFixed name withValue map [minBound..]

parseJSONTextMaxLength
  :: String
  -> Natural
  -> JSON.Value
  -> JSON.Parser Text
parseJSONTextMaxLength field max = parseJSONTextBoundedLength field (0, max)

parseJSONTextBoundedLength
  :: String
  -> (Natural, Natural)
  -> JSON.Value
  -> JSON.Parser Text
parseJSONTextBoundedLength field (min, max) = JSON.withText field parseLength
  where
    parseLength text
      | length == 0  = failMessage "cannot be empty String"
      | length < min = failMessage $ "cannot have less than " <> show min <> " characters"
      | length > max = failMessage $ "cannot be longer than " <> show max <> " characters"
      | otherwise    = pure text
      where
        length :: Natural
        length = convertUnsafe $ Text.length text

        failMessage :: String -> JSON.Parser Text
        failMessage message = fail $ "parsing " <> field <> " failed, " <> message

parseJSONIntBounded
  :: String
  -> (Natural, Natural)
  -> JSON.Value
  -> JSON.Parser Natural
parseJSONIntBounded field range = parseJSONNumBounded field range converter
  where
    converter :: Scientific -> Maybe Natural
    converter = fmap convert . Scientific.toBoundedInteger @Word16

parseJSONNumBounded
  :: forall a . (Ord a, Show a, Typeable a)
  => String
  -> (a, a)
  -> (Scientific -> Maybe a)
  -> JSON.Value
  -> JSON.Parser a
parseJSONNumBounded field (min, max) converter = JSON.withScientific field parseNumber
  where
    parseNumber :: Scientific -> JSON.Parser a
    parseNumber scientific = do
     number <- maybe (failMessage typeError) pure $ converter scientific
     if | number < min -> failMessage $ "cannot be less than " <> show min
        | number > max -> failMessage $ "cannot be greater than " <> show max
        | otherwise    -> pure number
      where
        typeError :: String
        typeError = "Number is not a valid " <> typeName @a

        failMessage :: String -> JSON.Parser a
        failMessage message = fail $ "parsing " <> field <> " failed, " <> message

parseJSONDoubleWithConversion
  :: forall e a . (Show e, Conversion (Either e a) Double)
  => String
  -> JSON.Value
  -> JSON.Parser a
parseJSONDoubleWithConversion = parseJSONRealFloatWithConversion @Double @e @a

parseJSONRealFloatWithConversion
  :: forall float e a . (Show e, RealFloat float, Conversion (Either e a) float)
  => String
  -> JSON.Value
  -> JSON.Parser a
parseJSONRealFloatWithConversion field = JSON.withScientific field converter
  where
    converter :: Scientific -> JSON.Parser a
    converter = eitherFail . convertEither @_ @_ @e . (Scientific.toRealFloat @float)

toJsonNumber :: forall a. (Conversion Integer a) => a -> JSON.Value
toJsonNumber = JSON.Number . fromInteger . convert

invalidInput :: JSON.Value -> String -> JSON.Parser a
invalidInput value message = fail $ message <> ": " <> show value

-- Parse a JSON numeric into a scaled natural such as the original numeric can
-- only have had 2 decimal fractions.
parseJSONFixedPrecisionNatural :: String -> JSON.Value -> JSON.Parser Natural
parseJSONFixedPrecisionNatural name value = case value of
  (JSON.Number scientific) -> parseNatural scientific
  _other                   -> JSON.typeMismatch "Number" value
  where
    parseNatural :: Scientific -> JSON.Parser Natural
    parseNatural input
      | input < 0    = failMessage $ "Invalid negative " <> name
      | hasFractions = failMessage $ "Invalid fractional " <> name
      | otherwise    = pure $ convertUnsafe integer
      where
        cents :: Real.Ratio Integer
        cents = Real.toRational $ input * 100

        (integer :: Integer, fraction :: Ratio.Rational) = Real.properFraction cents

        hasFractions :: Bool
        hasFractions = fraction /= 0

        failMessage :: String -> JSON.Parser Natural
        failMessage = invalidInput value

eitherFail :: (MonadFail m, Show e) => Either e a -> m a
eitherFail = either (fail . show) pure

typeName :: forall (a :: Type) . Typeable a => String
typeName
  = fromMaybe (error "GHC error invalid type name")
  . List.stripPrefix "Proxy * "
  . show
  $ typeOf (Proxy @a)
