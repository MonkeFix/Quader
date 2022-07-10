{-# LANGUAGE NoImplicitPrelude #-}
module Types.Aeson where

import Universum
import Data.Row
import Data.Aeson
import Data.Row.Records qualified as Rec
import Data.Row.Variants qualified as Var
import Data.Text qualified as T
import Data.Aeson.Types (typeMismatch)
import Data.Aeson.Encoding (pairStr)
import Data.Aeson.KeyMap (fromHashMapText)

newtype Bad r = Bad { unBad :: Var r }

instance Forall r ToJSON => ToJSON (Rec r) where
  toJSON = Object . fromHashMapText . Rec.eraseToHashMap @ToJSON toJSON

  toEncoding =
    pairs . foldMap (uncurry pairStr) . Rec.eraseWithLabels @ToJSON toEncoding

instance (AllUniqueLabels r, Forall r FromJSON) => FromJSON (Rec r) where
  parseJSON (Object o) = do
    r <- Rec.fromLabelsA @FromJSON $ \ l -> do x <- o .: fromString (show l)
                                               x `seq` pure x
    r `seq` pure r

  parseJSON v = typeMismatch msg v
    where msg = "REC: {" ++ intercalate "," (labels @r @FromJSON) ++ "}"

instance Forall r ToJSON => ToJSON (Bad r) where
  toJSON (Bad v) = object [foo l]
    where (l, foo) = Var.eraseWithLabels @ToJSON (flip (.=)) v

instance Forall r ToJSON => ToJSON (Var r) where
  toJSON v = Var.erase @ToJSON toJSON v

instance (AllUniqueLabels r, Forall r FromJSON) => FromJSON (Var r) where
  parseJSON v = Var.fromLabels @FromJSON $ \ l -> parseJSON v

instance (AllUniqueLabels r, Forall r FromJSON) => FromJSON (Bad r) where
  parseJSON (Object o) = Bad <$> Var.fromLabels @FromJSON (\ l -> o .: fromString (show l))
  parseJSON v = typeMismatch msg v
     where msg = "VAR: {" ++ intercalate "," (labels @r @FromJSON) ++ "}"
