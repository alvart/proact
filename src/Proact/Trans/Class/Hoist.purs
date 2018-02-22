{-
  @license MIT
  Hoist.purs
-}

module Proact.Trans.Class.Hoist
  ( class HoistT
  , hoist
  )
where

import Control.Comonad.Env (EnvT(..))
import Control.Comonad.Traced (TracedT(..))
import Control.Comonad.Store (StoreT(..))
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (StateT(..))
import Control.Monad.Writer (WriterT(..))
import Data.Bifunctor (lmap, rmap)
import Data.Newtype (over)
import Prelude

-- | Represents all transformers that support a natural transformation.
class HoistT t
  where
  -- | Given a homomorphism from `f` to `g` this yields a homomorphism from
  -- | `t f` to `t g`.
  hoist :: forall f g . Functor f => Functor g => (f ~> g) -> (t f ~> t g)

-- ReaderT :: HoistT
instance hoistTReaderT :: HoistT (ReaderT r)
  where
  hoist f = over ReaderT (map f)

-- WriterT :: HoistT
instance hoistTWriterT :: HoistT (WriterT w)
  where
  hoist f = over WriterT f

-- StateT :: HoistT
instance hoistTStateT :: HoistT (StateT s)
  where
  hoist f = over StateT (map f)

-- EnvT :: HoistT
instance hoistTEnvT :: HoistT (EnvT e)
  where
  hoist f = over EnvT (rmap f)

-- TracedT :: HoistT
instance hoistTTracedT :: HoistT (TracedT t)
  where
  hoist f = over TracedT f

-- StoreT :: HoistT
instance hoistTStoreT :: HoistT (StoreT s)
  where
  hoist f = over StoreT (lmap f)
