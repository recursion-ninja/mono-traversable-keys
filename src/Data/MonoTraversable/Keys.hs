-- |
-- Type-classes mirroring type-classes from 'Data.Key', but working with
-- monomorphic containers.
--
-- The motivation is that some commonly used data types (i.e., 'ByteString' and
-- 'Text') do not allow for instances of type-classes like 'Keyed', 'Indexable',
-- and 'FoldableWithKey', since they are monomorphic structures. This module
-- allows both monomorphic and polymorphic data types to be instances of the
-- same type-classes.
--
-- All of the laws for the polymorphic type-classes apply to their monomorphic
-- cousins.
--
-- Note that all type-classes have been prefixed with @Mono@, and functions have
-- been prefixed with @o@. The mnemonic is inherited from 'Data.MonoTraversable'.

{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}

module Data.MonoTraversable.Keys
  ( MonoKey
  -- * Keyed Monomorphic Structures
  , MonoKeyed(..)
  , MonoFoldableWithKey(..)
  , MonoTraversableWithKey(..)
  -- * Adjustable Monomorphic Structures
  , MonoAdjustable(..)
  -- * Zippable Monomorphic Structures
  , MonoZip(..)
  , MonoZipWithKey(..)
  -- * Monomorphic Indexing / Querries
  , MonoIndexable(..)
  , MonoLookup(..)
  -- * Monomorphic unwrapping with key
  , ofoldlWithKeyUnwrap
  , ofoldWithKeyMUnwrap
  ) where

import           Control.Applicative
import           Control.Arrow                            (Arrow)
import           Control.Comonad.Cofree                   (Cofree(..))
import           Control.Monad                            (Monad (..))
import           Control.Monad.Free
import           Control.Monad.Trans.Cont                 (ContT)
import           Control.Monad.Trans.Identity             (IdentityT)
import           Control.Monad.Trans.List                 (ListT(..))
import           Control.Monad.Trans.Maybe                (MaybeT(..))
import           Control.Monad.Trans.Reader               (ReaderT)
import           Control.Monad.Trans.RWS                  (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict    as S   (RWST(..))
import           Control.Monad.Trans.State                (StateT(..))
import qualified Control.Monad.Trans.State.Strict  as S   (StateT(..), evalState, get, modify)
import           Control.Monad.Trans.Writer               (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as S   (WriterT)
import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Lazy              as BSL
import           Data.Foldable                            (Foldable)
import           Data.Functor.Compose                     (Compose)
import           Data.Functor.Identity                    (Identity)
import           Data.Functor.Product                     (Product)
import           Data.HashMap.Strict                      (HashMap)
import qualified Data.HashMap.Strict               as HM
import           Data.HashSet                             (HashSet)
import           Data.Int                                 (Int)
import           Data.IntMap                              (IntMap)
import qualified Data.IntMap                       as IM
import           Data.IntSet                              (IntSet)
import           Data.Key
import           Data.List.NonEmpty                       (NonEmpty)
import           Data.Map                                 (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Monoid                              (Monoid(..))
import           Data.MonoTraversable                     (Element, MonoFoldable(..), MonoFunctor(..), MonoTraversable(..))
import           Data.Proxy
import           Data.Semigroup                           (Arg, Dual(..), Endo(..), Option(..))
import           Data.Sequence                            (Seq, ViewL(..), ViewR(..))
import qualified Data.Sequence                     as Seq
import           Data.Set                                 (Set)
import           Data.Tagged
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           Data.Tree                                (Tree(..))
import           Data.Vector                              (Vector)
import qualified Data.Vector                       as V
import           Data.Vector.Instances                    ()
import qualified Data.Vector.Storable              as VS
import qualified Data.Vector.Unboxed               as VU
import           Data.Void
import           GHC.Generics


-- |
-- Type family for getting the type of the key of a monomorphic container.
type family MonoKey key

type instance MonoKey (r -> a)             = ()
type instance MonoKey [a]                  = Int
type instance MonoKey (a, b)               = ()
--type instance MonoKey ((g :.: f) a)        = Key (f :.: g)
--type instance MonoKey ((f :*: g) a)        = Key (f :*: g)
--type instance MonoKey ((f :+: g) a)        = Key (f :+: g)
type instance MonoKey (Arg a b)            = ()
type instance MonoKey BS.ByteString        = Int
type instance MonoKey BSL.ByteString       = Int
--type instance MonoKey (Cofree f a)         = Key (Cofree f)
type instance MonoKey (Compose f g a)      = ()
type instance MonoKey (Const m a)          = ()
type instance MonoKey (ContT r m a)        = ()
type instance MonoKey (Either a b)         = ()
--type instance MonoKey (Free f a)           = Key (Free f)
type instance MonoKey (HashMap k v)        = k
type instance MonoKey (HashSet e)          = Int
type instance MonoKey (Identity a)         = ()
type instance MonoKey (IdentityT m a)      = ()
type instance MonoKey (IntMap a)           = Int
type instance MonoKey IntSet               = Int
type instance MonoKey (IO a)               = ()
--type instance MonoKey (K1 i c a)           = Key (K1 i c)
type instance MonoKey (ListT m a)          = Int
type instance MonoKey (Map k v)            = k
type instance MonoKey (Maybe a)            = ()
type instance MonoKey (MaybeT m a)         = ()
--type instance MonoKey (M1 i c f a)         = Key (M1 i c f)
type instance MonoKey (NonEmpty a)         = Int
type instance MonoKey (Option a)           = ()
--type instance MonoKey (Par1 a)             = ()
type instance MonoKey (Product f g a)      = ()
--type instance MonoKey (Proxy a)            = Void
type instance MonoKey (ReaderT r m a)      = ()
--type instance MonoKey (Rec1 f a)           = Key (Rec1 f)
type instance MonoKey (RWST r w s m a)     = ()
type instance MonoKey (S.RWST r w s m a)   = ()
type instance MonoKey (Seq a)              = Int
type instance MonoKey (Set e)              = Int
type instance MonoKey (StateT s m a)       = ()
type instance MonoKey (S.StateT s m a)     = ()
--type instance MonoKey (Tagged a b)         = ()
type instance MonoKey T.Text               = Int
type instance MonoKey TL.Text              = Int
type instance MonoKey (Tree a)             = Seq Int
--type instance MonoKey (U1 a)               = Void
--type instance MonoKey (V1 a)               = Void
type instance MonoKey (Vector a)           = Int
type instance MonoKey (VU.Vector a)        = Int
type instance MonoKey (VS.Vector a)        = Int
type instance MonoKey (ViewL a)            = ()
type instance MonoKey (ViewR a)            = ()
type instance MonoKey (WrappedArrow a b c) = ()
type instance MonoKey (WrappedMonad m a)   = ()
type instance MonoKey (WriterT w m a)      = ()
type instance MonoKey (S.WriterT w m a)    = ()
type instance MonoKey (ZipList a)          = Int


-- |
-- Monomorphic containers that can be mapped over.
class MonoKeyed mono where

    -- |
    -- Map over a monomorphic container
    {-# INLINE omapWithKey #-}
    omapWithKey :: (MonoKey mono -> Element mono -> Element mono) -> mono -> mono

    default omapWithKey :: (Keyed f, Element (f a) ~ a, MonoKey (f a) ~ Key f, f a ~ mono)
                 => (MonoKey mono -> Element mono -> Element mono) -> mono -> mono
    omapWithKey = mapWithKey


-- |
-- Monomorphic containers that can be folded over thier pairs of elements and
-- corresponding keys.
class MonoFoldable mono => MonoFoldableWithKey mono where
    {-# MINIMAL ofoldMapWithKey | ofoldlWithKey #-}

    otoKeyedList :: mono -> [(MonoKey mono, Element mono)]
    otoKeyedList = ofoldrWithKey (\k v t -> (k,v):t) []

    ofoldMapWithKey :: Monoid m => (MonoKey mono -> Element mono -> m) -> mono -> m
    ofoldMapWithKey f = ofoldlWithKey (\a k v -> mappend (f k v) a) mempty

    ofoldrWithKey :: (MonoKey mono -> Element mono -> a -> a) -> a -> mono -> a
    ofoldrWithKey f z t = appEndo (ofoldMapWithKey (\k v -> Endo (f k v)) t) z

    ofoldlWithKey :: (a -> MonoKey mono -> Element mono -> a) -> a -> mono -> a
{--
    default ofoldlWithKey
      :: ( Keyed f
         , Element (f a) ~ a
         , MonoKey (f a) ~ Key f
         , f a ~ mono
         , FoldableWithKey f
         )
      => (a -> MonoKey mono -> Element mono -> a) -> a -> mono -> a
    ofoldlWithKey = foldlWithKey
--}
    ofoldlWithKey f z t = appEndo (getDual (ofoldMapWithKey (\k a -> Dual (Endo (\b -> f b k a))) t)) z


-- |
-- Monomorphic containers that can be traversed from left to right over thier pairs of elements and corresponding keys.
--
-- NOTE: Due to limitations with the role system, GHC is yet unable to provide newtype-derivation of
-- 'MonoTraversableWithKey'. See <https://stackoverflow.com/questions/49776924/newtype-deriving-issequence>.
class (MonoKeyed mono, MonoFoldableWithKey mono, MonoTraversable mono) => MonoTraversableWithKey mono where
    {-# MINIMAL otraverseWithKey #-}

    -- |
    -- Map each key-element pair of a monomorphic container to an action,
    -- evaluate these actions from left to right, and collect the results.
--    {-# INLINE otraverseWithKey #-}
    otraverseWithKey :: Applicative f => (MonoKey mono -> Element mono -> f (Element mono)) -> mono -> f mono
    default otraverseWithKey :: (Applicative f, TraversableWithKey t, Element (t a) ~ a, MonoKey (t a) ~ Key t, t a ~ mono)
      => (MonoKey mono -> Element mono -> f (Element mono)) -> mono -> f mono
    otraverseWithKey = traverseWithKey

    -- |
    -- Like 'otraverse' but with a Monad constraint.
    {-# INLINE omapWithKeyM #-}
    omapWithKeyM :: Monad m => (MonoKey mono -> Element mono -> m (Element mono)) -> mono-> m mono
    omapWithKeyM f = unwrapMonad . otraverseWithKey (fmap WrapMonad . f)


-- |
-- Monomorphic container that can be indexed by a key for an element.
class MonoLookup mono => MonoIndexable mono where
    {-# MINIMAL oindex #-}

    oindex :: mono -> MonoKey mono -> Element mono


-- |
-- Monomorphic container that can be querried by a key for an element.
class MonoLookup mono where
    {-# MINIMAL olookup #-}

    olookup :: MonoKey mono -> mono -> Maybe (Element mono)


-- |
-- Monomorphic container that can adjust elements "in place".
class MonoFunctor mono => MonoAdjustable mono where
    {-# MINIMAL oadjust #-}

    oadjust :: (Element mono -> Element mono) -> MonoKey mono -> mono -> mono

    oreplace :: MonoKey mono -> Element mono -> mono -> mono
    oreplace k v = oadjust (const v) k


-- |
-- Monomorphic container that can be zipped together, merging thier elements.
--
-- Laws:
--
-- @
-- 'omap' 'fst' ('ozip' u u) = u
-- 'omap' 'snd' ('ozip' u u) = u
-- 'ozip' ('omap' 'fst' u) ('omap' 'snd' u) = u
-- 'ozip' ('flip' (,)) x y = 'ozip' y x
-- @
class MonoFunctor mono => MonoZip mono where
    {-# MINIMAL ozipWith #-}

    ozipWith :: (Element mono -> Element mono -> Element mono) -> mono -> mono -> mono
--    ozipWith f a b = uncurry f <$> ozip a b


-- |
-- Monomorphic container that can be zipped together, merging thier pairs of
-- elements and corresponding keys.
class (MonoKeyed mono, MonoZip mono) => MonoZipWithKey mono where
    {-# MINIMAL ozipWithKey #-}

    ozipWithKey :: (MonoKey mono -> Element mono -> Element mono -> Element mono) -> mono -> mono -> mono
--    ozipWithKey f = ozap . omapWithKey f


-- * MonoKeyed Instances


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (r -> a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed [a]


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (a, b) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (Arg a b) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed BS.ByteString where
    {-# INLINE omapWithKey #-}

    omapWithKey f = snd . BS.mapAccumL g 0
      where
        g k v = (succ k, f k v)


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed BSL.ByteString where
    {-# INLINE omapWithKey #-}

    omapWithKey f = snd . BSL.mapAccumL g 0
      where
        g k v = (succ k, f k v)


-- |
-- /Since @v0.1.0@/ 
instance (Functor f, Functor g) => MonoKeyed (Compose f g a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (Const m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Functor m => MonoKeyed (ContT r m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (Either a b) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (HashMap k v)


-- Cannot instantiate because the map might violate the internal structure
-- instance MonoKeyed (HashSet v)


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (Identity a)


-- |
-- /Since @v0.1.0@/ 
instance Functor m => MonoKeyed (IdentityT m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (IntMap a)


-- Cannot instantiate because the map might violate the internal structure
-- instance MonoKeyed IntSet


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (IO a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Functor m => MonoKeyed (ListT m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey f = ListT . fmap (omapWithKey f) . runListT


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (Map k v)


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (Maybe a)


-- |
-- /Since @v0.1.0@/ 
instance Functor m => MonoKeyed (MaybeT m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (NonEmpty a)


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (Option a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance (Functor f, Functor g) => MonoKeyed (Product f g a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Functor m => MonoKeyed (ReaderT r m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Functor m => MonoKeyed (RWST r w s m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Functor m => MonoKeyed (S.RWST r w s m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (Seq a)


-- Cannot instantiate because the map might violate the internal structure
-- instance MonoKeyed Set


-- |
-- /Since @v0.1.0@/ 
instance Functor m => MonoKeyed (StateT s m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Functor m => MonoKeyed (S.StateT s m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed T.Text where
    {-# INLINE omapWithKey #-}

    omapWithKey f = snd . T.mapAccumL g 0
      where
        g k v = (succ k, f k v)


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed TL.Text where
    {-# INLINE omapWithKey #-}

    omapWithKey f = snd . TL.mapAccumL g 0
      where
        g k v = (succ k, f k v)


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (Tree a)


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (Vector a)


-- |
-- /Since @v0.1.0@/ 
instance VU.Unbox a => MonoKeyed (VU.Vector a) where

    {-# INLINE omapWithKey #-}
    omapWithKey = VU.imap


-- |
-- /Since @v0.1.0@/ 
instance VS.Storable a => MonoKeyed (VS.Vector a) where

    {-# INLINE omapWithKey #-}
    omapWithKey = VS.imap


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (ViewL a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (ViewR a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Arrow a => MonoKeyed (WrappedArrow a b c) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Monad m => MonoKeyed (WrappedMonad m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Functor m => MonoKeyed (WriterT w m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Functor m => MonoKeyed (S.WriterT w m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoKeyed (ZipList a)


-- * MonoFoldable Instances


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey [a] where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (a, b) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey BS.ByteString where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey  = monoFoldableWithIntegralKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey BSL.ByteString where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey  = monoFoldableWithIntegralKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey T.Text where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey TL.Text where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (Maybe a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (Tree a) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = foldMapWithKey

    ofoldrWithKey   = foldrWithKey

    ofoldlWithKey   = foldlWithKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (Seq a) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = Seq.foldMapWithIndex

    ofoldrWithKey   = Seq.foldrWithIndex

    ofoldlWithKey   = Seq.foldlWithIndex


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (ViewL a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (ViewR a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (IntMap a) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = IM.foldMapWithKey

    ofoldrWithKey   = IM.foldrWithKey

    ofoldlWithKey   = IM.foldlWithKey'


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey IntSet where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (Option a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (NonEmpty a) where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (Identity a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (Map k v) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = Map.foldMapWithKey

    ofoldrWithKey   = Map.foldrWithKey

    ofoldlWithKey   = Map.foldlWithKey'


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (HashMap k v) where
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldrWithKey   = HM.foldrWithKey

    ofoldlWithKey   = HM.foldlWithKey'


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (HashSet v) where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (Vector a) where
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldrWithKey   = V.ifoldr

    ofoldlWithKey   = V.ifoldl'


-- |
-- /Since @v0.1.0@/ 
instance Ord e => MonoFoldableWithKey (Set e) where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- /Since @v0.1.0@/ 
instance VU.Unbox a => MonoFoldableWithKey (VU.Vector a) where
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldrWithKey   = VU.ifoldr

    ofoldlWithKey   = VU.ifoldl'


-- |
-- /Since @v0.1.0@/ 
instance VS.Storable a => MonoFoldableWithKey (VS.Vector a) where
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldrWithKey   = VS.ifoldr

    ofoldlWithKey   = VS.ifoldl'


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (Either a b) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoFoldableWithKey (Const m a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Foldable f => MonoFoldableWithKey (MaybeT f a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Foldable f => MonoFoldableWithKey (ListT f a) where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- /Since @v0.1.0@/ 
instance Foldable f => MonoFoldableWithKey (IdentityT f a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Foldable f => MonoFoldableWithKey (WriterT w f a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Foldable f => MonoFoldableWithKey (S.WriterT w f a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance (Foldable f, Foldable g) => MonoFoldableWithKey (Compose f g a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance (Foldable f, Foldable g) => MonoFoldableWithKey (Product f g a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey BS.ByteString where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f = fmap BS.pack . traverseWithKey f . BS.unpack

    omapWithKeyM f = fmap BS.pack . mapWithKeyM f . BS.unpack


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey BSL.ByteString where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f = fmap BSL.pack . traverseWithKey f . BSL.unpack

    omapWithKeyM f = fmap BSL.pack . mapWithKeyM f . BSL.unpack


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey T.Text where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f = fmap T.pack . traverseWithKey f . T.unpack

    omapWithKeyM f = fmap T.pack . mapWithKeyM f . T.unpack


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey TL.Text where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f = fmap TL.pack . traverseWithKey f . TL.unpack

    omapWithKeyM f = fmap TL.pack . mapWithKeyM f . TL.unpack


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey [a] where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (Maybe a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (Tree a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (Seq a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (ViewL a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (ViewR a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (IntMap a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (Option a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (NonEmpty a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (Identity a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (Map k v) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (HashMap k v) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (Vector a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance VU.Unbox a => MonoTraversableWithKey (VU.Vector a) where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f v = fmap (VU.fromListN (VU.length v)) . traverseWithKey f $ VU.toList v

    omapWithKeyM = otraverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance VS.Storable a => MonoTraversableWithKey (VS.Vector a) where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f v = fmap (VS.fromListN (VS.length v)) . traverseWithKey f $ VS.toList v

    omapWithKeyM = otraverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (Either a b) where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey _ (Left  a) = pure $ Left a
    otraverseWithKey f (Right b) = Right <$> f () b

    omapWithKeyM = otraverseWithKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (a, b) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance MonoTraversableWithKey (Const m a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Traversable f => MonoTraversableWithKey (MaybeT f a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Traversable f => MonoTraversableWithKey (ListT f a) where

   otraverseWithKey f = fmap ListT . traverse (traverseWithKey f) . runListT


-- |
-- /Since @v0.1.0@/ 
instance Traversable f => MonoTraversableWithKey (IdentityT f a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Traversable f => MonoTraversableWithKey (WriterT w f a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance Traversable f => MonoTraversableWithKey (S.WriterT w f a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance (Traversable f, Traversable g) => MonoTraversableWithKey (Compose f g a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- /Since @v0.1.0@/ 
instance (Traversable f, Traversable g) => MonoTraversableWithKey (Product f g a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- * Unwraping functions


-- |
-- /Since @v0.1.0@/
--
-- A strict left fold, together with an unwrap function.
--
-- This is convenient when the accumulator value is not the same as the final
-- expected type. It is provided mainly for integration with the
-- @[foldl](http://hackage.haskell.org/package/foldl)@
-- package, to be used in conjunction with
-- @[purely](hackage.haskell.org/package/foldl/docs/Control-Foldl.html#v:purely).@
ofoldlWithKeyUnwrap :: MonoFoldableWithKey mono
             => (x -> Element mono -> x) -> x -> (x -> b) -> mono -> b
ofoldlWithKeyUnwrap f x unwrap mono = unwrap (ofoldl' f x mono)

-- |
-- /Since @v0.1.0@/
--
-- A monadic strict left fold, together with an unwrap function.
--
-- Similar to 'foldlUnwrap', but allows monadic actions. To be used with
-- @[impurely](hackage.haskell.org/package/foldl/docs/Control-Foldl.html#v:impurely)@
-- from @[foldl](http://hackage.haskell.org/package/foldl).@
ofoldWithKeyMUnwrap :: (Monad m, MonoFoldableWithKey mono)
             => (x -> Element mono -> m x) -> m x -> (x -> m b) -> mono -> m b
ofoldWithKeyMUnwrap f mx unwrap mono = do
    x <- mx
    x' <- ofoldlM f x mono
    unwrap x'


-- * Utility Functions


omapWithUnitKey :: MonoFunctor mono => (() -> Element mono -> Element mono) -> mono -> mono
omapWithUnitKey f = omap (f ())


{-
omapWithIntegralKey
  :: ( Bounded i, Enum i, MonoTraversable mono)
  => (i -> Element mono -> Element mono) -> mono -> mono
omapWithIntegralKey f = (`S.evalState` minBound) . omapM g
  where
    g e = do
        k <- S.get
        S.modify succ
        return $ f k e
-}


monoFoldableWithUnitKey :: (Monoid m, MonoFoldable mono) => (() -> Element mono -> m) -> mono -> m
monoFoldableWithUnitKey f = ofoldMap (f ())


monoFoldableWithIntegralKey
  :: ( Bounded i, Enum i, MonoFoldable mono)
  => (a -> i -> Element mono -> a) -> a -> mono -> a
monoFoldableWithIntegralKey f z = (`S.evalState` minBound) . ofoldlM g z
  where
    g a e = do
        k <- S.get
        S.modify succ
        return $ f a k e

monoTraversableWithUnitKey
  :: (Applicative f, MonoTraversable mono)
  => (() -> Element mono -> f (Element mono)) -> mono -> f mono
monoTraversableWithUnitKey f = otraverse (f ())


