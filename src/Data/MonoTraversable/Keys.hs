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

{-# LANGUAGE BangPatterns            #-}
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
--import           Control.Comonad.Cofree                   (Cofree(..))
import           Control.Monad                            (Monad (..))
--import           Control.Monad.Free
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
import           Data.Foldable
import           Data.Functor.Compose                     (Compose(..))
import           Data.Functor.Identity                    (Identity)
import           Data.Functor.Product                     (Product(..))
import           Data.Hashable
import           Data.HashMap.Strict                      (HashMap)
import qualified Data.HashMap.Strict               as HM
import           Data.HashSet                             (HashSet)
import qualified Data.HashSet                      as HS
import           Data.Int                                 (Int)
import           Data.IntMap                              (IntMap)
import qualified Data.IntMap                       as IM
import           Data.IntSet                              (IntSet)
import qualified Data.IntSet                       as IS
import           Data.Key
import           Data.List.NonEmpty                       (NonEmpty(..))
import           Data.Map                                 (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe
import           Data.Monoid                              (Monoid(..))
import           Data.MonoTraversable                     (Element, MonoFoldable(..), MonoFunctor(..), MonoTraversable(..))
--import           Data.Proxy
import           Data.Semigroup                           (Semigroup(..), Arg(..), Dual(..), Endo(..), Option(..))
import           Data.Sequence                            (Seq, ViewL(..), ViewR(..))
import qualified Data.Sequence                     as Seq
import           Data.Set                                 (Set)
import qualified Data.Set                          as Set
--import           Data.Tagged
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL
import           Data.Tree                                (Tree(..))
import           Data.Vector                              (Vector)
import qualified Data.Vector                       as V
import           Data.Vector.Instances                    ()
import qualified Data.Vector.Storable              as VS
import qualified Data.Vector.Storable.Mutable      as VSM
import qualified Data.Vector.Unboxed               as VU
import qualified Data.Vector.Unboxed.Mutable       as VUM
--import           Data.Void
--import           GHC.Generics
import           Prelude                           hiding (lookup, zipWith)


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
type instance MonoKey (Compose f g a)      = (MonoKey (f a), MonoKey (g a))
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
type instance MonoKey (Product f g a)      = Either (Key f) (Key g)
--type instance MonoKey (Proxy a)            = Void
type instance MonoKey (ReaderT r m a)      = (r, Key m)
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
-- Monomorphic container that can be querried by a key for an element.
class MonoLookup mono where
    {-# MINIMAL olookup #-}

    olookup :: MonoKey mono -> mono -> Maybe (Element mono)
    default olookup :: (Lookup f, Element (f a) ~ a, MonoKey (f a) ~ Key f, f a ~ mono)
                    => MonoKey mono -> mono -> Maybe (Element mono)
    olookup = lookup


-- |
-- Monomorphic container that can be indexed by a key for an element.
class MonoLookup mono => MonoIndexable mono where
    {-# MINIMAL oindex #-}

    oindex :: mono -> MonoKey mono -> Element mono
    default oindex :: (Indexable f, Element (f a) ~ a, MonoKey (f a) ~ Key f, f a ~ mono)
                   => mono -> MonoKey mono -> Element mono
    oindex = index


-- |
-- Monomorphic container that can adjust elements "in place."
class MonoFunctor mono => MonoAdjustable mono where
    {-# MINIMAL oadjust #-}

    oadjust :: (Element mono -> Element mono) -> MonoKey mono -> mono -> mono
    default oadjust :: (Adjustable f, Element (f a) ~ a, MonoKey (f a) ~ Key f, f a ~ mono)
                    => (Element mono -> Element mono) -> MonoKey mono -> mono -> mono
    oadjust = adjust

    oreplace :: MonoKey mono -> Element mono -> mono -> mono
    oreplace k v = oadjust (const v) k


-- |
-- Monomorphic container that can be zipped together, merging thier elements.
--
-- Laws:
--
-- @
-- 'ozipWith' const u u === 'ozipWith' (flip const) u u === u
-- 'ozipWith' ('flip' f) x y === 'ozipWith' f y x
-- 'ozipWith' (\a b -> f (g a) (h b)) x y === 'ozipWith' f ('omap' g x) ('omap' h y)
-- @
class MonoFunctor mono => MonoZip mono where
    {-# MINIMAL ozipWith #-}

    ozipWith :: (Element mono -> Element mono -> Element mono) -> mono -> mono -> mono


-- |
-- Monomorphic container that can be zipped together, merging thier pairs of
-- elements and corresponding keys.
class (MonoKeyed mono, MonoZip mono) => MonoZipWithKey mono where
    {-# MINIMAL ozipWithKey #-}

    ozipWithKey :: (MonoKey mono -> Element mono -> Element mono -> Element mono) -> mono -> mono -> mono
--    ozipWithKey f = ozap . omapWithKey f


-- * MonoKeyed Instances


-- |
-- @since 0.1.0 
instance MonoKeyed (r -> a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance MonoKeyed [a]


-- |
-- @since 0.1.0 
instance MonoKeyed (a, b) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance MonoKeyed (Arg a b) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance MonoKeyed BS.ByteString where
    {-# INLINE omapWithKey #-}

    omapWithKey f = snd . BS.mapAccumL g 0
      where
        g k v = (succ k, f k v)


-- |
-- @since 0.1.0 
instance MonoKeyed BSL.ByteString where
    {-# INLINE omapWithKey #-}

    omapWithKey f = snd . BSL.mapAccumL g 0
      where
        g k v = (succ k, f k v)


-- |
-- @since 0.1.0 
instance ( Keyed f
         , Keyed g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoKeyed (Compose f g a)


-- |
-- @since 0.1.0 
instance MonoKeyed (Const m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance Functor m => MonoKeyed (ContT r m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance MonoKeyed (Either a b) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance MonoKeyed (HashMap k v)


-- Cannot instantiate because the map might violate the internal structure
-- instance MonoKeyed (HashSet v)


-- |
-- @since 0.1.0 
instance MonoKeyed (Identity a)


-- |
-- @since 0.1.0 
instance Functor m => MonoKeyed (IdentityT m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance MonoKeyed (IntMap a)


-- Cannot instantiate because the map might violate the internal structure
-- instance MonoKeyed IntSet


-- |
-- @since 0.1.0 
instance MonoKeyed (IO a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance Functor m => MonoKeyed (ListT m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey f = ListT . fmap (omapWithKey f) . runListT


-- |
-- @since 0.1.0 
instance MonoKeyed (Map k v)


-- |
-- @since 0.1.0 
instance MonoKeyed (Maybe a)


-- |
-- @since 0.1.0 
instance Functor m => MonoKeyed (MaybeT m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance MonoKeyed (NonEmpty a)


-- |
-- @since 0.1.0 
instance MonoKeyed (Option a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance ( Keyed f
         , Keyed g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoKeyed (Product f g a)


-- |
-- @since 0.1.0 
instance Keyed m => MonoKeyed (ReaderT r m a)


-- |
-- @since 0.1.0 
instance Functor m => MonoKeyed (RWST r w s m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance Functor m => MonoKeyed (S.RWST r w s m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance MonoKeyed (Seq a)


-- Cannot instantiate because the map might violate the internal structure
-- instance MonoKeyed Set


-- |
-- @since 0.1.0 
instance Functor m => MonoKeyed (StateT s m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance Functor m => MonoKeyed (S.StateT s m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance MonoKeyed T.Text where
    {-# INLINE omapWithKey #-}

    omapWithKey f = snd . T.mapAccumL g 0
      where
        g k v = (succ k, f k v)


-- |
-- @since 0.1.0 
instance MonoKeyed TL.Text where
    {-# INLINE omapWithKey #-}

    omapWithKey f = snd . TL.mapAccumL g 0
      where
        g k v = (succ k, f k v)


-- |
-- @since 0.1.0 
instance MonoKeyed (Tree a)


-- |
-- @since 0.1.0 
instance MonoKeyed (Vector a)


-- |
-- @since 0.1.0 
instance VU.Unbox a => MonoKeyed (VU.Vector a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = VU.imap


-- |
-- @since 0.1.0 
instance VS.Storable a => MonoKeyed (VS.Vector a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = VS.imap


-- |
-- @since 0.1.0 
instance MonoKeyed (ViewL a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance MonoKeyed (ViewR a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance Arrow a => MonoKeyed (WrappedArrow a b c) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance Monad m => MonoKeyed (WrappedMonad m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance Functor m => MonoKeyed (WriterT w m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance Functor m => MonoKeyed (S.WriterT w m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


-- |
-- @since 0.1.0 
instance MonoKeyed (ZipList a)


-- * MonoFoldable Instances


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey [a] where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (a, b) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey BS.ByteString where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey  = monoFoldableWithIntegralKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey BSL.ByteString where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey  = monoFoldableWithIntegralKey


-- |
-- @since 0.1.0 
instance ( FoldableWithKey f
         , FoldableWithKey g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoFoldableWithKey (Compose f g a) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = foldMapWithKey

    ofoldrWithKey   = foldrWithKey

    ofoldlWithKey   = foldlWithKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (Const m a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (Either a b) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (HashMap k v) where
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldrWithKey   = HM.foldrWithKey

    ofoldlWithKey   = HM.foldlWithKey'


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (HashSet v) where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (Identity a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- @since 0.1.0 
instance Foldable f => MonoFoldableWithKey (IdentityT f a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (IntMap a) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = IM.foldMapWithKey

    ofoldrWithKey   = IM.foldrWithKey

    ofoldlWithKey   = IM.foldlWithKey'


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey IntSet where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- @since 0.1.0 
instance Foldable f => MonoFoldableWithKey (ListT f a) where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (Map k v) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = Map.foldMapWithKey

    ofoldrWithKey   = Map.foldrWithKey

    ofoldlWithKey   = Map.foldlWithKey'


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (Maybe a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- @since 0.1.0 
instance Foldable f => MonoFoldableWithKey (MaybeT f a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (NonEmpty a) where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (Option a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- @since 0.1.0 
instance ( FoldableWithKey f
         , FoldableWithKey g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoFoldableWithKey (Product f g a) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = foldMapWithKey

    ofoldrWithKey   = foldrWithKey

    ofoldlWithKey   = foldlWithKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (Seq a) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = foldMapWithKey

    ofoldrWithKey   = Seq.foldrWithIndex

    ofoldlWithKey   = Seq.foldlWithIndex


-- |
-- @since 0.1.0 
instance Ord e => MonoFoldableWithKey (Set e) where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey T.Text where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey TL.Text where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (Tree a) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = foldMapWithKey

    ofoldrWithKey   = foldrWithKey

    ofoldlWithKey   = foldlWithKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (Vector a) where
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldrWithKey   = V.ifoldr

    ofoldlWithKey   = V.ifoldl'


-- |
-- @since 0.1.0 
instance VU.Unbox a => MonoFoldableWithKey (VU.Vector a) where
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldrWithKey   = VU.ifoldr

    ofoldlWithKey   = VU.ifoldl'


-- |
-- @since 0.1.0 
instance VS.Storable a => MonoFoldableWithKey (VS.Vector a) where
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldrWithKey   = VS.ifoldr

    ofoldlWithKey   = VS.ifoldl'


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (ViewL a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- @since 0.1.0 
instance MonoFoldableWithKey (ViewR a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- @since 0.1.0 
instance Foldable f => MonoFoldableWithKey (WriterT w f a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- |
-- @since 0.1.0 
instance Foldable f => MonoFoldableWithKey (S.WriterT w f a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


-- * MonoTraversableWithKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey [a] where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (a, b) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey BS.ByteString where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f = fmap BS.pack . traverseWithKey f . BS.unpack

    omapWithKeyM f = fmap BS.pack . mapWithKeyM f . BS.unpack


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey BSL.ByteString where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f = fmap BSL.pack . traverseWithKey f . BSL.unpack

    omapWithKeyM f = fmap BSL.pack . mapWithKeyM f . BSL.unpack


-- |
-- @since 0.1.0 
instance ( MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         , TraversableWithKey f
         , TraversableWithKey g
         ) => MonoTraversableWithKey (Compose f g a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (Const m a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (Either a b) where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey _ (Left  a) = pure $ Left a
    otraverseWithKey f (Right b) = Right <$> f () b

    omapWithKeyM = otraverseWithKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (HashMap k v) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (Identity a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- @since 0.1.0 
instance Traversable f => MonoTraversableWithKey (IdentityT f a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (IntMap a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- @since 0.1.0 
instance Traversable f => MonoTraversableWithKey (ListT f a) where

   otraverseWithKey f = fmap ListT . traverse (traverseWithKey f) . runListT


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (Map k v) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (Maybe a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- @since 0.1.0 
instance Traversable f => MonoTraversableWithKey (MaybeT f a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (NonEmpty a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (Option a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- @since 0.1.0 
instance ( MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         , TraversableWithKey f
         , TraversableWithKey g
         ) => MonoTraversableWithKey (Product f g a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (Seq a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey T.Text where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f = fmap T.pack . traverseWithKey f . T.unpack

    omapWithKeyM f = fmap T.pack . mapWithKeyM f . T.unpack


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey TL.Text where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f = fmap TL.pack . traverseWithKey f . TL.unpack

    omapWithKeyM f = fmap TL.pack . mapWithKeyM f . TL.unpack


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (Tree a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (Vector a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = traverseWithKey


-- |
-- @since 0.1.0 
instance VU.Unbox a => MonoTraversableWithKey (VU.Vector a) where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f v = fmap (VU.fromListN (VU.length v)) . traverseWithKey f $ VU.toList v

    omapWithKeyM = otraverseWithKey


-- |
-- @since 0.1.0 
instance VS.Storable a => MonoTraversableWithKey (VS.Vector a) where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f v = fmap (VS.fromListN (VS.length v)) . traverseWithKey f $ VS.toList v

    omapWithKeyM = otraverseWithKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (ViewL a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- @since 0.1.0 
instance MonoTraversableWithKey (ViewR a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- @since 0.1.0 
instance Traversable f => MonoTraversableWithKey (WriterT w f a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- |
-- @since 0.1.0 
instance Traversable f => MonoTraversableWithKey (S.WriterT w f a) where
    {-# INLINE otraverseWithKey #-}

    otraverseWithKey = monoTraversableWithUnitKey


-- * MonoLookup Instances


-- |
-- @since 0.1.0 
instance MonoLookup [a] where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance MonoLookup (a, b) where
    {-# INLINE olookup #-}

    olookup _ (_, v) = Just v


-- |
-- @since 0.1.0 
instance MonoLookup (Arg a b) where
    {-# INLINE olookup #-}

    olookup _ (Arg _ v) = Just v


-- |
-- @since 0.1.0 
instance MonoLookup BS.ByteString where
    {-# INLINE olookup #-}

    olookup i bs
      |  i < 0
      || i >= BS.length bs = Nothing
      |  otherwise         = Just $ BS.index bs i


-- |
-- @since 0.1.0 
instance MonoLookup BSL.ByteString where
    {-# INLINE olookup #-}

    olookup i bs
      |  i < 0
      || i >= fromEnum (BSL.length bs) = Nothing
      |  otherwise                     = Just . BSL.index bs $ toEnum i


-- |
-- @since 0.1.0 
instance ( Lookup f
         , Lookup g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoLookup (Compose f g a) where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance MonoLookup (Either a b) where
    {-# INLINE olookup #-}

    olookup _ (Left  _) = Nothing
    olookup _ (Right v) = Just v


-- |
-- @since 0.1.0 
instance (Eq k, Hashable k) => MonoLookup (HashMap k v) where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance MonoLookup (HashSet v) where
    {-# INLINE olookup #-}

    olookup =  monoLookupFoldable


-- |
-- @since 0.1.0 
instance MonoLookup (Identity a) where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance MonoLookup (IntMap a) where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance MonoLookup IntSet where
    {-# INLINE olookup #-}

    olookup = monoLookupFoldable


-- |
-- @since 0.1.0 
instance Ord k => MonoLookup (Map k v) where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance MonoLookup (Maybe a) where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance MonoLookup (NonEmpty a) where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance MonoLookup (Option a) where
    {-# INLINE olookup #-}

    olookup = const getOption


-- |
-- @since 0.1.0 
instance ( Lookup f
         , Lookup g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoLookup (Product f g a) where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance Lookup m => MonoLookup (ReaderT r m a) where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance MonoLookup (Seq a) where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance Ord a => MonoLookup (Set a) where
    {-# INLINE olookup #-}

    olookup = monoLookupFoldable


-- |
-- @since 0.1.0 
instance MonoLookup T.Text where
    {-# INLINE olookup #-}

    olookup i ts
      |  i < 0
      || i >= T.length ts = Nothing
      |  otherwise        = Just $ T.index ts i


-- |
-- @since 0.1.0 
instance MonoLookup TL.Text where
    {-# INLINE olookup #-}

    olookup i ts
      |  i < 0
      || i >= fromEnum (TL.length ts) = Nothing
      |  otherwise                    = Just . TL.index ts $ toEnum i


-- |
-- @since 0.1.0 
instance MonoLookup (Tree a) where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance MonoLookup (Vector a) where
    {-# INLINE olookup #-}

    olookup = lookup


-- |
-- @since 0.1.0 
instance VU.Unbox a => MonoLookup (VU.Vector a) where
    {-# INLINE olookup #-}

    olookup = flip (VU.!?)


-- |
-- @since 0.1.0 
instance VS.Storable a => MonoLookup (VS.Vector a) where
    {-# INLINE olookup #-}

    olookup = flip (VS.!?)


-- |
-- @since 0.1.0 
instance MonoLookup (ViewL a) where
    {-# INLINE olookup #-}

    olookup _ EmptyL = Nothing
    olookup _ (v:<_) = Just v


-- |
-- @since 0.1.0 
instance MonoLookup (ViewR a) where
    {-# INLINE olookup #-}

    olookup _ EmptyR = Nothing
    olookup _ (_:>v) = Just v


-- |
-- @since 0.1.0 
instance MonoLookup (ZipList a) where
    {-# INLINE olookup #-}

    olookup = lookup


-- * MonoIndexable Instances


-- |
-- @since 0.1.0 
instance MonoIndexable [a] where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance MonoIndexable (a, b) where
    {-# INLINE oindex #-}

    oindex (_, v) = const v


-- |
-- @since 0.1.0 
instance MonoIndexable (Arg a b) where
    {-# INLINE oindex #-}

    oindex (Arg _ v) = const v


-- |
-- @since 0.1.0 
instance MonoIndexable BS.ByteString where
    {-# INLINE oindex #-}

    oindex bs i
      |  i < 0
      || i >= BS.length bs = error $ mconcat [ "oindex on ByteString at point ", show i, " is outside the range: [0, ", show (BS.length bs - 1), "]."]
      |  otherwise         = BS.index bs i


-- |
-- @since 0.1.0 
instance MonoIndexable BSL.ByteString where
    {-# INLINE oindex #-}

    oindex bs i
      |  i < 0
      || i >= fromEnum (BSL.length bs) = error $ mconcat [ "oindex on Lazy ByteString at point ", show i, " is outside the range: [0, ", show (BSL.length bs - 1), "]."]
      |  otherwise                     = BSL.index bs $ toEnum i


-- |
-- @since 0.1.0 
instance ( Indexable f
         , Indexable g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoIndexable (Compose f g a) where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance MonoIndexable (Either a b) where
    {-# INLINE oindex #-}

    oindex (Right v) = const v
    oindex (Left  _) = error
        "oindex on Either is Left, cannot retreive a value. Consider using olookup instead."


-- |
-- @since 0.1.0 
instance (Eq k, Hashable k) => MonoIndexable (HashMap k v) where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance MonoIndexable (HashSet v) where
    {-# INLINE oindex #-}

    oindex hs i = fromMaybe errorMessage $ olookup i hs
      where
        errorMessage = error $ mconcat
            [ "oindex on HashSet at point "
            , show i
            , " is outside the range: [0, "
            , show (HS.size hs - 1)
            , "]."
            ]


-- |
-- @since 0.1.0 
instance MonoIndexable (Identity a) where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance MonoIndexable (IntMap a) where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance MonoIndexable IntSet where
    {-# INLINE oindex #-}

    oindex is i = fromMaybe errorMessage $ olookup i is
      where
        errorMessage = error $ mconcat
            [ "oindex on IntSet at point "
            , show i
            , " is outside the range: [0, "
            , show (IS.size is - 1)
            , "]."
            ]


-- |
-- @since 0.1.0 
instance Ord k => MonoIndexable (Map k v) where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance MonoIndexable (Maybe a) where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance MonoIndexable (NonEmpty a) where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance MonoIndexable (Option a) where
    {-# INLINE oindex #-}

    oindex = flip . const $ fromMaybe errorMessage . getOption
      where
        errorMessage = error 
            "oindex on empty Option, cannot retreive a value. Consider using olookup instead."


-- |
-- @since 0.1.0 
instance ( Indexable f
         , Indexable g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoIndexable (Product f g a) where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance Indexable m => MonoIndexable (ReaderT r m a) where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance MonoIndexable (Seq a) where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance Ord a => MonoIndexable (Set a) where
    {-# INLINE oindex #-}

    oindex s i = fromMaybe errorMessage $ olookup i s
      where
        errorMessage = error $ mconcat
            [ "oindex on Set at point "
            , show i
            , " is outside the range: [0, "
            , show (Set.size s - 1)
            , "]."
            ]


-- |
-- @since 0.1.0 
instance MonoIndexable T.Text where
    {-# INLINE oindex #-}

    oindex ts i
      |  i < 0
      || i >= T.length ts = error $ mconcat [ "oindex on Text at point ", show i, " is outside the range: [0, ", show (T.length ts - 1), "]."]
      |  otherwise        = T.index ts i


-- |
-- @since 0.1.0 
instance MonoIndexable TL.Text where
    {-# INLINE oindex #-}

    oindex ts i
      |  i < 0
      || i >= fromEnum (TL.length ts) = error $ mconcat [ "oindex on Lazy Text at point ", show i, " is outside the range: [0, ", show (TL.length ts - 1), "]."]
      |  otherwise                    = TL.index ts $ toEnum i


-- |
-- @since 0.1.0 
instance MonoIndexable (Tree a) where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance MonoIndexable (Vector a) where
    {-# INLINE oindex #-}

    oindex = index


-- |
-- @since 0.1.0 
instance VU.Unbox a => MonoIndexable (VU.Vector a) where
    {-# INLINE oindex #-}

    oindex = (VU.!)


-- |
-- @since 0.1.0 
instance VS.Storable a => MonoIndexable (VS.Vector a) where
    {-# INLINE oindex #-}

    oindex = (VS.!)


-- |
-- @since 0.1.0 
instance MonoIndexable (ViewL a) where
    {-# INLINE oindex #-}

    oindex (v:<_) = const v
    oindex EmptyL = error
        "oindex on ViewL is EmptyL, cannot retreive a value. Consider using olookup instead."


-- |
-- @since 0.1.0 
instance MonoIndexable (ViewR a) where
    {-# INLINE oindex #-}

    oindex (_:>v) = const v
    oindex EmptyR = error
        "oindex on ViewR is EmptyR, cannot retreive a value. Consider using olookup instead."


-- |
-- @since 0.1.0 
instance MonoIndexable (ZipList a) where
    {-# INLINE oindex #-}

    oindex = index


-- * MonoAdjustable Instances


-- |
-- @since 0.1.0 
instance MonoAdjustable (r -> a) where
    {-# INLINE oadjust #-}

    oadjust f _ g = f . g 


-- |
-- @since 0.1.0 
instance MonoAdjustable [a] where
    {-# INLINE oadjust #-}

    oadjust = adjust


-- |
-- @since 0.1.0 
instance MonoAdjustable (a, b) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance MonoAdjustable (Arg a b) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance MonoAdjustable BS.ByteString where
    {-# INLINE oadjust #-}

    oadjust f i bs
      |  i < 0
      || i >= BS.length bs = bs
      |  otherwise         = snd $ BS.mapAccumL g 0 bs
      where
        g k v = (succ k, if k == i then f v else v)


-- |
-- @since 0.1.0 
instance MonoAdjustable BSL.ByteString where
    {-# INLINE oadjust #-}

    oadjust f i bs
      |  i < 0
      || i >= fromEnum (BSL.length bs) = bs
      |  otherwise                     = snd $ BSL.mapAccumL g 0 bs
      where
        g k v = (succ k, if k == i then f v else v)


-- |
-- @since 0.1.0 
instance MonoAdjustable (Const m a) where
    {-# INLINE oadjust #-}

    oadjust = const $ const id


-- |
-- @since 0.1.0 
instance Functor m => MonoAdjustable (ContT r m a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance MonoAdjustable (Either a b) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance (Eq k, Hashable k) => MonoAdjustable (HashMap k v) where
    {-# INLINE oadjust #-}

    oadjust = HM.adjust


-- Cannot instantiate because the adjust might violate the internal structure
-- instance MonoAdjustable (HashSet v)


-- |
-- @since 0.1.0 
instance MonoAdjustable (Identity a) where
    {-# INLINE oadjust #-}

    oadjust = adjust


-- |
-- @since 0.1.0 
instance Functor m => MonoAdjustable (IdentityT m a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance MonoAdjustable (IntMap a) where
    {-# INLINE oadjust #-}

    oadjust = IM.adjust


-- Cannot instantiate because the adjust might violate the internal structure
-- instance MonoAdjustable IntSet


-- |
-- @since 0.1.0 
instance MonoAdjustable (IO a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance Functor m => MonoAdjustable (ListT m a) where
    {-# INLINE oadjust #-}

    oadjust f i = ListT . fmap (adjust f i) . runListT


-- |
-- @since 0.1.0 
instance Ord k => MonoAdjustable (Map k v) where
    {-# INLINE oadjust #-}
  
    oadjust = Map.adjust


-- |
-- @since 0.1.0 
instance MonoAdjustable (Maybe a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance Functor m => MonoAdjustable (MaybeT m a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance MonoAdjustable (NonEmpty a) where
    {-# INLINE oadjust #-}

    oadjust = adjust


-- |
-- @since 0.1.0 
instance MonoAdjustable (Option a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance ( Adjustable f
         , Adjustable g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoAdjustable (Product f g a) where
    {-# INLINE oadjust #-}

    oadjust = adjust


-- |
-- @since 0.1.0 
instance Functor m => MonoAdjustable (ReaderT r m a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance Functor m => MonoAdjustable (RWST r w s m a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance Functor m => MonoAdjustable (S.RWST r w s m a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance MonoAdjustable (Seq a) where
    {-# INLINE oadjust #-}

    oadjust = adjust


-- Cannot instantiate because the adjust might violate the internal structure
-- instance MonoAdjustable Set


-- |
-- @since 0.1.0 
instance Functor m => MonoAdjustable (StateT s m a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance Functor m => MonoAdjustable (S.StateT s m a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance MonoAdjustable T.Text where
    {-# INLINE oadjust #-}

    oadjust f i ts
      |  i < 0
      || i >= T.length ts = ts
      |  otherwise        = snd $ T.mapAccumL g 0 ts
      where
        g k v = (succ k, if k == i then f v else v)


-- |
-- @since 0.1.0 
instance MonoAdjustable TL.Text where
    {-# INLINE oadjust #-}

    oadjust f i ts
      |  i < 0
      || i >= fromEnum (TL.length ts) = ts
      |  otherwise                    = snd $ TL.mapAccumL g 0 ts
      where
        g k v = (succ k, if k == i then f v else v)


-- |
-- @since 0.1.0 
instance MonoAdjustable (Tree a) where
    {-# INLINE oadjust #-}

    oadjust = adjust


-- |
-- @since 0.1.0 
instance MonoAdjustable (Vector a) where
    {-# INLINE oadjust #-}

    oadjust = adjust


-- |
-- @since 0.1.0 
instance VU.Unbox a => MonoAdjustable (VU.Vector a) where
    {-# INLINE oadjust #-}

    oadjust f i = VU.modify $ \v -> VUM.modify v f i


-- |
-- @since 0.1.0 
instance VS.Storable a => MonoAdjustable (VS.Vector a) where
    {-# INLINE oadjust #-}

    oadjust f i = VS.modify $ \v -> VSM.modify v f i


-- |
-- @since 0.1.0 
instance MonoAdjustable (ViewL a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance MonoAdjustable (ViewR a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance Arrow a => MonoAdjustable (WrappedArrow a b c) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance Monad m => MonoAdjustable (WrappedMonad m a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance Functor m => MonoAdjustable (WriterT w m a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance Functor m => MonoAdjustable (S.WriterT w m a) where
    {-# INLINE oadjust #-}

    oadjust f = const $ fmap f


-- |
-- @since 0.1.0 
instance MonoAdjustable (ZipList a) where
    {-# INLINE oadjust #-}

    oadjust = adjust


-- * MonoZip Instances


-- |
-- @since 0.1.0 
instance MonoZip (r -> a) where
    {-# INLINE ozipWith #-}

    ozipWith = zipWith


-- |
-- @since 0.1.0 
instance MonoZip [a] where
    {-# INLINE ozipWith #-}

    ozipWith = zipWith


-- |
-- @since 0.1.0 
instance MonoZip (a, b) where
    {-# INLINE ozipWith #-}

    ozipWith f (_, b1) (a, b2) = (a, f b1 b2)


-- |
-- @since 0.1.0 
instance MonoZip (Arg a b) where
    {-# INLINE ozipWith #-}

    ozipWith f (Arg _ b1) (Arg a b2) = Arg a $ f b1 b2


-- |
-- @since 0.1.0 
instance MonoZip BS.ByteString where
    {-# INLINE ozipWith #-}

    ozipWith f bs = BS.pack . BS.zipWith f bs


-- |
-- @since 0.1.0 
instance MonoZip BSL.ByteString where
    {-# INLINE ozipWith #-}

    ozipWith f bs = BSL.pack . BSL.zipWith f bs


-- |
-- @since 0.1.0 
instance ( Zip f
         , Zip g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoZip (Compose f g a) where
    {-# INLINE ozipWith #-}

    ozipWith = zipWith


-- |
-- @since 0.1.0 
instance MonoZip (Const m a) where
    {-# INLINE ozipWith #-}

    ozipWith = const $ const id


-- |
-- @since 0.1.0 
instance Functor m => MonoZip (ContT r m a) where
    {-# INLINE ozipWith #-}

    ozipWith = liftA2


-- |
-- @since 0.1.0 
instance MonoZip (Either a b) where
    {-# INLINE ozipWith #-}

    ozipWith = liftA2


-- |
-- @since 0.1.0 
instance (Eq k, Hashable k) => MonoZip (HashMap k v) where
    {-# INLINE ozipWith #-}

    ozipWith f x y = HM.intersectionWith f x y <> HM.difference x y <> HM.difference y x


-- Cannot instantiate because the zip might violate the internal structure
-- instance MonoZip IntSet


-- |
-- @since 0.1.0 
instance MonoZip (Identity a) where
    {-# INLINE ozipWith #-}

    ozipWith = zipWith


-- |
-- @since 0.1.0 
instance Applicative m => MonoZip (IdentityT m a) where
    {-# INLINE ozipWith #-}

    ozipWith = liftA2


-- |
-- @since 0.1.0 
instance MonoZip (IntMap a) where
    {-# INLINE ozipWith #-}

    ozipWith f x y = IM.intersectionWith f x y <> IM.difference x y <> IM.difference y x


-- Cannot instantiate because the zip might violate the internal structure
-- instance MonoZip IntSet


-- |
-- @since 0.1.0 
instance MonoZip (IO a) where
    {-# INLINE ozipWith #-}

    ozipWith = liftA2 


-- |
-- @since 0.1.0 
instance Applicative m => MonoZip (ListT m a) where
    {-# INLINE ozipWith #-}

    ozipWith f x y = ListT $ zipWith f <$> runListT x <*> runListT y


-- |
-- @since 0.1.0 
instance Ord k => MonoZip (Map k v) where
    {-# INLINE ozipWith #-}

    ozipWith f x y = Map.intersectionWith f x y <> Map.difference x y <> Map.difference y x


-- |
-- @since 0.1.0 
instance MonoZip (Maybe a) where
    {-# INLINE ozipWith #-}

    ozipWith = liftA2


-- |
-- @since 0.1.0 
instance Applicative m => MonoZip (MaybeT m a) where
    {-# INLINE ozipWith #-}

    ozipWith f x y = MaybeT $ liftA2 f <$> runMaybeT x <*> runMaybeT y


-- |
-- @since 0.1.0 
instance MonoZip (NonEmpty a) where
    {-# INLINE ozipWith #-}

    ozipWith f (x:|xs) (y :|ys) = f x y :| zipWith f xs ys


-- |
-- @since 0.1.0 
instance MonoZip (Option a) where
    {-# INLINE ozipWith #-}

    ozipWith = liftA2


-- |
-- @since 0.1.0 
instance ( Zip f
         , Zip g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoZip (Product f g a) where
    {-# INLINE ozipWith #-}

    ozipWith = zipWith


-- |
-- @since 0.1.0 
instance Applicative m => MonoZip (ReaderT r m a) where
    {-# INLINE ozipWith #-}

    ozipWith = liftA2


-- |
-- @since 0.1.0 
instance (Applicative m, Semigroup w) => MonoZip (RWST r w s m a) where
    {-# INLINE ozipWith #-}

    ozipWith f (RWST x) (RWST y) = RWST $ \r s ->
        let g (a1, _, w1) (a2, _, w2) = (f a1 a2, s, w1 <> w2)
        in  g <$> x r s <*> y r s


-- |
-- @since 0.1.0 
instance (Applicative m, Semigroup w) => MonoZip (S.RWST r w s m a) where
    {-# INLINE ozipWith #-}

    ozipWith f (S.RWST x) (S.RWST y) = S.RWST $ \r s ->
        let g (a1, _, w1) (a2, _, w2) = (f a1 a2, s, w1 <> w2)
        in  g <$> x r s <*> y r s


-- |
-- @since 0.1.0 
instance MonoZip (Seq a) where
    {-# INLINE ozipWith #-}

    ozipWith = zipWith


-- Cannot instantiate because the zip might violate the internal structure
-- instance MonoZip Set


-- |
-- @since 0.1.0 
instance Applicative m => MonoZip (StateT s m a) where
    {-# INLINE ozipWith #-}

    ozipWith f (StateT x) (StateT y) = StateT $ \ s ->
        let g (a1, _) (a2, _) = (f a1 a2, s)
        in  g <$> x s <*> y s


-- |
-- @since 0.1.0 
instance Applicative m => MonoZip (S.StateT s m a) where
    {-# INLINE ozipWith #-}

    ozipWith f (S.StateT x) (S.StateT y) = S.StateT $ \ s ->
        let g (a1, _) (a2, _) = (f a1 a2, s)
        in  g <$> x s <*> y s


-- |
-- @since 0.1.0 
instance MonoZip T.Text where
    {-# INLINE ozipWith #-}

    ozipWith = T.zipWith


-- |
-- @since 0.1.0 
instance MonoZip TL.Text where
    {-# INLINE ozipWith #-}

    ozipWith = TL.zipWith


-- |
-- @since 0.1.0 
instance MonoZip (Tree a) where
    {-# INLINE ozipWith #-}

    ozipWith = zipWith


-- |
-- @since 0.1.0 
instance MonoZip (Vector a) where
    {-# INLINE ozipWith #-}

    ozipWith = zipWith


-- |
-- @since 0.1.0 
instance VU.Unbox a => MonoZip (VU.Vector a) where
    {-# INLINE ozipWith #-}

    ozipWith = VU.zipWith


-- |
-- @since 0.1.0 
instance VS.Storable a => MonoZip (VS.Vector a) where
    {-# INLINE ozipWith #-}

    ozipWith = VS.zipWith


-- |
-- @since 0.1.0 
instance MonoZip (ViewL a) where
    {-# INLINE ozipWith #-}

    ozipWith _ EmptyL _ = EmptyL
    ozipWith _ _ EmptyL = EmptyL
    ozipWith f (x:<xs) (y:<ys) = f x y :< Seq.zipWith f xs ys


-- |
-- @since 0.1.0 
instance MonoZip (ViewR a) where
    {-# INLINE ozipWith #-}

    ozipWith _ EmptyR _ = EmptyR
    ozipWith _ _ EmptyR = EmptyR
    ozipWith f (xs:>x) (ys:>y) = Seq.zipWith f xs ys :> f x y


-- |
-- @since 0.1.0 
instance Arrow a => MonoZip (WrappedArrow a b c) where
    {-# INLINE ozipWith #-}

    ozipWith = liftA2


-- |
-- @since 0.1.0 
instance Monad m => MonoZip (WrappedMonad m a) where
    {-# INLINE ozipWith #-}

    ozipWith = liftA2


-- |
-- @since 0.1.0 
instance (Applicative m, Monoid w) => MonoZip (WriterT w m a) where
    {-# INLINE ozipWith #-}

    ozipWith = liftA2


-- |
-- @since 0.1.0 
instance (Applicative m, Monoid w) => MonoZip (S.WriterT w m a) where
    {-# INLINE ozipWith #-}

    ozipWith = liftA2


-- |
-- @since 0.1.0 
instance MonoZip (ZipList a) where
    {-# INLINE ozipWith #-}

    ozipWith = zipWith


-- * MonoZipWithKey


-- |
-- @since 0.1.0 
instance MonoZipWithKey (r -> a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f = zipWith (f ())


-- |
-- @since 0.1.0 
instance MonoZipWithKey [a] where
    {-# INLINE ozipWithKey #-}

    ozipWithKey = zipWithKey

-- |
-- @since 0.1.0 
instance MonoZipWithKey (a, b) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f (_, b1) (a, b2) = (a, f () b1 b2)


-- |
-- @since 0.1.0 
instance MonoZipWithKey (Arg a b) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f (Arg _ b1) (Arg a b2) = Arg a $ f () b1 b2


-- |
-- @since 0.1.0 
instance MonoZipWithKey BS.ByteString where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f bs = BS.pack . zipWithKey f (BS.unpack bs) . BS.unpack


-- |
-- @since 0.1.0 
instance MonoZipWithKey BSL.ByteString where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f bs = BSL.pack . zipWithKey f (BSL.unpack bs) . BSL.unpack


-- |
-- @since 0.1.0 
instance ( ZipWithKey f
         , ZipWithKey g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoZipWithKey (Compose f g a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey = zipWithKey


-- |
-- @since 0.1.0 
instance MonoZipWithKey (Const m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey = const $ const id


-- |
-- @since 0.1.0 
instance Functor m => MonoZipWithKey (ContT r m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f = liftA2 (f ())


-- |
-- @since 0.1.0 
instance MonoZipWithKey (Either a b) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f = liftA2 (f ())


-- |
-- @since 0.1.0 
instance (Eq k, Hashable k) => MonoZipWithKey (HashMap k v) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f x y = HM.intersectionWithKey f x y <> HM.difference x y <> HM.difference y x


-- Cannot instantiate because the map might violate the internal structure
-- instance MonoZipWithKey (HashSet v)


-- |
-- @since 0.1.0 
instance MonoZipWithKey (Identity a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey = zipWithKey


-- |
-- @since 0.1.0 
instance Applicative m => MonoZipWithKey (IdentityT m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f = liftA2 (f ())


-- |
-- @since 0.1.0 
instance MonoZipWithKey (IntMap a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f x y = IM.intersectionWithKey f x y <> IM.difference x y <> IM.difference y x


-- Cannot instantiate because the map might violate the internal structure
-- instance MonoZipWithKey IntSet


-- |
-- @since 0.1.0 
instance MonoZipWithKey (IO a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f = liftA2 (f ())


-- |
-- @since 0.1.0 
instance Applicative m => MonoZipWithKey (ListT m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f x y = ListT $ zipWithKey f <$> runListT x <*> runListT y


-- |
-- @since 0.1.0 
instance Ord k => MonoZipWithKey (Map k v) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f x y = Map.intersectionWithKey f x y <> Map.difference x y <> Map.difference y x


-- |
-- @since 0.1.0 
instance MonoZipWithKey (Maybe a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f = liftA2 (f ())


-- |
-- @since 0.1.0 
instance Monad m => MonoZipWithKey (MaybeT m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f = liftA2 (f ())


-- |
-- @since 0.1.0 
instance MonoZipWithKey (NonEmpty a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey = zipWithKey


-- |
-- @since 0.1.0 
instance MonoZipWithKey (Option a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f = liftA2 (f ())


-- |
-- @since 0.1.0 
instance ( ZipWithKey f
         , ZipWithKey g
         , MonoKey (f a) ~ Key f
         , MonoKey (g a) ~ Key g
         ) => MonoZipWithKey (Product f g a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey = zipWithKey


-- |
-- @since 0.1.0 
instance (Applicative m, ZipWithKey m) => MonoZipWithKey (ReaderT r m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey = zipWithKey


-- |
-- @since 0.1.0 
instance (Applicative m, Semigroup w) => MonoZipWithKey (RWST r w s m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f (RWST x) (RWST y) = RWST $ \r s ->
        let g (a1, _, w1) (a2, _, w2) = (f () a1 a2, s, w1 <> w2)
        in  g <$> x r s <*> y r s


-- |
-- @since 0.1.0 
instance (Applicative m, Semigroup w) => MonoZipWithKey (S.RWST r w s m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f (S.RWST x) (S.RWST y) = S.RWST $ \r s ->
        let g (a1, _, w1) (a2, _, w2) = (f () a1 a2, s, w1 <> w2)
        in  g <$> x r s <*> y r s


-- |
-- @since 0.1.0 
instance MonoZipWithKey (Seq a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey = zipWithKey


-- Cannot instantiate because the map might violate the internal structure
-- instance MonoZipWithKey Set


-- |
-- @since 0.1.0 
instance Applicative m => MonoZipWithKey (StateT s m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f (StateT x) (StateT y) = StateT $ \ s ->
        let g (a1, _) (a2, _) = (f () a1 a2, s)
        in  g <$> x s <*> y s


-- |
-- @since 0.1.0 
instance Applicative m => MonoZipWithKey (S.StateT s m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f (S.StateT x) (S.StateT y) = S.StateT $ \ s ->
        let g (a1, _) (a2, _) = (f () a1 a2, s)
        in  g <$> x s <*> y s


-- |
-- @since 0.1.0 
instance MonoZipWithKey T.Text where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f ts = T.pack . zipWithKey f (T.unpack ts) . T.unpack


-- |
-- @since 0.1.0 
instance MonoZipWithKey TL.Text where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f ts = TL.pack . zipWithKey f (TL.unpack ts) . TL.unpack


-- |
-- @since 0.1.0 
instance MonoZipWithKey (Tree a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey = zipWithKey


-- |
-- @since 0.1.0 
instance MonoZipWithKey (Vector a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey = V.izipWith


-- |
-- @since 0.1.0 
instance VU.Unbox a => MonoZipWithKey (VU.Vector a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey = VU.izipWith


-- |
-- @since 0.1.0 
instance VS.Storable a => MonoZipWithKey (VS.Vector a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey = VS.izipWith


-- |
-- @since 0.1.0 
instance MonoZipWithKey (ViewL a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey _ EmptyL _ = EmptyL
    ozipWithKey _ _ EmptyL = EmptyL
    ozipWithKey f (x:<xs) (y:<ys) = f () x y :< Seq.fromList (zipWith (f ()) (toList xs) (toList ys))


-- |
-- @since 0.1.0 
instance MonoZipWithKey (ViewR a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey _ EmptyR _ = EmptyR
    ozipWithKey _ _ EmptyR = EmptyR
    ozipWithKey f (xs:>x) (ys:>y) = Seq.fromList (zipWith (f ()) (toList xs) (toList ys)) :> f () x y


-- |
-- @since 0.1.0 
instance Arrow a => MonoZipWithKey (WrappedArrow a b c) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f = liftA2 $ f ()


-- |
-- @since 0.1.0 
instance Monad m => MonoZipWithKey (WrappedMonad m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f = liftA2 $ f ()


-- |
-- @since 0.1.0 
instance (Applicative m, Monoid w) => MonoZipWithKey (WriterT w m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f = liftA2 $ f ()


-- |
-- @since 0.1.0 
instance (Applicative m, Monoid w) => MonoZipWithKey (S.WriterT w m a) where
    {-# INLINE ozipWithKey #-}

    ozipWithKey f = liftA2 $ f ()


-- |
-- @since 0.1.0 
instance MonoZipWithKey (ZipList a) where
    {-# INLINE ozipWithKey #-}
  
    ozipWithKey = zipWithKey


-- * Unwraping functions


-- |
-- @since 0.1.0
--
-- A strict left fold, together with an unwrap function.
--
-- This is convenient when the accumulator value is not the same as the final
-- expected type. It is provided mainly for integration with the
-- @[foldl](http://hackage.haskell.org/package/foldl)@
-- package, to be used in conjunction with
-- @[purely](hackage.haskell.org/package/foldl/docs/Conrtol-Foldl.html#v:purely).@
ofoldlWithKeyUnwrap :: MonoFoldableWithKey mono
             => (x -> Element mono -> x) -> x -> (x -> b) -> mono -> b
ofoldlWithKeyUnwrap f x unwrap mono = unwrap (ofoldl' f x mono)

-- |
-- @since 0.1.0
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
  :: ( Integral i, MonoFoldable mono)
  => (a -> i -> Element mono -> a) -> a -> mono -> a
monoFoldableWithIntegralKey f z = (`S.evalState` 0) . ofoldlM g z
  where
    g a e = do
        !k <- S.get
        S.modify succ
        pure $ f a k e


monoTraversableWithUnitKey
  :: (Applicative f, MonoTraversable mono)
  => (() -> Element mono -> f (Element mono)) -> mono -> f mono
monoTraversableWithUnitKey f = otraverse (f ())


monoLookupFoldable :: (Integral i, MonoFoldable mono) => i -> mono -> Maybe (Element mono)
monoLookupFoldable i t
  | i < 0 = Nothing
  | otherwise = go i $ otoList t
  where
    go  _    []  = Nothing
    go  0   [x]  = Just x
    go !n (_:xs) = go (n-1) xs
