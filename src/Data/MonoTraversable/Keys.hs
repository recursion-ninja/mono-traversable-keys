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
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE DefaultSignatures       #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}

module Data.MonoTraversable.Keys where

import           Control.Applicative
import           Control.Category
import           Control.Comonad.Cofree 
import           Control.Monad        (Monad (..), liftM)
import           Control.Monad.Free
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Foldable           (Foldable)
import qualified Data.Foldable        as F
import           Data.Functor
import           Data.Key
import           Data.Maybe           (fromMaybe)
import           Data.Monoid (Monoid (..), Any (..), All (..))
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Data.Traversable
import           Data.Word            (Word8)
import Data.Int (Int, Int64)
import           GHC.Exts             (build)
import           Prelude              (Bool (..), Bounded(..), const, Char, Enum(..), flip, IO, Maybe (..), Either (..),
                                       (+), Integral, Ordering (..), compare, fromIntegral, Num, (>=),
                                       (==), seq, otherwise, Eq, Ord, (-), (*), uncurry, ($), snd)
import qualified Prelude
import qualified Data.ByteString.Internal as Unsafe
import qualified Foreign.ForeignPtr.Unsafe as Unsafe
import           Foreign.Ptr (plusPtr)
import           Foreign.ForeignPtr (touchForeignPtr)
import           Foreign.Storable (peek)
import           Control.Arrow (Arrow)
import           Data.Tree (Tree (..))
import           Data.Sequence (Seq, ViewL (..), ViewR (..))
import qualified Data.Sequence as Seq
import           Data.IntMap (IntMap)
import qualified Data.IntMap   as IM
import           Data.IntSet (IntSet)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty)
import           Data.Functor.Identity (Identity)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Vector (Vector)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Control.Monad.Trans.List (ListT(..))
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as S (WriterT)
import           Control.Monad.Trans.State (StateT(..))
import qualified Control.Monad.Trans.State.Strict as S (StateT(..), State, get, modify, evalState)
import           Control.Monad.Trans.RWS (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict as S (RWST(..))
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Cont (ContT)
import           Data.Functor.Compose (Compose)
import           Data.Functor.Product (Product)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.Hashable (Hashable)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as VS
import qualified Data.IntSet as IntSet
import           Data.Semigroup (Semigroup, Option (..), Arg)
import qualified Data.ByteString.Unsafe as SU
import           Data.Proxy
import           Data.Tagged
import           Data.Void
import           Control.Monad.Trans.Identity (IdentityT)
import           GHC.Generics
import           Data.MonoTraversable (Element, MonoFunctor(..), MonoFoldable(..), MonoTraversable(..))
import           Data.Vector.Instances
import           Data.Semigroup (Dual(..), Endo(..))


-- | 
-- Type family for getting the type of the key of a monomorphic container.
type family MonoKey key

-- Type instances

type instance MonoKey (r -> a)             = ()
type instance MonoKey [a]                  = Int
type instance MonoKey (a, b)               = ()
type instance MonoKey ((g :.: f) a)        = Key (f :.: g)
type instance MonoKey ((f :*: g) a)        = Key (f :*: g)
type instance MonoKey ((f :+: g) a)        = Key (f :+: g)
type instance MonoKey (Arg a b)            = ()
type instance MonoKey BS.ByteString        = Int
type instance MonoKey BSL.ByteString       = Int
type instance MonoKey (Cofree f a)         = Key (Cofree f)
type instance MonoKey (Const m a)          = ()
type instance MonoKey (ContT r m a)        = ()
type instance MonoKey (Compose f g a)      = ()
type instance MonoKey (Either a b)         = ()
type instance MonoKey (Free f a)           = Key (Free f)
type instance MonoKey (HashMap k v)        = k
type instance MonoKey (HashSet e)          = Int
type instance MonoKey (Identity a)         = ()
type instance MonoKey (IdentityT m a)      = ()
type instance MonoKey (IntMap a)           = Int
type instance MonoKey IntSet               = Int
type instance MonoKey (IO a)               = ()
type instance MonoKey (K1 i c a)           = Key (K1 i c)
type instance MonoKey (ListT m a)          = Int
type instance MonoKey (Map k v)            = k
type instance MonoKey (Maybe a)            = ()
type instance MonoKey (MaybeT m a)         = ()
type instance MonoKey (M1 i c f a)         = Key (M1 i c f)
type instance MonoKey (NonEmpty a)         = Key NonEmpty
type instance MonoKey (Option a)           = ()
type instance MonoKey (Par1 a)             = ()
type instance MonoKey (Product f g a)      = ()
type instance MonoKey (Proxy a)            = Void
type instance MonoKey (ReaderT r m a)      = ()
type instance MonoKey (Rec1 f a)           = Key (Rec1 f)
type instance MonoKey (RWST r w s m a)     = ()
type instance MonoKey (S.RWST r w s m a)   = ()
type instance MonoKey (Seq a)              = Int
type instance MonoKey (Set e)              = Int
type instance MonoKey (StateT s m a)       = ()
type instance MonoKey (S.StateT s m a)     = ()
type instance MonoKey (Tagged a b)         = ()
type instance MonoKey T.Text               = Int
type instance MonoKey TL.Text              = Int
type instance MonoKey (Tree a)             = Seq Int
type instance MonoKey (U1 a)               = Void
type instance MonoKey (V1 a)               = Void
type instance MonoKey (Vector a)           = Int
type instance MonoKey (U.Vector a)         = Int
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
-- Monomorphic containers that can be folded over thier pairs of elements and
-- corresponding keys.
class MonoFoldable mono => MonoFoldableWithKey mono where
    {-# MINIMAL ofoldMapWithKey | ofoldrWithKey #-}

    otoKeyedList :: mono -> [(MonoKey mono, Element mono)]
    otoKeyedList = ofoldrWithKey (\k v t -> (k,v):t) []

    ofoldMapWithKey :: Monoid m => (MonoKey mono -> Element mono -> m) -> mono -> m
    ofoldMapWithKey f = ofoldrWithKey (\k v -> mappend (f k v)) mempty

    ofoldrWithKey :: (MonoKey mono -> Element mono -> a -> a) -> a -> mono -> a
    ofoldrWithKey f z t = appEndo (ofoldMapWithKey (\k v -> Endo (f k v)) t) z

    ofoldlWithKey :: (a -> MonoKey mono -> Element mono -> a) -> a -> mono -> a
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

    -- |
    -- Like 'otraverse' but with a Monad constraint.
    {-# INLINE omapWithKeyM #-}
    omapWithKeyM :: Monad m => (MonoKey mono -> Element mono -> m (Element mono)) -> mono-> m mono
    omapWithKeyM f = unwrapMonad . otraverseWithKey (fmap WrapMonad . f)


-- * Instances


instance MonoTraversableWithKey BS.ByteString where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}
  
    otraverseWithKey f = fmap BS.pack . traverseWithKey f . BS.unpack

    omapWithKeyM f = liftM BS.pack . mapWithKeyM f . BS.unpack


instance MonoTraversableWithKey BSL.ByteString where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f = fmap BSL.pack . traverseWithKey f . BSL.unpack

    omapWithKeyM f = liftM BSL.pack . mapWithKeyM f . BSL.unpack


instance MonoTraversableWithKey T.Text where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f = fmap T.pack . traverseWithKey f . T.unpack

    omapWithKeyM f = liftM T.pack . mapWithKeyM f . T.unpack


instance MonoTraversableWithKey TL.Text where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f = fmap TL.pack . traverseWithKey f . TL.unpack

    omapWithKeyM f = liftM TL.pack . mapWithKeyM f . TL.unpack


instance MonoTraversableWithKey [a]


instance MonoTraversableWithKey (Maybe a)


instance MonoTraversableWithKey (Tree a)


instance MonoTraversableWithKey (Seq a)


instance MonoTraversableWithKey (ViewL a)


instance MonoTraversableWithKey (ViewR a)


instance MonoTraversableWithKey (IntMap a)


instance MonoTraversableWithKey (Option a)


instance MonoTraversableWithKey (NonEmpty a)


instance MonoTraversableWithKey (Identity a)


instance MonoTraversableWithKey (Map k v)


instance MonoTraversableWithKey (HashMap k v)


instance MonoTraversableWithKey (Vector a)


instance U.Unbox a => MonoTraversableWithKey (U.Vector a) where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f v = fmap (U.fromListN (U.length v)) . traverseWithKey f $ U.toList v

    omapWithKeyM = otraverseWithKey


instance VS.Storable a => MonoTraversableWithKey (VS.Vector a) where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey f v = fmap (VS.fromListN (VS.length v)) . traverseWithKey f $ VS.toList v

    omapWithKeyM = otraverseWithKey


instance MonoTraversableWithKey (Either a b) where
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}

    otraverseWithKey _ (Left  a) = pure $ Left a
    otraverseWithKey f (Right b) = fmap Right $ f () b

    omapWithKeyM = otraverseWithKey


instance MonoTraversableWithKey (a, b)


instance MonoTraversableWithKey (Const m a)


instance Traversable f => MonoTraversableWithKey (MaybeT f a)


instance Traversable f => MonoTraversableWithKey (ListT f a)


instance Traversable f => MonoTraversableWithKey (IdentityT f a)


instance Traversable f => MonoTraversableWithKey (WriterT w f a)


instance Traversable f => MonoTraversableWithKey (S.WriterT w f a)


instance (Traversable f, Traversable g) => MonoTraversableWithKey (Compose f g a)


instance (Traversable f, Traversable g) => MonoTraversableWithKey (Product f g a)


omapWithUnitKey f = omap (f ())


instance MonoKeyed BS.ByteString where
    {-# INLINE omapWithKey #-}

    omapWithKey f = snd . BS.mapAccumL g 0
      where
        g k v = (succ k, f k v)


instance MonoKeyed BSL.ByteString where
    {-# INLINE omapWithKey #-}

    omapWithKey f = snd . BSL.mapAccumL g 0
      where
        g k v = (succ k, f k v)


instance MonoKeyed T.Text where
    {-# INLINE omapWithKey #-}

    omapWithKey f = snd . T.mapAccumL g 0
      where
        g k v = (succ k, f k v)


instance MonoKeyed TL.Text where
    {-# INLINE omapWithKey #-}

    omapWithKey f = snd . TL.mapAccumL g 0
      where
        g k v = (succ k, f k v)


instance MonoKeyed [a]


instance MonoKeyed (IO a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance MonoKeyed (ZipList a)


instance MonoKeyed (Maybe a)


instance MonoKeyed (Tree a)


instance MonoKeyed (Seq a)


instance MonoKeyed (ViewL a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance MonoKeyed (ViewR a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance MonoKeyed (IntMap a)


instance MonoKeyed (Option a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance MonoKeyed (NonEmpty a)


instance MonoKeyed (Identity a)


instance MonoKeyed (r -> a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance MonoKeyed (Either a b) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance MonoKeyed (a, b) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance MonoKeyed (Const m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance Monad m => MonoKeyed (WrappedMonad m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance MonoKeyed (Map k v)


instance MonoKeyed (HashMap k v)


instance MonoKeyed (Vector a)


instance MonoKeyed (Arg a b) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance Arrow a => MonoKeyed (WrappedArrow a b c) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance Functor m => MonoKeyed (MaybeT m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance Functor m => MonoKeyed (ListT m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey f = ListT . fmap (omapWithKey f) . runListT


instance Functor m => MonoKeyed (IdentityT m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance Functor m => MonoKeyed (WriterT w m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance Functor m => MonoKeyed (S.WriterT w m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance Functor m => MonoKeyed (StateT s m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance Functor m => MonoKeyed (S.StateT s m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance Functor m => MonoKeyed (RWST r w s m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance Functor m => MonoKeyed (S.RWST r w s m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance Functor m => MonoKeyed (ReaderT r m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance Functor m => MonoKeyed (ContT r m a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance (Functor f, Functor g) => MonoKeyed (Compose f g a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance (Functor f, Functor g) => MonoKeyed (Product f g a) where
    {-# INLINE omapWithKey #-}

    omapWithKey = omapWithUnitKey


instance U.Unbox a => MonoKeyed (U.Vector a) where

    {-# INLINE omapWithKey #-}
    omapWithKey = U.imap


instance VS.Storable a => MonoKeyed (VS.Vector a) where

    {-# INLINE omapWithKey #-}
    omapWithKey = VS.imap

{-
-- |
-- @'replaceElem' old new@ replaces all @old@ elements with @new@.
--
-- @since 1.0.1
replaceElem :: (MonoFunctor mono, Eq (Element mono)) => Element mono -> Element mono -> mono -> mono
replaceElem old new = omapWithKey (\x -> if x == old then new else x)

{-# INLINE [0] replaceElem #-}
{-# RULES "strict Text replaceElem" replaceElem = replaceElemStrictText #-}
replaceElemStrictText :: Char -> Char -> T.Text -> T.Text
replaceElemStrictText old new = T.replace (T.singleton old) (T.singleton new)
{-# RULES "lazy Text replaceElem" replaceElem = replaceElemLazyText #-}
replaceElemLazyText :: Char -> Char -> TL.Text -> TL.Text
replaceElemLazyText old new = TL.replace (TL.singleton old) (TL.singleton new)





-- | 'ofor' is 'otraverseWithKey' with its arguments flipped.
ofor :: (MonoTraversable mono, Applicative f) => mono -> (Element mono -> f (Element mono)) -> f mono
ofor = flip otraverseWithKey
{-# INLINE ofor #-}

-- | 'oforM' is 'omapWithKeyM' with its arguments flipped.
#if MIN_VERSION_base(4,8,0)
oforM :: (MonoTraversable mono, Applicative f) => mono -> (Element mono -> f (Element mono)) -> f mono
#else
oforM :: (MonoTraversable mono, Monad f) => mono -> (Element mono -> f (Element mono)) -> f mono
#endif
oforM = flip omapWithKeyM
{-# INLINE oforM #-}
-}


-- |
-- A strict left fold, together with an unwrap function.
--
-- This is convenient when the accumulator value is not the same as the final
-- expected type. It is provided mainly for integration with the @foldl@
-- package, to be used in conjunction with @purely@.
--
-- Since 0.3.1
ofoldlWithKeyUnwrap :: MonoFoldableWithKey mono
             => (x -> Element mono -> x) -> x -> (x -> b) -> mono -> b
ofoldlWithKeyUnwrap f x unwrap mono = unwrap (ofoldl' f x mono)

-- | A monadic strict left fold, together with an unwrap function.
--
-- Similar to 'foldlUnwrap', but allows monadic actions. To be used with
-- @impurely@ from @foldl@.
--
-- Since 0.3.1
ofoldWithKeyMUnwrap :: (Monad m, MonoFoldableWithKey mono)
             => (x -> Element mono -> m x) -> m x -> (x -> m b) -> mono -> m b
ofoldWithKeyMUnwrap f mx unwrap mono = do
    x <- mx
    x' <- ofoldlM f x mono
    unwrap x'


monoFoldableWithIntegralKey
  :: ( Bounded i
     , Enum i
     , MonoFoldable mono
     )
  => (a -> i -> Element mono -> a) -> a -> mono -> a
monoFoldableWithIntegralKey f z = (`S.evalState` minBound) . ofoldlM g z
  where 
    g a e = do
        k <- S.get
        S.modify succ
        return $ f a k e


monoFoldableWithUnitKey f = ofoldMap (f ())


instance MonoFoldableWithKey BS.ByteString where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey  = monoFoldableWithIntegralKey


instance MonoFoldableWithKey BSL.ByteString where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey  = monoFoldableWithIntegralKey


instance MonoFoldableWithKey T.Text where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


instance MonoFoldableWithKey TL.Text where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


instance MonoFoldableWithKey [a] where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey
        
        
instance MonoFoldableWithKey (Maybe a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey
    

instance MonoFoldableWithKey (Tree a)


instance MonoFoldableWithKey (Seq a) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = Seq.foldMapWithIndex

    ofoldrWithKey   = Seq.foldrWithIndex

    ofoldlWithKey   = Seq.foldlWithIndex


instance MonoFoldableWithKey (ViewL a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


instance MonoFoldableWithKey (ViewR a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


instance MonoFoldableWithKey (IntMap a) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = IM.foldMapWithKey

    ofoldrWithKey   = IM.foldrWithKey

    ofoldlWithKey   = IM.foldlWithKey'


instance MonoFoldableWithKey IntSet where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


instance MonoFoldableWithKey (Option a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


instance MonoFoldableWithKey (NonEmpty a) where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


instance MonoFoldableWithKey (Identity a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


instance MonoFoldableWithKey (Map k v) where
    {-# INLINE ofoldMapWithKey #-}
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldMapWithKey = Map.foldMapWithKey

    ofoldrWithKey   = Map.foldrWithKey

    ofoldlWithKey   = Map.foldlWithKey'


instance MonoFoldableWithKey (HashMap k v) where
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldrWithKey   = HM.foldrWithKey

    ofoldlWithKey   = HM.foldlWithKey'


instance MonoFoldableWithKey (HashSet v) where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


instance MonoFoldableWithKey (Vector a) where
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldrWithKey   = V.ifoldr

    ofoldlWithKey   = V.ifoldl'


instance Ord e => MonoFoldableWithKey (Set e) where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


instance U.Unbox a => MonoFoldableWithKey (U.Vector a) where
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldrWithKey   = U.ifoldr

    ofoldlWithKey   = U.ifoldl'


instance VS.Storable a => MonoFoldableWithKey (VS.Vector a) where
    {-# INLINE ofoldrWithKey #-}
    {-# INLINE ofoldlWithKey #-}

    ofoldrWithKey   = VS.ifoldr

    ofoldlWithKey   = VS.ifoldl'


instance MonoFoldableWithKey (Either a b) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


instance MonoFoldableWithKey (a, b) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


instance MonoFoldableWithKey (Const m a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


instance Foldable f => MonoFoldableWithKey (MaybeT f a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


instance Foldable f => MonoFoldableWithKey (ListT f a) where
    {-# INLINE ofoldlWithKey #-}

    ofoldlWithKey   = monoFoldableWithIntegralKey


instance Foldable f => MonoFoldableWithKey (IdentityT f a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


instance Foldable f => MonoFoldableWithKey (WriterT w f a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


instance Foldable f => MonoFoldableWithKey (S.WriterT w f a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


instance (Foldable f, Foldable g) => MonoFoldableWithKey (Compose f g a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey


instance (Foldable f, Foldable g) => MonoFoldableWithKey (Product f g a) where
    {-# INLINE ofoldMapWithKey #-}

    ofoldMapWithKey = monoFoldableWithUnitKey
