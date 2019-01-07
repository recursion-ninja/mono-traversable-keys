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
{-# LANGUAGE UndecidableInstances    #-}

module Data.MonoTraversable.Keys where

import           Control.Applicative
import           Control.Category
#if MIN_VERSION_base(4,8,0)
import           Control.Monad        (Monad (..))
#else
import           Control.Monad        (Monad (..), liftM)
#endif
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
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
import           Prelude              (Bool (..), const, Char, flip, IO, Maybe (..), Either (..),
                                       (+), Integral, Ordering (..), compare, fromIntegral, Num, (>=),
                                       (==), seq, otherwise, Eq, Ord, (-), (*))
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
import           Data.IntSet (IntSet)
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty)
import           Data.Functor.Identity (Identity)
import           Data.Map (Map)
import qualified Data.Map.Strict as Map
import           Data.HashMap.Strict (HashMap)
import           Data.Vector (Vector)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Control.Monad.Trans.List (ListT)
import           Control.Monad.Trans.Writer (WriterT)
import qualified Control.Monad.Trans.Writer.Strict as S (WriterT)
import           Control.Monad.Trans.State (StateT(..))
import qualified Control.Monad.Trans.State.Strict as S (StateT(..))
import           Control.Monad.Trans.RWS (RWST(..))
import qualified Control.Monad.Trans.RWS.Strict as S (RWST(..))
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.Cont (ContT)
import           Data.Functor.Compose (Compose)
import           Data.Functor.Product (Product)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Hashable (Hashable)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as VS
import qualified Data.IntSet as IntSet
import           Data.Semigroup (Semigroup, Option (..), Arg)
import qualified Data.ByteString.Unsafe as SU
import           Control.Monad.Trans.Identity (IdentityT)


-- | 
-- Type family for getting the type of the key of a monomorphic container.
type family MonoKey (f :: * -> *)

-- Type instances

type instance MonoKey (r -> a)             = ()
type instance MonoKey [a]                  = Int
type instance MonoKey (a, b)               = ()
type instance MonoKey (g :.: f)            = (MonoKey g, MonoKey f)
type instance MonoKey (f :*: g)            = Either (MonoKey f) (MonoKey g)
type instance MonoKey (f :+: g)            = Either (MonoKey f) (MonoKey g)
type instance MonoKey (Arg a b)            = ()
type instance MonoKey BS.ByteString        = Int
type instance MonoKey BSL.ByteString       = Int
type instance MonoKey (Cofree f)           = Seq (MonoKey f)
type instance MonoKey (Const m a)          = ()
type instance MonoKey (ContT r m a)        = ()
type instance MonoKey (Compose f g a)      = ()
type instance MonoKey (Either a b)         = ()
type instance MonoKey (Free f)             = Seq (MonoKey f)
type instance MonoKey (HashMap k v)        = k
type instance MonoKey (HashSet e)          = Int
type instance MonoKey (Identity a)         = ()
type instance MonoKey (IdentityT m a)      = ()
type instance MonoKey (IntMap a)           = Int
type instance MonoKey IntSet               = Int
type instance MonoKey (IO a)               = ()
type instance MonoKey (K1 i c)             = Void
type instance MonoKey (ListT m a)          = Int
type instance MonoKey (Map k v)            = k
type instance MonoKey (Maybe a)            = ()
type instance MonoKey (MaybeT m a)         = ()
type instance MonoKey (M1 i c f)           = MonoKey f
type instance MonoKey NonEmpty             = Int
type instance MonoKey (Option a)           = ()
type instance MonoKey Par1                 = ()
type instance MonoKey (Product f g a)      = ()
type instance MonoKey Proxy                = Void
type instance MonoKey (Tagged a)           = ()
type instance MonoKey (ReaderT r m a)      = ()
type instance MonoKey (Rec1 f)             = MonoKey f
type instance MonoKey (RWST r w s m a)     = ()
type instance MonoKey (S.RWST r w s m a)   = ()
type instance MonoKey (Seq a)              = Int
type instance MonoKey (Set e)              = Int
type instance MonoKey (StateT s m a)       = ()
type instance MonoKey (S.StateT s m a)     = ()
type instance MonoKey T.Text               = Int
type instance MonoKey TL.Text              = Int
type instance MonoKey Tree                 = Seq Int
type instance MonoKey U1                   = Void
type instance MonoKey V1                   = Void
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
    ozipWith f a b = uncurry f <$> ozip a b


-- |
-- Monomorphic container that can be zipped together, merging thier pairs of 
-- elements and corresponding keys.
class (MonoKeyed mono, MonoZip mono) => MonoZipWithKey mono where
    {-# MINIMAL ozipWithKey #-}

    ozipWithKey :: (MonoKey mono -> Element mono -> Element mono -> Element mono) -> mono -> mono -> mono
    ozipWithKey f = ozap . omapWithKey f



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

    toKeyedList :: t a -> [(MonoKey mono, Element mono)]
    toKeyedList = ofoldrWithKey (\k v t -> (k,v):t) []

    foldMapWithKey :: Monoid m => (MonoKey mono -> Element mono -> m) -> mono -> m
    foldMapWithKey f = ofoldrWithKey (\k v -> mappend (f k v)) mempty

    foldrWithKey :: (MonoKey mono -> Element mono -> a -> a) -> a -> mono -> a
    foldrWithKey f z t = appEndo (ofoldMapWithKey (\k v -> Endo (f k v)) t) z

    foldlWithKey :: (a -> MonoKey mono -> Element mono -> a) -> a -> mono -> a
    foldlWithKey f z t = appEndo (getDual (ofoldMapWithKey (\k a -> Dual (Endo (\b -> f b k a))) t)) z


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
    {-# INLINE otraverseWithKey #-}
    otraverseWithKey :: Applicative f => (MonoKey mono -> Element mono -> f (Element mono)) -> mono -> f mono

    -- |
    -- Like 'otraverse' but with a Monad constraint.
    {-# INLINE omapWithKeyM #-}
    omapWithKeyM :: Monad m => (MonoKey mono -> Element mono -> m (Element mono)) -> mono-> m mono
    omapWithKeyM f = unwrapMonad . otraverseWithKey (fmap WrapMonad . f)


-- * Instances


instance MonoTraversableWithKey BS.ByteString where

    otraverseWithKey f = fmap BS.pack . traverse f . BS.unpack
    {-# INLINE otraverseWithKey #-}
#if !MIN_VERSION_base(4,8,0)
    omapWithKeyM f = liftM BS.pack . mapM f . BS.unpack
    {-# INLINE omapWithKeyM #-}
#endif


instance MonoTraversableWithKey BSL.ByteString where

    otraverseWithKey f = fmap BSL.pack . traverse f . BSL.unpack
    {-# INLINE otraverseWithKey #-}
#if !MIN_VERSION_base(4,8,0)
    omapWithKeyM f = liftM BSL.pack . mapM f . BSL.unpack
#endif


instance MonoTraversableWithKey T.Text where

    otraverseWithKey f = fmap T.pack . traverse f . T.unpack
    {-# INLINE otraverseWithKey #-}
#if !MIN_VERSION_base(4,8,0)
    omapWithKeyM f = liftM T.pack . mapM f . T.unpack
    {-# INLINE omapWithKeyM #-}
#endif


instance MonoTraversableWithKey TL.Text where

    otraverseWithKey f = fmap TL.pack . traverse f . TL.unpack
    {-# INLINE otraverseWithKey #-}
#if !MIN_VERSION_base(4,8,0)
    omapWithKeyM f = liftM TL.pack . mapM f . TL.unpack
    {-# INLINE omapWithKeyM #-}
#endif


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

    -- FIXME do something more efficient
    otraverseWithKey f = fmap U.fromList . traverse f . U.toList
#if MIN_VERSION_base(4,8,0)
    omapWithKeyM = otraverseWithKey
#else
    omapWithKeyM = U.mapM
#endif
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}


instance VS.Storable a => MonoTraversableWithKey (VS.Vector a) where

    -- FIXME do something more efficient
    otraverseWithKey f = fmap VS.fromList . traverse f . VS.toList
#if MIN_VERSION_base(4,8,0)
    omapWithKeyM = otraverseWithKey
#else
    omapWithKeyM = VS.mapM
#endif
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}
instance MonoTraversableWithKey (Either a b) where
    otraverseWithKey _ (Left a) = pure (Left a)
    otraverseWithKey f (Right b) = fmap Right (f b)
#if MIN_VERSION_base(4,8,0)
    omapWithKeyM _ (Left a) = pure (Left a)
    omapWithKeyM f (Right b) = fmap Right (f b)
#else
    omapWithKeyM _ (Left a) = return (Left a)
    omapWithKeyM f (Right b) = liftM Right (f b)
#endif
    {-# INLINE otraverseWithKey #-}
    {-# INLINE omapWithKeyM #-}


instance MonoTraversableWithKey (a, b)


instance MonoTraversableWithKey (Const m a)


instance Traversable f => MonoTraversableWithKey (MaybeT f a)


instance Traversable f => MonoTraversableWithKey (ListT f a)


instance Traversable f => MonoTraversableWithKey (IdentityT f a)


instance Traversable f => MonoTraversableWithKey (WriterT w f a)


instance Traversable f => MonoTraversableWithKey (S.WriterT w f a)


instance (Traversable f, Traversable g) => MonoTraversableWithKey (Compose f g a)


instance (Traversable f, Traversable g) => MonoTraversableWithKey (Product f g a)




instance MonoKeyed BS.ByteString where

    {-# INLINE omapWithKey #-}
    omapWithKey = BS.map


instance MonoKeyed BSL.ByteString where

    {-# INLINE omapWithKey #-}
    omapWithKey = BSL.map


instance MonoKeyed T.Text where

    {-# INLINE omapWithKey #-}
    omapWithKey = T.map


instance MonoKeyed TL.Text where

    {-# INLINE omapWithKey #-}
    omapWithKey = TL.map


instance MonoKeyed [a]


instance MonoKeyed (IO a)


instance MonoKeyed (ZipList a)


instance MonoKeyed (Maybe a)


instance MonoKeyed (Tree a)


instance MonoKeyed (Seq a)


instance MonoKeyed (ViewL a)


instance MonoKeyed (ViewR a)


instance MonoKeyed (IntMap a)


instance MonoKeyed (Option a)


instance MonoKeyed (NonEmpty a)


instance MonoKeyed (Identity a)


instance MonoKeyed (r -> a)


instance MonoKeyed (Either a b)


instance MonoKeyed (a, b)


instance MonoKeyed (Const m a)


instance Monad m => MonoKeyed (WrappedMonad m a)


instance MonoKeyed (Map k v)


instance MonoKeyed (HashMap k v)


instance MonoKeyed (Vector a)


instance MonoKeyed (Arg a b)


instance Arrow a => MonoKeyed (WrappedArrow a b c)


instance Functor m => MonoKeyed (MaybeT m a)


instance Functor m => MonoKeyed (ListT m a)


instance Functor m => MonoKeyed (IdentityT m a)


instance Functor m => MonoKeyed (WriterT w m a)


instance Functor m => MonoKeyed (S.WriterT w m a)


instance Functor m => MonoKeyed (StateT s m a)


instance Functor m => MonoKeyed (S.StateT s m a)


instance Functor m => MonoKeyed (RWST r w s m a)


instance Functor m => MonoKeyed (S.RWST r w s m a)


instance Functor m => MonoKeyed (ReaderT r m a)


instance Functor m => MonoKeyed (ContT r m a)


instance (Functor f, Functor g) => MonoKeyed (Compose f g a)


instance (Functor f, Functor g) => MonoKeyed (Product f g a)


instance U.Unbox a => MonoKeyed (U.Vector a) where

    {-# INLINE omapWithKey #-}
    omapWithKey = U.map


instance VS.Storable a => MonoKeyed (VS.Vector a) where

    {-# INLINE omapWithKey #-}
    omapWithKey = VS.map

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


instance MonoFoldableWithKey S.ByteString where

    ofoldMap f = ofoldr (mappend . f) mempty

    ofoldr = S.foldr

    ofoldl' = S.foldl'

    otoList = S.unpack

    oall = S.all

    oany = S.any

    onull = S.null

    olength = S.length

    oelem = S.elem

    onotElem = S.notElem

    omapWithKeyM_ f (Unsafe.PS fptr offset len) = do
        let start = Unsafe.unsafeForeignPtrToPtr fptr `plusPtr` offset
            end = start `plusPtr` len
            loop ptr
                | ptr >= end = evil (touchForeignPtr fptr) `seq`
#if MIN_VERSION_base(4,8,0)
                    pure ()
#else
                    return ()
#endif
                | otherwise =
#if MIN_VERSION_base(4,8,0)
                    f (evil (peek ptr)) *>
                    loop (ptr `plusPtr` 1)
#else
                    f (evil (peek ptr)) >>
                    loop (ptr `plusPtr` 1)
#endif
        loop start
      where
#if MIN_VERSION_bytestring(0,10,6)
        evil = Unsafe.accursedUnutterablePerformIO
#else
        evil = Unsafe.inlinePerformIO
#endif
        {-# INLINE evil #-}
    ofoldr1Ex = S.foldr1

    ofoldl1Ex' = S.foldl1'

    headEx = S.head

    lastEx = S.last

    unsafeHead = SU.unsafeHead
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE omapWithKeyM_ #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
    {-# INLINE oelem #-}
    {-# INLINE onotElem #-}
{-# RULES "strict ByteString: ofoldMap = concatMap" ofoldMap = S.concatMap #-}

instance MonoFoldableWithKey BSL.ByteString where

    ofoldMap f = ofoldr (mappend . f) mempty

    ofoldr = L.foldr

    ofoldl' = L.foldl'

    otoList = L.unpack

    oall = L.all

    oany = L.any

    onull = L.null

    olength64 = L.length

    omapWithKeyM_ f = omapWithKeyM_ (omapWithKeyM_ f) . L.toChunks

    ofoldr1Ex = L.foldr1

    ofoldl1Ex' = L.foldl1'

    headEx = L.head

    lastEx = L.last

    oelem = L.elem

    onotElem = L.notElem

    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength64 #-}
    {-# INLINE omapWithKeyM_ #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE oelem #-}
    {-# INLINE onotElem #-}
{-# RULES "lazy ByteString: ofoldMap = concatMap" ofoldMap = L.concatMap #-}

instance MonoFoldableWithKey T.Text where

    ofoldMap f = ofoldr (mappend . f) mempty

    ofoldr = T.foldr

    ofoldl' = T.foldl'

    otoList = T.unpack

    oall = T.all

    oany = T.any

    onull = T.null

    olength = T.length

    ofoldr1Ex = T.foldr1

    ofoldl1Ex' = T.foldl1'

    headEx = T.head

    lastEx = T.last
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
{-# RULES "strict Text: ofoldMap = concatMap" ofoldMap = T.concatMap #-}

instance MonoFoldableWithKey TL.Text where

    ofoldMap f = ofoldr (mappend . f) mempty

    ofoldr = TL.foldr

    ofoldl' = TL.foldl'

    otoList = TL.unpack

    oall = TL.all

    oany = TL.any

    onull = TL.null

    olength64 = TL.length

    ofoldr1Ex = TL.foldr1

    ofoldl1Ex' = TL.foldl1'

    headEx = TL.head

    lastEx = TL.last
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
{-# RULES "lazy Text: ofoldMap = concatMap" ofoldMap = TL.concatMap #-}

instance MonoFoldableWithKey IntSet where

    ofoldMap f = ofoldr (mappend . f) mempty

    ofoldr = IntSet.foldr

    ofoldl' = IntSet.foldl'

    otoList = IntSet.toList

    onull = IntSet.null

    olength = IntSet.size

    ofoldr1Ex f = ofoldr1Ex f . IntSet.toList

    ofoldl1Ex' f = ofoldl1Ex' f . IntSet.toList
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    
    
instance MonoFoldableWithKey [a] where

    otoList = id
    {-# INLINE otoList #-}

    ocompareLength [] i = 0 `compare` i
    ocompareLength (_:xs) i
        | i Prelude.<= 0 = GT
        | otherwise = ocompareLength xs (i - 1)
        
        
instance MonoFoldableWithKey (Maybe a) where

#if MIN_VERSION_base(4,8,0)
    omapWithKeyM_ _ Nothing = pure ()
#else
    omapWithKeyM_ _ Nothing = return ()
#endif
    omapWithKeyM_ f (Just x) = f x
    {-# INLINE omapWithKeyM_ #-}
    
    
instance MonoFoldableWithKey (Tree a)


instance MonoFoldableWithKey (Seq a) where

    headEx = flip Seq.index 0

    lastEx xs = Seq.index xs (Seq.length xs - 1)
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}


instance MonoFoldableWithKey (ViewL a)


instance MonoFoldableWithKey (ViewR a)


instance MonoFoldableWithKey (IntMap a)


instance MonoFoldableWithKey (Option a)


instance MonoFoldableWithKey (NonEmpty a)


instance MonoFoldableWithKey (Identity a)


instance MonoFoldableWithKey (Map k v) where

    olength = Map.size
    {-# INLINE olength #-}


instance MonoFoldableWithKey (HashMap k v)


instance MonoFoldableWithKey (Vector a) where

    ofoldr = V.foldr

    ofoldl' = V.foldl'

    otoList = V.toList

    oall = V.all

    oany = V.any

    onull = V.null

    olength = V.length

    ofoldr1Ex = V.foldr1

    ofoldl1Ex' = V.foldl1'

    headEx = V.head

    lastEx = V.last

    unsafeHead = V.unsafeHead

    unsafeLast = V.unsafeLast

    maximumByEx = V.maximumBy

    minimumByEx = V.minimumBy
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
    {-# INLINE maximumByEx #-}
    {-# INLINE minimumByEx #-}


instance Ord e => MonoFoldableWithKey (Set e) where

    olength = Set.size

    oelem = Set.member

    onotElem = Set.notMember
    {-# INLINE olength #-}
    {-# INLINE oelem #-}
    {-# INLINE onotElem #-}


instance MonoFoldableWithKey (HashSet e)


instance U.Unbox a => MonoFoldableWithKey (U.Vector a) where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = U.foldr
    ofoldl' = U.foldl'
    otoList = U.toList
    oall = U.all
    oany = U.any
    onull = U.null
    olength = U.length
    ofoldr1Ex = U.foldr1
    ofoldl1Ex' = U.foldl1'
    headEx = U.head
    lastEx = U.last
    unsafeHead = U.unsafeHead
    unsafeLast = U.unsafeLast
    maximumByEx = U.maximumBy
    minimumByEx = U.minimumBy
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
    {-# INLINE maximumByEx #-}
    {-# INLINE minimumByEx #-}


instance VS.Storable a => MonoFoldableWithKey (VS.Vector a) where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr = VS.foldr
    ofoldl' = VS.foldl'
    otoList = VS.toList
    oall = VS.all
    oany = VS.any
    onull = VS.null
    olength = VS.length
    ofoldr1Ex = VS.foldr1
    ofoldl1Ex' = VS.foldl1'
    headEx = VS.head
    lastEx = VS.last
    unsafeHead = VS.unsafeHead
    unsafeLast = VS.unsafeLast
    maximumByEx = VS.maximumBy
    minimumByEx = VS.minimumBy
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}
    {-# INLINE headEx #-}
    {-# INLINE lastEx #-}
    {-# INLINE unsafeHead #-}
    {-# INLINE maximumByEx #-}
    {-# INLINE minimumByEx #-}


instance MonoFoldableWithKey (Either a b) where
    ofoldMap f = ofoldr (mappend . f) mempty
    ofoldr f b (Right a) = f a b
    ofoldr _ b (Left _) = b
    ofoldl' f a (Right b) = f a b
    ofoldl' _ a (Left _) = a
    otoList (Left _) = []
    otoList (Right b) = [b]
    oall _ (Left _) = True
    oall f (Right b) = f b
    oany _ (Left _) = False
    oany f (Right b) = f b
    onull (Left _) = True
    onull (Right _) = False
    olength (Left _) = 0
    olength (Right _) = 1
    ofoldr1Ex _ (Left _) = Prelude.error "ofoldr1Ex on Either"
    ofoldr1Ex _ (Right x) = x
    ofoldl1Ex' _ (Left _) = Prelude.error "ofoldl1Ex' on Either"
    ofoldl1Ex' _ (Right x) = x
#if MIN_VERSION_base(4,8,0)
    omapWithKeyM_ _ (Left _) = pure ()
#else
    omapWithKeyM_ _ (Left _) = return ()
#endif
    omapWithKeyM_ f (Right x) = f x
    {-# INLINE ofoldMap #-}
    {-# INLINE ofoldr #-}
    {-# INLINE ofoldl' #-}
    {-# INLINE otoList #-}
    {-# INLINE oall #-}
    {-# INLINE oany #-}
    {-# INLINE onull #-}
    {-# INLINE olength #-}
    {-# INLINE omapWithKeyM_ #-}
    {-# INLINE ofoldr1Ex #-}
    {-# INLINE ofoldl1Ex' #-}


instance MonoFoldableWithKey (a, b)


instance MonoFoldableWithKey (Const m a)


instance MonoFoldable f => MonoFoldableWithKey (MaybeT f a)


instance MonoFoldable f => MonoFoldableWithKey (ListT f a)


instance MonoFoldable f => MonoFoldableWithKey (IdentityT f a)


instance MonoFoldable f => MonoFoldableWithKey (WriterT w f a)


instance MonoFoldable f => MonoFoldableWithKey (S.WriterT w f a)


instance (MonoFoldable f, MonoFoldable g) => MonoFoldableWithKey (Compose f g a)


instance (MonoFoldable f, MonoFoldable g) => MonoFoldableWithKey (Product f g a)


-- | Safe version of 'headEx'.
--
-- Returns 'Nothing' instead of throwing an exception when encountering
-- an empty monomorphic container.
headMay :: MonoFoldable mono => mono -> Maybe (Element mono)
headMay mono
    | onull mono = Nothing
    | otherwise = Just (headEx mono)
{-# INLINE headMay #-}

-- | Safe version of 'lastEx'.
--
-- Returns 'Nothing' instead of throwing an exception when encountering
-- an empty monomorphic container.
lastMay :: MonoFoldable mono => mono -> Maybe (Element mono)
lastMay mono
    | onull mono = Nothing
    | otherwise = Just (lastEx mono)
{-# INLINE lastMay #-}

-- | 'osum' computes the sum of the numbers of a monomorphic container.
osum :: (MonoFoldable mono, Num (Element mono)) => mono -> Element mono
osum = ofoldl' (+) 0
{-# INLINE osum #-}

-- | 'oproduct' computes the product of the numbers of a monomorphic container.
oproduct :: (MonoFoldable mono, Num (Element mono)) => mono -> Element mono
oproduct = ofoldl' (*) 1
{-# INLINE oproduct #-}

-- | Are __all__ of the elements 'True'?
--
-- Since 0.6.0
oand :: (Element mono ~ Bool, MonoFoldable mono) => mono -> Bool
oand = oall id
{-# INLINE oand #-}

-- | Are __any__ of the elements 'True'?
--
-- Since 0.6.0
oor :: (Element mono ~ Bool, MonoFoldable mono) => mono -> Bool
oor = oany id
{-# INLINE oor #-}

-- | Synonym for 'ofoldMap'
--
-- @since 1.0.0
oconcatMap :: (MonoFoldable mono, Monoid m) => (Element mono -> m) -> mono -> m
oconcatMap = ofoldMap

-- | Monoidally combine all values in the container
--
-- @since 1.0.0
ofold :: (MonoFoldable mono, Monoid (Element mono)) => mono -> Element mono
ofold = ofoldMap id
{-# INLINE ofold #-}

-- | Synonym for 'ofold'
--
-- @since 1.0.0
oconcat :: (MonoFoldable mono, Monoid (Element mono)) => mono -> Element mono
oconcat = ofold
{-# INLINE oconcat #-}

-- | Synonym for 'ofoldlM'
--
-- @since 1.0.0
ofoldM :: (MonoFoldable mono, Monad m) => (a -> Element mono -> m a) -> a -> mono -> m a
ofoldM = ofoldlM
{-# INLINE ofoldM #-}

-- | Perform all actions in the given container
--
-- @since 1.0.0
#if MIN_VERSION_base(4,8,0)
osequence_ :: (Applicative m, MonoFoldable mono, Element mono ~ (m ())) => mono -> m ()
#else
osequence_ :: (Monad m, MonoFoldable mono, Element mono ~ (m ())) => mono -> m ()
#endif
osequence_ = omapWithKeyM_ id
{-# INLINE osequence_ #-}

-- | Get the minimum element of a monomorphic container.
--
-- Note: this is a partial function. On an empty 'MonoFoldable', it will
-- throw an exception.
--
-- /See 'Data.NonNull.maximum' from "Data.NonNull" for a total version of this function./
maximumEx :: (MonoFoldable mono, Ord (Element mono)) => mono -> Element mono
maximumEx = maximumByEx compare
{-# INLINE [0] maximumEx #-}

-- | Get the maximum element of a monomorphic container.
--
-- Note: this is a partial function. On an empty 'MonoFoldable', it will
-- throw an exception.
--
-- /See 'Data.NonNull.minimum' from "Data.NonNull" for a total version of this function./
minimumEx :: (MonoFoldable mono, Ord (Element mono)) => mono -> Element mono
minimumEx = minimumByEx compare
{-# INLINE [0] minimumEx #-}

{-# RULES "strict ByteString maximumEx" maximumEx = S.maximum #-}
{-# RULES "strict ByteString minimumEx" minimumEx = S.minimum #-}

{-# RULES "lazy ByteString maximumEx" maximumEx = L.maximum #-}
{-# RULES "lazy ByteString minimumEx" minimumEx = L.minimum #-}

{-# RULES "strict Text maximumEx" maximumEx = T.maximum #-}
{-# RULES "strict Text minimumEx" minimumEx = T.minimum #-}

{-# RULES "lazy Text maximumEx" maximumEx = TL.maximum #-}
{-# RULES "lazy Text minimumEx" minimumEx = TL.minimum #-}

{-# RULES "boxed Vector maximumEx" maximumEx = V.maximum #-}
{-# RULES "boxed Vector minimumEx" minimumEx = V.minimum #-}

{-# RULES "unboxed Vector maximumEx" forall (u :: U.Unbox a => U.Vector a). maximumEx u = U.maximum u #-}
{-# RULES "unboxed Vector minimumEx" forall (u :: U.Unbox a => U.Vector a). minimumEx u = U.minimum u #-}

{-# RULES "storable Vector maximumEx" forall (v :: VS.Storable a => VS.Vector a). maximumEx v = VS.maximum v #-}
{-# RULES "storable Vector minimumEx" forall (v :: VS.Storable a => VS.Vector a). minimumEx v = VS.minimum v #-}

-- | Safe version of 'maximumEx'.
--
-- Returns 'Nothing' instead of throwing an exception when
-- encountering an empty monomorphic container.
maximumMay :: (MonoFoldable mono, Ord (Element mono)) => mono -> Maybe (Element mono)
maximumMay mono
    | onull mono = Nothing
    | otherwise = Just (maximumEx mono)
{-# INLINE maximumMay #-}

-- | Safe version of 'maximumByEx'.
--
-- Returns 'Nothing' instead of throwing an exception when
-- encountering an empty monomorphic container.
maximumByMay :: MonoFoldable mono
             => (Element mono -> Element mono -> Ordering)
             -> mono
             -> Maybe (Element mono)
maximumByMay f mono
    | onull mono = Nothing
    | otherwise = Just (maximumByEx f mono)
{-# INLINE maximumByMay #-}


-- | Safe version of 'minimumEx'.
--
-- Returns 'Nothing' instead of throwing an exception when
-- encountering an empty monomorphic container.
minimumMay :: (MonoFoldable mono, Ord (Element mono)) => mono -> Maybe (Element mono)
minimumMay mono
    | onull mono = Nothing
    | otherwise = Just (minimumEx mono)
{-# INLINE minimumMay #-}


-- | Safe version of 'minimumByEx'.
--
-- Returns 'Nothing' instead of throwing an exception when
-- encountering an empty monomorphic container.
minimumByMay :: MonoFoldable mono
             => (Element mono -> Element mono -> Ordering)
             -> mono
             -> Maybe (Element mono)
minimumByMay f mono
    | onull mono = Nothing
    | otherwise = Just (minimumByEx f mono)
{-# INLINE minimumByMay #-}

