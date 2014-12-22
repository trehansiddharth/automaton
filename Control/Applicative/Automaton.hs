module Control.Applicative.Automaton where
	import Control.Applicative
	import Data.Monoid
	import Control.Monad
	import Data.Functor.Identity

	data Automaton f i a = Automaton { runAutomaton :: [i] -> f (a, [i]) }

	instance Functor f => Functor (Automaton f i) where
		fmap f s = Automaton $ fmap (overFst f) . runAutomaton s
			where
				overFst f (x, y) = (f x, y)

	instance (Applicative f, Monad f) => Applicative (Automaton f i) where
		pure x = Automaton $ pure . (,) x
		f <*> s = Automaton $ join . fmap (uncurry $ \f' -> fmap (overFst f') . runAutomaton s) . runAutomaton f
			where
				overFst f (x, y) = (f x, y)

	unit :: (Applicative f, Monad f, Monoid (f (i, [i]))) => Automaton f i i
	unit = Automaton $ \is -> case is of
		(i:is') -> pure (i, is')
		[] -> mempty

	compute :: (Applicative f, Monad f, Monoid (f (a, [i])), Monoid (f (i, [i]))) => (i -> a) -> Automaton f i a
	compute f = f <$> unit

	branch :: (Applicative f, Monad f, Monoid (f (a, [i]))) => f (Automaton f i a) -> Automaton f i a
	branch ss = Automaton $ \is -> join $ fmap (($ is) . runAutomaton) ss

	accept :: (Applicative f, Monad f, Monoid (f (i, [i]))) => (i -> Bool) -> Automaton f i i
	accept p = Automaton $ \is -> case is of
		(i:is') -> if p i then pure (i, is') else mempty
		[] -> mempty

	computeIf :: (Applicative f, Monad f, Monoid (f (i, [i]))) => (i -> Bool) -> (i -> a) -> Automaton f i a
	computeIf p f = f <$> accept p

	end :: (Applicative f, Monad f, Monoid (f ((), [i]))) => Automaton f i ()
	end = Automaton $ \is -> case is of
		[] -> pure ((), [])
		_ -> mempty

	type FiniteStateMachine = Automaton Identity
	type PushdownAutomaton = Automaton []

	data PriorityTree a = PriorityTree { next :: Maybe (PriorityTree a), top :: [a] }
		deriving (Eq, Show)

	instance Functor PriorityTree where
		fmap f p = PriorityTree (fmap (fmap f) . next $ p) (fmap f . top $ p)

	instance Monoid (PriorityTree a) where
		mempty = PriorityTree Nothing []
		mappend x y = PriorityTree (next x <> next y) (top x <> top y)

	instance Applicative PriorityTree where
		pure = PriorityTree Nothing . pure
		f <*> x = PriorityTree (fmap (<*> x) (next f) <> (pure . mconcat . fmap (<$> x) . top $ f)) (top f <*> top x)

	instance Monad PriorityTree where
		return = pure
		x >>= f = PriorityTree (fmap (>>= f) . next $ x) [] <> (mconcat . fmap f . top $ x)

	demote :: PriorityTree a -> PriorityTree a
	demote = (flip PriorityTree) [] . pure

	toPriorityTree :: [a] -> PriorityTree a
	toPriorityTree = PriorityTree Nothing

	fromPriorityTree :: PriorityTree a -> [a]
	fromPriorityTree p = (top p) ++ case next p of
		Nothing -> []
		Just p' -> fromPriorityTree p'

	bound :: Int -> PriorityTree a -> PriorityTree a
	bound 0 p = PriorityTree Nothing (top p)
	bound k p = PriorityTree (fmap (bound (k - 1)) (next p)) (top p)

	type QueueAutomaton = Automaton PriorityTree

	step :: QueueAutomaton i ()
	step = Automaton $ demote . runAutomaton (pure ())