{-| Simple fix-expression parser

    If in doubt, just use parsefix
-}

module Parser.Fix.Simple (TokenType (..), Fixity (..), parsefix, foldfix, shunt) where

import Control.Arrow;
import Control.Monad;
import Control.Monad.State;
import Control.Monad.Writer.Lazy;
import Control.Monad.Instances;
import Data.List;
import Util;

-- | Fixity/Associativity
data Fixity = Prefix | InfixLeft | InfixRight | InfixNull | Postfix deriving Eq;

data TokenType p = -- | Plain, i.e. operand
                   TokenPlain
                 | TokenLParenth
                 | TokenRParenth
                 -- | Operator, with fixity and precedence
                 | TokenOper Fixity p;

-- | Shunt and Fold
parsefix :: Ord p =>
            (t -> a, t -> a -> a, t -> a -> a -> a) -> (t -> TokenType p) -> [t] -> a;
parsefix apply = liftM2 (.) (foldfix apply) shunt;

-- | Fold expression in postfix form
foldfix :: Ord p =>
           (t -> a, t -> a -> a, t -> a -> a -> a) -> (t -> TokenType p) -> [t] -> a;
foldfix (apply0, apply1, apply2) lookup_t =
  let {
    foldfix' stack = flip list (list const (error "Empty Stack!") stack) $ \ t ->
      case lookup_t t of {
        TokenPlain -> foldfix' (apply0 t:stack);
        TokenOper fix p | fix_arity fix == 2,
                          (x:y:stack') <- stack -> foldfix' (apply2 t x y : stack')
                        | fix_arity fix == 1,
                          (x:stack')   <- stack -> foldfix' (apply1 t x   : stack')
                        | otherwise -> const $ error "Malformed fix expression";
        TokenLParenth -> const $ error "Mismatched Parentheses";
        TokenRParenth -> const $ error "Mismatched Parentheses";
       };

    fix_arity Prefix     = 1;
    fix_arity Postfix    = 1;
    fix_arity InfixLeft  = 2;
    fix_arity InfixRight = 2;
    fix_arity InfixNull  = 2;
  } in foldfix' [];

{-| Change from infix to postfix form

    Precedence at start is zero
-}
-- Dijkstra's algorithm
shunt :: Ord p => (t -> TokenType p) -> [t] -> [t];
shunt lookup_t = (.) (execWriter . flip runStateT []) $ (.) (>> (get >>= tell)) $
  (mapM_ $ \ t ->
   case lookup_t t of {
     TokenPlain -> tell [t];
     TokenOper fix1 p1 | or $ map (== fix1) [InfixLeft, InfixRight, InfixNull] ->
       get >>* span (lookup_t >>> \ t' ->
                     case t' of {
                       TokenOper fix2 p2 -> fix1 == InfixLeft  && p1 <= p2 ||
                                            fix1 == InfixRight && p1 <  p2;
                       _                 -> False;
                     }) >>= tell *=* put . (t:) >>* uncurry mappend
                       | Postfix == fix1 -> tell [t]
                       | Prefix  == fix1 -> modify (t:);
     TokenLParenth -> modify (t:);
     TokenRParenth -> get >>* break (\ t -> case lookup_t t of { TokenLParenth -> True; _ -> False; }) >>= tell *=* list (const put) (put $ error "Mismatched Parentheses") >>* uncurry mappend;
   });
