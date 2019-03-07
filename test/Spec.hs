import Test.Hspec
import TranQL.Syntax (expr, selector, field, ras, var, parseWithEof, Expr(..), V(..), Field(..), Selector(..), T(..))

main :: IO ()
main = hspec $ do
    describe "parsing" $ do
        it "as" $ do
            parseWithEof var "as" `shouldNotBe` Right (V "as")
        it "as" $ do
            parseWithEof ras "as" `shouldBe` Right ()
        it "b" $ do
            parseWithEof field "b" `shouldBe` Right (Field "b")
        it "1 c" $ do
            parseWithEof expr "1 c" `shouldBe` Right (App (IntegerConst 1) (Var (V "c")))
        it "1 c as b" $ do
            parseWithEof selector "1 c as b" `shouldBe` Right (Selector (App (IntegerConst 1) (Var (V "c"))) (Field "b"))
        it "a c as b" $ do
            parseWithEof selector "a c as b" `shouldBe` Right (Selector (App (Var (V "a")) (Var (V "c"))) (Field "b"))
        it "a as b" $ do
            parseWithEof selector "a as b" `shouldBe` Right (Selector (Var (V "a")) (Field "b"))
        it "select 1 c as b where true" $ do
            parseWithEof expr "select 1 c as b where true" `shouldBe` Right (Select [Selector (App (IntegerConst 1) (Var (V "c"))) (Field "b")] (Var (V "true")))
        it "assume x : prop in select 1 c as b where and x true" $ do
            parseWithEof expr "assume x : prop in select 1 c as b where and x true" `shouldBe` Right (Abs (V "x") TProp (Select [Selector (App (IntegerConst 1) (Var (V "c"))) (Field "b")] (App (App (Var (V "and")) (Var (V "x"))) (Var (V "true")))))
    