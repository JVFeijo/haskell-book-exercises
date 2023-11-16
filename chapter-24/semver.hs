import Control.Applicative
import Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer deriving (Show, Eq)

type Major = Integer

type Minor = Integer

type Patch = Integer

type Release = [NumberOrString]

type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq)

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = (NOSS <$> (some letter)) <|> (NOSI <$> integer)

parseSemVer :: Parser SemVer
parseSemVer = (SemVer <$> integer) <* (char '.') <*> integer <* (char '.') <*> integer <* (skipOptional (char '-')) <*> (parseNumberOrString `sepBy` symbol (".")) <* (skipOptional (char '+')) <*> (parseNumberOrString `sepBy` symbol ("."))

toMaybePsmv :: Result SemVer -> Maybe SemVer
toMaybePsmv (Success smv) = Just smv
toMaybePsmv (Failure _) = Nothing

instance Ord NumberOrString where
 compare (NOSS _) (NOSI _) = GT
 compare (NOSS str1) (NOSS str2) = compare str1 str2
 compare (NOSI _) (NOSS _) = LT
 compare (NOSI n1) (NOSI n2) = compare n1 n2

instance Ord SemVer where
 (SemVer mj mi pat [] _) <= (SemVer mj2  mi2 pat2 [] _) = (mj <= mj2) && (mi <= mj2) && (pat <= pat2) && True
 (SemVer mj mi pat [] _) <= (SemVer mj2  mi2 pat2 _ _) = (mj <= mj2) && (mi <= mj2) && (pat <= pat2) && False
 (SemVer mj mi pat rls _) <= (SemVer mj2  mi2 pat2 [] _) = (mj <= mj2) && (mi <= mj2) && (pat <= pat2) && True
 (SemVer mj mi pat rls _) <= (SemVer mj2  mi2 pat2 rls2 _) = (mj <= mj2) && (mi <= mj2) && (pat <= pat2) && (rls <= rls2)
