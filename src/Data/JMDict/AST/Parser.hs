{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.JMDict.AST.Parser
  where

import           Control.Lens
import qualified Data.JMDict.XML.Parser as X
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Text.Read
import Data.JMDict.AST.AST

type ParseError = T.Text

makeAST :: X.Entry -> Either ParseError Entry
makeAST e = Entry (EntryId $ e ^. X.entryUniqueId)
  <$> traverse makeKanjiElem (e ^. X.entryKanjiElements)
  <*> (getNE =<< traverse makeReadingElem
       (e ^. X.entryReadingElements))
  <*> traverse makeSense (e ^. X.entrySenses)
  where getNE ls = case NE.nonEmpty ls of
          (Just v) -> Right v
          Nothing -> Left "Empty ReadingElement"

makeKanjiElem :: X.KanjiElement -> Either ParseError KanjiElement
makeKanjiElem k = KanjiElement
  <$> makeKanjiPhrase (k ^. X.kanjiPhrase)
  <*> makeKanjiInfo (k ^. X.kanjiInfo)
  <*> traverse makePriority (k ^. X.kanjiPriority)

makeReadingElem :: X.ReadingElement -> Either ParseError ReadingElement
makeReadingElem r = ReadingElement
  <$> makeReadingPhrase (r ^. X.readingPhrase)
  <*> pure (r ^. X.readingNoKanji)
  <*> traverse makeKanjiPhrase (r ^. X.readingRestrictKanji)
  <*> makeReadingInfo (r ^. X.readingInfo)
  <*> traverse makePriority (r ^. X.readingPriority)

makeSense :: X.Sense -> Either ParseError Sense
makeSense s = Sense
  <$> traverse makeKanjiPhrase (s ^. X.senseRestrictKanji)
  <*> traverse makeReadingPhrase (s ^. X.senseRestrictReading)
  <*> makePOS (s ^. X.sensePartOfSpeech)
  <*> traverse makeXref (s ^. X.senseRelated)
  <*> traverse makeXref (s ^. X.senseAntonyms)
  <*> traverse makeSenseField (s ^. X.senseFields)
  <*> traverse makeSenseMisc (s ^. X.senseMisc)
  <*> pure (s ^. X.senseInfo)
  <*> traverse makeLanguageSource (s ^. X.senseSources)
  <*> makeDialect (s ^. X.senseDialects)
  <*> pure (s ^. X.senseGlosses)

---------------------------------------------------------------
-- TODO
makeKanjiPhrase :: T.Text -> Either ParseError KanjiPhrase
makeKanjiPhrase t = Right $ KanjiPhrase t

-- TODO
makeReadingPhrase :: T.Text -> Either ParseError ReadingPhrase
makeReadingPhrase t = Right $ ReadingPhrase t

makeReadingInfo :: [T.Text] -> Either ParseError ReadingInfo
makeReadingInfo = undefined
makeKanjiInfo :: [T.Text] -> Either ParseError KanjiInfo
makeKanjiInfo = undefined

makePriority :: T.Text -> Either ParseError Priority
makePriority t
  | t == "news1" = Right News1
  | t == "news2" = Right News2
  | t == "ichi1" = Right Ichi1
  | t == "ichi2" = Right Ichi2
  | t == "spec1" = Right Spec1
  | t == "spec2" = Right Spec2
  | t == "gai1" =  Right Gai1
  | t == "gai2" =  Right Gai2
  | "nf" `T.isPrefixOf` t =
    case (readMaybe $ T.unpack (T.drop 2 t)) of
      (Just v) -> Right $ FreqOfUse v
      Nothing -> Left $ "makePriority: Got: \"" <> t <>"\""
  | otherwise = Left $ "makePriority: Got: \"" <> t <>"\""

makeXref :: T.Text -> Either ParseError Xref
makeXref = undefined
makeSenseField :: T.Text -> Either ParseError Field
makeSenseField = undefined

makeSenseMisc :: T.Text -> Either ParseError SenseMisc
makeSenseMisc = undefined


makeLanguageSource
          :: X.LanguageSource -> Either ParseError LanguageSource
makeLanguageSource = undefined


makeDialect :: [T.Text] -> Either ParseError Dialect
makeDialect = undefined

makePOS :: [T.Text] -> Either ParseError [PartOfSpeech]
makePOS ts = fmap catMaybes $ traverse (makePartOfSpeech tSet) ts
  where
    tSet = Set.fromList ts

makePartOfSpeech :: Set T.Text -> T.Text
  -> Either ParseError (Maybe PartOfSpeech)
makePartOfSpeech tSet t = f
  where
  rj v = Right $ Just v
  verb ty = PosVerb ty isTrans
  checkVerbPresent = Right $ Nothing
    -- Set.delete "vi" $ Set.delete "vt" $ tSet
  isTrans = case (Set.member "vt" tSet, Set.member "vi" tSet) of
    (True, True) -> BothTransAndIntransitive
    (True, False) -> Transitive
    (False, True) -> Intransitive
    (False, False) -> NotSpecified
  f | t == "n"         = rj $ PosNoun
    | t == "vs"        = rj $ PosNounType NounWithSuru
    | t == "exp"       = rj $ PosExpressions
    | t == "adj-no"    = rj $ PosNounType AdjNoun_No
    | t == "adj-na"    = rj $ PosAdjective NaAdjective
    | t == "v1"        = rj $ verb (Regular Ichidan)
    | t == "vt"        = checkVerbPresent
    | t == "adv"       = rj $ PosAdverb Adverb
    | t == "vi"        = checkVerbPresent
    | t == "v5r"       = rj $ verb (Regular $ Godan RuEnding)
    | t == "v5s"       = rj $ verb (Regular $ Godan SuEnding)
    | t == "adj-i"     = rj $ PosAdjective IAdjective
    | t == "adv-to"    = rj $ PosAdverb Adverb_To
    | t == "v5k"       = rj $ verb (Regular $ Godan KuEnding)
    | t == "adj-f"     = rj $ PosAdjective PreNominalAdjective
    | t == "v5u"       = rj $ verb (Regular $ Godan UEnding)
    | t == "n-adv"     = rj $ PosNounType AdverbialNoun
    | t == "v5m"       = rj $ verb (Regular $ Godan MuEnding)
    | t == "n-suf"     = rj $ PosNounType SuffixNoun
    | t == "n-t"       = rj $ PosNounType TemporalNoun
    | t == "suf"       = rj $ PosSuffix
    | t == "int"       = rj $ PosIntejection
    | t == "adj-t"     = rj $ PosAdjective TaruAdjective
    | t == "vs-s"      = rj $ verb (Special SuruS)
    | t == "pref"      = rj $ PosPrefix
    | t == "v5t"       = rj $ verb (Regular $ Godan TuEnding)
    | t == "vs-i"      = rj $ verb (Irregular SuruI)
    | t == "conj"      = rj $ PosConjugation
    | t == "ctr"       = rj $ PosCounter
    | t == "pn"        = rj $ PosPronoun
    | t == "n-pref"    = rj $ PosNounType PrefixNoun
    | t == "v5g"       = rj $ verb (Regular $ Godan GuEnding)
    | t == "adj-pn"    = rj $ PosAdjective PreNounAdjective
    | t == "prt"       = rj $ PosParticle
    | t == "v5b"       = rj $ verb (Regular $ Godan BuEnding)
    | t == "aux-v"     = rj $ PosAuxiliary AuxiliaryVerb
    | t == "adj-ix"    = rj $ PosAdjective YoiIiAdjective
    | t == "vz"        = rj $ verb (Special Zuru)
    | t == "num"       = rj $ PosNumeric
    | t == "v5k-s"     = rj $ verb (Special IkuYuku)
    | t == "v5r-i"     = rj $ verb (Irregular GodanRu)
    | t == "aux"       = rj $ PosAuxiliary Auxiliary
    | t == "vk"        = rj $ verb (Special Kuru)
    | t == "n-pr"      = rj $ PosNounType ProperNoun
    | t == "v4r"       = rj $ verb (Regular $ Yodan RuEnding)
    | t == "vs-c"      = rj $ verb (Special SuVerb)
    | t == "v2r-s"     = rj $ verb (Regular $ Nidan t)
    | t == "aux-adj"   = rj $ PosAuxiliary AuxiliaryAdjective
    | t == "adj-ku"    = rj $ PosAdjective KuAdjective
    | t == "v4k"       = rj $ verb (Regular $ Yodan KuEnding)
    | t == "vr"        = rj $ verb (Irregular RuIrregular)
    | t == "v5n"       = rj $ verb (Regular $ Godan NuEnding)
    | t == "v5u-s"     = rj $ verb (Special GodanUEnding)
    | t == "v5aru"     = rj $ verb (Special GodanAru)
    | t == "v4h"       = rj $ verb (Regular $ Yodan HuEnding)
    | t == "unc"       = rj $ PosUnclassified
    | t == "adj-nari"  = rj $ PosAdjective NariAdjective
    | t == "v4s"       = rj $ verb (Regular $ Yodan SuEnding)
    | t == "v2m-s"     = rj $ verb (Regular $ Nidan t)
    | t == "vn"        = rj $ verb (Irregular NuIrregular)
    | t == "v2y-k"     = rj $ verb (Regular $ Nidan t)
    | t == "v2h-s"     = rj $ verb (Regular $ Nidan t)
    | t == "v2y-s"     = rj $ verb (Regular $ Nidan t)
    | t == "adj-shiku" = rj $ PosAdjective ShikuAdjective
    | t == "v4t"       = rj $ verb (Regular $ Yodan TuEnding)
    | t == "v4b"       = rj $ verb (Regular $ Yodan BuEnding)
    | t == "v2t-s"     = rj $ verb (Regular $ Nidan t)
    | t == "v2t-k"     = rj $ verb (Regular $ Nidan t)
    | t == "v2n-s"     = rj $ verb (Regular $ Nidan t)
    | t == "v2k-s"     = rj $ verb (Regular $ Nidan t)
    | t == "v2h-k"     = rj $ verb (Regular $ Nidan t)
    | t == "v2b-k"     = rj $ verb (Regular $ Nidan t)
    | t == "v2a-s"     = rj $ verb (Regular $ Nidan t)
    | t == "v1-s"      = rj $ verb (Special Kureru)
    | t == "v4m"       = rj $ verb (Regular $ Yodan MuEnding)
    | t == "v2z-s"     = rj $ verb (Regular $ Nidan t)
    | t == "v2w-s"     = rj $ verb (Regular $ Nidan t)
    | t == "v2s-s"     = rj $ verb (Regular $ Nidan t)
    | t == "v2r-k"     = rj $ verb (Regular $ Nidan t)
    | t == "v2k-k"     = rj $ verb (Regular $ Nidan t)
    | t == "v2g-s"     = rj $ verb (Regular $ Nidan t)
    | t == "v2g-k"     = rj $ verb (Regular $ Nidan t)
    | t == "v2d-s"     = rj $ verb (Regular $ Nidan t)
    | t == "cop-da"    = rj $ PosCopula
    | otherwise = Left "makePos"
