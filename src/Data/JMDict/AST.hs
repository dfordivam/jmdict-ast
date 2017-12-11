{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.JMDict.AST where

import           Control.Applicative hiding (many)
import           Control.Monad
import           Control.Lens
import           Data.Default
import           Data.Maybe
import qualified Data.Text as T
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

newtype EntryId = EntryId { unEntryId :: Int }
  deriving (Show, Eq, Ord)

newtype KanjiPhrase = KanjiPhrase { unKanjiPhrase :: T.Text }
  deriving (Show, Eq, Ord)

newtype ReadingPhrase = ReadingPhrase { unReadingPhrase :: T.Text }
  deriving (Show, Eq, Ord)

data Entry = Entry {
      _entryUniqueId :: EntryId
    , _entryKanjiElements :: [KanjiElement]
    , _entryReadingElements :: NonEmpty ReadingElement
    , _entrySenses :: [Sense]
    }
    deriving (Show)

data KanjiElement = KanjiElement {
      _kanjiPhrase :: KanjiPhrase
    , _kanjiInfo :: [KanjiInfo]
    , _kanjiPriority :: [Priority]
    }
    deriving (Show)

data ReadingElement = ReadingElement {
      _readingPhrase :: ReadingPhrase
    , _readingNoKanji :: Bool
      -- ^ This element, which will usually have a false value, indicates
      -- that the reading, while associated with the kanji, cannot be regjarded
      -- as a true reading of the kanji. It is typically used for words
      -- such as foreign place names, gairaigo which can be in kanji or
      -- katakana, etc.

    , _readingRestrictKanji :: [KanjiPhrase]
    -- ^ If non empty then this reading applies to only the given KanjiPhrase

    , _readingInfo :: [ReadingInfo]
    , _readingPriority :: [Priority]
    }
    deriving (Show)

type Xref = (Maybe KanjiPhrase, Maybe ReadingPhrase, Maybe Int)

data Sense = Sense {
      _senseRestrictKanji :: [KanjiPhrase] -- ^ If null, unrestricted
    , _senseRestrictReading :: [ReadingPhrase] -- ^ If null, unrestricted
    , _sensePartOfSpeech :: [PartOfSpeech]
    , _senseRelated :: [Xref]
    , _senseAntonyms :: [Xref]
    , _senseFields :: [Field]
    , _senseMisc :: [SenseMisc]
    , _senseInfo :: [T.Text] -- ^ Additional information
    , _senseSources :: [LanguageSource]
    , _senseDialect :: [Dialect]
    , _senseGlosses :: [Gloss]
    }
    deriving (Show)


data LanguageSource = LanguageSource {
      _sourceOrigin :: T.Text -- ^ Origin word
    , _sourceLanguage :: T.Text
    , _sourceFull :: Bool -- ^ Default True
    , _sourceWaseieigo :: Bool -- ^ Default False
    }
    deriving (Show, Read)

-- | NB: Doesn't support <pri>, since its not used.
data Gloss = Gloss {
      _glossDefinition :: T.Text
    , _glossLanguage :: T.Text
    }
    deriving (Show, Read)

-------------------------------------------------
data Priority
  = News1
  | News2
  | Ichi1
  | Ichi2
  | Spec1
  | Spec2
  | Gai1
  | Gai2
  | FreqOfUse Int
  deriving (Show)

   -- 1020 io
   --  828 oK
   --  742 iK
   --  476 ateji
   --   18 ik

data KanjiInfo
  = KI_IrregularOkuriganaUsage
  | KI_IrregularKanjiUsage
  | KI_IrregularKanaUsage
  | KI_OutDatedKanji
  | KI_Ateji
  deriving (Show)

    -- 807 ok
    -- 465 ik
    --  80 gikun
    --   1 oik

data ReadingInfo
  = RI_OutDatedOrObsoleteKanaUsage
  | RI_IrregularKanaUsage
  | RI_OldOrIrregularKanaForm
  | RI_Gikun
  deriving (Show)
  -- "gikun (meaning as reading) or jukujikun (special kanji reading)"

-------------------------------------------------
data PartOfSpeech
  = PosNoun -- n
  | PosNounType NounType
  | PosPronoun -- pn
  | PosVerb VerbType IsTransitive
  | PosAdverb Adverb
  | PosAdjective Adjective
  | PosNumeric -- num
  | PosCounter -- ctr
  | PosAuxiliary Auxiliary
  | PosExpressions -- exp
  | PosIntejection -- int
  | PosSuffix -- suf
  | PosPrefix -- pref
  | PosConjugation -- conj
  | PosUnclassified -- unc
  | PosCopula
  | PosParticle -- prt
  | PosMisc T.Text
  deriving (Show)

data NounType
  = NounWithSuru -- vs
  | AdjNoun_No -- adj-no
  | AdverbialNoun -- n-adv
  | SuffixNoun -- n-suf
  | PrefixNoun -- n-pref
  | TemporalNoun -- n-t
  | ProperNoun -- n-pr
  deriving (Show)

data VerbType
  = Regular RegularVerb
  | Irregular IrregularVerb
  | Special SpecialVerb
  deriving (Show)

data RegularVerb
  = Ichidan -- v1
  | Godan VerbEnding
  | Yodan VerbEnding -- v4r v4k v4h v4s
  | Nidan T.Text -- v2a-s v2r-s v2m-s v2y-k
  deriving (Show)

data IrregularVerb
  = SuruI -- vs-s
  | RuIrregular -- vr
  | NuIrregular -- vn
  | GodanRu -- v5r-i
  deriving (Show)

data SpecialVerb
  = Kureru -- v1-s
  | GodanAru -- v5-aru
  | IkuYuku -- v5k-s
  | GodanUEnding -- v5u-s
  | Kuru -- vk
  | SuVerb -- vs-c
  | SuruS -- vs-i
  | Zuru -- vz
  deriving (Show)

data Adjective
  = IAdjective -- adj-i
  | NaAdjective -- adj-na
  | PreNounAdjective -- adj-pn
  | TaruAdjective -- adj-t
  | NariAdjective -- adj-nari
  | PreNominalAdjective -- adj-f
  | YoiIiAdjective -- adj-ix
  | KuAdjective -- adj-ku
  | ShikuAdjective -- adj-shiku
  deriving (Show)

-- XXX does adv-to imply adv?
data Adverb
  = Adverb -- adv
  | Adverb_To -- adv-to
  deriving (Show)

data IsTransitive
  = Transitive -- vt
  | Intransitive -- vi
  | BothTransAndIntransitive
  | NotSpecified
  deriving (Show)

data VerbEnding
  = BuEnding -- v5b
  | GuEnding -- v5g
  | KuEnding -- v5k
  | MuEnding -- v5m
  | NuEnding -- v5n
  | RuEnding -- v5r
  | SuEnding -- v5s
  | TuEnding -- v5t
  | UEnding -- v5u
  | HuEnding -- v4h
  deriving (Show)

data Auxiliary
  = Auxiliary -- aux
  | AuxiliaryVerb -- aux-v
  | AuxiliaryAdjective --aux-adj
  deriving (Show)

-------------------------------------------------
data Field
 = FieldComp
 | FieldBuddh
 | FieldMath
 | FieldLing
 | FieldFood
 | FieldMed
 | FieldSumo
 | FieldPhysics
 | FieldAstron
 | FieldMusic
 | FieldBaseb
 | FieldMahj
 | FieldBiol
 | FieldLaw
 | FieldChem
 | FieldSports
 | FieldAnat
 | FieldMa
 | FieldGeol
 | FieldFinc
 | FieldBot
 | FieldShogi
 | FieldShinto
 | FieldMil
 | FieldArchit
 | FieldEcon
 | FieldBus
 | FieldEngr
 | FieldZool
  deriving (Show)

data SenseMisc
 = UsuallyKana
 | Abbreviation
 | Yojijukugo
 | Archaism
 | ObscureTerm
 | OnomatopoeicOrMimeticWord
 | Colloquialism
 | Slang
 | IdiomaticExpression
 | Honorific
 | Derogatory
 | Polite
 | ObsoleteTerm
 | Proverb
 | Sensitive
 | Humble
 | Vulgar
 | Jocular
 | Familiar
 | Childrens
 | FemaleTerm
 | MaleTerm
 | MangaSlang
 | Poetical
 | Rare
  deriving (Show)

data Dialect
 = KyotoBen
 | OsakaBen
 | KansaiBen
 | KantouBen
 | TosaBen
 | TouhokuBen
 | TsugaruBen
 | KyuushuuBen
 | RyuukyuuBen
 | NaganoBen
 | HokkaidoBen
  deriving (Show)

makeLenses ''Entry
makeLenses ''KanjiElement
makeLenses ''ReadingElement
makeLenses ''Sense
makePrisms ''Priority
makePrisms ''KanjiInfo
makePrisms ''ReadingInfo
makePrisms ''PartOfSpeech
makePrisms ''NounType
makePrisms ''VerbType
makePrisms ''RegularVerb
makePrisms ''IrregularVerb
makePrisms ''SpecialVerb
makePrisms ''Adjective
makePrisms ''Adverb
makePrisms ''Auxiliary
makePrisms ''Field
makePrisms ''SenseMisc
makePrisms ''Dialect
makeLenses ''Gloss
makeLenses ''LanguageSource
