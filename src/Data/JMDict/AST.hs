{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
module Data.JMDict.AST where

import           Control.Applicative hiding (many)
import           Control.Monad
import           Control.Lens
import           Data.Default
import           Data.Maybe
import qualified Data.Text as T
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import GHC.Generics

newtype EntryId = EntryId { unEntryId :: Int }
  deriving (Show, Generic, Eq, Ord)

newtype KanjiPhrase = KanjiPhrase { unKanjiPhrase :: T.Text }
  deriving (Show, Generic, Eq, Ord)

newtype ReadingPhrase = ReadingPhrase { unReadingPhrase :: T.Text }
  deriving (Show, Generic, Eq, Ord)

data Entry = Entry {
      _entryUniqueId :: EntryId
    , _entryKanjiElements :: [KanjiElement]
    , _entryReadingElements :: NonEmpty ReadingElement
    , _entrySenses :: [Sense]
    }
    deriving (Eq, Ord, Show, Generic)

data KanjiElement = KanjiElement {
      _kanjiPhrase :: KanjiPhrase
    , _kanjiInfo :: [KanjiInfo]
    , _kanjiPriority :: [Priority]
    }
    deriving (Eq, Ord, Show, Generic)

data ReadingElement = ReadingElement {
      _readingPhrase :: ReadingPhrase
    , _readingNoKanji :: Bool
      -- ^ This element, which will usually have a false value, indicates
      -- that the reading, while associated with the kanji, cannot be regarded
      -- as a true reading of the kanji. It is typically used for words
      -- such as foreign place names, gairaigo which can be in kanji or
      -- katakana, etc.

    , _readingRestrictKanji :: [KanjiPhrase]
    -- ^ If non empty then this reading applies to only the given KanjiPhrase

    , _readingInfo :: [ReadingInfo]
    , _readingPriority :: [Priority]
    }
    deriving (Eq, Ord, Show, Generic)

type Xref = (Maybe KanjiPhrase, Maybe ReadingPhrase, Maybe Int)

data Sense = Sense {
      _senseRestrictKanji :: [KanjiPhrase] -- ^ If null, unrestricted
    , _senseRestrictReading :: [ReadingPhrase] -- ^ If null, unrestricted
    , _sensePartOfSpeech :: [PartOfSpeech]
    , _senseRelated :: [Xref]
    , _senseAntonyms :: [Xref]
    , _senseFields :: [SenseField]
    , _senseMisc :: [SenseMisc]
    , _senseInfo :: [T.Text] -- ^ Additional information
    , _senseSources :: [LanguageSource]
    , _senseDialect :: [Dialect]
    , _senseGlosses :: [Gloss]
    }
    deriving (Eq, Ord, Show, Generic)


data LanguageSource = LanguageSource {
      _sourceOrigin :: T.Text -- ^ Origin word
    , _sourceLanguage :: T.Text
    , _sourceFull :: Bool -- ^ Default True
    , _sourceWaseieigo :: Bool -- ^ Default False
    }
    deriving (Eq, Ord, Show, Generic, Read)

-- | NB: Doesn't support <pri>, since its not used.
data Gloss = Gloss {
      _glossDefinition :: T.Text
    , _glossLanguage :: T.Text
    }
    deriving (Eq, Ord, Show, Generic, Read)

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
  deriving (Eq, Ord, Show, Generic)

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
  deriving (Eq, Ord, Show, Generic)

    -- 807 ok
    -- 465 ik
    --  80 gikun
    --   1 oik

data ReadingInfo
  = RI_OutDatedOrObsoleteKanaUsage
  | RI_IrregularKanaUsage
  | RI_OldOrIrregularKanaForm
  | RI_Gikun
  deriving (Eq, Ord, Show, Generic)
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
  deriving (Eq, Ord, Show, Generic)

data NounType
  = NounWithSuru -- vs
  | AdjNoun_No -- adj-no
  | AdverbialNoun -- n-adv
  | SuffixNoun -- n-suf
  | PrefixNoun -- n-pref
  | TemporalNoun -- n-t
  | ProperNoun -- n-pr
  deriving (Eq, Ord, Show, Generic)

data VerbType
  = Regular RegularVerb
  | Irregular IrregularVerb
  | Special SpecialVerb
  deriving (Eq, Ord, Show, Generic)

data RegularVerb
  = Ichidan -- v1
  | Godan VerbEnding
  | Yodan VerbEnding -- v4r v4k v4h v4s
  | Nidan T.Text -- v2a-s v2r-s v2m-s v2y-k
  deriving (Eq, Ord, Show, Generic)

data IrregularVerb
  = SuruI -- vs-s
  | RuIrregular -- vr
  | NuIrregular -- vn
  | GodanRu -- v5r-i
  deriving (Eq, Ord, Show, Generic)

data SpecialVerb
  = Kureru -- v1-s
  | GodanAru -- v5-aru
  | IkuYuku -- v5k-s
  | GodanUEnding -- v5u-s
  | Kuru -- vk
  | SuVerb -- vs-c
  | SuruS -- vs-i
  | Zuru -- vz
  deriving (Eq, Ord, Show, Generic)

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
  deriving (Eq, Ord, Show, Generic)

-- XXX does adv-to imply adv?
data Adverb
  = Adverb -- adv
  | Adverb_To -- adv-to
  deriving (Eq, Ord, Show, Generic)

data IsTransitive
  = Transitive -- vt
  | Intransitive -- vi
  | BothTransAndIntransitive
  | NotSpecified
  deriving (Eq, Ord, Show, Generic)

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
  deriving (Eq, Ord, Show, Generic)

data Auxiliary
  = Auxiliary -- aux
  | AuxiliaryVerb -- aux-v
  | AuxiliaryAdjective --aux-adj
  deriving (Eq, Ord, Show, Generic)

-------------------------------------------------
data SenseField
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
  deriving (Eq, Ord, Show, Generic)

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
  deriving (Eq, Ord, Show, Generic)

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
  deriving (Eq, Ord, Show, Generic)

-- makeLenses ''Entry
-- makeLenses ''KanjiElement
-- makeLenses ''ReadingElement
-- makeLenses ''Sense
-- makePrisms ''Priority
-- makePrisms ''KanjiInfo
-- makePrisms ''ReadingInfo
-- makePrisms ''PartOfSpeech
-- makePrisms ''NounType
-- makePrisms ''VerbType
-- makePrisms ''RegularVerb
-- makePrisms ''IrregularVerb
-- makePrisms ''SpecialVerb
-- makePrisms ''Adjective
-- makePrisms ''Adverb
-- makePrisms ''Auxiliary
-- makePrisms ''SenseField
-- makePrisms ''SenseMisc
-- makePrisms ''Dialect
-- makeLenses ''Gloss
-- makeLenses ''LanguageSource

-- src/Data/JMDict/AST.hs:312:1-18: Splicing declarations
--     makeLenses ''Entry
--   ======>
entryKanjiElements :: Lens' Entry [KanjiElement]
entryKanjiElements f_ajiB (Entry x1_ajiC x2_ajiD x3_ajiE x4_ajiF)
  = fmap
      (\ y1_ajiG -> Entry x1_ajiC y1_ajiG x3_ajiE x4_ajiF)
      (f_ajiB x2_ajiD)
{-# INLINE entryKanjiElements #-}
entryReadingElements :: Lens' Entry (NonEmpty ReadingElement)
entryReadingElements f_ajiH (Entry x1_ajiI x2_ajiJ x3_ajiK x4_ajiL)
  = fmap
      (\ y1_ajiM -> Entry x1_ajiI x2_ajiJ y1_ajiM x4_ajiL)
      (f_ajiH x3_ajiK)
{-# INLINE entryReadingElements #-}
entrySenses :: Lens' Entry [Sense]
entrySenses f_ajiN (Entry x1_ajiO x2_ajiP x3_ajiQ x4_ajiR)
  = fmap
      (\ y1_ajiS -> Entry x1_ajiO x2_ajiP x3_ajiQ y1_ajiS)
      (f_ajiN x4_ajiR)
{-# INLINE entrySenses #-}
entryUniqueId :: Lens' Entry EntryId
entryUniqueId f_ajiT (Entry x1_ajiU x2_ajiV x3_ajiW x4_ajiX)
  = fmap
      (\ y1_ajiY -> Entry y1_ajiY x2_ajiV x3_ajiW x4_ajiX)
      (f_ajiT x1_ajiU)
{-# INLINE entryUniqueId #-}
-- src/Data/JMDict/AST.hs:313:1-25: Splicing declarations
--     makeLenses ''KanjiElement
--   ======>
kanjiInfo :: Lens' KanjiElement [KanjiInfo]
kanjiInfo f_ajlt (KanjiElement x1_ajlu x2_ajlv x3_ajlw)
  = fmap
      (\ y1_ajlx -> KanjiElement x1_ajlu y1_ajlx x3_ajlw)
      (f_ajlt x2_ajlv)
{-# INLINE kanjiInfo #-}
kanjiPhrase :: Lens' KanjiElement KanjiPhrase
kanjiPhrase f_ajly (KanjiElement x1_ajlz x2_ajlA x3_ajlB)
  = fmap
      (\ y1_ajlC -> KanjiElement y1_ajlC x2_ajlA x3_ajlB)
      (f_ajly x1_ajlz)
{-# INLINE kanjiPhrase #-}
kanjiPriority :: Lens' KanjiElement [Priority]
kanjiPriority f_ajlD (KanjiElement x1_ajlE x2_ajlF x3_ajlG)
  = fmap
      (\ y1_ajlH -> KanjiElement x1_ajlE x2_ajlF y1_ajlH)
      (f_ajlD x3_ajlG)
{-# INLINE kanjiPriority #-}
-- src/Data/JMDict/AST.hs:314:1-27: Splicing declarations
--     makeLenses ''ReadingElement
--   ======>
readingInfo :: Lens' ReadingElement [ReadingInfo]
readingInfo
  f_ajmH
  (ReadingElement x1_ajmI x2_ajmJ x3_ajmK x4_ajmL x5_ajmM)
  = fmap
      (\ y1_ajmN
         -> ReadingElement x1_ajmI x2_ajmJ x3_ajmK y1_ajmN x5_ajmM)
      (f_ajmH x4_ajmL)
{-# INLINE readingInfo #-}
readingNoKanji :: Lens' ReadingElement Bool
readingNoKanji
  f_ajmO
  (ReadingElement x1_ajmP x2_ajmQ x3_ajmR x4_ajmS x5_ajmT)
  = fmap
      (\ y1_ajmU
         -> ReadingElement x1_ajmP y1_ajmU x3_ajmR x4_ajmS x5_ajmT)
      (f_ajmO x2_ajmQ)
{-# INLINE readingNoKanji #-}
readingPhrase :: Lens' ReadingElement ReadingPhrase
readingPhrase
  f_ajmV
  (ReadingElement x1_ajmW x2_ajmX x3_ajmY x4_ajmZ x5_ajn0)
  = fmap
      (\ y1_ajn1
         -> ReadingElement y1_ajn1 x2_ajmX x3_ajmY x4_ajmZ x5_ajn0)
      (f_ajmV x1_ajmW)
{-# INLINE readingPhrase #-}
readingPriority :: Lens' ReadingElement [Priority]
readingPriority
  f_ajn2
  (ReadingElement x1_ajn3 x2_ajn4 x3_ajn5 x4_ajn6 x5_ajn7)
  = fmap
      (\ y1_ajn8
         -> ReadingElement x1_ajn3 x2_ajn4 x3_ajn5 x4_ajn6 y1_ajn8)
      (f_ajn2 x5_ajn7)
{-# INLINE readingPriority #-}
readingRestrictKanji :: Lens' ReadingElement [KanjiPhrase]
readingRestrictKanji
  f_ajn9
  (ReadingElement x1_ajna x2_ajnb x3_ajnc x4_ajnd x5_ajne)
  = fmap
      (\ y1_ajnf
         -> ReadingElement x1_ajna x2_ajnb y1_ajnf x4_ajnd x5_ajne)
      (f_ajn9 x3_ajnc)
{-# INLINE readingRestrictKanji #-}
-- src/Data/JMDict/AST.hs:315:1-18: Splicing declarations
-- makeLenses ''Sense
--   ======>
senseAntonyms :: Lens' Sense [Xref]
senseAntonyms
  f_ajoP
  (Sense x1_ajoQ
         x2_ajoR
         x3_ajoS
         x4_ajoT
         x5_ajoU
         x6_ajoV
         x7_ajoW
         x8_ajoX
         x9_ajoY
         x10_ajoZ
         x11_ajp0)
  = fmap
      (\ y1_ajp1
         -> Sense
              x1_ajoQ
              x2_ajoR
              x3_ajoS
              x4_ajoT
              y1_ajp1
              x6_ajoV
              x7_ajoW
              x8_ajoX
              x9_ajoY
              x10_ajoZ
              x11_ajp0)
      (f_ajoP x5_ajoU)
{-# INLINE senseAntonyms #-}
senseDialect :: Lens' Sense [Dialect]
senseDialect
  f_ajp2
  (Sense x1_ajp3
         x2_ajp4
         x3_ajp5
         x4_ajp6
         x5_ajp7
         x6_ajp8
         x7_ajp9
         x8_ajpa
         x9_ajpb
         x10_ajpc
         x11_ajpd)
  = fmap
      (\ y1_ajpe
         -> Sense
              x1_ajp3
              x2_ajp4
              x3_ajp5
              x4_ajp6
              x5_ajp7
              x6_ajp8
              x7_ajp9
              x8_ajpa
              x9_ajpb
              y1_ajpe
              x11_ajpd)
      (f_ajp2 x10_ajpc)
{-# INLINE senseDialect #-}
senseFields :: Lens' Sense [SenseField]
senseFields
  f_ajpf
  (Sense x1_ajpg
         x2_ajph
         x3_ajpi
         x4_ajpj
         x5_ajpk
         x6_ajpl
         x7_ajpm
         x8_ajpn
         x9_ajpo
         x10_ajpp
         x11_ajpq)
  = fmap
      (\ y1_ajpr
         -> Sense
              x1_ajpg
              x2_ajph
              x3_ajpi
              x4_ajpj
              x5_ajpk
              y1_ajpr
              x7_ajpm
              x8_ajpn
              x9_ajpo
              x10_ajpp
              x11_ajpq)
      (f_ajpf x6_ajpl)
{-# INLINE senseFields #-}
senseGlosses :: Lens' Sense [Gloss]
senseGlosses
  f_ajps
  (Sense x1_ajpt
         x2_ajpu
         x3_ajpv
         x4_ajpw
         x5_ajpx
         x6_ajpy
         x7_ajpz
         x8_ajpA
         x9_ajpB
         x10_ajpC
         x11_ajpD)
  = fmap
      (\ y1_ajpE
         -> Sense
              x1_ajpt
              x2_ajpu
              x3_ajpv
              x4_ajpw
              x5_ajpx
              x6_ajpy
              x7_ajpz
              x8_ajpA
              x9_ajpB
              x10_ajpC
              y1_ajpE)
      (f_ajps x11_ajpD)
{-# INLINE senseGlosses #-}
senseInfo :: Lens' Sense [T.Text]
senseInfo
  f_ajpF
  (Sense x1_ajpG
         x2_ajpH
         x3_ajpI
         x4_ajpJ
         x5_ajpK
         x6_ajpL
         x7_ajpM
         x8_ajpN
         x9_ajpO
         x10_ajpP
         x11_ajpQ)
  = fmap
      (\ y1_ajpR
         -> Sense
              x1_ajpG
              x2_ajpH
              x3_ajpI
              x4_ajpJ
              x5_ajpK
              x6_ajpL
              x7_ajpM
              y1_ajpR
              x9_ajpO
              x10_ajpP
              x11_ajpQ)
      (f_ajpF x8_ajpN)
{-# INLINE senseInfo #-}
senseMisc :: Lens' Sense [SenseMisc]
senseMisc
  f_ajpS
  (Sense x1_ajpT
         x2_ajpU
         x3_ajpV
         x4_ajpW
         x5_ajpX
         x6_ajpY
         x7_ajpZ
         x8_ajq0
         x9_ajq1
         x10_ajq2
         x11_ajq3)
  = fmap
      (\ y1_ajq4
         -> Sense
              x1_ajpT
              x2_ajpU
              x3_ajpV
              x4_ajpW
              x5_ajpX
              x6_ajpY
              y1_ajq4
              x8_ajq0
              x9_ajq1
              x10_ajq2
              x11_ajq3)
      (f_ajpS x7_ajpZ)
{-# INLINE senseMisc #-}
sensePartOfSpeech :: Lens' Sense [PartOfSpeech]
sensePartOfSpeech
  f_ajq5
  (Sense x1_ajq6
         x2_ajq7
         x3_ajq8
         x4_ajq9
         x5_ajqa
         x6_ajqb
         x7_ajqc
         x8_ajqd
         x9_ajqe
         x10_ajqf
         x11_ajqg)
  = fmap
      (\ y1_ajqh
         -> Sense
              x1_ajq6
              x2_ajq7
              y1_ajqh
              x4_ajq9
              x5_ajqa
              x6_ajqb
              x7_ajqc
              x8_ajqd
              x9_ajqe
              x10_ajqf
              x11_ajqg)
      (f_ajq5 x3_ajq8)
{-# INLINE sensePartOfSpeech #-}
senseRelated :: Lens' Sense [Xref]
senseRelated
  f_ajqi
  (Sense x1_ajqj
         x2_ajqk
         x3_ajql
         x4_ajqm
         x5_ajqn
         x6_ajqo
         x7_ajqp
         x8_ajqq
         x9_ajqr
         x10_ajqs
         x11_ajqt)
  = fmap
      (\ y1_ajqu
         -> Sense
              x1_ajqj
              x2_ajqk
              x3_ajql
              y1_ajqu
              x5_ajqn
              x6_ajqo
              x7_ajqp
              x8_ajqq
              x9_ajqr
              x10_ajqs
              x11_ajqt)
      (f_ajqi x4_ajqm)
{-# INLINE senseRelated #-}
senseRestrictKanji :: Lens' Sense [KanjiPhrase]
senseRestrictKanji
  f_ajqv
  (Sense x1_ajqw
         x2_ajqx
         x3_ajqy
         x4_ajqz
         x5_ajqA
         x6_ajqB
         x7_ajqC
         x8_ajqD
         x9_ajqE
         x10_ajqF
         x11_ajqG)
  = fmap
      (\ y1_ajqH
         -> Sense
              y1_ajqH
              x2_ajqx
              x3_ajqy
              x4_ajqz
              x5_ajqA
              x6_ajqB
              x7_ajqC
              x8_ajqD
              x9_ajqE
              x10_ajqF
              x11_ajqG)
      (f_ajqv x1_ajqw)
{-# INLINE senseRestrictKanji #-}
senseRestrictReading :: Lens' Sense [ReadingPhrase]
senseRestrictReading
  f_ajqI
  (Sense x1_ajqJ
         x2_ajqK
         x3_ajqL
         x4_ajqM
         x5_ajqN
         x6_ajqO
         x7_ajqP
         x8_ajqQ
         x9_ajqR
         x10_ajqS
         x11_ajqT)
  = fmap
      (\ y1_ajqU
         -> Sense
              x1_ajqJ
              y1_ajqU
              x3_ajqL
              x4_ajqM
              x5_ajqN
              x6_ajqO
              x7_ajqP
              x8_ajqQ
              x9_ajqR
              x10_ajqS
              x11_ajqT)
      (f_ajqI x2_ajqK)
{-# INLINE senseRestrictReading #-}
senseSources :: Lens' Sense [LanguageSource]
senseSources
  f_ajqV
  (Sense x1_ajqW
         x2_ajqX
         x3_ajqY
         x4_ajqZ
         x5_ajr0
         x6_ajr1
         x7_ajr2
         x8_ajr3
         x9_ajr4
         x10_ajr5
         x11_ajr6)
  = fmap
      (\ y1_ajr7
         -> Sense
              x1_ajqW
              x2_ajqX
              x3_ajqY
              x4_ajqZ
              x5_ajr0
              x6_ajr1
              x7_ajr2
              x8_ajr3
              y1_ajr7
              x10_ajr5
              x11_ajr6)
      (f_ajqV x9_ajr4)
{-# INLINE senseSources #-}
-- src/Data/JMDict/AST.hs:316:1-21: Splicing declarations
-- makePrisms ''Priority
--   ======>
_News1 :: Prism' Priority ()
_News1
  = prism
      (\ () -> News1)
      (\ x_ajwl
         -> case x_ajwl of
              News1 -> Right ()
              _ -> Left x_ajwl)
_News2 :: Prism' Priority ()
_News2
  = prism
      (\ () -> News2)
      (\ x_ajwm
         -> case x_ajwm of
              News2 -> Right ()
              _ -> Left x_ajwm)
_Ichi1 :: Prism' Priority ()
_Ichi1
  = prism
      (\ () -> Ichi1)
      (\ x_ajwn
         -> case x_ajwn of
              Ichi1 -> Right ()
              _ -> Left x_ajwn)
_Ichi2 :: Prism' Priority ()
_Ichi2
  = prism
      (\ () -> Ichi2)
      (\ x_ajwo
         -> case x_ajwo of
              Ichi2 -> Right ()
              _ -> Left x_ajwo)
_Spec1 :: Prism' Priority ()
_Spec1
  = prism
      (\ () -> Spec1)
      (\ x_ajwp
         -> case x_ajwp of
              Spec1 -> Right ()
              _ -> Left x_ajwp)
_Spec2 :: Prism' Priority ()
_Spec2
  = prism
      (\ () -> Spec2)
      (\ x_ajwq
         -> case x_ajwq of
              Spec2 -> Right ()
              _ -> Left x_ajwq)
_Gai1 :: Prism' Priority ()
_Gai1
  = prism
      (\ () -> Gai1)
      (\ x_ajwr
         -> case x_ajwr of
              Gai1 -> Right ()
              _ -> Left x_ajwr)
_Gai2 :: Prism' Priority ()
_Gai2
  = prism
      (\ () -> Gai2)
      (\ x_ajws
         -> case x_ajws of
              Gai2 -> Right ()
              _ -> Left x_ajws)
_FreqOfUse :: Prism' Priority Int
_FreqOfUse
  = prism
      (\ x1_ajwt -> FreqOfUse x1_ajwt)
      (\ x_ajwu
         -> case x_ajwu of
              FreqOfUse y1_ajwv -> Right y1_ajwv
              _ -> Left x_ajwu)
-- src/Data/JMDict/AST.hs:317:1-22: Splicing declarations
-- makePrisms ''KanjiInfo
--   ======>
_KI_IrregularOkuriganaUsage :: Prism' KanjiInfo ()
_KI_IrregularOkuriganaUsage
  = prism
      (\ () -> KI_IrregularOkuriganaUsage)
      (\ x_ajN6
         -> case x_ajN6 of
              KI_IrregularOkuriganaUsage -> Right ()
              _ -> Left x_ajN6)
_KI_IrregularKanjiUsage :: Prism' KanjiInfo ()
_KI_IrregularKanjiUsage
  = prism
      (\ () -> KI_IrregularKanjiUsage)
      (\ x_ajN7
         -> case x_ajN7 of
              KI_IrregularKanjiUsage -> Right ()
              _ -> Left x_ajN7)
_KI_IrregularKanaUsage :: Prism' KanjiInfo ()
_KI_IrregularKanaUsage
  = prism
      (\ () -> KI_IrregularKanaUsage)
      (\ x_ajN8
         -> case x_ajN8 of
              KI_IrregularKanaUsage -> Right ()
              _ -> Left x_ajN8)
_KI_OutDatedKanji :: Prism' KanjiInfo ()
_KI_OutDatedKanji
  = prism
      (\ () -> KI_OutDatedKanji)
      (\ x_ajN9
         -> case x_ajN9 of
              KI_OutDatedKanji -> Right ()
              _ -> Left x_ajN9)
_KI_Ateji :: Prism' KanjiInfo ()
_KI_Ateji
  = prism
      (\ () -> KI_Ateji)
      (\ x_ajNa
         -> case x_ajNa of
              KI_Ateji -> Right ()
              _ -> Left x_ajNa)
-- src/Data/JMDict/AST.hs:318:1-24: Splicing declarations
-- makePrisms ''ReadingInfo
--   ======>
_RI_OutDatedOrObsoleteKanaUsage :: Prism' ReadingInfo ()
_RI_OutDatedOrObsoleteKanaUsage
  = prism
      (\ () -> RI_OutDatedOrObsoleteKanaUsage)
      (\ x_ajQc
         -> case x_ajQc of
              RI_OutDatedOrObsoleteKanaUsage -> Right ()
              _ -> Left x_ajQc)
_RI_IrregularKanaUsage :: Prism' ReadingInfo ()
_RI_IrregularKanaUsage
  = prism
      (\ () -> RI_IrregularKanaUsage)
      (\ x_ajQd
         -> case x_ajQd of
              RI_IrregularKanaUsage -> Right ()
              _ -> Left x_ajQd)
_RI_OldOrIrregularKanaForm :: Prism' ReadingInfo ()
_RI_OldOrIrregularKanaForm
  = prism
      (\ () -> RI_OldOrIrregularKanaForm)
      (\ x_ajQe
         -> case x_ajQe of
              RI_OldOrIrregularKanaForm -> Right ()
              _ -> Left x_ajQe)
_RI_Gikun :: Prism' ReadingInfo ()
_RI_Gikun
  = prism
      (\ () -> RI_Gikun)
      (\ x_ajQf
         -> case x_ajQf of
              RI_Gikun -> Right ()
              _ -> Left x_ajQf)
-- src/Data/JMDict/AST.hs:319:1-25: Splicing declarations
-- makePrisms ''PartOfSpeech
--   ======>
_PosNoun :: Prism' PartOfSpeech ()
_PosNoun
  = prism
      (\ () -> PosNoun)
      (\ x_ajSH
         -> case x_ajSH of
              PosNoun -> Right ()
              _ -> Left x_ajSH)
_PosNounType :: Prism' PartOfSpeech NounType
_PosNounType
  = prism
      (\ x1_ajSI -> PosNounType x1_ajSI)
      (\ x_ajSJ
         -> case x_ajSJ of
              PosNounType y1_ajSK -> Right y1_ajSK
              _ -> Left x_ajSJ)
_PosPronoun :: Prism' PartOfSpeech ()
_PosPronoun
  = prism
      (\ () -> PosPronoun)
      (\ x_ajSL
         -> case x_ajSL of
              PosPronoun -> Right ()
              _ -> Left x_ajSL)
_PosVerb :: Prism' PartOfSpeech (VerbType, IsTransitive)
_PosVerb
  = prism
      (\ (x1_ajSM, x2_ajSN) -> PosVerb x1_ajSM x2_ajSN)
      (\ x_ajSO
         -> case x_ajSO of
              PosVerb y1_ajSP y2_ajSQ -> Right (y1_ajSP, y2_ajSQ)
              _ -> Left x_ajSO)
_PosAdverb :: Prism' PartOfSpeech Adverb
_PosAdverb
  = prism
      (\ x1_ajSR -> PosAdverb x1_ajSR)
      (\ x_ajSS
         -> case x_ajSS of
              PosAdverb y1_ajST -> Right y1_ajST
              _ -> Left x_ajSS)
_PosAdjective :: Prism' PartOfSpeech Adjective
_PosAdjective
  = prism
      (\ x1_ajSU -> PosAdjective x1_ajSU)
      (\ x_ajSV
         -> case x_ajSV of
              PosAdjective y1_ajSW -> Right y1_ajSW
              _ -> Left x_ajSV)
_PosNumeric :: Prism' PartOfSpeech ()
_PosNumeric
  = prism
      (\ () -> PosNumeric)
      (\ x_ajSX
         -> case x_ajSX of
              PosNumeric -> Right ()
              _ -> Left x_ajSX)
_PosCounter :: Prism' PartOfSpeech ()
_PosCounter
  = prism
      (\ () -> PosCounter)
      (\ x_ajSY
         -> case x_ajSY of
              PosCounter -> Right ()
              _ -> Left x_ajSY)
_PosAuxiliary :: Prism' PartOfSpeech Auxiliary
_PosAuxiliary
  = prism
      (\ x1_ajSZ -> PosAuxiliary x1_ajSZ)
      (\ x_ajT0
         -> case x_ajT0 of
              PosAuxiliary y1_ajT1 -> Right y1_ajT1
              _ -> Left x_ajT0)
_PosExpressions :: Prism' PartOfSpeech ()
_PosExpressions
  = prism
      (\ () -> PosExpressions)
      (\ x_ajT2
         -> case x_ajT2 of
              PosExpressions -> Right ()
              _ -> Left x_ajT2)
_PosIntejection :: Prism' PartOfSpeech ()
_PosIntejection
  = prism
      (\ () -> PosIntejection)
      (\ x_ajT3
         -> case x_ajT3 of
              PosIntejection -> Right ()
              _ -> Left x_ajT3)
_PosSuffix :: Prism' PartOfSpeech ()
_PosSuffix
  = prism
      (\ () -> PosSuffix)
      (\ x_ajT4
         -> case x_ajT4 of
              PosSuffix -> Right ()
              _ -> Left x_ajT4)
_PosPrefix :: Prism' PartOfSpeech ()
_PosPrefix
  = prism
      (\ () -> PosPrefix)
      (\ x_ajT5
         -> case x_ajT5 of
              PosPrefix -> Right ()
              _ -> Left x_ajT5)
_PosConjugation :: Prism' PartOfSpeech ()
_PosConjugation
  = prism
      (\ () -> PosConjugation)
      (\ x_ajT6
         -> case x_ajT6 of
              PosConjugation -> Right ()
              _ -> Left x_ajT6)
_PosUnclassified :: Prism' PartOfSpeech ()
_PosUnclassified
  = prism
      (\ () -> PosUnclassified)
      (\ x_ajT7
         -> case x_ajT7 of
              PosUnclassified -> Right ()
              _ -> Left x_ajT7)
_PosCopula :: Prism' PartOfSpeech ()
_PosCopula
  = prism
      (\ () -> PosCopula)
      (\ x_ajT8
         -> case x_ajT8 of
              PosCopula -> Right ()
              _ -> Left x_ajT8)
_PosParticle :: Prism' PartOfSpeech ()
_PosParticle
  = prism
      (\ () -> PosParticle)
      (\ x_ajT9
         -> case x_ajT9 of
              PosParticle -> Right ()
              _ -> Left x_ajT9)
_PosMisc :: Prism' PartOfSpeech T.Text
_PosMisc
  = prism
      (\ x1_ajTa -> PosMisc x1_ajTa)
      (\ x_ajTb
         -> case x_ajTb of
              PosMisc y1_ajTc -> Right y1_ajTc
              _ -> Left x_ajTb)
-- src/Data/JMDict/AST.hs:320:1-21: Splicing declarations
-- makePrisms ''NounType
--   ======>
_NounWithSuru :: Prism' NounType ()
_NounWithSuru
  = prism
      (\ () -> NounWithSuru)
      (\ x_ak3Q
         -> case x_ak3Q of
              NounWithSuru -> Right ()
              _ -> Left x_ak3Q)
_AdjNoun_No :: Prism' NounType ()
_AdjNoun_No
  = prism
      (\ () -> AdjNoun_No)
      (\ x_ak3R
         -> case x_ak3R of
              AdjNoun_No -> Right ()
              _ -> Left x_ak3R)
_AdverbialNoun :: Prism' NounType ()
_AdverbialNoun
  = prism
      (\ () -> AdverbialNoun)
      (\ x_ak3S
         -> case x_ak3S of
              AdverbialNoun -> Right ()
              _ -> Left x_ak3S)
_SuffixNoun :: Prism' NounType ()
_SuffixNoun
  = prism
      (\ () -> SuffixNoun)
      (\ x_ak3T
         -> case x_ak3T of
              SuffixNoun -> Right ()
              _ -> Left x_ak3T)
_PrefixNoun :: Prism' NounType ()
_PrefixNoun
  = prism
      (\ () -> PrefixNoun)
      (\ x_ak3U
         -> case x_ak3U of
              PrefixNoun -> Right ()
              _ -> Left x_ak3U)
_TemporalNoun :: Prism' NounType ()
_TemporalNoun
  = prism
      (\ () -> TemporalNoun)
      (\ x_ak3V
         -> case x_ak3V of
              TemporalNoun -> Right ()
              _ -> Left x_ak3V)
_ProperNoun :: Prism' NounType ()
_ProperNoun
  = prism
      (\ () -> ProperNoun)
      (\ x_ak3W
         -> case x_ak3W of
              ProperNoun -> Right ()
              _ -> Left x_ak3W)
-- src/Data/JMDict/AST.hs:321:1-21: Splicing declarations
-- makePrisms ''VerbType
--   ======>
_Regular :: Prism' VerbType RegularVerb
_Regular
  = prism
      (\ x1_ak88 -> Regular x1_ak88)
      (\ x_ak89
         -> case x_ak89 of
              Regular y1_ak8a -> Right y1_ak8a
              _ -> Left x_ak89)
_Irregular :: Prism' VerbType IrregularVerb
_Irregular
  = prism
      (\ x1_ak8b -> Irregular x1_ak8b)
      (\ x_ak8c
         -> case x_ak8c of
              Irregular y1_ak8d -> Right y1_ak8d
              _ -> Left x_ak8c)
_Special :: Prism' VerbType SpecialVerb
_Special
  = prism
      (\ x1_ak8e -> Special x1_ak8e)
      (\ x_ak8f
         -> case x_ak8f of
              Special y1_ak8g -> Right y1_ak8g
              _ -> Left x_ak8f)
-- src/Data/JMDict/AST.hs:322:1-24: Splicing declarations
-- makePrisms ''RegularVerb
--   ======>
_Ichidan :: Prism' RegularVerb ()
_Ichidan
  = prism
      (\ () -> Ichidan)
      (\ x_aka8
         -> case x_aka8 of
              Ichidan -> Right ()
              _ -> Left x_aka8)
_Godan :: Prism' RegularVerb VerbEnding
_Godan
  = prism
      (\ x1_aka9 -> Godan x1_aka9)
      (\ x_akaa
         -> case x_akaa of
              Godan y1_akab -> Right y1_akab
              _ -> Left x_akaa)
_Yodan :: Prism' RegularVerb VerbEnding
_Yodan
  = prism
      (\ x1_akac -> Yodan x1_akac)
      (\ x_akad
         -> case x_akad of
              Yodan y1_akae -> Right y1_akae
              _ -> Left x_akad)
_Nidan :: Prism' RegularVerb T.Text
_Nidan
  = prism
      (\ x1_akaf -> Nidan x1_akaf)
      (\ x_akag
         -> case x_akag of
              Nidan y1_akah -> Right y1_akah
              _ -> Left x_akag)
-- src/Data/JMDict/AST.hs:323:1-26: Splicing declarations
-- makePrisms ''IrregularVerb
--   ======>
_SuruI :: Prism' IrregularVerb ()
_SuruI
  = prism
      (\ () -> SuruI)
      (\ x_akcJ
         -> case x_akcJ of
              SuruI -> Right ()
              _ -> Left x_akcJ)
_RuIrregular :: Prism' IrregularVerb ()
_RuIrregular
  = prism
      (\ () -> RuIrregular)
      (\ x_akcK
         -> case x_akcK of
              RuIrregular -> Right ()
              _ -> Left x_akcK)
_NuIrregular :: Prism' IrregularVerb ()
_NuIrregular
  = prism
      (\ () -> NuIrregular)
      (\ x_akcL
         -> case x_akcL of
              NuIrregular -> Right ()
              _ -> Left x_akcL)
_GodanRu :: Prism' IrregularVerb ()
_GodanRu
  = prism
      (\ () -> GodanRu)
      (\ x_akcM
         -> case x_akcM of
              GodanRu -> Right ()
              _ -> Left x_akcM)
-- src/Data/JMDict/AST.hs:324:1-24: Splicing declarations
-- makePrisms ''SpecialVerb
--   ======>
_Kureru :: Prism' SpecialVerb ()
_Kureru
  = prism
      (\ () -> Kureru)
      (\ x_akfe
         -> case x_akfe of
              Kureru -> Right ()
              _ -> Left x_akfe)
_GodanAru :: Prism' SpecialVerb ()
_GodanAru
  = prism
      (\ () -> GodanAru)
      (\ x_akff
         -> case x_akff of
              GodanAru -> Right ()
              _ -> Left x_akff)
_IkuYuku :: Prism' SpecialVerb ()
_IkuYuku
  = prism
      (\ () -> IkuYuku)
      (\ x_akfg
         -> case x_akfg of
              IkuYuku -> Right ()
              _ -> Left x_akfg)
_GodanUEnding :: Prism' SpecialVerb ()
_GodanUEnding
  = prism
      (\ () -> GodanUEnding)
      (\ x_akfh
         -> case x_akfh of
              GodanUEnding -> Right ()
              _ -> Left x_akfh)
_Kuru :: Prism' SpecialVerb ()
_Kuru
  = prism
      (\ () -> Kuru)
      (\ x_akfi
         -> case x_akfi of
              Kuru -> Right ()
              _ -> Left x_akfi)
_SuVerb :: Prism' SpecialVerb ()
_SuVerb
  = prism
      (\ () -> SuVerb)
      (\ x_akfj
         -> case x_akfj of
              SuVerb -> Right ()
              _ -> Left x_akfj)
_SuruS :: Prism' SpecialVerb ()
_SuruS
  = prism
      (\ () -> SuruS)
      (\ x_akfk
         -> case x_akfk of
              SuruS -> Right ()
              _ -> Left x_akfk)
_Zuru :: Prism' SpecialVerb ()
_Zuru
  = prism
      (\ () -> Zuru)
      (\ x_akfl
         -> case x_akfl of
              Zuru -> Right ()
              _ -> Left x_akfl)
-- src/Data/JMDict/AST.hs:325:1-22: Splicing declarations
-- makePrisms ''Adjective
--   ======>
_IAdjective :: Prism' Adjective ()
_IAdjective
  = prism
      (\ () -> IAdjective)
      (\ x_akk7
         -> case x_akk7 of
              IAdjective -> Right ()
              _ -> Left x_akk7)
_NaAdjective :: Prism' Adjective ()
_NaAdjective
  = prism
      (\ () -> NaAdjective)
      (\ x_akk8
         -> case x_akk8 of
              NaAdjective -> Right ()
              _ -> Left x_akk8)
_PreNounAdjective :: Prism' Adjective ()
_PreNounAdjective
  = prism
      (\ () -> PreNounAdjective)
      (\ x_akk9
         -> case x_akk9 of
              PreNounAdjective -> Right ()
              _ -> Left x_akk9)
_TaruAdjective :: Prism' Adjective ()
_TaruAdjective
  = prism
      (\ () -> TaruAdjective)
      (\ x_akka
         -> case x_akka of
              TaruAdjective -> Right ()
              _ -> Left x_akka)
_NariAdjective :: Prism' Adjective ()
_NariAdjective
  = prism
      (\ () -> NariAdjective)
      (\ x_akkb
         -> case x_akkb of
              NariAdjective -> Right ()
              _ -> Left x_akkb)
_PreNominalAdjective :: Prism' Adjective ()
_PreNominalAdjective
  = prism
      (\ () -> PreNominalAdjective)
      (\ x_akkc
         -> case x_akkc of
              PreNominalAdjective -> Right ()
              _ -> Left x_akkc)
_YoiIiAdjective :: Prism' Adjective ()
_YoiIiAdjective
  = prism
      (\ () -> YoiIiAdjective)
      (\ x_akkd
         -> case x_akkd of
              YoiIiAdjective -> Right ()
              _ -> Left x_akkd)
_KuAdjective :: Prism' Adjective ()
_KuAdjective
  = prism
      (\ () -> KuAdjective)
      (\ x_akke
         -> case x_akke of
              KuAdjective -> Right ()
              _ -> Left x_akke)
_ShikuAdjective :: Prism' Adjective ()
_ShikuAdjective
  = prism
      (\ () -> ShikuAdjective)
      (\ x_akkf
         -> case x_akkf of
              ShikuAdjective -> Right ()
              _ -> Left x_akkf)
-- src/Data/JMDict/AST.hs:326:1-19: Splicing declarations
-- makePrisms ''Adverb
--   ======>
_Adverb :: Prism' Adverb ()
_Adverb
  = prism
      (\ () -> Adverb)
      (\ x_akpB
         -> case x_akpB of
              Adverb -> Right ()
              _ -> Left x_akpB)
_Adverb_To :: Prism' Adverb ()
_Adverb_To
  = prism
      (\ () -> Adverb_To)
      (\ x_akpC
         -> case x_akpC of
              Adverb_To -> Right ()
              _ -> Left x_akpC)
-- src/Data/JMDict/AST.hs:327:1-22: Splicing declarations
-- makePrisms ''Auxiliary
--   ======>
_Auxiliary :: Prism' Auxiliary ()
_Auxiliary
  = prism
      (\ () -> Auxiliary)
      (\ x_akqU
         -> case x_akqU of
              Auxiliary -> Right ()
              _ -> Left x_akqU)
_AuxiliaryVerb :: Prism' Auxiliary ()
_AuxiliaryVerb
  = prism
      (\ () -> AuxiliaryVerb)
      (\ x_akqV
         -> case x_akqV of
              AuxiliaryVerb -> Right ()
              _ -> Left x_akqV)
_AuxiliaryAdjective :: Prism' Auxiliary ()
_AuxiliaryAdjective
  = prism
      (\ () -> AuxiliaryAdjective)
      (\ x_akqW
         -> case x_akqW of
              AuxiliaryAdjective -> Right ()
              _ -> Left x_akqW)
-- src/Data/JMDict/AST.hs:328:1-23: Splicing declarations
-- makePrisms ''SenseField
--   ======>
_FieldComp :: Prism' SenseField ()
_FieldComp
  = prism
      (\ () -> FieldComp)
      (\ x_aksO
         -> case x_aksO of
              FieldComp -> Right ()
              _ -> Left x_aksO)
_FieldBuddh :: Prism' SenseField ()
_FieldBuddh
  = prism
      (\ () -> FieldBuddh)
      (\ x_aksP
         -> case x_aksP of
              FieldBuddh -> Right ()
              _ -> Left x_aksP)
_FieldMath :: Prism' SenseField ()
_FieldMath
  = prism
      (\ () -> FieldMath)
      (\ x_aksQ
         -> case x_aksQ of
              FieldMath -> Right ()
              _ -> Left x_aksQ)
_FieldLing :: Prism' SenseField ()
_FieldLing
  = prism
      (\ () -> FieldLing)
      (\ x_aksR
         -> case x_aksR of
              FieldLing -> Right ()
              _ -> Left x_aksR)
_FieldFood :: Prism' SenseField ()
_FieldFood
  = prism
      (\ () -> FieldFood)
      (\ x_aksS
         -> case x_aksS of
              FieldFood -> Right ()
              _ -> Left x_aksS)
_FieldMed :: Prism' SenseField ()
_FieldMed
  = prism
      (\ () -> FieldMed)
      (\ x_aksT
         -> case x_aksT of
              FieldMed -> Right ()
              _ -> Left x_aksT)
_FieldSumo :: Prism' SenseField ()
_FieldSumo
  = prism
      (\ () -> FieldSumo)
      (\ x_aksU
         -> case x_aksU of
              FieldSumo -> Right ()
              _ -> Left x_aksU)
_FieldPhysics :: Prism' SenseField ()
_FieldPhysics
  = prism
      (\ () -> FieldPhysics)
      (\ x_aksV
         -> case x_aksV of
              FieldPhysics -> Right ()
              _ -> Left x_aksV)
_FieldAstron :: Prism' SenseField ()
_FieldAstron
  = prism
      (\ () -> FieldAstron)
      (\ x_aksW
         -> case x_aksW of
              FieldAstron -> Right ()
              _ -> Left x_aksW)
_FieldMusic :: Prism' SenseField ()
_FieldMusic
  = prism
      (\ () -> FieldMusic)
      (\ x_aksX
         -> case x_aksX of
              FieldMusic -> Right ()
              _ -> Left x_aksX)
_FieldBaseb :: Prism' SenseField ()
_FieldBaseb
  = prism
      (\ () -> FieldBaseb)
      (\ x_aksY
         -> case x_aksY of
              FieldBaseb -> Right ()
              _ -> Left x_aksY)
_FieldMahj :: Prism' SenseField ()
_FieldMahj
  = prism
      (\ () -> FieldMahj)
      (\ x_aksZ
         -> case x_aksZ of
              FieldMahj -> Right ()
              _ -> Left x_aksZ)
_FieldBiol :: Prism' SenseField ()
_FieldBiol
  = prism
      (\ () -> FieldBiol)
      (\ x_akt0
         -> case x_akt0 of
              FieldBiol -> Right ()
              _ -> Left x_akt0)
_FieldLaw :: Prism' SenseField ()
_FieldLaw
  = prism
      (\ () -> FieldLaw)
      (\ x_akt1
         -> case x_akt1 of
              FieldLaw -> Right ()
              _ -> Left x_akt1)
_FieldChem :: Prism' SenseField ()
_FieldChem
  = prism
      (\ () -> FieldChem)
      (\ x_akt2
         -> case x_akt2 of
              FieldChem -> Right ()
              _ -> Left x_akt2)
_FieldSports :: Prism' SenseField ()
_FieldSports
  = prism
      (\ () -> FieldSports)
      (\ x_akt3
         -> case x_akt3 of
              FieldSports -> Right ()
              _ -> Left x_akt3)
_FieldAnat :: Prism' SenseField ()
_FieldAnat
  = prism
      (\ () -> FieldAnat)
      (\ x_akt4
         -> case x_akt4 of
              FieldAnat -> Right ()
              _ -> Left x_akt4)
_FieldMa :: Prism' SenseField ()
_FieldMa
  = prism
      (\ () -> FieldMa)
      (\ x_akt5
         -> case x_akt5 of
              FieldMa -> Right ()
              _ -> Left x_akt5)
_FieldGeol :: Prism' SenseField ()
_FieldGeol
  = prism
      (\ () -> FieldGeol)
      (\ x_akt6
         -> case x_akt6 of
              FieldGeol -> Right ()
              _ -> Left x_akt6)
_FieldFinc :: Prism' SenseField ()
_FieldFinc
  = prism
      (\ () -> FieldFinc)
      (\ x_akt7
         -> case x_akt7 of
              FieldFinc -> Right ()
              _ -> Left x_akt7)
_FieldBot :: Prism' SenseField ()
_FieldBot
  = prism
      (\ () -> FieldBot)
      (\ x_akt8
         -> case x_akt8 of
              FieldBot -> Right ()
              _ -> Left x_akt8)
_FieldShogi :: Prism' SenseField ()
_FieldShogi
  = prism
      (\ () -> FieldShogi)
      (\ x_akt9
         -> case x_akt9 of
              FieldShogi -> Right ()
              _ -> Left x_akt9)
_FieldShinto :: Prism' SenseField ()
_FieldShinto
  = prism
      (\ () -> FieldShinto)
      (\ x_akta
         -> case x_akta of
              FieldShinto -> Right ()
              _ -> Left x_akta)
_FieldMil :: Prism' SenseField ()
_FieldMil
  = prism
      (\ () -> FieldMil)
      (\ x_aktb
         -> case x_aktb of
              FieldMil -> Right ()
              _ -> Left x_aktb)
_FieldArchit :: Prism' SenseField ()
_FieldArchit
  = prism
      (\ () -> FieldArchit)
      (\ x_aktc
         -> case x_aktc of
              FieldArchit -> Right ()
              _ -> Left x_aktc)
_FieldEcon :: Prism' SenseField ()
_FieldEcon
  = prism
      (\ () -> FieldEcon)
      (\ x_aktd
         -> case x_aktd of
              FieldEcon -> Right ()
              _ -> Left x_aktd)
_FieldBus :: Prism' SenseField ()
_FieldBus
  = prism
      (\ () -> FieldBus)
      (\ x_akte
         -> case x_akte of
              FieldBus -> Right ()
              _ -> Left x_akte)
_FieldEngr :: Prism' SenseField ()
_FieldEngr
  = prism
      (\ () -> FieldEngr)
      (\ x_aktf
         -> case x_aktf of
              FieldEngr -> Right ()
              _ -> Left x_aktf)
_FieldZool :: Prism' SenseField ()
_FieldZool
  = prism
      (\ () -> FieldZool)
      (\ x_aktg
         -> case x_aktg of
              FieldZool -> Right ()
              _ -> Left x_aktg)
-- src/Data/JMDict/AST.hs:329:1-22: Splicing declarations
-- makePrisms ''SenseMisc
--   ======>
_UsuallyKana :: Prism' SenseMisc ()
_UsuallyKana
  = prism
      (\ () -> UsuallyKana)
      (\ x_akKe
         -> case x_akKe of
              UsuallyKana -> Right ()
              _ -> Left x_akKe)
_Abbreviation :: Prism' SenseMisc ()
_Abbreviation
  = prism
      (\ () -> Abbreviation)
      (\ x_akKf
         -> case x_akKf of
              Abbreviation -> Right ()
              _ -> Left x_akKf)
_Yojijukugo :: Prism' SenseMisc ()
_Yojijukugo
  = prism
      (\ () -> Yojijukugo)
      (\ x_akKg
         -> case x_akKg of
              Yojijukugo -> Right ()
              _ -> Left x_akKg)
_Archaism :: Prism' SenseMisc ()
_Archaism
  = prism
      (\ () -> Archaism)
      (\ x_akKh
         -> case x_akKh of
              Archaism -> Right ()
              _ -> Left x_akKh)
_ObscureTerm :: Prism' SenseMisc ()
_ObscureTerm
  = prism
      (\ () -> ObscureTerm)
      (\ x_akKi
         -> case x_akKi of
              ObscureTerm -> Right ()
              _ -> Left x_akKi)
_OnomatopoeicOrMimeticWord :: Prism' SenseMisc ()
_OnomatopoeicOrMimeticWord
  = prism
      (\ () -> OnomatopoeicOrMimeticWord)
      (\ x_akKj
         -> case x_akKj of
              OnomatopoeicOrMimeticWord -> Right ()
              _ -> Left x_akKj)
_Colloquialism :: Prism' SenseMisc ()
_Colloquialism
  = prism
      (\ () -> Colloquialism)
      (\ x_akKk
         -> case x_akKk of
              Colloquialism -> Right ()
              _ -> Left x_akKk)
_Slang :: Prism' SenseMisc ()
_Slang
  = prism
      (\ () -> Slang)
      (\ x_akKl
         -> case x_akKl of
              Slang -> Right ()
              _ -> Left x_akKl)
_IdiomaticExpression :: Prism' SenseMisc ()
_IdiomaticExpression
  = prism
      (\ () -> IdiomaticExpression)
      (\ x_akKm
         -> case x_akKm of
              IdiomaticExpression -> Right ()
              _ -> Left x_akKm)
_Honorific :: Prism' SenseMisc ()
_Honorific
  = prism
      (\ () -> Honorific)
      (\ x_akKn
         -> case x_akKn of
              Honorific -> Right ()
              _ -> Left x_akKn)
_Derogatory :: Prism' SenseMisc ()
_Derogatory
  = prism
      (\ () -> Derogatory)
      (\ x_akKo
         -> case x_akKo of
              Derogatory -> Right ()
              _ -> Left x_akKo)
_Polite :: Prism' SenseMisc ()
_Polite
  = prism
      (\ () -> Polite)
      (\ x_akKp
         -> case x_akKp of
              Polite -> Right ()
              _ -> Left x_akKp)
_ObsoleteTerm :: Prism' SenseMisc ()
_ObsoleteTerm
  = prism
      (\ () -> ObsoleteTerm)
      (\ x_akKq
         -> case x_akKq of
              ObsoleteTerm -> Right ()
              _ -> Left x_akKq)
_Proverb :: Prism' SenseMisc ()
_Proverb
  = prism
      (\ () -> Proverb)
      (\ x_akKr
         -> case x_akKr of
              Proverb -> Right ()
              _ -> Left x_akKr)
_Sensitive :: Prism' SenseMisc ()
_Sensitive
  = prism
      (\ () -> Sensitive)
      (\ x_akKs
         -> case x_akKs of
              Sensitive -> Right ()
              _ -> Left x_akKs)
_Humble :: Prism' SenseMisc ()
_Humble
  = prism
      (\ () -> Humble)
      (\ x_akKt
         -> case x_akKt of
              Humble -> Right ()
              _ -> Left x_akKt)
_Vulgar :: Prism' SenseMisc ()
_Vulgar
  = prism
      (\ () -> Vulgar)
      (\ x_akKu
         -> case x_akKu of
              Vulgar -> Right ()
              _ -> Left x_akKu)
_Jocular :: Prism' SenseMisc ()
_Jocular
  = prism
      (\ () -> Jocular)
      (\ x_akKv
         -> case x_akKv of
              Jocular -> Right ()
              _ -> Left x_akKv)
_Familiar :: Prism' SenseMisc ()
_Familiar
  = prism
      (\ () -> Familiar)
      (\ x_akKw
         -> case x_akKw of
              Familiar -> Right ()
              _ -> Left x_akKw)
_Childrens :: Prism' SenseMisc ()
_Childrens
  = prism
      (\ () -> Childrens)
      (\ x_akKx
         -> case x_akKx of
              Childrens -> Right ()
              _ -> Left x_akKx)
_FemaleTerm :: Prism' SenseMisc ()
_FemaleTerm
  = prism
      (\ () -> FemaleTerm)
      (\ x_akKy
         -> case x_akKy of
              FemaleTerm -> Right ()
              _ -> Left x_akKy)
_MaleTerm :: Prism' SenseMisc ()
_MaleTerm
  = prism
      (\ () -> MaleTerm)
      (\ x_akKz
         -> case x_akKz of
              MaleTerm -> Right ()
              _ -> Left x_akKz)
_MangaSlang :: Prism' SenseMisc ()
_MangaSlang
  = prism
      (\ () -> MangaSlang)
      (\ x_akKA
         -> case x_akKA of
              MangaSlang -> Right ()
              _ -> Left x_akKA)
_Poetical :: Prism' SenseMisc ()
_Poetical
  = prism
      (\ () -> Poetical)
      (\ x_akKB
         -> case x_akKB of
              Poetical -> Right ()
              _ -> Left x_akKB)
_Rare :: Prism' SenseMisc ()
_Rare
  = prism
      (\ () -> Rare)
      (\ x_akKC
         -> case x_akKC of
              Rare -> Right ()
              _ -> Left x_akKC)
-- src/Data/JMDict/AST.hs:330:1-20: Splicing declarations
-- makePrisms ''Dialect
--   ======>
_KyotoBen :: Prism' Dialect ()
_KyotoBen
  = prism
      (\ () -> KyotoBen)
      (\ x_akZg
         -> case x_akZg of
              KyotoBen -> Right ()
              _ -> Left x_akZg)
_OsakaBen :: Prism' Dialect ()
_OsakaBen
  = prism
      (\ () -> OsakaBen)
      (\ x_akZh
         -> case x_akZh of
              OsakaBen -> Right ()
              _ -> Left x_akZh)
_KansaiBen :: Prism' Dialect ()
_KansaiBen
  = prism
      (\ () -> KansaiBen)
      (\ x_akZi
         -> case x_akZi of
              KansaiBen -> Right ()
              _ -> Left x_akZi)
_KantouBen :: Prism' Dialect ()
_KantouBen
  = prism
      (\ () -> KantouBen)
      (\ x_akZj
         -> case x_akZj of
              KantouBen -> Right ()
              _ -> Left x_akZj)
_TosaBen :: Prism' Dialect ()
_TosaBen
  = prism
      (\ () -> TosaBen)
      (\ x_akZk
         -> case x_akZk of
              TosaBen -> Right ()
              _ -> Left x_akZk)
_TouhokuBen :: Prism' Dialect ()
_TouhokuBen
  = prism
      (\ () -> TouhokuBen)
      (\ x_akZl
         -> case x_akZl of
              TouhokuBen -> Right ()
              _ -> Left x_akZl)
_TsugaruBen :: Prism' Dialect ()
_TsugaruBen
  = prism
      (\ () -> TsugaruBen)
      (\ x_akZm
         -> case x_akZm of
              TsugaruBen -> Right ()
              _ -> Left x_akZm)
_KyuushuuBen :: Prism' Dialect ()
_KyuushuuBen
  = prism
      (\ () -> KyuushuuBen)
      (\ x_akZn
         -> case x_akZn of
              KyuushuuBen -> Right ()
              _ -> Left x_akZn)
_RyuukyuuBen :: Prism' Dialect ()
_RyuukyuuBen
  = prism
      (\ () -> RyuukyuuBen)
      (\ x_akZo
         -> case x_akZo of
              RyuukyuuBen -> Right ()
              _ -> Left x_akZo)
_NaganoBen :: Prism' Dialect ()
_NaganoBen
  = prism
      (\ () -> NaganoBen)
      (\ x_akZp
         -> case x_akZp of
              NaganoBen -> Right ()
              _ -> Left x_akZp)
_HokkaidoBen :: Prism' Dialect ()
_HokkaidoBen
  = prism
      (\ () -> HokkaidoBen)
      (\ x_akZq
         -> case x_akZq of
              HokkaidoBen -> Right ()
              _ -> Left x_akZq)
-- src/Data/JMDict/AST.hs:331:1-18: Splicing declarations
-- makeLenses ''Gloss
--   ======>
glossDefinition :: Lens' Gloss T.Text
glossDefinition f_al5W (Gloss x1_al5X x2_al5Y)
  = fmap (\ y1_al5Z -> Gloss y1_al5Z x2_al5Y) (f_al5W x1_al5X)
{-# INLINE glossDefinition #-}
glossLanguage :: Lens' Gloss T.Text
glossLanguage f_al60 (Gloss x1_al61 x2_al62)
  = fmap (\ y1_al63 -> Gloss x1_al61 y1_al63) (f_al60 x2_al62)
{-# INLINE glossLanguage #-}
-- src/Data/JMDict/AST.hs:332:1-27: Splicing declarations
-- makeLenses ''LanguageSource
--   ======>
sourceFull :: Lens' LanguageSource Bool
sourceFull f_al6L (LanguageSource x1_al6M x2_al6N x3_al6O x4_al6P)
  = fmap
      (\ y1_al6Q -> LanguageSource x1_al6M x2_al6N y1_al6Q x4_al6P)
      (f_al6L x3_al6O)
{-# INLINE sourceFull #-}
sourceLanguage :: Lens' LanguageSource T.Text
sourceLanguage
  f_al6R
  (LanguageSource x1_al6S x2_al6T x3_al6U x4_al6V)
  = fmap
      (\ y1_al6W -> LanguageSource x1_al6S y1_al6W x3_al6U x4_al6V)
      (f_al6R x2_al6T)
{-# INLINE sourceLanguage #-}
sourceOrigin :: Lens' LanguageSource T.Text
sourceOrigin
  f_al6X
  (LanguageSource x1_al6Y x2_al6Z x3_al70 x4_al71)
  = fmap
      (\ y1_al72 -> LanguageSource y1_al72 x2_al6Z x3_al70 x4_al71)
      (f_al6X x1_al6Y)
{-# INLINE sourceOrigin #-}
sourceWaseieigo :: Lens' LanguageSource Bool
sourceWaseieigo
  f_al73
  (LanguageSource x1_al74 x2_al75 x3_al76 x4_al77)
  = fmap
      (\ y1_al78 -> LanguageSource x1_al74 x2_al75 x3_al76 y1_al78)
      (f_al73 x4_al77)
{-# INLINE sourceWaseieigo #-}
