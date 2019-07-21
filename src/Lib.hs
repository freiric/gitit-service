{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Lib
    ( app
    ) where

import qualified Data.ByteString.UTF8          as B
import qualified Data.ByteString.Lazy          as BSL (ByteString)
import qualified Data.ByteString.Lazy.Char8    as BSLC
import qualified Data.ByteString.Lazy.UTF8     as BSLU (toString)

import qualified Control.Exception             as EX (handle, throw, throwIO)
import qualified Control.Exception.Base        as CEB (Exception)
import           Control.Monad                 (foldM, void, when)
import qualified Control.Monad.Except          as MEX (throwError)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson.TH
import qualified Data.ByteString               as BS (ByteString, concat)
import           Data.FileStore                (FileStore,
                                                FileStoreError (NotFound),
                                                RevisionId, gitFileStore,
                                                retrieve)
import qualified Data.Map                      as M
import qualified Data.Maybe                    as MB (fromMaybe, mapMaybe)
import           Data.Monoid                   ((<>))
import qualified Data.Set                      as SET
import qualified Data.Text                     as T (Text, append, break, empty,
                                                     intercalate, pack, replace,
                                                     splitOn, toLower, unpack,
                                                     words)
import           Data.Yaml
import           GHC.Generics
import           Network.HTTP.Media            ((//), (/:))
import           Network.HTTP.Types.URI        (urlEncode)
import           Network.Wai
import           Servant
import           Servant.HTML.Blaze
import           System.FilePath               (joinPath, (</>))
import           Text.Blaze                    (ToMarkup)
import           Text.Blaze.Html               (Html)
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import           Text.HTML.SanitizeXSS         (sanitizeAttribute)
import qualified Text.Pandoc                   as P (Attr, Block (CodeBlock, Null, RawBlock),
                                                     Extension (Ext_literate_haskell),
                                                     HTMLMathMethod (MathML),
                                                     Inline (Code, Image, Link, RawInline, Str),
                                                     Pandoc (Pandoc), Target,
                                                     WrapOption (WrapNone),
                                                     bottomUp, def, nullMeta,
                                                     pandocExtensions, readHtml,
                                                     readLaTeX, readMarkdown,
                                                     readOrg, readRST,
                                                     readTextile,
                                                     readerExtensions,
--                                                     readerSmart, writeHtml,
                                                     writeHtml5,
                                                     writerHTMLMathMethod,
--                                                     writerHighlight,
--                                                     writerHtml5,
                                                     writerWrapText,
                                                     PandocMonad
                                                     )
import qualified Text.Pandoc.Builder           as PB (text, toList)
import qualified Text.Pandoc.Error             as PE (handleError)
import qualified Text.Pandoc.Extensions        as EXT (enableExtension)
import qualified Text.Pandoc.Class             as PdClass (runIO
                                                          , runIOorExplode
                                                          , PandocIO)
import           Text.Pandoc.Shared            (stringify)
import           Web.HttpApiData               (FromHttpApiData, parseUrlPiece)
-- import Data.Attoparsec.ByteString
-- import Data.String.Conversions

type API = Capture "page" PagePath :> Get '[HTML] Html

instance FromHttpApiData PagePath where
  -- | Parse URL path piece.
  parseUrlPiece :: T.Text -> Either T.Text PagePath
  parseUrlPiece t = Right $ PagePath $ T.splitOn "/" t

-- instance FromText PagePath where
--   fromText t = Just $ PagePath $ T.splitOn "/" t

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server pagePath = liftIO $ generateHtml Nothing pagePath

generateHtml :: Maybe RevisionId -> PagePath -> IO Html
generateHtml mbrev pagePath = do
  let pagePathStr = pathForPage pagePath
  mbcont <- getRawContents pagePathStr mbrev
  case mbcont of
              Just contents -> PdClass.runIOorExplode $ do
                          wikipage <- contentsToWikiPage pagePath contents
                          htmlContents <- pageToHtml wikipage
                          return htmlContents
              Nothing -> EX.throwIO err503 { errBody = "Sorry dear user. Page not found: "  `BSLC.append` BSLC.pack pagePathStr }

-- instance CEB.Exception ServantErr

-- | Path to a wiki page.  Page and page components can't begin with '_'.
data PagePath = PagePath [T.Text] deriving (Show, Read, Eq)

data WikiPage = WikiPage {
    wpName         :: T.Text
  , wpFormat       :: PageFormat
  , wpTOC          :: Bool
  , wpLHS          :: Bool
  , wpTitle        :: [P.Inline]
  , wpCategories   :: [T.Text]
  , wpMetadata     :: M.Map T.Text Value
  , wpCacheable    :: Bool
  , wpContent      :: [P.Block]
  , wpTocHierarchy :: [GititToc]
} deriving (Show)

-- | The Boolean is True for literate Haskell.
data PageFormat = Markdown Bool
                | RST Bool
                | LaTeX Bool
                | HTML Bool
                | Textile Bool
                | Org Bool
                deriving (Read, Show, Eq )

-- | Data type equivalent to Element where only sections and single links in paragraph have been kept
data GititToc = GititLink Int P.Attr [P.Inline] P.Target
                --        lvl attributes       label    link
              | GititSec Int [Int] P.Attr [P.Inline] [GititToc]
                --       lvl num   attributes       label    contents
                deriving (Eq, Read, Show, Generic)

contentsToWikiPage :: PagePath  -> BSL.ByteString -> PdClass.PandocIO WikiPage
contentsToWikiPage page contents = do
--   plugins' <- getPlugins
--  converter <- wikiLinksConverter (pageToPrefix page)
  let conf = getConfig
      title = lastTextFromPage page
      defaultFormat = default_format conf
      simpleTitle = simple_title conf
      converter = wikiLinksConverter (pageToPrefix page)
--  foldM applyPlugin (contentToWikiPage' title contents converter defaultFormat simpleTitle) (tocPlugin : plugins')
  contentToWikiPage' title contents converter defaultFormat simpleTitle
  where
    lastTextFromPage (PagePath ps) = last ps
    -- | Convert links with no URL to wikilinks.
    wikiLinksConverter :: T.Text -> ([P.Inline] -> String)
    wikiLinksConverter prefix = do
--       toMaster <- getRouteToParent
--      toUrl <- lift getUrlRender
       T.unpack . T.append prefix . T.pack . B.toString . urlEncode True . B.fromString . stringify

    pageToPrefix (PagePath []) = T.empty
    pageToPrefix (PagePath ps) = T.intercalate "/" $ init ps ++ [T.empty]

contentToWikiPage' :: T.Text -> BSL.ByteString -> ([P.Inline] -> String) -> PageFormat -> Bool -> PdClass.PandocIO WikiPage
contentToWikiPage' title contents converter defaultFormat simpleTitle = do
  doc <- reader $ T.pack $ BSLU.toString b
  let P.Pandoc _ blocks = sanitizePandoc $ addWikiLinks doc
  return WikiPage {
             wpName        = title
           , wpFormat      = format
           , wpTOC         = toc
           , wpLHS         = lhs
           , wpTitle       = PB.toList $ PB.text $ T.unpack title
           , wpCategories  = extractCategories metadata
           , wpMetadata    = metadata
           , wpCacheable   = True
           , wpContent     = blocks
           , wpTocHierarchy = []
           }
  where
    (h,b) = stripHeader $ BSLC.lines contents
    metadata :: M.Map T.Text Value
    metadata = if BSLC.null h
               then M.empty
                  else MB.fromMaybe M.empty
                       $ decode $! BS.concat $ BSLC.toChunks h
    formatStr = case M.lookup "format" metadata of
                       Just (String s) -> s
                       _               -> ""
    format = MB.fromMaybe defaultFormat $ readPageFormat formatStr
    readerOpts literate = P.def{  -- P.readerSmart = True, 
                               P.readerExtensions =
                               if literate
                                  then EXT.enableExtension P.Ext_literate_haskell P.pandocExtensions
                                  else P.pandocExtensions }
    (reader, lhs) = case format of
                      Markdown l -> (P.readMarkdown (readerOpts l) :: T.Text -> PdClass.PandocIO P.Pandoc, l)
                      Textile  l -> (P.readTextile (readerOpts l), l)
                      LaTeX    l -> (P.readLaTeX (readerOpts l), l)
                      RST      l -> (P.readRST (readerOpts l), l)
                      HTML     l -> (P.readHtml (readerOpts l), l)
                      Org      l -> (P.readOrg (readerOpts l), l)
    fromBool (Bool t) = t
    fromBool _        = False
    toc = maybe False fromBool (M.lookup "toc" metadata)

    convertWikiLinks :: P.Inline -> P.Inline
    convertWikiLinks (P.Link attr ref ("", "")) = P.Link attr (linkTitle ref) (converter ref, "")
    convertWikiLinks (P.Image attr ref ("", "")) = P.Image attr ref (converter ref, "")
    convertWikiLinks x = x

    linkTitle [P.Str refStr] | simpleTitle = [P.Str $ T.unpack $ last . T.splitOn "/" $ T.pack refStr]
    linkTitle x = x

    addWikiLinks :: P.Pandoc -> P.Pandoc
    addWikiLinks = P.bottomUp convertWikiLinks

    stripHeader :: [BSL.ByteString] -> (BSL.ByteString,BSL.ByteString)
    stripHeader (x:xs)
      | isHeaderStart x = let (hs, bs) = break isHeaderEnd xs
                          in  case bs of
                                 []     -> (BSLC.unlines (x:xs), BSLC.empty)
                                 (_:ys) -> (BSLC.unlines hs, BSLC.unlines ys)
      | otherwise = (BSLC.empty, BSLC.unlines (x:xs))
     where isHeaderStart z = ["---"] == BSLC.words z
           isHeaderEnd   z = ["..."] == BSLC.words z
    stripHeader [] = (BSLC.empty, BSLC.empty)

    sanitizePandoc :: P.Pandoc -> P.Pandoc
    sanitizePandoc = P.bottomUp sanitizeBlock . P.bottomUp sanitizeInline
      where
        sanitizeBlock (P.RawBlock _ _) = P.Null
        sanitizeBlock (P.CodeBlock (id',classes,attrs) x) =
          P.CodeBlock (id', classes, sanitizeAttrs attrs) x
        sanitizeBlock x = x
        sanitizeInline (P.RawInline _ _) = P.Str ""
        sanitizeInline (P.Code (id',classes,attrs) x) =
          P.Code (id', classes, sanitizeAttrs attrs) x
        sanitizeInline (P.Link attr lab (src,tit)) = P.Link attr lab (sanitizeURI src,tit)
        sanitizeInline (P.Image attr alt (src,tit)) = P.Image attr alt (sanitizeURI src,tit)
        sanitizeInline x = x
        sanitizeURI src = case sanitizeAttribute ("href", T.pack src) of
                               Just (_,z) -> T.unpack z
                               Nothing    -> ""
        sanitizeAttrs = MB.mapMaybe sanitizeAttr
        sanitizeAttr (x,y) = case sanitizeAttribute (T.pack x, T.pack y) of
                                  Just (w,z) -> Just (T.unpack w, T.unpack z)
                                  Nothing    -> Nothing

extractCategories :: M.Map T.Text Value -> [T.Text]
extractCategories metadata =
  case M.lookup ("categories" :: T.Text) metadata of
       Just (String t) -> T.words $ T.replace "," " " t
       _               -> []

pageToHtml :: P.PandocMonad m => WikiPage ->  m Html
pageToHtml wikiPage =
  P.writeHtml5 P.def{
               P.writerWrapText = P.WrapNone
--              , P.writerHtml5 = True
             -- , P.writerHighlight = True
             , P.writerHTMLMathMethod = P.MathML
             } $ P.Pandoc P.nullMeta (wpContent wikiPage)

getRawContents :: FilePath
               -> Maybe RevisionId
               -> IO (Maybe BSL.ByteString)
getRawContents path rev =
  let fs =  getFileStore in
  EX.handle (\e -> if e == NotFound then return Nothing else EX.throw e)
         $ Just <$> retrieve fs path rev

pathForPage :: PagePath -> FilePath -- 
pathForPage p =
  let conf = getConfig in
      pathForPageP (page_extension conf) p


pathForPageP :: PageExtension -> PagePath -> FilePath
pathForPageP pageExtension (PagePath pagepaths) =  joinPath (map T.unpack pagepaths) <> pageExtension

type PageExtension = FilePath

getConfig :: GititConfig
getConfig =  GititConfig { page_extension = ".page"
                         , repository_path = "/home/freiric/Documents/Perso/Prod/Haskell/gitit2/wikidata"                                         
                         , default_format = Markdown False
                         , simple_title = True
                         }

-- | Configuration for a gitit wiki.
data GititConfig = GititConfig{
--       mime_types       :: M.Map String ContentType -- ^ Table of mime types
       default_format         :: PageFormat               -- ^ Default format for wiki pages
     , repository_path        :: FilePath                 -- ^ Path to wiki
     , page_extension         :: FilePath                 -- ^ Extension for page files
     , static_path            :: FilePath                 -- ^ Path of static dir
     , use_mathjax            :: Bool                     -- ^ Link to mathjax script
     , feed_days              :: Integer                  -- ^ Days back for feed entries
     , feed_minutes           :: Integer                  -- ^ Minutes to cache feed before refresh
     , pandoc_user_data       :: Maybe FilePath           -- ^ Pandoc userdata directory
     , use_cache              :: Bool                     -- ^ Cache pages and files
     , cache_dir              :: FilePath                 -- ^ Path to cache
     , front_page             :: T.Text                     -- ^ Front page of wiki
     , help_page              :: T.Text                     -- ^ Help page
     , latex_engine           :: Maybe FilePath           -- ^ LaTeX engine to use for PDF export
     , simple_title           :: Bool                     -- ^ Hide directory structure for wikilinks? If True `[dir/subdir/mySubpage]()` rendered as mySubpage.
     , toc_depth              :: Maybe Int                -- ^ Depth of table of contents
     , extended_toc           :: Bool                     -- ^ Toc extends over subpage
     , subpage_toc_in_content :: Bool               -- ^ Subpage extends to their Toc in the content
     }

getFileStore :: FileStore
getFileStore =
  let config  = getConfig
      repopath = repository_path config in
  gitFileStore repopath


readPageFormat :: T.Text -> Maybe PageFormat
readPageFormat s =
  case T.toLower s' of
       "markdown"  -> Just $ Markdown lhs
       "textile"   -> Just $ Textile lhs
       "latex"     -> Just $ LaTeX lhs
       "html"      -> Just $ HTML lhs
       "rst"       -> Just $ RST lhs
       "org"       -> Just $ Org lhs
       _           -> Nothing
 where (s',rest) = T.break (=='+') s
       lhs = rest == "+lhs"
             
