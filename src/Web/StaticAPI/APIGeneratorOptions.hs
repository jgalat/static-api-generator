module Web.StaticAPI.APIGeneratorOptions where

data FileFormat = HTML | JSON | Markdown
data DirectoryFormat = RawDirectory

data APIGeneratorOptions = APIGeneratorOptions {
    output          :: FilePath
  , fileFormat      :: FileFormat
  , directoryFormat :: DirectoryFormat
}

defaultOpts :: APIGeneratorOptions
defaultOpts = APIGeneratorOptions {
    output          = "output"
  , fileFormat      = HTML
  , directoryFormat = RawDirectory
}

extension :: FileFormat -> String
extension Markdown  = ".md"
extension HTML      = ".html"
extension JSON      = ".json"
