module Web.StaticAPI.APIGeneratorOptions where

data APIGeneratorOptions = APIGeneratorOptions {
    outputDirectory :: FilePath
  , outputFile      :: FilePath
}

defaultOpts :: APIGeneratorOptions
defaultOpts = APIGeneratorOptions {
    outputDirectory = "output"
  , outputFile      = "index.html"
}
