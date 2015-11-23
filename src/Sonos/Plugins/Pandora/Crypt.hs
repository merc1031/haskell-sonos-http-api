{-# LANGUAGE OverloadedStrings #-}
module Sonos.Plugins.Pandora.Crypt where

import qualified Crypto.Cipher as C
import qualified Crypto.Padding as CP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as TE
import Data.Char (toLower)
import Crypto.Types (ByteLength)

outCrypt = "6#26FRL$ZWD"
inCrypt = "R=U!LH$O2B#"

initBlowfish :: BS.ByteString -> C.Blowfish
initBlowfish = either (error . show) C.cipherInit . C.makeKey

encryptBlowfishECB :: BS.ByteString -> BS.ByteString -> BS.ByteString
encryptBlowfishECB key str = C.ecbEncrypt (initBlowfish key) $ padBlockSize (initBlowfish key) str

decryptBlowfishECB :: BS.ByteString -> BS.ByteString -> BS.ByteString
decryptBlowfishECB key cipherText = unpadPkcs7 $ C.ecbDecrypt (initBlowfish key) cipherText

padBlockSize :: C.Blowfish -> BS.ByteString -> BS.ByteString
padBlockSize ciph str = padPkcs7 (C.blockSize ciph) str

padPkcs7 :: ByteLength -> BS.ByteString -> BS.ByteString
padPkcs7 = CP.padPKCS5
{-# INLINE padPkcs7 #-}

unpadPkcs7 :: BS.ByteString -> BS.ByteString
unpadPkcs7 x = BS.take (strlen - lastByte) x
  where
    lastByte = fromIntegral $ BS.last x
    strlen   = BS.length x
{-# INLINE unpadPkcs7 #-}


enc = BSC.map toLower . B16.encode . encryptBlowfishECB outCrypt
dec = decryptBlowfishECB inCrypt . fst . B16.decode . TE.encodeUtf8
decSyncTime = BS.drop 4 . dec

