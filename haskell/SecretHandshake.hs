module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n = go n []
  where
    go n acc
      | n >= 16 = reverse $ go (mod n 16) acc
      | n >= 8 = go (mod n 8) ("jump" : acc)
      | n >= 4 = go (mod n 4) ("close your eyes" : acc)
      | n >= 2 = go (mod n 2) ("double blink" : acc)
      | n == 1 = go (mod n 1) ("wink" : acc)
      | otherwise = acc
