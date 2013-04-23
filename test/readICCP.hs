{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary
import File.Binary.Instances
import File.Binary.Instances.BigEndian

main :: IO ()
main = do
	cnt <- readBinaryFile "test/iccp"
	let Right (ret, rest) = fromBinary () cnt :: Either String (ICCP, String)
	print ret

[binary|

ICCP deriving Show

4: profile_size
((), Just 4){String}: cmm_type
1: profile_version_major
1: profile_version_minor
2: 0
((), Just 4){String}: profile_device_class
((), Just 4){String}: color_space_of_data
((), Just 4){String}: profile_connection_space
2: create_year
2: create_month
2: create_day
2: create_hour
2: create_minuite
2: create_second
4: "acsp"
((), Just 4){String}: target_platform
4: flags
4: device_manufacturer
4: device_model
8: attributes
4: rendering_intent
4: illuminant_value_X
4: illuminant_value_Y
4: illuminant_value_Z
((), Just 4){String}: profile_creator
((), Just 44){String}: reserved
4: tag_count
((), Just 4){String}: tag_signature
4: offset
4: size
((), Just 4){String}: tag_signature2
4: offset2
4: size2

|]
