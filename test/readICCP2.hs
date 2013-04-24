{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

import File.Binary
import File.Binary.Instances ()
import File.Binary.Instances.BigEndian ()
import Control.Arrow
import System.Environment

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- readBinaryFile fp
	let Right (ret, _) = fromBinary () cnt :: Either String (ICCP, String)
	print ret
	print $ map tag_signature $ tags ret
	print $ map (($ cnt) . getElement) $ take 17 $ tags ret

getElement :: Tag -> String -> Element
getElement t str = let Right (e, _) = fromBinary t str in e

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
((), Just tag_count){[Tag]}: tags

|]

[binary|

Tag deriving Show

((), Just 4){String}: tag_signature
4: tag_data_offset
4: tag_element_size

|]

data ThreeColors = Red | Green | Blue | White deriving Show

data Element
	= ElementPara ThreeColors Para
	| ElementVCGT VCGT
	| ElementNDIN NDIN
	| ElementMLUC MLUC
	| ElementMMOD MMOD
	| EData String Data
	deriving Show

instance Field Element where
	type FieldArgument Element = Tag
	fromBinary (Tag "aarg" offset size) =
		fmap (first $ ElementPara Red) . fromBinary (offset, size)
	fromBinary (Tag "aagg" offset size) =
		fmap (first $ ElementPara Green) . fromBinary (offset, size)
	fromBinary (Tag "aabg" offset size) =
		fmap (first $ ElementPara Blue) . fromBinary (offset, size)
	fromBinary (Tag "vcgt" offset size) =
		fmap (first $ ElementVCGT) . fromBinary (offset, size)
	fromBinary (Tag "ndin" offset size) =
		fmap (first $ ElementNDIN) . fromBinary (offset, size)
	fromBinary (Tag "dscm" offset size) =
		fmap (first $ ElementMLUC) . fromBinary (offset, size)
	fromBinary (Tag "mmod" offset size) =
		fmap (first $ ElementMMOD) . fromBinary (offset, size)
{-
	fromBinary (Tag "cprt" offset size) =
		fmap (first $ ElementText) . fromBinary (offset, size)
-}
	fromBinary (Tag tn offset size) =
		fmap (first $ EData tn) . fromBinary (offset, size)

[binary|

XYZ

arg :: (Int, Int)

((), Just (fst arg)){String}: pre_XYZ
((), Just 4){String}: type_XYZ
4: 0
4: xyz_X_
4: xyz_Y_
4: xyz_Z_

|]

instance Show XYZ where
	show xyz = "(XYZ " ++ show (type_XYZ xyz) ++ " " ++
		show (xyz_X_ xyz) ++ " " ++ show (xyz_Y_ xyz) ++ " " ++
		show (xyz_Y_ xyz) ++ ")"

[binary|

CHAD

arg :: (Int, Int)

((), Just (fst arg)){String}: pre_CHAD
((), Just 4){String}: type_CHAD
4: 0
4: chad_a0
4: chad_a1
4: chad_a2
4: chad_a3
4: chad_a4
4: chad_a5
4: chad_a6
4: chad_a7
4: chad_a8

|]

instance Show CHAD where
	show chad = "(CHAD " ++
		show (type_CHAD chad) ++ " " ++
		show (chad_a0 chad) ++ " " ++
		show (chad_a1 chad) ++ " " ++
		show (chad_a2 chad) ++ " " ++
		show (chad_a3 chad) ++ " " ++
		show (chad_a4 chad) ++ " " ++
		show (chad_a5 chad) ++ " " ++
		show (chad_a6 chad) ++ " " ++
		show (chad_a7 chad) ++ " " ++
		show (chad_a8 chad) ++ ")"

[binary|

Curv

arg :: (Int, Int)

((), Just (fst arg)){String}: pre_Curv
((), Just 4){String}: type_Curv
4: 0
4: num_Curv
(2, Just num_Curv){[Int]} : body_Curv
-- ((), Just (snd arg - 8)){String}: body_Curv

|]

instance Show Curv where
	show curv = "(Curv " ++
		show (type_Curv curv) ++ " " ++
		show (num_Curv curv) ++ " " ++
		dotdot 100 100 (show $ body_Curv curv) ++ ")"

dotdot :: Int -> Int -> String -> String
dotdot i t str = take i str ++ " ... " ++ reverse (take t $ reverse str)

[binary|

Para

arg :: (Int, Int)

((), Just (fst arg)){String}: pre_Para
((), Just 4){String}: type_Para
4: 0
2: functype_Para
2: 0
4: g_Para
4: a_Para
4: b_Para
4: c_Para
4: d_Para
-- ((), Just (snd arg - 12)){String}: body_Para

|]

instance Show Para where
	show para = "(Para " ++
		show (type_Para para) ++ " " ++
		show (functype_Para para) ++ " " ++
		show (g_Para para) ++ " " ++
		show (a_Para para) ++ " " ++
		show (b_Para para) ++ " " ++
		show (c_Para para) ++ " " ++
		show (d_Para para) ++ ")"

[binary|

VCGT

arg :: (Int, Int)

((), Just (fst arg)){String}: pre_VCGT
((), Just 4){String}: type_VCGT
8: 0
2: hoge_VCGT
2: hage_VCGT
2: hige_VCGT
(2, Just ((snd arg - 18) `div` 2)){[Int]}: body_VCGT

|]

instance Show VCGT where
	show vcgt = "(VCGT " ++
		show (type_VCGT vcgt) ++ " " ++
		show (hoge_VCGT vcgt) ++ " " ++
		show (hage_VCGT vcgt) ++ " " ++
		show (hige_VCGT vcgt) ++ " " ++
		dotdot 100 100 (show $ body_VCGT vcgt) ++ ")"

[binary|

NDIN

arg :: (Int, Int)

((), Just (fst arg)){String}: pre_NDIN
((), Just 4){String}: type_NDIN
4: 0
(4, Just 9){[Int]}: hoge_NDIN
(4, Just 3){[Int]}: hage_NDIN
(2, Just 3){[Int]}: hige_NDIN
(2, Just ((snd arg - 62) `div` 2)){[Int]}: body_NDIN

|]

instance Show NDIN where
	show ndin = "(NDIN " ++
		show (type_NDIN ndin) ++ " " ++
		show (hoge_NDIN ndin) ++ " " ++
		show (hage_NDIN ndin) ++ " " ++
		show (hige_NDIN ndin) ++ " " ++
		dotdot 100 100 (show $ body_NDIN ndin) ++ ")"

[binary|

MLUC

arg :: (Int, Int)

((), Just (fst arg)){String}: pre_MLUC
((), Just 4){String}: type_MLUC
4: 0
4: num_MLUC
4: 12
((), Just num_MLUC){[MLUC_RECORD]}: record_MLUC
((), Just (snd arg - 12 * num_MLUC - 16)){String}: body_MLUC

|]

[binary|

MLUC_RECORD deriving Show

((), Just 2){String}: lang_MLUC
((), Just 2){String}: country_MLUC
4: len_MLUC
4: offset_MLUC

|]

instance Show MLUC where
	show mluc = "(MLUC " ++
		show (type_MLUC mluc) ++ " " ++
		show (num_MLUC mluc) ++ " " ++
		dotdot 90 90 (show $ record_MLUC mluc) ++ " " ++
		dotdot 100 100 (show $ body_MLUC mluc) ++ ")"
--		show (map (body_MLUC mluc !!) [1, 3 .. 300]) ++ ")"

[binary|

MMOD

arg :: (Int, Int)

((), Just (fst arg)){String}: pre_MMOD
((), Just 4){String}: type_MMOD
((), Just (snd arg - 4)){String}: body_MMOD

|]

instance Show MMOD where
	show mmod = "(MMOD " ++
		show (type_MMOD mmod) ++ " " ++
		show (body_MMOD mmod) ++ ")"

[binary|

Text

arg :: (Int, Int)

((), Just (fst arg)){String}: pre_Text
((), Just 4){String}: type_Text
4: 0
((), Just (snd arg - 8)){String}: body_Text

|]

instance Show Text where
	show txt = "(Text " ++
		show (type_Text txt) ++ " " ++
		show (body_Text txt) ++ ")"

[binary|

Data

arg :: (Int, Int)

((), Just (fst arg)){String}: pre_Data
((), Just 4){String}: data_type
(data_type, snd arg - 4){Elem}: data_body
-- ((), Just (snd arg - 4)){String}: body_Data

|]

instance Show Data where
	show dat = "(" ++ show (data_body dat) ++ ")"
{-
	show dat = "(Data " ++
		show (data_type dat) ++ " " ++
		"(" ++ show (data_body dat) ++ "))"
-}

data Elem
	= ElemText Text2
	| ElemXYZ XYZ2
	| ElemDesc Desc
	| ElemCurv Curv2
	| ElemChad CHAD2
	| ElemData String String
	deriving Show

instance Field Elem where
	type FieldArgument Elem = (String, Int)
	fromBinary ("XYZ ", size) =
		fmap (first $ ElemXYZ) . fromBinary size
	fromBinary ("curv", size) =
		fmap (first $ ElemCurv) . fromBinary size
	fromBinary ("sf32", size) =
		fmap (first $ ElemChad) . fromBinary size
	fromBinary ("text", size) =
		fmap (first $ ElemText) . fromBinary size
	fromBinary ("desc", size) =
		fmap (first $ ElemText) . fromBinary size
	fromBinary (typ, size) =
		fmap (first $ ElemData typ) . fromBinary ((), Just size)

[binary|

Text2

arg :: Int

4: 0
((), Just (arg - 4)){String}: text

|]

instance Show Text2 where
	show = const "" -- text

[binary|

XYZ2 deriving Show

arg :: Int

4: 0
4: xyz_X
4: xyz_Y
4: xyz_Z

|]

[binary|

Desc deriving Show

arg :: Int

|]

[binary|

Curv2 deriving Show

arg :: Int

4: 0
4: num_curv
(2, Just num_curv){[Int]}: body_curv

|]

[binary|

CHAD2 deriving Show

arg :: Int

4: 0
4: chad2_a0
4: chad2_a1
4: chad2_a2
4: chad2_a3
4: chad2_a4
4: chad2_a5
4: chad2_a6
4: chad2_a7
4: chad2_a8

|]
