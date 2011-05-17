{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module Parser where

import AST
import Lexer

-- parser produced by Happy Version 1.18.4

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Expr)
	| HappyAbsSyn5 ([Decl])
	| HappyAbsSyn6 (Decl)
	| HappyAbsSyn8 (SyntacticType)
	| HappyAbsSyn9 ([SyntacticType])
	| HappyAbsSyn10 ([(ID, TypeID)])
	| HappyAbsSyn15 (Expr -> Expr)
	| HappyAbsSyn18 ([Expr])
	| HappyAbsSyn22 ([(ID, Expr)])
	| HappyAbsSyn26 (Maybe Expr)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166 :: () => Int -> ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75 :: () => ({-HappyReduction (HappyIdentity) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (HappyIdentity) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (HappyIdentity) HappyAbsSyn)

action_0 (29) = happyShift action_12
action_0 (30) = happyShift action_13
action_0 (31) = happyShift action_14
action_0 (38) = happyShift action_15
action_0 (39) = happyShift action_16
action_0 (40) = happyShift action_17
action_0 (51) = happyShift action_18
action_0 (55) = happyShift action_19
action_0 (58) = happyShift action_20
action_0 (59) = happyShift action_21
action_0 (65) = happyShift action_22
action_0 (4) = happyGoto action_23
action_0 (13) = happyGoto action_2
action_0 (14) = happyGoto action_3
action_0 (16) = happyGoto action_4
action_0 (17) = happyGoto action_5
action_0 (19) = happyGoto action_6
action_0 (20) = happyGoto action_7
action_0 (21) = happyGoto action_8
action_0 (23) = happyGoto action_9
action_0 (24) = happyGoto action_10
action_0 (25) = happyGoto action_11
action_0 _ = happyFail

action_1 (29) = happyShift action_12
action_1 (30) = happyShift action_13
action_1 (31) = happyShift action_14
action_1 (38) = happyShift action_15
action_1 (39) = happyShift action_16
action_1 (40) = happyShift action_17
action_1 (51) = happyShift action_18
action_1 (55) = happyShift action_19
action_1 (58) = happyShift action_20
action_1 (59) = happyShift action_21
action_1 (65) = happyShift action_22
action_1 (13) = happyGoto action_2
action_1 (14) = happyGoto action_3
action_1 (16) = happyGoto action_4
action_1 (17) = happyGoto action_5
action_1 (19) = happyGoto action_6
action_1 (20) = happyGoto action_7
action_1 (21) = happyGoto action_8
action_1 (23) = happyGoto action_9
action_1 (24) = happyGoto action_10
action_1 (25) = happyGoto action_11
action_1 _ = happyFail

action_2 (43) = happyShift action_44
action_2 (51) = happyShift action_45
action_2 (62) = happyShift action_46
action_2 (63) = happyShift action_47
action_2 (64) = happyShift action_48
action_2 (65) = happyShift action_49
action_2 (66) = happyShift action_50
action_2 (67) = happyShift action_51
action_2 (68) = happyShift action_52
action_2 (69) = happyShift action_53
action_2 (70) = happyShift action_54
action_2 (71) = happyShift action_55
action_2 (72) = happyShift action_56
action_2 _ = happyReduce_1

action_3 (47) = happyShift action_43
action_3 _ = happyReduce_25

action_4 _ = happyReduce_26

action_5 _ = happyReduce_27

action_6 _ = happyReduce_28

action_7 _ = happyReduce_29

action_8 _ = happyReduce_56

action_9 _ = happyReduce_57

action_10 _ = happyReduce_30

action_11 _ = happyReduce_31

action_12 _ = happyReduce_37

action_13 _ = happyReduce_38

action_14 (48) = happyShift action_40
action_14 (49) = happyShift action_41
action_14 (53) = happyShift action_42
action_14 (15) = happyGoto action_39
action_14 _ = happyReduce_33

action_15 _ = happyReduce_36

action_16 _ = happyReduce_67

action_17 (32) = happyShift action_36
action_17 (36) = happyShift action_37
action_17 (37) = happyShift action_38
action_17 (5) = happyGoto action_31
action_17 (6) = happyGoto action_32
action_17 (7) = happyGoto action_33
action_17 (11) = happyGoto action_34
action_17 (12) = happyGoto action_35
action_17 _ = happyReduce_2

action_18 (29) = happyShift action_12
action_18 (30) = happyShift action_13
action_18 (31) = happyShift action_14
action_18 (38) = happyShift action_15
action_18 (39) = happyShift action_16
action_18 (40) = happyShift action_17
action_18 (51) = happyShift action_18
action_18 (55) = happyShift action_19
action_18 (58) = happyShift action_20
action_18 (59) = happyShift action_21
action_18 (65) = happyShift action_22
action_18 (13) = happyGoto action_28
action_18 (14) = happyGoto action_3
action_18 (16) = happyGoto action_4
action_18 (17) = happyGoto action_5
action_18 (19) = happyGoto action_6
action_18 (20) = happyGoto action_7
action_18 (21) = happyGoto action_8
action_18 (23) = happyGoto action_9
action_18 (24) = happyGoto action_10
action_18 (25) = happyGoto action_11
action_18 (27) = happyGoto action_29
action_18 (28) = happyGoto action_30
action_18 _ = happyReduce_72

action_19 (29) = happyShift action_12
action_19 (30) = happyShift action_13
action_19 (31) = happyShift action_14
action_19 (38) = happyShift action_15
action_19 (39) = happyShift action_16
action_19 (40) = happyShift action_17
action_19 (51) = happyShift action_18
action_19 (55) = happyShift action_19
action_19 (58) = happyShift action_20
action_19 (59) = happyShift action_21
action_19 (65) = happyShift action_22
action_19 (13) = happyGoto action_27
action_19 (14) = happyGoto action_3
action_19 (16) = happyGoto action_4
action_19 (17) = happyGoto action_5
action_19 (19) = happyGoto action_6
action_19 (20) = happyGoto action_7
action_19 (21) = happyGoto action_8
action_19 (23) = happyGoto action_9
action_19 (24) = happyGoto action_10
action_19 (25) = happyGoto action_11
action_19 _ = happyFail

action_20 (31) = happyShift action_26
action_20 _ = happyFail

action_21 (29) = happyShift action_12
action_21 (30) = happyShift action_13
action_21 (31) = happyShift action_14
action_21 (38) = happyShift action_15
action_21 (39) = happyShift action_16
action_21 (40) = happyShift action_17
action_21 (51) = happyShift action_18
action_21 (55) = happyShift action_19
action_21 (58) = happyShift action_20
action_21 (59) = happyShift action_21
action_21 (65) = happyShift action_22
action_21 (13) = happyGoto action_25
action_21 (14) = happyGoto action_3
action_21 (16) = happyGoto action_4
action_21 (17) = happyGoto action_5
action_21 (19) = happyGoto action_6
action_21 (20) = happyGoto action_7
action_21 (21) = happyGoto action_8
action_21 (23) = happyGoto action_9
action_21 (24) = happyGoto action_10
action_21 (25) = happyGoto action_11
action_21 _ = happyFail

action_22 (29) = happyShift action_12
action_22 (30) = happyShift action_13
action_22 (31) = happyShift action_14
action_22 (38) = happyShift action_15
action_22 (39) = happyShift action_16
action_22 (40) = happyShift action_17
action_22 (51) = happyShift action_18
action_22 (55) = happyShift action_19
action_22 (58) = happyShift action_20
action_22 (59) = happyShift action_21
action_22 (65) = happyShift action_22
action_22 (13) = happyGoto action_24
action_22 (14) = happyGoto action_3
action_22 (16) = happyGoto action_4
action_22 (17) = happyGoto action_5
action_22 (19) = happyGoto action_6
action_22 (20) = happyGoto action_7
action_22 (21) = happyGoto action_8
action_22 (23) = happyGoto action_9
action_22 (24) = happyGoto action_10
action_22 (25) = happyGoto action_11
action_22 _ = happyFail

action_23 (73) = happyAccept
action_23 _ = happyFail

action_24 (51) = happyFail
action_24 _ = happyReduce_55

action_25 (43) = happyShift action_44
action_25 (51) = happyShift action_45
action_25 (60) = happyShift action_87
action_25 (62) = happyShift action_46
action_25 (63) = happyShift action_47
action_25 (64) = happyShift action_48
action_25 (65) = happyShift action_49
action_25 (66) = happyShift action_50
action_25 (67) = happyShift action_51
action_25 (68) = happyShift action_52
action_25 (69) = happyShift action_53
action_25 (70) = happyShift action_54
action_25 (71) = happyShift action_55
action_25 (72) = happyShift action_56
action_25 _ = happyFail

action_26 (47) = happyShift action_86
action_26 _ = happyFail

action_27 (43) = happyShift action_44
action_27 (51) = happyShift action_45
action_27 (56) = happyShift action_85
action_27 (62) = happyShift action_46
action_27 (63) = happyShift action_47
action_27 (64) = happyShift action_48
action_27 (65) = happyShift action_49
action_27 (66) = happyShift action_50
action_27 (67) = happyShift action_51
action_27 (68) = happyShift action_52
action_27 (69) = happyShift action_53
action_27 (70) = happyShift action_54
action_27 (71) = happyShift action_55
action_27 (72) = happyShift action_56
action_27 _ = happyFail

action_28 (43) = happyShift action_44
action_28 (46) = happyShift action_84
action_28 (51) = happyShift action_45
action_28 (62) = happyShift action_46
action_28 (63) = happyShift action_47
action_28 (64) = happyShift action_48
action_28 (65) = happyShift action_49
action_28 (66) = happyShift action_50
action_28 (67) = happyShift action_51
action_28 (68) = happyShift action_52
action_28 (69) = happyShift action_53
action_28 (70) = happyShift action_54
action_28 (71) = happyShift action_55
action_28 (72) = happyShift action_56
action_28 _ = happyReduce_74

action_29 (52) = happyShift action_83
action_29 _ = happyFail

action_30 _ = happyReduce_73

action_31 (41) = happyShift action_82
action_31 _ = happyFail

action_32 (32) = happyShift action_36
action_32 (36) = happyShift action_37
action_32 (37) = happyShift action_38
action_32 (5) = happyGoto action_81
action_32 (6) = happyGoto action_32
action_32 (7) = happyGoto action_33
action_32 (11) = happyGoto action_34
action_32 (12) = happyGoto action_35
action_32 _ = happyReduce_2

action_33 _ = happyReduce_4

action_34 _ = happyReduce_5

action_35 _ = happyReduce_6

action_36 (31) = happyShift action_80
action_36 _ = happyFail

action_37 (31) = happyShift action_79
action_37 _ = happyFail

action_38 (31) = happyShift action_78
action_38 _ = happyFail

action_39 _ = happyReduce_32

action_40 (31) = happyShift action_77
action_40 _ = happyFail

action_41 (31) = happyShift action_75
action_41 (50) = happyShift action_76
action_41 (22) = happyGoto action_74
action_41 _ = happyFail

action_42 (29) = happyShift action_12
action_42 (30) = happyShift action_13
action_42 (31) = happyShift action_14
action_42 (38) = happyShift action_15
action_42 (39) = happyShift action_16
action_42 (40) = happyShift action_17
action_42 (51) = happyShift action_18
action_42 (55) = happyShift action_19
action_42 (58) = happyShift action_20
action_42 (59) = happyShift action_21
action_42 (65) = happyShift action_22
action_42 (13) = happyGoto action_73
action_42 (14) = happyGoto action_3
action_42 (16) = happyGoto action_4
action_42 (17) = happyGoto action_5
action_42 (19) = happyGoto action_6
action_42 (20) = happyGoto action_7
action_42 (21) = happyGoto action_8
action_42 (23) = happyGoto action_9
action_42 (24) = happyGoto action_10
action_42 (25) = happyGoto action_11
action_42 _ = happyFail

action_43 (29) = happyShift action_12
action_43 (30) = happyShift action_13
action_43 (31) = happyShift action_14
action_43 (38) = happyShift action_15
action_43 (39) = happyShift action_16
action_43 (40) = happyShift action_17
action_43 (51) = happyShift action_18
action_43 (55) = happyShift action_19
action_43 (58) = happyShift action_20
action_43 (59) = happyShift action_21
action_43 (65) = happyShift action_22
action_43 (13) = happyGoto action_72
action_43 (14) = happyGoto action_3
action_43 (16) = happyGoto action_4
action_43 (17) = happyGoto action_5
action_43 (19) = happyGoto action_6
action_43 (20) = happyGoto action_7
action_43 (21) = happyGoto action_8
action_43 (23) = happyGoto action_9
action_43 (24) = happyGoto action_10
action_43 (25) = happyGoto action_11
action_43 _ = happyFail

action_44 (29) = happyShift action_12
action_44 (30) = happyShift action_13
action_44 (31) = happyShift action_14
action_44 (38) = happyShift action_15
action_44 (39) = happyShift action_16
action_44 (40) = happyShift action_17
action_44 (51) = happyShift action_18
action_44 (55) = happyShift action_19
action_44 (58) = happyShift action_20
action_44 (59) = happyShift action_21
action_44 (65) = happyShift action_22
action_44 (13) = happyGoto action_71
action_44 (14) = happyGoto action_3
action_44 (16) = happyGoto action_4
action_44 (17) = happyGoto action_5
action_44 (19) = happyGoto action_6
action_44 (20) = happyGoto action_7
action_44 (21) = happyGoto action_8
action_44 (23) = happyGoto action_9
action_44 (24) = happyGoto action_10
action_44 (25) = happyGoto action_11
action_44 _ = happyFail

action_45 (29) = happyShift action_12
action_45 (30) = happyShift action_13
action_45 (31) = happyShift action_14
action_45 (38) = happyShift action_15
action_45 (39) = happyShift action_16
action_45 (40) = happyShift action_17
action_45 (51) = happyShift action_18
action_45 (52) = happyShift action_70
action_45 (55) = happyShift action_19
action_45 (58) = happyShift action_20
action_45 (59) = happyShift action_21
action_45 (65) = happyShift action_22
action_45 (13) = happyGoto action_68
action_45 (14) = happyGoto action_3
action_45 (16) = happyGoto action_4
action_45 (17) = happyGoto action_5
action_45 (18) = happyGoto action_69
action_45 (19) = happyGoto action_6
action_45 (20) = happyGoto action_7
action_45 (21) = happyGoto action_8
action_45 (23) = happyGoto action_9
action_45 (24) = happyGoto action_10
action_45 (25) = happyGoto action_11
action_45 _ = happyFail

action_46 (29) = happyShift action_12
action_46 (30) = happyShift action_13
action_46 (31) = happyShift action_14
action_46 (38) = happyShift action_15
action_46 (39) = happyShift action_16
action_46 (40) = happyShift action_17
action_46 (51) = happyShift action_18
action_46 (55) = happyShift action_19
action_46 (58) = happyShift action_20
action_46 (59) = happyShift action_21
action_46 (65) = happyShift action_22
action_46 (13) = happyGoto action_67
action_46 (14) = happyGoto action_3
action_46 (16) = happyGoto action_4
action_46 (17) = happyGoto action_5
action_46 (19) = happyGoto action_6
action_46 (20) = happyGoto action_7
action_46 (21) = happyGoto action_8
action_46 (23) = happyGoto action_9
action_46 (24) = happyGoto action_10
action_46 (25) = happyGoto action_11
action_46 _ = happyFail

action_47 (29) = happyShift action_12
action_47 (30) = happyShift action_13
action_47 (31) = happyShift action_14
action_47 (38) = happyShift action_15
action_47 (39) = happyShift action_16
action_47 (40) = happyShift action_17
action_47 (51) = happyShift action_18
action_47 (55) = happyShift action_19
action_47 (58) = happyShift action_20
action_47 (59) = happyShift action_21
action_47 (65) = happyShift action_22
action_47 (13) = happyGoto action_66
action_47 (14) = happyGoto action_3
action_47 (16) = happyGoto action_4
action_47 (17) = happyGoto action_5
action_47 (19) = happyGoto action_6
action_47 (20) = happyGoto action_7
action_47 (21) = happyGoto action_8
action_47 (23) = happyGoto action_9
action_47 (24) = happyGoto action_10
action_47 (25) = happyGoto action_11
action_47 _ = happyFail

action_48 (29) = happyShift action_12
action_48 (30) = happyShift action_13
action_48 (31) = happyShift action_14
action_48 (38) = happyShift action_15
action_48 (39) = happyShift action_16
action_48 (40) = happyShift action_17
action_48 (51) = happyShift action_18
action_48 (55) = happyShift action_19
action_48 (58) = happyShift action_20
action_48 (59) = happyShift action_21
action_48 (65) = happyShift action_22
action_48 (13) = happyGoto action_65
action_48 (14) = happyGoto action_3
action_48 (16) = happyGoto action_4
action_48 (17) = happyGoto action_5
action_48 (19) = happyGoto action_6
action_48 (20) = happyGoto action_7
action_48 (21) = happyGoto action_8
action_48 (23) = happyGoto action_9
action_48 (24) = happyGoto action_10
action_48 (25) = happyGoto action_11
action_48 _ = happyFail

action_49 (29) = happyShift action_12
action_49 (30) = happyShift action_13
action_49 (31) = happyShift action_14
action_49 (38) = happyShift action_15
action_49 (39) = happyShift action_16
action_49 (40) = happyShift action_17
action_49 (51) = happyShift action_18
action_49 (55) = happyShift action_19
action_49 (58) = happyShift action_20
action_49 (59) = happyShift action_21
action_49 (65) = happyShift action_22
action_49 (13) = happyGoto action_64
action_49 (14) = happyGoto action_3
action_49 (16) = happyGoto action_4
action_49 (17) = happyGoto action_5
action_49 (19) = happyGoto action_6
action_49 (20) = happyGoto action_7
action_49 (21) = happyGoto action_8
action_49 (23) = happyGoto action_9
action_49 (24) = happyGoto action_10
action_49 (25) = happyGoto action_11
action_49 _ = happyFail

action_50 (29) = happyShift action_12
action_50 (30) = happyShift action_13
action_50 (31) = happyShift action_14
action_50 (38) = happyShift action_15
action_50 (39) = happyShift action_16
action_50 (40) = happyShift action_17
action_50 (51) = happyShift action_18
action_50 (55) = happyShift action_19
action_50 (58) = happyShift action_20
action_50 (59) = happyShift action_21
action_50 (65) = happyShift action_22
action_50 (13) = happyGoto action_63
action_50 (14) = happyGoto action_3
action_50 (16) = happyGoto action_4
action_50 (17) = happyGoto action_5
action_50 (19) = happyGoto action_6
action_50 (20) = happyGoto action_7
action_50 (21) = happyGoto action_8
action_50 (23) = happyGoto action_9
action_50 (24) = happyGoto action_10
action_50 (25) = happyGoto action_11
action_50 _ = happyFail

action_51 (29) = happyShift action_12
action_51 (30) = happyShift action_13
action_51 (31) = happyShift action_14
action_51 (38) = happyShift action_15
action_51 (39) = happyShift action_16
action_51 (40) = happyShift action_17
action_51 (51) = happyShift action_18
action_51 (55) = happyShift action_19
action_51 (58) = happyShift action_20
action_51 (59) = happyShift action_21
action_51 (65) = happyShift action_22
action_51 (13) = happyGoto action_62
action_51 (14) = happyGoto action_3
action_51 (16) = happyGoto action_4
action_51 (17) = happyGoto action_5
action_51 (19) = happyGoto action_6
action_51 (20) = happyGoto action_7
action_51 (21) = happyGoto action_8
action_51 (23) = happyGoto action_9
action_51 (24) = happyGoto action_10
action_51 (25) = happyGoto action_11
action_51 _ = happyFail

action_52 (29) = happyShift action_12
action_52 (30) = happyShift action_13
action_52 (31) = happyShift action_14
action_52 (38) = happyShift action_15
action_52 (39) = happyShift action_16
action_52 (40) = happyShift action_17
action_52 (51) = happyShift action_18
action_52 (55) = happyShift action_19
action_52 (58) = happyShift action_20
action_52 (59) = happyShift action_21
action_52 (65) = happyShift action_22
action_52 (13) = happyGoto action_61
action_52 (14) = happyGoto action_3
action_52 (16) = happyGoto action_4
action_52 (17) = happyGoto action_5
action_52 (19) = happyGoto action_6
action_52 (20) = happyGoto action_7
action_52 (21) = happyGoto action_8
action_52 (23) = happyGoto action_9
action_52 (24) = happyGoto action_10
action_52 (25) = happyGoto action_11
action_52 _ = happyFail

action_53 (29) = happyShift action_12
action_53 (30) = happyShift action_13
action_53 (31) = happyShift action_14
action_53 (38) = happyShift action_15
action_53 (39) = happyShift action_16
action_53 (40) = happyShift action_17
action_53 (51) = happyShift action_18
action_53 (55) = happyShift action_19
action_53 (58) = happyShift action_20
action_53 (59) = happyShift action_21
action_53 (65) = happyShift action_22
action_53 (13) = happyGoto action_60
action_53 (14) = happyGoto action_3
action_53 (16) = happyGoto action_4
action_53 (17) = happyGoto action_5
action_53 (19) = happyGoto action_6
action_53 (20) = happyGoto action_7
action_53 (21) = happyGoto action_8
action_53 (23) = happyGoto action_9
action_53 (24) = happyGoto action_10
action_53 (25) = happyGoto action_11
action_53 _ = happyFail

action_54 (29) = happyShift action_12
action_54 (30) = happyShift action_13
action_54 (31) = happyShift action_14
action_54 (38) = happyShift action_15
action_54 (39) = happyShift action_16
action_54 (40) = happyShift action_17
action_54 (51) = happyShift action_18
action_54 (55) = happyShift action_19
action_54 (58) = happyShift action_20
action_54 (59) = happyShift action_21
action_54 (65) = happyShift action_22
action_54 (13) = happyGoto action_59
action_54 (14) = happyGoto action_3
action_54 (16) = happyGoto action_4
action_54 (17) = happyGoto action_5
action_54 (19) = happyGoto action_6
action_54 (20) = happyGoto action_7
action_54 (21) = happyGoto action_8
action_54 (23) = happyGoto action_9
action_54 (24) = happyGoto action_10
action_54 (25) = happyGoto action_11
action_54 _ = happyFail

action_55 (29) = happyShift action_12
action_55 (30) = happyShift action_13
action_55 (31) = happyShift action_14
action_55 (38) = happyShift action_15
action_55 (39) = happyShift action_16
action_55 (40) = happyShift action_17
action_55 (51) = happyShift action_18
action_55 (55) = happyShift action_19
action_55 (58) = happyShift action_20
action_55 (59) = happyShift action_21
action_55 (65) = happyShift action_22
action_55 (13) = happyGoto action_58
action_55 (14) = happyGoto action_3
action_55 (16) = happyGoto action_4
action_55 (17) = happyGoto action_5
action_55 (19) = happyGoto action_6
action_55 (20) = happyGoto action_7
action_55 (21) = happyGoto action_8
action_55 (23) = happyGoto action_9
action_55 (24) = happyGoto action_10
action_55 (25) = happyGoto action_11
action_55 _ = happyFail

action_56 (29) = happyShift action_12
action_56 (30) = happyShift action_13
action_56 (31) = happyShift action_14
action_56 (38) = happyShift action_15
action_56 (39) = happyShift action_16
action_56 (40) = happyShift action_17
action_56 (51) = happyShift action_18
action_56 (55) = happyShift action_19
action_56 (58) = happyShift action_20
action_56 (59) = happyShift action_21
action_56 (65) = happyShift action_22
action_56 (13) = happyGoto action_57
action_56 (14) = happyGoto action_3
action_56 (16) = happyGoto action_4
action_56 (17) = happyGoto action_5
action_56 (19) = happyGoto action_6
action_56 (20) = happyGoto action_7
action_56 (21) = happyGoto action_8
action_56 (23) = happyGoto action_9
action_56 (24) = happyGoto action_10
action_56 (25) = happyGoto action_11
action_56 _ = happyFail

action_57 (43) = happyShift action_44
action_57 (51) = happyShift action_45
action_57 (62) = happyShift action_46
action_57 (63) = happyShift action_47
action_57 (64) = happyShift action_48
action_57 (65) = happyShift action_49
action_57 (66) = happyShift action_50
action_57 (67) = happyShift action_51
action_57 (68) = happyShift action_52
action_57 (69) = happyShift action_53
action_57 (70) = happyShift action_54
action_57 _ = happyReduce_54

action_58 (43) = happyShift action_44
action_58 (51) = happyShift action_45
action_58 (62) = happyShift action_46
action_58 (63) = happyShift action_47
action_58 (64) = happyShift action_48
action_58 (65) = happyShift action_49
action_58 (66) = happyShift action_50
action_58 (67) = happyShift action_51
action_58 (68) = happyShift action_52
action_58 (69) = happyShift action_53
action_58 (70) = happyShift action_54
action_58 (72) = happyShift action_56
action_58 _ = happyReduce_53

action_59 (43) = happyFail
action_59 (51) = happyShift action_45
action_59 (62) = happyShift action_46
action_59 (63) = happyShift action_47
action_59 (64) = happyShift action_48
action_59 (65) = happyShift action_49
action_59 (66) = happyFail
action_59 (67) = happyFail
action_59 (68) = happyFail
action_59 (69) = happyFail
action_59 (70) = happyFail
action_59 _ = happyReduce_52

action_60 (43) = happyFail
action_60 (51) = happyShift action_45
action_60 (62) = happyShift action_46
action_60 (63) = happyShift action_47
action_60 (64) = happyShift action_48
action_60 (65) = happyShift action_49
action_60 (66) = happyFail
action_60 (67) = happyFail
action_60 (68) = happyFail
action_60 (69) = happyFail
action_60 (70) = happyFail
action_60 _ = happyReduce_51

action_61 (43) = happyFail
action_61 (51) = happyShift action_45
action_61 (62) = happyShift action_46
action_61 (63) = happyShift action_47
action_61 (64) = happyShift action_48
action_61 (65) = happyShift action_49
action_61 (66) = happyFail
action_61 (67) = happyFail
action_61 (68) = happyFail
action_61 (69) = happyFail
action_61 (70) = happyFail
action_61 _ = happyReduce_50

action_62 (43) = happyFail
action_62 (51) = happyShift action_45
action_62 (62) = happyShift action_46
action_62 (63) = happyShift action_47
action_62 (64) = happyShift action_48
action_62 (65) = happyShift action_49
action_62 (66) = happyFail
action_62 (67) = happyFail
action_62 (68) = happyFail
action_62 (69) = happyFail
action_62 (70) = happyFail
action_62 _ = happyReduce_49

action_63 (43) = happyFail
action_63 (51) = happyShift action_45
action_63 (62) = happyShift action_46
action_63 (63) = happyShift action_47
action_63 (64) = happyShift action_48
action_63 (65) = happyShift action_49
action_63 (66) = happyFail
action_63 (67) = happyFail
action_63 (68) = happyFail
action_63 (69) = happyFail
action_63 (70) = happyFail
action_63 _ = happyReduce_48

action_64 (51) = happyShift action_45
action_64 (62) = happyShift action_46
action_64 (63) = happyShift action_47
action_64 _ = happyReduce_44

action_65 (51) = happyShift action_45
action_65 (62) = happyShift action_46
action_65 (63) = happyShift action_47
action_65 _ = happyReduce_43

action_66 (51) = happyShift action_45
action_66 _ = happyReduce_46

action_67 (51) = happyShift action_45
action_67 _ = happyReduce_45

action_68 (43) = happyShift action_44
action_68 (44) = happyShift action_103
action_68 (51) = happyShift action_45
action_68 (62) = happyShift action_46
action_68 (63) = happyShift action_47
action_68 (64) = happyShift action_48
action_68 (65) = happyShift action_49
action_68 (66) = happyShift action_50
action_68 (67) = happyShift action_51
action_68 (68) = happyShift action_52
action_68 (69) = happyShift action_53
action_68 (70) = happyShift action_54
action_68 (71) = happyShift action_55
action_68 (72) = happyShift action_56
action_68 _ = happyReduce_41

action_69 (52) = happyShift action_102
action_69 _ = happyFail

action_70 _ = happyReduce_39

action_71 (43) = happyFail
action_71 (51) = happyShift action_45
action_71 (62) = happyShift action_46
action_71 (63) = happyShift action_47
action_71 (64) = happyShift action_48
action_71 (65) = happyShift action_49
action_71 (66) = happyFail
action_71 (67) = happyFail
action_71 (68) = happyFail
action_71 (69) = happyFail
action_71 (70) = happyFail
action_71 _ = happyReduce_47

action_72 _ = happyReduce_63

action_73 (43) = happyShift action_44
action_73 (51) = happyShift action_45
action_73 (54) = happyShift action_101
action_73 (62) = happyShift action_46
action_73 (63) = happyShift action_47
action_73 (64) = happyShift action_48
action_73 (65) = happyShift action_49
action_73 (66) = happyShift action_50
action_73 (67) = happyShift action_51
action_73 (68) = happyShift action_52
action_73 (69) = happyShift action_53
action_73 (70) = happyShift action_54
action_73 (71) = happyShift action_55
action_73 (72) = happyShift action_56
action_73 _ = happyFail

action_74 (50) = happyShift action_100
action_74 _ = happyFail

action_75 (43) = happyShift action_99
action_75 _ = happyFail

action_76 _ = happyReduce_58

action_77 (48) = happyShift action_40
action_77 (53) = happyShift action_98
action_77 (15) = happyGoto action_97
action_77 _ = happyReduce_33

action_78 (51) = happyShift action_96
action_78 _ = happyFail

action_79 (45) = happyShift action_94
action_79 (47) = happyShift action_95
action_79 _ = happyFail

action_80 (43) = happyShift action_93
action_80 _ = happyFail

action_81 _ = happyReduce_3

action_82 (29) = happyShift action_12
action_82 (30) = happyShift action_13
action_82 (31) = happyShift action_14
action_82 (38) = happyShift action_15
action_82 (39) = happyShift action_16
action_82 (40) = happyShift action_17
action_82 (51) = happyShift action_18
action_82 (55) = happyShift action_19
action_82 (58) = happyShift action_20
action_82 (59) = happyShift action_21
action_82 (65) = happyShift action_22
action_82 (13) = happyGoto action_28
action_82 (14) = happyGoto action_3
action_82 (16) = happyGoto action_4
action_82 (17) = happyGoto action_5
action_82 (19) = happyGoto action_6
action_82 (20) = happyGoto action_7
action_82 (21) = happyGoto action_8
action_82 (23) = happyGoto action_9
action_82 (24) = happyGoto action_10
action_82 (25) = happyGoto action_11
action_82 (27) = happyGoto action_92
action_82 (28) = happyGoto action_30
action_82 _ = happyReduce_72

action_83 _ = happyReduce_69

action_84 (29) = happyShift action_12
action_84 (30) = happyShift action_13
action_84 (31) = happyShift action_14
action_84 (38) = happyShift action_15
action_84 (39) = happyShift action_16
action_84 (40) = happyShift action_17
action_84 (51) = happyShift action_18
action_84 (55) = happyShift action_19
action_84 (58) = happyShift action_20
action_84 (59) = happyShift action_21
action_84 (65) = happyShift action_22
action_84 (13) = happyGoto action_28
action_84 (14) = happyGoto action_3
action_84 (16) = happyGoto action_4
action_84 (17) = happyGoto action_5
action_84 (19) = happyGoto action_6
action_84 (20) = happyGoto action_7
action_84 (21) = happyGoto action_8
action_84 (23) = happyGoto action_9
action_84 (24) = happyGoto action_10
action_84 (25) = happyGoto action_11
action_84 (28) = happyGoto action_91
action_84 _ = happyFail

action_85 (29) = happyShift action_12
action_85 (30) = happyShift action_13
action_85 (31) = happyShift action_14
action_85 (38) = happyShift action_15
action_85 (39) = happyShift action_16
action_85 (40) = happyShift action_17
action_85 (51) = happyShift action_18
action_85 (55) = happyShift action_19
action_85 (58) = happyShift action_20
action_85 (59) = happyShift action_21
action_85 (65) = happyShift action_22
action_85 (13) = happyGoto action_90
action_85 (14) = happyGoto action_3
action_85 (16) = happyGoto action_4
action_85 (17) = happyGoto action_5
action_85 (19) = happyGoto action_6
action_85 (20) = happyGoto action_7
action_85 (21) = happyGoto action_8
action_85 (23) = happyGoto action_9
action_85 (24) = happyGoto action_10
action_85 (25) = happyGoto action_11
action_85 _ = happyFail

action_86 (29) = happyShift action_12
action_86 (30) = happyShift action_13
action_86 (31) = happyShift action_14
action_86 (38) = happyShift action_15
action_86 (39) = happyShift action_16
action_86 (40) = happyShift action_17
action_86 (51) = happyShift action_18
action_86 (55) = happyShift action_19
action_86 (58) = happyShift action_20
action_86 (59) = happyShift action_21
action_86 (65) = happyShift action_22
action_86 (13) = happyGoto action_89
action_86 (14) = happyGoto action_3
action_86 (16) = happyGoto action_4
action_86 (17) = happyGoto action_5
action_86 (19) = happyGoto action_6
action_86 (20) = happyGoto action_7
action_86 (21) = happyGoto action_8
action_86 (23) = happyGoto action_9
action_86 (24) = happyGoto action_10
action_86 (25) = happyGoto action_11
action_86 _ = happyFail

action_87 (29) = happyShift action_12
action_87 (30) = happyShift action_13
action_87 (31) = happyShift action_14
action_87 (38) = happyShift action_15
action_87 (39) = happyShift action_16
action_87 (40) = happyShift action_17
action_87 (51) = happyShift action_18
action_87 (55) = happyShift action_19
action_87 (58) = happyShift action_20
action_87 (59) = happyShift action_21
action_87 (65) = happyShift action_22
action_87 (13) = happyGoto action_88
action_87 (14) = happyGoto action_3
action_87 (16) = happyGoto action_4
action_87 (17) = happyGoto action_5
action_87 (19) = happyGoto action_6
action_87 (20) = happyGoto action_7
action_87 (21) = happyGoto action_8
action_87 (23) = happyGoto action_9
action_87 (24) = happyGoto action_10
action_87 (25) = happyGoto action_11
action_87 _ = happyFail

action_88 _ = happyReduce_65

action_89 (43) = happyShift action_44
action_89 (51) = happyShift action_45
action_89 (61) = happyShift action_122
action_89 (62) = happyShift action_46
action_89 (63) = happyShift action_47
action_89 (64) = happyShift action_48
action_89 (65) = happyShift action_49
action_89 (66) = happyShift action_50
action_89 (67) = happyShift action_51
action_89 (68) = happyShift action_52
action_89 (69) = happyShift action_53
action_89 (70) = happyShift action_54
action_89 (71) = happyShift action_55
action_89 (72) = happyShift action_56
action_89 _ = happyFail

action_90 (43) = happyShift action_44
action_90 (51) = happyShift action_45
action_90 (57) = happyShift action_121
action_90 (62) = happyShift action_46
action_90 (63) = happyShift action_47
action_90 (64) = happyShift action_48
action_90 (65) = happyShift action_49
action_90 (66) = happyShift action_50
action_90 (67) = happyShift action_51
action_90 (68) = happyShift action_52
action_90 (69) = happyShift action_53
action_90 (70) = happyShift action_54
action_90 (71) = happyShift action_55
action_90 (72) = happyShift action_56
action_90 (26) = happyGoto action_120
action_90 _ = happyReduce_70

action_91 _ = happyReduce_75

action_92 (42) = happyShift action_119
action_92 _ = happyFail

action_93 (31) = happyShift action_115
action_93 (34) = happyShift action_116
action_93 (49) = happyShift action_117
action_93 (51) = happyShift action_118
action_93 (8) = happyGoto action_114
action_93 _ = happyFail

action_94 (31) = happyShift action_113
action_94 _ = happyFail

action_95 (29) = happyShift action_12
action_95 (30) = happyShift action_13
action_95 (31) = happyShift action_14
action_95 (38) = happyShift action_15
action_95 (39) = happyShift action_16
action_95 (40) = happyShift action_17
action_95 (51) = happyShift action_18
action_95 (55) = happyShift action_19
action_95 (58) = happyShift action_20
action_95 (59) = happyShift action_21
action_95 (65) = happyShift action_22
action_95 (13) = happyGoto action_112
action_95 (14) = happyGoto action_3
action_95 (16) = happyGoto action_4
action_95 (17) = happyGoto action_5
action_95 (19) = happyGoto action_6
action_95 (20) = happyGoto action_7
action_95 (21) = happyGoto action_8
action_95 (23) = happyGoto action_9
action_95 (24) = happyGoto action_10
action_95 (25) = happyGoto action_11
action_95 _ = happyFail

action_96 (31) = happyShift action_110
action_96 (52) = happyShift action_111
action_96 (10) = happyGoto action_109
action_96 _ = happyFail

action_97 _ = happyReduce_34

action_98 (29) = happyShift action_12
action_98 (30) = happyShift action_13
action_98 (31) = happyShift action_14
action_98 (38) = happyShift action_15
action_98 (39) = happyShift action_16
action_98 (40) = happyShift action_17
action_98 (51) = happyShift action_18
action_98 (55) = happyShift action_19
action_98 (58) = happyShift action_20
action_98 (59) = happyShift action_21
action_98 (65) = happyShift action_22
action_98 (13) = happyGoto action_108
action_98 (14) = happyGoto action_3
action_98 (16) = happyGoto action_4
action_98 (17) = happyGoto action_5
action_98 (19) = happyGoto action_6
action_98 (20) = happyGoto action_7
action_98 (21) = happyGoto action_8
action_98 (23) = happyGoto action_9
action_98 (24) = happyGoto action_10
action_98 (25) = happyGoto action_11
action_98 _ = happyFail

action_99 (29) = happyShift action_12
action_99 (30) = happyShift action_13
action_99 (31) = happyShift action_14
action_99 (38) = happyShift action_15
action_99 (39) = happyShift action_16
action_99 (40) = happyShift action_17
action_99 (51) = happyShift action_18
action_99 (55) = happyShift action_19
action_99 (58) = happyShift action_20
action_99 (59) = happyShift action_21
action_99 (65) = happyShift action_22
action_99 (13) = happyGoto action_107
action_99 (14) = happyGoto action_3
action_99 (16) = happyGoto action_4
action_99 (17) = happyGoto action_5
action_99 (19) = happyGoto action_6
action_99 (20) = happyGoto action_7
action_99 (21) = happyGoto action_8
action_99 (23) = happyGoto action_9
action_99 (24) = happyGoto action_10
action_99 (25) = happyGoto action_11
action_99 _ = happyFail

action_100 _ = happyReduce_59

action_101 (35) = happyShift action_106
action_101 (48) = happyShift action_40
action_101 (53) = happyShift action_98
action_101 (15) = happyGoto action_105
action_101 _ = happyReduce_33

action_102 _ = happyReduce_40

action_103 (29) = happyShift action_12
action_103 (30) = happyShift action_13
action_103 (31) = happyShift action_14
action_103 (38) = happyShift action_15
action_103 (39) = happyShift action_16
action_103 (40) = happyShift action_17
action_103 (51) = happyShift action_18
action_103 (55) = happyShift action_19
action_103 (58) = happyShift action_20
action_103 (59) = happyShift action_21
action_103 (65) = happyShift action_22
action_103 (13) = happyGoto action_68
action_103 (14) = happyGoto action_3
action_103 (16) = happyGoto action_4
action_103 (17) = happyGoto action_5
action_103 (18) = happyGoto action_104
action_103 (19) = happyGoto action_6
action_103 (20) = happyGoto action_7
action_103 (21) = happyGoto action_8
action_103 (23) = happyGoto action_9
action_103 (24) = happyGoto action_10
action_103 (25) = happyGoto action_11
action_103 _ = happyFail

action_104 _ = happyReduce_42

action_105 _ = happyReduce_35

action_106 (29) = happyShift action_12
action_106 (30) = happyShift action_13
action_106 (31) = happyShift action_14
action_106 (38) = happyShift action_15
action_106 (39) = happyShift action_16
action_106 (40) = happyShift action_17
action_106 (51) = happyShift action_18
action_106 (55) = happyShift action_19
action_106 (58) = happyShift action_20
action_106 (59) = happyShift action_21
action_106 (65) = happyShift action_22
action_106 (13) = happyGoto action_139
action_106 (14) = happyGoto action_3
action_106 (16) = happyGoto action_4
action_106 (17) = happyGoto action_5
action_106 (19) = happyGoto action_6
action_106 (20) = happyGoto action_7
action_106 (21) = happyGoto action_8
action_106 (23) = happyGoto action_9
action_106 (24) = happyGoto action_10
action_106 (25) = happyGoto action_11
action_106 _ = happyFail

action_107 (43) = happyShift action_44
action_107 (44) = happyShift action_138
action_107 (51) = happyShift action_45
action_107 (62) = happyShift action_46
action_107 (63) = happyShift action_47
action_107 (64) = happyShift action_48
action_107 (65) = happyShift action_49
action_107 (66) = happyShift action_50
action_107 (67) = happyShift action_51
action_107 (68) = happyShift action_52
action_107 (69) = happyShift action_53
action_107 (70) = happyShift action_54
action_107 (71) = happyShift action_55
action_107 (72) = happyShift action_56
action_107 _ = happyReduce_60

action_108 (43) = happyShift action_44
action_108 (51) = happyShift action_45
action_108 (54) = happyShift action_137
action_108 (62) = happyShift action_46
action_108 (63) = happyShift action_47
action_108 (64) = happyShift action_48
action_108 (65) = happyShift action_49
action_108 (66) = happyShift action_50
action_108 (67) = happyShift action_51
action_108 (68) = happyShift action_52
action_108 (69) = happyShift action_53
action_108 (70) = happyShift action_54
action_108 (71) = happyShift action_55
action_108 (72) = happyShift action_56
action_108 _ = happyFail

action_109 (52) = happyShift action_136
action_109 _ = happyFail

action_110 (45) = happyShift action_135
action_110 _ = happyFail

action_111 (43) = happyShift action_133
action_111 (45) = happyShift action_134
action_111 _ = happyFail

action_112 (43) = happyShift action_44
action_112 (51) = happyShift action_45
action_112 (62) = happyShift action_46
action_112 (63) = happyShift action_47
action_112 (64) = happyShift action_48
action_112 (65) = happyShift action_49
action_112 (66) = happyShift action_50
action_112 (67) = happyShift action_51
action_112 (68) = happyShift action_52
action_112 (69) = happyShift action_53
action_112 (70) = happyShift action_54
action_112 (71) = happyShift action_55
action_112 (72) = happyShift action_56
action_112 _ = happyReduce_19

action_113 (47) = happyShift action_132
action_113 _ = happyFail

action_114 (33) = happyShift action_131
action_114 _ = happyReduce_7

action_115 _ = happyReduce_8

action_116 (35) = happyShift action_130
action_116 _ = happyFail

action_117 (31) = happyShift action_110
action_117 (50) = happyShift action_129
action_117 (10) = happyGoto action_128
action_117 _ = happyFail

action_118 (31) = happyShift action_115
action_118 (34) = happyShift action_116
action_118 (49) = happyShift action_117
action_118 (51) = happyShift action_118
action_118 (52) = happyShift action_127
action_118 (8) = happyGoto action_125
action_118 (9) = happyGoto action_126
action_118 _ = happyFail

action_119 _ = happyReduce_68

action_120 _ = happyReduce_64

action_121 (29) = happyShift action_12
action_121 (30) = happyShift action_13
action_121 (31) = happyShift action_14
action_121 (38) = happyShift action_15
action_121 (39) = happyShift action_16
action_121 (40) = happyShift action_17
action_121 (51) = happyShift action_18
action_121 (55) = happyShift action_19
action_121 (58) = happyShift action_20
action_121 (59) = happyShift action_21
action_121 (65) = happyShift action_22
action_121 (13) = happyGoto action_124
action_121 (14) = happyGoto action_3
action_121 (16) = happyGoto action_4
action_121 (17) = happyGoto action_5
action_121 (19) = happyGoto action_6
action_121 (20) = happyGoto action_7
action_121 (21) = happyGoto action_8
action_121 (23) = happyGoto action_9
action_121 (24) = happyGoto action_10
action_121 (25) = happyGoto action_11
action_121 _ = happyFail

action_122 (29) = happyShift action_12
action_122 (30) = happyShift action_13
action_122 (31) = happyShift action_14
action_122 (38) = happyShift action_15
action_122 (39) = happyShift action_16
action_122 (40) = happyShift action_17
action_122 (51) = happyShift action_18
action_122 (55) = happyShift action_19
action_122 (58) = happyShift action_20
action_122 (59) = happyShift action_21
action_122 (65) = happyShift action_22
action_122 (13) = happyGoto action_123
action_122 (14) = happyGoto action_3
action_122 (16) = happyGoto action_4
action_122 (17) = happyGoto action_5
action_122 (19) = happyGoto action_6
action_122 (20) = happyGoto action_7
action_122 (21) = happyGoto action_8
action_122 (23) = happyGoto action_9
action_122 (24) = happyGoto action_10
action_122 (25) = happyGoto action_11
action_122 _ = happyFail

action_123 (43) = happyShift action_44
action_123 (51) = happyShift action_45
action_123 (60) = happyShift action_153
action_123 (62) = happyShift action_46
action_123 (63) = happyShift action_47
action_123 (64) = happyShift action_48
action_123 (65) = happyShift action_49
action_123 (66) = happyShift action_50
action_123 (67) = happyShift action_51
action_123 (68) = happyShift action_52
action_123 (69) = happyShift action_53
action_123 (70) = happyShift action_54
action_123 (71) = happyShift action_55
action_123 (72) = happyShift action_56
action_123 _ = happyFail

action_124 _ = happyReduce_71

action_125 (33) = happyShift action_131
action_125 (44) = happyShift action_152
action_125 _ = happyReduce_15

action_126 (52) = happyShift action_151
action_126 _ = happyFail

action_127 (33) = happyShift action_150
action_127 _ = happyFail

action_128 (50) = happyShift action_149
action_128 _ = happyFail

action_129 _ = happyReduce_9

action_130 (31) = happyShift action_148
action_130 _ = happyFail

action_131 (31) = happyShift action_115
action_131 (34) = happyShift action_116
action_131 (49) = happyShift action_117
action_131 (51) = happyShift action_118
action_131 (8) = happyGoto action_147
action_131 _ = happyFail

action_132 (29) = happyShift action_12
action_132 (30) = happyShift action_13
action_132 (31) = happyShift action_14
action_132 (38) = happyShift action_15
action_132 (39) = happyShift action_16
action_132 (40) = happyShift action_17
action_132 (51) = happyShift action_18
action_132 (55) = happyShift action_19
action_132 (58) = happyShift action_20
action_132 (59) = happyShift action_21
action_132 (65) = happyShift action_22
action_132 (13) = happyGoto action_146
action_132 (14) = happyGoto action_3
action_132 (16) = happyGoto action_4
action_132 (17) = happyGoto action_5
action_132 (19) = happyGoto action_6
action_132 (20) = happyGoto action_7
action_132 (21) = happyGoto action_8
action_132 (23) = happyGoto action_9
action_132 (24) = happyGoto action_10
action_132 (25) = happyGoto action_11
action_132 _ = happyFail

action_133 (29) = happyShift action_12
action_133 (30) = happyShift action_13
action_133 (31) = happyShift action_14
action_133 (38) = happyShift action_15
action_133 (39) = happyShift action_16
action_133 (40) = happyShift action_17
action_133 (51) = happyShift action_18
action_133 (55) = happyShift action_19
action_133 (58) = happyShift action_20
action_133 (59) = happyShift action_21
action_133 (65) = happyShift action_22
action_133 (13) = happyGoto action_145
action_133 (14) = happyGoto action_3
action_133 (16) = happyGoto action_4
action_133 (17) = happyGoto action_5
action_133 (19) = happyGoto action_6
action_133 (20) = happyGoto action_7
action_133 (21) = happyGoto action_8
action_133 (23) = happyGoto action_9
action_133 (24) = happyGoto action_10
action_133 (25) = happyGoto action_11
action_133 _ = happyFail

action_134 (31) = happyShift action_144
action_134 _ = happyFail

action_135 (31) = happyShift action_143
action_135 _ = happyFail

action_136 (43) = happyShift action_141
action_136 (45) = happyShift action_142
action_136 _ = happyFail

action_137 (48) = happyShift action_40
action_137 (53) = happyShift action_98
action_137 (15) = happyGoto action_105
action_137 _ = happyReduce_33

action_138 (31) = happyShift action_75
action_138 (22) = happyGoto action_140
action_138 _ = happyFail

action_139 _ = happyReduce_62

action_140 _ = happyReduce_61

action_141 (29) = happyShift action_12
action_141 (30) = happyShift action_13
action_141 (31) = happyShift action_14
action_141 (38) = happyShift action_15
action_141 (39) = happyShift action_16
action_141 (40) = happyShift action_17
action_141 (51) = happyShift action_18
action_141 (55) = happyShift action_19
action_141 (58) = happyShift action_20
action_141 (59) = happyShift action_21
action_141 (65) = happyShift action_22
action_141 (13) = happyGoto action_161
action_141 (14) = happyGoto action_3
action_141 (16) = happyGoto action_4
action_141 (17) = happyGoto action_5
action_141 (19) = happyGoto action_6
action_141 (20) = happyGoto action_7
action_141 (21) = happyGoto action_8
action_141 (23) = happyGoto action_9
action_141 (24) = happyGoto action_10
action_141 (25) = happyGoto action_11
action_141 _ = happyFail

action_142 (31) = happyShift action_160
action_142 _ = happyFail

action_143 (44) = happyShift action_159
action_143 _ = happyReduce_17

action_144 (43) = happyShift action_158
action_144 _ = happyFail

action_145 (43) = happyShift action_44
action_145 (51) = happyShift action_45
action_145 (62) = happyShift action_46
action_145 (63) = happyShift action_47
action_145 (64) = happyShift action_48
action_145 (65) = happyShift action_49
action_145 (66) = happyShift action_50
action_145 (67) = happyShift action_51
action_145 (68) = happyShift action_52
action_145 (69) = happyShift action_53
action_145 (70) = happyShift action_54
action_145 (71) = happyShift action_55
action_145 (72) = happyShift action_56
action_145 _ = happyReduce_21

action_146 (43) = happyShift action_44
action_146 (51) = happyShift action_45
action_146 (62) = happyShift action_46
action_146 (63) = happyShift action_47
action_146 (64) = happyShift action_48
action_146 (65) = happyShift action_49
action_146 (66) = happyShift action_50
action_146 (67) = happyShift action_51
action_146 (68) = happyShift action_52
action_146 (69) = happyShift action_53
action_146 (70) = happyShift action_54
action_146 (71) = happyShift action_55
action_146 (72) = happyShift action_56
action_146 _ = happyReduce_20

action_147 (33) = happyShift action_131
action_147 _ = happyReduce_12

action_148 _ = happyReduce_11

action_149 _ = happyReduce_10

action_150 (31) = happyShift action_115
action_150 (34) = happyShift action_116
action_150 (49) = happyShift action_117
action_150 (51) = happyShift action_118
action_150 (8) = happyGoto action_157
action_150 _ = happyFail

action_151 (33) = happyShift action_156
action_151 _ = happyFail

action_152 (31) = happyShift action_115
action_152 (34) = happyShift action_116
action_152 (49) = happyShift action_117
action_152 (51) = happyShift action_118
action_152 (8) = happyGoto action_125
action_152 (9) = happyGoto action_155
action_152 _ = happyFail

action_153 (29) = happyShift action_12
action_153 (30) = happyShift action_13
action_153 (31) = happyShift action_14
action_153 (38) = happyShift action_15
action_153 (39) = happyShift action_16
action_153 (40) = happyShift action_17
action_153 (51) = happyShift action_18
action_153 (55) = happyShift action_19
action_153 (58) = happyShift action_20
action_153 (59) = happyShift action_21
action_153 (65) = happyShift action_22
action_153 (13) = happyGoto action_154
action_153 (14) = happyGoto action_3
action_153 (16) = happyGoto action_4
action_153 (17) = happyGoto action_5
action_153 (19) = happyGoto action_6
action_153 (20) = happyGoto action_7
action_153 (21) = happyGoto action_8
action_153 (23) = happyGoto action_9
action_153 (24) = happyGoto action_10
action_153 (25) = happyGoto action_11
action_153 _ = happyFail

action_154 _ = happyReduce_66

action_155 _ = happyReduce_16

action_156 (31) = happyShift action_115
action_156 (34) = happyShift action_116
action_156 (49) = happyShift action_117
action_156 (51) = happyShift action_118
action_156 (8) = happyGoto action_165
action_156 _ = happyFail

action_157 (33) = happyShift action_131
action_157 _ = happyReduce_14

action_158 (29) = happyShift action_12
action_158 (30) = happyShift action_13
action_158 (31) = happyShift action_14
action_158 (38) = happyShift action_15
action_158 (39) = happyShift action_16
action_158 (40) = happyShift action_17
action_158 (51) = happyShift action_18
action_158 (55) = happyShift action_19
action_158 (58) = happyShift action_20
action_158 (59) = happyShift action_21
action_158 (65) = happyShift action_22
action_158 (13) = happyGoto action_164
action_158 (14) = happyGoto action_3
action_158 (16) = happyGoto action_4
action_158 (17) = happyGoto action_5
action_158 (19) = happyGoto action_6
action_158 (20) = happyGoto action_7
action_158 (21) = happyGoto action_8
action_158 (23) = happyGoto action_9
action_158 (24) = happyGoto action_10
action_158 (25) = happyGoto action_11
action_158 _ = happyFail

action_159 (31) = happyShift action_110
action_159 (10) = happyGoto action_163
action_159 _ = happyFail

action_160 (43) = happyShift action_162
action_160 _ = happyFail

action_161 (43) = happyShift action_44
action_161 (51) = happyShift action_45
action_161 (62) = happyShift action_46
action_161 (63) = happyShift action_47
action_161 (64) = happyShift action_48
action_161 (65) = happyShift action_49
action_161 (66) = happyShift action_50
action_161 (67) = happyShift action_51
action_161 (68) = happyShift action_52
action_161 (69) = happyShift action_53
action_161 (70) = happyShift action_54
action_161 (71) = happyShift action_55
action_161 (72) = happyShift action_56
action_161 _ = happyReduce_22

action_162 (29) = happyShift action_12
action_162 (30) = happyShift action_13
action_162 (31) = happyShift action_14
action_162 (38) = happyShift action_15
action_162 (39) = happyShift action_16
action_162 (40) = happyShift action_17
action_162 (51) = happyShift action_18
action_162 (55) = happyShift action_19
action_162 (58) = happyShift action_20
action_162 (59) = happyShift action_21
action_162 (65) = happyShift action_22
action_162 (13) = happyGoto action_166
action_162 (14) = happyGoto action_3
action_162 (16) = happyGoto action_4
action_162 (17) = happyGoto action_5
action_162 (19) = happyGoto action_6
action_162 (20) = happyGoto action_7
action_162 (21) = happyGoto action_8
action_162 (23) = happyGoto action_9
action_162 (24) = happyGoto action_10
action_162 (25) = happyGoto action_11
action_162 _ = happyFail

action_163 _ = happyReduce_18

action_164 (43) = happyShift action_44
action_164 (51) = happyShift action_45
action_164 (62) = happyShift action_46
action_164 (63) = happyShift action_47
action_164 (64) = happyShift action_48
action_164 (65) = happyShift action_49
action_164 (66) = happyShift action_50
action_164 (67) = happyShift action_51
action_164 (68) = happyShift action_52
action_164 (69) = happyShift action_53
action_164 (70) = happyShift action_54
action_164 (71) = happyShift action_55
action_164 (72) = happyShift action_56
action_164 _ = happyReduce_23

action_165 (33) = happyShift action_131
action_165 _ = happyReduce_13

action_166 (43) = happyShift action_44
action_166 (51) = happyShift action_45
action_166 (62) = happyShift action_46
action_166 (63) = happyShift action_47
action_166 (64) = happyShift action_48
action_166 (65) = happyShift action_49
action_166 (66) = happyShift action_50
action_166 (67) = happyShift action_51
action_166 (68) = happyShift action_52
action_166 (69) = happyShift action_53
action_166 (70) = happyShift action_54
action_166 (71) = happyShift action_55
action_166 (72) = happyShift action_56
action_166 _ = happyReduce_24

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  5 happyReduction_2
happyReduction_2  =  HappyAbsSyn5
		 ([]
	)

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1 : happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  6 happyReduction_4
happyReduction_4 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  6 happyReduction_5
happyReduction_5 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  6 happyReduction_6
happyReduction_6 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn6
		 (happy_var_1
	)
happyReduction_6 _  = notHappyAtAll 

happyReduce_7 = happyReduce 4 7 happyReduction_7
happyReduction_7 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (TypeDec happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_8 = happySpecReduce_1  8 happyReduction_8
happyReduction_8 (HappyTerminal (TokenID happy_var_1))
	 =  HappyAbsSyn8
		 (VarST happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  8 happyReduction_9
happyReduction_9 _
	_
	 =  HappyAbsSyn8
		 (RecordST []
	)

happyReduce_10 = happySpecReduce_3  8 happyReduction_10
happyReduction_10 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn8
		 (RecordST happy_var_2
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  8 happyReduction_11
happyReduction_11 (HappyTerminal (TokenID happy_var_3))
	_
	_
	 =  HappyAbsSyn8
		 (ArrayST happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  8 happyReduction_12
happyReduction_12 (HappyAbsSyn8  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn8
		 (FunST [happy_var_1] happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happyReduce 5 8 happyReduction_13
happyReduction_13 ((HappyAbsSyn8  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (FunST happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_14 = happyReduce 4 8 happyReduction_14
happyReduction_14 ((HappyAbsSyn8  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (FunST [] happy_var_4
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_1  9 happyReduction_15
happyReduction_15 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_15 _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  9 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  10 happyReduction_17
happyReduction_17 (HappyTerminal (TokenID happy_var_3))
	_
	(HappyTerminal (TokenID happy_var_1))
	 =  HappyAbsSyn10
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happyReduce 5 10 happyReduction_18
happyReduction_18 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_3)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 ((happy_var_1, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 4 11 happyReduction_19
happyReduction_19 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (UntypedVarDec happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 6 11 happyReduction_20
happyReduction_20 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (TypedVarDec happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 6 12 happyReduction_21
happyReduction_21 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (UntypedFunDec happy_var_2 [] happy_var_6
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 7 12 happyReduction_22
happyReduction_22 ((HappyAbsSyn4  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (UntypedFunDec happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 8 12 happyReduction_23
happyReduction_23 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_6)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (TypedFunDec happy_var_2 [] happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 9 12 happyReduction_24
happyReduction_24 ((HappyAbsSyn4  happy_var_9) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_7)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (TypedFunDec happy_var_2 happy_var_4 happy_var_7 happy_var_9
	) `HappyStk` happyRest

happyReduce_25 = happySpecReduce_1  13 happyReduction_25
happyReduction_25 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  13 happyReduction_26
happyReduction_26 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  13 happyReduction_27
happyReduction_27 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  13 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  13 happyReduction_29
happyReduction_29 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  13 happyReduction_30
happyReduction_30 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  13 happyReduction_31
happyReduction_31 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_2  14 happyReduction_32
happyReduction_32 (HappyAbsSyn15  happy_var_2)
	(HappyTerminal (TokenID happy_var_1))
	 =  HappyAbsSyn4
		 (happy_var_2 (Identifier happy_var_1)
	)
happyReduction_32 _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_0  15 happyReduction_33
happyReduction_33  =  HappyAbsSyn15
		 (id
	)

happyReduce_34 = happySpecReduce_3  15 happyReduction_34
happyReduction_34 (HappyAbsSyn15  happy_var_3)
	(HappyTerminal (TokenID happy_var_2))
	_
	 =  HappyAbsSyn15
		 (\object -> (happy_var_3 (FieldRef object happy_var_2))
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happyReduce 4 15 happyReduction_35
happyReduction_35 ((HappyAbsSyn15  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (\object -> happy_var_4 (ArrayRef object happy_var_2)
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_1  16 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn4
		 (Nil
	)

happyReduce_37 = happySpecReduce_1  16 happyReduction_37
happyReduction_37 (HappyTerminal (TokenInteger happy_var_1))
	 =  HappyAbsSyn4
		 (Num happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  16 happyReduction_38
happyReduction_38 (HappyTerminal (TokenString happy_var_1))
	 =  HappyAbsSyn4
		 (Str happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  17 happyReduction_39
happyReduction_39 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Funcall happy_var_1 []
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 4 17 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Funcall happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_1  18 happyReduction_41
happyReduction_41 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  18 happyReduction_42
happyReduction_42 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_3  19 happyReduction_43
happyReduction_43 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Binop Add happy_var_1 happy_var_3
	)
happyReduction_43 _ _ _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_3  19 happyReduction_44
happyReduction_44 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Binop Subtract happy_var_1 happy_var_3
	)
happyReduction_44 _ _ _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  19 happyReduction_45
happyReduction_45 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Binop Multiply happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  19 happyReduction_46
happyReduction_46 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Binop Divide happy_var_1 happy_var_3
	)
happyReduction_46 _ _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_3  19 happyReduction_47
happyReduction_47 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Binop Equals happy_var_1 happy_var_3
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  19 happyReduction_48
happyReduction_48 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Binop NotEq happy_var_1 happy_var_3
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_3  19 happyReduction_49
happyReduction_49 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Binop LessThan happy_var_1 happy_var_3
	)
happyReduction_49 _ _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  19 happyReduction_50
happyReduction_50 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Binop GreaterThan happy_var_1 happy_var_3
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_3  19 happyReduction_51
happyReduction_51 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Binop LessEq happy_var_1 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  19 happyReduction_52
happyReduction_52 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Binop GreaterEq happy_var_1 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  19 happyReduction_53
happyReduction_53 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Binop And happy_var_1 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  19 happyReduction_54
happyReduction_54 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Binop Or happy_var_1 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  19 happyReduction_55
happyReduction_55 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Negate happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_1  20 happyReduction_56
happyReduction_56 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_56 _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  20 happyReduction_57
happyReduction_57 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  21 happyReduction_58
happyReduction_58 _
	_
	(HappyTerminal (TokenID happy_var_1))
	 =  HappyAbsSyn4
		 (RecordCreation happy_var_1 []
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happyReduce 4 21 happyReduction_59
happyReduction_59 (_ `HappyStk`
	(HappyAbsSyn22  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (RecordCreation happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_60 = happySpecReduce_3  22 happyReduction_60
happyReduction_60 (HappyAbsSyn4  happy_var_3)
	_
	(HappyTerminal (TokenID happy_var_1))
	 =  HappyAbsSyn22
		 ([(happy_var_1, happy_var_3)]
	)
happyReduction_60 _ _ _  = notHappyAtAll 

happyReduce_61 = happyReduce 5 22 happyReduction_61
happyReduction_61 ((HappyAbsSyn22  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 ((happy_var_1, happy_var_3) : happy_var_5
	) `HappyStk` happyRest

happyReduce_62 = happyReduce 6 23 happyReduction_62
happyReduction_62 ((HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (ArrayCreation happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_63 = happySpecReduce_3  24 happyReduction_63
happyReduction_63 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (Assign happy_var_1 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happyReduce 5 25 happyReduction_64
happyReduction_64 ((HappyAbsSyn26  happy_var_5) `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (case happy_var_5 of
                  Nothing      -> If happy_var_2 happy_var_4 (Seq [])
                  Just elseexp -> If happy_var_2 happy_var_4 elseexp
	) `HappyStk` happyRest

happyReduce_65 = happyReduce 4 25 happyReduction_65
happyReduction_65 ((HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (While happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_66 = happyReduce 8 25 happyReduction_66
happyReduction_66 ((HappyAbsSyn4  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokenID happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (For happy_var_2 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_67 = happySpecReduce_1  25 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn4
		 (Break
	)

happyReduce_68 = happyReduce 5 25 happyReduction_68
happyReduction_68 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn5  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (Let happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_69 = happySpecReduce_3  25 happyReduction_69
happyReduction_69 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (Seq happy_var_2
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_0  26 happyReduction_70
happyReduction_70  =  HappyAbsSyn26
		 (Nothing
	)

happyReduce_71 = happySpecReduce_2  26 happyReduction_71
happyReduction_71 (HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn26
		 (Just happy_var_2
	)
happyReduction_71 _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_0  27 happyReduction_72
happyReduction_72  =  HappyAbsSyn18
		 ([]
	)

happyReduce_73 = happySpecReduce_1  27 happyReduction_73
happyReduction_73 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happySpecReduce_1  28 happyReduction_74
happyReduction_74 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn18
		 ([happy_var_1]
	)
happyReduction_74 _  = notHappyAtAll 

happyReduce_75 = happySpecReduce_3  28 happyReduction_75
happyReduction_75 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1 : happy_var_3
	)
happyReduction_75 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 73 73 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenInteger happy_dollar_dollar -> cont 29;
	TokenString happy_dollar_dollar -> cont 30;
	TokenID happy_dollar_dollar -> cont 31;
	TokenType -> cont 32;
	TokenArrow -> cont 33;
	TokenArray -> cont 34;
	TokenOf -> cont 35;
	TokenVar -> cont 36;
	TokenFunction -> cont 37;
	TokenNil -> cont 38;
	TokenBreak -> cont 39;
	TokenLet -> cont 40;
	TokenIn -> cont 41;
	TokenEnd -> cont 42;
	TokenEq -> cont 43;
	TokenComma -> cont 44;
	TokenColon -> cont 45;
	TokenSemicolon -> cont 46;
	TokenAssign -> cont 47;
	TokenDot -> cont 48;
	TokenLBrace -> cont 49;
	TokenRBrace -> cont 50;
	TokenLParen -> cont 51;
	TokenRParen -> cont 52;
	TokenLBracket -> cont 53;
	TokenRBracket -> cont 54;
	TokenIf -> cont 55;
	TokenThen -> cont 56;
	TokenElse -> cont 57;
	TokenFor -> cont 58;
	TokenWhile -> cont 59;
	TokenDo -> cont 60;
	TokenTo -> cont 61;
	TokenTimes -> cont 62;
	TokenDivide -> cont 63;
	TokenPlus -> cont 64;
	TokenMinus -> cont 65;
	TokenNeq -> cont 66;
	TokenLT -> cont 67;
	TokenGT -> cont 68;
	TokenLEQ -> cont 69;
	TokenGEQ -> cont 70;
	TokenAnd -> cont 71;
	TokenOr -> cont 72;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . error . show

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq



{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 317 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
