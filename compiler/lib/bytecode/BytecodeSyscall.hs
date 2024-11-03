{-
-- EPITECH PROJECT, 2024
-- B-FUN-500_glados
-- File description:
-- BytecodeSyscall
-}

module BytecodeSyscall (
  syscallAdd,
  syscallSub,
  syscallMul,
  syscallDiv,
  syscallEq,
  syscallLess,
  syscallNot,
  syscallOr,
  syscallAnd,
  syscallAccessArray,
  syscallModifyArray
) where

import BytecodeOperations (getPush, getCall)

syscallAdd :: String
syscallAdd = (getPush "Add") ++ getCall

syscallSub :: String
syscallSub = (getPush "Sub") ++ getCall

syscallMul :: String
syscallMul = (getPush "Mul") ++ getCall

syscallDiv :: String
syscallDiv = (getPush "Div") ++ getCall

syscallMod :: String
syscallMod = (getPush "Mod") ++ getCall

syscallEq :: String
syscallEq = (getPush "Eq") ++ getCall

syscallLess :: String
syscallLess = (getPush "Less") ++ getCall

syscallNot :: String
syscallNot = (getPush "Not") ++ getCall

syscallOr :: String
syscallOr = (getPush "Or") ++ getCall

syscallAnd :: String
syscallAnd = (getPush "And") ++ getCall

syscallAccessArray :: String
syscallAccessArray = (getPush "Access") ++ getCall

syscallModifyArray :: String
syscallModifyArray = (getPush "Modify") ++ getCall
