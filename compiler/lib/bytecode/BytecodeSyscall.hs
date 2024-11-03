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
  syscallModifyArray,
  syscallAccessObject,
  syscallModifyObject
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
syscallAccessArray = (getPush "AccessArray") ++ getCall

syscallModifyArray :: String
syscallModifyArray = (getPush "ModifyArray") ++ getCall

syscallAccessObject :: String
syscallAccessObject = (getPush "AccessObject") ++ getCall

syscallModifyObject :: String
syscallModifyObject = (getPush "ModifyObject") ++ getCall
