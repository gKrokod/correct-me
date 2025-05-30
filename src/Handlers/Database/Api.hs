module Handlers.Database.Api (createSpellBase, getAllSpells, addSpellBase, checkSpellBase)  where

-- import Handlers.Database.Authorization (getCopyRight, getPrivilege, getResultValid)
import Handlers.Database.Spell.Create (createSpellBase)
import Handlers.Database.Spell.Get (getAllSpells)
import Handlers.Database.Spell.Add (addSpellBase)
import Handlers.Database.Spell.Check (checkSpellBase)
