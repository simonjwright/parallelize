--  Copyright (C) 2024 Simon Wright <simon@pushface.org>
--  Licence: Apache 2.0

with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;

package body Commands is
   function Is_White_Space (Ch : Character) return Boolean
     is (Ch = ' ' or else Ch = Ada.Characters.Latin_1.HT);

   function Command_Part (Of_String : String) return String
   is
   begin
      for J in Of_String'Range loop
         if Is_White_Space (Of_String (J)) then
            return Of_String (Of_String'First .. J - 1);
         end if;
      end loop;
      return Of_String;
   end Command_Part;

   function Argument_Part (Of_String : String) return String
   is
      Command : constant String := Command_Part (Of_String);
      Remainder : constant String
        := Of_String (Of_String'First + Command'Length
                        .. Of_String'Last);
   begin
      return Ada.Strings.Fixed.Trim (Remainder, Ada.Strings.Both);
   end Argument_Part;
end Commands;
