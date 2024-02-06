package Commands is
   function Command_Part (Of_String : String) return String;
   --  Returns the first white-space separated word in Of_String.

   function Argument_Part (Of_String : String) return String;
   --  Returns everything after the first white-space separated word
   --  in Of_String.
end Commands;
