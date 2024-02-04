with Ada.Characters.Latin_1;
with Ada.Strings.Fixed;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib;
with GNAT.Strings;

procedure Parallel is
   function Is_White_Space (Ch : Character) return Boolean
     is (Ch = ' ' or else Ch = Ada.Characters.Latin_1.HT);

   --  function Command_Part (Of_String : String) return String
   --  with Pre => Of_String'Length > 0
   --              and then not Is_White_Space (Of_String (Of_String'First));

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

   function Argument_Part (Of_String : String;
                           Given_Command : String) return String
   is
      Remainder : constant String
        := Of_String (Of_String'First + Given_Command'Length
                        .. Of_String'Last);
   begin
      return Ada.Strings.Fixed.Trim (Remainder, Ada.Strings.Both);
   end Argument_Part;

   Id : GNAT.OS_Lib.Process_Id;
   Finished_Pid : GNAT.OS_Lib.Process_Id;
   Success : Boolean;

   use type GNAT.OS_Lib.Process_Id;
begin

   Read_Commands :
   loop
      begin
         Read_Command :
         declare
            Input : constant String
              := Ada.Strings.Fixed.Trim (Get_Line, Ada.Strings.Both);
         begin
            exit Read_Commands when Input'Length = 0;
            Process_Command :
            declare
               Command : constant String := Command_Part (Input);
               Executable : GNAT.OS_Lib.String_Access
                 := GNAT.OS_Lib.Locate_Exec_On_Path (Command);
               Args : constant String
                 := Argument_Part (Of_String => Input,
                                   Given_Command => Command);
               use type GNAT.OS_Lib.String_Access;
            begin
               if Executable = null then
                  Put_Line ("'" & Command & "' is not executable");
               else
                  Put_Line ("cmd: '" & Command & "', args: '" & Args & "'");
                  Id := GNAT.OS_Lib.Non_Blocking_Spawn
                    (Program_Name => Executable.all,
                     Args => GNAT.OS_Lib.Argument_String_To_List (Args).all);
                  if Id = GNAT.OS_Lib.Invalid_Pid then
                     Put_Line ("couldn't spawn");
                  else
                     Put_Line ("the spawned pid: " & Id'Image);
                  end if;
                  GNAT.Strings.Free (Executable);
               end if;
            end Process_Command;
         end Read_Command;
      exception
         when End_Error =>
            exit Read_Commands;
      end;
   end loop Read_Commands;

   Put_Line ("waiting ...");
   Waiting :
   loop
      GNAT.OS_Lib.Wait_Process (Pid => Finished_Pid, Success => Success);
      exit Waiting when Finished_Pid = GNAT.OS_Lib.Invalid_Pid;
      Put_Line ("finished pid: " & Finished_Pid'Image
                & ", status: " & Success'Image);
   end loop Waiting;
end Parallel;
