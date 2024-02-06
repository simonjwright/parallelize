with Ada.Containers.Ordered_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Commands;
with GNAT.OS_Lib;
with GNAT.Strings;

procedure Parallelize is

   --  We need to keep info about a parallel Job: at least the Pid of
   --  the process running it, and the name of the file receiving the
   --  output (which will contain the standard and error text,
   --  merged). Maybe also the Job text.
   --
   --  We keep the Job output in a temporary file so that the output
   --  of Parallel is presented in the order that the Jobs were
   --  submitted in.
   --
   --  When a Job completes, we get its Pid (and its status, not that
   --  that's very important, I think). So, we have a Map (ordered)
   --  from Pid to Job_Number. At this point, we can present the
   --  output from all the Jobs that have completed so far; there'll
   --  be a Vector (or Map in case we want to delete completed Jobs
   --  rather than skipping over them?) indexed by Job_Number.
   --
   --  I thnk we need to create the vector of Jobs before starting to
   --  process them, since we can't use tasking.

   --  Read in the Jobs
   --
   --  Start the first N Jobs
   --
   --  Wait until a Job finishes. Close its output. Present the
   --  outputs of all the Jobs that have completed so far, until we
   --  find a Job that hasn't completed. Start the next unstarted Job,
   --  if there is one. THINK MORE ABOUT THIS.

   function "+" (L : String) return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;
   function "+" (L : Ada.Strings.Unbounded.Unbounded_String) return String
     renames Ada.Strings.Unbounded.To_String;

   type Job is record
      Command          : Ada.Strings.Unbounded.Unbounded_String;
      Not_Executable   : Boolean := False;
      Started          : Boolean := False;
      Complete         : Boolean := False;
      Status           : Boolean := False;
      Output_File_Name : GNAT.OS_Lib.String_Access;
   end record;

   subtype Job_Index is Positive;

   package Job_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Job_Index,
      Element_Type => Job);
   use Job_Maps;

   function "<" (L, R : GNAT.OS_Lib.Process_Id) return Boolean;

   package Process_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => GNAT.OS_Lib.Process_Id,
      Element_Type => Job_Index);
   use Process_Maps;

   procedure Start_Processes;

   Jobs      : Job_Maps.Map;
   Processes : Process_Maps.Map;
   Max_Processes : Ada.Containers.Count_Type := 4;
   use type Ada.Containers.Count_Type;

   function "<" (L, R : GNAT.OS_Lib.Process_Id) return Boolean
   is
      --  Process_Id (on Darwin, anyway) is new Integer.
      function Convert
        is new Ada.Unchecked_Conversion (GNAT.OS_Lib.Process_Id, Integer);
   begin
      return Convert (L) < Convert (R);
   end "<";

   procedure Start_Processes is
   begin
      Next_Job :
      for J in Jobs.Iterate loop
         exit Next_Job when Processes.Length = Max_Processes;
         if not Jobs (J).Started then
            Set_Up_Job :
            declare
               The_Job : Job renames Jobs (J);
               Whole_Command : constant String := +The_Job.Command;
               Command : constant String
                 := Commands.Command_Part (Whole_Command);
               Executable : GNAT.OS_Lib.String_Access
                 := GNAT.OS_Lib.Locate_Exec_On_Path (Command);
               Pid : GNAT.OS_Lib.Process_Id;
               use type GNAT.OS_Lib.Process_Id;
               Unused_FD : GNAT.OS_Lib.File_Descriptor;
               use type GNAT.Strings.String_Access; -- for Executable
            begin
               The_Job.Started := True;
               if Executable = null then
                  Put_Line (Standard_Error,
                            "'" & Command & "' not executable");
                  The_Job.Not_Executable := True;
               else
                  GNAT.OS_Lib.Create_Temp_Output_File
                    (Unused_FD, The_Job.Output_File_Name);
                  Pid := GNAT.OS_Lib.Non_Blocking_Spawn
                    (Program_Name => Executable.all,
                     Args         => GNAT.OS_Lib.Argument_String_To_List
                       (Commands.Argument_Part (Whole_Command)).all,
                     Output_File  => The_Job.Output_File_Name.all,
                     Err_To_Out   => True);
                  if Pid = GNAT.OS_Lib.Invalid_Pid then
                     Put_Line (Standard_Error,
                               "couldn't spawn '" & Whole_Command & "'");
                     GNAT.Strings.Free (The_Job.Output_File_Name);
                  else
                     Processes.Insert (Key      => Pid,
                                       New_Item => Job_Maps.Key (J));
                  end if;
                  GNAT.Strings.Free (Executable);
               end if;  -- job was executable
            end Set_Up_Job;
         end if;
      end loop Next_Job;
   end Start_Processes;

begin

   Read_Commands :
   loop
      begin
         Read_Command :
         declare
            Input : constant String
              := Ada.Strings.Fixed.Trim (Get_Line, Ada.Strings.Both);
            Job_Number : constant Positive
              := (if Jobs.Is_Empty
                  then 1
                  else Jobs.Last_Key + 1);
         begin
            exit Read_Commands when Input'Length = 0;
            Jobs.Insert (Key      => Job_Number,
                         New_Item => Job'(Command => +Input,
                                          others  => <>));
         end Read_Command;
      exception
         when End_Error =>
            exit Read_Commands;
      end;
   end loop Read_Commands;

   Run :
   loop
      Find_Finished_Process :
      declare
         Finished_Pid : GNAT.OS_Lib.Process_Id;
         use type GNAT.OS_Lib.Process_Id;
         Job_Status : Boolean;
      begin
         Start_Processes;
         GNAT.OS_Lib.Wait_Process (Pid => Finished_Pid, Success => Job_Status);
         exit Run when Finished_Pid = GNAT.OS_Lib.Invalid_Pid; -- XXX
         Handle_Completed_Job :
         declare
            Job_ID : constant Job_Index := Processes (Finished_Pid);
            The_Job : Job renames Jobs (Job_ID);
            File : Ada.Text_IO.File_Type;
         begin
            Processes.Delete (Finished_Pid);
            Open (File, Name => The_Job.Output_File_Name.all, Mode => In_File);
            while not End_Of_File (File) loop
               Put_Line (Get_Line (File));
            end loop;
            Delete (File);
            GNAT.Strings.Free (The_Job.Output_File_Name);
            The_Job.Complete := True;
         end Handle_Completed_Job;
      end Find_Finished_Process;
   end loop Run;

end Parallelize;
