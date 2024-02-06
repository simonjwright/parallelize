with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Commands;
with GNAT.OS_Lib;
with GNAT.Strings;
with System.Multiprocessors;

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
      Executable       : Boolean := False;
      Started          : Boolean := False;
      Finished         : Boolean := False;
      Status           : Boolean := False;
      Complete         : Boolean := False;
      Output_File_Name : GNAT.OS_Lib.String_Access;
   end record;

   subtype Job_Index is Positive;

   package Job_Vectors is new Ada.Containers.Vectors
     (Index_Type     => Job_Index,
      Element_Type => Job);
   use Job_Vectors;

   function "<" (L, R : GNAT.OS_Lib.Process_Id) return Boolean;

   package Process_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => GNAT.OS_Lib.Process_Id,
      Element_Type => Job_Index);
   use Process_Maps;

   procedure Start_Processes;

   Jobs      : Job_Vectors.Vector;
   Processes : Process_Maps.Map;
   Max_Processes : Ada.Containers.Count_Type;
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
               else
                  The_Job.Executable := True;
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
                     Put_Line (Standard_Error,
                               "starting '" & Whole_Command & "'");
                     Processes.Insert (Key      => Pid,
                                       New_Item => Job_Vectors.To_Index (J));
                  end if;
                  GNAT.Strings.Free (Executable);
               end if;  -- job was executable
            end Set_Up_Job;
         end if;
      end loop Next_Job;
   end Start_Processes;

begin

   Max_Processes
     := Ada.Containers.Count_Type (System.Multiprocessors.Number_Of_CPUs);

   Read_Commands :
   loop
      begin
         Read_Command :
         declare
            Input : constant String
              := Ada.Strings.Fixed.Trim (Get_Line, Ada.Strings.Both);
         begin
            exit Read_Commands when Input'Length = 0;
            Jobs.Append (New_Item => Job'(Command => +Input,
                                          others  => <>));
         end Read_Command;
      exception
         when End_Error =>
            exit Read_Commands;
      end;
   end loop Read_Commands;

   Run :
   declare
      Last_Job_Output : Natural := Job_Index'First - 1;
   begin
      Monitor_Process_Completion :
      loop
         Find_Finished_Process :
         declare
            Finished_Pid : GNAT.OS_Lib.Process_Id;
            use type GNAT.OS_Lib.Process_Id;
            Job_Status : Boolean;
         begin
            Start_Processes;
            GNAT.OS_Lib.Wait_Process (Pid     => Finished_Pid,
                                      Success => Job_Status);
            exit Monitor_Process_Completion
            when Finished_Pid = GNAT.OS_Lib.Invalid_Pid;
            Handle_Completed_Job :
            declare
               Job_ID : constant Job_Index := Processes (Finished_Pid);
               The_Job : Job renames Jobs (Job_ID);
            begin
               Put_Line (Standard_Error,
                         "finished '" & (+The_Job.Command) & "'");
               The_Job.Status := Job_Status;
               The_Job.Finished := True;
               Processes.Delete (Finished_Pid);
            end Handle_Completed_Job;
            Output_Completed_Jobs_In_Order :
            for J in Last_Job_Output + 1 .. Jobs.Last_Index loop
               if Jobs (J).Executable then
                  exit Output_Completed_Jobs_In_Order
                  when not Jobs (J).Finished;
                  Last_Job_Output := J;
                  Output_Job :
                  declare
                     The_Job : Job renames Jobs (J);
                     File : Ada.Text_IO.File_Type;
                  begin
                     Open (File,
                           Name => The_Job.Output_File_Name.all,
                           Mode => In_File);
                     while not End_Of_File (File) loop
                        Put_Line (Get_Line (File));
                     end loop;
                     Delete (File);
                     GNAT.Strings.Free (The_Job.Output_File_Name);
                     The_Job.Complete := True;
                     Put_Line (Standard_Error,
                               "completed '" & (+The_Job.Command) & "'");
                  end Output_Job;
               end if;
            end loop Output_Completed_Jobs_In_Order;
         end Find_Finished_Process;
      end loop Monitor_Process_Completion;
   end Run;

end Parallelize;
